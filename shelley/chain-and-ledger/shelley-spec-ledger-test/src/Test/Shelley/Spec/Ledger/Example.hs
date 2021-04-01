{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Shelley.Spec.Ledger.Example where

import Control.Monad
import Control.State.Transition.Extended
import Data.Foldable
import Data.Functor.Compose
import Data.Group
import Data.Map (Map)
import Data.Monoid
import Data.Set (Set)
import Data.Typeable
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Void
import Data.Default

import Shelley.Spec.Ledger.API.Validation
import qualified Cardano.Ledger.Core as Core


-- TODO: find this or put this in a library
newtype MonoidMap k w = MonoidMap { getMonoidMap :: Map k w }
  deriving (Eq, Show, Foldable)

instance (Ord k, Monoid w, Eq w) => Semigroup (MonoidMap k w) where
  MonoidMap xs <> MonoidMap ys = MonoidMap $ Map.filter (/= mempty) $ Map.unionWith (<>) xs ys

instance (Ord k, Monoid w, Eq w) => Monoid (MonoidMap k w) where
  mempty = MonoidMap Map.empty

instance (Ord k, Group w, Eq w) => Group (MonoidMap k w) where
  invert (MonoidMap xs) = MonoidMap (invert <$> xs)


-- TODO: find this or put this in a library
data Valid e a = Invalid e | Valid a
  deriving (Functor, Foldable, Traversable, Eq, Ord, Show)

valid :: (e -> c) -> (a -> c) -> Valid e a -> c
valid e2c a2c = \case
  Invalid e -> e2c e
  Valid a -> a2c a

instance Semigroup e => Applicative (Valid e) where
  pure = Valid
  Invalid l <*> Invalid r = Invalid (l <> r)
  Invalid l <*> Valid _ = Invalid l
  Valid _ <*> Invalid r = Invalid r
  Valid f <*> Valid x = Valid (f x)

deriving via (Ap (Valid e) a)
  instance (Semigroup e, Semigroup a) => Semigroup (Valid e a)

deriving via (Ap (Valid e) a)
  instance (Semigroup e, Monoid a) => Monoid (Valid e a)


type family MOutput a
type family MTxId a
type family MInvalidTxn a

data ModelChain a

data ModelChainPredicateFailure a
   = ReusedTxnId (Set (MTxId a))
   | DoubleSpend (Set (MTxId a, Int))
   | InvalidTxn (MInvalidTxn a)

deriving stock instance
  ( Eq (MTxId a)
  , Eq (MOutput a)
  , Eq (MInvalidTxn a)
  ) => Eq (ModelChainPredicateFailure a)

deriving stock instance
  ( Show (MTxId a)
  , Show (MOutput a)
  , Show (MInvalidTxn a)
  ) => Show (ModelChainPredicateFailure a)

data ModelChainState a = ModelChainState
  { _modelChainState_utxos :: Map.Map (MTxId a, Int) (MOutput a)
  , _modelChainState_slots :: Set (MTxId a)
  }

deriving stock instance
  ( Show (MTxId a)
  , Show (MOutput a)
  ) => Show (ModelChainState a)

data ModelTxn a = ModelTxn
  { _modelTxn_id :: MTxId a
  , _modelTxn_inputs :: [(MTxId a, Int)]
  , _modelTxn_outputs :: [MOutput a]
  }

class ModelTransactionValidator a where
  modelTransactionValidator_
    :: proxy a
    -> [MOutput a]
    -> [MOutput a]
    -> Valid (MInvalidTxn a) ()

data ModelTxnX a

instance
    ( Typeable a
    , Eq (MInvalidTxn a), Show (MInvalidTxn a)
    , ModelTransactionValidator a
    )
    => STS (ModelTxnX a) where
  type PredicateFailure (ModelTxnX a) = MInvalidTxn a
  type State (ModelTxnX a) = ()
  type Environment (ModelTxnX a) = ([MOutput a], [MOutput a])
  type Signal (ModelTxnX a) = ()
  type BaseM (ModelTxnX a) = BaseM (ModelChain a)

  transitionRules = pure $ judgmentContext >>= \(TRC ((inputs, outputs), (), ())) ->
    valid failBecause pure $ modelTransactionValidator_ (Proxy :: Proxy a) inputs outputs


instance
    ( Typeable a, ModelTransactionValidator a
    , Eq (MOutput a), Show (MOutput a)
    , Ord (MTxId a), Show (MTxId a)
    , Eq (MInvalidTxn a), Show (MInvalidTxn a)
    )
    => STS (ModelChain a) where
  type PredicateFailure (ModelChain a) = ModelChainPredicateFailure a
  type State (ModelChain a) = ModelChainState a
  type Environment (ModelChain a) = ()
  type Signal (ModelChain a) = [ModelTxn a]

  transitionRules = pure modelChainTransitionRule


instance
    ( Typeable a
    , Eq (MInvalidTxn a)
    , Show (MInvalidTxn a)
    , ModelTransactionValidator a
    )
    => Embed (ModelTxnX a) (ModelChain a) where
  wrapFailed = InvalidTxn

modelChainTransitionRule :: forall a.
  ( Ord (MTxId a)
  , ModelTransactionValidator a
  , Typeable a
  , Eq (MInvalidTxn a)
  , Show (MInvalidTxn a)
  )
  => Rule (ModelChain a) 'Transition (ModelChainState a)
modelChainTransitionRule = judgmentContext >>= \(TRC (_, ModelChainState utxos slots, block)) -> do
  let
    spentUtxos :: Set.Set (MTxId a, Int)
    spentUtxos = Map.keysSet allInputs

    allInputs :: Map (MTxId a, Int) (Sum Int)
    allInputs = getMonoidMap $ foldMap (\(ModelTxn _ inputs _) -> foldMap (MonoidMap . flip Map.singleton (Sum 1)) inputs) block

    newUtxos :: Map (MTxId a, Int) (MOutput a)
    newUtxos = foldMap (\(ModelTxn txid _ outputs) -> foldMap (\(n, x) -> Map.singleton (txid, n) x) $ zipWith (,) [0,1..] outputs) block

    newTxnIds :: Set (MTxId a)
    newTxnIds = Set.fromList $ (\(ModelTxn txid _ _) -> txid) <$> block

    reusedTxIds = newTxnIds `Set.intersection` slots

  reusedTxIds == Set.empty
    ?! ReusedTxnId reusedTxIds
  all (== Sum 1) allInputs
    ?! DoubleSpend (Map.keysSet $ Map.filter (/= Sum 1) allInputs)
  Map.difference allInputs utxos == Map.empty
    ?! DoubleSpend (Map.keysSet $ Map.difference allInputs utxos)

  flip traverse_ block $ \(ModelTxn _ inputs outputs) ->
    case traverse (\k -> maybe (Invalid $ Set.singleton k) Valid $ Map.lookup k utxos) inputs of
      Invalid inputs' -> failBecause $ DoubleSpend inputs'
      Valid inputs' -> do
        trans @ (ModelTxnX a) $ TRC ((inputs', outputs), (), ())


  pure $ ModelChainState
    (Map.union (Map.withoutKeys utxos spentUtxos) newUtxos)
    (Set.union slots newTxnIds)


-- a "sketch" of a blockchain with a respectable set of features comparable to
-- Shelley

data V1

data V1Error
  = V1ImbalancedTxn [MOutput V1] [MOutput V1]
  | V1NegativeOutput [MOutput V1]
  deriving (Show, Eq)

type instance MOutput V1 = (String, (Sum Integer))
type instance MTxId V1 = Int
type instance MInvalidTxn V1 = [V1Error]

instance ModelTransactionValidator V1 where
  modelTransactionValidator_ _ inputs outputs
    =  when (foldMap snd inputs /= foldMap snd outputs) (Invalid $ pure $ V1ImbalancedTxn inputs outputs)
    *> unless (all (> Sum 0) $ Compose outputs) (Invalid $ pure $ V1NegativeOutput outputs)

data V1_Tick

type instance Core.EraRule "TICK" V1 = V1_Tick
type instance Core.EraRule "BBODY" V1 = ModelChain V1

instance Default (ModelChainState a) where
  def = ModelChainState Map.empty Set.empty

instance STS V1_Tick where
  type Environment V1_Tick = ()
  type PredicateFailure V1_Tick = Void
  type State V1_Tick = (Int, State (ModelChain V1))
  type Signal V1_Tick = ()

  transitionRules = pure $ judgmentContext >>= \(TRC (~(), (slot, state) , ())) -> do
    pure (succ slot, state)


instance ApplyBlock' V1 where

  getBBodyState _ = snd
  setBBodyState _ (slot, _) st = (slot, st)
  getBBodyEnv _ _ = ()
  -- wrapBlockError _ = id

genStateFromAccounts :: Ord (MTxId a) => MTxId a -> [MOutput a] -> ModelChainState a
genStateFromAccounts txid xs = ModelChainState
  (Map.fromList $ zip ((,) txid <$> [0..]) xs)
  (Set.singleton txid)

testExample :: IO ()
testExample = do
  let p = print

  p $ applySTSList (Proxy :: Proxy (ModelChain V1))
    (genStateFromAccounts 0 [("alice", 2000)])
    [ [ModelTxn 1 [(0,0)] [("bob", 100), ("alice", 1900)]]
    ]

  p $ applySTSList (Proxy :: Proxy (ModelChain V1))
    (genStateFromAccounts 0 [("alice", 2000)])
    [ [ModelTxn 1 [(0,0)] [("bob", 100), ("alice", 1900)]]
    , [ModelTxn 1 [(0,0)] [("bob", 100), ("alice", 1900)]]
    ]

  p $ applySTSList (Proxy :: Proxy (ModelChain V1))
    (genStateFromAccounts 0 [("alice", 2000)])
    [ [ModelTxn 1 [(0,0)] [("bob", 100), ("carol", 200), ("alice", 1700)]]
    ]

  p $ applySTSList (Proxy :: Proxy (ModelChain V1))
    (genStateFromAccounts 0 [("alice", 2000)])
    [ [ModelTxn 1 [(0,0)] [("bob", 100), ("alice", 1900)]]
    , [ModelTxn 1 [(1,1)] [("carol", 100), ("alice", 1800)]]
    ]

  p $ applySTSList (Proxy :: Proxy (ModelChain V1))
    (genStateFromAccounts 0 [("alice", 2000)])
    [ [ModelTxn 1 [(0,0)] [("bob", 100), ("alice", 1900)]]
    , [ModelTxn 2 [(1,1)] [("carol", 100), ("alice", 1800)]]
    ]

  pure ()
