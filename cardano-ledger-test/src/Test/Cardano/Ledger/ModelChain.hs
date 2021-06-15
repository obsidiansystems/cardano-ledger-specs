{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.ModelChain where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import qualified Cardano.Ledger.Crypto as C
import Cardano.Ledger.Mary.Value (AssetName, PolicyID (..))
import qualified Cardano.Ledger.Mary.Value
import qualified Cardano.Ledger.Val as Val
import Cardano.Slotting.Slot hiding (at)
import Control.Lens
import Data.Kind (Type)
import qualified Data.Map as Map
import Data.Proxy
import Data.Set (Set)
import Numeric.Natural
import Shelley.Spec.Ledger.STS.EraMapping ()
import Test.Cardano.Ledger.ModelChain.Address
import Test.Cardano.Ledger.ModelChain.Script
import Test.Cardano.Ledger.ModelChain.Value

class Val.Val val => ValueFromList val crypto | val -> crypto where
  valueFromList :: Integer -> [(PolicyID crypto, AssetName, Integer)] -> val

  insert :: (Integer -> Integer -> Integer) -> PolicyID crypto -> AssetName -> Integer -> val -> val

  gettriples :: val -> (Integer, [(PolicyID crypto, AssetName, Integer)])

instance C.Crypto crypto => ValueFromList (Cardano.Ledger.Mary.Value.Value crypto) crypto where
  valueFromList = Cardano.Ledger.Mary.Value.valueFromList

  insert = Cardano.Ledger.Mary.Value.insert

  gettriples (Cardano.Ledger.Mary.Value.Value c m1) = (c, triples)
    where
      triples =
        [ (policyId, aname, amount)
          | (policyId, m2) <- Map.toList m1,
            (aname, amount) <- Map.toList m2
        ]

-- TODO: this is a bit overconstrained, we probably want a more constraints
-- based approach using ValueFromList
type family ElaborateValueType (valF :: TyValueExpected) crypto :: Type where
  ElaborateValueType 'ExpectAdaOnly _ = Coin
  ElaborateValueType 'ExpectAnyOutput crypto = Cardano.Ledger.Mary.Value.Value crypto

-- same convention as Data.Bool.bool;  True case comes last
data IfSupportsMint a b (valF :: TyValueExpected) where
  NoMintSupport :: a -> IfSupportsMint a b 'ExpectAdaOnly
  SupportsMint :: b -> IfSupportsMint a b 'ExpectAnyOutput

type family ValueFeature (a :: FeatureSet) where
  ValueFeature ('FeatureSet v _) = v

type family ScriptFeature (a :: FeatureSet) where
  ScriptFeature ('FeatureSet _ s) = s

data FeatureSet = FeatureSet TyValueExpected TyScriptFeature

data FeatureTag (tag :: FeatureSet) where
  FeatureTag :: ValueFeatureTag v -> ScriptFeatureTag s -> FeatureTag ('FeatureSet v s)

data ValueFeatureTag (v :: TyValueExpected) where
  ValueFeatureTag_AdaOnly :: ValueFeatureTag 'ExpectAdaOnly
  ValueFeatureTag_AnyOutput :: ValueFeatureTag 'ExpectAnyOutput

type family MaxValueFeature (a :: TyValueExpected) (b :: TyValueExpected) :: TyValueExpected where
  MaxValueFeature 'ExpectAdaOnly b = b
  MaxValueFeature a 'ExpectAdaOnly = a
  MaxValueFeature 'ExpectAnyOutput b = b
  MaxValueFeature a 'ExpectAnyOutput = a

-- law: () <$ filterFeatures (minFeatures x) y == () <$ filterFeatures x y
class RequiredFeatures (f :: FeatureSet -> Type) where
  -- minFeatures :: f a -> FeatureTag a
  filterFeatures ::
    KnownRequiredFeatures a =>
    FeatureTag b ->
    f a ->
    Maybe (f b)

class KnownValueFeature (v :: TyValueExpected) where reifyValueFeature :: proxy v -> ValueFeatureTag v

instance KnownValueFeature 'ExpectAdaOnly where reifyValueFeature _ = ValueFeatureTag_AdaOnly

instance KnownValueFeature 'ExpectAnyOutput where reifyValueFeature _ = ValueFeatureTag_AnyOutput

hasKnownValueFeature :: ValueFeatureTag v -> (KnownValueFeature v => c) -> c
hasKnownValueFeature = \case
  ValueFeatureTag_AdaOnly -> \x -> x
  ValueFeatureTag_AnyOutput -> \x -> x

class
  ( KnownValueFeature (ValueFeature a),
    KnownScriptFeature (ScriptFeature a),
    a ~ 'FeatureSet (ValueFeature a) (ScriptFeature a)
  ) =>
  KnownRequiredFeatures (a :: FeatureSet)
  where
  reifyRequiredFeatures :: proxy a -> FeatureTag a
  reifyRequiredFeatures _ =
    FeatureTag
      (reifyValueFeature (Proxy :: Proxy (ValueFeature a)))
      (reifyScriptFeature (Proxy :: Proxy (ScriptFeature a)))

hasKnownRequiredFeatures :: FeatureTag a -> (KnownRequiredFeatures a => c) -> c
hasKnownRequiredFeatures (FeatureTag v s) =
  \x -> hasKnownValueFeature v (hasKnownScriptFeature s x)

instance
  ( KnownValueFeature v,
    KnownScriptFeature s
  ) =>
  KnownRequiredFeatures ('FeatureSet v s)

instance RequiredFeatures FeatureTag where
  -- minFeatures = id
  filterFeatures (FeatureTag v0 s0) (FeatureTag v1 s1) = FeatureTag <$> v <*> s
    where
      v = case (v0, v1) of
        (ValueFeatureTag_AdaOnly, ValueFeatureTag_AdaOnly) -> Just v0
        (ValueFeatureTag_AdaOnly, ValueFeatureTag_AnyOutput) -> Just v0
        (ValueFeatureTag_AnyOutput, ValueFeatureTag_AdaOnly) -> Nothing
        (ValueFeatureTag_AnyOutput, ValueFeatureTag_AnyOutput) -> Just v0

      s = case (s0, s1) of
        (ScriptFeatureTag_None, ScriptFeatureTag_None) -> Just s0
        (ScriptFeatureTag_None, ScriptFeatureTag_Simple) -> Just s0
        (ScriptFeatureTag_None, ScriptFeatureTag_PlutusV1) -> Just s0
        (ScriptFeatureTag_Simple, ScriptFeatureTag_None) -> Nothing
        (ScriptFeatureTag_Simple, ScriptFeatureTag_Simple) -> Just s0
        (ScriptFeatureTag_Simple, ScriptFeatureTag_PlutusV1) -> Just s0
        (ScriptFeatureTag_PlutusV1, ScriptFeatureTag_None) -> Nothing
        (ScriptFeatureTag_PlutusV1, ScriptFeatureTag_Simple) -> Nothing
        (ScriptFeatureTag_PlutusV1, ScriptFeatureTag_PlutusV1) -> Just s0

instance RequiredFeatures ModelTxOut where
  filterFeatures tag@(FeatureTag v _) (ModelTxOut addr qty) =
    hasKnownValueFeature v $
      ModelTxOut addr <$> (filterFeatures tag =<< filterModelValue qty)

reifyExpectAnyOutput :: ValueFeatureTag a -> Maybe (a :~: 'ExpectAnyOutput)
reifyExpectAnyOutput = \case
  ValueFeatureTag_AnyOutput -> Just Refl
  ValueFeatureTag_AdaOnly -> Nothing

filterModelValueVars ::
  forall a b c d.
  (KnownRequiredFeatures c, KnownValueFeature d) =>
  ModelValueVars a b ->
  Maybe (ModelValueVars c d)
filterModelValueVars (ModelValue_Reward x) = Just (ModelValue_Reward x)
filterModelValueVars (ModelValue_MA x ys) = do
  Refl <- reifyExpectAnyOutput (reifyValueFeature (Proxy @d))
  Refl <- reifyExpectAnyOutput (reifyValueFeature (Proxy @(ValueFeature c)))

  let f :: ModelScript (ScriptFeature a) -> Maybe (ModelScript (ScriptFeature c))
      f = \case
        ModelScript_Timelock t -> case reifyScriptFeature (Proxy @(ScriptFeature c)) of
          ScriptFeatureTag_None -> Nothing
          ScriptFeatureTag_Simple -> Just $ ModelScript_Timelock t
          ScriptFeatureTag_PlutusV1 -> Just $ ModelScript_Timelock t

  ModelValue_MA x . Map.fromList <$> (traverse . _1) f (Map.toList ys)

-- change the "expected return type" of a ModelValue
filterModelValue ::
  forall a b c.
  (KnownValueFeature b, KnownRequiredFeatures c) =>
  ModelValue a c ->
  Maybe (ModelValue b c)
filterModelValue = \case
  ModelValue x -> ModelValue <$> traverse filterModelValueVars x

instance KnownValueFeature v => RequiredFeatures (ModelValue v) where
  filterFeatures tag (ModelValue val) = ModelValue <$> traverse (hasKnownRequiredFeatures tag filterModelValueVars) val

instance RequiredFeatures ModelTx where
  filterFeatures :: forall a b. KnownRequiredFeatures a => FeatureTag b -> ModelTx a -> Maybe (ModelTx b)
  filterFeatures tag (ModelTx a b c d e f g) =
    ModelTx a b
      <$> traverse (filterFeatures tag) c
      <*> (filterFeatures tag d)
      <*> pure e
      <*> traverse (filterFeatures tag) f
      <*> case g of
        NoMintSupport () -> case tag of
          FeatureTag ValueFeatureTag_AdaOnly _ -> pure $ NoMintSupport ()
          FeatureTag ValueFeatureTag_AnyOutput _ -> pure (SupportsMint . ModelValue . ModelValue_Inject $ Coin 0)
        SupportsMint g' -> case tag of
          FeatureTag ValueFeatureTag_AdaOnly _ -> Nothing
          FeatureTag ValueFeatureTag_AnyOutput _ -> SupportsMint <$> filterFeatures tag g'

instance RequiredFeatures ModelBlock where
  filterFeatures tag (ModelBlock slotNo txns) =
    ModelBlock slotNo
      <$> traverse (filterFeatures tag) txns

instance RequiredFeatures ModelEpoch where
  filterFeatures tag (ModelEpoch blocks x) =
    ModelEpoch
      <$> traverse (filterFeatures tag) blocks
      <*> pure x

newtype ModelTxId = ModelTxId Integer
  deriving (Eq, Ord, Show, Num)

data ModelTxIn
  = ModelTxIn ModelTxId Natural
  | ModelGensisIn ModelAddress
  deriving (Eq, Ord, Show)

type ModelMA era = Map.Map (ModelScript era) (Map.Map AssetName Integer)

data ModelValueVars era (k :: TyValueExpected) where
  ModelValue_Reward :: ModelAddress -> ModelValueVars era k
  ModelValue_MA ::
    ('ExpectAnyOutput ~ ValueFeature era) =>
    Coin ->
    ModelMA (ScriptFeature era) ->
    ModelValueVars era 'ExpectAnyOutput

deriving instance Show (ModelValueVars era valF)

deriving instance Eq (ModelValueVars era valF)

deriving instance Ord (ModelValueVars era valF)

newtype ModelValue k era = ModelValue {unModelValue :: ModelValueF (ModelValueVars era k)}
  deriving (Eq, Ord, Show)

data ModelTxOut era = ModelTxOut ModelAddress (ModelValue (ValueFeature era) era)
  deriving (Eq, Ord, Show)

data ModelUTxOId = ModelUTxOId ModelTxId Natural
  deriving (Eq, Ord, Show)

data ModelTx (era :: FeatureSet) = ModelTx
  { _mtxId :: !ModelTxId,
    _mtxInputs :: !(Set ModelTxIn),
    _mtxOutputs :: ![ModelTxOut era],
    _mtxFee :: !(ModelValue 'ExpectAdaOnly era),
    _mtxDCert :: ![ModelDCert],
    _mtxWdrl :: !(Map.Map ModelAddress (ModelValue 'ExpectAdaOnly era)),
    _mtxMint :: !(IfSupportsMint () (ModelValue (ValueFeature era) era) (ValueFeature era))
  }

data ModelBlock era = ModelBlock SlotNo [ModelTx era]

data ModelBlocksMade = ModelBlocksMade (Map.Map ModelAddress Natural)

data ModelEpoch era = ModelEpoch [ModelBlock era] ModelBlocksMade

data ModelDelegation = ModelDelegation
  { _mdDelegator :: !ModelAddress,
    _mdDelegatee :: !ModelAddress
  }

data ModelPoolParams = ModelPoolParams
  { _mppId :: !ModelAddress,
    _mppPledge :: !Coin,
    _mppCost :: !Coin,
    _mppMargin :: !UnitInterval,
    _mppRAcnt :: !ModelAddress,
    _mppOwners :: ![ModelAddress]
  }

-- ignores genesis delegation details.
data ModelDCert
  = ModelRegisterStake ModelAddress
  | ModelDeRegisterStake ModelAddress
  | ModelDelegate ModelDelegation
  | ModelRegisterPool ModelPoolParams
  | ModelRetirePool ModelAddress EpochNo

-- TODO: | ModelMIRCert Shelley.MIRPot (Map.Map ModelAddress DeltaCoin)

instance Semigroup ModelBlocksMade where
  ModelBlocksMade x <> ModelBlocksMade y = ModelBlocksMade $ Map.unionWith (+) x y

instance Monoid ModelBlocksMade where
  mempty = ModelBlocksMade Map.empty

data ModelPredicateFailure era
  = ModelValueNotConservedUTxO
      !(ModelValue (ValueFeature era) era)
      -- ^ the Coin consumed by this transaction
      !(ModelValue (ValueFeature era) era)
      -- ^ the Coin produced by this transaction
