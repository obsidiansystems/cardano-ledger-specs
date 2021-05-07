{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.ModelChain where

import Cardano.Ledger.Coin
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Shelley.Constraints
import Cardano.Slotting.Slot
import Control.Lens
import Control.Monad.State (MonadState(..))
import Control.Monad.Trans.Class (lift)
import Data.Default.Class
import Data.Foldable
import Data.Group
import Data.Kind (Type)
import Data.Proxy
import Data.Set (Set)
import Data.Traversable
import Data.Word (Word64)
import Numeric.Natural
import Shelley.Spec.Ledger.API.Genesis
import Shelley.Spec.Ledger.API.Mempool
import Shelley.Spec.Ledger.API.Validation
import Shelley.Spec.Ledger.Address
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.Genesis
import Shelley.Spec.Ledger.Keys
import Shelley.Spec.Ledger.LedgerState (NewEpochState)
import Shelley.Spec.Ledger.STS.EraMapping ()
import Test.Shelley.Spec.Ledger.Generator.ScriptClass (mkKeyPairs)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as C
import qualified Cardano.Ledger.Era as Era
import qualified Cardano.Ledger.Val as Val
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Trans.State as State
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified GHC.Exts as GHC
import qualified Shelley.Spec.Ledger.LedgerState as LedgerState
import qualified Shelley.Spec.Ledger.Tx as Shelley

type KeyPair' crypto = (KeyPair 'Payment crypto, KeyPair 'Staking crypto)

data EraElaboratorState era = EraElaboratorState
  { _unusedKeyPairs :: Word64
  , _keys :: Map.Map ModelAddress (KeyPair' (Crypto era))
  , _txIds :: Map.Map ModelTxId (Shelley.TxId (Crypto era))
  }

class HasEraElaboratorState s era | s -> era where
  eraElaboratorState
    :: forall f. Functor f
    => (EraElaboratorState era -> f (EraElaboratorState era)) -> s -> f s

instance HasEraElaboratorState (EraElaboratorState era) era where
  eraElaboratorState = id

unusedKeyPairs :: Functor f => (Word64 -> f Word64) -> EraElaboratorState era -> f (EraElaboratorState era)
unusedKeyPairs a2fb s = (\b -> s {_unusedKeyPairs = b}) <$> a2fb (_unusedKeyPairs s)

keys :: Functor f => (Map.Map ModelAddress (KeyPair' (Crypto era)) -> f (Map.Map ModelAddress (KeyPair' (Crypto era)))) -> EraElaboratorState era -> f (EraElaboratorState era)
keys a2fb s = (\b -> s {_keys = b}) <$> a2fb (_keys s)

txIds :: Functor f => (Map.Map ModelTxId (Shelley.TxId (Crypto era)) -> f (Map.Map ModelTxId (Shelley.TxId (Crypto era)))) -> EraElaboratorState era -> f (EraElaboratorState era)
txIds a2fb s = (\b -> s {_txIds = b}) <$> a2fb (_txIds s)

getKeyPairFor
  :: forall m era st proxy.
  ( MonadState st m
  , HasEraElaboratorState st era
  , C.Crypto (Crypto era)
  )
  => proxy era -> ModelAddress -> m (KeyPair' (Crypto era))
getKeyPairFor _ mAddr = do

  st <- use eraElaboratorState
  case Map.lookup mAddr (_keys st) of
    Just k -> pure k
    Nothing -> do
      unusedKeyPairId <- eraElaboratorState . unusedKeyPairs <<%= succ

      let k = mkKeyPairs @(Crypto era) unusedKeyPairId
      eraElaboratorState . keys .= Map.insert mAddr k (_keys st)
      pure k

instance Default (EraElaboratorState era) where
  def = EraElaboratorState
    { _unusedKeyPairs = 1
    , _keys = Map.empty
    , _txIds = Map.empty
    }

mkTxOut
  :: forall m era st.
  ( MonadState st m
  , HasEraElaboratorState st era
  , Era era
  , UsesTxOut era
  )
  => ModelTxOut
  -> m (Core.TxOut era)
mkTxOut (ModelTxOut mAddr (ModelValue mValue)) = do
  addr <- getKeyPairFor (Proxy :: Proxy era) mAddr
  pure (makeTxOut (Proxy :: Proxy era) (toAddr Testnet addr) (Val.inject $ Coin mValue))

mkTxIn :: forall m era st.
  ( MonadState st m
  , HasEraElaboratorState st era
  , C.Crypto (Crypto era)
  )
  => ModelTxIn
  -> m (Set.Set (Shelley.TxIn (Crypto era)))
mkTxIn = \case
  -- TODO: handle missing txnIds more gracefully?
  ModelTxIn mtxId idx -> do
    ses <- use eraElaboratorState
    pure . maybe Set.empty Set.singleton $ Shelley.TxIn <$> Map.lookup mtxId (_txIds ses) <*> pure idx
  ModelGensisIn mAddr -> do
    myKeys <- getKeyPairFor (Proxy :: Proxy era) mAddr
    pure $ Set.singleton $ initialFundsPseudoTxIn $ toAddr Testnet myKeys


newtype ModelTxId = ModelTxId Integer
  deriving (Eq, Ord, Show, Num)
newtype ModelAddress = ModelAddress String
  deriving (Eq, Ord, Show, GHC.IsString)

-- similarity to coin is merely a temporary convenience; not a design feature.
newtype ModelValue = ModelValue { unModelValue :: Integer }
  deriving (Eq, Ord, Show, Num)
deriving via Coin instance Semigroup ModelValue
deriving via Coin instance Monoid ModelValue
deriving via Coin instance Group ModelValue
deriving via Coin instance Abelian ModelValue
deriving via Coin instance Val.Val ModelValue



data ModelTxIn
  = ModelTxIn ModelTxId Natural
  | ModelGensisIn ModelAddress
  deriving (Eq, Ord, Show)
data ModelTxOut = ModelTxOut ModelAddress ModelValue
  deriving (Eq, Ord, Show)

data ModelTx = ModelTx
  { _mtxId :: !ModelTxId
  , _mtxInputs :: !(Set ModelTxIn)
  , _mtxOutputs :: ![ModelTxOut]
  , _mtxFee :: !ModelValue
  , _mtxWitness :: !(Set ModelAddress)
  }

data ModelBlock = ModelBlock
  { _mbSlot :: !SlotNo
  , _mbUtxo :: ![ModelTx]
  }


data ModelPredicateFailure
  = ModelValueNotConservedUTxO
      !ModelValue
      -- ^ the Coin consumed by this transaction
      !ModelValue
      -- ^ the Coin produced by this transaction

mempoolState :: Functor f => (MempoolState era -> f (MempoolState era)) -> (NewEpochState era -> f (NewEpochState era))
mempoolState = \a2b s ->
  let
    nesEs = LedgerState.nesEs s
    esLState = LedgerState.esLState nesEs

    mkNES (utxoState, delegationState) = s
      { LedgerState.nesEs = nesEs
        { LedgerState.esLState = esLState
          { LedgerState._utxoState = utxoState
          , LedgerState._delegationState = delegationState
          }
        }
      }
  in mkNES <$> a2b (mkMempoolState s)
{-# INLINE mempoolState #-}

class ElaborateEraModel era where
  type ElaborateEraModelState era :: Type
  type ElaborateEraModelState era = EraElaboratorState era

  elaborateBlock
    :: Globals
    -> ModelBlock
    -> (NewEpochState era, ElaborateEraModelState era)
    -> ( Either (ApplyTxError era) (SlotNo, [Era.TxInBlock era])
       , (NewEpochState era, ElaborateEraModelState era)
       )

  default elaborateBlock
    :: ( ApplyBlock era, ApplyTx era )
    => Globals
    -> ModelBlock
    -> (NewEpochState era, ElaborateEraModelState era)
    -> ( Either (ApplyTxError era) (SlotNo, [Era.TxInBlock era])
       , (NewEpochState era, ElaborateEraModelState era)
       )
  elaborateBlock globals blk = State.runState $ Except.runExceptT $ do
    let
      slot = (_mbSlot blk)
      ttl = succ slot

    -- tick the model
    lift $ zoom _1 $ State.state $ \nes0 -> ((), applyTick globals nes0 slot)

    txSeq <- lift $ zoom _2 $ for (_mbUtxo blk) $ \tx -> State.state $ makeTx (Proxy :: Proxy era) globals ttl tx

    mempoolEnv <- (\(nes0, _) -> mkMempoolEnv nes0 slot) <$> get

    -- apply the transactions.
    for_ txSeq $ \tx -> do
      (nes0, ems) <- get
      mps' <- applyTxInBlock globals mempoolEnv (view mempoolState nes0) tx
      let nes1 = set mempoolState mps' nes0
      put (nes1, ems)

    pure (slot, txSeq)

  elaborateInitialState
    :: ShelleyGenesis era
    -> AdditionalGenesisConfig era
    -> Map.Map ModelAddress Coin
    -> ElaborateEraModelState era
    -> ( NewEpochState era
       , ElaborateEraModelState era
       )

  default elaborateInitialState
    :: ( HasEraElaboratorState (ElaborateEraModelState era) era
       , C.Crypto (Crypto era)
       , CanStartFromGenesis era
       )
    => ShelleyGenesis era
    -> AdditionalGenesisConfig era
    -> Map.Map ModelAddress Coin
    -> ElaborateEraModelState era
    -> ( NewEpochState era
       , ElaborateEraModelState era
       )
  elaborateInitialState sg additionalGenesesConfig genesisAccounts = State.runState $ do
    utxo0 <- fmap Map.fromList $ for (Map.toList genesisAccounts) $ \(mAddr, coins) -> do
      addr <- getKeyPairFor (Proxy :: Proxy era) mAddr
      pure (toAddr Testnet addr, coins)

    pure $ initialState sg {sgInitialFunds = Map.unionWith const utxo0 $ sgInitialFunds sg } additionalGenesesConfig

  makeTx
    :: proxy era
    -> Globals
    -> SlotNo
    -> ModelTx
    -> ElaborateEraModelState era
    -> (Era.TxInBlock era, ElaborateEraModelState era)

  toEraPredicateFailure
    :: ModelPredicateFailure
    -> ApplyBlockTransitionError era

elaborateBlocks_
  :: ElaborateEraModel era
  => Globals
  -> [ModelBlock]
  -> (NewEpochState era, ElaborateEraModelState era)
  -> ( Either (ApplyTxError era) ()
     , (NewEpochState era, ElaborateEraModelState era)
     )

elaborateBlocks_ globals = State.runState . Except.runExceptT . traverse_ (Except.ExceptT . State.state . elaborateBlock globals)


data ApplyBlockTransitionError era
   = ApplyBlockTransitionError_Tx (ApplyTxError era)

deriving instance
 ( Show (ApplyTxError era)
 ) => Show (ApplyBlockTransitionError era)

deriving instance
 ( Eq (ApplyTxError era)
 ) => Eq (ApplyBlockTransitionError era)

deriving instance
 ( Ord (ApplyTxError era)
 ) => Ord (ApplyBlockTransitionError era)

