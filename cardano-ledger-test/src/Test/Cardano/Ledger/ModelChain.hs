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

import Control.Lens
import Control.Monad.State (MonadState(..))
import Data.Default.Class
import Data.Group
import Data.Kind (Type)
import Data.Proxy
import Data.Semigroup (Max(..))
import Data.Set (Set)
import Data.Traversable
import Numeric.Natural
import qualified Control.Monad.Trans.State as State
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified GHC.Exts as GHC
import Cardano.Crypto.Util (SignableRepresentation)
import Cardano.Ledger.Coin
import Cardano.Ledger.Crypto (KES, DSIGN)
import Cardano.Ledger.Era (Crypto, Era)
import qualified Cardano.Ledger.Era as Era
import Cardano.Ledger.SafeHash
import Cardano.Ledger.Shelley.Constraints
import Cardano.Slotting.Block
import Cardano.Slotting.Slot
import Control.State.Transition.Extended
import Shelley.Spec.Ledger.API.Genesis
import Shelley.Spec.Ledger.API.Validation
import Shelley.Spec.Ledger.Address
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.BlockChain
import Shelley.Spec.Ledger.Genesis
import Shelley.Spec.Ledger.Keys
import Shelley.Spec.Ledger.OCert
import Shelley.Spec.Ledger.STS.EraMapping ()
import qualified Cardano.Crypto.DSIGN.Class as DSIGN
import qualified Cardano.Crypto.Hash.Class as CHC
import qualified Cardano.Crypto.KES.Class as KES
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as C
import qualified Cardano.Ledger.Val as Val
import qualified Shelley.Spec.Ledger.Tx as Shelley

import Test.Shelley.Spec.Ledger.Generator.Core (AllIssuerKeys)
import Test.Shelley.Spec.Ledger.Generator.Core (mkOCert)
import Test.Shelley.Spec.Ledger.Generator.Core (mkBlock)
import Test.Shelley.Spec.Ledger.Generator.Constants (defaultConstants)
import Test.Shelley.Spec.Ledger.Generator.Presets (issuerKeys)
import Test.Shelley.Spec.Ledger.Generator.ScriptClass (mkKeyPairs)
import Shelley.Spec.Ledger.API (PraosCrypto)
import Shelley.Spec.Ledger.Serialization (ToCBORGroup)

import Shelley.Spec.Ledger.LedgerState (NewEpochState)

type KeyPair' crypto = (KeyPair 'Payment crypto, KeyPair 'Staking crypto)

data EraElaboratorState era = EraElaboratorState
  { _unusedKeyPairs :: [KeyPair' (Crypto era)]
  , _keys :: Map.Map ModelAddress (KeyPair' (Crypto era))
  , _txIds :: Map.Map ModelTxId (Shelley.TxId (Crypto era))
  , _blockNo :: BlockNo
  , _prevHash :: HashHeader (Crypto era)
  , _stakePool :: AllIssuerKeys (Crypto era) 'StakePool
  , _delegate :: AllIssuerKeys (Crypto era) 'BlockIssuer
  }

class HasEraElaboratorState s era | s -> era where
  eraElaboratorState
    :: forall f. Functor f
    => (EraElaboratorState era -> f (EraElaboratorState era)) -> s -> f s

instance HasEraElaboratorState (EraElaboratorState era) era where
  eraElaboratorState = id

unusedKeyPairs :: Functor f => ([KeyPair' (Crypto era)] -> f [KeyPair' (Crypto era)]) -> EraElaboratorState era -> f (EraElaboratorState era)
unusedKeyPairs a2fb s = (\b -> s {_unusedKeyPairs = b}) <$> a2fb (_unusedKeyPairs s)

keys :: Functor f => (Map.Map ModelAddress (KeyPair' (Crypto era)) -> f (Map.Map ModelAddress (KeyPair' (Crypto era)))) -> EraElaboratorState era -> f (EraElaboratorState era)
keys a2fb s = (\b -> s {_keys = b}) <$> a2fb (_keys s)

txIds :: Functor f => (Map.Map ModelTxId (Shelley.TxId (Crypto era)) -> f (Map.Map ModelTxId (Shelley.TxId (Crypto era)))) -> EraElaboratorState era -> f (EraElaboratorState era)
txIds a2fb s = (\b -> s {_txIds = b}) <$> a2fb (_txIds s)

blockNo :: Functor f => (BlockNo -> f BlockNo) -> EraElaboratorState era -> f (EraElaboratorState era)
blockNo a2fb s = (\b -> s {_blockNo = b}) <$> a2fb (_blockNo s)

prevHash :: Functor f => (HashHeader (Crypto era) -> f (HashHeader (Crypto era))) -> EraElaboratorState era -> f (EraElaboratorState era)
prevHash a2fb s = (\b -> s {_prevHash = b}) <$> a2fb (_prevHash s)

getKeyPairFor
  :: forall m era st proxy.
  ( MonadState st m
  , HasEraElaboratorState st era
  )
  => proxy era -> ModelAddress -> m (KeyPair' (Crypto era))
getKeyPairFor _ mAddr = do

  st <- use eraElaboratorState
  case Map.lookup mAddr (_keys st) of
    Just k -> pure k
    Nothing -> case _unusedKeyPairs st of
      [] -> error "ran out of keys"
      (k:ks) -> do
        eraElaboratorState . unusedKeyPairs .= ks
        eraElaboratorState . keys .= Map.insert mAddr k (_keys st)
        pure k

instance
    ( C.Crypto (Crypto era)
    ) => Default (EraElaboratorState era) where
  def = EraElaboratorState
    { _unusedKeyPairs = mkKeyPairs @(Crypto era) <$> [1..]
    , _keys = Map.empty
    , _txIds = Map.singleton (ModelTxId 0) genesisHash
    , _blockNo = 0
    , _prevHash = genesisHash'
    , _stakePool = issuerKeys defaultConstants 1 1
    , _delegate = issuerKeys defaultConstants 1 2
    }
    where
      genesisHash :: Shelley.TxId (Crypto era)
      genesisHash = Shelley.TxId $ unsafeMakeSafeHash $ CHC.castHash $ CHC.hashWith id "TEST GENESIS"
      genesisHash' :: HashHeader (Crypto era)
      genesisHash' = HashHeader $ CHC.castHash $ CHC.hashWith id "TEST GENESIS"

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


class ElaborateEraModel era where
  type ElaborateEraModelState era :: Type
  type ElaborateEraModelState era = EraElaboratorState era

  elaborateEraModel
    :: ShelleyGenesis era
    -> AdditionalGenesisConfig era
    -> Globals
    -> Map.Map ModelAddress Coin
    -> [ModelBlock]
    -> ( NewEpochState era
       , [ApplyBlockData era]
       )

  default elaborateEraModel
    :: ( Default (ElaborateEraModelState era)
       , HasEraElaboratorState (ElaborateEraModelState era) era
       , C.Crypto (Crypto era)
       , CanStartFromGenesis era
       , ApplyBlock era
       )
    => ShelleyGenesis era
    -> AdditionalGenesisConfig era
    -> Globals
    -> Map.Map ModelAddress Coin
    -> [ModelBlock]
    -> ( NewEpochState era
       , [ApplyBlockData era]
       )
  elaborateEraModel sg additionalGenesesConfig globals genesisAccounts blocks = flip State.evalState def $ do
    let
      maxTTL = maybe 1 getMax $ foldMap (Just . Max . _mbSlot) blocks

    nes <- do
      utxo0 <- fmap Map.fromList $ for (Map.toList genesisAccounts) $ \(mAddr, coins) -> do
        addr <- getKeyPairFor (Proxy :: Proxy era) mAddr
        pure (toAddr Testnet addr, coins)

      pure $ initialState sg {sgInitialFunds = Map.unionWith const utxo0 $ sgInitialFunds sg } additionalGenesesConfig

    blocks' :: [[ApplyBlockData era]] <- for blocks $ \block -> do
      (newTick, newBlock) <- State.state $ makeBlock globals maxTTL block
      pure $
        [ ApplyTick newTick
        , ApplyBlock newBlock
        ]

    pure $ (nes, concat blocks')


  makeTx
    :: proxy era
    -> Globals
    -> SlotNo
    -> ModelTx
    -> ElaborateEraModelState era
    -> (Era.TxInBlock era, ElaborateEraModelState era)

  makeBlock
    :: Globals
    -> SlotNo
    -> ModelBlock
    -> ElaborateEraModelState era
    -> ((SlotNo, Block era), ElaborateEraModelState era)

  default makeBlock
    ::
    ( HasEraElaboratorState (ElaborateEraModelState era) era
    , C.Crypto (Crypto era)
    , PraosCrypto (Crypto era)
    , ToCBORGroup (Era.TxSeq era)
    , Era era
    -- , ShelleyBasedEra era
    -- , SafeToHash (Core.Witnesses era)
    , KES.Signable (KES (Crypto era)) ~ SignableRepresentation
    , DSIGN.Signable (DSIGN (Crypto era)) ~ SignableRepresentation
    )
    => Globals
    -> SlotNo
    -> ModelBlock
    -> ElaborateEraModelState era
    -> ((SlotNo, Block era), ElaborateEraModelState era)
  makeBlock globals =
    let
    in \ttl mBlk -> State.runState $ do
      txSeq <- for (_mbUtxo mBlk) $ \blk -> do
        State.state $ makeTx (Proxy :: Proxy era) globals ttl blk
      st <- use eraElaboratorState
      let

        bHeaderHash :: HashHeader (Crypto era)
        bHeaderHash = bhHash $ bheader newBlock

        newBlock :: Block era
        newBlock = mkBlock
          (_prevHash st)
          (_stakePool st) -- (head . ksStakePools . geKeySpace $ myGenEnv)
          txSeq
          (_mbSlot mBlk)
          (_blockNo st)
          NeutralNonce
          0
          0
          (mkOCert (_delegate st) 0 (KESPeriod 0))
      eraElaboratorState . prevHash .= bHeaderHash
      pure (_mbSlot mBlk, newBlock)

  toEraPredicateFailure
    :: ModelPredicateFailure
    -> ApplyBlockTransitionError era

data ApplyBlockData era
  = ApplyTick (Signal (Core.EraRule "TICK" era))
  | ApplyBlock (Signal (Core.EraRule "BBODY" era))

deriving instance
  ( Eq (Signal (Core.EraRule "TICK" era))
  , Eq (Signal (Core.EraRule "BBODY" era))
  ) => Eq (ApplyBlockData era)

deriving instance
  ( Ord (Signal (Core.EraRule "TICK" era))
  , Ord (Signal (Core.EraRule "BBODY" era))
  ) => Ord (ApplyBlockData era)

deriving instance
  ( Show (Signal (Core.EraRule "TICK" era))
  , Show (Signal (Core.EraRule "BBODY" era))
  ) => Show (ApplyBlockData era)

data ApplyBlockTransitionError era
   = ApplyBlockTransitionError_Block (BlockTransitionError era)
   | ApplyBlockTransitionError_Tick (TickTransitionError era)

deriving instance
 ( Show (BlockTransitionError era)
 , Show (TickTransitionError era)
 ) => Show (ApplyBlockTransitionError era)

deriving instance
 ( Eq (BlockTransitionError era)
 , Eq (TickTransitionError era)
 ) => Eq (ApplyBlockTransitionError era)

deriving instance
 ( Ord (BlockTransitionError era)
 , Ord (TickTransitionError era)
 ) => Ord (ApplyBlockTransitionError era)

type ApplyBlockError era =
  ( ApplyBlockTransitionError era
  , State (Core.EraRule "TICK" era)
  , ApplyBlockData era
  )


