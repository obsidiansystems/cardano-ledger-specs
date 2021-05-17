{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
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
import Cardano.Slotting.Slot hiding (at)
import Control.Lens
import Control.Monad
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
import Test.Shelley.Spec.Ledger.Utils (mkVRFKeyPair)
import Cardano.Slotting.EpochInfo.API
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Crypto.Hash.Class as Hash
import qualified Data.Sequence.Strict as StrictSeq
import qualified Cardano.Ledger.Crypto as C
import qualified Cardano.Ledger.Era as Era
import qualified Cardano.Ledger.Val as Val
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Trans.State as State hiding (state)
import qualified Control.Monad.State.Class as State
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified GHC.Exts as GHC
import qualified Shelley.Spec.Ledger.LedgerState as LedgerState
import qualified Shelley.Spec.Ledger.Tx as Shelley
import Shelley.Spec.Ledger.EpochBoundary (BlocksMade (..))
import Shelley.Spec.Ledger.Credential
  ( Credential(..)
  , StakeCredential
  )
import Shelley.Spec.Ledger.TxBody
  ( DCert(..)
  , PoolParams(..)
  , DelegCert(..)
  , Delegation(..)
  , PoolCert(..)
  )
import qualified Cardano.Crypto.VRF.Class (VerKeyVRF)


type KeyPair' crypto = (KeyPair 'Payment crypto, KeyPair 'Staking crypto)

data EraElaboratorState era = EraElaboratorState
  { _eesUnusedKeyPairs :: Word64
  , _eesKeys :: Map.Map ModelAddress (TestKeyPair (Crypto era))
  , _eesTxIds :: Map.Map ModelTxId (Shelley.TxId (Crypto era))
  , _eesCurrentEpoch :: EpochNo
  }

class HasEraElaboratorState s era | s -> era where
  eraElaboratorState
    :: forall f. Functor f
    => (EraElaboratorState era -> f (EraElaboratorState era)) -> s -> f s

instance HasEraElaboratorState (EraElaboratorState era) era where
  eraElaboratorState = id

eesUnusedKeyPairs :: Functor f => (Word64 -> f Word64) -> EraElaboratorState era -> f (EraElaboratorState era)
eesUnusedKeyPairs a2fb s = (\b -> s {_eesUnusedKeyPairs = b}) <$> a2fb (_eesUnusedKeyPairs s)

eesKeys :: Functor f => (Map.Map ModelAddress (TestKeyPair (Crypto era)) -> f (Map.Map ModelAddress (TestKeyPair (Crypto era)))) -> EraElaboratorState era -> f (EraElaboratorState era)
eesKeys a2fb s = (\b -> s {_eesKeys = b}) <$> a2fb (_eesKeys s)

eesTxIds :: Functor f => (Map.Map ModelTxId (Shelley.TxId (Crypto era)) -> f (Map.Map ModelTxId (Shelley.TxId (Crypto era)))) -> EraElaboratorState era -> f (EraElaboratorState era)
eesTxIds a2fb s = (\b -> s {_eesTxIds = b}) <$> a2fb (_eesTxIds s)

eesCurrentEpoch :: Functor f => (EpochNo -> f EpochNo) -> EraElaboratorState era -> f (EraElaboratorState era)
eesCurrentEpoch a2fb s = (\b -> s {_eesCurrentEpoch = b}) <$> a2fb (_eesCurrentEpoch s)

data TestKeyPair crypto = TestKeyPair
  { _tkpKeyPair :: KeyPair' crypto
  , _tkpVRF :: (SignKeyVRF crypto , VerKeyVRF crypto )
  , _tkpAddr :: Addr crypto
  , _tkpVRFHash :: Hash.Hash (C.HASH crypto) (Cardano.Crypto.VRF.Class.VerKeyVRF (C.VRF crypto))
  , _tkpStakePool :: KeyHash 'StakePool crypto
  , _tpkStakeCredential :: StakeCredential crypto
  }


getKeyPairForImpl
  :: forall m era st proxy.
  ( MonadState st m
  , HasEraElaboratorState st era
  , C.Crypto (Crypto era)
  )
  => proxy era -> ModelAddress
  -> m (TestKeyPair (Crypto era))
getKeyPairForImpl _ mAddr = do
  st <- use eraElaboratorState
  case Map.lookup mAddr (_eesKeys st) of
    Just k -> pure k
    Nothing -> do
      unusedKeyPairId <- eraElaboratorState . eesUnusedKeyPairs <<%= succ

      let
        keyPair@(_, poolKey) = mkKeyPairs @(Crypto era) unusedKeyPairId
        vrf@(_, vrf') = mkVRFKeyPair @(C.VRF (Crypto era)) (1, 0, 0, 0, unusedKeyPairId)
        k = TestKeyPair
          { _tkpKeyPair = keyPair
          , _tkpVRF = vrf
          , _tkpAddr = toAddr Testnet keyPair
          , _tkpVRFHash = hashVerKeyVRF vrf'
          , _tkpStakePool = hashKey . coerceKeyRole @_ @_ @(Crypto era) $ vKey poolKey
          , _tpkStakeCredential = KeyHashObj . hashKey $ vKey poolKey
          }

      eraElaboratorState . eesKeys . at mAddr .= Just k
      pure k

getKeyPairFor :: forall m era st proxy.
  ( MonadState st m , HasEraElaboratorState st era , C.Crypto (Crypto era))
  => proxy era -> ModelAddress
  -> m (KeyPair' (Crypto era))
getKeyPairFor proxy mAddr = _tkpKeyPair <$> getKeyPairForImpl proxy mAddr

getAddrFor
  :: forall m era st proxy.
  ( MonadState st m , HasEraElaboratorState st era , C.Crypto (Crypto era))
  => proxy era -> ModelAddress
  -> m (Addr (Crypto era))
getAddrFor proxy mAddr = _tkpAddr <$> getKeyPairForImpl proxy mAddr

getStakePoolFor
  :: forall m era st proxy.
  ( MonadState st m , HasEraElaboratorState st era , C.Crypto (Crypto era))
  => proxy era -> ModelAddress
  -> m (KeyHash 'StakePool (Crypto era))
getStakePoolFor proxy maddr = _tkpStakePool <$> getKeyPairForImpl proxy maddr

getStakeCredenetialFor
  :: forall m era st proxy.
  ( MonadState st m , HasEraElaboratorState st era , C.Crypto (Crypto era))
  => proxy era -> ModelAddress
  -> m (StakeCredential (Crypto era))
getStakeCredenetialFor proxy maddr = _tpkStakeCredential <$> getKeyPairForImpl proxy maddr

getHashKeyVRFFor
  :: forall m era st proxy.
  ( MonadState st m
  , HasEraElaboratorState st era
  , C.Crypto (Crypto era)
  )
  => proxy era -> ModelAddress
  -> m (Hash.Hash (C.HASH (Crypto era)) (Cardano.Crypto.VRF.Class.VerKeyVRF (C.VRF (Crypto era))))
getHashKeyVRFFor proxy maddr = _tkpVRFHash <$> getKeyPairForImpl proxy maddr


instance Default (EraElaboratorState era) where
  def = EraElaboratorState
    { _eesUnusedKeyPairs = 1
    , _eesKeys = Map.empty
    , _eesTxIds = Map.empty
    , _eesCurrentEpoch = 1
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
  addr <- getAddrFor (Proxy :: Proxy era) mAddr
  pure (makeTxOut (Proxy :: Proxy era) addr (Val.inject $ Coin mValue))

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
    pure . maybe Set.empty Set.singleton $ Shelley.TxIn <$> Map.lookup mtxId (_eesTxIds ses) <*> pure idx
  ModelGensisIn mAddr -> do
    myKeys <- getAddrFor (Proxy :: Proxy era) mAddr
    pure $ Set.singleton $ initialFundsPseudoTxIn $ myKeys


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
  , _mtxDCert :: ![ModelDCert]
  -- , _mtxWdrls :: !(Map.Map ModelAddress Coin)
  }

data ModelBlock = ModelBlock SlotNo [ModelTx]
data ModelBlocksMade = ModelBlocksMade (Map.Map ModelAddress Natural)
data ModelEpoch = ModelEpoch [ModelBlock] ModelBlocksMade

data ModelDelegation = ModelDelegation
  { _mdDelegator :: !ModelAddress
  , _mdDelegatee :: !ModelAddress
  }

data ModelPoolParams = ModelPoolParams
  { _mppId :: !ModelAddress
  , _mppPledge :: !Coin
  , _mppCost :: !Coin
  , _mppMargin :: !UnitInterval
  , _mppRAcnt :: !ModelAddress
  }

-- ignores genesis delegation details.
data ModelDCert
   = ModelRegisterStake ModelAddress
   | ModelDeRegisterStake ModelAddress
   | ModelDelegate ModelDelegation
   | ModelRegisterPool ModelPoolParams
   | ModelRetirePool ModelAddress EpochNo
   -- TODO: MIR

instance Semigroup ModelBlocksMade where
  ModelBlocksMade x <> ModelBlocksMade y = ModelBlocksMade $ Map.unionWith (+) x y
instance Monoid ModelBlocksMade where
  mempty = ModelBlocksMade Map.empty


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
    -> ( Either (ApplyTxError era) ()
       , (NewEpochState era, ElaborateEraModelState era)
       )

  default elaborateBlock
    ::
    ( ApplyBlock era
    , ApplyTx era
    , HasEraElaboratorState (ElaborateEraModelState era) era
    )
    => Globals
    -> ModelBlock
    -> (NewEpochState era, ElaborateEraModelState era)
    -> ( Either (ApplyTxError era) ()
       , (NewEpochState era, ElaborateEraModelState era)
       )
  elaborateBlock globals = State.runState . Except.runExceptT . \case
    ModelBlock mslot mtxSeq -> do
      currentEpoch <- use $ _2 . eraElaboratorState . eesCurrentEpoch
      let
        slot = runIdentity (epochInfoFirst ei currentEpoch) + mslot
        ei = epochInfo globals
        ttl = succ slot

      unless (currentEpoch == runIdentity (epochInfoEpoch ei slot)) $ error $ "model slot out of range: " <> show mslot
      -- tick the model
      lift $ zoom _1 $ State.state $ \nes0 -> ((), applyTick globals nes0 slot)
      txSeq <- lift $ zoom _2 $ for mtxSeq $ \tx -> State.state $ makeTx (Proxy :: Proxy era) globals ttl tx
      mempoolEnv <- (\(nes0, _) -> mkMempoolEnv nes0 slot) <$> get

      -- apply the transactions.
      for_ txSeq $ \tx -> do
        (nes0, ems) <- get
        mps' <- applyTxInBlock globals mempoolEnv (view mempoolState nes0) tx
        let nes1 = set mempoolState mps' nes0
        put (nes1, ems)

      -- pure (slot, txSeq)
      pure ()

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
      addr <- getAddrFor (Proxy :: Proxy era) mAddr
      pure (addr, coins)

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

  elaborateBlocksMade
    :: Globals
    -> ModelBlocksMade
    -> ( NewEpochState era, ElaborateEraModelState era)
    -> ( BlocksMade (Crypto era)
       , (NewEpochState era, ElaborateEraModelState era)
       )

  default elaborateBlocksMade ::
    ( HasEraElaboratorState (ElaborateEraModelState era) era
    , C.Crypto (Crypto era)
    , ApplyBlock era
    )
    => Globals
    -> ModelBlocksMade
    -> (NewEpochState era, ElaborateEraModelState era)
    -> ( BlocksMade (Crypto era)
       , (NewEpochState era, ElaborateEraModelState era)
       )
  elaborateBlocksMade globals (ModelBlocksMade mblocksMade) = State.runState $ do
    bs <- for (Map.toList mblocksMade) $ \(maddr, n) -> do
      poolKey <- zoom _2 $ getStakePoolFor (Proxy :: Proxy era) maddr
      pure (poolKey, n)
    epoch <- _2 . eraElaboratorState . eesCurrentEpoch <%= succ
    let
      bs' = BlocksMade $ Map.fromList bs
      ei = epochInfo globals
      slot = runIdentity $ epochInfoFirst ei epoch
    _1 %= emulateBlocksMade bs'
    zoom _1 $ State.state $ \nes0 -> ((), applyTick globals nes0 slot)

    pure bs'


  elaborateDCert
    :: proxy era
    -> ModelDCert
    -> ElaborateEraModelState era
    -> (DCert (Crypto era), ElaborateEraModelState era)

  default elaborateDCert ::
    ( HasEraElaboratorState (ElaborateEraModelState era) era
    , C.Crypto (Crypto era)
    )
    => proxy era
    -> ModelDCert
    -> ElaborateEraModelState era
    -> (DCert (Crypto era), ElaborateEraModelState era)
  elaborateDCert proxy = State.runState . \case
    ModelRegisterStake maddr -> DCertDeleg . RegKey
      <$> getStakeCredenetialFor proxy maddr
    ModelDeRegisterStake maddr -> DCertDeleg . DeRegKey
      <$> getStakeCredenetialFor proxy maddr
    ModelDelegate (ModelDelegation mdelegator mdelegatee) ->
      fmap (DCertDeleg . Delegate) $ Delegation
        <$> getStakeCredenetialFor proxy mdelegator
        <*> getStakePoolFor proxy mdelegatee
    ModelRegisterPool (ModelPoolParams poolId pledge cost margin rAcnt) ->
      fmap (DCertPool . RegPool) $ PoolParams
        <$> getStakePoolFor proxy poolId
        <*> getHashKeyVRFFor proxy poolId
        <*> pure pledge
        <*> pure cost
        <*> pure margin
        <*> (RewardAcnt Testnet <$> getStakeCredenetialFor proxy rAcnt)
        <*> pure Set.empty
        <*> pure StrictSeq.empty
        <*> pure SNothing
    ModelRetirePool maddr epochNo -> do
      fmap DCertPool $ RetirePool
        <$> getStakePoolFor proxy maddr
        <*> pure epochNo

mkDCerts :: forall m era proxy.
  ( MonadState (ElaborateEraModelState era) m
  , ElaborateEraModel era
  , C.Crypto (Crypto era)
  )
  => proxy era
  -> ModelDCert
  -> m (DCert (Crypto era))
mkDCerts proxy x = State.state $ elaborateDCert proxy x

-- | simulate blocks made in the current epoch.  this functions like ApplyBlock or
-- ApplyTx, but without presenting real blocks to the ledger.  This is only
-- useful in testing the correctness of specific aspects of the ledger rather
-- than in normal use.
emulateBlocksMade
  :: forall era.
     BlocksMade (Crypto era)
  -> NewEpochState era
  -> NewEpochState era
emulateBlocksMade (BlocksMade newBlocksMade) nes@(LedgerState.NewEpochState {LedgerState.nesBcur = BlocksMade currentBlocksMade })
  = nes {LedgerState.nesBcur = BlocksMade (Map.unionWith (+) newBlocksMade currentBlocksMade)}

type ModelChainInteraction = ModelEpoch

elaborateBlocks_
  :: forall era.
  ( ElaborateEraModel era )
  => Globals
  -> [ModelChainInteraction]
  -> (NewEpochState era, ElaborateEraModelState era)
  -> ( Either (ApplyTxError era) ()
     , (NewEpochState era, ElaborateEraModelState era)
     )
elaborateBlocks_ globals = State.runState . Except.runExceptT . traverse_ f
  where
    f (ModelEpoch blocks blocksMade) = do
      for_ blocks (Except.ExceptT . State.state . elaborateBlock globals)
      _ <- lift $ State.state $ elaborateBlocksMade globals blocksMade
      pure ()


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

