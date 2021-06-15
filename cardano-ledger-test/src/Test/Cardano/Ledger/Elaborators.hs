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
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Test.Cardano.Ledger.Elaborators where

import qualified Cardano.Crypto.DSIGN.Class as DSIGN
import qualified Cardano.Crypto.Hash.Class as Hash
-- TODO use CPS'ed writer

import qualified Cardano.Crypto.VRF.Class (VerKeyVRF)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as C
import Cardano.Ledger.Era
import qualified Cardano.Ledger.Era as Era
import Cardano.Ledger.Hashes (ScriptHash)
import Cardano.Ledger.Keys
import Cardano.Ledger.Mary.Value (PolicyID (..))
import Cardano.Ledger.SafeHash (HashAnnotated, SafeHash, hashAnnotated)
import Cardano.Ledger.Shelley.Constraints
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock)
import qualified Cardano.Ledger.Val as Val
import Cardano.Slotting.EpochInfo.API
import Cardano.Slotting.Slot hiding (at)
import Control.Lens
import Control.Monad
import qualified Control.Monad.Except as Except
import Control.Monad.State (MonadState (..))
import qualified Control.Monad.State.Class as State
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as State hiding (get, state)
import qualified Control.Monad.Writer as Writer
import Data.Default.Class
import Data.Foldable
import Data.Functor.Compose
import qualified Data.Map as Map
import Data.Proxy
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Traversable
import Data.Void
import Data.Word (Word64)
import Numeric.Natural
import Shelley.Spec.Ledger.API.Genesis
import Shelley.Spec.Ledger.API.Mempool
import Shelley.Spec.Ledger.API.Validation
import Shelley.Spec.Ledger.Address
import Shelley.Spec.Ledger.Credential
  ( Credential (..),
    StakeCredential,
  )
import Shelley.Spec.Ledger.EpochBoundary (BlocksMade (..))
import Shelley.Spec.Ledger.Genesis
import Shelley.Spec.Ledger.LedgerState (NewEpochState)
import qualified Shelley.Spec.Ledger.LedgerState as LedgerState
import Shelley.Spec.Ledger.STS.EraMapping ()
import qualified Shelley.Spec.Ledger.Tx as Shelley
import Shelley.Spec.Ledger.TxBody
  ( DCert (..),
    DelegCert (..),
    Delegation (..),
    PoolCert (..),
    PoolParams (..),
  )
import qualified Shelley.Spec.Ledger.TxBody as Shelley
import qualified Shelley.Spec.Ledger.UTxO as UTxO
import Test.Cardano.Ledger.ModelChain
import Test.Cardano.Ledger.ModelChain.Address
import Test.Cardano.Ledger.ModelChain.Script
import Test.Cardano.Ledger.ModelChain.Value
import Test.Shelley.Spec.Ledger.Generator.ScriptClass (mkKeyPairs)
import Test.Shelley.Spec.Ledger.Utils (RawSeed (..), mkVRFKeyPair)
import qualified Text.Show as Show

data ExpectedValueTypeC era where
  ExpectedValueTypeC_Simple ::
    ( Core.Value era ~ Coin,
      EraValueFeature era ~ 'ExpectAdaOnly
    ) =>
    ExpectedValueTypeC era
  ExpectedValueTypeC_MA ::
    ( ValueFromList (Core.Value era) (Crypto era),
      EraValueFeature era ~ 'ExpectAnyOutput,
      ValidateScript era
    ) =>
    ExpectedValueTypeC era

-- | both keypairs used for an address
type AddressKeyPair crypto = (KeyPair 'Payment crypto, KeyPair 'Staking crypto)

newtype ElaboratedScriptsCache era = ElaboratedScriptsCache
  {unElaboratedScriptsCache :: Map.Map (ModelScript (EraScriptFeature era)) (ScriptHash (Crypto era), Core.Script era)}

instance Show (ElaboratedScriptsCache era) where
  showsPrec n (ElaboratedScriptsCache xs) =
    Show.showParen (n >= 11) $
      Show.showString "ElaboratedScriptsCache "
        . showsPrec 11 (Map.keysSet xs)

data EraElaboratorState era = EraElaboratorState
  { _eesUnusedKeyPairs :: Word64,
    _eesKeys :: Map.Map ModelAddress (TestKeyPair (Crypto era)),
    _eesTxIds :: Map.Map ModelTxId (Shelley.TxId (Crypto era)),
    _eesCurrentEpoch :: EpochNo,
    _eesUTxOs :: Map.Map ModelUTxOId ModelAddress,
    _eesPendingWitnessKeys :: [KeyPair 'Witness (Crypto era)],
    _eesCurrentSlot :: SlotNo,
    _eesScripts :: ElaboratedScriptsCache era
  }

deriving instance
  (C.Crypto (Crypto era)) =>
  Show (EraElaboratorState era)

eesUnusedKeyPairs :: Functor f => (Word64 -> f Word64) -> EraElaboratorState era -> f (EraElaboratorState era)
eesUnusedKeyPairs a2fb s = (\b -> s {_eesUnusedKeyPairs = b}) <$> a2fb (_eesUnusedKeyPairs s)

eesKeys :: Functor f => (Map.Map ModelAddress (TestKeyPair (Crypto era)) -> f (Map.Map ModelAddress (TestKeyPair (Crypto era)))) -> EraElaboratorState era -> f (EraElaboratorState era)
eesKeys a2fb s = (\b -> s {_eesKeys = b}) <$> a2fb (_eesKeys s)

eesTxIds :: Functor f => (Map.Map ModelTxId (Shelley.TxId (Crypto era)) -> f (Map.Map ModelTxId (Shelley.TxId (Crypto era)))) -> EraElaboratorState era -> f (EraElaboratorState era)
eesTxIds a2fb s = (\b -> s {_eesTxIds = b}) <$> a2fb (_eesTxIds s)

eesCurrentEpoch :: Functor f => (EpochNo -> f EpochNo) -> EraElaboratorState era -> f (EraElaboratorState era)
eesCurrentEpoch a2fb s = (\b -> s {_eesCurrentEpoch = b}) <$> a2fb (_eesCurrentEpoch s)

eesPendingWitnessKeys :: Functor f => ([KeyPair 'Witness (Crypto era)] -> f [KeyPair 'Witness (Crypto era)]) -> EraElaboratorState era -> f (EraElaboratorState era)
eesPendingWitnessKeys a2fb s = (\b -> s {_eesPendingWitnessKeys = b}) <$> a2fb (_eesPendingWitnessKeys s)

eesUTxOs :: Lens' (EraElaboratorState era) (Map.Map ModelUTxOId ModelAddress)
eesUTxOs a2fb s = (\b -> s {_eesUTxOs = b}) <$> a2fb (_eesUTxOs s)

eesCurrentSlot :: Lens' (EraElaboratorState era) SlotNo
eesCurrentSlot a2fb s = (\b -> s {_eesCurrentSlot = b}) <$> a2fb (_eesCurrentSlot s)

eesScripts :: Lens' (EraElaboratorState era) (Map.Map (ModelScript (EraScriptFeature era)) (ScriptHash (Crypto era), Core.Script era))
eesScripts a2fb s = (\b -> s {_eesScripts = ElaboratedScriptsCache b}) <$> a2fb (unElaboratedScriptsCache $ _eesScripts s)

-- various derived values for an address to avoid recomputing them
data TestKeyPair crypto = TestKeyPair
  { _tkpKeyPair :: AddressKeyPair crypto,
    _tkpVRF :: (SignKeyVRF crypto, VerKeyVRF crypto),
    _tkpAddr :: Addr crypto,
    _tkpVRFHash :: Hash.Hash (C.HASH crypto) (Cardano.Crypto.VRF.Class.VerKeyVRF (C.VRF crypto)),
    _tkpPayment :: KeyHash 'Payment crypto,
    _tkpStakePool :: KeyHash 'Staking crypto,
    _tkpStakeCredential :: StakeCredential crypto
  }

deriving instance C.Crypto crypto => Show (TestKeyPair crypto)

-- | get the TestKeyPair for a ModelAddress
getKeyPairForImpl ::
  forall m era proxy.
  ( MonadState (EraElaboratorState era) m,
    C.Crypto (Crypto era)
  ) =>
  proxy era ->
  ModelAddress ->
  m (TestKeyPair (Crypto era))
getKeyPairForImpl _ mAddr = do
  st <- use id
  case Map.lookup mAddr (_eesKeys st) of
    Just k -> pure k
    Nothing -> do
      unusedKeyPairId <- eesUnusedKeyPairs <<%= succ

      let keyPair@(pmtKey, poolKey) = mkKeyPairs @(Crypto era) unusedKeyPairId
          vrf@(_, vrf') = mkVRFKeyPair @(C.VRF (Crypto era)) (RawSeed 1 0 0 0 unusedKeyPairId)
          k =
            TestKeyPair
              { _tkpKeyPair = keyPair,
                _tkpVRF = vrf,
                _tkpAddr = toAddr Testnet keyPair, -- TODO: network can be read from Globals
                _tkpVRFHash = hashVerKeyVRF vrf',
                _tkpPayment = hashKey $ vKey pmtKey,
                _tkpStakePool = hashKey $ vKey poolKey,
                _tkpStakeCredential = KeyHashObj . hashKey $ vKey poolKey
              }

      eesKeys . at mAddr .= Just k
      pure k

-- | get the AddressKeyPair for a ModelAddress
getKeyPairFor ::
  forall m era proxy.
  (MonadState (EraElaboratorState era) m, C.Crypto (Crypto era)) =>
  proxy era ->
  ModelAddress ->
  m (AddressKeyPair (Crypto era))
getKeyPairFor proxy mAddr = _tkpKeyPair <$> getKeyPairForImpl proxy mAddr

-- | get the Addr for a ModelAddress
getAddrFor ::
  forall m era proxy.
  (MonadState (EraElaboratorState era) m, C.Crypto (Crypto era)) =>
  proxy era ->
  ModelAddress ->
  m (Addr (Crypto era))
getAddrFor proxy mAddr = _tkpAddr <$> getKeyPairForImpl proxy mAddr

-- | get the Addr for a ModelAddress
getStakingKeyHashFor ::
  forall m era proxy.
  (MonadState (EraElaboratorState era) m, C.Crypto (Crypto era)) =>
  proxy era ->
  ModelAddress ->
  m (KeyHash 'Staking (Crypto era))
getStakingKeyHashFor proxy maddr = _tkpStakePool <$> getKeyPairForImpl proxy maddr

-- | get the StakePool hash for a ModelAddress
getStakePoolFor ::
  forall m era proxy.
  (MonadState (EraElaboratorState era) m, C.Crypto (Crypto era)) =>
  proxy era ->
  ModelAddress ->
  m (KeyHash 'StakePool (Crypto era))
getStakePoolFor proxy maddr = coerceKeyRole @_ @_ @(Crypto era) . _tkpStakePool <$> getKeyPairForImpl proxy maddr

-- | get the StakePool hash for a ModelAddress
getScriptWitnessFor ::
  forall m era proxy.
  (MonadState (EraElaboratorState era) m, C.Crypto (Crypto era)) =>
  proxy era ->
  ModelAddress ->
  m (KeyHash 'Witness (Crypto era))
getScriptWitnessFor proxy maddr = coerceKeyRole @_ @_ @(Crypto era) . _tkpPayment <$> getKeyPairForImpl proxy maddr

-- | get the StakeCredential for a ModelAddress
getStakeCredenetialFor ::
  forall m era proxy.
  (MonadState (EraElaboratorState era) m, C.Crypto (Crypto era)) =>
  proxy era ->
  ModelAddress ->
  m (StakeCredential (Crypto era))
getStakeCredenetialFor proxy maddr = _tkpStakeCredential <$> getKeyPairForImpl proxy maddr

-- | get the VRFHash for a ModelAddress
getHashKeyVRFFor ::
  forall m era proxy.
  ( MonadState (EraElaboratorState era) m,
    C.Crypto (Crypto era)
  ) =>
  proxy era ->
  ModelAddress ->
  m (Hash.Hash (C.HASH (Crypto era)) (Cardano.Crypto.VRF.Class.VerKeyVRF (C.VRF (Crypto era))))
getHashKeyVRFFor proxy maddr = _tkpVRFHash <$> getKeyPairForImpl proxy maddr

instance Default (EraElaboratorState era) where
  def =
    EraElaboratorState
      { _eesUnusedKeyPairs = 1,
        _eesKeys = Map.empty,
        _eesTxIds = Map.empty,
        _eesCurrentEpoch = 0,
        _eesCurrentSlot = 0,
        _eesPendingWitnessKeys = [],
        _eesUTxOs = Map.empty,
        _eesScripts = ElaboratedScriptsCache Map.empty
      }

popScriptWitnesses ::
  ( MonadState (NewEpochState era, EraElaboratorState era) m,
    ElaborateEraModel era
  ) =>
  m (Map.Map (ScriptHash (Crypto era)) (Core.Script era))
popScriptWitnesses = Map.fromList . toList <$> (_2 . eesScripts <<.= Map.empty)

pushScriptWitness ::
  ( MonadState (NewEpochState era, EraElaboratorState era) m,
    ElaborateEraModel era,
    C.Crypto (Crypto era)
  ) =>
  proxy era ->
  ModelScript (EraScriptFeature era) ->
  ScriptHash (Crypto era) ->
  Core.Script era ->
  m ()
pushScriptWitness _ modelPolicy policyHash realPolicy = do
  State.state $
    State.runState $
      zoom _2 $
        for_ (modelScriptNeededSigs modelPolicy) $ \mAddr -> do
          (myKeys, _) <- getKeyPairFor (Proxy :: Proxy era) mAddr
          pushWitness myKeys
  _2 . eesScripts . at modelPolicy .= Just (policyHash, realPolicy)

noScriptAction ::
  Applicative m =>
  proxy era ->
  ModelScript (EraScriptFeature era) ->
  ScriptHash (Crypto era) ->
  Core.Script era ->
  m ()
noScriptAction _ _ _ _ = pure ()

lookupModelValue ::
  forall era valF m.
  ( Val.Val (ElaborateValueType valF (Crypto era)),
    MonadState (NewEpochState era, EraElaboratorState era) m,
    ElaborateEraModel era
  ) =>
  (Proxy era -> ModelScript (EraScriptFeature era) -> ScriptHash (Crypto era) -> Core.Script era -> m ()) ->
  ModelValueVars (EraFeatureSet era) valF ->
  m (ElaborateValueType valF (Crypto era))
lookupModelValue scriptAction = \case
  ModelValue_Reward maddr -> do
    allRewards <- observeRewards <$> State.get
    pure $ Val.inject $ maybe (Coin 0) id $ Map.lookup maddr allRewards
  ModelValue_MA (Coin ad) x -> case reifyValueConstraint @era of
    ExpectedValueTypeC_MA -> do
      x' <- Writer.execWriterT $
        ifor_ (Compose x) $ \(modelPolicy, assetName) qty -> do
          realPolicy :: Core.Script era <- State.state $ elaborateScript modelPolicy
          let policyHash = hashScript @era realPolicy
          lift $ scriptAction (Proxy @era) modelPolicy policyHash realPolicy

          Writer.tell $ [(PolicyID policyHash, assetName, qty)]
      pure $ valueFromList ad $ x'

mkTxOut ::
  forall m era.
  ( MonadState (NewEpochState era, EraElaboratorState era) m,
    Except.MonadError (ElaborateBlockError era) m,
    Era era,
    UsesTxOut era,
    ElaborateEraModel era
  ) =>
  ModelUTxOId ->
  ModelTxOut (EraFeatureSet era) ->
  m (Core.TxOut era)
mkTxOut mutxoId (ModelTxOut mAddr (ModelValue mValue)) = (=<<) Except.liftEither $
  State.state $
    State.runState $
      Except.runExceptT $ do
        _2 . eesUTxOs . at mutxoId .= Just mAddr
        addr <- zoom _2 $ getAddrFor (Proxy :: Proxy era) mAddr
        val :: Core.Value era <- either (Except.throwError . ElaborateBlockError_TxValue @era) pure =<< evalModelValue (lookupModelValue noScriptAction) mValue
        pure (makeTxOut (Proxy :: Proxy era) addr val)

mkTxIn ::
  forall m era.
  ( MonadState (EraElaboratorState era) m,
    C.Crypto (Crypto era)
  ) =>
  ModelTxIn ->
  m (Set.Set (Shelley.TxIn (Crypto era)))
mkTxIn = \case
  -- TODO: handle missing txnIds more gracefully?
  ModelTxIn mtxId idx -> do
    ses <- use id
    ownerMAddr <- use $ eesUTxOs . at (ModelUTxOId mtxId idx)
    for_ ownerMAddr $ \mAddr -> do
      (myKeys, _) <- getKeyPairFor (Proxy :: Proxy era) mAddr
      pushWitness myKeys
    pure . maybe Set.empty Set.singleton $ Shelley.TxIn <$> Map.lookup mtxId (_eesTxIds ses) <*> pure idx
  ModelGensisIn mAddr -> do
    myAddr <- getAddrFor (Proxy :: Proxy era) mAddr
    (myKeys, _) <- getKeyPairFor (Proxy :: Proxy era) mAddr
    pushWitness myKeys
    pure $ Set.singleton $ initialFundsPseudoTxIn $ myAddr

-- | accumulate a witness while elaborating a ModelTx.
pushWitness ::
  forall kr m era.
  ( MonadState (EraElaboratorState era) m
  ) =>
  KeyPair kr (Crypto era) ->
  m ()
pushWitness keyP = eesPendingWitnessKeys %= (:) (coerceKeyRole keyP)

-- | return all accumulated witnesses.
popWitnesses ::
  forall proxy m era.
  ( MonadState (EraElaboratorState era) m,
    DSIGN.Signable (C.DSIGN (Crypto era)) (Hash.Hash (C.HASH (Crypto era)) Shelley.EraIndependentTxBody),
    C.Crypto (Crypto era)
  ) =>
  proxy era ->
  SafeHash (Crypto era) Shelley.EraIndependentTxBody ->
  m (Set.Set (Shelley.WitVKey 'Witness (Crypto era)))
popWitnesses _ bodyHash = do
  witness <- eesPendingWitnessKeys <<.= []
  pure $
    flip foldMap witness $ \keyP ->
      Set.singleton $ UTxO.makeWitnessVKey bodyHash keyP

-- | lens to focus the ledger state from that used by ApplyBlock to that used by
-- ApplyTx
mempoolState :: Functor f => (MempoolState era -> f (MempoolState era)) -> (NewEpochState era -> f (NewEpochState era))
mempoolState = \a2b s ->
  let nesEs = LedgerState.nesEs s
      esLState = LedgerState.esLState nesEs

      mkNES (utxoState, delegationState) =
        s
          { LedgerState.nesEs =
              nesEs
                { LedgerState.esLState =
                    esLState
                      { LedgerState._utxoState = utxoState,
                        LedgerState._delegationState = delegationState
                      }
                }
          }
   in mkNES <$> a2b (mkMempoolState s)
{-# INLINE mempoolState #-}

data ElaborateBlockError era
  = ElaborateBlockError_ApplyTx (ApplyTxError era)
  | ElaborateBlockError_Fee (ModelValueError Coin)
  | ElaborateBlockError_TxValue (ModelValueError (Core.Value era))

deriving instance
  ( Show (ApplyTxError era),
    Show (Core.Value era)
  ) =>
  Show (ElaborateBlockError era)

data TxBodyArguments era = TxBodyArguments
  { -- | ttl
    _txBodyArguments_ttl :: !SlotNo,
    -- | fee
    _txBodyArguments_fee :: !Coin,
    -- | inputs
    _txBodyArguments_inputs :: !(Set.Set (Shelley.TxIn (Crypto era))),
    -- | outputs
    _txBodyArguments_outputs :: !(StrictSeq.StrictSeq (Core.TxOut era)),
    -- | Deleg certs.
    _txBodyArguments_delegCerts :: !(StrictSeq.StrictSeq (DCert (Crypto era))),
    -- | withdrawals
    _txBodyArguments_withdrawals :: !(Shelley.Wdrl (Crypto era)),
    -- | mint
    _txBodyArguments_mint :: !(IfSupportsMint () (Core.Value era) (EraValueFeature era))
  }

data TxWitnessArguments era = TxWitnessArguments
  { _txWitnessArguments_vkey :: !(Set.Set (Shelley.WitVKey 'Witness (Crypto era))),
    _txWitnessArguments_scripts ::
      !( IfSupportsScript
           ()
           (Map.Map (ScriptHash (Crypto era)) (Core.Script era))
           (EraScriptFeature era)
       )
  }

type EraValueFeature era = ValueFeature (EraFeatureSet era)

type EraScriptFeature era = ScriptFeature (EraFeatureSet era)

class
  ( ElaborateValueType (EraValueFeature era) (Crypto era) ~ Core.Value era,
    KnownRequiredFeatures (EraFeatureSet era)
  ) =>
  ElaborateEraModel era
  where
  type EraFeatureSet era :: FeatureSet
  eraFeatureSet :: proxy era -> FeatureTag (EraFeatureSet era)

  -- type ValueFeature era :: TyValueExpected
  -- type ScriptFeature era :: TyScriptFeature

  reifyValueConstraint ::
    ExpectedValueTypeC era

  -- | Apply a ModelBlock to a specific era's ledger.
  elaborateBlock ::
    Globals ->
    ModelBlock (EraFeatureSet era) ->
    (NewEpochState era, EraElaboratorState era) ->
    ( Either (ElaborateBlockError era) (),
      (NewEpochState era, EraElaboratorState era)
    )
  default elaborateBlock ::
    ( ApplyBlock era,
      ApplyTx era
    ) =>
    Globals ->
    ModelBlock (EraFeatureSet era) ->
    (NewEpochState era, EraElaboratorState era) ->
    ( Either (ElaborateBlockError era) (),
      (NewEpochState era, EraElaboratorState era)
    )
  elaborateBlock globals =
    State.runState . Except.runExceptT . \case
      ModelBlock mslot mtxSeq -> do
        currentEpoch <- use $ _2 . eesCurrentEpoch
        let slot = runIdentity (epochInfoFirst ei currentEpoch) + mslot
            ei = epochInfo globals
            ttl = succ slot

        unless (currentEpoch == runIdentity (epochInfoEpoch ei slot)) $ error $ "model slot out of range: " <> show mslot
        _2 . eesCurrentSlot .= slot
        -- tick the model
        lift $ zoom _1 $ State.state $ \nes0 -> ((), applyTick globals nes0 slot)
        txSeq ::
          [Era.TxInBlock era] <-
          for mtxSeq $ \tx -> Except.ExceptT $ State.state $ elaborateTx (Proxy :: Proxy era) globals ttl tx
        mempoolEnv <- (\(nes0, _) -> mkMempoolEnv nes0 slot) <$> get

        -- apply the transactions.
        for_ txSeq $ \tx -> do
          (nes0, ems) <- get
          (mps', _) <- liftApplyTxError $ applyTx globals mempoolEnv (view mempoolState nes0) $ extractTx tx
          -- mps' <- liftApplyTxError $ applyTxInBlock globals mempoolEnv (view mempoolState nes0) tx
          let nes1 = set mempoolState mps' nes0
          put (nes1, ems)

        -- pure (slot, txSeq)
        pure ()

  -- | get a NewEpochState from genesis conditions.
  elaborateInitialState ::
    ShelleyGenesis era ->
    AdditionalGenesisConfig era ->
    -- | Initial coins present at time of genesis.  Each address will have its full balance in a single UTxO from a single "transaction"
    Map.Map ModelAddress Coin ->
    EraElaboratorState era ->
    ( NewEpochState era,
      EraElaboratorState era
    )
  default elaborateInitialState ::
    ( C.Crypto (Crypto era),
      CanStartFromGenesis era
    ) =>
    ShelleyGenesis era ->
    AdditionalGenesisConfig era ->
    Map.Map ModelAddress Coin ->
    EraElaboratorState era ->
    ( NewEpochState era,
      EraElaboratorState era
    )
  elaborateInitialState sg additionalGenesesConfig genesisAccounts = State.runState $ do
    utxo0 <- fmap Map.fromList $
      for (Map.toList genesisAccounts) $ \(mAddr, coins) -> do
        addr <- getAddrFor (Proxy :: Proxy era) mAddr
        pure (addr, coins)

    pure $ initialState sg {sgInitialFunds = Map.unionWith const utxo0 $ sgInitialFunds sg} additionalGenesesConfig

  makeTimelockScript ::
    proxy era ->
    IfSupportsTimelock (Timelock (Crypto era)) (EraScriptFeature era) ->
    Core.Script era
  default makeTimelockScript ::
    (IfSupportsTimelock (Timelock (Crypto era)) (EraScriptFeature era) ~ Void) =>
    proxy era ->
    IfSupportsTimelock (Timelock (Crypto era)) (EraScriptFeature era) ->
    Core.Script era
  makeTimelockScript _ = absurd

  elaborateScript ::
    ModelScript (EraScriptFeature era) ->
    (NewEpochState era, EraElaboratorState era) ->
    ( (Core.Script era),
      (NewEpochState era, EraElaboratorState era)
    )
  default elaborateScript ::
    (C.Crypto (Crypto era)) =>
    ModelScript (EraScriptFeature era) ->
    (NewEpochState era, EraElaboratorState era) ->
    ( (Core.Script era),
      (NewEpochState era, EraElaboratorState era)
    )
  elaborateScript ms0 = State.runState $ case ms0 of
    ModelScript_Timelock ms -> do
      x <- elaborateModelTimelock (zoom _2 . getScriptWitnessFor (Proxy :: Proxy era)) ms
      pure $ makeTimelockScript (Proxy :: Proxy era) x

  -- | Construct a TxBody from some elaborated inputs.
  makeTxBody ::
    TxBodyArguments era ->
    Core.TxBody era

  -- | apply a single Model transaction to a specific eras ledger
  elaborateTx ::
    proxy era ->
    Globals ->
    -- | ttl
    SlotNo ->
    ModelTx (EraFeatureSet era) ->
    (NewEpochState era, EraElaboratorState era) ->
    ( Either (ElaborateBlockError era) (Era.TxInBlock era),
      (NewEpochState era, EraElaboratorState era)
    )
  default elaborateTx ::
    ( DSIGN.Signable (C.DSIGN (Crypto era)) (Hash.Hash (C.HASH (Crypto era)) Shelley.EraIndependentTxBody),
      Hash.HashAlgorithm (C.HASH (Crypto era)),
      HashAnnotated (Core.TxBody era) Shelley.EraIndependentTxBody (Crypto era),
      C.Crypto (Crypto era),
      UsesTxOut era
    ) =>
    proxy era ->
    Globals ->
    SlotNo ->
    ModelTx (EraFeatureSet era) ->
    (NewEpochState era, EraElaboratorState era) ->
    ( Either (ElaborateBlockError era) (Era.TxInBlock era),
      (NewEpochState era, EraElaboratorState era)
    )
  elaborateTx proxy _ maxTTL (ModelTx mtxId mtxInputs mtxOutputs (ModelValue mtxFee) mtxDCert mtxWdrl mtxMint) = State.runState $
    Except.runExceptT $ do
      outs <- ifor mtxOutputs $ \idx -> mkTxOut $ ModelUTxOId mtxId $ toEnum @Natural idx
      ins :: Set.Set (Shelley.TxIn (Crypto era)) <- zoom _2 $ fmap fold $ traverse mkTxIn $ Set.toList mtxInputs
      dcerts <- traverse (zoom _2 . mkDCerts (Proxy :: Proxy era)) mtxDCert
      wdrl <- fmap Map.fromList $
        for (Map.toList mtxWdrl) $ \(mAddr, ModelValue mqty) -> do
          stakeCredential <- zoom _2 $ getStakeCredenetialFor proxy mAddr
          (_, stakeKey) <- zoom _2 $ getKeyPairFor proxy mAddr
          zoom _2 $ pushWitness stakeKey
          qty <- Except.withExceptT ElaborateBlockError_Fee $ Except.liftEither =<< evalModelValue (lookupModelValue noScriptAction) mqty
          pure (RewardAcnt Testnet stakeCredential, qty)
      fee <- Except.withExceptT ElaborateBlockError_Fee $ Except.liftEither =<< evalModelValue (lookupModelValue noScriptAction) mtxFee

      mint ::
        IfSupportsMint () (Core.Value era) (EraValueFeature era) <-
        case reifyValueConstraint @era of
          ExpectedValueTypeC_Simple -> pure $ NoMintSupport ()
          ExpectedValueTypeC_MA -> case mtxMint of
            SupportsMint m' -> fmap SupportsMint $ Except.withExceptT ElaborateBlockError_TxValue $ Except.liftEither =<< evalModelValue (lookupModelValue pushScriptWitness) (unModelValue m')

      let realTxBody =
            makeTxBody @era
              TxBodyArguments
                { _txBodyArguments_ttl = maxTTL,
                  _txBodyArguments_fee = fee,
                  _txBodyArguments_inputs = ins,
                  _txBodyArguments_outputs = StrictSeq.fromList outs,
                  _txBodyArguments_delegCerts = StrictSeq.fromList dcerts,
                  _txBodyArguments_withdrawals = Shelley.Wdrl wdrl,
                  _txBodyArguments_mint = mint
                }
          bodyHash = hashAnnotated realTxBody

      wits <- zoom _2 $ popWitnesses (Proxy :: Proxy era) bodyHash

      scripts1 <- popScriptWitnesses

      scripts ::
        IfSupportsScript () (Map.Map (ScriptHash (Crypto era)) (Core.Script era)) (EraScriptFeature era) <-
        case reifyScriptFeature (Proxy @(EraScriptFeature era)) of
          ScriptFeatureTag_None -> pure $ NoScriptSupport ()
          tag@ScriptFeatureTag_Simple -> pure $ SupportsScript tag scripts1
          tag@ScriptFeatureTag_PlutusV1 -> pure $ SupportsScript tag scripts1

      _2 . eesTxIds . at mtxId .= Just (UTxO.txid @era realTxBody)
      pure $
        makeTx
          proxy
          realTxBody
          TxWitnessArguments
            { _txWitnessArguments_vkey = wits,
              _txWitnessArguments_scripts = scripts
            }

  -- | build a full tx from TxBody and set of witnesses.
  makeTx ::
    proxy era ->
    Core.TxBody era ->
    TxWitnessArguments era ->
    Era.TxInBlock era

  -- | convert an expeced failure by the model to the concrete error that will
  -- be produced.
  toEraPredicateFailure ::
    ModelPredicateFailure (EraFeatureSet era) ->
    (NewEpochState era, EraElaboratorState era) ->
    Either (ModelValueError (Core.Value era)) (ApplyBlockTransitionError era)

  -- | apply the model New Epoch event to a specific eras ledger
  elaborateBlocksMade ::
    Globals ->
    ModelBlocksMade ->
    (NewEpochState era, EraElaboratorState era) ->
    ( BlocksMade (Crypto era),
      (NewEpochState era, EraElaboratorState era)
    )
  default elaborateBlocksMade ::
    ( C.Crypto (Crypto era),
      ApplyBlock era
    ) =>
    Globals ->
    ModelBlocksMade ->
    (NewEpochState era, EraElaboratorState era) ->
    ( BlocksMade (Crypto era),
      (NewEpochState era, EraElaboratorState era)
    )
  elaborateBlocksMade globals (ModelBlocksMade mblocksMade) = State.runState $ do
    bs <- for (Map.toList mblocksMade) $ \(maddr, n) -> do
      poolKey <- zoom _2 $ getStakePoolFor (Proxy :: Proxy era) maddr
      pure (poolKey, n)

    let bs' = BlocksMade $ Map.fromList bs
    _1 %= emulateBlocksMade bs'

    prevEpoch <- use $ _2 . eesCurrentEpoch
    epoch <- _2 . eesCurrentEpoch <%= succ
    let ei = epochInfo globals
        firstOfNew = runIdentity $ epochInfoFirst ei epoch

        neededSlot =
          SlotNo (randomnessStabilisationWindow globals)
            + runIdentity (epochInfoFirst ei prevEpoch)

    currentSlot <- _2 . eesCurrentSlot <<.= firstOfNew

    unless (currentSlot > neededSlot) $
      zoom _1 $ State.state $ \nes0 -> ((), applyTick globals nes0 (neededSlot + 1))
    zoom _1 $ State.state $ \nes0 -> ((), applyTick globals nes0 firstOfNew)

    pure bs'

  -- | convert a model deleg certificate to a real DCert
  elaborateDCert ::
    proxy era ->
    ModelDCert ->
    EraElaboratorState era ->
    (DCert (Crypto era), EraElaboratorState era)
  default elaborateDCert ::
    ( C.Crypto (Crypto era)
    ) =>
    proxy era ->
    ModelDCert ->
    EraElaboratorState era ->
    (DCert (Crypto era), EraElaboratorState era)
  elaborateDCert proxy =
    State.runState . \case
      ModelRegisterStake maddr ->
        DCertDeleg . RegKey
          <$> getStakeCredenetialFor proxy maddr
      ModelDeRegisterStake maddr ->
        DCertDeleg . DeRegKey
          <$> getStakeCredenetialFor proxy maddr
      ModelDelegate (ModelDelegation mdelegator mdelegatee) -> do
        dtor <- getStakeCredenetialFor proxy mdelegator
        dtee <- getStakePoolFor proxy mdelegatee
        (_, stakeKey) <- getKeyPairFor proxy mdelegator
        pushWitness stakeKey
        pure $ (DCertDeleg . Delegate) $ Delegation dtor dtee
      ModelRegisterPool (ModelPoolParams mPoolId pledge cost margin mRAcnt mOwners) -> do
        poolId <- getStakePoolFor proxy mPoolId
        poolVRF <- getHashKeyVRFFor proxy mPoolId
        (_, poolKey) <- getKeyPairFor proxy mPoolId
        poolOwners <- Set.fromList <$> traverse (getStakingKeyHashFor proxy) mOwners
        pushWitness poolKey
        rAcnt <- RewardAcnt Testnet <$> getStakeCredenetialFor proxy mRAcnt
        pure $
          (DCertPool . RegPool) $
            PoolParams
              { _poolId = poolId,
                _poolVrf = poolVRF,
                _poolPledge = pledge,
                _poolCost = cost,
                _poolMargin = margin,
                _poolRAcnt = rAcnt,
                _poolOwners = poolOwners,
                _poolRelays = StrictSeq.empty,
                _poolMD = SNothing
              }
      ModelRetirePool maddr epochNo -> do
        fmap DCertPool $
          RetirePool
            <$> getStakePoolFor proxy maddr
            <*> pure epochNo

-- TODO: maybe useful someday
-- ModelMIRCert srcPot mRewards ->
--   fmap ( DCertMir . Shelley.MIRCert srcPot . Shelley.StakeAddressesMIR . Map.fromList) $
--   for (Map.toList mRewards) $ \(maddr, reward) -> (,)
--     <$> getStakeCredenetialFor proxy maddr
--     <*> pure reward

class AsApplyTxError era e | e -> era where
  asApplyTxError :: Prism' e (ApplyTxError era)

instance AsApplyTxError era (ApplyTxError era) where asApplyTxError = id

instance AsApplyTxError era (ElaborateBlockError era) where
  asApplyTxError = prism ElaborateBlockError_ApplyTx $ \case
    ElaborateBlockError_ApplyTx x -> Right x
    x -> Left x

-- instance AsApplyTxError era (ApplyBlockTransitionError era) where
--   asApplyTxError = prism ApplyBlockTransitionError_Tx $ \case
--     ApplyBlockTransitionError_Tx x -> Right x
--     x -> Left x

liftApplyTxError ::
  (Except.MonadError e m, AsApplyTxError era e) =>
  Except.Except (ApplyTxError era) a ->
  m a
liftApplyTxError = either (Except.throwError . review asApplyTxError) pure . Except.runExcept

mkDCerts ::
  forall m era proxy.
  ( MonadState (EraElaboratorState era) m,
    ElaborateEraModel era
  ) =>
  proxy era ->
  ModelDCert ->
  m (DCert (Crypto era))
mkDCerts proxy x = State.state $ elaborateDCert proxy x

-- | simulate blocks made in the current epoch.  this functions like ApplyBlock or
-- ApplyTx, but without presenting real blocks to the ledger.  This is only
-- useful in testing the correctness of specific aspects of the ledger rather
-- than in normal use.
emulateBlocksMade ::
  forall era.
  BlocksMade (Crypto era) ->
  NewEpochState era ->
  NewEpochState era
emulateBlocksMade (BlocksMade newBlocksMade) nes@(LedgerState.NewEpochState {LedgerState.nesBcur = BlocksMade currentBlocksMade}) =
  nes {LedgerState.nesBcur = BlocksMade (Map.unionWith (+) newBlocksMade currentBlocksMade)}

-- | apply several epochs full of blocks to a ledger
elaborateBlocks_ ::
  forall era.
  (ElaborateEraModel era) =>
  Globals ->
  [ModelEpoch (EraFeatureSet era)] ->
  (NewEpochState era, EraElaboratorState era) ->
  ( Either (ElaborateBlockError era) (),
    (NewEpochState era, EraElaboratorState era)
  )
elaborateBlocks_ globals = State.runState . Except.runExceptT . traverse_ f
  where
    f (ModelEpoch blocks blocksMade) = do
      for_ blocks (Except.ExceptT . State.state . elaborateBlock globals)
      _ :: BlocksMade (Crypto era) <- lift $ State.state $ elaborateBlocksMade globals blocksMade
      pure ()

-- | get the current balance of rewards in terms of the model addresses that can
-- spend them.
observeRewards ::
  forall era.
  (NewEpochState era, EraElaboratorState era) ->
  Map.Map ModelAddress Coin
observeRewards (nes, ems) =
  let creds :: Map.Map (StakeCredential (Crypto era)) ModelAddress
      creds = Map.fromList $ (\(alias, tkp) -> (_tkpStakeCredential tkp, alias)) <$> Map.toList (view eesKeys ems)
   in Map.fromList $ do
        (a, b) <- Map.toList . LedgerState._rewards . LedgerState._dstate . LedgerState._delegationState . LedgerState.esLState $ LedgerState.nesEs nes
        a' <- case Map.lookup a creds of
          Just a' -> pure a'
          Nothing -> error $ "observeRewards: can't find " <> show a
        pure (a', b)

data ApplyBlockTransitionError era
  = ApplyBlockTransitionError_Tx (ApplyTxError era)

deriving instance
  ( Show (ApplyTxError era)
  ) =>
  Show (ApplyBlockTransitionError era)

deriving instance
  ( Eq (ApplyTxError era)
  ) =>
  Eq (ApplyBlockTransitionError era)

deriving instance
  ( Ord (ApplyTxError era)
  ) =>
  Ord (ApplyBlockTransitionError era)
