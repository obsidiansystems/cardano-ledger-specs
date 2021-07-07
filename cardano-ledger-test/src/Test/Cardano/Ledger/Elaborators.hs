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

-- TODO use CPS'ed writer

import qualified Cardano.Crypto.DSIGN.Class as DSIGN
import qualified Cardano.Crypto.Hash.Class as Hash
import qualified Cardano.Crypto.VRF.Class (VerKeyVRF)
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWitness as Alonzo
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
import qualified Control.Monad.Trans.State as State hiding (get, gets, state)
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
import GHC.Records (HasField (..))
import Numeric.Natural
import qualified PlutusTx
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

-- when creating outputs at "genesis", we know the txid's apriori; and recorde
-- them immediately.  otherwise we don't know the txid's until we're done
-- composing the txBody, and need a layer of indirection.
type ModelTxId' c = Either (Shelley.TxIn c) (ModelTxId, Natural)

data EraElaboratorState era = EraElaboratorState
  { _eesUnusedKeyPairs :: Word64,
    _eesKeys :: Map.Map ModelAddress (TestKeyPair (Crypto era)),
    _eesTxIds :: Map.Map ModelTxId (Shelley.TxId (Crypto era)),
    _eesCurrentEpoch :: EpochNo,
    -- _eesUTxOs :: Map.Map ModelUTxOId (ModelTxId' (Crypto era), ModelTxOut (EraFeatureSet era)),
    _eesUTxOs :: Map.Map ModelUTxOId (ModelTxId' (Crypto era), ModelAddress),
    _eesPendingWitnessKeys :: [KeyPair 'Witness (Crypto era)],
    _eesCurrentSlot :: SlotNo,
    _eesScripts :: ElaboratedScriptsCache era,
    _eesRedeemers :: [ModelRedeemer era]
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

-- eesUTxOs :: Lens' (EraElaboratorState era) (Map.Map ModelUTxOId (ModelTxId' (Crypto era), ModelTxOut (EraFeatureSet era)))
eesUTxOs :: Lens' (EraElaboratorState era) (Map.Map ModelUTxOId (ModelTxId' (Crypto era), ModelAddress))
eesUTxOs a2fb s = (\b -> s {_eesUTxOs = b}) <$> a2fb (_eesUTxOs s)

eesCurrentSlot :: Lens' (EraElaboratorState era) SlotNo
eesCurrentSlot a2fb s = (\b -> s {_eesCurrentSlot = b}) <$> a2fb (_eesCurrentSlot s)

eesScripts :: Lens' (EraElaboratorState era) (Map.Map (ModelScript (EraScriptFeature era)) (ScriptHash (Crypto era), Core.Script era))
eesScripts a2fb s = (\b -> s {_eesScripts = ElaboratedScriptsCache b}) <$> a2fb (unElaboratedScriptsCache $ _eesScripts s)

eesRedeemers :: Lens' (EraElaboratorState era) [ModelRedeemer era]
eesRedeemers a2fb s = (\b -> s {_eesRedeemers = b}) <$> a2fb (_eesRedeemers s)

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
        _eesScripts = ElaboratedScriptsCache Map.empty,
        _eesRedeemers = []
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
  () <- case modelPolicy of
    ModelScript_PlutusV1 _s1 ->
      (%=) (_2 . eesRedeemers) $
        (:) $
          ModelRedeemer
            (Alonzo.Minting $ PolicyID policyHash)
            (PlutusTx.I 0)
            (ExUnits 10 10)
    ModelScript_Timelock tl -> for_ (modelScriptNeededSigs tl) $ \mAddr -> State.state $
      State.runState $
        zoom _2 $ do
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
  ModelTxId ->
  Natural ->
  ModelUTxOId ->
  ModelTxOut (EraFeatureSet era) ->
  m (Core.TxOut era)
mkTxOut mtxId idx mutxoId (ModelTxOut mAddr (ModelValue mValue)) = (=<<) Except.liftEither $
  State.state $
    State.runState $
      Except.runExceptT $ do
        _2 . eesUTxOs . at mutxoId .= Just (Right (mtxId, idx), mAddr)
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
mkTxIn moid = do
  ses <- use id
  mutxo <- use $ eesUTxOs . at moid
  case mutxo of
    -- TODO: handle missing txnIds more gracefully?
    Nothing -> pure mempty
    Just (mutxo', mAddr) -> do
      (myKeys, _) <- getKeyPairFor (Proxy :: Proxy era) mAddr
      pushWitness myKeys

      case mutxo' of
        Left txin -> pure (Set.singleton txin)
        Right (mtxId, idx) ->
          pure . maybe Set.empty Set.singleton $ Shelley.TxIn <$> Map.lookup mtxId (_eesTxIds ses) <*> pure idx

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

popRedeemers ::
  MonadState (EraElaboratorState era) m =>
  m [ModelRedeemer era]
popRedeemers = eesRedeemers <<.= []

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
    _txBodyArguments_mint :: !(IfSupportsMint () (Core.Value era) (EraValueFeature era)),
    _txBodyArguments_redeemers :: !(IfSupportsPlutus' () (StrictMaybe (Alonzo.Redeemers era, Alonzo.TxDats era)) (EraScriptFeature era))
  }

data TxWitnessArguments era = TxWitnessArguments
  { _txWitnessArguments_vkey :: !(Set.Set (Shelley.WitVKey 'Witness (Crypto era))),
    _txWitnessArguments_scripts ::
      !( IfSupportsScript
           ()
           (Map.Map (ScriptHash (Crypto era)) (Core.Script era))
           (EraScriptFeature era)
       ),
    _txWitnessArguments_redeemers ::
      !( IfSupportsPlutus'
           ()
           (Alonzo.Redeemers era, Alonzo.TxDats era)
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
          let nes1 = set mempoolState mps' nes0
          put (nes1, ems)

        pure ()

  -- | get a NewEpochState from genesis conditions.
  elaborateInitialState ::
    ShelleyGenesis era ->
    AdditionalGenesisConfig era ->
    -- | Initial coins present at time of genesis.  Each address will have its full balance in a single UTxO from a single "transaction"
    [(ModelUTxOId, ModelAddress, Coin)] ->
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
    [(ModelUTxOId, ModelAddress, Coin)] ->
    EraElaboratorState era ->
    ( NewEpochState era,
      EraElaboratorState era
    )
  elaborateInitialState sg additionalGenesesConfig genesisAccounts = State.runState $ do
    utxo0 <- fmap Map.fromList $
      for genesisAccounts $ \(oid, mAddr, coins) -> do
        addr <- getAddrFor (Proxy :: Proxy era) mAddr
        eesUTxOs . at oid .= Just (Left (initialFundsPseudoTxIn addr), mAddr)
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

  makePlutusScript ::
    proxy era ->
    IfSupportsPlutus (Alonzo.Script era) (EraScriptFeature era) ->
    Core.Script era
  default makePlutusScript ::
    (IfSupportsPlutus (Alonzo.Script era) (EraScriptFeature era) ~ Void) =>
    proxy era ->
    IfSupportsPlutus (Alonzo.Script era) (EraScriptFeature era) ->
    Core.Script era
  makePlutusScript _ = absurd

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
    ModelScript_PlutusV1 ms ->
      pure $ makePlutusScript (Proxy :: Proxy era) (elaborateModelScript ms)
    ModelScript_Timelock ms -> do
      x <- elaborateModelTimelock (zoom _2 . getScriptWitnessFor (Proxy :: Proxy era)) ms
      pure $ makeTimelockScript (Proxy :: Proxy era) x

  -- | Construct a TxBody from some elaborated inputs.
  makeTxBody ::
    NewEpochState era ->
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
  -- TODO: the Core.PParams ~ Alonzo.PParams constraint is somewhat crude, and a
  -- typeclass to get the underlying language view would be a better solution.
  default elaborateTx ::
    ( DSIGN.Signable (C.DSIGN (Crypto era)) (Hash.Hash (C.HASH (Crypto era)) Shelley.EraIndependentTxBody),
      Hash.HashAlgorithm (C.HASH (Crypto era)),
      HashAnnotated (Core.TxBody era) Shelley.EraIndependentTxBody (Crypto era),
      C.Crypto (Crypto era),
      UsesTxOut era,
      HasField "inputs" (Core.TxBody era) (Set.Set (Shelley.TxIn (Crypto era))),
      HasField "wdrls" (Core.TxBody era) (Shelley.Wdrl (Crypto era)),
      HasField "certs" (Core.TxBody era) (StrictSeq.StrictSeq (DCert (Crypto era)))
      -- Core.PParams era ~ Cardano.Ledger.Alonzo.PParams.PParams era
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
      outs <- ifor mtxOutputs $ \idx (oid, o) -> mkTxOut mtxId (toEnum @Natural idx) oid o
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

      scripts1 <- popScriptWitnesses

      scripts ::
        IfSupportsScript () (Map.Map (ScriptHash (Crypto era)) (Core.Script era)) (EraScriptFeature era) <-
        case reifyScriptFeature (Proxy @(EraScriptFeature era)) of
          ScriptFeatureTag_None -> pure $ NoScriptSupport ()
          tag@ScriptFeatureTag_Simple -> pure $ SupportsScript tag scripts1
          tag@ScriptFeatureTag_PlutusV1 -> pure $ SupportsScript tag scripts1

      nes <- State.gets fst
      let txBodyArguments =
            TxBodyArguments
              { _txBodyArguments_ttl = maxTTL,
                _txBodyArguments_fee = fee,
                _txBodyArguments_inputs = ins,
                _txBodyArguments_outputs = StrictSeq.fromList outs,
                _txBodyArguments_delegCerts = StrictSeq.fromList dcerts,
                _txBodyArguments_withdrawals = Shelley.Wdrl wdrl,
                _txBodyArguments_mint = mint,
                _txBodyArguments_redeemers = ifSupportsPlutus (Proxy @(EraScriptFeature era)) () SNothing
              }
          fakeTxBody = makeTxBody @era nes $ txBodyArguments
      redeemers <- case reifySupportsPlutus (Proxy @(EraScriptFeature era)) of
        NoPlutusSupport () -> pure (NoPlutusSupport ())
        SupportsPlutus () -> do
          r <- zoom _2 $ popRedeemers
          case traverse (elaborateModelRedeemer fakeTxBody) r of
            SNothing -> error "cant elaborate ptr"
            SJust xs -> do
              let redeemers = Alonzo.Redeemers $ Map.fromList xs
                  txDats = foldMapOf (traverse . _2 . _1) (\d -> Alonzo.TxDats $ Map.singleton (Alonzo.hashData d) d) xs
              pure (SupportsPlutus (redeemers, txDats))

      let realTxBody = case redeemers of
            SupportsPlutus (r, _)
              | not (Alonzo.nullRedeemers r) ->
                makeTxBody @era nes $
                  txBodyArguments
                    { _txBodyArguments_redeemers = mapSupportsPlutus SJust redeemers
                    }
            _ -> fakeTxBody

          bodyHash = hashAnnotated realTxBody

      wits <- zoom _2 $ popWitnesses (Proxy :: Proxy era) bodyHash

      _2 . eesTxIds . at mtxId .= Just (UTxO.txid @era realTxBody)
      pure $
        makeTx
          proxy
          realTxBody
          TxWitnessArguments
            { _txWitnessArguments_vkey = wits,
              _txWitnessArguments_scripts = scripts,
              _txWitnessArguments_redeemers = redeemers
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

-- under normal circumstances, this arises during elaboration, at which time the
-- era is already known
data ModelRedeemer era = ModelRedeemer
  { _mrdmrPtr :: !(Alonzo.ScriptPurpose (Crypto era)),
    _mrdmData :: !PlutusTx.Data,
    _mrdmExUnits :: !ExUnits
  }
  deriving (Eq, Show)

elaborateModelRedeemer ::
  forall era.
  ( HasField "inputs" (Core.TxBody era) (Set.Set (Shelley.TxIn (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Shelley.Wdrl (Crypto era)),
    HasField "certs" (Core.TxBody era) (StrictSeq.StrictSeq (DCert (Crypto era))),
    HasField "minted" (Core.TxBody era) (Set.Set (ScriptHash (Crypto era)))
  ) =>
  Core.TxBody era ->
  ModelRedeemer era ->
  StrictMaybe (Alonzo.RdmrPtr, (Alonzo.Data era, Alonzo.ExUnits))
elaborateModelRedeemer tx (ModelRedeemer scriptPurpose dat exUnits) =
  (,) <$> (Alonzo.rdptr @era tx scriptPurpose) <*> pure (Alonzo.Data dat, exUnits)

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
