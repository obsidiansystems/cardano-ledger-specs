{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fmax-relevant-binds=0 #-}

module Test.Cardano.Ledger.Elaborators.Alonzo where

import Cardano.Crypto.Util (SignableRepresentation)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Rules.Utxo
import Cardano.Ledger.Alonzo.Rules.Utxow
import Cardano.Ledger.Alonzo.Scripts (Prices(..), ExUnits(..), CostModel(..))
import Cardano.Ledger.Alonzo.Translation (AlonzoGenesis(..))
import Cardano.Ledger.Alonzo.TxBody as Alonzo (TxOut (..))
import Cardano.Ledger.Coin
import Cardano.Ledger.Crypto (KES, DSIGN)
import Cardano.Ledger.Era (Era (Crypto))
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval(..))
import Cardano.Slotting.Slot
import Control.Lens
import Numeric.Natural
import Control.Monad.State (MonadState(..))
import Data.Default.Class
import Data.Foldable
import Data.Maybe.Strict (StrictMaybe(..))
import Data.Proxy
import Data.Traversable
import Shelley.Spec.Ledger.API (ShelleyBasedEra)
import Shelley.Spec.Ledger.API.Mempool (ApplyTxError(..))
import Shelley.Spec.Ledger.API.Protocol (PraosCrypto)
import Shelley.Spec.Ledger.STS.Ledger
import Shelley.Spec.Ledger.STS.Utxow
import qualified Cardano.Crypto.DSIGN.Class as DSIGN
import qualified Cardano.Crypto.KES.Class as KES
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo (Script)
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWitness as Alonzo
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Val as Val
import qualified Control.Monad.Trans.State as State
import qualified Data.Map as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import qualified Shelley.Spec.Ledger.Tx as Shelley (TxIn(..))
import qualified Shelley.Spec.Ledger.TxBody as Shelley (Wdrl(..))
import qualified Shelley.Spec.Ledger.UTxO as UTxO

import Test.Cardano.Ledger.ModelChain

instance Default AlonzoGenesis where
  def = AlonzoGenesis
    { adaPerUTxOWord = Coin 10000  -- error "Default AlonzoGenesis :: adaPerUTxOWord"
    , costmdls = Map.fromSet (const $ CostModel Map.empty) $ Set.fromList [minBound..] -- error "Default AlonzoGenesis :: costmdls"
    , prices = Prices (Coin 1000) (Coin 1000)
    , maxTxExUnits = ExUnits 100 100
    , maxBlockExUnits = ExUnits 100 100
    , maxValSize = 100000
    }

instance
    ( PraosCrypto crypto
    , KES.Signable (KES crypto) ~ SignableRepresentation
    , DSIGN.Signable (DSIGN crypto) ~ SignableRepresentation
    ) => ElaborateEraModel (AlonzoEra crypto) where

  makeTx _ _ = \ttl mtx -> State.runState (mkAlonzoTx ttl mtx)

  toEraPredicateFailure = \case
    ModelValueNotConservedUTxO x y -> ApplyBlockTransitionError_Tx $ ApplyTxError
      [UtxowFailure (WrappedShelleyEraFailure
        (UtxoFailure (ValueNotConservedUTxO (Val.inject $ Coin $ unModelValue x) (Val.inject $ Coin $ unModelValue y)))
        )]



mkAlonzoTx
  :: forall m era st.
  ( MonadState st m
  , HasEraElaboratorState st era
  , Core.TxBody era ~ Alonzo.TxBody era
  , Core.TxOut era ~ Alonzo.TxOut era
  , ShelleyBasedEra era
  , Core.Script era ~ Alonzo.Script era
  , st ~ ElaborateEraModelState era
  , ElaborateEraModel era
  )
  => SlotNo
  -> ModelTx
  -> m (Alonzo.ValidatedTx era)
mkAlonzoTx maxTTL (ModelTx mtxId mtxInputs mtxOutputs mtxFee mtxDCert) = do
  outs <- ifor mtxOutputs $ \idx -> mkTxOut (ModelUTxOId mtxId $ toEnum @Natural idx)

  ins :: Set.Set (Shelley.TxIn (Crypto era)) <- fmap fold $ traverse mkTxIn $ Set.toList mtxInputs
  dcerts <- traverse (mkDCerts (Proxy :: Proxy era)) mtxDCert
  let
    realTxBody = Alonzo.TxBody
      { Alonzo.inputs = ins
      , Alonzo.txinputs_fee = ins
      , Alonzo.outputs = StrictSeq.fromList outs
      , Alonzo.txcerts = StrictSeq.fromList dcerts
      , Alonzo.txwdrls = Shelley.Wdrl Map.empty
      , Alonzo.txfee = Coin . unModelValue $ mtxFee
      , Alonzo.txvldt = ValidityInterval SNothing $ SJust (1+maxTTL)
      , Alonzo.txUpdates = SNothing
      , Alonzo.reqSignerHashes = Set.empty
      , Alonzo.mint = Val.zero
      , Alonzo.wppHash = SNothing
      , Alonzo.adHash = SNothing
      , Alonzo.txnetworkid = SNothing -- SJust Testnet
      }
    bodyHash = hashAnnotated realTxBody

  wits <- popWitnesses (Proxy :: Proxy era) bodyHash

  eraElaboratorState . eesTxIds %= Map.insert mtxId (UTxO.txid @era realTxBody)
  let
    witSet = Alonzo.TxWitness
      { Alonzo.txwitsVKey = wits
      , Alonzo.txwitsBoot = Set.empty
      , Alonzo.txscripts = Map.empty
      , Alonzo.txdats = Map.empty
      , Alonzo.txrdmrs = Alonzo.Redeemers Map.empty
      }
  pure (Alonzo.ValidatedTx realTxBody witSet (Alonzo.IsValidating True) SNothing)
