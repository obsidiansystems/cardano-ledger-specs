{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Elaborators.Shelley where

import Cardano.Crypto.Util (SignableRepresentation)
import Cardano.Ledger.Coin
import Cardano.Ledger.Crypto (KES, DSIGN)
import Cardano.Ledger.Era (Era (Crypto))
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Slotting.Slot
import Control.Lens
import Control.Monad.State (MonadState(..))
import Data.Foldable
import Data.Maybe.Strict (StrictMaybe(..))
import Data.Proxy
import Data.Traversable
import Shelley.Spec.Ledger.API (ShelleyBasedEra)
import Shelley.Spec.Ledger.API.Mempool (ApplyTxError(..))
import Shelley.Spec.Ledger.API.Protocol (PraosCrypto)
import Shelley.Spec.Ledger.STS.EraMapping ()
import Shelley.Spec.Ledger.STS.Ledger (LedgerPredicateFailure (..))
import Shelley.Spec.Ledger.STS.Utxo (UtxoPredicateFailure (..))
import Shelley.Spec.Ledger.STS.Utxow (UtxowPredicateFailure(..))
import qualified Cardano.Crypto.DSIGN.Class as DSIGN
import qualified Cardano.Crypto.KES.Class as KES
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Val as Val
import qualified Control.Monad.Trans.State as State
import qualified Data.Map as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import qualified Shelley.Spec.Ledger.Tx as Shelley
import qualified Shelley.Spec.Ledger.TxBody as Shelley
import qualified Shelley.Spec.Ledger.UTxO as UTxO

import Test.Cardano.Ledger.ModelChain

instance
    ( PraosCrypto crypto
    , KES.Signable (KES crypto) ~ SignableRepresentation
    , DSIGN.Signable (DSIGN crypto) ~ SignableRepresentation
    ) => ElaborateEraModel (ShelleyEra crypto) where

  makeTx _ _ = \ttl mtx -> State.runState (mkShelleyTx ttl mtx)

  toEraPredicateFailure = \case
    ModelValueNotConservedUTxO x y -> ApplyBlockTransitionError_Tx $ ApplyTxError
      [UtxowFailure (UtxoFailure (ValueNotConservedUTxO (Val.inject $ Coin $ unModelValue x) (Val.inject $ Coin $ unModelValue y)))]

mkShelleyTx
  :: forall m era st.
  ( MonadState st m
  , HasEraElaboratorState st era
  , Core.TxBody era ~ Shelley.TxBody era
  , Core.TxOut era ~ Shelley.TxOut era
  , ShelleyBasedEra era
  , Core.Witnesses era ~ Shelley.WitnessSet era
  )
  => SlotNo
  -> ModelTx
  -> m (Shelley.Tx era)
mkShelleyTx maxTTL mtx@(ModelTx {}) = do
  outs <- traverse mkTxOut $ _mtxOutputs mtx

  ins :: Set.Set (Shelley.TxIn (Crypto era)) <- fmap fold $ traverse mkTxIn $ Set.toList $ _mtxInputs mtx
  let
    realTxBody = Shelley.TxBody
      { Shelley._inputs = ins
      , Shelley._outputs = StrictSeq.fromList outs
      , Shelley._certs = StrictSeq.empty
      , Shelley._wdrls = Shelley.Wdrl Map.empty
      , Shelley._txfee = Coin . unModelValue $ _mtxFee mtx
      , Shelley._ttl = maxTTL
      , Shelley._txUpdate = SNothing
      , Shelley._mdHash = SNothing
      }
    bodyHash = hashAnnotated realTxBody
  wits :: Core.Witnesses era <- fmap fold $ for (toList $ _mtxWitness mtx) $ \mAddr -> do
    (keyP, _) <- getKeyPairFor (Proxy :: Proxy era) mAddr
    let wit = UTxO.makeWitnessVKey bodyHash keyP
    pure $ mempty {Shelley.addrWits = Set.singleton wit}

  eraElaboratorState . txIds %= Map.insert (_mtxId mtx) (UTxO.txid @era realTxBody)
  pure (Shelley.Tx realTxBody wits SNothing)


