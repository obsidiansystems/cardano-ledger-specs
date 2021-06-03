{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Elaborators.Shelley where

import qualified Cardano.Crypto.DSIGN.Class as DSIGN
import qualified Cardano.Crypto.KES.Class as KES
import Cardano.Crypto.Util (SignableRepresentation)
import Cardano.Ledger.Crypto (DSIGN, KES)
import Cardano.Ledger.Shelley (ShelleyEra)
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Trans.State as State hiding (state)
import Data.Maybe.Strict (StrictMaybe (..))
import Shelley.Spec.Ledger.API.Mempool (ApplyTxError (..))
import Shelley.Spec.Ledger.API.Protocol (PraosCrypto)
import Shelley.Spec.Ledger.STS.EraMapping ()
import Shelley.Spec.Ledger.STS.Ledger (LedgerPredicateFailure (..))
import Shelley.Spec.Ledger.STS.Utxo (UtxoPredicateFailure (..))
import Shelley.Spec.Ledger.STS.Utxow (UtxowPredicateFailure (..))
import qualified Shelley.Spec.Ledger.Tx as Shelley
import Test.Cardano.Ledger.ModelChain
import Test.Cardano.Ledger.ModelChain.Value

instance
  ( PraosCrypto crypto,
    KES.Signable (KES crypto) ~ SignableRepresentation,
    DSIGN.Signable (DSIGN crypto) ~ SignableRepresentation
  ) =>
  ElaborateEraModel (ShelleyEra crypto)
  where
  toEraPredicateFailure = \case
    ModelValueNotConservedUTxO x y -> State.evalState $
      Except.runExceptT $ do
        x' <- Except.ExceptT $ evalModelValue lookupModelValue x
        y' <- Except.ExceptT $ evalModelValue lookupModelValue y
        pure $
          ApplyBlockTransitionError_Tx $
            ApplyTxError
              [UtxowFailure (UtxoFailure (ValueNotConservedUTxO x' y'))]

  makeTxBody _ maxTTL fee ins outs dcerts wdrl =
    Shelley.TxBody
      { Shelley._inputs = ins,
        Shelley._outputs = outs,
        Shelley._certs = dcerts,
        Shelley._wdrls = wdrl,
        Shelley._txfee = fee,
        Shelley._ttl = maxTTL,
        Shelley._txUpdate = SNothing,
        Shelley._mdHash = SNothing
      }

  makeTx _ realTxBody wits =
    Shelley.Tx realTxBody (mempty {Shelley.addrWits = wits}) SNothing
