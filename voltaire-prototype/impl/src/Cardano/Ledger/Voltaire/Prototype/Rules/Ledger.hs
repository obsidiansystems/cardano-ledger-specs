{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : LedgerState
-- Description : Copy/paste from 'Shelley.Spec.Ledger.STS.Ledger' changing
--               only the environment passed to the UTXOW rule
module Cardano.Ledger.Voltaire.Prototype.Rules.Ledger
  ( LEDGER
  )
where

import Cardano.Ledger.Coin (Coin)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era, TxInBlock)
import Control.State.Transition
  ( Assertion (..),
    AssertionViolation (..),
    Embed (..),
    STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
    trans,
  )
import Data.Sequence (Seq)
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import GHC.Records (HasField, getField)
import Shelley.Spec.Ledger.BaseTypes (ShelleyBase)
import Shelley.Spec.Ledger.EpochBoundary (obligation)
import Shelley.Spec.Ledger.Keys (DSignable, Hash)
import Shelley.Spec.Ledger.LedgerState
  ( AccountState,
    DPState (..),
    DState (..),
    PState (..),
    UTxOState (..),
  )
import qualified Shelley.Spec.Ledger.STS.Ledger as Shelley
import Shelley.Spec.Ledger.STS.Delegs (DELEGS, DelegsEnv (..), DelegsPredicateFailure)
import Shelley.Spec.Ledger.STS.Utxo
  ( UtxoEnv (..),
  )
import Shelley.Spec.Ledger.STS.Utxow (UTXOW, UtxowPredicateFailure)
import Shelley.Spec.Ledger.TxBody (DCert, EraIndependentTxBody)

data LEDGER era

-- | Copy/paste from Shelley.Spec.Ledger.STS.Ledger
instance
  ( Show (Core.PParams era),
    Show (TxInBlock era),
    DSignable (Crypto era) (Hash (Crypto era) EraIndependentTxBody),
    Era era,
    Embed (Core.EraRule "DELEGS" era) (LEDGER era),
    Embed (Core.EraRule "UTXOW" era) (LEDGER era),
    Environment (Core.EraRule "UTXOW" era) ~ (UtxoEnv era, AccountState),
    State (Core.EraRule "UTXOW" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXOW" era) ~ TxInBlock era,
    Environment (Core.EraRule "DELEGS" era) ~ DelegsEnv era,
    State (Core.EraRule "DELEGS" era) ~ DPState (Crypto era),
    Signal (Core.EraRule "DELEGS" era) ~ Seq (DCert (Crypto era)),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "_keyDeposit" (Core.PParams era) Coin,
    HasField "_poolDeposit" (Core.PParams era) Coin,
    Show (UTxOState era)
  ) =>
  STS (LEDGER era)
  where
  type
    State (LEDGER era) =
      (UTxOState era, DPState (Crypto era))
  type Signal (LEDGER era) = TxInBlock era
  type Environment (LEDGER era) = Shelley.LedgerEnv era
  type BaseM (LEDGER era) = ShelleyBase
  type PredicateFailure (LEDGER era) = Shelley.LedgerPredicateFailure era

  initialRules = []
  transitionRules = [ledgerTransition]

  renderAssertionViolation AssertionViolation {avSTS, avMsg, avCtx, avState} =
    "AssertionViolation (" <> avSTS <> "): " <> avMsg
      <> "\n"
      <> show avCtx
      <> "\n"
      <> show avState

  assertions =
    [ PostCondition
        "Deposit pot must equal obligation"
        ( \(TRC (Shelley.LedgerEnv {Shelley.ledgerPp}, _, _))
           (utxoSt, DPState {_dstate, _pstate}) ->
              obligation ledgerPp (_rewards _dstate) (_pParams _pstate)
                == _deposited utxoSt
        )
    ]

-- | Copy/paste from Shelley.Spec.Ledger.STS.Ledger, except
--   with a different UTXOW environment.
ledgerTransition ::
  forall era.
  ( Embed (Core.EraRule "DELEGS" era) (LEDGER era),
    Environment (Core.EraRule "DELEGS" era) ~ DelegsEnv era,
    State (Core.EraRule "DELEGS" era) ~ DPState (Crypto era),
    Signal (Core.EraRule "DELEGS" era) ~ Seq (DCert (Crypto era)),
    Embed (Core.EraRule "UTXOW" era) (LEDGER era),
    Environment (Core.EraRule "UTXOW" era) ~ (UtxoEnv era, AccountState),
    State (Core.EraRule "UTXOW" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXOW" era) ~ TxInBlock era,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "body" (TxInBlock era) (Core.TxBody era)
  ) =>
  TransitionRule (LEDGER era)
ledgerTransition = do
  TRC (Shelley.LedgerEnv slot txIx pp account, (utxoSt, dpstate), tx) <- judgmentContext

  dpstate' <-
    trans @(Core.EraRule "DELEGS" era) $
      TRC
        ( DelegsEnv slot txIx pp tx account,
          dpstate,
          StrictSeq.fromStrict $ getField @"certs" $ getField @"body" tx
        )

  let DPState dstate pstate = dpstate
      genDelegs = _genDelegs dstate
      stpools = _pParams pstate

  utxoSt' <-
    trans @(Core.EraRule "UTXOW" era) $
      TRC
        ( (UtxoEnv slot pp stpools genDelegs, account),
          utxoSt,
          tx
        )
  pure (utxoSt', dpstate')

-- | Copy/paste from Shelley.Spec.Ledger.STS.Ledger
instance
  ( Era era,
    STS (DELEGS era),
    PredicateFailure (Core.EraRule "DELEGS" era) ~ DelegsPredicateFailure era
  ) =>
  Embed (DELEGS era) (LEDGER era)
  where
  wrapFailed = Shelley.DelegsFailure

-- | Copy/paste from Shelley.Spec.Ledger.STS.Ledger
instance
  ( Era era,
    STS (UTXOW era),
    PredicateFailure (Core.EraRule "UTXOW" era) ~ UtxowPredicateFailure era
  ) =>
  Embed (UTXOW era) (LEDGER era)
  where
  wrapFailed = Shelley.UtxowFailure
