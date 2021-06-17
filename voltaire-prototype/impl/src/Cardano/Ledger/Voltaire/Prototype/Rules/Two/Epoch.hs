{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Cardano.Ledger.Voltaire.Prototype.Rules.Two.Epoch
-- Description : Same as 'Shelley.Spec.Ledger.STS.Epoch' except for UPEC. MIR
--               transfers now happen in UPEC. This entails the following changes:
--               (1) different UPEC environment type; (2) different UPEC state
--               type; (3) the EpochState returned by UPEC contains changes from
--               MIR transfers (therefore include these changes in the returned value).
module Cardano.Ledger.Voltaire.Prototype.Rules.Two.Epoch
  ( EPOCH,
    Shelley.EpochPredicateFailure (..),
    PredicateFailure,
  )
where

import qualified Cardano.Ledger.Voltaire.Prototype.Rules.Two.Upec as Two
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era (Crypto))
import Cardano.Ledger.Shelley.Constraints (UsesTxOut, UsesValue)
import Control.SetAlgebra (eval, (⨃))
import Control.State.Transition
  ( Embed (..),
    STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
    trans,
  )
import Data.Default.Class (Default)
import qualified Data.Map.Strict as Map
import GHC.Records
import Shelley.Spec.Ledger.BaseTypes (ShelleyBase)
import Shelley.Spec.Ledger.EpochBoundary (SnapShots, obligation)
import Shelley.Spec.Ledger.LedgerState
  ( EpochState,
    LedgerState,
    PState (..),
    UpecState (..),
    esAccountState,
    esLState,
    esNonMyopic,
    esPp,
    esPrevPp,
    esSnapshots,
    _delegationState,
    _deposited,
    _ppups,
    _reserves,
    _rewards,
    _utxoState,
    pattern DPState,
    pattern EpochState,
  )
import Shelley.Spec.Ledger.Rewards ()
import Shelley.Spec.Ledger.STS.PoolReap (POOLREAP, PoolreapPredicateFailure, PoolreapState (..))
import Shelley.Spec.Ledger.STS.Snap (SNAP, SnapPredicateFailure)
import qualified Shelley.Spec.Ledger.STS.Epoch as Shelley
import Shelley.Spec.Ledger.Slot (EpochNo)

data EPOCH era

instance
  ( UsesTxOut era,
    UsesValue era,
    Embed (Core.EraRule "SNAP" era) (EPOCH era),
    Environment (Core.EraRule "SNAP" era) ~ LedgerState era,
    State (Core.EraRule "SNAP" era) ~ SnapShots (Crypto era),
    Signal (Core.EraRule "SNAP" era) ~ (),
    Embed (Core.EraRule "POOLREAP" era) (EPOCH era),
    Environment (Core.EraRule "POOLREAP" era) ~ Core.PParams era,
    State (Core.EraRule "POOLREAP" era) ~ PoolreapState era,
    Signal (Core.EraRule "POOLREAP" era) ~ EpochNo,
    Embed (Core.EraRule "UPEC" era) (EPOCH era),
    Environment (Core.EraRule "UPEC" era) ~ (),
    State (Core.EraRule "UPEC" era) ~ (UpecState era, EpochState era),
    Signal (Core.EraRule "UPEC" era) ~ (),
    Default (State (Core.EraRule "PPUP" era)),
    Default (Core.PParams era),
    HasField "_keyDeposit" (Core.PParams era) Coin,
    HasField "_poolDeposit" (Core.PParams era) Coin
  ) =>
  STS (EPOCH era)
  where
  type State (EPOCH era) = EpochState era
  type Signal (EPOCH era) = EpochNo
  type Environment (EPOCH era) = ()
  type BaseM (EPOCH era) = ShelleyBase
  type PredicateFailure (EPOCH era) = Shelley.EpochPredicateFailure era
  transitionRules = [epochTransition]

epochTransition ::
  forall era.
  ( Embed (Core.EraRule "SNAP" era) (EPOCH era),
    Environment (Core.EraRule "SNAP" era) ~ LedgerState era,
    State (Core.EraRule "SNAP" era) ~ SnapShots (Crypto era),
    Signal (Core.EraRule "SNAP" era) ~ (),
    Embed (Core.EraRule "POOLREAP" era) (EPOCH era),
    Environment (Core.EraRule "POOLREAP" era) ~ Core.PParams era,
    State (Core.EraRule "POOLREAP" era) ~ PoolreapState era,
    Signal (Core.EraRule "POOLREAP" era) ~ EpochNo,
    Embed (Core.EraRule "UPEC" era) (EPOCH era),
    Environment (Core.EraRule "UPEC" era) ~ (),
    State (Core.EraRule "UPEC" era) ~ (UpecState era, EpochState era),
    Signal (Core.EraRule "UPEC" era) ~ (),
    HasField "_keyDeposit" (Core.PParams era) Coin,
    HasField "_poolDeposit" (Core.PParams era) Coin
  ) =>
  TransitionRule (EPOCH era)
epochTransition = do
  TRC
    ( _,
      EpochState
        { esAccountState = acnt,
          esSnapshots = ss,
          esLState = ls,
          esPrevPp = pr,
          esPp = pp,
          esNonMyopic = nm
        },
      e
      ) <-
    judgmentContext
  let utxoSt = _utxoState ls
  let DPState dstate pstate = _delegationState ls
  ss' <-
    trans @(Core.EraRule "SNAP" era) $ TRC (ls, ss, ())

  let PState pParams fPParams _ = pstate
      ppp = eval (pParams ⨃ fPParams)
      pstate' =
        pstate
          { _pParams = ppp,
            _fPParams = Map.empty
          }
  PoolreapState utxoSt' acnt' dstate' pstate'' <-
    trans @(Core.EraRule "POOLREAP" era) $
      TRC (pp, PoolreapState utxoSt acnt dstate pstate', e)

  let epochState' =
        EpochState
          acnt'
          ss'
          (ls {_utxoState = utxoSt', _delegationState = DPState dstate' pstate''})
          pr
          pp
          nm

  (UpecState pp' ppupSt', epochState'') <-
    trans @(Core.EraRule "UPEC" era) $
      TRC ((), (UpecState pp (_ppups utxoSt'), epochState'), ())
  let utxoSt'' = utxoSt' {_ppups = ppupSt'}

  let Coin oblgCurr = obligation pp (_rewards dstate') (_pParams pstate'')
      Coin oblgNew = obligation pp' (_rewards dstate') (_pParams pstate'')
      Coin reserves = _reserves acnt'
      utxoSt''' = utxoSt'' {_deposited = Coin oblgNew}
      acnt'' = acnt' {_reserves = Coin $ reserves + oblgCurr - oblgNew}
  pure $
    epochState''
      { esAccountState = acnt'',
        esLState = (esLState epochState'') {_utxoState = utxoSt'''},
        esPrevPp = pp,
        esPp = pp'
      }

instance
  ( UsesTxOut era,
    UsesValue era,
    PredicateFailure (Core.EraRule "SNAP" era) ~ SnapPredicateFailure era
  ) =>
  Embed (SNAP era) (EPOCH era)
  where
  wrapFailed = Shelley.SnapFailure

instance
  ( Era era,
    STS (POOLREAP era),
    PredicateFailure (Core.EraRule "POOLREAP" era) ~ PoolreapPredicateFailure era
  ) =>
  Embed (POOLREAP era) (EPOCH era)
  where
  wrapFailed = Shelley.PoolReapFailure

instance
  ( Era era,
    STS (Two.UPEC era),
    PredicateFailure (Core.EraRule "UPEC" era) ~ Two.UpecPredicateFailure era
  ) =>
  Embed (Two.UPEC era) (EPOCH era)
  where
  wrapFailed = Shelley.UpecFailure
