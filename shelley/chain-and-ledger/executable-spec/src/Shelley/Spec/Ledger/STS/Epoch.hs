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

module Shelley.Spec.Ledger.STS.Epoch
  ( EPOCH,
    EpochPredicateFailure (..),
    PredicateFailure,
  )
where

import Control.Exception (assert)
import Cardano.Ledger.Shelley.Constraints (ShelleyBased)
import Control.SetAlgebra (eval, (⨃))
import Control.State.Transition (Embed (..), InitialRule, STS (..), TRC (..), TransitionRule, judgmentContext, trans)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.BaseTypes (ShelleyBase)
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.EpochBoundary (emptySnapShots, obligation)
import Shelley.Spec.Ledger.LedgerState
  ( EpochState,
    PState (..),
    emptyAccount,
    emptyLedgerState,
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
import Shelley.Spec.Ledger.PParams
  ( emptyPParams,
  )
import Shelley.Spec.Ledger.Rewards (emptyNonMyopic)
import Shelley.Spec.Ledger.STS.PoolReap (POOLREAP, PoolreapState (..))
import Shelley.Spec.Ledger.STS.Snap (SNAP)
import Shelley.Spec.Ledger.STS.Upec (UPEC, UPECState (..))
import Shelley.Spec.Ledger.Slot (EpochNo)

data EPOCH era

data EpochPredicateFailure era
  = PoolReapFailure (PredicateFailure (POOLREAP era))
  | SnapFailure (PredicateFailure (SNAP era))
  | UpecFailure (PredicateFailure (UPEC era))
  deriving (Generic)

deriving stock instance
  (Eq (PredicateFailure (SNAP era))) =>
  Eq (EpochPredicateFailure era)

deriving stock instance
  (Show (PredicateFailure (SNAP era))) =>
  Show (EpochPredicateFailure era)

instance ShelleyBased era => STS (EPOCH era) where
  type State (EPOCH era) = EpochState era
  type Signal (EPOCH era) = EpochNo
  type Environment (EPOCH era) = ()
  type BaseM (EPOCH era) = ShelleyBase
  type PredicateFailure (EPOCH era) = EpochPredicateFailure era
  initialRules = [initialEpoch]
  transitionRules = [epochTransition]

instance NoThunks (EpochPredicateFailure era)

initialEpoch :: InitialRule (EPOCH era)
initialEpoch =
  pure $
    EpochState
      emptyAccount
      emptySnapShots
      emptyLedgerState
      emptyPParams
      emptyPParams
      emptyNonMyopic

epochTransition ::
  forall era.
  ShelleyBased era =>
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
    trans @(SNAP era) $ TRC (ls, ss, ())

  let PState pParams fPParams _ = pstate
      ppp = eval (pParams ⨃ fPParams)
      pstate' =
        pstate
          { _pParams = ppp,
            _fPParams = Map.empty
          }
  PoolreapState utxoSt' acnt' dstate' pstate'' <-
    trans @(POOLREAP era) $ TRC (pp, PoolreapState utxoSt acnt dstate pstate', e)

  let epochState' =
        EpochState
          acnt'
          ss'
          (ls {_utxoState = utxoSt', _delegationState = DPState dstate' pstate''})
          pr
          pp
          nm

  UPECState pp' ppupSt' <-
    trans @(UPEC era) $ TRC (epochState', UPECState pp (_ppups utxoSt'), ())
  let utxoSt'' = utxoSt' {_ppups = ppupSt'}

  let Coin oblgCurr = obligation pp (_rewards dstate') (_pParams pstate'')
      Coin oblgNew = obligation pp' (_rewards dstate') (_pParams pstate'')
      Coin reserves = _reserves acnt'
      utxoSt''' =
        assert (_deposited utxoSt'' == Coin oblgCurr) $
          utxoSt'' {_deposited = Coin oblgNew}
      acnt'' = acnt' {_reserves = Coin $ reserves + oblgCurr - oblgNew}
  pure $
    epochState'
    { esAccountState = acnt'',
      esLState = (esLState epochState') {_utxoState = utxoSt'''},
      esPrevPp = pp,
      esPp = pp'
    }

instance ShelleyBased era => Embed (SNAP era) (EPOCH era) where
  wrapFailed = SnapFailure

instance ShelleyBased era => Embed (POOLREAP era) (EPOCH era) where
  wrapFailed = PoolReapFailure

instance ShelleyBased era => Embed (UPEC era) (EPOCH era) where
  wrapFailed = UpecFailure
