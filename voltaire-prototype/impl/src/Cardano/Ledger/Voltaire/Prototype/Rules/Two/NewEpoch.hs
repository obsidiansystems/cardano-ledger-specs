{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Cardano.Ledger.Voltaire.Prototype.Rules.Two.NewEpoch
-- Description : Same as 'Shelley.Spec.Ledger.STS.NewEpoch' except does NOT fire "MIR" rule.
module Cardano.Ledger.Voltaire.Prototype.Rules.Two.NewEpoch
  ( NEWEPOCH,
    NewEpochPredicateFailure (..),
    PredicateFailure,
    Shelley.calculatePoolDistr,
  )
where

import qualified Shelley.Spec.Ledger.STS.NewEpoch as Shelley
import Cardano.Ledger.Coin
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto)
import Cardano.Ledger.Shelley.Constraints (UsesTxOut, UsesValue)
import qualified Cardano.Ledger.Val as Val
import Control.Provenance (runProvM)
import Control.State.Transition
import Data.Default.Class (Default, def)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import GHC.Records
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.Delegation.Certificates
import Shelley.Spec.Ledger.EpochBoundary
import Shelley.Spec.Ledger.LedgerState
import Shelley.Spec.Ledger.PParams (ProtVer)
import Shelley.Spec.Ledger.Rewards (sumRewards)
import Shelley.Spec.Ledger.STS.Epoch
import Shelley.Spec.Ledger.Slot

data NEWEPOCH era

data NewEpochPredicateFailure era
  = EpochFailure (PredicateFailure (Core.EraRule "EPOCH" era)) -- Subtransition Failures
  | CorruptRewardUpdate
      !(RewardUpdate (Crypto era)) -- The reward update which violates an invariant
  deriving (Generic)

deriving stock instance
  ( Show (PredicateFailure (Core.EraRule "EPOCH" era))
  ) =>
  Show (NewEpochPredicateFailure era)

deriving stock instance
  ( Eq (PredicateFailure (Core.EraRule "EPOCH" era))
  ) =>
  Eq (NewEpochPredicateFailure era)

instance
  ( NoThunks (PredicateFailure (Core.EraRule "EPOCH" era))
  ) =>
  NoThunks (NewEpochPredicateFailure era)

instance
  ( UsesTxOut era,
    UsesValue era,
    Embed (Core.EraRule "EPOCH" era) (NEWEPOCH era),
    Environment (Core.EraRule "EPOCH" era) ~ (),
    State (Core.EraRule "EPOCH" era) ~ EpochState era,
    Signal (Core.EraRule "EPOCH" era) ~ EpochNo,
    Default (EpochState era),
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    Default (State (Core.EraRule "PPUP" era)),
    Default (Core.PParams era)
  ) =>
  STS (NEWEPOCH era)
  where
  type State (NEWEPOCH era) = NewEpochState era

  type Signal (NEWEPOCH era) = EpochNo

  type Environment (NEWEPOCH era) = ()

  type BaseM (NEWEPOCH era) = ShelleyBase
  type PredicateFailure (NEWEPOCH era) = NewEpochPredicateFailure era

  initialRules =
    [ pure $
        NewEpochState
          (EpochNo 0)
          (BlocksMade Map.empty)
          (BlocksMade Map.empty)
          def
          SNothing
          (PoolDistr Map.empty)
    ]

  transitionRules = [newEpochTransition]

newEpochTransition ::
  forall era.
  ( Embed (Core.EraRule "EPOCH" era) (NEWEPOCH era),
    Environment (Core.EraRule "EPOCH" era) ~ (),
    State (Core.EraRule "EPOCH" era) ~ EpochState era,
    Signal (Core.EraRule "EPOCH" era) ~ EpochNo,
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    UsesTxOut era,
    UsesValue era,
    Default (State (Core.EraRule "PPUP" era)),
    Default (Core.PParams era)
  ) =>
  TransitionRule (NEWEPOCH era)
newEpochTransition = do
  TRC
    ( _,
      src@(NewEpochState (EpochNo eL) _ bcur es ru _pd),
      e@(EpochNo e_)
      ) <-
    judgmentContext
  if e_ /= eL + 1
    then pure src
    else do
      es' <- case ru of
        SNothing -> pure es
        SJust (p@(Pulsing _ _)) -> do
          ru'@(RewardUpdate dt dr rs_ df _) <- liftSTS $ runProvM $ completeRupd p
          let totRs = sumRewards (esPrevPp es) rs_
          Val.isZero (dt <> (dr <> (toDeltaCoin totRs) <> df)) ?! CorruptRewardUpdate ru'
          pure $ applyRUpd ru' es
        SJust (Complete ru') -> do
          let RewardUpdate dt dr rs_ df _ = ru'
              totRs = sumRewards (esPrevPp es) rs_
          Val.isZero (dt <> (dr <> (toDeltaCoin totRs) <> df)) ?! CorruptRewardUpdate ru'
          pure $ applyRUpd ru' es

      es'' <- trans @(Core.EraRule "EPOCH" era) $ TRC ((), es', e)
      let EpochState _acnt ss _ls _pr _ _ = es''
          pd' = Shelley.calculatePoolDistr (_pstakeSet ss)
      pure $
        NewEpochState
          e
          bcur
          (BlocksMade Map.empty)
          es''
          SNothing
          pd'

instance
  ( UsesTxOut era,
    UsesValue era,
    STS (EPOCH era),
    PredicateFailure (Core.EraRule "EPOCH" era) ~ EpochPredicateFailure era
  ) =>
  Embed (EPOCH era) (NEWEPOCH era)
  where
  wrapFailed = EpochFailure
