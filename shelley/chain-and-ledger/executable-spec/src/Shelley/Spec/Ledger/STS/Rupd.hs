{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.STS.Rupd
  ( RUPD,
    RupdEnv (..),
    PredicateFailure,
    RupdPredicateFailure,
    rewardsAreNotStable,
  )
where

import Control.Monad.Trans.Reader (asks)
import Control.State.Transition
  ( STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
    liftSTS,
  )
import Data.Functor ((<&>))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.BaseTypes
  ( ShelleyBase,
    StrictMaybe (..),
    epochInfo,
    maxLovelaceSupply,
    randomnessStabilisationWindow,
  )
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.EpochBoundary (BlocksMade)
import Shelley.Spec.Ledger.LedgerState (EpochState, RewardUpdate, createRUpd)
import Shelley.Spec.Ledger.Rewards (RewardProvenanceFlag (WithoutRewardProvenance))
import Shelley.Spec.Ledger.Slot
  ( Duration (..),
    SlotNo,
    epochInfoEpoch,
    epochInfoFirst,
    epochInfoSize,
    (+*),
  )

data RUPD era

data RupdEnv era
  = RupdEnv (BlocksMade era) (EpochState era)

data RupdPredicateFailure era -- No predicate failures
  deriving (Show, Eq, Generic)

instance NoThunks (RupdPredicateFailure era)

instance Typeable era => STS (RUPD era) where
  type State (RUPD era) = StrictMaybe (RewardUpdate era)
  type Signal (RUPD era) = SlotNo
  type Environment (RUPD era) = RupdEnv era
  type BaseM (RUPD era) = ShelleyBase
  type PredicateFailure (RUPD era) = RupdPredicateFailure era

  initialRules = [pure SNothing]
  transitionRules = [rupdTransition]

rewardsAreNotStable :: SlotNo -> ShelleyBase Bool
rewardsAreNotStable s = do
  ei <- asks epochInfo
  sr <- asks randomnessStabilisationWindow
  e <- epochInfoEpoch ei s
  slot <- epochInfoFirst ei e <&> (+* (Duration sr))
  return (s <= slot)

rupdTransition :: Typeable era => TransitionRule (RUPD era)
rupdTransition = do
  TRC (RupdEnv b es, ru, s) <- judgmentContext
  (slotsPerEpoch, instable, maxLL) <- liftSTS $ do
    ei <- asks epochInfo
    e <- epochInfoEpoch ei s
    slotsPerEpoch <- epochInfoSize ei e
    maxLL <- asks maxLovelaceSupply
    instable <- rewardsAreNotStable s
    return (slotsPerEpoch, instable, maxLL)
  if instable
    then pure ru
    else case ru of
      SNothing ->
        SJust
          <$> ( liftSTS $
                  fst
                    <$> createRUpd
                      slotsPerEpoch
                      b
                      es
                      (Coin $ fromIntegral maxLL)
                      WithoutRewardProvenance
              )
      SJust _ -> pure ru
