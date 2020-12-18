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

module Shelley.Spec.Ledger.STS.NewEpoch
  ( NEWEPOCH,
    NewEpochPredicateFailure (..),
    PredicateFailure,
    calculatePoolDistr,
  )
where

import Cardano.Ledger.Era (Crypto)
import Cardano.Ledger.Shelley.Constraints (ShelleyBased)
import qualified Cardano.Ledger.Val as Val
import Control.State.Transition
import Data.Foldable (fold)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.Coin
import Shelley.Spec.Ledger.Delegation.Certificates
import Shelley.Spec.Ledger.EpochBoundary
import Shelley.Spec.Ledger.LedgerState
import Shelley.Spec.Ledger.STS.Epoch
import Shelley.Spec.Ledger.STS.Mir
import Shelley.Spec.Ledger.Slot
import Shelley.Spec.Ledger.TxBody

data NEWEPOCH era

data NewEpochPredicateFailure era
  = EpochFailure (PredicateFailure (EPOCH era)) -- Subtransition Failures
  | CorruptRewardUpdate
      !(RewardUpdate (Crypto era)) -- The reward update which violates an invariant
  | MirFailure (PredicateFailure (MIR era)) -- Subtransition Failures
  deriving (Generic)

deriving stock instance
  Show (NewEpochPredicateFailure era)

deriving stock instance
  Eq (NewEpochPredicateFailure era)

instance NoThunks (NewEpochPredicateFailure era)

instance ShelleyBased era => STS (NEWEPOCH era) where
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
          emptyEpochState
          SNothing
          (PoolDistr Map.empty)
    ]

  transitionRules = [newEpochTransition]

newEpochTransition ::
  forall era.
  ( ShelleyBased era
  ) =>
  TransitionRule (NEWEPOCH era)
newEpochTransition = do
  TRC
    ( _,
      src@(NewEpochState eL _ bcur es0 ru _pd),
      e
      ) <-
    judgmentContext
  case compare e eL of
    LT -> error $ "bad epoch " <> show (e, eL)
    EQ -> pure src
    GT -> do
      es1 <- case ru of
        SNothing -> pure es0
        SJust ru' -> do
          let RewardUpdate dt dr rs_ df _ = ru'
          Val.isZero (dt <> (dr <> (toDeltaCoin $ fold rs_) <> df)) ?! CorruptRewardUpdate ru'
          pure $ applyRUpd ru' es0

      es2 <- trans @(MIR era) $ TRC ((), es1, ())
      es3 <- trans @(EPOCH era) $ TRC ((), es2, eL + 1)
      let EpochState _acnt ss _ls _pr _ _ = es3
          pd' = calculatePoolDistr (_pstakeSet ss)
      es4 <- if e == eL + 1 then pure es3 else
          -- TODO also update pd' again for the next epoch?
          trans @(EPOCH era) $ TRC ((), es3, eL + 2)
      pure $
        NewEpochState
          e
          bcur
          (BlocksMade Map.empty)
          es4
          SNothing
          pd'

calculatePoolDistr :: SnapShot crypto -> PoolDistr crypto
calculatePoolDistr (SnapShot (Stake stake) delegs poolParams) =
  let Coin total = Map.foldl' (<>) mempty stake
      sd =
        Map.fromListWith (+) $
          catMaybes
            [ (,fromIntegral c / fromIntegral (if total == 0 then 1 else total))
                <$> Map.lookup hk delegs -- TODO mgudemann total could be zero (in
                -- particular when shrinking)
              | (hk, Coin c) <- Map.toList stake
            ]
   in PoolDistr $ Map.intersectionWith IndividualPoolStake sd (Map.map _poolVrf poolParams)

instance
  ShelleyBased era =>
  Embed (EPOCH era) (NEWEPOCH era)
  where
  wrapFailed = EpochFailure

instance
  ShelleyBased era =>
  Embed (MIR era) (NEWEPOCH era)
  where
  wrapFailed = MirFailure
