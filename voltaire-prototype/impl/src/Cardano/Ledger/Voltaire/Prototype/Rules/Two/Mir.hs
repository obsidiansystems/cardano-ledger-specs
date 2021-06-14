{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

module Cardano.Ledger.Voltaire.Prototype.Rules.Two.Mir
  ( handleMIR,
    DelegMirPredicateFailure (..)
  )
where

import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..), addDeltaCoin, toDeltaCoin)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition
import Data.Foldable (fold)
import Data.Group (Group (..))
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes
  ( Globals (..),
  )
import Shelley.Spec.Ledger.HardForks as HardForks
import Shelley.Spec.Ledger.LedgerState
  ( AccountState (..),
    InstantaneousRewards (..),
    availableAfterMIR,
    _irwd,
  )
import Shelley.Spec.Ledger.Slot
  ( Duration (..),
    EpochNo (..),
    SlotNo,
    epochInfoEpoch,
    epochInfoFirst,
    (*-),
  )
import Shelley.Spec.Ledger.TxBody
  ( MIRPot (..),
    MIRTarget (..)
  )

-- | The MIR-related constructors that
--   were removed from 'DelegPredicateFailure'.
data DelegMirPredicateFailure era
  = InsufficientForInstantaneousRewardsDELEG
      !MIRPot -- which pot the rewards are to be drawn from, treasury or reserves
      !Coin -- amount of rewards to be given out
      !Coin -- size of the pot from which the lovelace is drawn
  | MIRCertificateTooLateinEpochDELEG
      !SlotNo -- current slot
      !SlotNo -- Core.EraRule "MIR" must be submitted before this slot
  | MIRTransferNotCurrentlyAllowed
  | MIRNegativesNotCurrentlyAllowed
  | InsufficientForTransferDELEG
      !MIRPot -- which pot the rewards are to be drawn from, treasury or reserves
      !Coin -- amount attempted to transfer
      !Coin -- amount available
  | MIRProducesNegativeUpdate
  deriving (Show, Eq, Generic)

-- | Handles the MIR-related stuff that was removed from
-- 'Cardano.Ledger.Voltaire.Prototype.Rules.Two.Deleg.delegationTransition'
handleMIR (slot, acnt, pp) ds targetPot (StakeAddressesMIR credCoinMap) =
    if HardForks.allowMIRTransfer pp
      then do
        sp <- liftSTS $ asks stabilityWindow
        firstSlot <- liftSTS $ do
          ei <- asks epochInfo
          EpochNo currEpoch <- epochInfoEpoch ei slot
          epochInfoFirst ei $ EpochNo (currEpoch + 1)
        let tooLate = firstSlot *- Duration sp
        slot < tooLate
          ?! MIRCertificateTooLateinEpochDELEG slot tooLate

        let (potAmount, delta, instantaneousRewards) =
              case targetPot of
                ReservesMIR -> (_reserves acnt, deltaReserves . _irwd $ ds, iRReserves $ _irwd ds)
                TreasuryMIR -> (_treasury acnt, deltaTreasury . _irwd $ ds, iRTreasury $ _irwd ds)
            credCoinMap' = Map.map (\(DeltaCoin x) -> Coin x) credCoinMap
            combinedMap = Map.unionWith (<>) credCoinMap' instantaneousRewards
            requiredForRewards = fold combinedMap
            available = potAmount `addDeltaCoin` delta

        all (>= mempty) combinedMap ?! MIRProducesNegativeUpdate

        requiredForRewards <= available
          ?! InsufficientForInstantaneousRewardsDELEG targetPot requiredForRewards available

        case targetPot of
          ReservesMIR -> pure $ ds {_irwd = (_irwd ds) {iRReserves = combinedMap}}
          TreasuryMIR -> pure $ ds {_irwd = (_irwd ds) {iRTreasury = combinedMap}}
      else do
        sp <- liftSTS $ asks stabilityWindow
        firstSlot <- liftSTS $ do
          ei <- asks epochInfo
          EpochNo currEpoch <- epochInfoEpoch ei slot
          epochInfoFirst ei $ EpochNo (currEpoch + 1)
        let tooLate = firstSlot *- Duration sp
        slot < tooLate
          ?! MIRCertificateTooLateinEpochDELEG slot tooLate

        all (>= mempty) credCoinMap ?! MIRNegativesNotCurrentlyAllowed

        let (potAmount, instantaneousRewards) =
              case targetPot of
                ReservesMIR -> (_reserves acnt, iRReserves $ _irwd ds)
                TreasuryMIR -> (_treasury acnt, iRTreasury $ _irwd ds)
        let credCoinMap' = Map.map (\(DeltaCoin x) -> Coin x) credCoinMap
            combinedMap = Map.union credCoinMap' instantaneousRewards
            requiredForRewards = fold combinedMap
        requiredForRewards <= potAmount
          ?! InsufficientForInstantaneousRewardsDELEG targetPot requiredForRewards potAmount

        case targetPot of
          ReservesMIR -> pure $ ds {_irwd = (_irwd ds) {iRReserves = combinedMap}}
          TreasuryMIR -> pure $ ds {_irwd = (_irwd ds) {iRTreasury = combinedMap}}
handleMIR (slot, acnt, pp) ds targetPot (SendToOppositePotMIR coin) =
      if HardForks.allowMIRTransfer pp
        then do
          sp <- liftSTS $ asks stabilityWindow
          firstSlot <- liftSTS $ do
            ei <- asks epochInfo
            EpochNo currEpoch <- epochInfoEpoch ei slot
            epochInfoFirst ei $ EpochNo (currEpoch + 1)
          let tooLate = firstSlot *- Duration sp
          slot < tooLate
            ?! MIRCertificateTooLateinEpochDELEG slot tooLate

          let available = availableAfterMIR targetPot acnt (_irwd ds)
          coin <= available
            ?! InsufficientForTransferDELEG targetPot coin available

          let ir = _irwd ds
              dr = deltaReserves ir
              dt = deltaTreasury ir
          case targetPot of
            ReservesMIR ->
              pure $
                ds
                  { _irwd =
                      ir
                        { deltaReserves = dr <> (invert $ toDeltaCoin coin),
                          deltaTreasury = dt <> (toDeltaCoin coin)
                        }
                  }
            TreasuryMIR ->
              pure $
                ds
                  { _irwd =
                      ir
                        { deltaReserves = dr <> (toDeltaCoin coin),
                          deltaTreasury = dt <> (invert $ toDeltaCoin coin)
                        }
                  }
        else do
          failBecause MIRTransferNotCurrentlyAllowed
          pure ds
