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
import GHC.Records (HasField)
import Shelley.Spec.Ledger.BaseTypes
  ( Globals (..),
    ShelleyBase,
  )
import Shelley.Spec.Ledger.HardForks as HardForks
import Shelley.Spec.Ledger.LedgerState
  ( AccountState (..),
    InstantaneousRewards (..),
    availableAfterMIR,
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
    MIRTarget (..),
    MIRCert (MIRCert),
  )
import Shelley.Spec.Ledger.PParams
  ( ProtVer,
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
handleMIR ::
  ( HasField "_protocolVersion" pp ProtVer,
    STS sts,
    BaseM sts ~ ShelleyBase,
    PredicateFailure sts ~ predicateFailure
  )
  => (DelegMirPredicateFailure era -> predicateFailure)
  -> (SlotNo, AccountState, pp)
  -> InstantaneousRewards crypto
  -> MIRCert crypto
  -> Rule sts rtype (InstantaneousRewards crypto)
handleMIR wrapFail (slot, acnt, pp) irwd (MIRCert targetPot (StakeAddressesMIR credCoinMap)) =
    if HardForks.allowMIRTransfer pp
      then do
        sp <- liftSTS $ asks stabilityWindow
        firstSlot <- liftSTS $ do
          ei <- asks epochInfo
          EpochNo currEpoch <- epochInfoEpoch ei slot
          epochInfoFirst ei $ EpochNo (currEpoch + 1)
        let tooLate = firstSlot *- Duration sp
        slot < tooLate
          ?! wrapFail (MIRCertificateTooLateinEpochDELEG slot tooLate)

        let (potAmount, delta, instantaneousRewards) =
              case targetPot of
                ReservesMIR -> (_reserves acnt, deltaReserves irwd, iRReserves irwd)
                TreasuryMIR -> (_treasury acnt, deltaTreasury irwd, iRTreasury irwd)
            credCoinMap' = Map.map (\(DeltaCoin x) -> Coin x) credCoinMap
            combinedMap = Map.unionWith (<>) credCoinMap' instantaneousRewards
            requiredForRewards = fold combinedMap
            available = potAmount `addDeltaCoin` delta

        all (>= mempty) combinedMap ?! wrapFail MIRProducesNegativeUpdate

        requiredForRewards <= available
          ?! wrapFail (InsufficientForInstantaneousRewardsDELEG targetPot requiredForRewards available)

        case targetPot of
          ReservesMIR -> pure $ irwd {iRReserves = combinedMap}
          TreasuryMIR -> pure $ irwd {iRTreasury = combinedMap}
      else do
        sp <- liftSTS $ asks stabilityWindow
        firstSlot <- liftSTS $ do
          ei <- asks epochInfo
          EpochNo currEpoch <- epochInfoEpoch ei slot
          epochInfoFirst ei $ EpochNo (currEpoch + 1)
        let tooLate = firstSlot *- Duration sp
        slot < tooLate
          ?! wrapFail (MIRCertificateTooLateinEpochDELEG slot tooLate)

        all (>= mempty) credCoinMap ?! wrapFail MIRNegativesNotCurrentlyAllowed

        let (potAmount, instantaneousRewards) =
              case targetPot of
                ReservesMIR -> (_reserves acnt, iRReserves irwd)
                TreasuryMIR -> (_treasury acnt, iRTreasury irwd)
        let credCoinMap' = Map.map (\(DeltaCoin x) -> Coin x) credCoinMap
            combinedMap = Map.union credCoinMap' instantaneousRewards
            requiredForRewards = fold combinedMap
        requiredForRewards <= potAmount
          ?! wrapFail (InsufficientForInstantaneousRewardsDELEG targetPot requiredForRewards potAmount)

        case targetPot of
          ReservesMIR -> pure $ irwd {iRReserves = combinedMap}
          TreasuryMIR -> pure $ irwd {iRTreasury = combinedMap}
handleMIR wrapFail (slot, acnt, pp) irwd (MIRCert targetPot (SendToOppositePotMIR coin)) =
      if HardForks.allowMIRTransfer pp
        then do
          sp <- liftSTS $ asks stabilityWindow
          firstSlot <- liftSTS $ do
            ei <- asks epochInfo
            EpochNo currEpoch <- epochInfoEpoch ei slot
            epochInfoFirst ei $ EpochNo (currEpoch + 1)
          let tooLate = firstSlot *- Duration sp
          slot < tooLate
            ?! wrapFail (MIRCertificateTooLateinEpochDELEG slot tooLate)

          let available = availableAfterMIR targetPot acnt irwd
          coin <= available
            ?! wrapFail (InsufficientForTransferDELEG targetPot coin available)

          let dr = deltaReserves irwd
              dt = deltaTreasury irwd
          case targetPot of
            ReservesMIR ->
              pure $
                irwd
                  { deltaReserves = dr <> (invert $ toDeltaCoin coin),
                      deltaTreasury = dt <> (toDeltaCoin coin)
                  }
            TreasuryMIR ->
              pure $
                irwd
                  { deltaReserves = dr <> (toDeltaCoin coin),
                      deltaTreasury = dt <> (invert $ toDeltaCoin coin)
                  }
        else do
          failBecause $ wrapFail MIRTransferNotCurrentlyAllowed
          pure irwd
