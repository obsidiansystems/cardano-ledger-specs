{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Voltaire.Prototype.Rules.Ppup
  ( PPUP,
    PPUPEnv (..),
    PpupPredicateFailure (..),
    PredicateFailure,
    VotingPeriod (..),
  )
where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    decodeWord,
    encodeListLen,
  )
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era)
import Cardano.Ledger.Shelley.Constraints (PParamsDelta)
import Cardano.Ledger.Voltaire.Prototype.Class
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.Records
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.PParams hiding (Update)
import Shelley.Spec.Ledger.Serialization (decodeRecordSum)
import Shelley.Spec.Ledger.Slot

data PPUP era

data VotingPeriod = VoteForThisEpoch | VoteForNextEpoch
  deriving (Show, Eq, Generic)

instance NoThunks VotingPeriod

instance ToCBOR VotingPeriod where
  toCBOR VoteForThisEpoch = toCBOR (0 :: Word8)
  toCBOR VoteForNextEpoch = toCBOR (1 :: Word8)

instance FromCBOR VotingPeriod where
  fromCBOR =
    decodeWord >>= \case
      0 -> pure VoteForThisEpoch
      1 -> pure VoteForNextEpoch
      k -> invalidKey k

data PpupPredicateFailure era
  = PPUpdateSubmissionPredicateFailure (SubmissionPredicateFailure era)
  | PPUpdateVotePredicateFailure (VotePredicateFailure era)
  | PPUpdateWrongEpoch
      !EpochNo -- current epoch
      !EpochNo -- intended epoch of update
      !VotingPeriod -- voting period within the epoch
  deriving (Generic)

deriving instance (Show (SubmissionPredicateFailure era), Show (VotePredicateFailure era)) => Show (PpupPredicateFailure era)
deriving instance VoltaireClass era => Eq (PpupPredicateFailure era)

instance VoltaireClass era => NoThunks (PpupPredicateFailure era)

instance
  ( Typeable era,
    VoltaireClass era,
    Show (SubmissionPredicateFailure era),
    Show (VotePredicateFailure era),
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    HasField "_protocolVersion" (PParamsDelta era) (StrictMaybe ProtVer)
  ) =>
  STS (PPUP era)
  where
  type State (PPUP era) = PPUPState era
  type Signal (PPUP era) = Maybe (Update era)
  type Environment (PPUP era) = PPUPEnv era
  type BaseM (PPUP era) = ShelleyBase
  type PredicateFailure (PPUP era) = PpupPredicateFailure era

  initialRules = []

  transitionRules = [ppupTransitionNonEmpty]

instance
  (Typeable era, Era era, VoltaireClass era) =>
  ToCBOR (PpupPredicateFailure era)
  where
  toCBOR = \case
    PPUpdateSubmissionPredicateFailure a ->
      encodeListLen 2
        <> toCBOR (0 :: Word8)
        <> toCBOR a
    PPUpdateVotePredicateFailure a ->
      encodeListLen 2
        <> toCBOR (1 :: Word8)
        <> toCBOR a
    PPUpdateWrongEpoch ce e vp ->
      encodeListLen 4 <> toCBOR (2 :: Word8) <> toCBOR ce <> toCBOR e <> toCBOR vp

instance
  (Era era, VoltaireClass era) =>
  FromCBOR (PpupPredicateFailure era)
  where
  fromCBOR = decodeRecordSum "PredicateFailure (PPUP era)" $
    \case
      0 -> do
        a <- fromCBOR
        pure (2, PPUpdateSubmissionPredicateFailure a)
      1 -> do
        a <- fromCBOR
        pure (2, PPUpdateVotePredicateFailure a)
      2 -> do
        a <- fromCBOR
        b <- fromCBOR
        c <- fromCBOR
        pure (4, PPUpdateWrongEpoch a b c)
      k -> invalidKey k

ppupTransitionNonEmpty ::
  ( Typeable era,
    VoltaireClass era,
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    HasField "_protocolVersion" (PParamsDelta era) (StrictMaybe ProtVer)
  ) =>
  TransitionRule (PPUP era)
ppupTransitionNonEmpty = do
  TRC
    ( env@(PPUPEnv slot _ _),
      state,
      up
      ) <-
    judgmentContext

  case up of
    Nothing -> pure state
    Just (Update submissions votes te) -> do
      case validateSubmissions env submissions of
        Nothing -> pure ()
        Just err -> failBecause (PPUpdateSubmissionPredicateFailure err)
      case validateVotes env votes of
        Nothing -> pure ()
        Just err -> failBecause (PPUpdateVotePredicateFailure err)

      sp <- liftSTS $ asks stabilityWindow
      firstSlotNextEpoch <- liftSTS $ do
        ei <- asks epochInfo
        EpochNo e <- epochInfoEpoch ei slot
        epochInfoFirst ei (EpochNo $ e + 1)
      let tooLate = firstSlotNextEpoch *- (Duration (2 * sp))

      currentEpoch <- liftSTS $ do
        ei <- asks epochInfo
        epochInfoEpoch ei slot

      if slot < tooLate
        then do
          currentEpoch == te ?! PPUpdateWrongEpoch currentEpoch te VoteForThisEpoch
          pure $ updateState env submissions votes state
        else do
          currentEpoch + 1 == te ?! PPUpdateWrongEpoch currentEpoch te VoteForNextEpoch
          pure $ updateState env submissions votes state
