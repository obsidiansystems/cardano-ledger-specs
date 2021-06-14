{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Cardano.Ledger.Voltaire.Prototype.One where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    encodeListLen,
    decodeWord,
  )

import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Voltaire.Prototype.Class (Proposal(..), Votes(..), Submissions(..), Update(..))
import qualified Cardano.Ledger.Voltaire.Prototype.Class as Voltaire
import qualified Cardano.Ledger.Shelley.Constraints as Shelley (UsesPParams (PParamsDelta))
import Control.DeepSeq (NFData)
import Cardano.Ledger.Pretty
import Control.Monad (foldM)
import Control.Monad.Reader.Class
import Control.SetAlgebra (dom, eval, (⊆), (⨃))
import Control.State.Transition
import Data.Coders
  ( Decode (From, RecD),
    decode,
    (<!),
  )
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq((:<|)))
import qualified Data.Sequence.Strict as Seq
import Data.Set (Set)
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.Records
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.Keys
import Shelley.Spec.Ledger.LedgerState (pvCanFollow)
import qualified Shelley.Spec.Ledger.LedgerState as Shelley
import Shelley.Spec.Ledger.Slot
import qualified Shelley.Spec.Ledger.STS.Ppup as Shelley
import Shelley.Spec.Ledger.PParams (ProtVer)
import qualified Shelley.Spec.Ledger.PParams as Shelley
import Shelley.Spec.Ledger.Serialization (decodeRecordSum)
import Shelley.Spec.Ledger.STS.Utxo (UtxoEnv(..))

data ProposalHeader era = ProposalHeader
  { proposal_submitter :: KeyHash 'Genesis (Crypto era)
  , proposal_targetEpoch :: !EpochNo
  } deriving (Generic)

instance NFData (ProposalHeader era)
instance NoThunks (ProposalHeader era)
deriving instance Eq (ProposalHeader era)
deriving instance Show (ProposalHeader era)

instance Era era => ToCBOR (ProposalHeader era) where
  toCBOR (ProposalHeader sub te) =
    encodeListLen 2 <> toCBOR sub <> toCBOR te

instance
  Era era =>
  FromCBOR (ProposalHeader era)
  where
  fromCBOR = decode $ RecD ProposalHeader <! From <! From

instance Era era =>
  PrettyA (ProposalHeader era)
  where
  prettyA (ProposalHeader sub te) = ppSexp "ProposalHeader"
    [ prettyA sub
    , prettyA te
    ]

-- The first prototype simply implements the Shelley PPUP rules in the
-- Voltaire prototype scaffolding. In particular we do not try to incorporate
-- the central funds transfer logic, nor do we separate protocol version
-- upgrades from mere protocol parameter updates.
type ProposalBody era = Shelley.PParamsDelta era

-- In Shelley the genesis key delegates vote for a proposal by submitting it
-- identically. We mimic that behavior here.
type ProposalId era = ProposalBody era

data VotingPeriod = VoteForThisEpoch | VoteForNextEpoch
  deriving (Show, Eq, Generic)

instance NFData VotingPeriod
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
  = NonGenesisUpdatePPUP
      !(Set (KeyHash 'Genesis (Crypto era))) -- KeyHashes which are voting
      !(Set (KeyHash 'Genesis (Crypto era))) -- KeyHashes which should be voting
  | PPUpdateWrongEpoch
      !EpochNo -- current epoch
      !EpochNo -- intended epoch of update
      !VotingPeriod -- voting period within the epoch
  | PVCannotFollowPPUP
      !ProtVer -- the first bad protocol version
  | UnsupportedVotesPPUP
      !(Votes era)
  | VaryingTargetEpochPPUP
      !EpochNo
      !EpochNo
  | MultipleProposalsPPUP
      !(KeyHash 'Genesis (Crypto era))
  deriving (Show, Eq, Generic)

instance Voltaire.VoltaireClass era => NoThunks (PpupPredicateFailure era)

instance
  (Typeable era, Era era, Voltaire.VoltaireClass era) =>
  ToCBOR (PpupPredicateFailure era)
  where
  toCBOR = \case
    (NonGenesisUpdatePPUP a b) ->
      encodeListLen 3
        <> toCBOR (0 :: Word8)
        <> toCBOR a
        <> toCBOR b
    PPUpdateWrongEpoch ce e vp ->
      encodeListLen 4 <> toCBOR (1 :: Word8) <> toCBOR ce <> toCBOR e <> toCBOR vp
    PVCannotFollowPPUP p -> encodeListLen 2 <> toCBOR (2 :: Word8) <> toCBOR p
    UnsupportedVotesPPUP vs -> encodeListLen 2 <> toCBOR (3 :: Word8) <> toCBOR vs
    VaryingTargetEpochPPUP e0 e1 -> encodeListLen 3 <> toCBOR (4 :: Word8) <> toCBOR e0 <> toCBOR e1
    MultipleProposalsPPUP k -> encodeListLen 2 <> toCBOR (5 :: Word8) <> toCBOR k

instance
  (Era era, Voltaire.VoltaireClass era) =>
  FromCBOR (PpupPredicateFailure era)
  where
  fromCBOR = decodeRecordSum "PredicateFailure (PPUP era)" $
    \case
      0 -> do
        a <- fromCBOR
        b <- fromCBOR
        pure (3, NonGenesisUpdatePPUP a b)
      1 -> do
        a <- fromCBOR
        b <- fromCBOR
        c <- fromCBOR
        pure (4, PPUpdateWrongEpoch a b c)
      2 -> do
        p <- fromCBOR
        pure (2, PVCannotFollowPPUP p)
      3 -> do
        v <- fromCBOR
        pure (2, UnsupportedVotesPPUP v)
      4 -> do
        e0 <- fromCBOR
        e1 <- fromCBOR
        pure (3, VaryingTargetEpochPPUP e0 e1)
      5 -> do
        k <- fromCBOR
        pure (2, MultipleProposalsPPUP k)
      k -> invalidKey k

type PpupEnv era = Shelley.PPUPEnv era
type PpupState era = Shelley.PPUPState era

fromUtxoEnv :: UtxoEnv era -> PpupEnv era
fromUtxoEnv (UtxoEnv slot pp _ genDelegs) = Shelley.PPUPEnv slot pp genDelegs

ppupTransition ::
  ( Typeable era,
    Voltaire.VoltaireClass era,
    Voltaire.ProposalHeader era ~ ProposalHeader era,
    Voltaire.ProposalBody era ~ ProposalBody era,
    Voltaire.PpupPredicateFailure era ~ PpupPredicateFailure era,
    Voltaire.PpupState era ~ PpupState era,
    Voltaire.PpupEnv era ~ PpupEnv era,
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    HasField "_protocolVersion" (ProposalBody era) (StrictMaybe ProtVer)
  ) =>
  TransitionRule (Voltaire.PPUP era)
ppupTransition = do
  TRC
    ( Shelley.PPUPEnv slot pp (GenDelegs _genDelegs),
      Shelley.PPUPState (Shelley.ProposedPPUpdates pupS) (Shelley.ProposedPPUpdates fpupS),
      upM
      ) <-
    judgmentContext
  case upM of
    Nothing -> pure $ Shelley.PPUPState (Shelley.ProposedPPUpdates pupS) (Shelley.ProposedPPUpdates fpupS)
    Just (Update (Submissions ps) (Votes vs)) -> do
      -- Shelley does not have a notion of voting as distinct from submissions.
      -- Genesis key delegates reach a quorum by submitting identical proposals.
      Map.null vs ?! UnsupportedVotesPPUP (Votes vs)
      case ps of
        Seq.Empty -> pure $ Shelley.PPUPState (Shelley.ProposedPPUpdates pupS) (Shelley.ProposedPPUpdates fpupS)
      -- Here we have an impedance mismatch between the structure of proposal
      -- submission in Voltaire and in Shelley. In Shelley, a genesis key delegate
      -- can only submit one proposal per transaction in a map structure.
      -- In Voltaire, a submitter can submit as many proposals as they can fit in
      -- the transaction, and proposals are ordered within a transaction.
      --
      -- There is another mismatch in that the target epoch is uniform for all
      -- proposals submitted within a Shelley transaction, whereas the target can
      -- be freely set for Voltaire proposals.
      --
      -- We reconcile this situation by folding over the sequence of Voltaire
      -- proposals and fail if multiple proposals correspond to the same submitter
      -- or the target epoch is not uniform across all proposals.
        Proposal (ProposalHeader s0 te) d0 :<| ps' -> do
          let combineProposals m (Proposal (ProposalHeader submitter targetEpoch) paramUpdate) = do
                targetEpoch == te ?! VaryingTargetEpochPPUP te targetEpoch
                not (Map.member submitter m) ?! MultipleProposalsPPUP submitter
                pure (Map.insert submitter paramUpdate m)
          pup <- foldM combineProposals (Map.singleton s0 d0) ps'
          -- The rest of the logic after this reconciliation step is the same as Shelley
          eval (dom pup ⊆ dom _genDelegs) ?! NonGenesisUpdatePPUP (eval (dom pup)) (eval (dom _genDelegs))
          let goodPV =
                pvCanFollow (getField @"_protocolVersion" pp)
                  . getField @"_protocolVersion"
          let badPVs = Map.filter (not . goodPV) pup
          case Map.toList (Map.map (getField @"_protocolVersion") badPVs) of
            ((_, SJust pv) : _) -> failBecause $ PVCannotFollowPPUP pv
            _ -> pure ()
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
              pure $
                Shelley.PPUPState
                  (Shelley.ProposedPPUpdates (eval (pupS ⨃ pup)))
                  (Shelley.ProposedPPUpdates fpupS)
            else do
              currentEpoch + 1 == te ?! PPUpdateWrongEpoch currentEpoch te VoteForNextEpoch
              pure $
                Shelley.PPUPState
                  (Shelley.ProposedPPUpdates pupS)
                  (Shelley.ProposedPPUpdates (eval (fpupS ⨃ pup)))