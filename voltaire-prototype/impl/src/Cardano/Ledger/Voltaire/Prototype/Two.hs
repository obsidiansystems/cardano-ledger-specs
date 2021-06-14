{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
module Cardano.Ledger.Voltaire.Prototype.Two where

import Cardano.Ledger.Era
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Voltaire.Prototype.Class (Proposal(..), Votes(..), Submissions(..), Update(..))
import qualified Cardano.Ledger.Voltaire.Prototype.One as One
import qualified Cardano.Ledger.Voltaire.Prototype.Class as Voltaire
import qualified Cardano.Ledger.Shelley.Constraints as Shelley (UsesPParams (PParamsDelta))
import Control.Monad (foldM_)
import Control.State.Transition
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq((:<|)))
import qualified Data.Sequence.Strict as Seq
import Data.Typeable (Typeable)
import GHC.Records
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.Keys
import qualified Shelley.Spec.Ledger.LedgerState as Shelley
import qualified Shelley.Spec.Ledger.STS.Ppup as Shelley
import Shelley.Spec.Ledger.PParams (ProtVer)
import qualified Shelley.Spec.Ledger.TxBody as Shelley
import Shelley.Spec.Ledger.STS.Utxo (UtxoEnv(..))
import Data.Foldable (toList)
import Cardano.Prelude (catMaybes, Generic, Map)

-- The second prototype implements the Shelley PPUP rules and MIRs.
data ProposalBody era
  = BodyPPUP (Shelley.PParamsDelta era)
  | BodyMIR (Shelley.MIRCert era)

isPPUPBody :: ProposalBody era -> Bool
isPPUPBody (BodyPPUP _) = True
isPPUPBody _ = False

isMIRBody :: ProposalBody era -> Bool
isMIRBody (BodyMIR _) = True
isMIRBody _ = False

-- In Shelley the genesis key delegates vote for a proposal by submitting it
-- identically. We mimic that behavior here.
type ProposalId era = ProposalBody era

type PpupEnv era = Shelley.PPUPEnv era
type PpupState era = UpdateState era

data UpdateState era = UpdateState
  { proposals :: !(ProposedUpdates era),
    futureProposals :: !(ProposedUpdates era)
  }
  deriving (Generic)

-- | 
newtype ProposedUpdates era
  = ProposedUpdates (Map (KeyHash 'Genesis (Crypto era)) (ProposalBody era))
  deriving (Generic)

fromUtxoEnv :: UtxoEnv era -> PpupEnv era
fromUtxoEnv (UtxoEnv slot pp _ genDelegs) = Shelley.PPUPEnv slot pp genDelegs

ppupTransition ::
  ( Typeable era,
    Voltaire.VoltaireClass era,
    Voltaire.ProposalHeader era ~ One.ProposalHeader era,
    Voltaire.ProposalBody era ~ ProposalBody era,
    Voltaire.PpupPredicateFailure era ~ One.PpupPredicateFailure era,
    Voltaire.PpupState era ~ PpupState era,
    Voltaire.PpupEnv era ~ PpupEnv era,
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    HasField "_protocolVersion" (Shelley.PParamsDelta era) (StrictMaybe ProtVer)
  ) =>
  TransitionRule (Voltaire.PPUP era)
ppupTransition = do
  TRC (ppupEnv, ppupState, upM) <- judgmentContext
  case upM of
    Nothing -> pure ppupState
    Just (Update (Submissions ps) (Votes vs)) -> do
      -- Shelley does not have a notion of voting as distinct from submissions.
      -- Genesis key delegates reach a quorum by submitting identical proposals.
      Map.null vs ?! One.UnsupportedVotesPPUP (Votes vs)

      case ps of
        Seq.Empty -> pure ppupState
        Proposal (One.ProposalHeader _ te) _ :<| _ -> do
          consistentHeaders te (fmap (\(Proposal h _) -> h) ps)
          -- Handle PPUPs
          let ppupProposal (Proposal h (BodyPPUP ppup)) = Just (h, ppup)
              ppupProposal (Proposal _ (BodyMIR _)) = Nothing
          ppupState' <- One.handlePpup ppupEnv ppupState (Seq.fromList . catMaybes $ map ppupProposal (toList ps))
          -- Handle MIRs
          error "TODO"
          pure ppupState'
 where
  -- "combineProposals" copy/pasted from One
  consistentHeaders te proposalHeaders = do
    let consistentHeader m (One.ProposalHeader submitter targetEpoch) = do
          targetEpoch == te ?! One.VaryingTargetEpochPPUP te targetEpoch
          not (Map.member submitter m) ?! One.MultipleProposalsPPUP submitter
          pure (Map.insert submitter () m)
    foldM_ consistentHeader Map.empty proposalHeaders
