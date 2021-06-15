{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
module Cardano.Ledger.Voltaire.Prototype.Two where

import Cardano.Ledger.Era
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Voltaire.Prototype.Class (Proposal(..), Votes(..), Submissions(..), Update(..))
import qualified Cardano.Ledger.Voltaire.Prototype.One as One
import qualified Cardano.Ledger.Voltaire.Prototype.Class as Voltaire
import qualified Cardano.Ledger.Shelley.Constraints as Shelley (UsesPParams (PParamsDelta))
import Control.State.Transition
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq((:<|)))
import qualified Data.Sequence.Strict as Seq
import Data.Typeable (Typeable)
import GHC.Records
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.Keys
import Shelley.Spec.Ledger.Slot
import qualified Shelley.Spec.Ledger.STS.Ppup as Shelley
import Shelley.Spec.Ledger.PParams (ProtVer)
import qualified Shelley.Spec.Ledger.TxBody as Shelley
import Shelley.Spec.Ledger.STS.Utxo (UtxoEnv(..))
import Cardano.Prelude (Generic, Map)
import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
  )

import Control.DeepSeq (NFData)
import Cardano.Ledger.Pretty
import Control.Monad (foldM)
import Control.Monad.Reader.Class
import Control.SetAlgebra (dom, eval, (⊆), (⨃))
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.LedgerState (pvCanFollow)
import qualified Shelley.Spec.Ledger.STS.Deleg as Shelley -- TMP
import qualified Shelley.Spec.Ledger.LedgerState as Shelley -- TMP
import qualified Cardano.Ledger.Voltaire.Prototype.Rules.Two.Mir

-- | The second prototype implements the Shelley PPUP rules and MIRs
data ProposalBody era
  = BodyPPUP (Shelley.PParamsDelta era)
  -- | TODO: 'MIRCert' contains the info we need but should be renamed/moved to avoid confusion
  | BodyMIR (Shelley.MIRCert era)
    deriving (Generic)

deriving instance Eq (Shelley.PParamsDelta era) => Eq (ProposalBody era)

-- | TODO: actually implement orphan (required because a ProposalId must be orderable)
instance Ord (Shelley.MIRCert era) where
  compare = error "TODO"

deriving instance Ord (Shelley.PParamsDelta era) => Ord (ProposalBody era)

deriving instance Show (Shelley.PParamsDelta era) => Show (ProposalBody era)

deriving instance NFData (Shelley.PParamsDelta era) => NFData (ProposalBody era)

instance NoThunks (Shelley.PParamsDelta era) => NoThunks (ProposalBody era)

instance PrettyA (ProposalBody era) where
  prettyA = error "TODO"

instance (Era era, ToCBOR (Shelley.PParamsDelta era)) => ToCBOR (ProposalBody era) where
  toCBOR _ =
    error "TODO"

instance
  (Era era, FromCBOR (Shelley.PParamsDelta era)) =>
  FromCBOR (ProposalBody era)
  where
  fromCBOR =
    error "TODO"

bodyPParamsDelta :: ProposalBody era -> Maybe (Shelley.PParamsDelta era)
bodyPParamsDelta (BodyPPUP pParams) = Just pParams
bodyPParamsDelta _ = Nothing

-- In Shelley the genesis key delegates vote for a proposal by submitting it
-- identically. We mimic that behavior here.
type ProposalId era = ProposalBody era

data PPUPState era = PPUPState
  { proposals :: !(ProposedUpdates era),
    futureProposals :: !(ProposedUpdates era)
  }
  deriving (Generic)

deriving instance Show (Shelley.PParamsDelta era) => Show (PPUPState era)

deriving instance Eq (Shelley.PParamsDelta era) => Eq (PPUPState era)

deriving instance NFData (Shelley.PParamsDelta era) => NFData (PPUPState era)

instance NoThunks (Shelley.PParamsDelta era) => NoThunks (PPUPState era)

instance (Era era, ToCBOR (Shelley.PParamsDelta era)) => ToCBOR (PPUPState era) where
  toCBOR _ =
    error "TODO"

instance
  (Era era, FromCBOR (Shelley.PParamsDelta era)) =>
  FromCBOR (PPUPState era)
  where
  fromCBOR =
    error "TODO"

-- |
newtype ProposedUpdates era
  = ProposedUpdates (Map (KeyHash 'Genesis (Crypto era)) (ProposalBody era))
  deriving (Generic)

deriving instance Show (Shelley.PParamsDelta era) => Show (ProposedUpdates era)

deriving instance Eq (Shelley.PParamsDelta era) => Eq (ProposedUpdates era)

deriving instance NFData (Shelley.PParamsDelta era) => NFData (ProposedUpdates era)

instance NoThunks (Shelley.PParamsDelta era) => NoThunks (ProposedUpdates era)

instance
  (Era era, ToCBOR (Shelley.PParamsDelta era)) =>
  ToCBOR (ProposedUpdates era)
  where
  toCBOR _ = error "TODO"

instance
  (Era era, FromCBOR (Shelley.PParamsDelta era)) =>
  FromCBOR (ProposedUpdates era)
  where
  fromCBOR = error "TODO"

fromUtxoEnv :: UtxoEnv era -> One.PpupEnv era
fromUtxoEnv (UtxoEnv slot pp _ genDelegs) = Shelley.PPUPEnv slot pp genDelegs

data PpupPredicateFailure era
  = UpdatePPUPFailure (One.PpupPredicateFailure era)
  | UpdateMIRFailure (Cardano.Ledger.Voltaire.Prototype.Rules.Two.Mir.DelegMirPredicateFailure era)
    deriving (Eq, Show)

-- | Identical to Cardano.Ledger.Voltaire.Prototype.One.ppupTransition
--   except for:
--      * different 'ProposalBody'
--      * different 'PpupState'
ppupTransition ::
  ( Typeable era,
    Voltaire.VoltaireClass era,
    Voltaire.ProposalHeader era ~ One.ProposalHeader era,
    Voltaire.ProposalBody era ~ ProposalBody era,
    Voltaire.PpupPredicateFailure era ~ PpupPredicateFailure era,
    Voltaire.PpupState era ~ PPUPState era,
    Voltaire.PpupEnv era ~ One.PpupEnv era,
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    HasField "_protocolVersion" (Shelley.PParamsDelta era) (StrictMaybe ProtVer)
  ) =>
  TransitionRule (Voltaire.PPUP era)
ppupTransition = do
  TRC
    ( Shelley.PPUPEnv slot pp (GenDelegs _genDelegs),
      updateState@(PPUPState (ProposedUpdates pupS) (ProposedUpdates fpupS)),
      upM
      ) <-
    judgmentContext
  case upM of
    Nothing -> pure updateState
    Just (Update (Submissions ps) (Votes vs)) -> do
      Map.null vs ?! ppupFail (One.UnsupportedVotesPPUP $ Votes vs)
      case ps of
        Seq.Empty -> pure updateState
        Proposal (One.ProposalHeader s0 te) d0 :<| ps' -> do
          let combineProposals m (Proposal (One.ProposalHeader submitter targetEpoch) paramUpdate) = do
                targetEpoch == te ?! ppupFail (One.VaryingTargetEpochPPUP te targetEpoch)
                not (Map.member submitter m) ?! ppupFail (One.MultipleProposalsPPUP submitter)
                pure (Map.insert submitter paramUpdate m)
          pup <- foldM combineProposals (Map.singleton s0 d0) ps'
          -- The rest of the logic after this reconciliation step is the same as Shelley
          eval (dom pup ⊆ dom _genDelegs) ?! ppupFail (One.NonGenesisUpdatePPUP (eval (dom pup)) (eval (dom _genDelegs)))
          let goodPV =
                pvCanFollow (getField @"_protocolVersion" pp)
                  . getField @"_protocolVersion"
          let badPVs = Map.filter (not . goodPV) (Map.mapMaybe bodyPParamsDelta pup)
          case Map.toList (Map.map (getField @"_protocolVersion") badPVs) of
            ((_, SJust pv) : _) -> failBecause $ ppupFail $ One.PVCannotFollowPPUP pv
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
              currentEpoch == te ?! ppupFail (One.PPUpdateWrongEpoch currentEpoch te One.VoteForThisEpoch)
              pure $
                PPUPState
                  (ProposedUpdates (eval (pupS ⨃ pup)))
                  (ProposedUpdates fpupS)
            else do
              currentEpoch + 1 == te ?! ppupFail (One.PPUpdateWrongEpoch currentEpoch te One.VoteForNextEpoch)
              pure $
                PPUPState
                  (ProposedUpdates pupS)
                  (ProposedUpdates (eval (fpupS ⨃ pup)))
 where
  ppupFail = UpdatePPUPFailure