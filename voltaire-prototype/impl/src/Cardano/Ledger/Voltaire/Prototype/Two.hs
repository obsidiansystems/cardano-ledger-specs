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
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- Shelley.MIRCert instances
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Cardano.Ledger.Voltaire.Prototype.Two
( module Cardano.Ledger.Voltaire.Prototype.Two
, Shelley.MIRCert
)
where

import Cardano.Ledger.Era
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC (Crypto)
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
import Data.Default.Class (Default(def))
import Data.Coders
import Generic.Data (Generically(..))

-- | The second prototype implements the Shelley PPUP rules and MIRs
data ProposalBody era
  = BodyPPUP (Shelley.PParamsDelta era)
  -- | TODO: 'MIRCert' contains the info we need but should be renamed/moved to avoid confusion
  | BodyMIR (Shelley.MIRCert (Crypto era))
    deriving (Generic)

deriving instance Eq (Shelley.PParamsDelta era) => Eq (ProposalBody era)

deriving instance Ord (Shelley.PParamsDelta era) => Ord (ProposalBody era)

deriving via (Generically (Shelley.MIRCert crypto)) instance Ord (Shelley.MIRCert crypto)
deriving via (Generically (Shelley.MIRTarget crypto)) instance Ord (Shelley.MIRTarget crypto)
deriving via (Generically (Shelley.MIRPot)) instance Ord (Shelley.MIRPot)

deriving instance Show (Shelley.PParamsDelta era) => Show (ProposalBody era)

deriving instance NFData (Shelley.PParamsDelta era) => NFData (ProposalBody era)

instance NoThunks (Shelley.PParamsDelta era) => NoThunks (ProposalBody era)

instance PrettyA (Shelley.PParamsDelta era) => PrettyA (ProposalBody era) where
  prettyA = ppProposalBody
    where
      ppProposalBody (BodyPPUP pParamsDelta) =
        ppSexp "BodyPPUP" [prettyA pParamsDelta]
      ppProposalBody (BodyMIR mirCert) =
        ppSexp "BodyMIR" [prettyA mirCert]


instance
  ( Typeable (Crypto era)
  , Typeable era
  , ToCBOR (Shelley.PParamsDelta era)
  , CC.Crypto (Crypto era)
  ) => ToCBOR (ProposalBody era) where
  toCBOR x = encode (encodePB x)
    where
    encodePB :: ToCBOR (Shelley.PParamsDelta era)
             => CC.Crypto (Crypto era)
             => ProposalBody era
             -> Encode 'Open (ProposalBody era)
    encodePB (BodyPPUP i) = Sum BodyPPUP 0 !> To i
    encodePB (BodyMIR s) = Sum BodyMIR 1 !> To s

instance
  ( CC.Crypto (Crypto era)
  , Typeable (Crypto era)
  , Typeable era
  , FromCBOR (Shelley.PParamsDelta era)
  ) =>
  FromCBOR (ProposalBody era)
  where
  fromCBOR = decode (Summands "VoltaireTwo ProposalBody" decodePB)
    where
    decodePB 0 = SumD BodyPPUP <! From
    decodePB 1 = SumD BodyMIR <! From
    decodePB n = Invalid n


bodyPParamsDelta :: ProposalBody era -> Maybe (Shelley.PParamsDelta era)
bodyPParamsDelta (BodyPPUP pParams) = Just pParams
bodyPParamsDelta _ = Nothing

bodyMirCert :: ProposalBody era -> Maybe (Shelley.MIRCert (Crypto era))
bodyMirCert (BodyMIR mirCert) = Just mirCert
bodyMirCert _ = Nothing

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

instance Default (PPUPState era) where
  def = PPUPState emptyPUpdates emptyPUpdates
    where
    emptyPUpdates :: ProposedUpdates era
    emptyPUpdates = ProposedUpdates Map.empty

instance (Era era, ToCBOR (Shelley.PParamsDelta era)) => ToCBOR (PPUPState era) where
  toCBOR (PPUPState m s) =
    encode $ Rec PPUPState !> To m !> To s

instance
  (Era era, FromCBOR (Shelley.PParamsDelta era), FromCBOR (ProposalBody era)) =>
  FromCBOR (PPUPState era)
  where
  fromCBOR =
    decode $ RecD PPUPState <! From <! From

-- |
newtype ProposedUpdates era
  = ProposedUpdates (Map (KeyHash 'Genesis (Crypto era)) (ProposalBody era))
  deriving (Generic)
deriving instance Show (Shelley.PParamsDelta era) => Show (ProposedUpdates era)

deriving instance Eq (Shelley.PParamsDelta era) => Eq (ProposedUpdates era)

deriving newtype instance NFData (Shelley.PParamsDelta era) => NFData (ProposedUpdates era)

instance NoThunks (Shelley.PParamsDelta era) => NoThunks (ProposedUpdates era)

deriving newtype instance
  ( CC.Crypto (Crypto era)
  , ToCBOR (Shelley.PParamsDelta era)
  , Typeable era
  ) => ToCBOR (ProposedUpdates era)

deriving newtype instance
  ( CC.Crypto (Crypto era)
  , Typeable era
  , FromCBOR (ProposalBody era)
  ) => FromCBOR (ProposedUpdates era)

fromUtxoEnv :: UtxoEnv era -> One.PpupEnv era
fromUtxoEnv (UtxoEnv slot pp _ genDelegs) = Shelley.PPUPEnv slot pp genDelegs

-- | Identical to Cardano.Ledger.Voltaire.Prototype.One.ppupTransition
--   except for:
--      * different 'ProposalBody'
--      * different 'PpupState'
ppupTransition ::
  ( Typeable era,
    Voltaire.VoltaireClass era,
    Voltaire.ProposalHeader era ~ One.ProposalHeader era,
    Voltaire.ProposalBody era ~ ProposalBody era,
    Voltaire.PpupPredicateFailure era ~ One.PpupPredicateFailure era,
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
      Map.null vs ?! One.UnsupportedVotesPPUP (Votes vs)
      case ps of
        Seq.Empty -> pure updateState
        Proposal (One.ProposalHeader s0 te) d0 :<| ps' -> do
          let combineProposals m (Proposal (One.ProposalHeader submitter targetEpoch) paramUpdate) = do
                targetEpoch == te ?! One.VaryingTargetEpochPPUP te targetEpoch
                not (Map.member submitter m) ?! One.MultipleProposalsPPUP submitter
                pure (Map.insert submitter paramUpdate m)
          pup <- foldM combineProposals (Map.singleton s0 d0) ps'
          -- The rest of the logic after this reconciliation step is the same as Shelley
          eval (dom pup ⊆ dom _genDelegs) ?! One.NonGenesisUpdatePPUP (eval (dom pup)) (eval (dom _genDelegs))
          let goodPV =
                pvCanFollow (getField @"_protocolVersion" pp)
                  . getField @"_protocolVersion"
          let badPVs = Map.filter (not . goodPV) (Map.mapMaybe bodyPParamsDelta pup)
          case Map.toList (Map.map (getField @"_protocolVersion") badPVs) of
            ((_, SJust pv) : _) -> failBecause $ One.PVCannotFollowPPUP pv
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
              currentEpoch == te ?! One.PPUpdateWrongEpoch currentEpoch te One.VoteForThisEpoch
              pure $
                PPUPState
                  (ProposedUpdates (eval (pupS ⨃ pup)))
                  (ProposedUpdates fpupS)
            else do
              currentEpoch + 1 == te ?! One.PPUpdateWrongEpoch currentEpoch te One.VoteForNextEpoch
              pure $
                PPUPState
                  (ProposedUpdates pupS)
                  (ProposedUpdates (eval (fpupS ⨃ pup)))
