{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Cardano.Ledger.Voltaire.Prototype.One where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    encodeListLen,
  )

import Cardano.Ledger.Core (PParams)
import Cardano.Ledger.Era (Crypto, Era)
import qualified Cardano.Ledger.Voltaire.Prototype.Class as Voltaire
import qualified Cardano.Ledger.Shelley.Constraints as Shelley (UsesPParams (PParamsDelta))
import Control.DeepSeq (NFData)
import Control.SetAlgebra (dom, eval, (⊆))
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid (First(..))
import Data.Set (Set)
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.Records
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.Keys
import Shelley.Spec.Ledger.LedgerState (pvCanFollow)
import Shelley.Spec.Ledger.PParams (ProtVer)
import Shelley.Spec.Ledger.Serialization (decodeRecordSum)
import Shelley.Spec.Ledger.STS.Utxo (UtxoEnv(..))

-- The first prototype simply implements the Shelley PPUP and UPEC rules in the
-- Voltaire prototype scaffolding. In particular we do not try to incorporate
-- the MIR logic in prototype one.
type Proposal era = Shelley.PParamsDelta era

-- Shelley submitters and voters are Genesis key delegates
type Submitter era = 'Genesis
type Voter era = 'Genesis
type VotePayload era = Shelley.PParamsDelta era
type VoltaireEnv era = GenDelegs (Crypto era)
type VoltaireState era = Voltaire.Submissions era

data SubmissionPredicateFailure era
  = NonGenesisUpdatePPUP
      !(Set (KeyHash 'Genesis (Crypto era))) -- KeyHashes which are voting
      !(Set (KeyHash 'Genesis (Crypto era))) -- KeyHashes which should be voting
  | PVCannotFollowPPUP
      !ProtVer -- the first bad protocol version
  deriving (Show, Eq, Generic)

instance NFData (SubmissionPredicateFailure era)
instance NoThunks (SubmissionPredicateFailure era)

instance
  (Typeable era, Era era) =>
  ToCBOR (SubmissionPredicateFailure era)
  where
  toCBOR = \case
    (NonGenesisUpdatePPUP a b) ->
      encodeListLen 3
        <> toCBOR (0 :: Word8)
        <> toCBOR a
        <> toCBOR b
    PVCannotFollowPPUP p -> encodeListLen 2 <> toCBOR (1 :: Word8) <> toCBOR p

instance
  (Era era) =>
  FromCBOR (SubmissionPredicateFailure era)
  where
  fromCBOR = decodeRecordSum "SubmissionPredicateFailure (PPUP era)" $
    \case
      0 -> do
        a <- fromCBOR
        b <- fromCBOR
        pure (3, NonGenesisUpdatePPUP a b)
      1 -> do
        p <- fromCBOR
        pure (2, PVCannotFollowPPUP p)
      k -> invalidKey k

type VotePredicateFailure era = SubmissionPredicateFailure era

validateSubmissions
  :: ( Voltaire.VoltaireEnv era ~ VoltaireEnv era
     , Voltaire.Submitter era ~ Submitter era
     , Voltaire.Proposal era ~ Proposal era
     , HasField "_protocolVersion" (Proposal era) (StrictMaybe ProtVer)
     , HasField "_protocolVersion" (PParams era) ProtVer
     )
  => Voltaire.PPUPEnv era
  -> Voltaire.Submissions era
  -> Maybe (SubmissionPredicateFailure era)
validateSubmissions env (Voltaire.Submissions pup) = validatePup env pup

validateVotes
  :: ( Voltaire.Voter era ~ Voter era
     , Voltaire.VoltaireEnv era ~ GenDelegs (Crypto era)
     , Voltaire.VotePayload era ~ VotePayload era
     , HasField "_protocolVersion" (VotePayload era) (StrictMaybe ProtVer)
     , HasField "_protocolVersion" (PParams era) ProtVer
     )
  => Voltaire.PPUPEnv era
  -> Voltaire.Votes era
  -> Maybe (VotePredicateFailure era)
validateVotes env (Voltaire.Votes pup) = validatePup env pup

validatePup
 :: ( Voltaire.VoltaireEnv era ~ GenDelegs (Crypto era)
    , HasField "_protocolVersion" (Proposal era) (StrictMaybe ProtVer)
    , HasField "_protocolVersion" (PParams era) ProtVer
    )
 => Voltaire.PPUPEnv era
 -> Map (KeyHash (Submitter era) (Crypto era)) (Proposal era)
 -> Maybe (SubmissionPredicateFailure era)
validatePup (Voltaire.PPUPEnv _ pp (GenDelegs _genDelegs)) pup = getFirst $ foldMap First $
  [ case eval (dom pup ⊆ dom _genDelegs) of
      False -> Just $ NonGenesisUpdatePPUP (eval (dom pup)) (eval (dom _genDelegs))
      True -> Nothing
  , let goodPV =
          pvCanFollow (getField @"_protocolVersion" pp)
            . getField @"_protocolVersion"
        badPVs = Map.filter (not . goodPV) pup
     in case Map.toList (Map.map (getField @"_protocolVersion") badPVs) of
          ((_, SJust pv) : _) -> Just $ PVCannotFollowPPUP pv
          _ -> Nothing
  ]

fromUtxoEnv :: UtxoEnv era -> VoltaireEnv era
fromUtxoEnv (UtxoEnv _ _ _ genDelegs) = genDelegs

updateState
  :: Voltaire.PPUPEnv era
  -> Voltaire.Submissions era
  -> Voltaire.Votes era
  -> Voltaire.PPUPState era
  -> Voltaire.PPUPState era
updateState _ _ _ = id
