{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Cardano.Ledger.Voltaire.Prototype.Class where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    decodeWord,
    encodeListLen,
    encodeMapLen,
    encodeWord,
  )
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era
import Cardano.Ledger.Pretty
import Control.DeepSeq (NFData)
import Data.Coders
  ( Decode (From, RecD),
    decode,
    (<!),
  )
import Data.Map.Strict (Map)
import Data.Kind
import Data.Typeable
import GHC.Generics
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.Keys
import Shelley.Spec.Ledger.Serialization
  ( CBORGroup (..),
    FromCBORGroup (..),
    ToCBORGroup (..),
    decodeMapContents,
    decodeRecordNamed,
    mapFromCBOR,
    mapToCBOR,
    ratioFromCBOR,
    ratioToCBOR,
  )
import Shelley.Spec.Ledger.Slot (EpochNo (..), SlotNo (..))
import qualified Shelley.Spec.Ledger.STS.Utxo as Shelley

class
  ( PrettyA (Proposal era)
  , PrettyA (VotePayload era)
  , FromCBOR (Proposal era)
  , FromCBOR (VotePayload era)
  , FromCBOR (SubmissionPredicateFailure era)
  , FromCBOR (VotePredicateFailure era)
  , ToCBOR (Proposal era)
  , ToCBOR (VotePayload era)
  , ToCBOR (SubmissionPredicateFailure era)
  , ToCBOR (VotePredicateFailure era)
  , Eq (Proposal era)
  , Eq (VotePayload era)
  , Eq (VoltaireState era)
  , Eq (SubmissionPredicateFailure era)
  , Eq (VotePredicateFailure era)
  , NFData (Proposal era)
  , NFData (VotePayload era)
  , NFData (SubmissionPredicateFailure era)
  , NFData (VotePredicateFailure era)
  , NoThunks (Proposal era)
  , NoThunks (VotePayload era)
  , NoThunks (SubmissionPredicateFailure era)
  , NoThunks (VotePredicateFailure era)
  , Show (Proposal era)
  , Show (VotePayload era)
  , Show (VoltaireState era)
  , Show (SubmissionPredicateFailure era)
  , Show (VotePredicateFailure era)
  , Typeable (Submitter era)
  , Typeable (Voter era)
  ) =>
  VoltaireClass era where
  -- Transaction data
  type Proposal era :: Type
  type Submitter era :: KeyRole
  type Voter era :: KeyRole
  type VotePayload era :: Type
  -- STS data
  type VoltaireEnv era :: Type
  -- Per-epoch proposal state
  type VoltaireState era :: Type
  -- Validation data
  type SubmissionPredicateFailure era :: Type
  type VotePredicateFailure era :: Type
  -- STS logic
  fromUtxoEnv :: Shelley.UtxoEnv era -> VoltaireEnv era
  validateSubmissions :: PPUPEnv era -> Submissions era -> Maybe (SubmissionPredicateFailure era)
  validateVotes :: PPUPEnv era -> Votes era -> Maybe (VotePredicateFailure era)
  updateState :: PPUPEnv era -> Submissions era -> Votes era -> PPUPState era -> PPUPState era

data PPUPEnv era = PPUPEnv SlotNo (Core.PParams era) (VoltaireEnv era)

data PPUPState era = PPUPState
  { currentProposals :: !(VoltaireState era)
  , futureProposals :: !(VoltaireState era)
  }

deriving instance Eq (VoltaireState era) => Eq (PPUPState era)
deriving instance Show (VoltaireState era) => Show (PPUPState era)

newtype Submissions era
  = Submissions (Map (KeyHash (Submitter era) (Crypto era)) (Proposal era))
  deriving (Generic)

deriving instance VoltaireClass era => Eq (Submissions era)

deriving instance VoltaireClass era => NFData (Submissions era)

deriving instance Show (Proposal era) => Show (Submissions era)

instance VoltaireClass era => NoThunks (Submissions era)

instance
  (Era era, VoltaireClass era) =>
  ToCBOR (Submissions era)
  where
  toCBOR (Submissions m) = mapToCBOR m

instance
  (Era era, VoltaireClass era) =>
  FromCBOR (Submissions era)
  where
  fromCBOR = Submissions <$> mapFromCBOR

newtype Votes era
  = Votes (Map (KeyHash (Voter era) (Crypto era)) (VotePayload era))
  deriving (Generic)

deriving instance VoltaireClass era => Eq (Votes era)

deriving instance VoltaireClass era => NFData (Votes era)

deriving instance Show (VotePayload era) => Show (Votes era)

instance VoltaireClass era => NoThunks (Votes era)

instance
  (Era era, VoltaireClass era) =>
  ToCBOR (Votes era)
  where
  toCBOR (Votes m) = mapToCBOR m

instance
  (Era era, VoltaireClass era) =>
  FromCBOR (Votes era)
  where
  fromCBOR = Votes <$> mapFromCBOR

data Update era = Update
  { _voltaireUpdate_submissions :: !(Submissions era)
  , _voltaireUpdate_votes :: !(Votes era)
  , _voltaireUpdate_epochNumber :: !EpochNo
  } deriving (Generic)

deriving instance VoltaireClass era => Eq (Update era)

deriving instance VoltaireClass era => NFData (Update era)

deriving instance (Show (Proposal era), Show (VotePayload era)) => Show (Update era)

instance VoltaireClass era => NoThunks (Update era)

instance (Era era, VoltaireClass era) => ToCBOR (Update era) where
  toCBOR (Update subs votes e) =
    encodeListLen 3 <> toCBOR subs <> toCBOR votes <> toCBOR e

instance
  (Era era, VoltaireClass era) =>
  FromCBOR (Update era)
  where
  fromCBOR = decode $ RecD Update <! From <! From <! From

ppSubmissions :: VoltaireClass era => Submissions era -> PDoc
ppSubmissions (Submissions m) = ppMap ppKeyHash prettyA m

ppVotes :: VoltaireClass era => Votes era -> PDoc
ppVotes (Votes m) = ppMap ppKeyHash prettyA m

ppUpdate :: VoltaireClass era => Update era -> PDoc
ppUpdate (Update subs votes e) = ppSexp "Update" [ ppSubmissions subs, ppVotes votes, ppEpochNo e ]
