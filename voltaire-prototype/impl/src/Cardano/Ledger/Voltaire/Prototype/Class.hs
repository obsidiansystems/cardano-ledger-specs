{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
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
  )
import Cardano.Ledger.Era
import Cardano.Ledger.Pretty
import Control.DeepSeq (NFData)
import Control.State.Transition
import Data.Coders
  ( Decode (From, RecD),
    decode,
    (<!),
    invalidKey,
  )
import Data.Set (Set)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Kind
import Data.Sequence.Strict (StrictSeq)
import Data.Typeable
import Data.Word
import GHC.Generics
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.Keys
import Shelley.Spec.Ledger.Serialization
  ( decodeStrictSeq,
    encodeFoldable,
    mapFromCBOR,
    mapToCBOR,
  )
import Shelley.Spec.Ledger.BaseTypes (ShelleyBase)
import qualified Shelley.Spec.Ledger.STS.Utxo as Shelley

class
  ( Eq (PpupPredicateFailure era)
  , Show (PpupPredicateFailure era)
  , PrettyA (ProposalHeader era)
  , Eq (ProposalHeader era)
  , Show (ProposalHeader era)
  , ToCBOR (ProposalHeader era)
  , FromCBOR (ProposalHeader era)
  , NFData (ProposalHeader era)
  , NoThunks (ProposalHeader era)
  , PrettyA (ProposalBody era)
  , Eq (ProposalBody era)
  , Show (ProposalBody era)
  , ToCBOR (ProposalBody era)
  , FromCBOR (ProposalBody era)
  , NFData (ProposalBody era)
  , NoThunks (ProposalBody era)
  , PrettyA (ProposalId era)
  , Ord (ProposalId era)
  , Show (ProposalId era)
  , ToCBOR (ProposalId era)
  , FromCBOR (ProposalId era)
  , NFData (ProposalId era)
  , NoThunks (ProposalId era)
  , Eq (PpupState era)
  , Show (PpupState era)
  ) =>
  VoltaireClass era where
    -- | The proposal header holds metadata relevant to the governance rules
    -- surrounding proposals. In pre-Voltaire Shelley this is simply the
    -- Genesis Key Delegate submitting the proposal and the target Epoch for
    -- enactment.
    --
    -- In Voltaire governance there is metadata pertaining to voting periods
    -- and thresholds as well as other such parameters.
    type ProposalHeader era :: Type
    -- | The proposal body specifies the changes to the chain being proposed.
    -- In pre-Voltaire Shelley, the only update proposals are changes to
    -- parameters, including the protocol version.
    --
    -- In Voltaire governance there are 3 proposal types:
    --   * protocol version upgrades
    --   * non-version protocol parameter updates
    --   * central (treasury and reserve) funds transfers
    type ProposalBody era :: Type
    -- | When casting votes for proposals, as well as other situations, we need
    -- a way to succinctly refer to a particular proposal. In pre-Voltaire
    -- Shelley, since the number of voters is small (the set of genesis key
    -- delegates), we simply use the proposal body itself.
    --
    -- In Voltaire governance, one possible ProposalId is a pointer to a
    -- submitted proposal that has been recorded in a transaction.
    type ProposalId era :: Type
    --
    type PpupEnv era :: Type
    type PpupState era :: Type
    type PpupPredicateFailure era :: Type
    fromUtxoEnv :: Shelley.UtxoEnv era -> PpupEnv era
    ppupTransition :: TransitionRule (PPUP era)
    submissionsWitnesses :: Shelley.UtxoEnv era -> Submissions era -> Set (KeyHash 'Witness (Crypto era))

-- | A proposal is its body which describes what the proposed change is paired
-- with its header which describes how the proposal should be handled by the
-- governance process.
data Proposal era = Proposal
  { proposalHeader :: ProposalHeader era
  , proposalBody :: ProposalBody era
  } deriving (Generic)

instance VoltaireClass era => NFData (Proposal era)
instance VoltaireClass era => NoThunks (Proposal era)
deriving instance VoltaireClass era => Eq (Proposal era)
deriving instance VoltaireClass era => Show (Proposal era)

instance (Era era, VoltaireClass era) => ToCBOR (Proposal era) where
  toCBOR (Proposal h b) =
    encodeListLen 2 <> toCBOR h <> toCBOR b

instance
  (Era era, VoltaireClass era) =>
  FromCBOR (Proposal era)
  where
  fromCBOR = decode $ RecD Proposal <! From <! From

-- | Proposals that are submitted within a transaction
data Submissions era = Submissions !(StrictSeq (Proposal era))
  deriving (Generic)

instance VoltaireClass era => NFData (Submissions era)
instance VoltaireClass era => NoThunks (Submissions era)
deriving instance VoltaireClass era => Eq (Submissions era)
deriving instance VoltaireClass era => Show (Submissions era)

instance
  (Era era, VoltaireClass era) =>
  ToCBOR (Submissions era)
  where
  toCBOR (Submissions m) = encodeFoldable m

instance
  (Era era, VoltaireClass era) =>
  FromCBOR (Submissions era)
  where
  fromCBOR = Submissions <$> decodeStrictSeq fromCBOR


-- | A vote is either in favor or in opposition to a proposal.
data VoteIntention
   = VoteIntention_No
   | VoteIntention_Yes
  deriving (Eq, Show, Generic)

instance NFData VoteIntention
instance NoThunks VoteIntention

instance ToCBOR VoteIntention where
  toCBOR = \case
    VoteIntention_No -> toCBOR (0 :: Word8)
    VoteIntention_Yes -> toCBOR (1 :: Word8)

instance FromCBOR VoteIntention where
  fromCBOR = decodeWord >>= \case
    0 -> pure VoteIntention_No
    1 -> pure VoteIntention_Yes
    k -> invalidKey k

-- | Votes that are recorded within a transaction
data Votes era = Votes (Map (ProposalId era) (Map (KeyHash 'Witness (Crypto era)) VoteIntention))
  deriving Generic

instance VoltaireClass era => NFData (Votes era)
instance VoltaireClass era => NoThunks (Votes era)
deriving instance VoltaireClass era => Eq (Votes era)
deriving instance VoltaireClass era => Show (Votes era)

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
  { _update_submissions :: Submissions era
  , _update_votes :: Votes era
  } deriving (Generic)

emptyUpdate :: Update era
emptyUpdate = Update
  { _update_submissions = Submissions mempty
  , _update_votes = Votes Map.empty
  }

instance VoltaireClass era => NFData (Update era)
instance VoltaireClass era => NoThunks (Update era)
deriving instance VoltaireClass era => Eq (Update era)
deriving instance VoltaireClass era => Show (Update era)

instance (Era era, VoltaireClass era) => ToCBOR (Update era) where
  toCBOR (Update subs votes) =
    encodeListLen 2 <> toCBOR subs <> toCBOR votes

instance
  (Era era, VoltaireClass era) =>
  FromCBOR (Update era)
  where
  fromCBOR = decode $ RecD Update <! From <! From

-- STS for Voltaire
data PPUP era

instance
  ( Typeable era,
    VoltaireClass era
  ) =>
  STS (PPUP era)
  where
    type State (PPUP era) = PpupState era
    type Signal (PPUP era) = Maybe (Update era)
    type Environment (PPUP era) = PpupEnv era
    type BaseM (PPUP era) = ShelleyBase
    type PredicateFailure (PPUP era) = PpupPredicateFailure era
    initialRules = []
    transitionRules = [ppupTransition]

ppProposal :: VoltaireClass era => Proposal era -> PDoc
ppProposal (Proposal h b) = ppSexp "Proposal"
  [ prettyA h, prettyA b ]

ppVoteIntention :: VoteIntention -> PDoc
ppVoteIntention = \case
  VoteIntention_Yes -> text "Yes"
  VoteIntention_No -> text "No"

ppSubmissions :: VoltaireClass era => Submissions era -> PDoc
ppSubmissions (Submissions ps) = ppStrictSeq ppProposal ps

ppVotes :: VoltaireClass era => Votes era -> PDoc
ppVotes (Votes m) = ppMap prettyA (ppMap ppKeyHash ppVoteIntention) m

ppUpdate :: VoltaireClass era => Update era -> PDoc
ppUpdate (Update subs votes) = ppSexp "Update"
  [ ppSubmissions subs, ppVotes votes ]

