{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Epoch change registration.
--
-- The rules of this module determine how the update subsystem of the ledger
-- handles the epoch transitions.
module Cardano.Ledger.Voltaire.Prototype.Rules.Two.Upec where

import qualified Cardano.Ledger.Voltaire.Prototype.Two as Two
import Cardano.Ledger.Coin (Coin)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Shelley.Constraints
  ( ShelleyBased,
    UsesAuxiliary,
    UsesPParams (PParamsDelta, mergePPUpdates),
    UsesScript,
    UsesTxBody,
    UsesValue,
  )
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition
  ( Embed (..),
    STS (..),
    TRC (..),
    judgmentContext,
    liftSTS,
    trans,
  )
import Data.Default.Class (Default)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import GHC.Records
import Cardano.Prelude (Generic)
import NoThunks.Class (NoThunks)
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.BaseTypes (Globals (..), ShelleyBase, StrictMaybe)
import Shelley.Spec.Ledger.LedgerState
  ( EpochState,
    PPUPState (..),
    UpecState (..),
    esAccountState,
    esLState,
    _delegationState,
    _ppups,
    _utxoState,
    _dstate,
    _irwd,
    pattern DPState,
    pattern EpochState,
  )
import Shelley.Spec.Ledger.PParams (ProposedPPUpdates (..), ProtVer)
import Shelley.Spec.Ledger.STS.Newpp (NEWPP, NewppEnv (..), NewppState (..))
import Shelley.Spec.Ledger.Keys (KeyHash, KeyRole (Genesis))
import qualified Shelley.Spec.Ledger.STS.Mir as Shelley
import qualified Cardano.Ledger.Voltaire.Prototype.Rules.Two.Mir as Mir
import Cardano.Ledger.Era (Era, Crypto)

-- | Update epoch change
data UPEC era

-- | TODO: do we want to separate MIR "validation" failures
--   from MIR "transfer" failures? Currently, the transfer itself
--   (performed by the "MIR" rule) cannot fail, while the validation
--   *can* fail.
data UpecPredicateFailure era
  = NewPpFailure (PredicateFailure (NEWPP era))
  | ValidateMirFailure (Mir.DelegMirPredicateFailure era)
  | MirFailure (PredicateFailure (Core.EraRule "MIR" era))
    deriving (Generic)

deriving stock instance
  ( Show (PredicateFailure (Core.EraRule "MIR" era))
  ) =>
  Show (UpecPredicateFailure era)

deriving stock instance
  ( Eq (PredicateFailure (Core.EraRule "MIR" era))
  ) =>
  Eq (UpecPredicateFailure era)

instance
  ( NoThunks (PredicateFailure (Core.EraRule "MIR" era))
  ) =>
  NoThunks (UpecPredicateFailure era)

instance
  ( UsesAuxiliary era,
    UsesTxBody era,
    UsesScript era,
    UsesValue era,
    UsesPParams era,
    Default (Core.PParams era),
    State (Core.EraRule "PPUP" era) ~ Two.PPUPState era,
    Ord (PParamsDelta era),
    Embed (Core.EraRule "MIR" era) (UPEC era),
    Environment (Core.EraRule "MIR" era) ~ (),
    State (Core.EraRule "MIR" era) ~ EpochState era,
    Signal (Core.EraRule "MIR" era) ~ (),
    HasField "_keyDeposit" (Core.PParams era) Coin,
    HasField "_maxBBSize" (Core.PParams era) Natural,
    HasField "_maxTxSize" (Core.PParams era) Natural,
    HasField "_maxBHSize" (Core.PParams era) Natural,
    HasField "_poolDeposit" (Core.PParams era) Coin,
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    HasField "_protocolVersion" (PParamsDelta era) (StrictMaybe ProtVer)
  ) =>
  STS (UPEC era)
  where
  type State (UPEC era) = (UpecState era, EpochState era)
  type Signal (UPEC era) = ()
  type Environment (UPEC era) = ()
  type BaseM (UPEC era) = ShelleyBase
  type PredicateFailure (UPEC era) = UpecPredicateFailure era
  initialRules = []
  transitionRules =
    [ do
        TRC
          ( (),
            (UpecState pp ppupSt,
             es@EpochState
              { esAccountState = acnt,
                esLState = ls
              }
            ),
            _
            ) <-
          judgmentContext

        coreNodeQuorum <- liftSTS $ asks quorum

        let utxoSt = _utxoState ls
            DPState dstate pstate = _delegationState ls
            Two.PPUPState (Two.ProposedUpdates proposals')
                          (Two.ProposedUpdates futureProposals') = _ppups utxoSt
            futureMirProposals = Map.mapMaybe Two.bodyMirCert futureProposals'
            winnerM = votedValue proposals' (fromIntegral coreNodeQuorum)
            winnerParamsDeltaM
              | Nothing                 <- winnerM = Nothing
              | Just (Two.BodyMIR _)    <- winnerM = Nothing
              | Just (Two.BodyPPUP ppd) <- winnerM = Just ppd
            ppNew = mergePPUpdates (Proxy @era) pp <$> winnerParamsDeltaM
            shelleyPpupSt = toShelleyPPUPState ppupSt
        NewppState pp' ppupSt' <-
          trans @(NEWPP era) $
            TRC (NewppEnv dstate pstate utxoSt acnt, NewppState pp shelleyPpupSt, ppNew)
        es' <- case winnerM of
          Nothing -> pure es
          Just (Two.BodyPPUP _) -> pure es
          Just (Two.BodyMIR mirCert) -> do
            -- TODO: Are we doing it at the last slot of this epoch
            --  or the first slot of the next epoch?
            -- Alternatively we can just remove the logic from handleMIR
            --  that checks the slot since we now know this statically.
            let slotNo = fromInteger 0
            irwd' <- Mir.handleMIR ValidateMirFailure ((slotNo, acnt, pp)) (_irwd dstate) mirCert
            trans @(Core.EraRule "MIR" era) $ TRC ((), updateIrwd es irwd', ())
        pure $
          (UpecState pp' (fromShelleyPPUPState futureMirProposals ppupSt'), es')
    ]
    where
      updateIrwd es _irwd' =
        let delegState = _delegationState (esLState es)
        in es {
          esLState = (esLState es) {
            _delegationState = delegState {
              _dstate = (_dstate delegState) {
                _irwd = _irwd'
              }
            }
          }
        }

-- | Given (1) a new Shelley 'PPUPState' (as returned by 'NEWPP'), and (2) the future
--   MIR-proposals, return a 'Two.PPUPState' with the current proposals set to a
--   union of the future MIR-proposals and the current PPUP-proposals.
--
--   So, this function moves the future MIR-proposals to the current proposals,
--   analogous to what 'Shelley.Spec.Ledger.STS.Newpp.updatePpup' does for PPUP-proposals.
fromShelleyPPUPState
  :: Map (KeyHash 'Genesis (Crypto era)) (Two.MIRCert (Crypto era))
  -> PPUPState era
  -> Two.PPUPState era
fromShelleyPPUPState
  futureMirProposals
  (PPUPState (ProposedPPUpdates proposals') (ProposedPPUpdates futureProposals')) =
    Two.PPUPState
      (Two.ProposedUpdates $ currentMirProposals `Map.union` currentPpupProposls)
      (Two.ProposedUpdates $ fmap Two.BodyPPUP futureProposals')
 where
  currentMirProposals = fmap Two.BodyMIR futureMirProposals
  currentPpupProposls = fmap Two.BodyPPUP proposals'

toShelleyPPUPState :: Two.PPUPState era -> PPUPState era
toShelleyPPUPState ppupSt =
  PPUPState
    (convert $ Two.proposals ppupSt)
    (convert $ Two.futureProposals ppupSt)
 where
  convert :: Two.ProposedUpdates era -> ProposedPPUpdates era
  convert (Two.ProposedUpdates map') =
    ProposedPPUpdates $ Map.mapMaybe Two.bodyPParamsDelta map'

-- | If at least @n@ nodes voted to change __the same__ protocol parameters to
-- __the same__ values, return the given protocol parameters updated to these
-- values. Here @n@ is the quorum needed.
votedValue ::
  Ord b =>
  -- | A map from a voter to the value that is voted on
  Map a b ->
  -- | Quorum needed to change the protocol parameters.
  Int ->
  Maybe b
votedValue pup quorumN =
  let incrTally vote tally = 1 + Map.findWithDefault 0 vote tally
      votes =
        Map.foldr
          (\vote tally -> Map.insert vote (incrTally vote tally) tally)
          (Map.empty :: Map a Int)
          pup
      consensus = Map.filter (>= quorumN) votes
   in case Map.toList consensus of
        -- NOTE that `quorumN` is a global constant, and that we require
        -- it to be strictly greater than half the number of genesis nodes.
        -- The keys in the `pup` correspond to the genesis nodes,
        -- and therefore either:
        --   1) `consensus` is empty, or
        --   2) `consensus` has exactly one element.
        [res] ->
          Just . fst $ res
        -- NOTE that `updatePParams` corresponds to the union override right
        -- operation in the formal spec.
        _ -> Nothing

instance
  (ShelleyBased era, STS (NEWPP era)) =>
  Embed (NEWPP era) (UPEC era)
  where
  wrapFailed = NewPpFailure

instance
  ( Era era,
    Default (EpochState era),
    PredicateFailure (Core.EraRule "MIR" era) ~ Shelley.MirPredicateFailure era
  ) =>
  Embed (Shelley.MIR era) (UPEC era)
  where
  wrapFailed = MirFailure
