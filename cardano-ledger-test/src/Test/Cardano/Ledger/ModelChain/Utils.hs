{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.ModelChain.Utils where

import Cardano.Ledger.BaseTypes (Globals (..), activeSlotVal, epochInfo, unitIntervalFromRational, unitIntervalToRational)
import Cardano.Ledger.Coin
import qualified Cardano.Ledger.Core as Core
import Cardano.Slotting.EpochInfo.API (epochInfoSize)
import Cardano.Slotting.Slot (EpochNo (..))
import Control.State.Transition.Extended
import Data.Default.Class
import Data.Functor.Identity
import Data.List (nub, (\\))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Shelley.Spec.Ledger.API.Genesis
import Shelley.Spec.Ledger.API.Mempool (ApplyTxError (..))
import Shelley.Spec.Ledger.Genesis
import Shelley.Spec.Ledger.LedgerState (NewEpochState)
import Shelley.Spec.Ledger.PParams (emptyPParams)
import qualified Shelley.Spec.Ledger.PParams as PParams
import Test.Cardano.Ledger.ModelChain
import Test.Cardano.Ledger.ModelChain.Value
import Test.Shelley.Spec.Ledger.Utils (testGlobals)
import Test.Tasty.QuickCheck

-- type ApplyBlockError era = (ApplyBlockTransitionError era)

-- | apply a list of ModelEpoch to an empty ledger and return the resulting
-- state, or the error if one occured
chainModelInteractionWith ::
  forall era proxy.
  ( Default (AdditionalGenesisConfig era),
    ElaborateEraModel era
  ) =>
  proxy era ->
  Map.Map ModelAddress Coin ->
  [ModelEpoch] ->
  (Either (ElaborateBlockError era) (), (NewEpochState era, EraElaboratorState era))
chainModelInteractionWith _ genesisAccounts modelBlocks =
  let -- TODO, pass this in as a generator.

      globals = testGlobals

      sg :: ShelleyGenesis era
      sg =
        ShelleyGenesis
          { sgSystemStart = (posixSecondsToUTCTime 0),
            sgNetworkMagic = 1, -- genNetworkMagic
            sgNetworkId = networkId globals,
            sgActiveSlotsCoeff = unitIntervalToRational $ activeSlotVal $ activeSlotCoeff globals,
            sgSecurityParam = securityParameter globals,
            sgEpochLength = runIdentity $ flip epochInfoSize (EpochNo 1) $ epochInfo globals,
            sgSlotsPerKESPeriod = slotsPerKESPeriod globals,
            sgMaxKESEvolutions = maxKESEvo globals,
            sgSlotLength = (secondsToNominalDiffTime 1),
            sgUpdateQuorum = quorum globals,
            sgMaxLovelaceSupply = maxLovelaceSupply globals,
            sgProtocolParams =
              emptyPParams
                { PParams._rho = unitIntervalFromRational 0.02
                }, -- genPParams
            sgGenDelegs = mempty, --  genGenesisDelegationList
            sgInitialFunds = mempty, -- genFundsList
            sgStaking = emptyGenesisStaking -- genStaking
          }

      elabState = elaborateInitialState sg def genesisAccounts def
   in elaborateBlocks_ globals modelBlocks elabState

-- | Apply a list of ModelEpoch's to an empty ledger, then check the resulting
-- ledger against a user supplied predicate.
testChainModelInteractionWith ::
  ( Testable prop,
    ElaborateEraModel era,
    Default (AdditionalGenesisConfig era),
    Show (PredicateFailure (Core.EraRule "LEDGER" era)),
    Show (Core.Value era)
  ) =>
  proxy era ->
  (NewEpochState era -> EraElaboratorState era -> prop) ->
  Map.Map ModelAddress Coin ->
  [ModelEpoch] ->
  Property
testChainModelInteractionWith proxy p a b =
  let (result, (nes, ees)) = chainModelInteractionWith proxy a b
   in case result of
        Right () -> property $! p nes ees
        Left bad -> counterexample (show bad) False

compareLists :: forall a. (Show a, Eq a) => [a] -> [a] -> Property
compareLists a b = case nub a \\ nub b of
  [] -> property True
  _ -> a === b

-- | Apply a list of ModelEpoch's to an empty ledger, then check the resulting
-- error is the one predicted by the model.
testChainModelInteractionRejection ::
  forall era proxy.
  ( ElaborateEraModel era,
    Default (AdditionalGenesisConfig era),
    Eq (PredicateFailure (Core.EraRule "LEDGER" era)),
    Show (PredicateFailure (Core.EraRule "LEDGER" era)),
    Show (Core.Value era)
  ) =>
  proxy era ->
  ModelPredicateFailure ->
  Map.Map ModelAddress Coin ->
  [ModelEpoch] ->
  Property
testChainModelInteractionRejection proxy e a b =
  let (result, (nes, ees)) = chainModelInteractionWith proxy a b
   in case result of
        Left e' ->
          let elaboratedError' = toEraPredicateFailure @era e (nes, ees)
           in case elaboratedError' of
                Left bad -> counterexample ("couldn't elaborate expected error:" <> show bad) False
                Right elaboratedError -> case (e', elaboratedError) of
                  (bad@(ElaborateBlockError_Fee {}), _) -> counterexample (show bad) False
                  (bad@(ElaborateBlockError_TxValue {}), _) -> counterexample (show bad) False
                  (ElaborateBlockError_ApplyTx (ApplyTxError te), ApplyBlockTransitionError_Tx (ApplyTxError te')) ->
                    compareLists te te'
        -- fallthrough if/when more error types are added
        -- (te, te') -> te === te'

        Right _ -> counterexample "no error encountered" False

-- | Apply a list of ModelEpoch's to an empty ledger, and fail if there was an
-- error
testChainModelInteraction ::
  ( Show (PredicateFailure (Core.EraRule "LEDGER" era)),
    ElaborateEraModel era,
    Default (AdditionalGenesisConfig era),
    Show (Core.Value era)
  ) =>
  proxy era ->
  Map.Map ModelAddress Coin ->
  [ModelEpoch] ->
  Property
testChainModelInteraction proxy = testChainModelInteractionWith proxy $ (\x y -> x `seq` y `seq` True)

-- | helper to produce a "blank" ModelTx with most fields set to a reasonable
-- "default"
modelTx :: ModelTxId -> ModelTx
modelTx txId =
  ModelTx
    { _mtxId = txId,
      _mtxInputs = Set.empty,
      _mtxOutputs = [],
      _mtxFee = ModelValue_Inject $ Coin 0,
      _mtxDCert = [],
      _mtxWdrl = Map.empty
    }
