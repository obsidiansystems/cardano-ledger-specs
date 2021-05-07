{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.ModelChain.Utils where

import Cardano.Ledger.Coin
import Data.Bifunctor
import Cardano.Slotting.Slot (EpochSize(..))
import Control.State.Transition.Extended
import Data.Default.Class
import Data.List ((\\), nub)
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Shelley.Spec.Ledger.API.Genesis
import Shelley.Spec.Ledger.API.Validation
import Shelley.Spec.Ledger.BaseTypes (Network(Testnet))
import Shelley.Spec.Ledger.Genesis
import Shelley.Spec.Ledger.LedgerState (NewEpochState)
import Shelley.Spec.Ledger.PParams (emptyPParams)
import Test.Cardano.Ledger.ModelChain
import Test.Shelley.Spec.Ledger.Utils (testGlobals)
import Test.Tasty.QuickCheck
import qualified Cardano.Ledger.Core as Core
import qualified Data.Map as Map
import Shelley.Spec.Ledger.API.Mempool (ApplyTxError(..))

type ApplyBlockError era = (ApplyBlockTransitionError era)

chainModelInteractionWith
  :: forall era proxy .
  ( Default (AdditionalGenesisConfig era)
  , Default (ElaborateEraModelState era)
  , ElaborateEraModel era
  -- , ApplyBlock era
  )
  => proxy era
  -> Map.Map ModelAddress Coin
  -> [ModelBlock]
  -> Either (ApplyBlockError era) (NewEpochState era)
chainModelInteractionWith _ genesisAccounts modelBlocks =
  let
    -- TODO, pass this in as a generator.
    sg :: ShelleyGenesis era
    sg = ShelleyGenesis
      { sgSystemStart       = (posixSecondsToUTCTime 0)
      , sgNetworkMagic      = 1 -- genNetworkMagic
      , sgNetworkId         = Testnet
      , sgActiveSlotsCoeff  = 1 -- fmap realToFrac genSlotLength
      , sgSecurityParam     = 1 -- Gen.word64 (Range.linear 1 1000000)
      , sgEpochLength       = (EpochSize 1) -- genSecurityParam
      , sgSlotsPerKESPeriod = 1 -- Gen.word64 (Range.linear 1 100000)
      , sgMaxKESEvolutions  = 1 -- Gen.word64 (Range.linear 1 100000)
      , sgSlotLength        = (secondsToNominalDiffTime 1) -- genSlotLength
      , sgUpdateQuorum      = 1 -- Gen.word64 (Range.linear 1 100000)
      , sgMaxLovelaceSupply = 1 -- Gen.word64 (Range.linear 1 100000)
      , sgProtocolParams    = emptyPParams -- genPParams
      , sgGenDelegs         = mempty --  genGenesisDelegationList
      , sgInitialFunds      = mempty -- genFundsList
      , sgStaking           = emptyGenesisStaking -- genStaking
      }

    elabState = elaborateInitialState sg def genesisAccounts def
    (x, (y, _)) = elaborateBlocks_ testGlobals modelBlocks elabState
  in bimap ApplyBlockTransitionError_Tx (\() -> y) x


testChainModelInteractionWith ::
  ( Testable prop
  , ElaborateEraModel era, ApplyBlock era
  , Default (AdditionalGenesisConfig era)
  , Show (PredicateFailure (Core.EraRule "LEDGER" era))
  , Default (ElaborateEraModelState era)
  )
  => proxy era
  -> (State (Core.EraRule "TICK" era) -> prop)
  -> Map.Map ModelAddress Coin
  -> [ModelBlock]
  -> Property
testChainModelInteractionWith proxy p a b =
  case chainModelInteractionWith proxy a b of
    Right good -> property $! p good
    Left bad -> counterexample (show bad) False

compareLists :: forall a. (Show a, Eq a) => [a] -> [a] -> Property
compareLists a b = case nub a \\ nub b of
  [] -> property True
  _ -> a === b

testChainModelInteractionRejection
  :: forall era proxy.
  ( ElaborateEraModel era
  , Default (AdditionalGenesisConfig era)
  , Eq (PredicateFailure (Core.EraRule "LEDGER" era))
  , Show (PredicateFailure (Core.EraRule "LEDGER" era ))
  , Default (ElaborateEraModelState era)
  )
  => proxy era
  -> ModelPredicateFailure
  -> Map.Map ModelAddress Coin
  -> [ModelBlock]
  -> Property
testChainModelInteractionRejection proxy e a b =
  case chainModelInteractionWith proxy a b of
    Left e' ->
      let
        elaboratedError = toEraPredicateFailure @era e
      in case (e', elaboratedError) of
        (ApplyBlockTransitionError_Tx (ApplyTxError te), ApplyBlockTransitionError_Tx (ApplyTxError te')) ->
          compareLists te te'
        -- fallthrough if/when more error types are added
        -- (te, te') -> te === te'

    Right _ -> counterexample "no error encountered" False

testChainModelInteraction ::
  ( Show (PredicateFailure (Core.EraRule "LEDGER" era))
  , ElaborateEraModel era, ApplyBlock era
  , Default (AdditionalGenesisConfig era)
  , Default (ElaborateEraModelState era)
  )
  => proxy era
  -> Map.Map ModelAddress Coin
  -> [ModelBlock]
  -> Property
testChainModelInteraction proxy = testChainModelInteractionWith proxy $ (`seq` True)


