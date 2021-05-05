{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.ModelChain.Utils where

import Cardano.Ledger.Coin
import Cardano.Slotting.Slot (EpochSize(..))
import Control.Monad
import Control.State.Transition.Extended
import Data.Default.Class
import Data.Function ((&))
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


chainModelInteractionWith
  :: forall era proxy .
  ( Default (AdditionalGenesisConfig era)
  , ElaborateEraModel era
  , ApplyBlock era
  )
  => proxy era
  -> Map.Map ModelAddress Coin
  -> [ModelBlock]
  -> Either (ApplyBlockError era) (NewEpochState era)
chainModelInteractionWith _ a b =
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
    (st0, blks) = elaborateEraModel sg def testGlobals a b

    -- fold over a list of tick and block actions into a ledger state.
    runApplyBlockData opts globals = go
      where
        loop
          :: forall a. ([a] -> (ApplyBlockTransitionError era))
          -> ApplyBlockData era
          -> [ApplyBlockData era]
          -> (NewEpochState era, [[a]])
          -> (Either (ApplyBlockError era) (NewEpochState era))
        loop _ _ xs (good, []) = go good xs
        loop c x _ (st, bad) = Left . (,st,x) . c $ join bad
        {-# INLINE loop #-}

        go st [] = Right $! st
        go st (x:xs) = case x of
          ApplyTick tick -> applyTickOpts opts globals st tick
            & loop (ApplyBlockTransitionError_Tick . TickTransitionError) x xs
          ApplyBlock blk -> applyBlockOpts opts globals st blk
            & loop (ApplyBlockTransitionError_Block . BlockTransitionError) x xs
  in runApplyBlockData (ApplySTSOpts AssertionsAll ValidateAll) testGlobals st0 blks


testChainModelInteractionWith ::
  ( Testable prop
  , Show (State (Core.EraRule "TICK" era))
  , Show (Signal (Core.EraRule "TICK" era))
  , Show (Signal (Core.EraRule "BBODY" era))
  , ElaborateEraModel era, ApplyBlock era
  , Default (AdditionalGenesisConfig era)
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
  ( ApplyBlock era
  , ElaborateEraModel era
  , Default (AdditionalGenesisConfig era)
  )
  => proxy era
  -> ModelPredicateFailure -- ApplyBlockTransitionError era
  -> Map.Map ModelAddress Coin
  -> [ModelBlock]
  -> Property
testChainModelInteractionRejection proxy e a b =
  case chainModelInteractionWith proxy a b of
    Left (e', _, _) ->
      let
        elaboratedError = toEraPredicateFailure @era e
      in case (e', elaboratedError) of
        (ApplyBlockTransitionError_Block (BlockTransitionError te), ApplyBlockTransitionError_Block (BlockTransitionError te')) -> compareLists te te'
        (ApplyBlockTransitionError_Tick (TickTransitionError te), ApplyBlockTransitionError_Tick (TickTransitionError te')) -> compareLists te te'
        (te, te') -> te === te'

    Right _ -> counterexample "no error encountered" False

testChainModelInteraction ::
  ( Show (State (Core.EraRule "TICK" era))
  , Show (Signal (Core.EraRule "TICK" era))
  , Show (Signal (Core.EraRule "BBODY" era))
  , ElaborateEraModel era, ApplyBlock era
  , Default (AdditionalGenesisConfig era)
  )
  => proxy era
  -> Map.Map ModelAddress Coin
  -> [ModelBlock]
  -> Property
testChainModelInteraction proxy = testChainModelInteractionWith proxy $ (`seq` True)


