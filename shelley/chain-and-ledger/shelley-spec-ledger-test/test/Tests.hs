{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

import Cardano.Crypto.Libsodium (sodiumInit)
import Test.Control.Iterate.SetAlgebra (setAlgTest)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (C)
import Test.Shelley.Spec.Ledger.PropertyTests (minimalPropertyTests, propertyTests)
import Test.Shelley.Spec.Ledger.Rewards (rewardTests)
import Test.Shelley.Spec.Ledger.STSTests (chainExamples, multisigExamples)
import Test.Shelley.Spec.Ledger.Pretty(prettyTest)
import qualified Test.Shelley.Spec.Ledger.Serialisation as Serialisation
import Test.Shelley.Spec.Ledger.UnitTests (unitTests)
import Test.Tasty
import Test.TestScenario (TestScenario (..), mainWithTestScenario)


import Control.Monad.Reader
import Data.Functor.Identity
import Data.Proxy
import Shelley.Spec.Ledger.API.Validation
import Test.Shelley.Spec.Ledger.ApplyBlock ()
import Cardano.Ledger.Shelley
import Test.Tasty.QuickCheck
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (C_Crypto)
import Test.Shelley.Spec.Ledger.Utils (testGlobals)
import Control.State.Transition.Extended

import Shelley.Spec.Ledger.LedgerState
import qualified Data.Set as Set

import Shelley.Spec.Ledger.STS.Bbody (BbodyPredicateFailure (..))
import Shelley.Spec.Ledger.STS.Ledgers (LedgersPredicateFailure (..))
import Shelley.Spec.Ledger.STS.Ledger (LedgerPredicateFailure (..))
import Shelley.Spec.Ledger.STS.Utxo (UtxoPredicateFailure (..))
import Shelley.Spec.Ledger.Coin (DeltaCoin(..))
import Shelley.Spec.Ledger.STS.Utxow (UtxowPredicateFailure(..))

tests :: TestTree
tests = askOption $ \case
  Nightly -> nightlyTests
  Fast -> fastTests
  _ -> mainTests

-- TODO: move this to a common, reusable location.
chainModelInteractionWith
  :: forall crypto .
  ( crypto ~ C_Crypto
  )
  => [ModelTxOut]
  -> [ModelBlock]
  -> Either
    (ApplyBlockError (ShelleyEra crypto))
    (NewEpochState (ShelleyEra crypto))
chainModelInteractionWith a b =
    runIdentity $ flip runReaderT testGlobals $ do
      (st, blks) <- (toEra (Proxy :: Proxy (ShelleyEra crypto)) a b)
      runApplyBlockData (ApplySTSOpts AssertionsAll ValidateAll) st blks

-- TODO: move this to a common, reusable location.
testChainModelInteractionWith
  :: forall crypto prop.
  ( crypto ~ C_Crypto -- todo, relax this?
  , Testable prop
  )
  => (NewEpochState (ShelleyEra crypto) -> prop)
  -> [ModelTxOut]
  -> [ModelBlock]
  -> Property
testChainModelInteractionWith p a b =
  case chainModelInteractionWith a b of
    Right good -> property $ p good
    Left bad -> counterexample (show bad) False

-- TODO: move this to a common, reusable location.
testChainModelInteractionRejection
  :: forall crypto .
  ( crypto ~ C_Crypto -- todo, relax this?
  )
  => ApplyBlockTransitionError (ShelleyEra C_Crypto)
  -> [ModelTxOut]
  -> [ModelBlock]
  -> Property
testChainModelInteractionRejection e a b =
  case chainModelInteractionWith a b of
    Left (e', _, _) -> e' === e
    Right _ -> counterexample "no error encountered" False

testChainModelInteraction
  :: [ModelTxOut]
  -> [ModelBlock]
  -> Property
testChainModelInteraction = testChainModelInteractionWith $ const True



newTestFw :: TestTree
newTestFw = testGroup "new-test-fw"
  [ testProperty "noop" $ testChainModelInteraction [] []

  , testProperty "noop-2" $ testChainModelInteraction
    [ ModelTxOut "alice" 1_000_000
    , ModelTxOut "alice" 1_000_000
    , ModelTxOut "bob" 1_000_000
    ]
    []
  , testProperty "xfer" $ testChainModelInteraction
    [ ModelTxOut "alice" 1_000_000
    ]
    [ ModelBlock 1
      [ ModelTx
        { _mtxId = 1
        , _mtxInputs = Set.fromList [ModelTxIn 0 0]
        , _mtxOutputs =
          [ ModelTxOut "bob" 100_000
          , ModelTxOut "alice" ( 1_000_000 - ( 100_000 + 1_000))
          ]
        , _mtxFee = 1_000
        , _mtxWitness = Set.fromList ["alice", "bob"]
        }
      ]
    ]
  , testProperty "unbalanced" $ testChainModelInteractionRejection
    (ApplyBlockTransitionError_Block (BlockTransitionError [LedgersFailure (LedgerFailure (UtxowFailure (UtxoFailure (ValueNotConservedUTxO (DeltaCoin 1000000) (DeltaCoin 101000)))))]))
    [ ModelTxOut "alice" 1_000_000
    ]
    [ ModelBlock 1
      [ ModelTx 1
        (Set.fromList [ModelTxIn 0 0])
        [ModelTxOut "bob" 100_000]
        1_000
        (Set.fromList ["alice", "bob"])
      ]
    ]
  ]

mainTests :: TestTree
mainTests =
  testGroup
    "Ledger with Delegation"
    [ minimalPropertyTests @C,
      rewardTests,
      Serialisation.tests 5,
      chainExamples,
      multisigExamples,
      unitTests,
      setAlgTest,
      prettyTest,
      newTestFw
    ]

nightlyTests :: TestTree
nightlyTests =
  testGroup
    "Ledger with Delegation nightly"
    [ propertyTests @C,
      Serialisation.tests 50
    ]

fastTests :: TestTree
fastTests =
  testGroup
    "Ledger with Delegation fast"
    [ Serialisation.tests 1,
      chainExamples,
      multisigExamples,
      unitTests,
      setAlgTest,
      prettyTest
    ]

-- main entry point
main :: IO ()
main = sodiumInit >> mainWithTestScenario tests
