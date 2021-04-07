{-# LANGUAGE FlexibleContexts #-}
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

tests :: TestTree
tests = askOption $ \case
  Nightly -> nightlyTests
  Fast -> fastTests
  _ -> mainTests

newTestFw :: TestTree
newTestFw = testGroup "new-test-fw"
  [ testProperty "noop" $ do
      let
        result = runIdentity $ flip runReaderT testGlobals $ do
          (st, blks) <- (toEra (Proxy :: Proxy (ShelleyEra C_Crypto)) [] [])
          runApplyBlockData (ApplySTSOpts AssertionsAll ValidateAll) st blks
      case result of
        Right _ -> property True
        Left bad -> counterexample (show bad) False
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
