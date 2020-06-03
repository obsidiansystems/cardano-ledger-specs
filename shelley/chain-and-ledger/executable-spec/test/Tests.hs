{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

import Test.Shelley.Spec.Ledger.Address (addressTests)
import Test.Shelley.Spec.Ledger.CDDL (cddlTests)
import Test.Shelley.Spec.Ledger.NonTraceProperties.PropertyTests (nonTracePropertyTests)
import Test.Shelley.Spec.Ledger.PropertyTests (minimalPropertyTests, propertyTests)
import Test.Shelley.Spec.Ledger.STSTests (stsTests)
import Test.Shelley.Spec.Ledger.Serialization (serializationTests)
import Test.Shelley.Spec.Ledger.UnitTests (unitTests)
import Test.Tasty
import Test.TestScenario (TestScenario (..), mainWithTestScenario)

-------TESTING
import Test.Tasty.HUnit (Assertion, (@?=))
import Test.Shelley.Spec.Ledger.Utils (runShelleyBase)
import Control.State.Transition.Extended (PredicateFailure, TRC (..), applySTS)
import Control.State.Transition.Trace ((.-), (.->), checkTrace)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (
  LedgerEnv, UTxOState, Tx, DPState, LEDGER)
import Test.Shelley.Spec.Ledger.Examples (aliceAddr, txEx2A, ppsEx1)
import Shelley.Spec.Ledger.PParams (emptyPPPUpdates)
import Shelley.Spec.Ledger.LedgerState (genesisCoins, emptyDPState, pattern UTxOState)
import Shelley.Spec.Ledger.TxData (pattern TxOut)
import Shelley.Spec.Ledger.Tx (pattern Tx)
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Slot (SlotNo (..))
import Shelley.Spec.Ledger.STS.Ledger (pattern LedgerEnv)

utxoState :: Integer -> UTxOState
utxoState n =
  UTxOState
    ( genesisCoins $
        fmap
          (\x -> TxOut aliceAddr (Coin $ 100+x))
          [0..n]
    )
    (Coin 0)
    (Coin 0)
    emptyPPPUpdates

ledgerEnv :: LedgerEnv
ledgerEnv = LedgerEnv (SlotNo 0) 0 ppsEx1 0

testLEDGER ::
  (UTxOState, DPState) ->
  Tx ->
  LedgerEnv ->
  Either [[PredicateFailure LEDGER]] (UTxOState, DPState) ->
  Assertion
testLEDGER initSt tx env (Right expectedSt) = do
  checkTrace @LEDGER runShelleyBase env $ pure initSt .- tx .-> expectedSt
testLEDGER initSt tx env predicateFailure@(Left _) = do
  let st = runShelleyBase $ applySTS @LEDGER (TRC (env, initSt, tx))
  st @?= predicateFailure

ledger :: Integer -> Assertion
ledger _n = testLEDGER (utxoState 10, emptyDPState) txEx2A ledgerEnv (Left [])
-------TESTING

tests :: TestTree
tests = askOption $ \case
  Nightly -> nightlyTests
  Fast -> fastTests
  _ -> mainTests

mainTests :: TestTree
mainTests =
  testGroup
    "Ledger with Delegation"
    [ addressTests,
      cddlTests 5,
      minimalPropertyTests,
      serializationTests,
      stsTests,
      unitTests
    ]

nightlyTests :: TestTree
nightlyTests =
  testGroup
    "Ledger with Delegation nightly"
    [ propertyTests,
      nonTracePropertyTests,
      cddlTests 50
    ]

fastTests :: TestTree
fastTests =
  testGroup
    "Ledger with Delegation fast"
    [ addressTests,
      cddlTests 1,
      serializationTests,
      stsTests,
      unitTests
    ]

-- main entry point
main :: IO ()
main = mainWithTestScenario tests
