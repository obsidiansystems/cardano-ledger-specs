{-# OPTIONS_GHC -fno-warn-orphans  -Wno-unused-binds  -Wno-unused-imports #-}

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.BenchFuns(ledgerBench1) where


import Control.State.Transition.Extended (PredicateFailure, TRC (..), applySTS)
import Control.State.Transition.Trace ((.-), (.->), checkTrace)
import Test.Tasty.HUnit (Assertion, (@?=))

import Test.Shelley.Spec.Ledger.Utils (runShelleyBase)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (CHAIN, LedgerEnv, UTxOState, Tx, TxOut, DPState, LEDGER)
import Test.Shelley.Spec.Ledger.Examples ( aliceAddr, txEx2A, ppsEx1)

import Shelley.Spec.Ledger.PParams (emptyPPPUpdates)
import Shelley.Spec.Ledger.LedgerState (genesisCoins, emptyDPState, pattern UTxOState)
import Shelley.Spec.Ledger.TxData (pattern TxOut)
-- import Shelley.Spec.Ledger.Tx (pattern Tx)
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Slot (SlotNo (..))
import Shelley.Spec.Ledger.STS.Ledger (pattern LedgerEnv)

import Test.Shelley.Spec.Ledger.Examples
  ( CHAINExample(..),
    ex1, ex2A, ex2B, ex2C, ex2Cbis, ex2Cquater, ex2Cter, ex2D, ex2E, ex2F, ex2G, ex2H, ex2I, ex2J, ex2K, ex2L, ex3A,
    ex3B, ex3C, ex4A, ex4B, ex5A, ex5B, ex5C, ex5D', ex6A, ex6A', ex6BExpectedNES, ex6BExpectedNES', ex6BPoolParams,
  )


coins :: Integer -> [TxOut]
coins n = fmap (\x -> TxOut aliceAddr (Coin $ 100+x)) [0..n]

utxoState :: Integer -> UTxOState
utxoState _n =
  UTxOState
    ( genesisCoins (coins _n) )
    (Coin 0)
    (Coin 0)
    emptyPPPUpdates


ledgerEnv :: LedgerEnv
ledgerEnv = LedgerEnv (SlotNo 0) 0 ppsEx1 0


testLEDGER :: (UTxOState, DPState)
                    -> Tx
                    -> LedgerEnv
                    -> Either
                         [[PredicateFailure LEDGER]]
                         (UTxOState, DPState)
                    -> Assertion


testLEDGER initSt tx env (Right expectedSt) = do
  checkTrace @LEDGER runShelleyBase env $ pure initSt .- tx .-> expectedSt
testLEDGER initSt tx env predicateFailure@(Left _) = do
  let st = runShelleyBase $ applySTS @LEDGER (TRC (env, initSt, tx))
  st @?= predicateFailure


ledgerBench1 :: Integer -> Assertion
ledgerBench1 n = testLEDGER (utxoState n, emptyDPState) txEx2A ledgerEnv (Left [])

-- ===========================================================================================

testCHAINExample :: CHAINExample -> Assertion
testCHAINExample (CHAINExample initSt block (Right expectedSt)) = do
  checkTrace @CHAIN runShelleyBase () $ pure initSt .- block .-> expectedSt
testCHAINExample (CHAINExample initSt block predicateFailure@(Left _)) = do
  let st = runShelleyBase $ applySTS @CHAIN (TRC ((), initSt, block))
  st @?= predicateFailure