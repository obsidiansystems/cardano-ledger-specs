{-# OPTIONS_GHC -fno-warn-orphans  -Wno-unused-binds  -Wno-unused-imports #-}

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.BenchFuns(ledgerBench1) where


import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence.Strict as StrictSeq

import Control.State.Transition.Extended (PredicateFailure, TRC (..), applySTS)
import Control.State.Transition.Trace ((.-), (.->), checkTrace)
import Test.Tasty.HUnit (Assertion, (@?=))

import Test.Shelley.Spec.Ledger.Utils (runShelleyBase)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (
  CHAIN, LedgerEnv, UTxOState, Tx, TxBody, TxOut, DPState, LEDGER)
import Test.Shelley.Spec.Ledger.Examples (aliceAddr, alicePay, txEx2A, ppsEx1)

import Shelley.Spec.Ledger.BaseTypes (StrictMaybe (..))
import Shelley.Spec.Ledger.PParams (emptyPPPUpdates, PParams, PParams' (..))
import Shelley.Spec.Ledger.LedgerState (genesisCoins, emptyDPState, pattern UTxOState, genesisId)
import Shelley.Spec.Ledger.TxData (pattern TxIn, pattern TxOut, pattern TxBody, pattern Wdrl)
import Shelley.Spec.Ledger.Tx (pattern Tx)
import Shelley.Spec.Ledger.UTxO (hashTxBody, makeWitnessesVKey)
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Keys (asWitness)
import Shelley.Spec.Ledger.Slot (SlotNo (..))
import Shelley.Spec.Ledger.STS.Ledger (pattern LedgerEnv)

import Test.Shelley.Spec.Ledger.Examples
  ( CHAINExample(..),
    ex1, ex2A, ex2B, ex2C, ex2Cbis, ex2Cquater, ex2Cter, ex2D, ex2E, ex2F, ex2G, ex2H, ex2I, ex2J, ex2K, ex2L, ex3A,
    ex3B, ex3C, ex4A, ex4B, ex5A, ex5B, ex5C, ex5D', ex6A, ex6A', ex6BExpectedNES, ex6BExpectedNES', ex6BPoolParams,
  )


coins :: Integer -> [TxOut]
coins n = fmap (\_ -> TxOut aliceAddr (Coin $ 100)) [0..n]

utxoState :: Integer -> UTxOState
utxoState n =
  UTxOState
    (genesisCoins (coins n))
    (Coin 0)
    (Coin 0)
    emptyPPPUpdates

ppsBench :: PParams
ppsBench = ppsEx1 { _minUTxOValue = 10 }

ledgerEnv :: LedgerEnv
ledgerEnv = LedgerEnv (SlotNo 0) 0 ppsBench 0

testLEDGER :: (UTxOState, DPState)
                    -> Tx
                    -> LedgerEnv
                    -> Bool
testLEDGER initSt tx env = do
  let st = runShelleyBase $ applySTS @LEDGER (TRC (env, initSt, tx))
  case st of
    Right _ -> True
    Left _ -> False

txbodyBench1 :: TxBody
txbodyBench1 =
  TxBody
    (Set.fromList [TxIn genesisId 0])
    (StrictSeq.fromList [TxOut aliceAddr (Coin 10), TxOut aliceAddr (89)])
    StrictSeq.empty
    (Wdrl Map.empty)
    (Coin 1)
    (SlotNo 10)
    SNothing
    SNothing

txBench1 :: Tx
txBench1 =
  Tx
    txbodyBench1
    (makeWitnessesVKey (hashTxBody txbodyBench1) [asWitness alicePay])
    Map.empty
    SNothing

ledgerBench1 :: Integer -> Bool
ledgerBench1 n = testLEDGER (utxoState n, emptyDPState) txBench1 ledgerEnv

