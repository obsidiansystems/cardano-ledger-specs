{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Main where
import Criterion.Main (defaultMain, whnf, bgroup, bench)

import Test.Tasty.HUnit (Assertion, (@?=))
import Test.Shelley.Spec.Ledger.Utils (runShelleyBase)
import Control.State.Transition.Extended (PredicateFailure, TRC (..), applySTS)
import Control.State.Transition.Trace ((.-), (.->), checkTrace)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (LedgerEnv, UTxOState, Tx, TxOut, DPState, LEDGER)
import Test.Shelley.Spec.Ledger.Examples ( aliceAddr, txEx2A, ppsEx1)


import Shelley.Spec.Ledger.PParams (emptyPPPUpdates)
import Shelley.Spec.Ledger.LedgerState (genesisCoins, emptyDPState, pattern UTxOState)
import Shelley.Spec.Ledger.TxData (pattern TxOut)
-- import Shelley.Spec.Ledger.Tx (pattern Tx)
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Slot (SlotNo (..))
import Shelley.Spec.Ledger.STS.Ledger (pattern LedgerEnv)


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


ledger :: Integer -> Assertion
ledger _n = testLEDGER (utxoState _n, emptyDPState) txEx2A ledgerEnv (Left [])


-- The function we're benchmarking.
fib:: Int -> Int
fib m | m < 0     = error "negative!"
      | otherwise = go m
  where
    go 0 = 0
    go 1 = 1
    go n = go (n-1) + go (n-2)


-- Our benchmark harness.
main :: IO ()
main = defaultMain [
  bgroup "fib" [ bench "1"  $ whnf fib 1
               , bench "5"  $ whnf fib 5
               , bench "9"  $ whnf fib 9
               , bench "11" $ whnf fib 11
               ]
  , bgroup "ledger" [ bench "4"  $ whnf ledger 4]
  ]


-- =======================================================
-- Directions for creating a benchmark with stack
-- ======================================================
{-
In a Stack system (one where there is a stack.yaml file)
In a package sub-directory (where you find the xxx.cabal file)
1) Make a directory "bench" (if it is not alread there)
2) Then add a benchmark to the cabal file. Here is a simple one.
   -------------------------------------------
   | benchmark mainbench
   | type:             exitcode-stdio-1.0
   | hs-source-dirs:   bench
   | main-is:          Bench1.hs
   | build-depends:    base,
   |                   criterion,
   |                   random,
   |                   MonadRandom
   | ghc-options:      -Wall
   |                  -O2
   | default-language: Haskell2010
   --------------------------------------------
3) Then create the actual file "Bench1.hs" in this example, in the "bench" directory.
   Be sure it is a "Main" module, with a function called "main". Note how we use "defaultMain"
   imported from Criterion. Here is one I copied from the Criterion web page.
   -----------------------------------------------
   | module Main where
   | import Criterion.Main
   |
   | -- The function we're benchmarking.
   | fib m | m < (0::Int)     = error "negative!"
   |       | otherwise = go m
   |   where
   |     go 0 = 0
   |     go 1 = 1
   |     go n = go (n-1) + go (n-2)
   |
   | -- Our benchmark harness.
   | main = defaultMain [
   |   bgroup "fib" [ bench "1"  $ whnf fib 1
   |                , bench "5"  $ whnf fib 5
   |                , bench "9"  $ whnf fib 9
   |                , bench "11" $ whnf fib 11
   |                ]
   |   ]
   ------------------------------------------------
4) Now to start it up type:
   stack bench --ba --output=fibber.html
   The "--ba --output=fibber.html", makes the call create an html file that plots the outout.
-}