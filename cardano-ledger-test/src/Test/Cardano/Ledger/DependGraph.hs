{-# LANGUAGE FlexibleInstances #-}

module Test.Cardano.Ledger.DependGraph where

import qualified Data.Graph.Inductive as FGL
import qualified Data.Set as Set
import Data.Set.NonEmpty (NESet)

import Test.QuickCheck (Arbitrary, Gen, arbitrary, frequency, sublistOf)

import Test.Cardano.Ledger.ModelChain

-- All the ways in which one transaction may depend on another.
data Depends = CoinValue | Delegate | Script
  deriving (Eq, Ord, Show)

-- You can depend upon another transaction in one or more ways.
type TxDependency = NESet Depends

type DependGraph = FGL.Gr ModelTx TxDependency

dependGraphGenerator :: Gen DependGraph
dependGraphGenerator = frequency
  [ (1, pure FGL.empty)
    (29, linearGraphGenerator)
  ]

instance Arbitrary DependGraph where
  arbitrary = dependGraphGenerator

linearGraphGenerator :: Gen DependGraph
linearGraphGenerator = do
  addrs <- flip take standardAddrs <$> chooseInt (minAddrs, maxAddrs)
  genesis <- genesisInputsGenerator addrs
  -- generate first tx, etc.
  initialWits <- sublistOf addrs
  initialOuts <- sublistOf addrs
  let initialTx = ModelTx (ModelTxId 0) genesis initialOuts (ModelValue 1.7) initialWits
  where
    minAddrs = 4
    maxAddrs = 16
    minChainLength = 3
    maxChainLength = 12

linearBasicTxGenerator :: ModelTx -> Gen ModelTx
linearBasicTxGenerator = do
  pure ModelTx

genesisInputsGenerator :: [ModelAddress] -> Gen (Set.Set ModelTxIn)
genesisInputsGenerator addrs = Set.fromList . (map ModelGenesisIn) <$> sublistOf addrs

standardAddrs :: [ModelAddress]
standardAddrs = map (ModelAddress . ("a" ++) . show) [1..]
