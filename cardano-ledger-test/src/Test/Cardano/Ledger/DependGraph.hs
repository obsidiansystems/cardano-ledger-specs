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
  ]

instance Arbitrary DependGraph where
  arbitrary = dependGraphGenerator

genesisInputsGenerator :: [ModelAddress] -> Gen (Set.Set ModelTxIn)
genesisInputsGenerator addrs = Set.fromList . (map ModelGenesisIn) <$> sublistOf addrs

standardAddrs :: [ModelAddress]
standardAddrs = map ModelAddress
  [ "Alice_Alderson"
  , "Bob_Bonaparte"
  , "Cho_Chang"
  , "Darshan_Dhingra"
  , "Evelyn_Eto"
  , "Fernando_Frisco"
  , "Ghada_Ghannam"
  , "Horatio_Hurst"
  ]
