{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}

module Test.Cardano.Ledger.DependGraph where

import qualified Data.Graph.Inductive as FGL
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NES

import Test.QuickCheck

import Cardano.Ledger.Coin

import Test.Cardano.Ledger.ModelChain

-- All the ways in which one transaction may depend on another.
data Depends = CoinValue | Delegate | Script
  deriving (Eq, Ord, Show)

-- You can depend upon another transaction in one or more ways.
type TxDependency = NES.NESet Depends

-- In order to maintain consistency with the terminology of FGL, the dependency
-- relations between transactions should always be as follows:
-- (EarlierTx) ---Depends---> (LaterTx)
-- The directed edges should originate with the transaction having the Utxos,
-- and "point toward" the transaction where they are spent.
-- Also note: Try to keep node numbering matching the model transaction ID.
type DependGraph = FGL.Gr ModelTx TxDependency

dependGraphGenerator :: Gen DependGraph
dependGraphGenerator = frequency
  [ (1, pure FGL.empty)
  , (29, linearGraphGenerator)
  ]

instance Arbitrary DependGraph where
  arbitrary = dependGraphGenerator

-- Create the first transaction in a model chain.
-- TODO: Replace hard-coded values
genesisTxGenerator :: Gen ModelTx
genesisTxGenerator = do
  addrs <- flip take standardAddrs <$> chooseInt (minAddrs, maxAddrs)
  genesis <- Set.fromList . (map ModelGenesisIn) <$> sublistOf addrs
  initOutAddrs <- sublistOf addrs
  let initialOuts = map (\a -> ModelTxOut a (ModelValue 970_223_481)) initOutAddrs
      initDCerts = map ModelRegisterStake initOutAddrs
  pure $ ModelTx (ModelTxId 0) genesis initialOuts (ModelValue 1) initDCerts
  where
    minAddrs = 4
    maxAddrs = 16

-- Take the current state and add one transaction
-- TODO: Replace hard-coded values
-- TODO: Spend some outputs later, instead of always the very next block
linearStepGenerator :: DependGraph -> Gen DependGraph
linearStepGenerator start
  | FGL.isEmpty start = do
      initialTx <- genesisTxGenerator
      pure $ FGL.buildGr [([], 0, initialTx, [])]
  | otherwise = case mLatestOutputs of
      Nothing -> error "DependGraph.linearStepGenerator found unlabeled latest node"
      Just [] -> error "DependGraph.linearStepGenerator latest node has no utxos"
      Just utxos -> do
        fee <- chooseInteger (1, 99)
        let newTxIns = Set.fromList $ map (\(ModelTxOut addr val) -> ModelTxIn (ModelTxId (fromIntegral latestNode)) 0) utxos  -- is this right?
            utxoMap = Map.fromList $ map (\(ModelTxOut a v) -> (a, v)) utxos
            newDCert = map ModelRegisterStake $ Map.keys utxoMap
            spender : spendList = Map.keys utxoMap
            available = unModelValue $ Map.findWithDefault 0 spender utxoMap
        -- Take some of the available amount and send it to others, also calculate change
        -- TODO: Make this more random instead of an even split
        toSpend <- chooseInteger (1, (available - fee))
        let numRecipients = fromIntegral . length $ spendList
            share = if numRecipients > 0 then toSpend `div` numRecipients else 0
            expends = map (flip ModelTxOut (ModelValue share)) spendList
            change = available - fee - (share * numRecipients)
            changeOut = ModelTxOut spender (ModelValue change)
            newOuts = changeOut : expends
            nodeId = latestNode + 1
            newTxId = ModelTxId $ fromIntegral nodeId
            newTx = ModelTx newTxId newTxIns newOuts (ModelValue fee) newDCert
        pure $ ([(NES.singleton CoinValue, latestNode)], nodeId, newTx, []) FGL.& start
    where
      latestNode = snd $ FGL.nodeRange start
      mLatestOutputs = _mtxOutputs <$> FGL.lab start latestNode

-- -- Find unspent outputs in the graph
-- filterModelUtxos :: DependGraph -> Map.Map ModelTx [ModelTxOut]
-- filterModelUtxos graph = ufold
--   where
--     notSpent :: [ModelTx] -> ModelTxOut -> Bool
--     notSpent laterNodes txOut = undefined  -- need to lookup ModelTx from the Node key
--     singleFilter :: Context ModelTx TxDependency -> [ModelTxOut]
--     singleFilter (laterNodes, _, mtx, _) = filter (isUnspent laterNodes) (_mtxOutputs mtx)

-- Generate a simple dependency graph of transactions, just spending outputs
linearGraphGenerator :: Gen DependGraph
linearGraphGenerator = do
  -- Note that in graph theory terms, the number of nodes in the graph is called
  -- the "order", while the "size" is the number of edges. Here we're using the
  -- QuickCheck "size" implicit parameter to determine the order of our
  -- generated graph, i.e. the number of transactions.
  n <- getSize
  initialGraph <- linearStepGenerator FGL.empty
  -- generate first tx and "loop" until we have enough
  (iterate (>>= linearStepGenerator) (pure initialGraph)) !! n

-- Convert to the model form used for tests
-- Genesis accounts, and then lists of blocks with some extra metadata
dependGraphToModelChain :: DependGraph -> (Map.Map ModelAddress Coin, [ModelEpoch])
dependGraphToModelChain graph = error "dependGraphToModelChain not yet implemented"

standardAddrs :: [ModelAddress]
standardAddrs = map (ModelAddress . ("a" ++) . show) posIntegers
  where
    posIntegers = [1..] :: [Integer]
