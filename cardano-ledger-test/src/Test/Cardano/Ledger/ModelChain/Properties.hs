{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.ModelChain.Properties where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.BaseTypes (unitIntervalFromRational)
import Cardano.Ledger.Coin
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Era
import Cardano.Ledger.Shelley (ShelleyEra)
import Control.State.Transition.Extended
import Data.Default.Class
import Data.Foldable
import qualified Data.Map as Map
import Data.Ratio ((%))
import qualified Data.Set as Set
import Data.Typeable
import GHC.Natural
import Shelley.Spec.Ledger.API.Genesis
import Test.Cardano.Ledger.Elaborators.Alonzo ()
import Test.Cardano.Ledger.Elaborators.Shelley ()
import Test.Cardano.Ledger.ModelChain
import Test.Cardano.Ledger.ModelChain.Utils
import Test.Cardano.Ledger.ModelChain.Value
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (C_Crypto)
import Test.Tasty
import Test.Tasty.QuickCheck

modelCoin :: Integer -> ModelValue
modelCoin = ModelValue_Inject . Coin

modelReward :: ModelAddress -> ModelValue
modelReward = ModelValue_Var . ModelValue_Reward

modelRewards :: [ModelAddress] -> Map.Map ModelAddress ModelValue
modelRewards = foldMap $ \maddr -> Map.singleton maddr $ modelReward maddr

infixl 6 $+

infixl 6 $-

infixl 7 $*

($*) :: Natural -> ModelValue -> ModelValue
($*) = ModelValue_Scale

($+) :: ModelValue -> ModelValue -> ModelValue
($+) = ModelValue_Add

($-) :: ModelValue -> ModelValue -> ModelValue
($-) = ModelValue_Sub

-- | some hand-written model based unit tests
modelUnitTests ::
  forall era proxy.
  ( ElaborateEraModel era,
    Default (AdditionalGenesisConfig era),
    Eq (PredicateFailure (Core.EraRule "LEDGER" era)),
    Show (PredicateFailure (Core.EraRule "LEDGER" era)),
    Cardano.Ledger.Era.Era era,
    Show (Core.Value era)
  ) =>
  proxy era ->
  TestTree
modelUnitTests proxy =
  testGroup
    (show $ typeRep proxy)
    [ testProperty "noop" $ testChainModelInteraction proxy Map.empty [],
      testProperty "noop-2" $
        testChainModelInteraction
          proxy
          ( Map.fromList
              [ ("alice", Coin 1_000_000),
                ("bob", Coin 1_000_000)
              ]
          )
          [ModelEpoch [] mempty],
      let genAct =
            Map.fromList
              [ ("alice", Coin 1_000_000_000_000)
              ]
          checks nes ems =
            let rewards = observeRewards (nes, ems)
             in counterexample (show rewards) $ Coin 0 === fold rewards
       in testProperty "deleg" $
            testChainModelInteractionWith
              proxy
              checks
              genAct
              [ ModelEpoch
                  [ ModelBlock
                      0
                      [ (modelTx 1)
                          { _mtxInputs = Set.fromList [ModelGensisIn "alice"],
                            _mtxOutputs =
                              [ ModelTxOut "alice" (modelCoin 1_000_000_000_000 $- (modelCoin 100_000_000_000))
                              ],
                            _mtxFee = modelCoin 100_000_000_000,
                            _mtxDCert =
                              [ ModelRegisterStake "alice",
                                ModelRegisterPool (ModelPoolParams "pool1" (Coin 0) (Coin 0) (unitIntervalFromRational (0 % 1)) "alice" ["alice"]),
                                ModelDelegate (ModelDelegation "alice" "pool1")
                              ]
                          }
                      ]
                  ]
                  (ModelBlocksMade $ Map.fromList []),
                ModelEpoch [] (ModelBlocksMade $ Map.fromList []),
                ModelEpoch [] (ModelBlocksMade $ Map.fromList [("pool1", 100)]),
                -- , ModelEpoch [] (ModelBlocksMade $ Map.fromList [("pool1", 100)])
                ModelEpoch [] (ModelBlocksMade $ Map.fromList []),
                ModelEpoch
                  [ ModelBlock
                      0
                      [ (modelTx 1)
                          { _mtxInputs = Set.fromList [ModelTxIn 1 0],
                            _mtxOutputs =
                              [ ModelTxOut "alice" (modelCoin 1_000_000_000_000 $- (2 $* modelCoin 100_000_000_000)),
                                ModelTxOut "bob" (modelReward "alice" $- modelCoin 100_000_000),
                                ModelTxOut "carol" (modelCoin 100_000_000)
                              ],
                            _mtxFee = modelCoin 100_000_000_000,
                            _mtxWdrl = modelRewards ["alice"]
                          }
                      ]
                  ]
                  (ModelBlocksMade $ Map.fromList [])
              ],
      testProperty "xfer" $
        testChainModelInteraction
          proxy
          ( Map.fromList
              [ ("alice", Coin 1_000_000_000)
              ]
          )
          [ ModelEpoch
              [ ModelBlock
                  1
                  [ (modelTx 1)
                      { _mtxInputs = Set.fromList [ModelGensisIn "alice"],
                        _mtxOutputs =
                          [ ModelTxOut "bob" (modelCoin 100_000_000),
                            ModelTxOut "alice" (modelCoin 1_000_000_000 $- (modelCoin 100_000_000 $+ modelCoin 1_000_000))
                          ],
                        _mtxFee = modelCoin 1_000_000
                      }
                  ]
              ]
              mempty
          ],
      testProperty "unbalanced" $
        testChainModelInteractionRejection
          proxy
          (ModelValueNotConservedUTxO (modelCoin 1_000_000_000) (modelCoin 101_000_000))
          ( Map.fromList
              [ ("alice", Coin 1_000_000_000)
              ]
          )
          [ ModelEpoch
              [ ModelBlock
                  1
                  [ (modelTx 1)
                      { _mtxInputs = (Set.fromList [ModelGensisIn "alice"]),
                        _mtxOutputs = [ModelTxOut "bob" $ modelCoin 100_000_000],
                        _mtxFee = modelCoin 1_000_000
                      }
                  ]
              ]
              mempty
          ],
      testProperty "xfer-2" $
        testChainModelInteraction
          proxy
          ( Map.fromList
              [ ("alice", Coin 1_000_000_000)
              ]
          )
          [ ModelEpoch
              [ ModelBlock
                  1
                  [ (modelTx 1)
                      { _mtxInputs = Set.fromList [ModelGensisIn "alice"],
                        _mtxOutputs =
                          [ ModelTxOut "bob" (modelCoin 100_000_000),
                            ModelTxOut "alice" (modelCoin 1_000_000_000 $- (modelCoin 100_000_000 $+ modelCoin 1_000_000))
                          ],
                        _mtxFee = modelCoin 1_000_000
                      }
                  ],
                ModelBlock
                  2
                  [ (modelTx 2)
                      { _mtxInputs = Set.fromList [ModelTxIn 1 1],
                        _mtxOutputs =
                          [ ModelTxOut "bob" (modelCoin 100_000_000),
                            ModelTxOut "alice" (modelCoin 1_000_000_000 $- 2 $* (modelCoin 100_000_000 $+ modelCoin 1_000_000))
                          ],
                        _mtxFee = modelCoin 1_000_000
                      }
                  ]
              ]
              mempty
          ]
    ]

modelUnitTests_ :: TestTree
modelUnitTests_ =
  testGroup
    "model-unit-tests"
    [ modelUnitTests (Proxy :: Proxy (ShelleyEra C_Crypto)),
      modelUnitTests (Proxy :: Proxy (AlonzoEra C_Crypto))
    ]

defaultTestMain :: IO ()
defaultTestMain = defaultMain modelUnitTests_
