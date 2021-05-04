{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.ModelChain.Properties where

import Cardano.Ledger.Coin
import Control.State.Transition.Extended
import Data.Default.Class
import Shelley.Spec.Ledger.BaseTypes (unitIntervalFromRational)
import Data.Typeable
import Shelley.Spec.Ledger.API.Genesis
-- import Shelley.Spec.Ledger.API.Validation
import Test.Cardano.Ledger.ModelChain
import Test.Cardano.Ledger.ModelChain.Utils
import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Val as Val
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Ratio ((%))
import qualified Cardano.Ledger.Era
import qualified Cardano.Binary


import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Data.Proxy
import Test.Cardano.Ledger.Elaborators.Shelley ()
import Test.Cardano.Ledger.Elaborators.Alonzo ()
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (C_Crypto)
import Test.Tasty (TestTree, testGroup, defaultMain)

newTestFw ::
  forall era proxy.
  ( ElaborateEraModel era
  , HasEraElaboratorState (ElaborateEraModelState era) era
  , Show (ElaborateEraModelState era)
  , Default (AdditionalGenesisConfig era)
  , Default (ElaborateEraModelState era)
  , Eq (PredicateFailure (Core.EraRule "LEDGER" era))
  , Show (PredicateFailure (Core.EraRule "LEDGER" era))
  , Typeable era
  , Cardano.Ledger.Era.Era era
  , Cardano.Binary.ToCBOR (Core.TxBody era)
  , Cardano.Binary.ToCBOR (Core.TxOut era)
  , Cardano.Binary.ToCBOR (Core.Value era)
  , Show (Core.Value era)
  , Show (Core.TxOut era)
  , Show (Core.PParams era)
  , Show (State (Core.EraRule "PPUP" era))
  )
  => proxy era -> TestTree
newTestFw proxy = testGroup (show $ typeRep proxy)
  [ testProperty "noop" $ testChainModelInteraction proxy Map.empty []

  , testProperty "noop-2" $ testChainModelInteraction proxy
    (Map.fromList
      [ ("alice", Coin 1_000_000)
      , ("bob", Coin 1_000_000)
      ])
    [ModelEpoch [] mempty]
  , let
      genAct = Map.fromList
        [ ("alice", Coin 1_000_000_000_000)
        ]
      checks nes ems =
        let
          rewards = observeRewards (nes, ems)
        in counterexample (show nes) $ counterexample (show ems) $ counterexample (show rewards) $ False -- rewards === mempty
    in testProperty "deleg" $ testChainModelInteractionWith proxy checks genAct
      -- []
      [ ModelEpoch
        [ ModelBlock 0
          [ (modelTx 1)
            { _mtxInputs = Set.fromList [ModelGenesisIn "alice"]
            , _mtxOutputs =
              [ ModelTxOut "alice" ( 1_000_000_000_000 - ( 1_000_000))
              ]
            , _mtxFee = 1_000_000
            , _mtxDCert =
              [ ModelRegisterStake "alice"
              , ModelRegisterPool (ModelPoolParams "pool1" (Coin 1_000_000) (Coin 1) (unitIntervalFromRational (1 % 1) ) "alice")
              , ModelDelegate (ModelDelegation "alice" "pool1")
              ]
            }
          ]
        ] (ModelBlocksMade $ Map.fromList [("pool1", 100)])
      -- , ModelEpoch
      --   [ ModelBlock 1
      --     [ (modelTx 2)
      --       { _mtxInputs = Set.fromList [ModelTxIn 1 0]
      --       , _mtxOutputs =
      --         [ ModelTxOut "alice" ( 1_000_000_000_000 - ( 2 * 1_000_000))
      --         ]
      --       , _mtxFee = 1_000_000
      --       -- , _mtxDCert =
      --       --   [ ModelRegisterStake "alice"
      --       --   , ModelRegisterPool (ModelPoolParams "pool1" (Coin 0) (Coin 0) (unitIntervalFromRational (1 % 1) ) "alice")
      --       --   , ModelDelegate (ModelDelegation "alice" "pool1")
      --       --   ]
      --       }
      --     ]
      --   ] (ModelBlocksMade $ Map.fromList [("pool1", 100)])
      , ModelEpoch [] (ModelBlocksMade $ Map.fromList [("pool1", 100)])
      , ModelEpoch [] (ModelBlocksMade $ Map.fromList [("pool1", 100)])
      , ModelEpoch [] (ModelBlocksMade $ Map.fromList [("pool1", 100)])
      , ModelEpoch [] (ModelBlocksMade $ Map.fromList [("pool1", 100)])
      , ModelEpoch [] (ModelBlocksMade $ Map.fromList [("pool1", 100)])
      , ModelEpoch [] (ModelBlocksMade $ Map.fromList [("pool1", 100)])
      , ModelEpoch [] (ModelBlocksMade $ Map.fromList [("pool1", 100)])
      , ModelEpoch [] (ModelBlocksMade $ Map.fromList [("pool1", 100)])
      , ModelEpoch [] (ModelBlocksMade $ Map.fromList [("pool1", 100)])
      , ModelEpoch [] (ModelBlocksMade $ Map.fromList [("pool1", 100)])
      , ModelEpoch [] (ModelBlocksMade $ Map.fromList [("pool1", 100)])
      ]
  , testProperty "xfer" $ testChainModelInteraction proxy
    (Map.fromList
      [ ("alice", Coin 1_000_000_000)
      ])
    [ModelEpoch [ ModelBlock 1
      [ (modelTx 1)
        { _mtxInputs = Set.fromList [ModelGenesisIn "alice"]
        , _mtxOutputs =
          [ ModelTxOut "bob" 100_000_000
          , ModelTxOut "alice" ( 1_000_000_000 - ( 100_000_000 + 1_000_000))
          ]
        , _mtxFee = 1_000_000
        }
      ]
    ] mempty]
  , testProperty "unbalanced" $ testChainModelInteractionRejection proxy
    (ModelValueNotConservedUTxO (Val.inject $ Coin 1_000_000_000) (Val.inject $ Coin 101_000_000))
    (Map.fromList
      [ ("alice", Coin 1_000_000_000)
      ])
    [ModelEpoch [ ModelBlock 1
      [ (modelTx 1)
        { _mtxInputs = (Set.fromList [ModelGenesisIn "alice"])
        , _mtxOutputs = [ModelTxOut "bob" 100_000_000]
        , _mtxFee = 1_000_000
        }
      ]
    ] mempty]
  , testProperty "xfer-2" $ testChainModelInteraction proxy
    (Map.fromList
      [ ("alice", Coin 1_000_000_000)
      ])
    [ModelEpoch [ ModelBlock 1
      [ (modelTx 1)
        { _mtxInputs = Set.fromList [ModelGenesisIn "alice"]
        , _mtxOutputs =
          [ ModelTxOut "bob" 100_000_000
          , ModelTxOut "alice" ( 1_000_000_000 - ( 100_000_000 + 1_000_000))
          ]
        , _mtxFee = 1_000_000
        }
      ]
    , ModelBlock 2
      [ (modelTx 2)
        { _mtxInputs = Set.fromList [ModelTxIn 1 1]
        , _mtxOutputs =
          [ ModelTxOut "bob" 100_000_000
          , ModelTxOut "alice" (1_000_000_000 - 2 * ( 100_000_000 + 1_000_000))
          ]
        , _mtxFee = 1_000_000
        }
      ]
    ] mempty]
  ]


defaultTestMain :: IO ()
defaultTestMain = defaultMain $
  testGroup "new-unit-tests"
    [ newTestFw (Proxy :: Proxy (ShelleyEra C_Crypto))
    , newTestFw (Proxy :: Proxy (AlonzoEra C_Crypto))
    ]
