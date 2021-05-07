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
import Data.Typeable
import Shelley.Spec.Ledger.API.Genesis
import Shelley.Spec.Ledger.API.Validation
import Test.Cardano.Ledger.ModelChain
import Test.Cardano.Ledger.ModelChain.Utils
import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Val as Val
import qualified Data.Map as Map
import qualified Data.Set as Set

newTestFw ::
  forall era proxy.
  ( ApplyBlock era
  , ElaborateEraModel era
  , Default (AdditionalGenesisConfig era)
  , Default (ElaborateEraModelState era)
  , Eq (PredicateFailure (Core.EraRule "LEDGER" era))
  , Show (PredicateFailure (Core.EraRule "LEDGER" era))
  , Typeable era
  )
  => proxy era -> TestTree
newTestFw proxy = testGroup (show $ typeRep proxy)
  [ testProperty "noop" $ testChainModelInteraction proxy Map.empty []

  , testProperty "noop-2" $ testChainModelInteraction proxy
    (Map.fromList
      [ ("alice", Coin 1_000_000)
      , ("bob", Coin 1_000_000)
      ])
    []
  , testProperty "xfer" $ testChainModelInteraction proxy
    (Map.fromList
      [ ("alice", Coin 1_000_000_000)
      ])
    [ ModelBlock 1
      [ ModelTx
        { _mtxId = 1
        , _mtxInputs = Set.fromList [ModelGensisIn "alice"]
        , _mtxOutputs =
          [ ModelTxOut "bob" 100_000_000
          , ModelTxOut "alice" ( 1_000_000_000 - ( 100_000_000 + 1_000_000))
          ]
        , _mtxFee = 1_000_000
        , _mtxWitness = Set.fromList ["alice"]
        }
      ]
    ]
  , testProperty "unbalanced" $ testChainModelInteractionRejection proxy
    (ModelValueNotConservedUTxO (Val.inject $ Coin 1_000_000_000) (Val.inject $ Coin 101_000_000))
    (Map.fromList
      [ ("alice", Coin 1_000_000_000)
      ])
    [ ModelBlock 1
      [ ModelTx 1
        (Set.fromList [ModelGensisIn "alice"])
        [ModelTxOut "bob" 100_000_000]
        1_000_000
        (Set.fromList ["alice", "bob"])
      ]
    ]
  , testProperty "xfer-2" $ testChainModelInteraction proxy
    (Map.fromList
      [ ("alice", Coin 1_000_000_000)
      ])
    [ ModelBlock 1
      [ ModelTx
        { _mtxId = 1
        , _mtxInputs = Set.fromList [ModelGensisIn "alice"]
        , _mtxOutputs =
          [ ModelTxOut "bob" 100_000_000
          , ModelTxOut "alice" ( 1_000_000_000 - ( 100_000_000 + 1_000_000))
          ]
        , _mtxFee = 1_000_000
        , _mtxWitness = Set.fromList ["alice"]
        }
      ]
    , ModelBlock 2
      [ ModelTx
        { _mtxId = 2
        , _mtxInputs = Set.fromList [ModelTxIn 1 1]
        , _mtxOutputs =
          [ ModelTxOut "bob" 100_000_000
          , ModelTxOut "alice" (1_000_000_000 - 2 * ( 100_000_000 + 1_000_000))
          ]
        , _mtxFee = 1_000_000
        , _mtxWitness = Set.fromList ["alice"]
        }
      ]
    ]
  ]



