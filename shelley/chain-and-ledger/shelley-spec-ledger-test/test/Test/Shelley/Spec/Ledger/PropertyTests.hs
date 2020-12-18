{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Shelley.Spec.Ledger.PropertyTests
  ( propertyTests,
    minimalPropertyTests,
  )
where

import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash, ValidateAuxiliaryData)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto)
import Control.State.Transition (Environment, Signal, State)
import qualified Control.State.Transition.Trace.Generator.QuickCheck as QC
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import GHC.Records (HasField (..))
import Shelley.Spec.Ledger.BaseTypes
  ( Globals,
    StrictMaybe (..),
  )
import Shelley.Spec.Ledger.BlockChain (Block)
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Delegation.Certificates (DCert)
import Shelley.Spec.Ledger.PParams (Update (..))
import Shelley.Spec.Ledger.STS.Chain (ChainState)
import Shelley.Spec.Ledger.STS.Ledger (LEDGER)
import Shelley.Spec.Ledger.TxBody (TxIn, TxOut, Wdrl)
import Test.Shelley.Spec.Ledger.Address.Bootstrap
  ( bootstrapHashTest,
  )
import Test.Shelley.Spec.Ledger.Address.CompactAddr
  ( propCompactAddrRoundTrip,
    propCompactSerializationAgree,
    propDecompactAddrLazy,
    propDecompactShelleyLazyAddr,
  )
import Test.Shelley.Spec.Ledger.ByronTranslation (testGroupByronTranslation)
import Test.Shelley.Spec.Ledger.Generator.Core (GenEnv)
import Test.Shelley.Spec.Ledger.Generator.EraGen (EraGen)
import Test.Shelley.Spec.Ledger.Rules.ClassifyTraces
  ( onlyValidChainSignalsAreGenerated,
    onlyValidLedgerSignalsAreGenerated,
    relevantCasesAreCovered,
  )
import Test.Shelley.Spec.Ledger.Rules.TestChain
  ( adaPreservationChain,
    collisionFreeComplete,
    delegProperties,
    poolProperties,
    removedAfterPoolreap,
  )
import Test.Shelley.Spec.Ledger.ShelleyTranslation (testGroupShelleyTranslation)
import Test.Shelley.Spec.Ledger.Utils (ChainProperty)
import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as TQC

minimalPropertyTests ::
  forall era.
  ( EraGen era,
    ChainProperty era,
    ValidateAuxiliaryData era,
    QC.HasTrace (Core.EraRule "CHAIN" era) (GenEnv era),
    Show (Environment (Core.EraRule "CHAIN" era)),
    State (Core.EraRule "CHAIN" era) ~ ChainState era,
    Signal (Core.EraRule "CHAIN" era) ~ Block era,
    QC.BaseEnv (Core.EraRule "CHAIN" era) ~ Globals,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "outputs" (Core.TxBody era) (StrictSeq (TxOut era)),
    HasField "txfee" (Core.TxBody era) Coin,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "adHash" (Core.TxBody era) (StrictMaybe (AuxiliaryDataHash (Crypto era))),
    HasField "update" (Core.TxBody era) (StrictMaybe (Update era))
  ) =>
  TestTree
minimalPropertyTests =
  testGroup
    "Minimal Property Tests"
    [ TQC.testProperty "Chain and Ledger traces cover the relevant cases" (relevantCasesAreCovered @era),
      TQC.testProperty "total amount of Ada is preserved (Chain)" (adaPreservationChain @era),
      TQC.testProperty "Only valid CHAIN STS signals are generated" (onlyValidChainSignalsAreGenerated @era),
      bootstrapHashTest,
      testGroup
        "Compact Address Tests"
        [ TQC.testProperty "Compact address round trip" (propCompactAddrRoundTrip @(Crypto era)),
          TQC.testProperty "Compact address binary representation" (propCompactSerializationAgree @(Crypto era)),
          TQC.testProperty "determining address type doesn't force contents" (propDecompactAddrLazy @(Crypto era)),
          TQC.testProperty "reading the keyhash doesn't force the stake reference" (propDecompactShelleyLazyAddr @(Crypto era))
        ]
    ]

-- | 'TestTree' of property-based testing properties.
propertyTests ::
  forall era.
  ( EraGen era,
    ChainProperty era,
    ValidateAuxiliaryData era,
    QC.HasTrace (Core.EraRule "CHAIN" era) (GenEnv era),
    QC.HasTrace (Core.EraRule "LEDGER" era) (GenEnv era),
    Show (Environment (Core.EraRule "CHAIN" era)),
    State (Core.EraRule "CHAIN" era) ~ ChainState era,
    Signal (Core.EraRule "CHAIN" era) ~ Block era,
    QC.BaseEnv (Core.EraRule "CHAIN" era) ~ Globals,
    Core.EraRule "LEDGER" era ~ LEDGER era,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "outputs" (Core.TxBody era) (StrictSeq (TxOut era)),
    HasField "txfee" (Core.TxBody era) Coin,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "adHash" (Core.TxBody era) (StrictMaybe (AuxiliaryDataHash (Crypto era))),
    HasField "update" (Core.TxBody era) (StrictMaybe (Update era))
  ) =>
  TestTree
propertyTests =
  testGroup
    "Property-Based Testing"
    [ testGroup
        "Classify Traces"
        [ TQC.testProperty
            "Chain and Ledger traces cover the relevant cases"
            (relevantCasesAreCovered @era)
        ],
      testGroup
        "STS Rules - Delegation Properties"
        [ TQC.testProperty
            "properties of the DELEG STS"
            (delegProperties @era)
        ],
      testGroup
        "STS Rules - Pool Properties"
        [ TQC.testProperty
            "properties of the POOL STS"
            (poolProperties @era)
        ],
      testGroup
        "STS Rules - Poolreap Properties"
        [ TQC.testProperty
            "pool is removed from stake pool and retiring maps"
            (removedAfterPoolreap @era)
        ],
      testGroup
        "CHAIN level Properties"
        [ TQC.testProperty
            "collection of Ada preservation properties"
            (adaPreservationChain @era),
          TQC.testProperty
            "inputs are eliminated, outputs added to utxo and TxIds are unique"
            (collisionFreeComplete @era)
        ],
      testGroup
        "Properties of Trace generators"
        [ TQC.testProperty
            "Only valid LEDGER STS signals are generated"
            (onlyValidLedgerSignalsAreGenerated @era),
          TQC.testProperty
            "Only valid CHAIN STS signals are generated"
            (onlyValidChainSignalsAreGenerated @era)
        ],
      testGroupByronTranslation,
      testGroupShelleyTranslation
    ]
