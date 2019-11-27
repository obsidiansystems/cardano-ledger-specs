{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE PatternSynonyms   #-}

module Test.Cardano.Chain.UTxO.CBOR
  ( tests
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import qualified Data.Map.Strict as M
import Data.Typeable (typeRep)
import Data.Vector (Vector)

import Hedgehog (Gen, Property)
import qualified Hedgehog as H

import Cardano.Binary (ToCBOR, Case(..), LengthOf, SizeOverride(..), szCases)
import Cardano.Chain.Common (AddrAttributes(..), Attributes(..), mkAttributes)
import Cardano.Chain.UTxO
  (Tx(..), TxIn(..), TxInWitness(..), TxOut(..), TxSigData(..), taTx, taWitness, txInWitnesses)
import Cardano.Crypto (pattern ProtocolMagicId, SignTag(..), Signature, sign)

import Test.Cardano.Binary.Helpers (SizeTestConfig(..), scfg, sizeTest)
import Test.Cardano.Binary.Helpers.GoldenRoundTrip
  (goldenTestCBOR, goldenTestCBORAnnotated, roundTripsCBORBuildable, roundTripsCBORAnnotatedBuildable, roundTripsCBORShow, roundTripsCBORAnnotatedShow)
import Test.Cardano.Chain.UTxO.Example
  ( exampleHashTx
  , exampleRedeemSignature
  , exampleTxId
  , exampleTxInList
  , exampleTxInUtxo
  , exampleTxOut
  , exampleTxOut1
  , exampleTxOutList
  , exampleTxPayload1
  , exampleTxProof
  , exampleTxSig
  , exampleTxSigData
  , exampleTxWitness
  )
import Test.Cardano.Chain.UTxO.Gen
  ( genTx
  , genTxAttributes
  , genTxAux
  , genTxHash
  , genTxId
  , genTxIn
  , genTxInList
  , genTxInWitness
  , genTxOut
  , genTxOutList
  , genTxPayload
  , genTxProof
  , genTxSig
  , genTxSigData
  , genTxValidationError
  , genTxWitness
  , genUTxOError
  , genUTxOValidationError
  )
import Test.Cardano.Crypto.Example
  (exampleVerificationKey, exampleRedeemVerificationKey, exampleSigningKey)
import Test.Cardano.Crypto.Gen (feedPM)
import Test.Options (TSGroup, TSProperty, concatTSGroups, eachOfTS)


--------------------------------------------------------------------------------
-- Tx
--------------------------------------------------------------------------------

goldenTx :: Property
goldenTx = goldenTestCBORAnnotated tx "test/golden/cbor/utxo/Tx"
  where tx = Tx exampleTxInList exampleTxOutList (mkAttributes ())

ts_roundTripTx :: TSProperty
ts_roundTripTx = eachOfTS 50 genTx roundTripsCBORAnnotatedBuildable


--------------------------------------------------------------------------------
-- TxAttributes
--------------------------------------------------------------------------------

goldenTxAttributes :: Property
goldenTxAttributes = goldenTestCBOR txA "test/golden/cbor/utxo/TxAttributes"
  where txA = mkAttributes ()

ts_roundTripTxAttributes :: TSProperty
ts_roundTripTxAttributes = eachOfTS 10 genTxAttributes roundTripsCBORBuildable


--------------------------------------------------------------------------------
-- TxAux
--------------------------------------------------------------------------------

ts_roundTripTxAux :: TSProperty
ts_roundTripTxAux = eachOfTS 100 (feedPM genTxAux) roundTripsCBORAnnotatedBuildable


--------------------------------------------------------------------------------
-- Tx Hash
--------------------------------------------------------------------------------

goldenHashTx :: Property
goldenHashTx = goldenTestCBOR exampleHashTx "test/golden/cbor/utxo/HashTx"

ts_roundTripHashTx :: TSProperty
ts_roundTripHashTx = eachOfTS 50 genTxHash roundTripsCBORBuildable


--------------------------------------------------------------------------------
-- TxIn
--------------------------------------------------------------------------------

goldenTxInUtxo :: Property
goldenTxInUtxo =
  goldenTestCBOR exampleTxInUtxo "test/golden/cbor/utxo/TxIn_Utxo"

ts_roundTripTxIn :: TSProperty
ts_roundTripTxIn = eachOfTS 100 genTxIn roundTripsCBORBuildable


--------------------------------------------------------------------------------
-- TxId
--------------------------------------------------------------------------------

goldenTxId :: Property
goldenTxId = goldenTestCBOR exampleTxId "test/golden/cbor/utxo/TxId"

ts_roundTripTxId :: TSProperty
ts_roundTripTxId = eachOfTS 50 genTxId roundTripsCBORBuildable


--------------------------------------------------------------------------------
-- TxInList
--------------------------------------------------------------------------------

goldenTxInList :: Property
goldenTxInList = goldenTestCBOR exampleTxInList "test/golden/cbor/utxo/TxInList"

ts_roundTripTxInList :: TSProperty
ts_roundTripTxInList = eachOfTS 50 genTxInList roundTripsCBORShow


--------------------------------------------------------------------------------
-- TxInWitness
--------------------------------------------------------------------------------

goldenVKWitness :: Property
goldenVKWitness = goldenTestCBOR
  vkWitness
  "test/golden/cbor/utxo/TxInWitness_VKWitness"
  where vkWitness = VKWitness exampleVerificationKey exampleTxSig

goldenRedeemWitness :: Property
goldenRedeemWitness = goldenTestCBOR
  redeemWitness
  "test/golden/cbor/utxo/TxInWitness_RedeemWitness"
 where
  redeemWitness = RedeemWitness exampleRedeemVerificationKey exampleRedeemSignature

ts_roundTripTxInWitness :: TSProperty
ts_roundTripTxInWitness =
  eachOfTS 50 (feedPM genTxInWitness) roundTripsCBORBuildable


--------------------------------------------------------------------------------
-- TxOutList
--------------------------------------------------------------------------------

goldenTxOutList :: Property
goldenTxOutList =
  goldenTestCBOR exampleTxOutList "test/golden/cbor/utxo/TxOutList"

ts_roundTripTxOutList :: TSProperty
ts_roundTripTxOutList = eachOfTS 50 genTxOutList roundTripsCBORShow


--------------------------------------------------------------------------------
-- TxOut
--------------------------------------------------------------------------------

goldenTxOut :: Property
goldenTxOut = goldenTestCBOR exampleTxOut "test/golden/cbor/utxo/TxOut"

goldenTxOut1 :: Property
goldenTxOut1 = goldenTestCBOR exampleTxOut1 "test/golden/cbor/utxo/TxOut1"

ts_roundTripTxOut :: TSProperty
ts_roundTripTxOut = eachOfTS 50 genTxOut roundTripsCBORBuildable


--------------------------------------------------------------------------------
-- TxPayload
--------------------------------------------------------------------------------

goldenTxPayload1 :: Property
goldenTxPayload1 =
  goldenTestCBORAnnotated exampleTxPayload1 "test/golden/cbor/utxo/TxPayload1"

ts_roundTripTxPayload :: TSProperty
ts_roundTripTxPayload = eachOfTS 50 (feedPM genTxPayload) roundTripsCBORAnnotatedShow


--------------------------------------------------------------------------------
-- TxProof
--------------------------------------------------------------------------------

goldenTxProof :: Property
goldenTxProof = goldenTestCBORAnnotated exampleTxProof "test/golden/cbor/utxo/TxProof"

ts_roundTripTxProof :: TSProperty
ts_roundTripTxProof = eachOfTS 50 (feedPM genTxProof) roundTripsCBORAnnotatedBuildable


--------------------------------------------------------------------------------
-- TxSig
--------------------------------------------------------------------------------

goldenTxSig :: Property
goldenTxSig = goldenTestCBOR txSigGold "test/golden/cbor/utxo/TxSig"
 where
  txSigGold = sign
    (ProtocolMagicId 0)
    SignForTestingOnly
    exampleSigningKey
    exampleTxSigData

ts_roundTripTxSig :: TSProperty
ts_roundTripTxSig = eachOfTS 50 (feedPM genTxSig) roundTripsCBORBuildable


--------------------------------------------------------------------------------
-- TxSigData
--------------------------------------------------------------------------------

goldenTxSigData :: Property
goldenTxSigData =
  goldenTestCBOR exampleTxSigData "test/golden/cbor/utxo/TxSigData"

ts_roundTripTxSigData :: TSProperty
ts_roundTripTxSigData = eachOfTS 50 genTxSigData roundTripsCBORShow


--------------------------------------------------------------------------------
-- TxValidationError
--------------------------------------------------------------------------------

ts_roundTripTxValidationError :: TSProperty
ts_roundTripTxValidationError =
  eachOfTS 50 genTxValidationError roundTripsCBORAnnotatedShow


--------------------------------------------------------------------------------
-- TxWitness
--------------------------------------------------------------------------------

goldenTxWitness :: Property
goldenTxWitness =
  goldenTestCBORAnnotated exampleTxWitness "test/golden/cbor/utxo/TxWitness"

ts_roundTripTxWitness :: TSProperty
ts_roundTripTxWitness = eachOfTS 20 (feedPM genTxWitness) roundTripsCBORAnnotatedShow


--------------------------------------------------------------------------------
-- UtxOError
--------------------------------------------------------------------------------

ts_roundTripUTxOError :: TSProperty
ts_roundTripUTxOError =
  eachOfTS 50 genUTxOError roundTripsCBORShow


--------------------------------------------------------------------------------
-- UTxOValidationError
--------------------------------------------------------------------------------

ts_roundTripUTxOValidationError :: TSProperty
ts_roundTripUTxOValidationError =
  eachOfTS 50 genUTxOValidationError roundTripsCBORAnnotatedShow


--------------------------------------------------------------------------------
-- Size Estimates
--------------------------------------------------------------------------------

sizeEstimates :: H.Group
sizeEstimates
  = let
      sizeTestGen :: (Show a, ToCBOR a) => Gen a -> Property
      sizeTestGen g = sizeTest $ scfg { gen = g }
      pm           = ProtocolMagicId 0

      -- Explicit bounds for types, based on the generators from Gen.
      attrUnitSize = (typeRep (Proxy @(Attributes ())), SizeConstant 1)
      attrAddrSize =
        ( typeRep (Proxy @(Attributes AddrAttributes))
        , SizeConstant (szCases [Case "min" 1, Case "max" 1024])
        )
      txSigSize = (typeRep (Proxy @(Signature TxSigData)), SizeConstant 66)
    in H.Group
      "Encoded size bounds for core types."
      [ ("TxId", sizeTestGen genTxId)
      , ( "Tx"
        , sizeTest $ scfg
          { gen         = genTx
          , addlCtx     = M.fromList [attrUnitSize, attrAddrSize]
          , computedCtx = \tx -> M.fromList
            [ ( typeRep (Proxy @(LengthOf [TxIn]))
              , SizeConstant (fromIntegral $ length $ txInputs tx)
              )
            , ( typeRep (Proxy @(LengthOf [TxOut]))
              , SizeConstant (fromIntegral $ length $ txOutputs tx)
              )
            ]
          }
        )
      , ("TxIn", sizeTestGen genTxIn)
      , ( "TxOut"
        , sizeTest
          $ scfg { gen = genTxOut, addlCtx = M.fromList [attrAddrSize] }
        )
      , ( "TxAux"
        , sizeTest $ scfg
          { gen         = genTxAux pm
          , addlCtx     = M.fromList [attrUnitSize, attrAddrSize, txSigSize]
          , computedCtx = \ta -> M.fromList
            [ ( typeRep (Proxy @(LengthOf [TxIn]))
              , SizeConstant (fromIntegral $ length $ txInputs $ taTx ta)
              )
            , ( typeRep (Proxy @(LengthOf (Vector TxInWitness)))
              , SizeConstant (fromIntegral $ length $ txInWitnesses $ taWitness ta)
              )
            , ( typeRep (Proxy @(LengthOf [TxOut]))
              , SizeConstant (fromIntegral $ length $ txOutputs $ taTx ta)
              )
            ]
          }
        )
      , ( "TxInWitness"
        , sizeTest
          $ scfg { gen = genTxInWitness pm, addlCtx = M.fromList [txSigSize] }
        )
      , ("TxSigData", sizeTestGen genTxSigData)
      , ( "Signature TxSigData"
        , sizeTest
          $ scfg { gen = genTxSig pm, addlCtx = M.fromList [txSigSize] }
        )
      ]


--------------------------------------------------------------------------------
-- Main test export
--------------------------------------------------------------------------------

tests :: TSGroup
tests = concatTSGroups
  [const $$discoverGolden, $$discoverRoundTripArg, const sizeEstimates]
