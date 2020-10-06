{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Shelley.Spec.Ledger.PoolRanking where

import Data.Ratio
import Test.Shelley.Spec.Ledger.Examples.Cast
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
import Cardano.Ledger.Shelley
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Shelley.Spec.Ledger.Address
import Shelley.Spec.Ledger.PParams
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.Coin
import Shelley.Spec.Ledger.EpochBoundary
import Shelley.Spec.Ledger.Rewards
import Shelley.Spec.Ledger.Keys
import Shelley.Spec.Ledger.TxBody
import Test.Shelley.Spec.Ledger.Generator.Core
import Test.Shelley.Spec.Ledger.Utils

toAda :: Integer -> Coin
toAda lovelace = Coin $ lovelace * 1000000

onePCTpledge :: Coin
onePCTpledge = toAda 111000

onePCTcost :: Coin
onePCTcost = toAda 340

onePCTmargin :: UnitInterval
onePCTmargin = unsafeMkUnitInterval 0.01

onePCTparams :: PoolParams (Shelley C_Crypto)
onePCTparams =
  PoolParams
    { _poolPubKey = (hashKey . vKey . cold) alicePoolKeys,
      _poolVrf = hashVerKeyVRF . snd $ vrf (alicePoolKeys @(Shelley C_Crypto)),
      _poolPledge = onePCTpledge,
      _poolCost = onePCTcost,
      _poolMargin = onePCTmargin,
      _poolRAcnt = RewardAcnt Testnet aliceSHK,
      _poolOwners = Set.singleton $ (hashKey . vKey) aliceStake,
      _poolRelays = StrictSeq.empty,
      _poolMD = SNothing
    }

ppEx :: PParams era
ppEx =
  emptyPParams
    {
      _d = unsafeMkUnitInterval 0.62,
      _tau = unsafeMkUnitInterval 0.2,
      _rho = unsafeMkUnitInterval 0.003,
      _nOpt = 150,
      _a0 = 3 % 10
    }

rPot :: Coin
rPot = toAda $ 32*1000*1000

totalStake :: Coin
totalStake = toAda $ 31800000000

onePCTperf :: Double
onePCTperf = 1

onePCTDesirability :: Double
onePCTDesirability =
  desirability
    ppEx
    rPot
    onePCTparams
    (PerformanceEstimate onePCTperf)
    totalStake

toShare :: Integer -> StakeShare
toShare x = StakeShare $ x % (unCoin totalStake)

onePCTPledgeShare :: StakeShare
onePCTPledgeShare = toShare $ unCoin onePCTpledge

memberShare :: StakeShare
memberShare = toShare 10000000

z0 :: Rational
z0 = 1 % (fromIntegral (_nOpt ppEx))

data PoolCliff = Top | Bottom

onePCTNMShare :: PoolCliff -> StakeShare
onePCTNMShare Top = StakeShare z0
onePCTNMShare Bottom = onePCTPledgeShare

onePCTmaxPool :: PoolCliff -> Coin
onePCTmaxPool pc =
  maxPool
    ppEx
    rPot
    (unStakeShare $ onePCTNMShare pc)
    (unStakeShare onePCTPledgeShare)

onePCTfHat :: PoolCliff -> Coin
onePCTfHat pc = Coin $
  floor (onePCTperf * (fromRational . coinToRational) (onePCTmaxPool pc))

onePCTnmmr :: PoolCliff -> Coin
onePCTnmmr pc =
  nonMyopicMemberRew
    ppEx
    onePCTparams
    rPot
    onePCTPledgeShare
    memberShare
    (onePCTNMShare pc)
    (PerformanceEstimate onePCTperf)
