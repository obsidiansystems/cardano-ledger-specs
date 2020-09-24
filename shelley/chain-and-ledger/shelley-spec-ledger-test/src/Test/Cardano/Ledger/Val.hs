
module Test.Cardano.Ledger.Val where

import Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC
import Shelley.Spec.Ledger.Coin (Coin (..))
import Cardano.Ledger.Val (Val)

class Val t => ValTest t where
  genVal :: Gen t
  shrinkVal :: t -> [t]

instance ValTest Coin where
  genVal = genCoin 1 1000000000
  shrinkVal = shrinkCoin

shrinkCoin :: Coin -> [Coin]
shrinkCoin (Coin x) = Coin <$> QC.shrinkIntegral x

-- TODO this should be an exponential distribution, not constant
genCoin :: Integer -> Integer -> Gen Coin
genCoin minCoin maxCoin = Coin <$> QC.choose (minCoin, maxCoin)

