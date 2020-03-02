{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE EmptyDataDecls        #-}

import qualified Data.Fixed as FP
import           Test.QuickCheck
import           NonIntegral

data E34

instance FP.HasResolution E34 where
    resolution _ = 10^(34::Int)

type FixedPoint = FP.Fixed E34

epsD :: Double
epsD = 1 / 10^(12::Int)

epsFP :: FixedPoint
epsFP = 1 / 10^(16::Int)

eps :: Rational
eps = 1 / 10^(16::Int)

map2 :: (a -> b) -> (a,a) -> (b,b)
map2 f (x,y) = (f x, f y)

both :: (a -> Bool) -> (a,a) -> Bool
both p (x,y) = p x && p y

type Diff a = (a,a)

absDiff :: Num a => Diff a -> a
absDiff (x,y) = abs (x - y)

(~=) :: (Ord a, Num a) => Diff a -> a -> Bool
w ~= epsilon = absDiff w < epsilon

newtype Normalized a = Norm (a,a) deriving Show
newtype UnitInterval a = Unit a deriving Show

instance (Fractional a, Arbitrary a) => Arbitrary (Normalized a) where
    arbitrary = return . Norm . normalize =<< arbitrary

instance (Fractional a, Arbitrary a) => Arbitrary (UnitInterval a) where
    arbitrary = return . Unit . toUnit =<< arbitrary

type NonNegInts = (NonNegative Integer, Positive Integer)

-- | Normalizes the integers, return a pair of integers, such that:
-- fst >= 0, snd > 0, fst <= snd.
normalizeInts :: NonNegInts -> (Integer,Integer)
normalizeInts (NonNegative x, Positive y) = (min x y, max x y)

normalize :: Fractional a => NonNegInts -> (a,a)
normalize = map2 fromInteger . normalizeInts

toUnit :: Fractional a => NonNegInts -> a
toUnit = uncurry (/) . normalize -- [0,1]

------------------------
-- Generic Properties --
------------------------

type Monotonic a = (a -> Bool) -> (a -> a) -> a -> a -> Property

monotonic :: (Ord a, Ord b) => (a -> b) -> a -> a -> Bool
monotonic f x y =
    if x < y
    then f x < f y
    else f x >= f y

log_pow :: (RealFrac a, Show a, Enum a) => a -> a -> Diff a
log_pow x y = (ln' (x *** y) , y * ln' x)

log_law :: (RealFrac a, Show a, Enum a) => a -> a -> Diff a
log_law x y = (ln' (x * y) , ln' x + ln' y)

exp_law :: (RealFrac a, Show a) => a -> a -> Diff a
exp_law x y = (exp' (x + y) , exp' x * exp' y)

exp_log,log_exp :: (RealFrac a, Show a, Enum a) => a -> Diff a
exp_log x = (exp' (ln' x), x)
log_exp x = (ln' (exp' x), x)

exp_UnitInterval :: (RealFrac a, Show a, Enum a) => a -> a -> Bool
exp_UnitInterval x y = let z = x *** y in z >= 0 && z <= 1

findD :: (RealFrac a, Show a) => a -> Bool
findD x = e ^^ n <= x && x < e ^^ (n + 1)
    where n = findE e x
          e = exp' 1

pow_Diff :: (RealFrac a, Enum a, Show a) => a -> (a,a) -> Diff a
pow_Diff z (y,x) = ((z *** (1/x)) *** y , (z *** y) *** (1/x))

leader :: (RealFrac a, Show a, Enum a) => a -> a -> a
leader f sigma = 1 - ((1 - f) *** sigma)

taylor :: (RealFrac a, Show a, Enum a) => a -> a -> a -> a -> Bool
taylor f a p sigma = case taylorExpCmp 3 (1/q) (-sigma*c) of
                        ABOVE _ _    -> p >= a
                        BELOW _ _    -> p <  a
                        MaxReached _ -> False
                        where c = ln' (1 - f)
                              q = 1 - p


---------------------------------------
-- FixedPoint Versions of Properties --
---------------------------------------

prop_FPMonotonic :: Monotonic FixedPoint
prop_FPMonotonic constrain f x y =
    both constrain (x,y) ==>
    classify zeroes "both zero case" $
    (zeroes || monotonic f x y) === True
    where zeroes = both zero (x,y)
          zero a = (f a, 0) ~= epsFP

prop_FPPowDiff :: UnitInterval FixedPoint -> Normalized FixedPoint -> Property
prop_FPPowDiff (Unit z) (Norm w) = let e = absDiff (pow_Diff z w) in
    classify (e < epsFP) "OK 10e-16" $
    classify (e < epsFP * 1000 && e >= epsFP) "OK 10e-12" $
    classify (e >= epsFP * 1000) "KO" $
    True === True

prop_FPExpLaw :: UnitInterval FixedPoint -> UnitInterval FixedPoint -> Property
prop_FPExpLaw (Unit x) (Unit y) = (exp_law x y ~= epsFP) === True

prop_FPlnLaw :: Positive FixedPoint -> Positive FixedPoint -> Property
prop_FPlnLaw (Positive x) (Positive y) = (log_law x y ~= epsFP) === True

prop_FPExpUnitInterval :: UnitInterval FixedPoint -> UnitInterval FixedPoint -> Property
prop_FPExpUnitInterval (Unit x) (Unit y) = exp_UnitInterval x y === True

prop_FPIdemPotent :: Positive FixedPoint -> Property
prop_FPIdemPotent (Positive x) = (exp_log x ~= epsFP) === True

prop_FPIdemPotent' :: Positive FixedPoint -> Property
prop_FPIdemPotent' (Positive x) = (log_exp x ~= epsFP) === True

prop_FPfindD :: Positive FixedPoint -> Property
prop_FPfindD (Positive x) = findD x === True

prop_FPlnPow :: UnitInterval FixedPoint -> UnitInterval FixedPoint -> Property
prop_FPlnPow (Unit x) (Unit y) = (x > 0) ==> (log_pow x y ~= epsFP) === True

prop_LeaderCmp :: UnitInterval FixedPoint -> UnitInterval FixedPoint -> Property
prop_LeaderCmp (Unit p) (Unit s) =
    both (\x -> 0 < x && x < 1) (p,s) ==>
    classify (p < a) "is leader" $ taylor f a p s
    where a = leader f s
          f = 1 / 10

-----------------------------------
-- Double Versions of Properties --
-----------------------------------

prop_DMonotonic :: Monotonic Double
prop_DMonotonic constrain f x y =
    both constrain (x,y) ==> monotonic f x y

-- | Takes very long, but (e *** b) *** c is not an operation that we use.
prop_DPowDiff :: UnitInterval Double -> Normalized Double -> Property
prop_DPowDiff (Unit z) (Norm w) = (pow_Diff z w ~= epsD) === True

prop_DExpLaw :: UnitInterval Double -> UnitInterval Double -> Property
prop_DExpLaw (Unit x) (Unit y) = (exp_law x y ~= epsD) === True

prop_DlnLaw :: Positive Double -> Positive Double -> Property
prop_DlnLaw (Positive x) (Positive y) = (log_law x y ~= epsD) === True

prop_DExpUnitInterval :: UnitInterval Double -> UnitInterval Double -> Property
prop_DExpUnitInterval (Unit x) (Unit y) = exp_UnitInterval x y === True

prop_DIdemPotent :: Positive Double -> Property
prop_DIdemPotent (Positive x) = (exp_log x ~= epsD) === True

prop_DIdemPotent' :: Positive Double -> Property
prop_DIdemPotent' (Positive x) = (log_exp x ~= epsD) === True

prop_DfindD :: Positive Double -> Property
prop_DfindD (Positive x) = findD x === True

prop_DlnPow :: UnitInterval Double -> UnitInterval Double -> Property
prop_DlnPow (Unit x) (Unit y) = (x > 0) ==> (log_pow x y ~= epsD) === True

-----------------------------
-- Properties for Rational --
-----------------------------

prop_Monotonic :: Monotonic Rational
prop_Monotonic constrain f x y =
    both constrain (x,y) ==> monotonic f x y

prop_PowDiff :: UnitInterval Rational -> Normalized Rational -> Property
prop_PowDiff (Unit z) (Norm w) = (pow_Diff z w ~= eps) === True

prop_ExpLaw :: UnitInterval Rational -> UnitInterval Rational -> Property
prop_ExpLaw (Unit x) (Unit y) = (exp_law x y ~= eps) === True

prop_lnLaw :: Positive Rational -> Positive Rational -> Property
prop_lnLaw (Positive x) (Positive y) = (log_law x y ~= eps) === True

prop_ExpUnitInterval :: UnitInterval Rational -> UnitInterval Rational -> Property
prop_ExpUnitInterval (Unit x) (Unit y) = exp_UnitInterval x y === True

prop_IdemPotent :: Positive Rational -> Property
prop_IdemPotent (Positive x) = (exp_log x ~= eps) === True

prop_IdemPotent' :: Positive Rational -> Property
prop_IdemPotent' (Positive x) = (log_exp x ~= eps) === True

prop_findD :: Positive Rational -> Property
prop_findD (Positive x) = findD x === True

prop_lnPow :: UnitInterval Rational -> UnitInterval Rational -> Property
prop_lnPow (Unit x) (Unit y) = (x > 0) ==> (log_pow x y ~= eps) === True


qcWithLabel :: Testable prop => String -> Int -> prop -> IO ()
qcWithLabel text count prop = do
  putStrLn text
  if count > 0
    then quickCheck (withMaxSuccess count prop)
    else putStrLn "--- SKIPPED, takes too long"

main :: IO ()
main = do
  putStrLn "QuickCheck properties for non-integral calculation"
  putStrLn ""

  putStrLn "---------------------"
  putStrLn "-- Test of Double  --"
  putStrLn "---------------------"
  qcWithLabel "property exponential is monotonic"
    1000 $ prop_DMonotonic (const True) exp'
  qcWithLabel "property logarithm is monotonic"
    1000 $ prop_DMonotonic (> 0) ln'
  qcWithLabel "property x,y in [0,1] -> x^y in [0,1]"
    1000 prop_DExpUnitInterval
  qcWithLabel "property x > 0 -> exp(ln(x)) = x"
    1000 prop_DIdemPotent
  qcWithLabel "property x > 0 -> ln(exp(x)) = x"
    1000 prop_DIdemPotent'
  qcWithLabel "property pow diff in [0,1]: (a^(1/x))^y = (a^y)^(1/x)"
    1000 prop_DPowDiff
  qcWithLabel "property exponential law in [0,1]: exp(x + y) = exp(x) · exp(y)"
    1000 prop_DExpLaw
  qcWithLabel "property logarithm law in (0,..): ln(x · y) = ln(x) + ln(y)"
    1000 prop_DlnLaw
  qcWithLabel "property logarithm of pow in [0,1]: ln(x^y) = y · ln(x)"
    1000 prop_DlnPow
  qcWithLabel "check bound of `findE`"
    1000 prop_DfindD
  putStrLn ""

  putStrLn "-------------------------------------------"
  putStrLn "-- Test of 34 Decimal Digits Fixed Point --"
  putStrLn "-------------------------------------------"
  qcWithLabel "property exponential is monotonic"
    1000 $ prop_FPMonotonic (const True) exp'
  qcWithLabel "property logarithm is monotonic"
    1000 $ prop_FPMonotonic (> 0) ln'
  qcWithLabel "property x,y in [0,1] -> x^y in [0,1]"
    1000 prop_FPExpUnitInterval
  qcWithLabel "property x > 0 -> exp(ln(x)) = x"
    1000 prop_FPIdemPotent
  qcWithLabel "property x > 0 -> ln(exp(x)) = x"
    1000 prop_FPIdemPotent'
  qcWithLabel "property pow diff in [0,1]: (a^(1/x))^y = (a^y)^(1/x)"
    1000 prop_FPPowDiff
  qcWithLabel "property exponential law in [0,1]: exp(x + y) = exp(x) · exp(y)"
    1000 prop_FPExpLaw
  qcWithLabel "property logarithm law in (0,..): ln(x · y) = ln(x) + ln(y)"
    1000 prop_FPlnLaw
  qcWithLabel "property logarithm of pow in [0,1]: ln(x^y) = y · ln(x)"
    1000 prop_FPlnPow
  qcWithLabel "check bound of `findE`"
    1000 prop_FPfindD
  qcWithLabel "property σ,p in [0,1]: p < 1 - (1 - f)^σ <=> taylorExpCmp 3 (1/(1 - p)) (-σ · ln (1 - f))"
    10000 prop_LeaderCmp
  putStrLn ""

  putStrLn "------------------------------"
  putStrLn "-- Test of Rational Numbers --"
  putStrLn "------------------------------"
  qcWithLabel "property exponential is monotonic"
    10 $ prop_Monotonic (const True) exp'
  qcWithLabel "property logarithm is monotonic"
    10 $ prop_Monotonic (> 0) ln'
  qcWithLabel "property x,y in [0,1] -> x^y in [0,1]"
    10 prop_ExpUnitInterval
  qcWithLabel "property x > 0 -> exp(ln(x)) = x"
    10 prop_IdemPotent
  qcWithLabel "property x > 0 -> ln(exp(x)) = x"
    10 prop_IdemPotent'
  qcWithLabel "property pow diff in [0,1]: (a^(1/x))^y = (a^y)^(1/x)"
    0 prop_PowDiff
  qcWithLabel "property exponential law in [0,1]: exp(x + y) = exp(x) · exp(y)"
    10 prop_ExpLaw
  qcWithLabel "property logarithm law in (0,..): ln(x · y) = ln(x) + ln(y)"
    10 prop_lnLaw
  qcWithLabel "property logarithm of pow in [0,1]: ln(x^y) = y · ln(x)"
    0 prop_lnPow
  qcWithLabel "check bound of `findE`"
    100 prop_findD
  putStrLn ""
