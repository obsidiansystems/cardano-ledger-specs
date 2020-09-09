{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.Coin
  ( Coin (..),
    Core.CompactForm (..),
    word64ToCoin,
    coinToRational,
    rationalToCoinViaFloor,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Prelude (NFData, NoUnexpectedThunks (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Group (Abelian, Group (..))
import Data.Monoid (Sum (..))
import Data.PartialOrd (PartialOrd)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Quiet

-- | The amount of value held by a transaction output.
newtype Coin = Coin {unCoin :: Integer}
  deriving
    ( Eq,
      Ord,
      Enum,
      NoUnexpectedThunks,
      Generic,
      ToJSON,
      FromJSON,
      NFData
    )
  deriving (Show) via Quiet Coin
  deriving (ToCBOR, FromCBOR) via Core.Compact Coin
  deriving (Semigroup, Monoid, Group, Abelian) via Sum Integer
  deriving newtype (PartialOrd)

word64ToCoin :: Word64 -> Coin
word64ToCoin w = Coin $ fromIntegral w

coinToRational :: Coin -> Rational
coinToRational (Coin c) = fromIntegral c

rationalToCoinViaFloor :: Rational -> Coin
rationalToCoinViaFloor r = Coin . floor $ r

instance Core.Compactible Coin where
  newtype CompactForm Coin = CompactCoin Word64
  toCompact = CompactCoin . fromIntegral . unCoin
  fromCompact (CompactCoin c) = word64ToCoin c

instance ToCBOR (Core.CompactForm Coin) where
  toCBOR (CompactCoin c) = toCBOR c

instance FromCBOR (Core.CompactForm Coin) where
  fromCBOR = CompactCoin <$> fromCBOR
