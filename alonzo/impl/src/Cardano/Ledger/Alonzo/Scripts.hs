{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Cardano.Ledger.Alonzo.Scripts
  ( Tag (..),
    Script (..),
    ExUnits (..),
  )
where

import Cardano.Binary
import Cardano.Ledger.Era (Era (Crypto))
import qualified Cardano.Ledger.Crypto as CC(Crypto)
import Cardano.Ledger.ShelleyMA.Timelocks
import Data.Coders
import Data.Word (Word64)
import Data.Typeable
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

-- | Marker indicating the part of a transaction for which this script is acting
-- as a validator.
data Tag
  = -- | Validates spending a script-locked UTxO
    Spend
  | -- | Validates minting new tokens
    Mint
  | -- | Validates certificate transactions
    Cert
  | -- | Validates withdrawl from a reward account
    Rewrd
  deriving (Eq, Generic, Ord, Show)

instance NoThunks Tag


data Script era
  = NativeScript (Timelock era)
  | PlutusScript
-- type Script era = Timelock (Crypto era)

-- | Arbitrary execution unit in which we measure the cost of scripts.
data ExUnits = ExUnits
  { exUnitsMem :: !Word64,
    exUnitsSteps :: !Word64
  }
  deriving (Eq, Generic, Show, Ord)

instance NoThunks ExUnits

instance Semigroup ExUnits where
  ExUnits a c <> ExUnits b d = ExUnits (a + b) (c + d)

instance Monoid ExUnits where
  mempty = ExUnits 0 0


--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

instance ToCBOR Tag where
  toCBOR = encode . encodeTag
    where
      encodeTag Spend = Sum Spend 0
      encodeTag Mint = Sum Mint 1
      encodeTag Cert = Sum Cert 2
      encodeTag Rewrd = Sum Rewrd 3

instance FromCBOR Tag where
  fromCBOR = decode $ Summands "Tag" decodeTag
    where
      decodeTag 0 = SumD Spend
      decodeTag 1 = SumD Mint
      decodeTag 2 = SumD Cert
      decodeTag 3 = SumD Rewrd
      decodeTag n = Invalid n

instance ToCBOR ExUnits where
  toCBOR (ExUnits m s) = encode $ Rec ExUnits !> To m !> To s

instance FromCBOR ExUnits where
  fromCBOR = decode $ RecD ExUnits <! From <! From

instance Typeable era => ToCBOR (Script era) where
  toCBOR x = encode(encodeScript x)
     where encodeScript (NativeScript i) = Sum NativeScript 0 !> To i
           encodeScript PlutusScript     = Sum PlutusScript 1

instance (CC.Crypto era,Typeable era) => FromCBOR (Annotator (Script era)) where
  fromCBOR = decode (Summands "Alonzo Script" decodeScript)
     where decodeScript 0 = Ann (SumD NativeScript) <*! From
           decodeScript 1 = Ann(SumD PlutusScript)