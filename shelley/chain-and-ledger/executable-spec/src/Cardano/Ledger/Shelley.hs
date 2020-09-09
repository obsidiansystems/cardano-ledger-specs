{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Shelley where

import qualified Cardano.Ledger.Crypto as CryptoClass
import Cardano.Ledger.Era
import Cardano.Ledger.Core (Value, ValType)
import Shelley.Spec.Ledger.Coin (Coin)


--------------------------------------------------------------------------------
-- Shelley Era
--------------------------------------------------------------------------------

data Shelley c

instance CryptoClass.Crypto c => Era (Shelley c) where
  type Crypto (Shelley c) = c

type instance Value (Shelley c) = Coin

instance ValType (Shelley c)

