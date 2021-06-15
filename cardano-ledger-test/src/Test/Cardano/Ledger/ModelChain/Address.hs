{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Cardano.Ledger.ModelChain.Address where

import qualified GHC.Exts as GHC

newtype ModelAddress = ModelAddress String
  deriving (Eq, Ord, Show, GHC.IsString)
