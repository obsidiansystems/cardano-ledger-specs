{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.ModelChain.Value where

import Cardano.Ledger.Coin
import Cardano.Ledger.Val
import Control.Lens
import Control.Monad
import qualified Control.Monad.Except as Except
import Numeric.Natural

data ModelValueF a
  = ModelValue_Var a
  | ModelValue_Inject Coin
  | ModelValue_Add (ModelValueF a) (ModelValueF a)
  | ModelValue_Scale Natural (ModelValueF a)
  | ModelValue_Sub (ModelValueF a) (ModelValueF a)
  deriving (Functor, Foldable, Traversable, Show, Eq, Ord)

data ModelValueError a
  = ValueUnderflow a a
  deriving (Show)

evalModelValue ::
  forall val m a.
  (Val val, Monad m) =>
  (a -> m val) ->
  ModelValueF a ->
  m (Either (ModelValueError val) val)
evalModelValue env x = Except.runExceptT (go x)
  where
    go :: ModelValueF a -> Except.ExceptT (ModelValueError val) m val
    go (ModelValue_Var a) = Except.lift $ env a
    go (ModelValue_Inject c) = pure (inject c)
    go (ModelValue_Add a b) = (<+>) <$> go a <*> go b
    go (ModelValue_Scale n a) = (toInteger n <×>) <$> go a
    go (ModelValue_Sub a b) = do
      a' <- go a
      b' <- go b
      unless (pointwise (>=) a' b') $ Except.throwError $ ValueUnderflow a' b'
      pure $ a' <-> b'
