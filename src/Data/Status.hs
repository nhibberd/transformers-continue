{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

-- | Status type.
module Data.Status (
  -- * Status
    Status (..)
  ) where


import           Control.Applicative (Applicative (..))
import           Control.Monad (Monad (..))

import           Data.Bifunctor (Bifunctor (..))
import           Data.Eq (Eq)
import           Data.Foldable (Foldable (..))
import           Data.Function (($),)
import           Data.Functor (Functor (..), (<$>))
import           Data.Traversable (Traversable (..))

import           Text.Show (Show)


-- | The 'Status' type represents values with three possibilities: a value
--   of type @'Status' a b@ is one of @'Success'@ , @'Failure' x@ or
--   @'Continue' a@.
data Status x a =
    Success
  | Failure x
  | Continue a
    deriving (Eq, Show, Functor, Foldable, Traversable)

instance Applicative (Status x) where
  (<*>) ff fa =
    case fa of
      Success ->
        Success
      Failure x ->
        Failure x
      Continue a ->
        ($ a) <$> ff

  pure a =
    Continue a

instance Bifunctor Status where
  bimap f g ta =
    case ta of
      Success ->
        Success
      Failure x ->
        Failure $ f x
      Continue a ->
        Continue $ g a

instance Monad (Status x) where
  (>>=) ma f =
    case ma of
      Success ->
        Success
      Failure x ->
        Failure x
      Continue a ->
        f a
