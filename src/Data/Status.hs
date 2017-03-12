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
--   of type @'Status' a b@ is one of @'Stop'@ , @'Failure' x@ or
--   @'Success' a@.
data Status x a =
    Stop
  | Failure x
  | Success a
    deriving (Eq, Show, Functor, Foldable, Traversable)

instance Applicative (Status x) where
  (<*>) ff fa =
    case fa of
      Stop ->
        Stop
      Failure x ->
        Failure x
      Success a ->
        ($ a) <$> ff

  pure a =
    Success a

instance Bifunctor Status where
  bimap f g ta =
    case ta of
      Stop ->
        Stop
      Failure x ->
        Failure $ f x
      Success a ->
        Success $ g a

instance Monad (Status x) where
  (>>=) ma f =
    case ma of
      Stop ->
        Stop
      Failure x ->
        Failure x
      Success a ->
        f a
