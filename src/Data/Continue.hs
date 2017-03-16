{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

-- | Continue type.
module Data.Continue (
  -- * Continue
    Continue (..)
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


-- | The 'Continue' type represents values with three possibilities: a value
--   of type @'Continue' a b@ is one of @'Stop'@ , @'Failure' x@ or
--   @'Continue' a@.
data Continue x a =
    Stop
  | Failure x
  | Continue a
    deriving (Eq, Show, Functor, Foldable, Traversable)

instance Applicative (Continue x) where
  (<*>) ff fa =
    case fa of
      Stop ->
        Stop
      Failure x ->
        Failure x
      Continue a ->
        ($ a) <$> ff

  pure a =
    Continue a

instance Bifunctor Continue where
  bimap f g ta =
    case ta of
      Stop ->
        Stop
      Failure x ->
        Failure $ f x
      Continue a ->
        Continue $ g a

instance Monad (Continue x) where
  (>>=) ma f =
    case ma of
      Stop ->
        Stop
      Failure x ->
        Failure x
      Continue a ->
        f a
