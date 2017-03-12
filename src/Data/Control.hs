{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Control type.
module Data.Control (
  -- * Control
    Control (..)
  ) where


import           Control.Applicative (Applicative (..))
import           Control.Monad (Monad (..))

import           Data.Bifunctor (Bifunctor (..))
import           Data.Eq (Eq)
import           Data.Monoid (mempty)
import           Data.Foldable (Foldable (..))
import           Data.Function (($),)
import           Data.Functor (Functor (..), (<$>))
import           Data.Traversable (Traversable (..))

import           Text.Show (Show)


-- | The 'Control' type represents values with three possibilities: a value
--   of type @'Control' a b@ is one of @'Stop'@ , @'Failure' x@ or
--   @'Success' a@.
data Control x a =
    Stop
  | Failure x
  | Success a
    deriving (Eq, Show)

instance Functor (Control x) where
  fmap f fa =
    case fa of
      Stop ->
        Stop
      Failure x ->
        Failure x
      Success a ->
        Success $ f a

instance Applicative (Control x) where
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

instance Bifunctor Control where
  bimap f g ta =
    case ta of
      Stop ->
        Stop
      Failure x ->
        Failure $ f x
      Success a ->
        Success $ g a

instance Foldable (Control x) where
  foldMap f ta =
    case ta of
      Stop ->
        mempty
      Failure _ ->
        mempty
      Success a ->
        f a

  foldr f b ta =
    case ta of
      Stop ->
        b
      Failure _ ->
        b
      Success a ->
        f a b

  length ta =
    case ta of
      Stop ->
        0
      Failure _ ->
        0
      Success _ ->
        1

instance Traversable (Control x) where
  traverse f ta =
    case ta of
      Stop ->
        pure Stop
      Failure x ->
        pure $ Failure x
      Success a ->
        Success <$> f a


instance Monad (Control x) where
  (>>=) ma f =
    case ma of
      Stop ->
        Stop
      Failure x ->
        Failure x
      Success a ->
        f a
