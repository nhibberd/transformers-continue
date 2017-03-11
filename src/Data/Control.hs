{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Control (
  -- * Control
    Control (..)
  ) where


import           Control.Applicative (Applicative (..))
import           Control.Monad (Monad (..))

import           Data.Bifunctor (Bifunctor (..))
import           Data.Monoid (mempty)
import           Data.Foldable (Foldable (..))
import           Data.Function (($),)
import           Data.Functor (Functor (..), (<$>))
import           Data.Traversable (Traversable (..))


data Control e a =
    Stop
  | Failure e
  | Success a

instance Functor (Control e) where
  fmap f fa =
    case fa of
      Stop ->
        Stop
      Failure e ->
        Failure e
      Success a ->
        Success $ f a

instance Applicative (Control e) where
  (<*>) ff fa =
    case fa of
      Stop ->
        Stop
      Failure e ->
        Failure e
      Success a ->
        ($ a) <$> ff

  pure a =
    Success a

instance Bifunctor Control where
  bimap f g ta =
    case ta of
      Stop ->
        Stop
      Failure e ->
        Failure $ f e
      Success a ->
        Success $ g a

instance Foldable (Control e) where
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

instance Traversable (Control e) where
  traverse f ta =
    case ta of
      Stop ->
        pure Stop
      Failure e ->
        pure $ Failure e
      Success a ->
        Success <$> f a


instance Monad (Control e) where
  (>>=) ma f =
    case ma of
      Stop ->
        Stop
      Failure e ->
        Failure e
      Success a ->
        f a
