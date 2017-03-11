{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Control (
  -- * Control
    Control (..)
  ) where


import           Control.Applicative (Applicative (..))
import           Control.Monad (Monad (..))

import           Data.Function (($),)
import           Data.Functor (Functor (..), (<$>))


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

instance Monad (Control e) where
  (>>=) ma f =
    case ma of
      Stop ->
        Stop
      Failure e ->
        Failure e
      Success a ->
        f a
