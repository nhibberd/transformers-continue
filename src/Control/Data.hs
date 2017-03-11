{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Control.Data (
    Control (..)
  , ControlT (..)
  ) where


import           Control.Applicative (Applicative (..))
import           Control.Monad (Monad (..))
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Class (MonadTrans (..))

import           Data.Function (($), (.))
import           Data.Functor (Functor (..), (<$>))

data Control e a =
    Stop
  | Fail e
  | Pure a

instance Functor (Control e) where
  fmap f fa =
    case fa of
      Stop ->
        Stop
      Fail e ->
        Fail e
      Pure a ->
        Pure $ f a

instance Applicative (Control e) where
  (<*>) ff fa =
    case fa of
      Stop ->
        Stop
      Fail e ->
        Fail e
      Pure a ->
        ($ a) <$> ff

  pure a =
    Pure a

instance Monad (Control e) where
  (>>=) ma f =
    case ma of
      Stop ->
        Stop
      Fail e ->
        Fail e
      Pure a ->
        f a

newtype ControlT e m a =
  ControlT {
      runControlT :: m (Control e a)
    }

instance Functor m => Functor (ControlT e m) where
  fmap f fa =
    ControlT . (fmap . fmap) f $ runControlT fa

instance (Applicative m, Monad m) => Applicative (ControlT e m) where
  (<*>) f fa =
    ControlT $ do
      fab <- runControlT f
      a <- runControlT fa
      case a of
        Stop ->
          pure Stop
        Fail e ->
          pure $ Fail e
        Pure ax ->
          pure $ ($ ax) <$> fab

  pure a =
    ControlT . pure $ pure a

instance Monad m => Monad (ControlT e m) where
  (>>=) ma f =
    ControlT $ do
      a <- runControlT ma
      case a of
        Stop ->
          pure $ Stop
        Fail e ->
          pure $ Fail e
        Pure ax ->
          runControlT $ f ax

  return =
    ControlT . return . return

instance MonadIO m => MonadIO (ControlT e m) where
  liftIO =
    ControlT . fmap Pure . liftIO

instance MonadTrans (ControlT e) where
  lift =
    ControlT . fmap Pure
