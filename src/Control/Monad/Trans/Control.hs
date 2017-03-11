{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.Trans.Control (
  -- * ControlT
    ControlT (..)
  , stop
  , failure
  , success
  , hoistControl
  , mapControlT
  , bimapControlT
  , firstControlT
  , secondControlT
  , mapFailure
  , stopAtNothing

  -- * EitherT / ExceptT extensions
  , liftEitherT
  , liftExceptT
  , runToEitherT
  , runToExceptT
  ) where

import           Data.Control (Control (..))

import           Control.Applicative (Applicative (..))
import           Control.Monad (Monad (..))
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Class (MonadTrans (..), lift)
import           Control.Monad.Trans.Except (ExceptT (..))
import qualified Control.Monad.Trans.Except as Except

import           Data.Maybe (Maybe (..))
import           Data.Either (Either (..))
import           Data.Function (($), (.), id)
import           Data.Functor (Functor (..), (<$>))

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
        Failure e ->
          pure $ Failure e
        Success ax ->
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
        Failure e ->
          pure $ Failure e
        Success ax ->
          runControlT $ f ax

  return =
    ControlT . return . return

instance MonadIO m => MonadIO (ControlT e m) where
  liftIO =
    lift . liftIO

instance MonadTrans (ControlT e) where
  lift =
    ControlT . fmap Success


-- THINGS

stop :: Applicative m => ControlT e m a
stop =
  ControlT . pure $ Stop
{-# INLINE stop #-}

failure :: Applicative m => e -> ControlT e m a
failure =
  ControlT . pure . Failure
{-# INLINE failure #-}

success :: Applicative m => a -> ControlT e m a
success =
  ControlT . pure . Success
{-# INLINE success #-}


hoistControl :: Monad m => Control x a -> ControlT x m a
hoistControl =
  ControlT . return
{-# INLINE hoistControl #-}

mapControlT :: (m (Control x a) -> n (Control y b)) -> ControlT x m a -> ControlT y n b
mapControlT f =
  ControlT . f . runControlT
{-# INLINE mapControlT #-}

bimapControlT :: Functor m => (x -> y) -> (a -> b) -> ControlT x m a -> ControlT y m b
bimapControlT f g =
  let
    h c =
      case c of
        Stop ->
          Stop
        Failure x ->
          Failure (f x)
        Success a ->
          Success (g a)
  in
    mapControlT (fmap h)
{-# INLINE bimapControlT #-}

firstControlT :: Functor m => (x -> y) -> ControlT x m a -> ControlT y m a
firstControlT f =
  bimapControlT f id
{-# INLINE firstControlT #-}

secondControlT :: Functor m => (a -> b) -> ControlT x m a -> ControlT x m b
secondControlT f =
  bimapControlT id f
{-# INLINE secondControlT #-}

mapFailure :: Functor m => (x -> y) -> ControlT x m a -> ControlT y m a
mapFailure =
  firstControlT
{-# INLINE mapFailure #-}

stopAtNothing :: Applicative m => Maybe a -> ControlT x m a
stopAtNothing m =
  case m of
    Nothing ->
      stop
    Just a ->
      success a
{-# INLINE stopAtNothing #-}


-- EitherT / ExceptT extensions

runToEitherT :: Monad m => ControlT e m () -> ExceptT e m ()
runToEitherT =
  runToExceptT
{-# INLINE runToEitherT #-}

runToExceptT :: Monad m => ControlT e m () -> ExceptT e m ()
runToExceptT c = do
  r <- lift $ runControlT c
  case r of
    Stop ->
      pure ()
    Failure e ->
      Except.throwE e
    Success a ->
      ExceptT . pure $ pure a
{-# INLINE runToExceptT #-}


liftEitherT :: Monad m => ExceptT e m a -> ControlT e m a
liftEitherT =
  liftExceptT
{-# INLINE liftEitherT #-}

liftExceptT :: Monad m => ExceptT e m a -> ControlT e m a
liftExceptT e =
  ControlT $ do
    r <- Except.runExceptT e
    return $ case r of
      Left er ->
        Failure er
      Right a ->
        Success a
{-# INLINE liftExceptT #-}
