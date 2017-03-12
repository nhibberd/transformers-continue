{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

-- |
-- This monad transformer extends a monad with the ability to handle
-- multiple terminating cases.
--
-- A sequence of actions terminates normally, producing a value, only
-- if none of the actions in the sequence are 'Success' or 'Failure'.  If
-- one action is 'Success' or 'Failure', the rest of the sequence is
-- skipped and the composite action exits with that result.
--

module Control.Monad.Trans.Status (
  -- * StatusT
    StatusT (..)
  , success
  , failure
  , continue
  , hoistStatus
  , mapStatusT
  , bimapStatusT
  , firstStatusT
  , secondStatusT
  , mapFailure
  , successFromNothing
  , hoistEither

  -- * EitherT / ExceptT extensions
  , liftEitherT
  , liftExceptT
  , runToEitherT
  , runToExceptT
  ) where

import           Data.Status (Status (..))

import           Control.Applicative (Applicative (..))
import           Control.Monad (Monad (..))
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Class (MonadTrans (..), lift)
import           Control.Monad.Trans.Except (ExceptT (..))
import qualified Control.Monad.Trans.Except as Except

import           Data.Bifunctor (Bifunctor (..))
import           Data.Either (Either (..))
import           Data.Foldable (Foldable (..))
import           Data.Function (($), (.), id)
import           Data.Functor (Functor (..), (<$>))
import           Data.Maybe (Maybe (..))
import           Data.Traversable (Traversable (..))

-- | A monad transfomer that extends the 'Status' monad.
--
-- Computations are successes, failures or normal values.
--
-- The 'return' function returns a normal value, while @>>=@ exits on
-- the first success or failure.
newtype StatusT x m a =
  StatusT {
      runStatusT :: m (Status x a)
    } deriving (Functor, Foldable, Traversable)

instance (Applicative m, Monad m) => Applicative (StatusT x m) where
  (<*>) f fa =
    StatusT $ do
      fab <- runStatusT f
      a <- runStatusT fa
      case a of
        Success ->
          pure Success
        Failure e ->
          pure $ Failure e
        Continue ax ->
          pure $ ($ ax) <$> fab

  pure a =
    StatusT . pure $ pure a

instance Monad m => Monad (StatusT x m) where
  (>>=) ma f =
    StatusT $ do
      a <- runStatusT ma
      case a of
        Success ->
          pure $ Success
        Failure x ->
          pure $ Failure x
        Continue ax ->
          runStatusT $ f ax

  return =
    StatusT . return . return

instance MonadIO m => MonadIO (StatusT x m) where
  liftIO =
    lift . liftIO

instance MonadTrans (StatusT x) where
  lift =
    StatusT . fmap Continue


-- | Singal a success.
--
-- * @'runStatusT' 'success' = 'return' 'Success'@
success :: Applicative m => StatusT x m a
success =
  StatusT . pure $ Success
{-# INLINE success #-}

-- | Singal a failure value @x@.
--
-- * @'runStatusT' ('failure' x) = 'return' ('Failure' x)@
failure :: Applicative m => x -> StatusT x m a
failure =
  StatusT . pure . Failure
{-# INLINE failure #-}

-- | Singal a continue value @x@.
--
-- * @'runStatusT' ('continue' x) = 'return' ('Continue' x)@
continue :: Applicative m => a -> StatusT x m a
continue =
  StatusT . pure . Continue
{-# INLINE continue #-}

-- | Lift an 'Status' into an 'StatusT'
hoistStatus :: Monad m => Status x a -> StatusT x m a
hoistStatus =
  StatusT . return
{-# INLINE hoistStatus #-}

-- | Map the unwrapped computation using the given function.
--
-- @
-- 'runStatusT' ('mapStatusT' f m) = f ('runStatusT' m)
-- @
mapStatusT :: (m (Status x a) -> n (Status y b)) -> StatusT x m a -> StatusT y n b
mapStatusT f =
  StatusT . f . runStatusT
{-# INLINE mapStatusT #-}

-- | Map over both failure and continue.
bimapStatusT :: Functor m => (x -> y) -> (a -> b) -> StatusT x m a -> StatusT y m b
bimapStatusT f g =
   mapStatusT (fmap (bimap f g))
{-# INLINE bimapStatusT #-}

-- | Map over failure.
firstStatusT :: Functor m => (x -> y) -> StatusT x m a -> StatusT y m a
firstStatusT f =
  bimapStatusT f id
{-# INLINE firstStatusT #-}

-- | Map over continue.
secondStatusT :: Functor m => (a -> b) -> StatusT x m a -> StatusT x m b
secondStatusT f =
  bimapStatusT id f
{-# INLINE secondStatusT #-}

-- | Map over failure.
mapFailure :: Functor m => (x -> y) -> StatusT x m a -> StatusT y m a
mapFailure =
  firstStatusT
{-# INLINE mapFailure #-}

-- | Lift an 'Maybe' into an 'StatusT'
--
-- * @'runStatusT' ('successFromNothing' 'Nothing') = 'return' 'Success'@
--
-- * @'runStatusT' ('successFromNothing' ('Just' a)) = 'return' ('Continue' a)@
successFromNothing :: Applicative m => Maybe a -> StatusT x m a
successFromNothing m =
  case m of
    Nothing ->
      success
    Just a ->
      continue a
{-# INLINE successFromNothing #-}

-- | Lift an 'Either' into an 'StatusT'
--
-- * @'runStatusT' ('hoistEither' ('Left' x)) = 'return' ('Failure' x)@
--
-- * @'runStatusT' ('hoistEither' ('Right' a)) = 'return' ('Continue' a)@
hoistEither :: Applicative m => Either x a -> StatusT x m a
hoistEither e =
  case e of
    Left x ->
      failure x
    Right a ->
      continue a
{-# INLINE hoistEither #-}


------------------------------------------------------------------------
-- EitherT / ExceptT extensions

-- | Utility function for EitherT pattern synonym over 'ExceptT'
runToEitherT :: Monad m => StatusT x m () -> ExceptT x m ()
runToEitherT =
  runToExceptT
{-# INLINE runToEitherT #-}

-- | Convert an 'StatusT' into an 'ExceptT'
--
-- * @'runExceptT' ('runToExceptT' ('continue' a)) = 'return' ('Right' a)@
--
-- * @'runExceptT' ('runToExceptT' ('failure' x)) = 'return' ('Left' x)@
--
-- * @'runExceptT' ('runToExceptT' 'success') = 'return' ('Right' ())@
--
runToExceptT :: Monad m => StatusT x m () -> ExceptT x m ()
runToExceptT c = do
  r <- lift $ runStatusT c
  case r of
    Success ->
      pure ()
    Failure x ->
      Except.throwE x
    Continue a ->
      ExceptT . pure $ pure a
{-# INLINE runToExceptT #-}

-- | Utility function for EitherT pattern synonym over 'ExceptT'
liftEitherT :: Monad m => ExceptT x m a -> StatusT x m a
liftEitherT =
  liftExceptT
{-# INLINE liftEitherT #-}

-- | Convert an 'ExceptT' into an 'StatusT'
--
-- * @'runExceptT' ('return' ('Left' x)) = 'failure' x@
--
-- * @'runExceptT' ('return' ('Right' a)) = 'continue' a@
--
--
liftExceptT :: Monad m => ExceptT x m a -> StatusT x m a
liftExceptT e =
  StatusT $ do
    r <- Except.runExceptT e
    return $ case r of
      Left x ->
        Failure x
      Right a ->
        Continue a
{-# INLINE liftExceptT #-}
