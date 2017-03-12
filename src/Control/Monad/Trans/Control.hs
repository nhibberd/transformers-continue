{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- This monad transformer extends a monad with the ability to handle
-- multiple terminating cases.
--
-- A sequence of actions terminates normally, producing a value, only
-- if none of the actions in the sequence are 'Stop' or 'Failure'.  If
-- one action is 'Stop' or 'Failure', the rest of the sequence is
-- skipped and the composite action exits with that result.
--

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

import           Data.Bifunctor (Bifunctor (..))
import           Data.Either (Either (..))
import           Data.Foldable (Foldable (..))
import           Data.Function (($), (.), id)
import           Data.Functor (Functor (..), (<$>))
import           Data.Maybe (Maybe (..))
import           Data.Traversable (Traversable (..))

-- | A monad transfomer that extends the 'Control' monad.
--
-- Computations are successes, failures or normal values.
--
-- The 'return' function returns a normal value, while @>>=@ exits on
-- the first stop or failure.
newtype ControlT x m a =
  ControlT {
      runControlT :: m (Control x a)
    }

instance Functor m => Functor (ControlT x m) where
  fmap f fa =
    ControlT . (fmap . fmap) f $ runControlT fa

instance (Applicative m, Monad m) => Applicative (ControlT x m) where
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


instance Foldable m => Foldable (ControlT x m) where
  foldMap f ta =
    foldMap (foldMap f) (runControlT ta)

instance Traversable m => Traversable (ControlT x m) where
  traverse f ta =
    ControlT <$>
      traverse (traverse f) (runControlT ta)

instance Monad m => Monad (ControlT x m) where
  (>>=) ma f =
    ControlT $ do
      a <- runControlT ma
      case a of
        Stop ->
          pure $ Stop
        Failure x ->
          pure $ Failure x
        Success ax ->
          runControlT $ f ax

  return =
    ControlT . return . return

instance MonadIO m => MonadIO (ControlT x m) where
  liftIO =
    lift . liftIO

instance MonadTrans (ControlT x) where
  lift =
    ControlT . fmap Success


-- | Singal a stop.
--
-- * @'runControlT' 'stop' = 'return' 'Stop'@
stop :: Applicative m => ControlT x m a
stop =
  ControlT . pure $ Stop
{-# INLINE stop #-}

-- | Singal a failure value @x@.
--
-- * @'runControlT' ('failure' x) = 'return' ('Failure' x)@
failure :: Applicative m => x -> ControlT x m a
failure =
  ControlT . pure . Failure
{-# INLINE failure #-}

-- | Singal a success value @x@.
--
-- * @'runControlT' ('success' x) = 'return' ('Success' x)@
success :: Applicative m => a -> ControlT x m a
success =
  ControlT . pure . Success
{-# INLINE success #-}

-- | Lift an 'Control' into an 'ControlT'
hoistControl :: Monad m => Control x a -> ControlT x m a
hoistControl =
  ControlT . return
{-# INLINE hoistControl #-}

-- | Map the unwrapped computation using the given function.
--
-- @
-- 'runControlT' ('mapControlT' f m) = f ('runControlT' m)
-- @
mapControlT :: (m (Control x a) -> n (Control y b)) -> ControlT x m a -> ControlT y n b
mapControlT f =
  ControlT . f . runControlT
{-# INLINE mapControlT #-}

-- | Map over both failure and success.
bimapControlT :: Functor m => (x -> y) -> (a -> b) -> ControlT x m a -> ControlT y m b
bimapControlT f g =
   mapControlT (fmap (bimap f g))
{-# INLINE bimapControlT #-}

-- | Map over failure.
firstControlT :: Functor m => (x -> y) -> ControlT x m a -> ControlT y m a
firstControlT f =
  bimapControlT f id
{-# INLINE firstControlT #-}

-- | Map over success.
secondControlT :: Functor m => (a -> b) -> ControlT x m a -> ControlT x m b
secondControlT f =
  bimapControlT id f
{-# INLINE secondControlT #-}

-- | Map over failure.
mapFailure :: Functor m => (x -> y) -> ControlT x m a -> ControlT y m a
mapFailure =
  firstControlT
{-# INLINE mapFailure #-}

-- | Lift an 'Maybe' into an 'ControlT'
--
-- * @'runControlT' ('stopAtNothing' 'Nothing') = 'return' 'Stop'@
--
-- * @'runControlT' ('stopAtNothing' ('Just' a) = 'return' ('Success' a)@
stopAtNothing :: Applicative m => Maybe a -> ControlT x m a
stopAtNothing m =
  case m of
    Nothing ->
      stop
    Just a ->
      success a
{-# INLINE stopAtNothing #-}


------------------------------------------------------------------------
-- EitherT / ExceptT extensions

-- | Utility function for EitherT pattern synonym over 'ExceptT'
runToEitherT :: Monad m => ControlT x m () -> ExceptT x m ()
runToEitherT =
  runToExceptT
{-# INLINE runToEitherT #-}

-- | Convert an 'ControlT' into an 'ExceptT'
--
-- * @'runExceptT' ('runToExceptT' ('success' a)) = 'return' ('Right' a)@
--
-- * @'runExceptT' ('runToExceptT' ('failure' x)) = 'return' ('Left' x)@
--
-- * @'runExceptT' ('runToExceptT' 'stop') = 'return' ('Right' ())@
--
runToExceptT :: Monad m => ControlT x m () -> ExceptT x m ()
runToExceptT c = do
  r <- lift $ runControlT c
  case r of
    Stop ->
      pure ()
    Failure x ->
      Except.throwE x
    Success a ->
      ExceptT . pure $ pure a
{-# INLINE runToExceptT #-}

-- | Utility function for EitherT pattern synonym over 'ExceptT'
liftEitherT :: Monad m => ExceptT x m a -> ControlT x m a
liftEitherT =
  liftExceptT
{-# INLINE liftEitherT #-}

-- | Convert an 'ExceptT' into an 'ControlT'
--
-- * @'runExceptT' ('return' ('Left' x)) = 'failure' x@
--
-- * @'runExceptT' ('return' ('Right' a)) = 'success' a@
--
--
liftExceptT :: Monad m => ExceptT x m a -> ControlT x m a
liftExceptT e =
  ControlT $ do
    r <- Except.runExceptT e
    return $ case r of
      Left x ->
        Failure x
      Right a ->
        Success a
{-# INLINE liftExceptT #-}
