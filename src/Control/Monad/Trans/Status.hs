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

module Control.Monad.Trans.Status (
  -- * StatusT
    StatusT (..)
  , stop
  , failure
  , success
  , hoistStatus
  , mapStatusT
  , bimapStatusT
  , firstStatusT
  , secondStatusT
  , mapFailure
  , stopAtNothing

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
-- the first stop or failure.
newtype StatusT x m a =
  StatusT {
      runStatusT :: m (Status x a)
    }

instance Functor m => Functor (StatusT x m) where
  fmap f fa =
    StatusT . (fmap . fmap) f $ runStatusT fa

instance (Applicative m, Monad m) => Applicative (StatusT x m) where
  (<*>) f fa =
    StatusT $ do
      fab <- runStatusT f
      a <- runStatusT fa
      case a of
        Stop ->
          pure Stop
        Failure e ->
          pure $ Failure e
        Success ax ->
          pure $ ($ ax) <$> fab

  pure a =
    StatusT . pure $ pure a


instance Foldable m => Foldable (StatusT x m) where
  foldMap f ta =
    foldMap (foldMap f) (runStatusT ta)

instance Traversable m => Traversable (StatusT x m) where
  traverse f ta =
    StatusT <$>
      traverse (traverse f) (runStatusT ta)

instance Monad m => Monad (StatusT x m) where
  (>>=) ma f =
    StatusT $ do
      a <- runStatusT ma
      case a of
        Stop ->
          pure $ Stop
        Failure x ->
          pure $ Failure x
        Success ax ->
          runStatusT $ f ax

  return =
    StatusT . return . return

instance MonadIO m => MonadIO (StatusT x m) where
  liftIO =
    lift . liftIO

instance MonadTrans (StatusT x) where
  lift =
    StatusT . fmap Success


-- | Singal a stop.
--
-- * @'runStatusT' 'stop' = 'return' 'Stop'@
stop :: Applicative m => StatusT x m a
stop =
  StatusT . pure $ Stop
{-# INLINE stop #-}

-- | Singal a failure value @x@.
--
-- * @'runStatusT' ('failure' x) = 'return' ('Failure' x)@
failure :: Applicative m => x -> StatusT x m a
failure =
  StatusT . pure . Failure
{-# INLINE failure #-}

-- | Singal a success value @x@.
--
-- * @'runStatusT' ('success' x) = 'return' ('Success' x)@
success :: Applicative m => a -> StatusT x m a
success =
  StatusT . pure . Success
{-# INLINE success #-}

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

-- | Map over both failure and success.
bimapStatusT :: Functor m => (x -> y) -> (a -> b) -> StatusT x m a -> StatusT y m b
bimapStatusT f g =
   mapStatusT (fmap (bimap f g))
{-# INLINE bimapStatusT #-}

-- | Map over failure.
firstStatusT :: Functor m => (x -> y) -> StatusT x m a -> StatusT y m a
firstStatusT f =
  bimapStatusT f id
{-# INLINE firstStatusT #-}

-- | Map over success.
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
-- * @'runStatusT' ('stopAtNothing' 'Nothing') = 'return' 'Stop'@
--
-- * @'runStatusT' ('stopAtNothing' ('Just' a) = 'return' ('Success' a)@
stopAtNothing :: Applicative m => Maybe a -> StatusT x m a
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
runToEitherT :: Monad m => StatusT x m () -> ExceptT x m ()
runToEitherT =
  runToExceptT
{-# INLINE runToEitherT #-}

-- | Convert an 'StatusT' into an 'ExceptT'
--
-- * @'runExceptT' ('runToExceptT' ('success' a)) = 'return' ('Right' a)@
--
-- * @'runExceptT' ('runToExceptT' ('failure' x)) = 'return' ('Left' x)@
--
-- * @'runExceptT' ('runToExceptT' 'stop') = 'return' ('Right' ())@
--
runToExceptT :: Monad m => StatusT x m () -> ExceptT x m ()
runToExceptT c = do
  r <- lift $ runStatusT c
  case r of
    Stop ->
      pure ()
    Failure x ->
      Except.throwE x
    Success a ->
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
-- * @'runExceptT' ('return' ('Right' a)) = 'success' a@
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
        Success a
{-# INLINE liftExceptT #-}
