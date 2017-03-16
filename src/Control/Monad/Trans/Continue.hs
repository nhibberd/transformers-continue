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
-- if none of the actions in the sequence are 'Stop' or 'Failure'.  If
-- one action is 'Stop' or 'Failure', the rest of the sequence is
-- skipped and the composite action exits with that result.
--

module Control.Monad.Trans.Continue (
  -- * ContinueT
    ContinueT (..)
  , stop
  , failure
  , continue
  , hoistContinue
  , mapContinueT
  , bimapContinueT
  , firstContinueT
  , secondContinueT
  , mapFailure
  , stopFromNothing
  , hoistEither

  -- * EitherT / ExceptT extensions
  , liftEitherT
  , liftExceptT
  , runToEitherT
  , runToExceptT
  ) where

import           Data.Continue (Continue (..))

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

-- | A monad transfomer that extends the 'Continue' monad.
--
-- Computations are stopes, failures or normal values.
--
-- The 'return' function returns a normal value, while @>>=@ exits on
-- the first stop or failure.
newtype ContinueT x m a =
  ContinueT {
      runContinueT :: m (Continue x a)
    } deriving (Functor, Foldable, Traversable)

instance (Applicative m, Monad m) => Applicative (ContinueT x m) where
  (<*>) f fa =
    ContinueT $ do
      fab <- runContinueT f
      a <- runContinueT fa
      case a of
        Stop ->
          pure Stop
        Failure e ->
          pure $ Failure e
        Continue ax ->
          pure $ ($ ax) <$> fab

  pure a =
    ContinueT . pure $ pure a

instance Monad m => Monad (ContinueT x m) where
  (>>=) ma f =
    ContinueT $ do
      a <- runContinueT ma
      case a of
        Stop ->
          pure $ Stop
        Failure x ->
          pure $ Failure x
        Continue ax ->
          runContinueT $ f ax

  return =
    ContinueT . return . return

instance MonadIO m => MonadIO (ContinueT x m) where
  liftIO =
    lift . liftIO

instance MonadTrans (ContinueT x) where
  lift =
    ContinueT . fmap Continue


-- | Singal a stop.
--
-- * @'runContinueT' 'stop' = 'return' 'Stop'@
stop :: Applicative m => ContinueT x m a
stop =
  ContinueT . pure $ Stop
{-# INLINE stop #-}

-- | Singal a failure value @x@.
--
-- * @'runContinueT' ('failure' x) = 'return' ('Failure' x)@
failure :: Applicative m => x -> ContinueT x m a
failure =
  ContinueT . pure . Failure
{-# INLINE failure #-}

-- | Singal a continue value @x@.
--
-- * @'runContinueT' ('continue' x) = 'return' ('Continue' x)@
continue :: Applicative m => a -> ContinueT x m a
continue =
  ContinueT . pure . Continue
{-# INLINE continue #-}

-- | Lift an 'Continue' into an 'ContinueT'
hoistContinue :: Monad m => Continue x a -> ContinueT x m a
hoistContinue =
  ContinueT . return
{-# INLINE hoistContinue #-}

-- | Map the unwrapped computation using the given function.
--
-- @
-- 'runContinueT' ('mapContinueT' f m) = f ('runContinueT' m)
-- @
mapContinueT :: (m (Continue x a) -> n (Continue y b)) -> ContinueT x m a -> ContinueT y n b
mapContinueT f =
  ContinueT . f . runContinueT
{-# INLINE mapContinueT #-}

-- | Map over both failure and continue.
bimapContinueT :: Functor m => (x -> y) -> (a -> b) -> ContinueT x m a -> ContinueT y m b
bimapContinueT f g =
   mapContinueT (fmap (bimap f g))
{-# INLINE bimapContinueT #-}

-- | Map over failure.
firstContinueT :: Functor m => (x -> y) -> ContinueT x m a -> ContinueT y m a
firstContinueT f =
  bimapContinueT f id
{-# INLINE firstContinueT #-}

-- | Map over continue.
secondContinueT :: Functor m => (a -> b) -> ContinueT x m a -> ContinueT x m b
secondContinueT f =
  bimapContinueT id f
{-# INLINE secondContinueT #-}

-- | Map over failure.
mapFailure :: Functor m => (x -> y) -> ContinueT x m a -> ContinueT y m a
mapFailure =
  firstContinueT
{-# INLINE mapFailure #-}

-- | Lift an 'Maybe' into an 'ContinueT'
--
-- * @'runContinueT' ('stopFromNothing' 'Nothing') = 'return' 'Stop'@
--
-- * @'runContinueT' ('stopFromNothing' ('Just' a)) = 'return' ('Continue' a)@
stopFromNothing :: Applicative m => Maybe a -> ContinueT x m a
stopFromNothing m =
  case m of
    Nothing ->
      stop
    Just a ->
      continue a
{-# INLINE stopFromNothing #-}

-- | Lift an 'Either' into an 'ContinueT'
--
-- * @'runContinueT' ('hoistEither' ('Left' x)) = 'return' ('Failure' x)@
--
-- * @'runContinueT' ('hoistEither' ('Right' a)) = 'return' ('Continue' a)@
hoistEither :: Applicative m => Either x a -> ContinueT x m a
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
runToEitherT :: Monad m => ContinueT x m () -> ExceptT x m ()
runToEitherT =
  runToExceptT
{-# INLINE runToEitherT #-}

-- | Convert an 'ContinueT' into an 'ExceptT'
--
-- * @'runExceptT' ('runToExceptT' ('continue' a)) = 'return' ('Right' a)@
--
-- * @'runExceptT' ('runToExceptT' ('failure' x)) = 'return' ('Left' x)@
--
-- * @'runExceptT' ('runToExceptT' 'stop') = 'return' ('Right' ())@
--
runToExceptT :: Monad m => ContinueT x m () -> ExceptT x m ()
runToExceptT c = do
  r <- lift $ runContinueT c
  case r of
    Stop ->
      pure ()
    Failure x ->
      Except.throwE x
    Continue a ->
      ExceptT . pure $ pure a
{-# INLINE runToExceptT #-}

-- | Utility function for EitherT pattern synonym over 'ExceptT'
liftEitherT :: Monad m => ExceptT x m a -> ContinueT x m a
liftEitherT =
  liftExceptT
{-# INLINE liftEitherT #-}

-- | Convert an 'ExceptT' into an 'ContinueT'
--
-- * @'runExceptT' ('return' ('Left' x)) = 'failure' x@
--
-- * @'runExceptT' ('return' ('Right' a)) = 'continue' a@
--
--
liftExceptT :: Monad m => ExceptT x m a -> ContinueT x m a
liftExceptT e =
  ContinueT $ do
    r <- Except.runExceptT e
    return $ case r of
      Left x ->
        Failure x
      Right a ->
        Continue a
{-# INLINE liftExceptT #-}
