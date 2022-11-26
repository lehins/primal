{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Primal.Exception.Uninterruptible
-- Copyright   : (c) Alexey Kuleshevich 2020-2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module Primal.Exception.Uninterruptible (
  onException,
  -- TODO: Implement:
  -- , onAsyncException
  withException,
  withAnyException,
  -- TODO: Implement:
  -- , withAsyncException
  finally,
  bracket,
  bracket_,
  bracketOnError,
  mask,
  mask_,
  liftMask,
  liftMask_,
) where

import qualified Control.Exception as GHC
import Control.Monad
import Primal.Exception
import Primal.Exception.Unsafe
import Primal.Monad.Internal
import Primal.Monad.Unsafe

-- | Run an action, while invoking an exception handler when that action fails
-- for some reason. Exception handling function has async exceptions masked, but
-- it is still interruptible, which can be undesired in some scenarios. If you
-- are sure that the cleanup action does not deadlock and you do need hard
-- guarantees that it gets executed you can run it as uninterruptible:
--
-- > uninterruptibleMask $ \restore -> withException (restore action) handler
--
-- @since 1.0.0
withException
  :: (UnliftPrimal RW m, GHC.Exception e) => m a -> (e -> m b) -> m a
withException action handler =
  mask $ \restore -> do
    catch
      (restore action)
      (\exc -> catchAllSync (void $ handler exc) (\_ -> pure ()) >> raise exc)

-- | Same as `withException`, but will invoke exception handling function on all
-- synchronous exceptions.
--
-- @since 1.0.0
withAnyException :: UnliftPrimal RW m => m a -> (GHC.SomeException -> m b) -> m a
withAnyException thing after =
  mask $ \restore -> do
    catchAll
      (restore thing)
      (\exc -> catchAllSync (void $ after exc) (\_ -> pure ()) >> raise exc)

-- | Async safe version of 'GHC.onException'.
--
-- @since 1.0.0
onException :: UnliftPrimal RW m => m a -> m b -> m a
onException thing after = withAnyException thing (const after)

--
-- @since 1.0.0
bracket :: UnliftPrimal RW m => m a -> (a -> m b) -> (a -> m c) -> m c
bracket acquire cleanup action =
  mask $ \restore -> do
    resource <- acquire
    result <-
      catchAll (restore (action resource)) $ \exc -> do
        catchAllSync (void $ cleanup resource) $ \_ -> pure ()
        raise exc
    result <$ cleanup resource
{-# INLINEABLE bracket #-}

bracketOnError :: UnliftPrimal RW m => m a -> (a -> m b) -> (a -> m c) -> m c
bracketOnError acquire cleanup action =
  mask $ \restore -> do
    resource <- acquire
    catchAll (restore (action resource)) $ \exc -> do
      catchAllSync (void $ cleanup resource) $ \_ -> pure ()
      raise exc

finally :: UnliftPrimal RW m => m a -> m b -> m a
finally action cleanup =
  mask $ \restore -> do
    result <-
      catchAll (restore action) $ \exc -> do
        catchAllSync (void cleanup) $ \_ -> pure ()
        raise exc
    result <$ cleanup

--
-- @since 1.0.0
bracket_ :: UnliftPrimal RW m => m a -> m b -> m c -> m c
bracket_ acquire cleanup action = bracket acquire (const cleanup) (const action)

-- | Mask all async exceptions and make sure evaluation cannot be
-- interrupted. It is polymorphic in the state token because it is perfectly
-- safe to use with `ST` actions . It doesn't have to be restricted to
-- `RealWorld` because it has no impact on other threads and can't affect the
-- result of computation, moreover pure functions that implement tight loops
-- that don't perform any allocations are already non-interruptible. In fact
-- using this function is trickier in `IO` than it is in `ST`, because
-- misuse can lead to deadlocks in a concurrent setting.
--
-- @since 1.0.0
mask_ :: forall a m s. UnliftPrimal s m => m a -> m a
mask_ action =
  unsafeIOToPrimal getMaskingState >>= \case
    GHC.Unmasked -> blockUninterruptible action
    GHC.MaskedInterruptible -> blockUninterruptible action
    _ -> action
{-# INLINEABLE mask_ #-}

-- | Run an action with asychronous exceptions masked and mark it as
-- uninterruptible. In other words same as
-- `Control.Exception.uninterruptibleMask`, except that it is polymorphic in the
-- state token @s@. Inside a state thread it cannot affect the result of
-- computation, therefore it might come as a surprise, but it is safe to use it
-- within `ST` monad.
--
-- @since 1.0.0
mask
  :: forall a m s
   . UnliftPrimal s m
  => ((forall b. m b -> m b) -> m a)
  -> m a
mask action = do
  unsafeIOToPrimal getMaskingState >>= \case
    GHC.Unmasked -> blockUninterruptible $ action unblockAsyncExceptions
    GHC.MaskedInterruptible -> blockUninterruptible $ action blockAsyncExceptions
    GHC.MaskedUninterruptible -> action blockUninterruptible
{-# INLINEABLE mask #-}

-- | Similar to `mask_`, but allows a wider range of target monads
--
-- @since 1.0.0
liftMask_ :: forall a n m s. (PrimalState s n, Primal s m) => n a -> m a
liftMask_ action =
  unsafeIOToPrimal getMaskingState >>= \case
    GHC.Unmasked -> liftBlockUninterruptible action
    GHC.MaskedInterruptible -> liftBlockUninterruptible action
    _ -> liftP action
{-# INLINEABLE liftMask_ #-}

liftMask
  :: forall a n m s
   . (PrimalState s n, Primal s m)
  => ((forall b. n b -> n b) -> n a)
  -> m a
liftMask action = do
  unsafeIOToPrimal getMaskingState >>= \case
    GHC.Unmasked -> liftBlockUninterruptible $ action liftUnblockAsyncExceptions
    GHC.MaskedInterruptible -> liftBlockUninterruptible $ action liftBlockAsyncExceptions
    GHC.MaskedUninterruptible -> liftP (action blockUninterruptible)
{-# INLINEABLE liftMask #-}
