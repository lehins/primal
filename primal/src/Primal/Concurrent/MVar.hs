{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Primal.Concurrent.MVar
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Concurrent.MVar
  ( -- * MVar
    MVar(..)
  , isEmptyMVar
  , isSameMVar
    -- ** Create
  , newMVar
  , newLazyMVar
  , newDeepMVar
  , newEmptyMVar
    -- ** Write
  , putMVar
  , putLazyMVar
  , putDeepMVar
  , tryPutMVar
  , tryPutLazyMVar
  , tryPutDeepMVar
  , writeMVar
    -- ** Read
  , readMVar
  , tryReadMVar
  , takeMVar
  , tryTakeMVar
  , clearMVar
  -- ** Modify
  , swapMVar
  , swapLazyMVar
  , swapDeepMVar
  , withMVar
  , withMVarMasked
  , modifyMVar_
  , modifyMVarMasked_
  , modifyFetchOldMVar
  , modifyFetchOldMVarMasked
  , modifyFetchNewMVar
  , modifyFetchNewMVarMasked
  , modifyMVar
  , modifyMVarMasked
  -- ** Weak Pointer
  , mkWeakMVar
  -- ** Conversion
  , toBaseMVar
  , fromBaseMVar
  ) where

import Control.DeepSeq
import Primal.Monad
import Primal.Exception
import qualified Primal.Exception.Interruptible as EI
import GHC.Exts
import GHC.Weak
import qualified GHC.MVar as GHC

-- | Mutable variable that can either be empty or full. Same as
-- `Control.Concurrent.MVar.MVar`, but works with any state token therefore it is also
-- usable within `ST` monad.
--
-- @since 0.3.0
data MVar a s = MVar (MVar# s a)

-- | Calls `isSameMVar`
instance Eq (MVar a s) where
  (==) = isSameMVar
  {-# INLINE (==) #-}


-- | Checks whether supplied `MVar`s refer to the exact same one.
--
-- @since 0.3.0
isSameMVar :: forall a s. MVar a s -> MVar a s -> Bool
isSameMVar (MVar mvar1#) (MVar mvar2#) = isTrue# (sameMVar# mvar1# mvar2#)
{-# INLINE isSameMVar #-}

-- | Checks whether supplied `MVar` is empty.
--
-- @since 0.3.0
isEmptyMVar :: forall a m s. Primal s m => MVar a s -> m Bool
isEmptyMVar (MVar mvar#) =
  primal $ \s ->
    case isEmptyMVar# mvar# s of
      (# s', isEmpty# #) -> (# s', isTrue# isEmpty# #)
{-# INLINE isEmptyMVar #-}


-- | Construct an `MVar` with initial value in it, which is evaluated to WHNF
--
-- @since 0.3.0
newMVar :: forall a m s. Primal s m => a -> m (MVar a s)
newMVar a = newEmptyMVar >>= \mvar -> mvar <$ putMVar mvar a
{-# INLINE newMVar #-}

-- | Construct an `MVar` with initial value in it.
--
-- Same as `Control.Concurrent.MVar.newMVar` from @base@, but works in any `Primal`.
--
-- @since 0.3.0
newLazyMVar :: forall a m s. Primal s m => a -> m (MVar a s)
newLazyMVar a = newEmptyMVar >>= \mvar -> mvar <$ putLazyMVar mvar a
{-# INLINE newLazyMVar #-}


-- | Construct an `MVar` with initial value in it.
--
-- @since 0.3.0
newDeepMVar :: forall a m s. (NFData a, Primal s m) => a -> m (MVar a s)
newDeepMVar a = newEmptyMVar >>= \mvar -> mvar <$ putDeepMVar mvar a
{-# INLINE newDeepMVar #-}


-- | Construct an empty `MVar`.
--
-- Same as `Control.Concurrent.MVar.newEmptyMVar` from @base@, but works in any `Primal`.
--
-- @since 0.3.0
newEmptyMVar :: forall a m s. Primal s m => m (MVar a s)
newEmptyMVar =
  primal $ \s ->
    case newMVar# s of
      (# s', mvar# #) -> (# s', MVar mvar# #)
{-# INLINE newEmptyMVar #-}


-- | Write a value into an `MVar`. Blocks the current thread if `MVar` is empty and waits
-- until it gets filled by another thread. Evaluates the argument to WHNF prior to writing
-- it.
--
-- @since 0.3.0
putMVar :: forall a m s. Primal s m => MVar a s -> a -> m ()
putMVar mvar x = putLazyMVar (x `seq` mvar) x
{-# INLINE putMVar #-}


-- | Same as `putMVar`, but allows to write a thunk into an MVar.
--
-- Same as `Control.Concurrent.MVar.putMVar` from @base@, but works in any `Primal`.
--
-- @since 0.3.0
putLazyMVar :: forall a m s. Primal s m => MVar a s -> a -> m ()
putLazyMVar (MVar mvar#) x = primal_ (putMVar# mvar# x)
{-# INLINE putLazyMVar #-}


-- | Same as putMVar, but evaluates the argument to NF prior to writing it.
--
-- @since 0.3.0
putDeepMVar :: forall a m s. (NFData a, Primal s m) => MVar a s -> a -> m ()
putDeepMVar mvar x = putLazyMVar (x `deepseq` mvar) x
{-# INLINE putDeepMVar #-}


-- | Attempt to write a value into `MVar`. Unlike `putMVar` this function never blocks. It
-- also returns `True` if `MVar` was empty and placing the value in it turned out to be
-- successfull and `False` otherwise. Evaluates the supplied argumetn to WHNF prior to
-- attempting a write operation.
--
-- @since 0.3.0
tryPutMVar :: forall a m s. Primal s m => MVar a s -> a -> m Bool
tryPutMVar mvar x = tryPutLazyMVar (x `seq` mvar) x
{-# INLINE tryPutMVar #-}

-- | Same as `tryPutMVar`, but allows to put thunks into an `MVar`
--
-- Same as `Control.Concurrent.MVar.tryPutMVar` from @base@, but works in any `Primal`.
--
-- @since 0.3.0
tryPutLazyMVar :: forall a m s. Primal s m => MVar a s -> a -> m Bool
tryPutLazyMVar (MVar mvar#) x =
  primal $ \s ->
    case tryPutMVar# mvar# x s of
      (# s', b# #) -> (# s', isTrue# b# #)
{-# INLINE tryPutLazyMVar #-}


-- | Same as `tryPutMVar`, but evaluates the argument to NF prior to attempting to write
-- into the `MVar`
--
-- @since 0.3.0
tryPutDeepMVar :: forall a m s. (NFData a, Primal s m) => MVar a s -> a -> m Bool
tryPutDeepMVar mvar x = tryPutLazyMVar mvar $! force x
{-# INLINE tryPutDeepMVar #-}


-- | Write a value into the MVar regardless if it is currently empty or not. If there is a
-- currently a value it will in the MVar it will simply b discarded. However, if there is
-- another thread that is blocked on attempt to write into this MVar, current operation
-- will block on attempt to fill the MVar. Therefore `writeMVar` is not atomic. Argument
-- is evaluated to WHNF prior to clearing the contents of `MVar`
--
-- @since 0.3.0
writeMVar :: forall a m s. Primal s m => MVar a s -> a -> m ()
writeMVar mvar a =
  EI.liftMask_ $ do
    clearMVar (a `seq` mvar)
    putLazyMVar mvar a :: ST s ()
{-# INLINE writeMVar #-}


-- | Replace current value in an `MVar` with a new one. Supplied value is evaluated to
-- WHNF prior to current value being extracted from the `MVar`. If `MVar` is currently
-- empty this operation will block the current thread until it gets filled in another
-- thread. Furthermore it is possible for another thread to fill the `MVar` after the old
-- value is extracted, but before the new one has a chance to placed inside the `MVar`,
-- thus blocking current thread once more until another thread empties this `MVar`. In
-- other words this operation is not atomic.
--
-- @since 0.3.0
swapMVar :: forall a m s. Primal s m => MVar a s -> a -> m a
swapMVar mvar new =
  EI.liftMask_ $ do
    old <- takeMVar (new `seq` mvar)
    old <$ (putLazyMVar mvar new :: ST s ())
{-# INLINE swapMVar #-}

-- | Same as `swapMVar`, but allows writing thunks into the `MVar`.
--
-- Same as `Control.Concurrent.MVar.swapMVar` from @base@, but works in any `UnliftPrimal`.
--
-- @since 0.3.0
swapLazyMVar :: forall a m s. Primal s m => MVar a s -> a -> m a
swapLazyMVar mvar new =
  EI.liftMask_ $ do
    old <- takeMVar mvar
    old <$ (putLazyMVar mvar new :: ST s ())
{-# INLINE swapLazyMVar #-}


-- | Same as `swapMVar`, but evaluates the argument value to NF.
--
-- @since 0.3.0
swapDeepMVar :: forall a m s. (NFData a, Primal s m) => MVar a s -> a -> m a
swapDeepMVar mvar new =
  EI.liftMask_ $ do
    old <- takeMVar (new `deepseq` mvar)
    old <$ (putLazyMVar mvar new :: ST s ())
{-# INLINE swapDeepMVar #-}


-- | Remove the value from `MVar` and return it. Blocks the cuurent thread if `MVar` is empty and
-- waits until antoher thread fills it.
--
-- Same as `Control.Concurrent.MVar.takeMVar` from @base@, but works in any `Primal`.
--
-- @since 0.3.0
takeMVar :: forall a m s. Primal s m => MVar a s -> m a
takeMVar (MVar mvar#) = primal $ \ s# -> takeMVar# mvar# s#
{-# INLINE takeMVar #-}



-- | Remove the value from `MVar` and return it immediately without blocking. `Nothing` is
-- returned if `MVar` was empty.
--
-- Same as `Control.Concurrent.MVar.tryTakeMVar` from @base@, but works in any `Primal`.
--
-- @since 0.3.0
tryTakeMVar :: forall a m s. Primal s m => MVar a s -> m (Maybe a)
tryTakeMVar (MVar mvar#) =
  primal $ \s ->
    case tryTakeMVar# mvar# s of
      (# s', 0#, _ #) -> (# s', Nothing #)
      (# s', _, a #)  -> (# s', Just a #)
{-# INLINE tryTakeMVar #-}

-- | Get the value from `MVar` atomically without affecting its contents. Blocks the
-- current thread if the `MVar` is currently empty and waits until another thread fills
-- it with a value.
--
-- Same as `Control.Concurrent.MVar.readMVar` from @base@, but works in any `Primal`.
--
-- @since 0.3.0
readMVar :: forall a m s. Primal s m => MVar a s -> m a
readMVar (MVar mvar#) = primal (readMVar# mvar#)
{-# INLINE readMVar #-}


-- | Get the value from `MVar` atomically without affecting its contents. It does not
-- block and returns the immediately or `Nothing` if the supplied `MVar` was empty.
--
-- Same as `Control.Concurrent.MVar.tryReadMVar` from @base@, but works in any `Primal`.
--
-- @since 0.3.0
tryReadMVar :: forall a m s. Primal s m => MVar a s -> m (Maybe a)
tryReadMVar (MVar mvar#) =
  primal $ \s ->
    case tryReadMVar# mvar# s of
      (# s', 0#, _ #) -> (# s', Nothing #)
      (# s', _, a #) -> (# s', Just a #)
{-# INLINE tryReadMVar #-}

-- | Remove a value from an `MVar`, unless it was already empty. It effectively empties
-- the `MVar` however note that by the time this action returns there is a possibility
-- that another thread might have filled it with a different value.
--
-- @since 0.3.0
clearMVar :: forall a m s. Primal s m => MVar a s -> m ()
clearMVar (MVar mvar#) =
  primal $ \s ->
    case tryTakeMVar# mvar# s of
      (# s', _, _ #) -> (# s', () #)
{-# INLINE clearMVar #-}


-- | Apply an action to the contents of an `MVar`. Current thread will be blocked if
-- supplied MVar is empty and will wait until another thread fills it with a value. While
-- the action is being appplied other threads should not put anything into the `MVar`
-- otherwise current thread will get blocked again until another thread empties the
-- `MVar`. In other words this is not an atomic operation, but it is exception safe, since
-- the contents of `MVar` are restored regardless of the outcome of supplied action.
--
-- Same as `Control.Concurrent.MVar.withMVar` from @base@, but works in `UnliftPrimal`
-- with `RealWorld` state token.
--
-- @since 0.3.0
withMVar :: forall a b m. UnliftPrimal RW m => MVar a RW -> (a -> m b) -> m b
withMVar mvar !action =
  EI.mask $ \restore -> do
    a <- takeMVar mvar
    b <- restore (action a) `catchAll` \exc -> putLazyMVar mvar a >> raise exc
    b <$ putLazyMVar mvar a
{-# INLINE withMVar #-}


-- | Same as `withMVar`, but with supplied action executed with async exceptions masked,
-- but still interruptable.
--
-- Same as `Control.Concurrent.MVar.withMVarMasked` from @base@, but works in
-- `UnliftPrimal` with `RealWorld` state token.
--
-- @since 0.3.0
withMVarMasked :: forall a b m. UnliftPrimal RW m => MVar a RW -> (a -> m b) -> m b
withMVarMasked mvar !action =
  EI.mask_ $ do
    a <- takeMVar mvar
    b <- action a `catchAll` \exc -> putLazyMVar mvar a >> raise exc
    b <$ putLazyMVar mvar a
{-# INLINE withMVarMasked #-}




-- | Internal modification function that does no masking or forcing
modifyFetchLazyMVar :: UnliftPrimal RW m => (a -> a -> b) -> MVar a RW -> (a -> m a) -> m b
modifyFetchLazyMVar select mvar action = do
  a <- takeMVar mvar
  a' <- action a `catchAll` \exc -> putLazyMVar mvar a >> raise exc
  select a a' <$ putLazyMVar mvar a'
{-# INLINE modifyFetchLazyMVar #-}


-- | Apply a monadic action to the contents of supplied `MVar`. Provides the same
-- guarantees as `withMVar`.
--
-- Same as `GHC.modifyMVar_` from @base@, but is strict with respect to result of the
-- action and works in `UnliftPrimal` with `RealWorld` state token.
--
-- @since 0.3.0
modifyMVar_ :: forall a m. UnliftPrimal RW m => MVar a RW -> (a -> m a) -> m ()
modifyMVar_ mvar = void . modifyFetchOldMVar mvar
{-# INLINE modifyMVar_ #-}


-- | Same as `modifyMVarMAsked_`, but the supplied action has async exceptions masked.
--
-- Same as `GHC.modifyMVar` from @base@, except that it is strict in the new value and it
-- works in `UnliftPrimal` with `RealWorld` state token.
--
-- @since 0.3.0
modifyMVarMasked_ :: forall a m. UnliftPrimal RW m => MVar a RW -> (a -> m a) -> m ()
modifyMVarMasked_ mvar !action =
  EI.mask_ $ modifyFetchLazyMVar (\_ _ -> ()) mvar (action >=> \a' -> pure $! a')
{-# INLINE modifyMVarMasked_ #-}


-- | Same as `modifyMVar_`, but also returns the original value that was stored in the `MVar`
--
-- @since 0.3.0
modifyFetchOldMVar :: forall a m. UnliftPrimal RW m => MVar a RW -> (a -> m a) -> m a
modifyFetchOldMVar mvar !action =
  EI.mask $ \restore ->
    modifyFetchLazyMVar const mvar $ \a ->
      restore (action a >>= \a' -> pure $! a')
{-# INLINE modifyFetchOldMVar #-}



-- | Same as `modifyFetchOldMVar`, but supplied action will run with async exceptions
-- masked, but still interruptible
--
-- @since 0.3.0
modifyFetchOldMVarMasked :: forall a m. UnliftPrimal RW m => MVar a RW -> (a -> m a) -> m a
modifyFetchOldMVarMasked mvar !action =
  EI.mask_ $ modifyFetchLazyMVar const mvar (action >=> \a' -> pure $! a')
{-# INLINE modifyFetchOldMVarMasked #-}

-- | Same as `modifyMVar_`, but also returns the result of running the supplied action,
-- i.e. the new value that got stored in the `MVar`.
--
-- @since 0.3.0
modifyFetchNewMVar :: forall a m. UnliftPrimal RW m => MVar a RW -> (a -> m a) -> m a
modifyFetchNewMVar mvar !action =
  EI.mask $ \restore ->
    modifyFetchLazyMVar (flip const) mvar $ \a ->
      restore (action a >>= \a' -> pure $! a')
{-# INLINE modifyFetchNewMVar #-}


-- | Same as `modifyFetchNewMVar`, but supplied action will run with async exceptions
-- masked, but still interruptible
--
-- @since 0.3.0
modifyFetchNewMVarMasked :: forall a m. UnliftPrimal RW m => MVar a RW -> (a -> m a) -> m a
modifyFetchNewMVarMasked mvar !action =
  EI.mask_ $ modifyFetchLazyMVar (flip const) mvar (action >=> \a' -> pure $! a')
{-# INLINE modifyFetchNewMVarMasked #-}



-- | Apply a monadic action to the contents of supplied `MVar`. Provides the same
-- guarantees as `withMVar`.
--
-- Same as `GHC.modifyMVar` from @base@, except that it is strict in the new value and it
-- works in `UnliftPrimal` with `RealWorld` state token.
--
-- @since 0.3.0
modifyMVar :: forall a b m. UnliftPrimal RW m => MVar a RW -> (a -> m (a, b)) -> m b
modifyMVar mvar action =
  EI.mask $ \restore -> do
    a <- takeMVar mvar
    let run = restore (action a >>= \t@(!_, _) -> pure t)
    -- TODO: test against `force a'`
    (a', b) <- run `catchAll` \exc -> putLazyMVar mvar a >> raise exc
    b <$ putLazyMVar mvar a'
{-# INLINE modifyMVar #-}


-- | Apply a monadic action to the contents of supplied `MVar`. Provides the same
-- guarantees as `withMVar`.
--
-- Same as `GHC.modifyMVarMasked` from @base@, except that it is strict in the new value
-- and it works in `UnliftPrimal` with `RealWorld` state token.
--
-- @since 0.3.0
modifyMVarMasked :: forall a b m. UnliftPrimal RW m => MVar a RW -> (a -> m (a, b)) -> m b
modifyMVarMasked mvar action =
  EI.mask_ $ do
    a <- takeMVar mvar
    let run = action a >>= \t@(!_, _) -> pure t
    -- TODO: test against `force a'`
    (a', b) <- run `catchAll` \exc -> putLazyMVar mvar a >> raise exc
    b <$ putLazyMVar mvar a'
{-# INLINE modifyMVarMasked #-}


-- | Create a `Weak` pointer associated with the supplied `MVar`.
--
-- Same as `Control.Concurrent.MVar.mkWeakMVar` from @base@, but works in any `Primal`
-- with `RealWorld` state token.
--
-- @since 0.3.0
mkWeakMVar ::
     forall a b m. UnliftPrimal RW m
  => MVar a RW
  -> m b -- ^ An action that will get executed whenever `MVar` gets garbage collected by
         -- the runtime.
  -> m (Weak (MVar a RW))
mkWeakMVar mvar@(MVar mvar#) !finalizer =
  runInPrimalState finalizer $ \f# s ->
    case mkWeak# mvar# mvar f# s of
      (# s', weak# #) -> (# s', Weak weak# #)
{-# INLINE mkWeakMVar #-}



-- | Cast `MVar` into and the `Control.Concurrent.MVar.MVar` from @base@
--
-- @since 0.3.0
toBaseMVar :: MVar a RW -> GHC.MVar a
toBaseMVar (MVar mvar#) = GHC.MVar mvar#
{-# INLINE toBaseMVar #-}

-- | Cast `Control.Concurrent.MVar.MVar` from @base@ into `MVar`.
--
-- @since 0.3.0
fromBaseMVar :: GHC.MVar a -> MVar a RW
fromBaseMVar (GHC.MVar mvar#) = MVar mvar#
{-# INLINE fromBaseMVar #-}
