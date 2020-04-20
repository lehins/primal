{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Control.Monad.Prim.Core
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Control.Monad.Prim.Core where
  -- , withPrimBase
  -- , with#

import Control.Exception
import GHC.Conc
import Control.Monad.ST
import Control.Monad.Prim.Internal
import Control.Monad.Prim.Unsafe
import GHC.Exts

type RW = RealWorld


----- Exceptions

throwPrim :: (Exception e, MonadPrim s m) => e -> m a
throwPrim e = unsafeIOToPrim $ prim (raiseIO# (toException e))

catchPrim ::
     forall e a m. (Exception e, MonadUnliftPrim RW m)
  => m a
  -> (e -> m a)
  -> m a
catchPrim action handler =
  withRunInPrimBase $ \run ->
    let handler# :: SomeException -> (State# RW -> (# State# RW, a #))
        handler# e =
          case fromException e of
            Just e' -> primBase (run (handler e') :: IO a)
            Nothing -> raiseIO# e
     in prim (catch# (primBase (run action :: IO a)) handler#)

catchAnyPrim ::
     forall a m. (MonadUnliftPrim RW m)
  => m a
  -> (forall e . Exception e => e -> m a)
  -> m a
catchAnyPrim action handler =
  withRunInPrimBase $ \run ->
    let handler# :: SomeException -> (State# RW -> (# State# RW, a #))
        handler# (SomeException e) = primBase (run (handler e) :: IO a)
     in prim (catch# (primBase (run action :: IO a)) handler#)

catchAnySyncPrim ::
     forall a m. (MonadUnliftPrim RW m)
  => m a
  -> (forall e . Exception e => e -> m a)
  -> m a
catchAnySyncPrim action handler =
  withRunInPrimBase $ \run ->
    let handler# :: SomeException -> (State# RW -> (# State# RW, a #))
        handler# exc@(SomeException e) =
          case fromException exc of
            Just (SomeAsyncException _asyncExc) -> raiseIO# exc
            Nothing -> primBase (run (handler e) :: IO a)
     in prim (catch# (primBase (run action :: IO a)) handler#)

maskAsyncExceptionsPrim :: forall a m. MonadUnliftPrim RW m => m a -> m a
maskAsyncExceptionsPrim action =
  withRunInPrimBase $ \run -> prim (maskAsyncExceptions# (primBase (run action :: IO a)))

unmaskAsyncExceptionsPrim :: forall a m. MonadUnliftPrim RW m => m a -> m a
unmaskAsyncExceptionsPrim action =
  withRunInPrimBase $ \run -> prim (unmaskAsyncExceptions# (primBase (run action :: IO a)))

maskUninterruptiblePrim :: forall a m. MonadUnliftPrim RW m => m a -> m a
maskUninterruptiblePrim action =
  withRunInPrimBase $ \run -> prim (maskUninterruptible# (primBase (run action :: IO a)))


getMaskingStatePrim :: MonadPrim s m => m MaskingState
getMaskingStatePrim = unsafeIOToPrim getMaskingState



------- Evaluation


-- | This is an action that ensures that the value is still available and garbage
-- collector has not cleaned it up.
--
-- Make sure not to use it after some computation that doesn't return, like after
-- `forever` for example, otherwise touch will simply be removed by ghc and bad things
-- will happen. If you have a case like that, make sure to use `withPrimBase` instead.
touch :: MonadPrim s m => a -> m ()
touch x = unsafeIOToPrim $ prim_ (touch# x)
{-# INLINE touch #-}


-- | An action that evaluates a value to weak head normal form. Same
-- as `Control.Exception.evaluate`, except it work in a `MonadPrim`
seqPrim :: MonadPrim s m => a -> m a
seqPrim a = prim (seq# a)

sparkPrim :: MonadPrim s m => a -> m a
sparkPrim a = prim (spark# a)

numSparksPrim :: MonadPrim s m => m Int
numSparksPrim =
  prim $ \s# ->
    case numSparks# s# of
      (# s'#, n# #) -> (# s'#, I# n# #)


------- Concurrency

delay :: MonadPrim s m => Int -> m ()
delay (I# i#) = prim_ (delay# i#)


waitRead :: MonadPrim s m => Int -> m ()
waitRead (I# i#) = prim_ (waitRead# i#)


waitWrite :: MonadPrim s m => Int -> m ()
waitWrite (I# i#) = prim_ (waitWrite# i#)

-- | Unlike `Control.Concurrent.forkIO` it does not install any exception handlers on the
-- action, so you gotta make sure to do it yourself.
forkPrim :: MonadPrim RW m => m () -> m ThreadId
forkPrim action =
  prim $ \s# ->
    case fork# action s# of
      (# s'#, tid# #) -> (# s'#, ThreadId tid# #)

-- | Unlike `Control.Concurrent.forkOn` it does not install any exception handlers on the
-- action, so you gotta make sure to do it yourself.
forkOnPrim :: MonadPrim RW m => Int -> m () -> m ThreadId
forkOnPrim (I# cap#) action =
  prim $ \s# ->
    case forkOn# cap# action s# of
      (# s'#, tid# #) -> (# s'#, ThreadId tid# #)

throwToPrim :: (MonadPrim RW m, Exception e) => ThreadId -> e -> m ()
throwToPrim tid = liftPrimBase . throwTo tid

killThreadPrim :: MonadPrim RW m => ThreadId -> m ()
killThreadPrim tid = throwToPrim tid ThreadKilled


yield :: MonadPrim RW m => m ()
yield = prim_ yield#


myThreadIdPrim :: MonadPrim RW m => m ThreadId
myThreadIdPrim =
  prim $ \s# ->
    case myThreadId# s# of
      (# s'#, tid# #) -> (# s'#, ThreadId tid# #)

-- | Pointer should refer to UTF8 encoded string of bytes
labelThreadPrim :: MonadPrim RW m => ThreadId -> Ptr a -> m ()
labelThreadPrim (ThreadId tid#) (Ptr addr#) = prim_ (labelThread# tid# addr#)

isCurrentThreadBoundPrim :: MonadPrim RW m => m Bool
isCurrentThreadBoundPrim =
  prim $ \s# ->
    case isCurrentThreadBound# s# of
      (# s'#, bool# #) -> (# s'#, isTrue# bool# #)

threadStatusPrim :: MonadPrim RW m => ThreadId -> m ThreadStatus
threadStatusPrim = liftPrimBase . threadStatus

threadCapabilityPrim :: MonadPrim RW m => ThreadId -> m (Int, Bool)
threadCapabilityPrim = liftPrimBase . threadCapability


