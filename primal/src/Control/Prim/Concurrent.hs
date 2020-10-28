{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
-- |
-- Module      : Control.Prim.Concurrent
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Control.Prim.Concurrent
  ( GHC.ThreadId(..)
  , fork
  , forkCatchAny
  , forkOn
  , forkOnCatchAny
  , forkOS
  , killThread
  , yield

  , threadDelay
  , timeout
  , timeout_

  , myThreadId
  , threadIdToCInt
  , threadStatus
  , labelThread
  , isCurrentThreadBound
  , threadCapability
  -- * Sparks
  , spark
  , numSparks
  , runSparks
  -- * Single threaded RTS
  , delay
  , waitRead
  , waitWrite
  ) where

import qualified Control.Exception as GHC
import qualified Control.Concurrent as GHC
import Control.Prim.Exception
import Control.Prim.Monad
import Foreign.Prim
import qualified GHC.Conc as GHC
import qualified System.Timeout as GHC

spark :: MonadPrim s m => a -> m a
spark a = prim (spark# a)

numSparks :: MonadPrim s m => m Int
numSparks =
  prim $ \s ->
    case numSparks# s of
      (# s', n# #) -> (# s', I# n# #)

runSparks :: MonadPrim s m => m ()
runSparks = prim_ loop
  where
    loop s =
      case getSpark# s of
        (# s', n#, p #) ->
          if isTrue# (n# ==# 0#)
            then s'
            else p `seq` loop s'

-- | Wrapper for `delay#`. Sleep specified number of microseconds. Not designed for
-- threaded runtime: __Errors when compiled with @-threaded@__
delay :: MonadPrim s m => Int -> m ()
delay (I# i#) = prim_ (delay# i#)

-- | Wrapper for `waitRead#`. Block and wait for input to become available on the
-- `Fd`. Not designed for threaded runtime: __Errors out when compiled with @-threaded@__
waitRead :: MonadPrim s m => Fd -> m ()
waitRead !fd =
  case fromIntegral fd of
    I# i# -> prim_ (waitRead# i#)


-- | Wrapper for `waitWrite#`. Block and wait until output is possible on the `Fd`.
-- Not designed for threaded runtime: __Errors out when compiled with @-threaded@__
waitWrite :: MonadPrim s m => Fd -> m ()
waitWrite !fd =
  case fromIntegral fd of
    I# i# -> prim_ (waitWrite# i#)

-- | Wrapper around `fork#`. Unlike `Control.Concurrent.forkIO` it does not install
-- any exception handlers on the action, so you need make sure to do it yourself.
fork :: MonadUnliftPrim RW m => m () -> m GHC.ThreadId
fork action =
  runInPrimBase action $ \action# s ->
    case fork# (IO action#) s of
      (# s', tid# #) -> (# s', GHC.ThreadId tid# #)

-- | Spawn a thread and run an action in it. Any exception raised by the new thread will
-- be passed to the supplied exception handler, which itself will be run in a masked state
forkCatchAny :: MonadUnliftPrim RW m => m () -> (SomeException -> m ()) -> m GHC.ThreadId
forkCatchAny action handler =
  mask_ $ fork $ catchAny (unmaskAsyncExceptions action) handler

-- | Wrapper around `forkOn#`. Unlike `Control.Concurrent.forkOn` it does not install any
-- exception handlers on the action, so you need make sure to do it yourself.
forkOn :: MonadUnliftPrim RW m => Int -> m () -> m GHC.ThreadId
forkOn (I# cap#) action =
  runInPrimBase action $ \action# s ->
    case forkOn# cap# (IO action#) s of
      (# s', tid# #) -> (# s', GHC.ThreadId tid# #)

forkOnCatchAny :: MonadUnliftPrim RW m => Int -> m () -> (SomeException -> m ()) -> m GHC.ThreadId
forkOnCatchAny cap action handler =
  mask_ $ forkOn cap $ catchAny (unmaskAsyncExceptions action) handler


forkOS :: MonadUnliftPrim RW m => m () -> m GHC.ThreadId
forkOS action = withRunInIO $ \run -> GHC.forkOS (run action)

-- | Wrapper around `killThread#`, which throws `GHC.ThreadKilled` exception in the target
-- thread. Use `throwTo` if you want a different exception to be thrown.
killThread :: MonadPrim RW m => GHC.ThreadId -> m ()
killThread !tid = throwTo tid GHC.ThreadKilled

-- | Lifted version of `GHC.threadDelay`
threadDelay :: MonadPrim RW m => Int -> m ()
threadDelay = liftIO . GHC.threadDelay

-- @since 0.3.0
timeout :: MonadUnliftPrim RW m => Int -> m a -> m (Maybe a)
timeout !n !action = withRunInIO $ \run -> GHC.timeout n (run action)

timeout_ :: MonadUnliftPrim RW m => Int -> m a -> m ()
timeout_ n = void . timeout n



-- | Just like `Control.Concurrent.yield` this is a Wrapper around `yield#` primop ,
-- except that this version works for any state token. It is safe to use within `ST`
-- because it can't affect the result of computation, just the order of evaluation with
-- respect to other threads, which is not relevant for the state thread monad anyways.
--
-- @since 0.3.0
yield :: forall m s. MonadPrim s m => m ()
yield = prim_ (unsafeCoerce# yield# :: State# s -> State# s)

-- | Wrapper around `myThreadId#`.
myThreadId :: MonadPrim RW m => m GHC.ThreadId
myThreadId =
  prim $ \s ->
    case myThreadId# s of
      (# s', tid# #) -> (# s', GHC.ThreadId tid# #)

-- | Pointer should refer to UTF8 encoded string of bytes
labelThread :: MonadPrim RW m => GHC.ThreadId -> Ptr a -> m ()
labelThread (GHC.ThreadId tid#) (Ptr addr#) = prim_ (labelThread# tid# addr#)

-- | Check if current thread was spawned with `forkOn#`
--
-- @since 0.3.0
isCurrentThreadBound :: MonadPrim RW m => m Bool
isCurrentThreadBound =
  prim $ \s ->
    case isCurrentThreadBound# s of
      (# s', bool# #) -> (# s', isTrue# bool# #)

threadStatus :: MonadPrim RW m => GHC.ThreadId -> m GHC.ThreadStatus
threadStatus = liftPrimBase . GHC.threadStatus

threadCapability :: MonadPrim RW m => GHC.ThreadId -> m (Int, Bool)
threadCapability = liftPrimBase . GHC.threadCapability

-- | Something that is not exported from @base@: convert a `GHC.ThreadId` to a regular
-- integral type.
--
-- @since 0.0.0
threadIdToCInt :: GHC.ThreadId -> CInt
threadIdToCInt tid = getThreadId (id2TSO tid)

id2TSO :: GHC.ThreadId -> ThreadId#
id2TSO (GHC.ThreadId t) = t

-- Relevant ticket: https://gitlab.haskell.org/ghc/ghc/-/issues/8281
foreign import ccall unsafe "rts_getThreadId" getThreadId :: ThreadId# -> CInt


