{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
-- |
-- Module      : Primal.Concurrent
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Concurrent
  ( GHC.ThreadId(..)
  , fork
  , forkFinally
  , forkOn
  , forkOnFinally
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
  , getNumCapabilities
  , setNumCapabilities
  -- * Sparks
  , spark
  , numSparks
  , runSparks
  -- * Single threaded RTS
  , delay
  , waitRead
  , waitWrite
  , module Primal.Monad
  ) where

import qualified Control.Exception as GHC
import qualified Control.Concurrent as GHC
import Primal.Exception
import Primal.Monad
import Primal.Foreign
import qualified GHC.Conc as GHC
import qualified System.Timeout as GHC

spark :: Primal s m => a -> m a
spark a = primal (spark# a)

numSparks :: Primal s m => m Int
numSparks =
  primal $ \s ->
    case numSparks# s of
      (# s', n# #) -> (# s', I# n# #)

runSparks :: Primal s m => m ()
runSparks = primal_ loop
  where
    loop s =
      case getSpark# s of
        (# s', n#, p #) ->
          if isTrue# (n# ==# 0#)
            then s'
            else p `seq` loop s'

-- | Wrapper for `delay#`. Sleep specified number of microseconds. Not designed for
-- threaded runtime: __Errors when compiled with @-threaded@__
delay :: Primal s m => Int -> m ()
delay (I# i#) = primal_ (delay# i#)

-- | Wrapper for `waitRead#`. Block and wait for input to become available on the
-- `Fd`. Not designed for threaded runtime: __Errors out when compiled with @-threaded@__
waitRead :: Primal s m => Fd -> m ()
waitRead !fd =
  case fromIntegral fd of
    I# i# -> primal_ (waitRead# i#)


-- | Wrapper for `waitWrite#`. Block and wait until output is possible on the `Fd`.
-- Not designed for threaded runtime: __Errors out when compiled with @-threaded@__
waitWrite :: Primal s m => Fd -> m ()
waitWrite !fd =
  case fromIntegral fd of
    I# i# -> primal_ (waitWrite# i#)

-- | Wrapper around `fork#`. Unlike `Control.Concurrent.forkIO` it does not install
-- any exception handlers on the action, so you need make sure to do it yourself.
fork :: UnliftPrimal RW m => m () -> m GHC.ThreadId
fork action =
  runInPrimalState action $ \action# s ->
    case fork# (IO action#) s of
      (# s', tid# #) -> (# s', GHC.ThreadId tid# #)

-- | Spawn a thread and run an action in it. Any exception raised by the new thread will
-- be passed to the supplied exception handler, which itself will be run in a masked state
forkFinally :: UnliftPrimal RW m => m a -> (Either SomeException a -> m ()) -> m GHC.ThreadId
forkFinally action handler =
  mask $ \restore -> fork $ tryAny (restore action) >>= handler

-- | Wrapper around `forkOn#`. Unlike `Control.Concurrent.forkOn` it does not install any
-- exception handlers on the action, so you need make sure to do it yourself.
forkOn :: UnliftPrimal RW m => Int -> m () -> m GHC.ThreadId
forkOn (I# cap#) action =
  runInPrimalState action $ \action# s ->
    case forkOn# cap# (IO action#) s of
      (# s', tid# #) -> (# s', GHC.ThreadId tid# #)

forkOnFinally ::
     UnliftPrimal RW m
  => Int
  -> m a
  -> (Either SomeException a -> m ())
  -> m GHC.ThreadId
forkOnFinally cap action handler =
  mask $ \restore -> forkOn cap $ tryAny (restore action) >>= handler


forkOS :: UnliftPrimal RW m => m () -> m GHC.ThreadId
forkOS action = withRunInIO $ \run -> GHC.forkOS (run action)



-- | Wrapper around `killThread#`, which throws `GHC.ThreadKilled` exception in the target
-- thread. Use `throwTo` if you want a different exception to be thrown.
killThread :: Primal RW m => GHC.ThreadId -> m ()
killThread !tid = throwTo tid GHC.ThreadKilled

-- | Lifted version of `GHC.threadDelay`
threadDelay :: Primal RW m => Int -> m ()
threadDelay = liftIO . GHC.threadDelay

-- | Lifted version of `GHC.timeout`
--
-- @since 0.3.0
timeout :: UnliftPrimal RW m => Int -> m a -> m (Maybe a)
timeout !n !action = withRunInIO $ \run -> GHC.timeout n (run action)

-- | Same as `timeout`, but ignores the outcome
--
-- @since 0.3.0
timeout_ :: UnliftPrimal RW m => Int -> m a -> m ()
timeout_ n = void . timeout n



-- | Just like `Control.Concurrent.yield` this is a Wrapper around `yield#` primop ,
-- except that this version works for any state token. It is safe to use within `ST`
-- because it can't affect the result of computation, just the order of evaluation with
-- respect to other threads, which is not relevant for the state thread monad anyways.
--
-- @since 0.3.0
yield :: forall m s. Primal s m => m ()
yield = primal_ (unsafeCoerce# yield# :: State# s -> State# s)

-- | Wrapper around `myThreadId#`.
myThreadId :: Primal RW m => m GHC.ThreadId
myThreadId =
  primal $ \s ->
    case myThreadId# s of
      (# s', tid# #) -> (# s', GHC.ThreadId tid# #)

-- | Pointer should refer to UTF8 encoded string of bytes
labelThread :: Primal RW m => GHC.ThreadId -> Ptr a -> m ()
labelThread (GHC.ThreadId tid#) (Ptr addr#) = primal_ (labelThread# tid# addr#)

-- | Check if current thread was spawned with `forkOn#`
--
-- @since 0.3.0
isCurrentThreadBound :: Primal RW m => m Bool
isCurrentThreadBound =
  primal $ \s ->
    case isCurrentThreadBound# s of
      (# s', bool# #) -> (# s', isTrue# bool# #)

threadStatus :: Primal RW m => GHC.ThreadId -> m GHC.ThreadStatus
threadStatus = liftPrimalState . GHC.threadStatus

threadCapability :: Primal RW m => GHC.ThreadId -> m (Int, Bool)
threadCapability = liftPrimalState . GHC.threadCapability

getNumCapabilities :: Primal RW m => m Int
getNumCapabilities = liftPrimalState GHC.getNumCapabilities

setNumCapabilities :: Primal RW m => Int -> m ()
setNumCapabilities = liftPrimalState . GHC.setNumCapabilities



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


