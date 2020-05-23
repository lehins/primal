{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Control.Prim.Concurrent
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Control.Prim.Concurrent
  ( module Control.Prim.Concurrent
  ) where

import qualified Control.Exception as GHC
import qualified GHC.Conc as GHC
import Control.Prim.Exception
import Control.Prim.Monad.Internal
import GHC.Exts
import System.Posix.Types


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
-- `Fd`. Not designed for threaded runtime: __Errors when compiled with @-threaded@__
waitRead :: MonadPrim s m => Fd -> m ()
waitRead fd =
  case fromIntegral fd of
    I# i# -> prim_ (waitRead# i#)


-- | Wrapper for `waitWrite#`. Block and wait until output is possible on the `Fd`.
-- Not designed for threaded runtime: __Errors when compiled with @-threaded@__
waitWrite :: MonadPrim s m => Fd -> m ()
waitWrite fd =
  case fromIntegral fd of
    I# i# -> prim_ (waitWrite# i#)

-- | Wrapper around `fork#`. Unlike `Control.Concurrent.forkIO` it does not install
-- any exception handlers on the action, so you need make sure to do it yourself.
fork :: MonadPrim RW m => m () -> m GHC.ThreadId
fork action =
  prim $ \s ->
    case fork# action s of
      (# s', tid# #) -> (# s', GHC.ThreadId tid# #)

-- | Wrapper around `forkOn#`. Unlike `Control.Concurrent.forkOn` it does not install any
-- exception handlers on the action, so you need make sure to do it yourself.
forkOn :: MonadPrim RW m => Int -> m () -> m GHC.ThreadId
forkOn (I# cap#) action =
  prim $ \s ->
    case forkOn# cap# action s of
      (# s', tid# #) -> (# s', GHC.ThreadId tid# #)

-- | Wrapper around `killThread#`, which throws `GHC.ThreadKilled` exception in the target
-- thread. Use `throwTo` if you want a different exception to be thrown.
killThread :: MonadPrim RW m => GHC.ThreadId -> m ()
killThread tid = throwTo tid GHC.ThreadKilled


-- | Wrapper around `yield#`.
yield :: MonadPrim RW m => m ()
yield = prim_ yield#

-- | Wrapper around `myThreadId#`.
myThreadId :: MonadPrim RW m => m GHC.ThreadId
myThreadId =
  prim $ \s ->
    case myThreadId# s of
      (# s', tid# #) -> (# s', GHC.ThreadId tid# #)

-- | Pointer should refer to UTF8 encoded string of bytes
labelThread :: MonadPrim RW m => GHC.ThreadId -> Ptr a -> m ()
labelThread (GHC.ThreadId tid#) (Ptr addr#) = prim_ (labelThread# tid# addr#)

isCurrentThreadBoundPrim :: MonadPrim RW m => m Bool
isCurrentThreadBoundPrim =
  prim $ \s ->
    case isCurrentThreadBound# s of
      (# s', bool# #) -> (# s', isTrue# bool# #)

threadStatus :: MonadPrim RW m => GHC.ThreadId -> m GHC.ThreadStatus
threadStatus = liftPrimBase . GHC.threadStatus

threadCapability :: MonadPrim RW m => GHC.ThreadId -> m (Int, Bool)
threadCapability = liftPrimBase . GHC.threadCapability


