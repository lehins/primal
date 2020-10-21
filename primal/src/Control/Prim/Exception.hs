{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
#if !MIN_VERSION_base(4,9,0)
  {-# LANGUAGE ConstraintKinds #-}
  {-# LANGUAGE KindSignatures #-}
  {-# LANGUAGE ImplicitParams #-}
#endif
-- |
-- Module      : Control.Prim.Exception
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Control.Prim.Exception
  (
  -- * Throwing
    module Control.Prim.Monad.Throw
  , throw
  , throwPrim
  , throwToPrim
  -- * Catching
  , catchPrim
  , catchAnyPrim
  , catchAnySyncPrim
  , catchAllPrim
  , catchAllSyncPrim
  , maskAsyncExceptions
  , unmaskAsyncExceptions
  , maskUninterruptible
  , getMaskingStatePrim
  -- * Exceptions
  , Exception(..)
  , SomeException
  -- ** Async exceptions
  , AsyncException(..)
  , SomeAsyncException
  , isSyncException
  , isAsyncException
  , asyncExceptionToException
  , asyncExceptionFromException
  -- ** Standard exceptions
  , ErrorCall(..)
  , ArithException(..)
  , ArrayException(..)
  , AssertionFailed(..)
  , IOException
  , NonTermination(..)
  , NestedAtomically(..)
  , BlockedIndefinitelyOnMVar(..)
  , BlockedIndefinitelyOnSTM(..)
  , AllocationLimitExceeded(..)
  , Deadlock(..)
  -- * CallStack
  , CallStack
  , HasCallStack
  , callStack
  , getCallStack
  , prettyCallStack
  , SrcLoc(..)
  , prettySrcLoc
  ) where

import Control.Exception as GHC
import Control.Prim.Monad.Internal
import Control.Prim.Monad.Throw
import Control.Prim.Monad.Unsafe
import qualified GHC.Conc as GHC
import GHC.Exts
import GHC.Stack
#if !MIN_VERSION_base(4,9,0)
import Data.List (intercalate)
import GHC.SrcLoc
#endif
import GHC.IO (IO(..))


----- Exceptions

isSyncException :: Exception e => e -> Bool
isSyncException = not . isAsyncException

isAsyncException :: Exception e => e -> Bool
isAsyncException exc =
  case fromException (toException exc) of
    Just (SomeAsyncException _) -> True
    Nothing                     -> False

-- | This is the same as `throwIO`, but works with any `MonadPrim` without restriction on
-- `RealWorld`.
throwPrim :: (Exception e, MonadPrim s m) => e -> m a
throwPrim e = unsafeIOToPrim $ prim (raiseIO# (toException e))


-- | Similar to `throwTo`, except that it wraps any known non-async exception with
-- `SomeAsyncException`, just like
-- [@UnliftIO.Exception.throwTo@](https://hackage.haskell.org/package/unliftio/docs/UnliftIO-Exception.html#v:throwTo)
-- does. This is necessary, because receiving thread will get the exception in an
-- asynchronous manner and without proper wrapping it will not be able to distinguish it
-- from a regular synchronous exception
throwToPrim :: (MonadPrim RW m, Exception e) => GHC.ThreadId -> e -> m ()
throwToPrim tid e =
  liftPrimBase $
  GHC.throwTo tid $
  if isAsyncException e
    then toException e
    else toException $ SomeAsyncException e

-- | Behaves exactly as `catch`, except that it works in any `MonadUnliftPrim`.
catchPrim ::
     forall e a m. (Exception e, MonadUnliftPrim RW m)
  => m a
  -> (e -> m a)
  -> m a
catchPrim action handler =
  withRunInPrimBase $ \run ->
    let handler# :: SomeException -> (State# RW -> (# State# RW, a #))
        handler# someExc =
          case fromException someExc of
            Just exc -> primBase (run (handler exc) :: IO a)
            Nothing -> raiseIO# someExc
     in prim (catch# (primBase (run action :: IO a)) handler#)


catchAnyPrim ::
     forall a m. MonadUnliftPrim RW m
  => m a
  -> (SomeException -> m a)
  -> m a
catchAnyPrim action handler =
  withRunInPrimBase $ \run ->
    let handler# :: SomeException -> (State# RW -> (# State# RW, a #))
        handler# exc = primBase (run (handler exc) :: IO a)
     in prim (catch# (primBase (run action :: IO a)) handler#)

catchAnySyncPrim ::
     forall a m. MonadUnliftPrim RW m
  => m a
  -> (SomeException -> m a)
  -> m a
catchAnySyncPrim action handler =
  withRunInPrimBase $ \run ->
    let handler# :: SomeException -> (State# RW -> (# State# RW, a #))
        handler# exc
          | isAsyncException exc = raiseIO# exc
          | otherwise = primBase (run (handler exc) :: IO a)
     in prim (catch# (primBase (run action :: IO a)) handler#)

catchAllPrim ::
     forall a m. MonadUnliftPrim RW m
  => m a
  -> (forall e . Exception e => e -> m a)
  -> m a
catchAllPrim action handler =
  withRunInPrimBase $ \run ->
    let handler# :: SomeException -> (State# RW -> (# State# RW, a #))
        handler# (SomeException e) = primBase (run (handler e) :: IO a)
     in prim (catch# (primBase (run action :: IO a)) handler#)

catchAllSyncPrim ::
     forall a m. MonadUnliftPrim RW m
  => m a
  -> (forall e . Exception e => e -> m a)
  -> m a
catchAllSyncPrim action handler =
  withRunInPrimBase $ \run ->
    let handler# :: SomeException -> (State# RW -> (# State# RW, a #))
        handler# exc@(SomeException e)
          | isAsyncException exc = raiseIO# exc
          | otherwise = primBase (run (handler e) :: IO a)
     in prim (catch# (primBase (run action :: IO a)) handler#)


--maskAsyncExceptions :: forall a m. MonadUnliftPrim RW m => m a -> m a
maskAsyncExceptions :: MonadPrimBase RW m => m a -> m a
maskAsyncExceptions action = prim (maskAsyncExceptions# (primBase action))
  --withRunInPrimBase $ \run -> prim (maskAsyncExceptions# (primBase (run action :: IO a)))
{-# INLINE maskAsyncExceptions  #-}

unmaskAsyncExceptions :: forall a m. MonadUnliftPrim RW m => m a -> m a
unmaskAsyncExceptions action =
  withRunInPrimBase $ \run -> prim (unmaskAsyncExceptions# (primBase (run action :: IO a)))

-- maskUninterruptible :: MonadPrimBase RW m => m a -> m a
-- maskUninterruptible action = prim (maskUninterruptible# (primBase action))
maskUninterruptible :: forall a m. MonadUnliftPrim RW m => m a -> m a
maskUninterruptible action =
  withRunInPrimBase (\run -> IO (maskUninterruptible# (primBase (run action :: IO a))))
{-# INLINE maskUninterruptible  #-}

-- | Same as `GHC.getMaskingState`, but generalized to `MonadPrim`
getMaskingStatePrim :: MonadPrim RW m => m MaskingState
getMaskingStatePrim = liftPrimBase GHC.getMaskingState


#if !MIN_VERSION_base(4,9,0)

-- | (Implemented for compatibility with GHC-7.10.2)
type HasCallStack = (?callStack :: CallStack)

callStack :: HasCallStack => CallStack
callStack = ?callStack

-- | Pretty print a 'SrcLoc'. (Implemented for compatibility with GHC-7.10.2)
--
-- @since 3.0.0
prettySrcLoc :: SrcLoc -> String
prettySrcLoc = showSrcLoc

-- | Pretty print a 'CallStack'. (Implemented for compatibility with GHC-7.10.2)
--
-- @since 3.0.0
prettyCallStack :: CallStack -> String
prettyCallStack = intercalate "\n" . prettyCallStackLines


prettyCallStackLines :: CallStack -> [String]
prettyCallStackLines cs = case getCallStack cs of
  []  -> []
  stk -> "CallStack (from HasCallStack):"
       : map (("  " ++) . prettyCallSite) stk
  where
    prettyCallSite (f, loc) = f ++ ", called at " ++ prettySrcLoc loc
#endif
