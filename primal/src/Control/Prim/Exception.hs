{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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
  , GHC.throw
  , throwPrim
  , throwToPrim
  -- * Catching
  , catch
  , catchAny
  , catchAnySyncPrim
  , catchAllPrim
  , catchAllSyncPrim
  , bracket
  , bracket_
  , mask_
  , uninterruptibleMask_
  , maskAsyncExceptions
  , unmaskAsyncExceptions
  , maskUninterruptible
  , getMaskingStatePrim
  -- * Exceptions
  , GHC.Exception(..)
  , GHC.SomeException
  -- ** Async exceptions
  , GHC.AsyncException(..)
  , GHC.SomeAsyncException
  , isSyncException
  , isAsyncException
  , GHC.asyncExceptionToException
  , GHC.asyncExceptionFromException
  -- ** Standard exceptions
  , GHC.ErrorCall(..)
  , GHC.ArithException(..)
  , GHC.ArrayException(..)
  , GHC.AssertionFailed(..)
  , GHC.IOException
  , GHC.NonTermination(..)
  , GHC.NestedAtomically(..)
  , GHC.BlockedIndefinitelyOnMVar(..)
  , GHC.BlockedIndefinitelyOnSTM(..)
  , GHC.AllocationLimitExceeded(..)
  , GHC.Deadlock(..)
  -- * CallStack
  , CallStack
  , HasCallStack
  , callStack
  , getCallStack
  , prettyCallStack
  , SrcLoc(..)
  , prettySrcLoc
  ) where

import qualified Control.Exception as GHC
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
--import GHC.IO (IO(..))


----- Exceptions

isSyncException :: GHC.Exception e => e -> Bool
isSyncException = not . isAsyncException

isAsyncException :: GHC.Exception e => e -> Bool
isAsyncException exc =
  case GHC.fromException (GHC.toException exc) of
    Just (GHC.SomeAsyncException _) -> True
    Nothing                     -> False

-- | This is the same as `throwIO`, but works with any `MonadPrim` without restriction on
-- `RealWorld`.
throwPrim :: (GHC.Exception e, MonadPrim s m) => e -> m a
throwPrim e = unsafePrim (raiseIO# (GHC.toException e))
{-# INLINE throwPrim  #-}


-- | Similar to `throwTo`, except that it wraps any known non-async exception with
-- `SomeAsyncException`. This is necessary, because receiving thread will get the exception in
-- an asynchronous manner and without proper wrapping it will not be able to distinguish it
-- from a regular synchronous exception
throwToPrim :: (MonadPrim RW m, GHC.Exception e) => GHC.ThreadId -> e -> m ()
throwToPrim tid e =
  liftPrimIO $
  GHC.throwTo tid $
  if isAsyncException e
    then GHC.toException e
    else GHC.toException $ GHC.SomeAsyncException e

--
-- @since 0.3.0
bracket :: MonadUnliftPrim RW m => m a -> (a -> m b) -> (a -> m c) -> m c
bracket before after thing =
  withRunInPrimBase $ \run ->
    GHC.mask $ \restore -> do
      x <- run before
      res1 <- GHC.try $ restore $ run $ thing x
      case res1 of
        Left (e1 :: GHC.SomeException) -> do
          _ :: Either GHC.SomeException b <-
            GHC.try $ uninterruptibleMask_ $ run $ after x
          throwPrim e1
        Right y -> do
          _ <- uninterruptibleMask_ $ run $ after x
          return y

--
-- @since 0.3.0
bracket_ :: MonadUnliftPrim RW m => m a -> m b -> m c -> m c
bracket_ before after thing = bracket before (const after) (const thing)

-- | Behaves exactly as `catch`, except that it works in any `MonadUnliftPrim`.
catch ::
     forall e a m. (GHC.Exception e, MonadUnliftPrim RW m)
  => m a
  -> (e -> m a)
  -> m a
catch action handler =
  withRunInPrimBase $ \run ->
    let handler# :: GHC.SomeException -> (State# RW -> (# State# RW, a #))
        handler# someExc =
          case GHC.fromException someExc of
            Just exc -> primBase (run (handler exc) :: IO a)
            Nothing -> raiseIO# someExc
     in prim (catch# (primBase (run action :: IO a)) handler#)


catchAny ::
     forall a m. MonadUnliftPrim RW m
  => m a
  -> (GHC.SomeException -> m a)
  -> m a
catchAny action handler =
  withRunInPrimBase $ \run ->
    let handler# :: GHC.SomeException -> (State# RW -> (# State# RW, a #))
        handler# exc = primBase (run (handler exc) :: IO a)
     in prim (catch# (primBase (run action :: IO a)) handler#)

catchAnySyncPrim ::
     forall a m. MonadUnliftPrim RW m
  => m a
  -> (GHC.SomeException -> m a)
  -> m a
catchAnySyncPrim action handler =
  withRunInPrimBase $ \run ->
    let handler# :: GHC.SomeException -> (State# RW -> (# State# RW, a #))
        handler# exc
          | isAsyncException exc = raiseIO# exc
          | otherwise = primBase (run (handler exc) :: IO a)
     in prim (catch# (primBase (run action :: IO a)) handler#)

catchAllPrim ::
     forall a m. MonadUnliftPrim RW m
  => m a
  -> (forall e . GHC.Exception e => e -> m a)
  -> m a
catchAllPrim action handler =
  withRunInPrimBase $ \run ->
    let handler# :: GHC.SomeException -> (State# RW -> (# State# RW, a #))
        handler# (GHC.SomeException e) = primBase (run (handler e) :: IO a)
     in prim (catch# (primBase (run action :: IO a)) handler#)

catchAllSyncPrim ::
     forall a m. MonadUnliftPrim RW m
  => m a
  -> (forall e . GHC.Exception e => e -> m a)
  -> m a
catchAllSyncPrim action handler =
  withRunInPrimBase $ \run ->
    let handler# :: GHC.SomeException -> (State# RW -> (# State# RW, a #))
        handler# exc@(GHC.SomeException e)
          | isAsyncException exc = raiseIO# exc
          | otherwise = primBase (run (handler e) :: IO a)
     in prim (catch# (primBase (run action :: IO a)) handler#)

-- | Mask all asychronous exceptions, but keep it interruptible, unless the inherited state
-- was uninterruptible already, in which case this action has no affect. Same as
-- `Control.Exception.mask_`, except that it is polymorphic in state token. Inside a state
-- thread it cannot affect the result of computation, therefore it is safe to use it within
-- `ST` monad.
--
-- @since 0.3.0
mask_ :: forall a m s. MonadPrimBase s m => m a -> m a
mask_ action =
  unsafeIOToPrim getMaskingStatePrim >>= \case
    GHC.Unmasked -> unsafePrim (maskAsyncExceptions# (unsafePrimBase action))
    _ -> action
{-# INLINE mask_  #-}


-- | Mask all async exceptions and make sure evaluation cannot be interrupted. It is
-- polymorphic in the state token because it is perfectly safe to use with `ST` actions that
-- don't perform any allocations. It doesn't have to be restricted to `RealWorld` because it
-- has no impact on other threads and can't affect the result of computation, moreover pure
-- functions that implement tight loops are already non-interruptible. In fact using this
-- function is more dangerous in `IO` than it is in `ST`, because misuse can lead to deadlocks
-- in a concurrent setting.
--
-- @since 0.3.0
uninterruptibleMask_ :: forall a m s. MonadPrimBase s m => m a -> m a
uninterruptibleMask_ action =
  unsafePrim (maskUninterruptible# (unsafePrimBase action))
{-# INLINE uninterruptibleMask_ #-}


-- | A direct wrapper around `maskAsyncExceptions#` primop. This is different and more
-- dangerous than `mask_` because it can turn uninterrubtable state into interruptable.
maskAsyncExceptions :: forall a m. MonadPrimBase RW m => m a -> m a
maskAsyncExceptions action = prim (maskAsyncExceptions# (primBase action))
{-# INLINE maskAsyncExceptions  #-}


-- | A direct wrapper around `unmaskAsyncExceptions#` primop.
unmaskAsyncExceptions :: forall a m. MonadPrimBase RW m => m a -> m a
unmaskAsyncExceptions action = prim (unmaskAsyncExceptions# (primBase action))
{-# INLINE unmaskAsyncExceptions  #-}


-- | A direct wrapper around `maskUninterruptible#` primop.
maskUninterruptible :: forall a m. MonadPrimBase RW m => m a -> m a
maskUninterruptible action = prim (maskUninterruptible# (primBase action))
{-# INLINE maskUninterruptible #-}

-- | Same as `GHC.getMaskingState`, but generalized to `MonadPrim`
getMaskingStatePrim :: MonadPrim RW m => m GHC.MaskingState
getMaskingStatePrim = liftPrimIO GHC.getMaskingState
{-# INLINE getMaskingStatePrim #-}


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
