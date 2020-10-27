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
  , throwTo
  -- * Catching
  , catch
  , catchAny
  , catchAnySync
  , catchAll
  , catchAllSync
  , onException
  , withException
  , withAnyException
  , finally
  , bracket
  , bracket_
  , bracketOnError
  , ufinally
  , ubracket
  , ubracket_
  , ubracketOnError
  , mask
  , mask_
  , uninterruptibleMask
  , uninterruptibleMask_
  , maskAsyncExceptions
  , unmaskAsyncExceptions
  , maskUninterruptible
  , getMaskingState
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
import Control.Prim.Monad
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
    Nothing                         -> False

-- | This is the same as `throwIO`, but works with any `MonadPrim` without restriction on
-- `RealWorld`.
throwPrim :: (GHC.Exception e, MonadPrim s m) => e -> m a
throwPrim e = unsafePrim (raiseIO# (GHC.toException e))


-- | Similar to `throwTo`, except that it wraps any known non-async exception with
-- `SomeAsyncException`. This is necessary, because receiving thread will get the exception in
-- an asynchronous manner and without proper wrapping it will not be able to distinguish it
-- from a regular synchronous exception
throwTo :: (MonadPrim RW m, GHC.Exception e) => GHC.ThreadId -> e -> m ()
throwTo tid e =
  liftPrimIO $
  GHC.throwTo tid $
  if isAsyncException e
    then GHC.toException e
    else GHC.toException $ GHC.SomeAsyncException e

-- | Behaves exactly as `catch`, except that it works in any `MonadUnliftPrim`.
catch ::
     forall e a m. (GHC.Exception e, MonadUnliftPrim RW m)
  => m a
  -> (e -> m a)
  -> m a
catch action handler =
  runInPrimBase2 (const action) handler $ \action# handler# ->
    let handler'# :: GHC.SomeException -> (State# RW -> (# State# RW, a #))
        handler'# someExc =
          case GHC.fromException someExc of
            Just exc -> handler# exc
            Nothing -> raiseIO# someExc
     in catch# (action# ()) handler'#

catchAny ::
     forall a m. MonadUnliftPrim RW m
  => m a
  -> (GHC.SomeException -> m a)
  -> m a
catchAny action handler =
  runInPrimBase2 (const action) handler $ \action# handler# ->
    catch# (action# ()) handler#


catchAnySync ::
     forall a m. MonadUnliftPrim RW m
  => m a
  -> (GHC.SomeException -> m a)
  -> m a
catchAnySync action handler =
  catchAny action $ \exc ->
    when (isAsyncException exc) (throwPrim exc) >> handler exc

catchAll ::
     forall a m. MonadUnliftPrim RW m
  => m a
  -> (forall e . GHC.Exception e => e -> m a)
  -> m a
catchAll action handler =
  runInPrimBase2
    (const action)
    (\(GHC.SomeException e) -> handler e)
    (\action# handler# -> catch# (action# ()) handler#)

catchAllSync ::
     forall a m. MonadUnliftPrim RW m
  => m a
  -> (forall e . GHC.Exception e => e -> m a)
  -> m a
catchAllSync action handler =
  catchAll action $ \exc ->
    when (isAsyncException exc) (throwPrim exc) >> handler exc


-- | Run an action, while invoking an exception handler if that action fails for some
-- reason. Exception handling function has async exceptions masked, but it is still
-- interruptible, which can be undesired in some scenarios. If you are sure that the
-- cleanup action does not deadlock and you do need hard guarantees that it gets executed
-- you can run it as uninterruptible:
--
-- > uninterruptibleMask $ \restore -> withException (restore action) handler
--
-- @since 0.3.0
withException ::
     (MonadUnliftPrim RW m, GHC.Exception e) => m a -> (e -> m b) -> m a
withException action handler =
  mask $ \restore -> do
    catch
      (restore action)
      (\exc -> catchAnySync (void $ handler exc) (\_ -> pure ()) >> throwPrim exc)


-- | Same as `withException`, but will invoke exception handling function on all
-- exceptions.
--
-- @since 0.3.0
withAnyException :: MonadUnliftPrim RW m => m a -> (GHC.SomeException -> m b) -> m a
withAnyException thing after =
  mask $ \restore -> do
    catchAny
      (restore thing)
      (\exc -> catchAnySync (void $ after exc) (\_ -> pure ()) >> throwPrim exc)

-- | Async safe version of 'EUnsafe.onException'.
--
-- @since 0.1.0.0
onException :: MonadUnliftPrim RW m => m a -> m b -> m a
onException thing after = withAnyException thing (const after)


--
-- @since 0.3.0
bracket :: MonadUnliftPrim RW m => m a -> (a -> m b) -> (a -> m c) -> m c
bracket acquire cleanup action =
  mask $ \restore -> do
    resource <- acquire
    result <-
      catchAny (restore (action resource)) $ \exc -> do
        catchAnySync (void $ cleanup resource) $ \_ -> pure ()
        throwPrim exc
    result <$ cleanup resource

bracketOnError :: MonadUnliftPrim RW m => m a -> (a -> m b) -> (a -> m c) -> m c
bracketOnError acquire cleanup action =
  mask $ \restore -> do
    resource <- acquire
    catchAny (restore (action resource)) $ \exc -> do
      catchAnySync (void $ cleanup resource) $ \_ -> pure ()
      throwPrim exc

finally :: MonadUnliftPrim RW m => m a -> m b -> m a
finally action cleanup =
  mask $ \restore -> do
    result <-
      catchAny (restore action) $ \exc -> do
        catchAnySync (void cleanup) $ \_ -> pure ()
        throwPrim exc
    result <$ cleanup



--
-- @since 0.3.0
bracket_ :: MonadUnliftPrim RW m => m a -> m b -> m c -> m c
bracket_ acquire cleanup action = bracket acquire (const cleanup) (const action)


ubracket :: MonadUnliftPrim RW m => m a -> (a -> m b) -> (a -> m c) -> m c
ubracket acquire cleanup action =
  uninterruptibleMask $ \restore ->
    bracket (restore acquire) cleanup (restore . action)


--
-- @since 0.3.0
ubracket_ :: MonadUnliftPrim RW m => m a -> m b -> m c -> m c
ubracket_ acquire cleanup action = ubracket acquire (const cleanup) (const action)


ubracketOnError :: MonadUnliftPrim RW m => m a -> (a -> m b) -> (a -> m c) -> m c
ubracketOnError acquire cleanup action =
  uninterruptibleMask $ \restore ->
    bracketOnError (restore acquire) cleanup (restore . action)

ufinally :: MonadUnliftPrim RW m => m a -> m b -> m a
ufinally action cleanup =
  uninterruptibleMask $ \restore -> finally (restore action) cleanup


-- | Mask all asychronous exceptions, but keep it interruptible, unless the inherited state
-- was uninterruptible already, in which case this action has no affect. Same as
-- `Control.Exception.mask_`, except that it is polymorphic in state token. Inside a state
-- thread it cannot affect the result of computation, therefore it is safe to use it within
-- `ST` monad.
--
-- @since 0.3.0
mask_ :: forall a m s. MonadUnliftPrim s m => m a -> m a
mask_ action =
  unsafeIOToPrim getMaskingState >>= \case
    GHC.Unmasked -> runInPrimBase action mask#
    _ -> action
  where
    mask# :: (State# s -> (# State# s, a #)) -> State# s -> (# State# s, a #)
    mask# = unsafeCoerce# maskAsyncExceptions#
{-# INLINE mask_  #-}


-- | Mask all asychronous exceptions, but keep it interruptible, unless the inherited state
-- was uninterruptible already, in which case this action has no affect. Same as
-- `Control.Exception.mask`, except that it is polymorphic in state token. Inside a state
-- thread it cannot affect the result of computation, therefore it is safe to use it within
-- `ST` monad.
--
-- @since 0.3.0
mask ::
     forall a m s. MonadUnliftPrim s m
  => ((forall b. m b -> m b) -> m a)
  -> m a
mask action = do
  unsafeIOToPrim getMaskingState >>= \case
    GHC.Unmasked -> runInPrimBase (action (\subAction -> runInPrimBase subAction unmask#)) mask#
    GHC.MaskedInterruptible -> action (\subAction -> runInPrimBase subAction mask#)
    GHC.MaskedUninterruptible -> action uninterruptibleMask_
  where
    mask# :: (State# s -> (# State# s, c #)) -> State# s -> (# State# s, c #)
    mask# = unsafeCoerce# maskAsyncExceptions#
    unmask# :: (State# s -> (# State# s, b #)) -> State# s -> (# State# s, b #)
    unmask# = unsafeCoerce# unmaskAsyncExceptions#
{-# INLINE mask #-}


-- | Mask all asychronous exceptions and mark it uninterruptible. Same as
-- `Control.Exception.uninterruptibleMask`, except that it is polymorphic in state
-- token. Inside a state thread it cannot affect the result of computation, therefore it
-- is safe to use it within `ST` monad.
--
-- @since 0.3.0
uninterruptibleMask ::
     forall a m s. MonadUnliftPrim s m
  => ((forall b. m b -> m b) -> m a)
  -> m a
uninterruptibleMask action = do
  unsafeIOToPrim getMaskingState >>= \case
    GHC.Unmasked -> runInPrimBase (action (\subAction -> runInPrimBase subAction unmask#)) mask#
    GHC.MaskedInterruptible -> action (\subAction -> runInPrimBase subAction mask#)
    GHC.MaskedUninterruptible -> action uninterruptibleMask_
  where
    mask# :: (State# s -> (# State# s, c #)) -> State# s -> (# State# s, c #)
    mask# = unsafeCoerce# maskAsyncExceptions#
    unmask# :: (State# s -> (# State# s, b #)) -> State# s -> (# State# s, b #)
    unmask# = unsafeCoerce# unmaskAsyncExceptions#
{-# INLINE uninterruptibleMask #-}


-- | Mask all async exceptions and make sure evaluation cannot be interrupted. It is
-- polymorphic in the state token because it is perfectly safe to use with `ST` actions that
-- don't perform any allocations. It doesn't have to be restricted to `RealWorld` because it
-- has no impact on other threads and can't affect the result of computation, moreover pure
-- functions that implement tight loops are already non-interruptible. In fact using this
-- function is more dangerous in `IO` than it is in `ST`, because misuse can lead to deadlocks
-- in a concurrent setting.
--
-- @since 0.3.0
uninterruptibleMask_ :: forall a m s. MonadUnliftPrim s m => m a -> m a
uninterruptibleMask_ action = runInPrimBase action mask#
  where
    mask# :: (State# s -> (# State# s, a #)) -> State# s -> (# State# s, a #)
    mask# = unsafeCoerce# maskUninterruptible#
{-# INLINE uninterruptibleMask_ #-}


-- | A direct wrapper around `maskAsyncExceptions#` primop. This is different and more
-- dangerous than `mask_` because it can turn uninterrubtable state into interruptable.
maskAsyncExceptions :: forall a m. MonadUnliftPrim RW m => m a -> m a
maskAsyncExceptions action = runInPrimBase action maskAsyncExceptions#
{-# INLINE maskAsyncExceptions  #-}


-- | A direct wrapper around `unmaskAsyncExceptions#` primop.
unmaskAsyncExceptions :: forall a m. MonadUnliftPrim RW m => m a -> m a
unmaskAsyncExceptions action = runInPrimBase action unmaskAsyncExceptions#
{-# INLINE unmaskAsyncExceptions  #-}


-- | A direct wrapper around `maskUninterruptible#` primop.
maskUninterruptible :: forall a m. MonadUnliftPrim RW m => m a -> m a
maskUninterruptible action = runInPrimBase action maskUninterruptible#
{-# INLINE maskUninterruptible #-}

-- | Same as `GHC.getMaskingState`, but generalized to `MonadPrim`
--
-- @since 0.3.0
getMaskingState :: MonadPrim RW m => m GHC.MaskingState
getMaskingState = liftPrimIO GHC.getMaskingState
{-# INLINE getMaskingState #-}


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
