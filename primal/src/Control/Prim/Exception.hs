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
  , throw
  , throwTo
  , impureThrow
  -- * Catching
  , catch
  , catchAny
  , catchAnySync
  , catchAll
  , catchAllSync
  , try
  , tryAny
  , tryAnySync
  , onException
  -- TODO: Implement:
  -- , onAsyncException
  , withException
  , withAnyException
  -- TODO: Implement:
  -- , withAsyncException
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
  , maskPrimBase_
  , uninterruptibleMask
  , uninterruptibleMask_
  , uninterruptibleMaskPrimBase_
  , maskAsyncExceptions
  , unmaskAsyncExceptions
  , maskUninterruptible
  , GHC.MaskingState(..)
  , getMaskingState
  -- * Exceptions
  , GHC.Exception(..)
  , GHC.SomeException
  -- ** Async exceptions
  , GHC.AsyncException(..)
  , GHC.SomeAsyncException(..)
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
  , module Control.Prim.Monad
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
{-# INLINE isSyncException #-}

isAsyncException :: GHC.Exception e => e -> Bool
isAsyncException exc =
  case GHC.fromException (GHC.toException exc) of
    Just (GHC.SomeAsyncException _) -> True
    Nothing                         -> False
{-# INLINE isAsyncException #-}

-- | This is the same as `throwIO`, but works with any `MonadPrim` without restriction on
-- `RealWorld`.
throw :: (GHC.Exception e, MonadPrim s m) => e -> m a
throw e = unsafePrim (raiseIO# (GHC.toException e))
-- {-# INLINEABLE throw #-}


-- | Raise an impure exception from pure code. Returns a thunk, which will result in a
-- supplied exceptionn being thrown when evaluated.
--
-- @since 0.3.0
impureThrow :: GHC.Exception e => e -> a
impureThrow e = raise# (GHC.toException e)


-- | Similar to `throwTo`, except that it wraps any known non-async exception with
-- `SomeAsyncException`. This is necessary, because receiving thread will get the exception in
-- an asynchronous manner and without proper wrapping it will not be able to distinguish it
-- from a regular synchronous exception
throwTo :: (MonadPrim s m, GHC.Exception e) => GHC.ThreadId -> e -> m ()
throwTo tid e =
  unsafeIOToPrim $
  GHC.throwTo tid $
  if isAsyncException e
    then GHC.toException e
    else GHC.toException $ GHC.SomeAsyncException e
-- {-# INLINEABLE throwTo #-}

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
-- {-# INLINEABLE catch #-}
--{-# SPECIALIZE catch :: GHC.Exception e => IO a -> (e -> IO a) -> IO a #-}

catchAny ::
     forall a m. MonadUnliftPrim RW m
  => m a
  -> (GHC.SomeException -> m a)
  -> m a
catchAny action handler =
  runInPrimBase2 (const action) handler $ \action# handler# ->
    catch# (action# ()) handler#
-- {-# INLINEABLE catchAny #-}
--{-# SPECIALIZE catchAny :: IO a -> (GHC.SomeException -> IO a) -> IO a #-}


catchAnySync ::
     forall a m. MonadUnliftPrim RW m
  => m a
  -> (GHC.SomeException -> m a)
  -> m a
catchAnySync action handler =
  catchAny action $ \exc ->
    when (isAsyncException exc) (throw exc) >> handler exc
-- {-# INLINEABLE catchAnySync #-}

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
-- {-# INLINEABLE catchAll #-}

catchAllSync ::
     forall a m. MonadUnliftPrim RW m
  => m a
  -> (forall e . GHC.Exception e => e -> m a)
  -> m a
catchAllSync action handler =
  catchAll action $ \exc ->
    when (isAsyncException exc) (throw exc) >> handler exc
-- {-# INLINEABLE catchAllSync #-}


try :: (GHC.Exception e, MonadUnliftPrim RW m) => m a -> m (Either e a)
try f = catch (fmap Right f) (pure . Left)
-- {-# INLINEABLE try #-}
--{-# SPECIALIZE try :: GHC.Exception e => IO a -> IO (Either e a) #-}

tryAny :: MonadUnliftPrim RW m => m a -> m (Either GHC.SomeException a)
tryAny f = catchAny (Right <$> f) (pure . Left)
-- {-# INLINEABLE tryAny #-}

tryAnySync :: MonadUnliftPrim RW m => m a -> m (Either GHC.SomeException a)
tryAnySync f = catchAnySync (Right <$> f) (pure . Left)


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
      (\exc -> catchAnySync (void $ handler exc) (\_ -> pure ()) >> throw exc)


-- | Same as `withException`, but will invoke exception handling function on all
-- exceptions.
--
-- @since 0.3.0
withAnyException :: MonadUnliftPrim RW m => m a -> (GHC.SomeException -> m b) -> m a
withAnyException thing after =
  mask $ \restore -> do
    catchAny
      (restore thing)
      (\exc -> catchAnySync (void $ after exc) (\_ -> pure ()) >> throw exc)

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
        throw exc
    result <$ cleanup resource
{-# INLINEABLE bracket #-}

bracketOnError :: MonadUnliftPrim RW m => m a -> (a -> m b) -> (a -> m c) -> m c
bracketOnError acquire cleanup action =
  mask $ \restore -> do
    resource <- acquire
    catchAny (restore (action resource)) $ \exc -> do
      catchAnySync (void $ cleanup resource) $ \_ -> pure ()
      throw exc

finally :: MonadUnliftPrim RW m => m a -> m b -> m a
finally action cleanup =
  mask $ \restore -> do
    result <-
      catchAny (restore action) $ \exc -> do
        catchAnySync (void cleanup) $ \_ -> pure ()
        throw exc
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
    GHC.Unmasked -> runInPrimBase action maskAsyncExceptionsInternal#
    _ -> action
{-# INLINEABLE mask_  #-}


maskPrimBase_ :: forall a n m s. (MonadPrim s m, MonadPrimBase s n) => n a -> m a
maskPrimBase_ action =
  unsafeIOToPrim getMaskingState >>= \case
    GHC.Unmasked -> prim (maskAsyncExceptionsInternal# (primBase action))
    _ -> liftPrimBase action
{-# INLINEABLE maskPrimBase_  #-}

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
    GHC.Unmasked ->
      runInPrimBase
        (action (`runInPrimBase` unmaskAsyncExceptionsInternal#))
        maskAsyncExceptionsInternal#
    GHC.MaskedInterruptible ->
      action (`runInPrimBase` maskAsyncExceptionsInternal#)
    GHC.MaskedUninterruptible -> action uninterruptibleMask_
{-# INLINEABLE mask #-}
--{-# SPECIALIZE mask :: ((forall a. IO a -> IO a) -> IO b) -> IO b #-}

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
    GHC.Unmasked ->
      runInPrimBase
        (action (`runInPrimBase` unmaskAsyncExceptionsInternal#))
        maskAsyncExceptionsInternal#
    GHC.MaskedInterruptible ->
      action (`runInPrimBase` maskAsyncExceptionsInternal#)
    GHC.MaskedUninterruptible -> action uninterruptibleMask_
{-# INLINEABLE uninterruptibleMask #-}


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
uninterruptibleMask_ action = runInPrimBase action maskUninterruptibleInternal#
{-# INLINEABLE uninterruptibleMask_ #-}

uninterruptibleMaskPrimBase_ :: forall a n m s. (MonadPrimBase s n, MonadPrim s m) => n a -> m a
uninterruptibleMaskPrimBase_ action = prim (maskUninterruptibleInternal# (primBase action))
{-# INLINEABLE uninterruptibleMaskPrimBase_ #-}


-- | A direct wrapper around `maskAsyncExceptions#` primop. This is different and more
-- dangerous than `mask_` because it can turn uninterrubtable state into interruptable.
maskAsyncExceptions :: forall a m. MonadUnliftPrim RW m => m a -> m a
maskAsyncExceptions action = runInPrimBase action maskAsyncExceptions#
{-# INLINEABLE maskAsyncExceptions  #-}


-- | A direct wrapper around `unmaskAsyncExceptions#` primop.
unmaskAsyncExceptions :: forall a m. MonadUnliftPrim RW m => m a -> m a
unmaskAsyncExceptions action = runInPrimBase action unmaskAsyncExceptions#
{-# INLINEABLE unmaskAsyncExceptions  #-}


-- | A direct wrapper around `maskUninterruptible#` primop.
maskUninterruptible :: forall a m. MonadUnliftPrim RW m => m a -> m a
maskUninterruptible action = runInPrimBase action maskUninterruptible#
{-# INLINEABLE maskUninterruptible #-}

maskAsyncExceptionsInternal# :: (State# s -> (# State# s, a #)) -> State# s -> (# State# s, a #)
maskAsyncExceptionsInternal# = unsafeCoerce# maskAsyncExceptions#
{-# INLINEABLE maskAsyncExceptionsInternal# #-}

maskUninterruptibleInternal# :: (State# s -> (# State# s, a #)) -> State# s -> (# State# s, a #)
maskUninterruptibleInternal# = unsafeCoerce# maskUninterruptible#
{-# INLINEABLE maskUninterruptibleInternal# #-}

unmaskAsyncExceptionsInternal# :: (State# s -> (# State# s, b #)) -> State# s -> (# State# s, b #)
unmaskAsyncExceptionsInternal# = unsafeCoerce# unmaskAsyncExceptions#
{-# INLINEABLE unmaskAsyncExceptionsInternal# #-}

-- | Same as `GHC.getMaskingState`, but generalized to `MonadPrim`
--
-- @since 0.3.0
getMaskingState :: MonadPrim RW m => m GHC.MaskingState
getMaskingState = liftIO GHC.getMaskingState
{-# INLINEABLE getMaskingState #-}


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
