{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
#if !MIN_VERSION_base(4,9,0)
  {-# LANGUAGE ConstraintKinds #-}
  {-# LANGUAGE KindSignatures #-}
  {-# LANGUAGE ImplicitParams #-}
#endif
-- |
-- Module      : Primal.Exception
-- Copyright   : (c) Alexey Kuleshevich 2020-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Exception
  (
  -- * Raising
    module Primal.Monad.Throw
  , raise
  , raiseLeft
  , raiseTo
  , raiseImprecise
  , raiseLeftImprecise
  , ImpreciseException(..)
  -- * Catching
  , catch
  , catchJust
  , catchSync
  , catchAsync
  -- , catchAny
  -- , catchAnySync
  , catchAll
  , catchAllSync
  , catchAllAsync
  , try
  , trySync
  , tryAsync
  , tryAll
  , tryAllSync
  , tryAllAsync
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
  , maskPrimalState_
  , uninterruptibleMask
  , uninterruptibleMask_
  , uninterruptibleMaskPrimalState_
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
  ) where

import qualified Control.Exception as GHC
import Control.Monad
import Primal.Monad.Internal
import Primal.Monad.Throw
import Primal.Monad.Unsafe
import qualified GHC.Conc as GHC
import GHC.Exts
import GHC.Stack
#if !MIN_VERSION_base(4,9,0)
import Data.List (intercalate)
import GHC.SrcLoc
#endif
--import GHC.IO (IO(..))


----- Exceptions

-- | Check if it is a synchronous exception.
--
-- @since 1.0.0
isSyncException :: GHC.Exception e => e -> Bool
isSyncException = not . isAsyncException
{-# INLINE isSyncException #-}

-- | Check if it is an asynchronous exception.
--
-- @since 1.0.0
isAsyncException :: GHC.Exception e => e -> Bool
isAsyncException exc =
  case GHC.fromException (GHC.toException exc) of
    Just (GHC.SomeAsyncException _) -> True
    Nothing                         -> False
{-# INLINE isAsyncException #-}

-- | This is the same as `GHC.throwIO`, but works in any `Primal` action without
-- restriction on `RealWorld`.
raise :: (GHC.Exception e, Primal s m) => e -> m a
raise e = unsafePrimal (raiseIO# (GHC.toException e))
-- {-# INLINEABLE raise #-}


-- | Raise an exception when it is supplied with Left or return a value unmodified upon Right.
--
-- @since 1.0.0
raiseLeft :: (GHC.Exception e, Primal s m) => Either e a -> m a
raiseLeft =
  \case
    Left exc -> raise exc
    Right res -> pure res


data ImpreciseException =
  ImpreciseException
    { impreciseException :: GHC.SomeException
    , impreciseCallStack :: CallStack
    }

instance Show ImpreciseException where
  showsPrec _ ImpreciseException {impreciseException, impreciseCallStack} =
    ("ImpreciseException '" ++) .
    (GHC.displayException impreciseException ++) .
    ("' was raised:\n" ++) . (prettyCallStack impreciseCallStack ++)

instance GHC.Exception ImpreciseException


-- | Create a thunk, which, upon evaluation to WHNF, will cause an exception
-- being raised. The actual exception that will be thrown is the
-- `ImpreciseException`, which will contain the supplied exception as well as the
-- callstack.
--
-- @since 1.0.0
raiseImprecise :: (HasCallStack, GHC.Exception e) => e -> a
raiseImprecise e = raise# (GHC.toException (ImpreciseException (GHC.toException e) ?callStack))

-- | Convert the Left exception into a thunk that will a result in an
-- `ImpreciseException`. Right value is returned unmodified and unevaluated.
--
-- @since 1.0.0
raiseLeftImprecise :: (HasCallStack, GHC.Exception e) => Either e a -> a
raiseLeftImprecise =
  \case
    Left exc -> raiseImprecise exc
    Right res -> res
{-# INLINE raiseLeftImprecise #-}

-- | Similar to `GHC.throwTo`, except that it wraps any known non-async
-- exception with `GHC.SomeAsyncException`. This is necessary, because the
-- receiving thread gets the exception in an asynchronous manner and without
-- proper wrapping it will not be able to distinguish it from a regular
-- synchronous exception.
--
-- ====__Examples__
--
-- >>> import Primal.Concurrent
-- >>> import Primal.Exception
-- >>> (`raiseTo` DivideByZero) =<< forkFinally (threadDelay 100000) (either (print . isAsyncException) print)
-- True
-- >>> import qualified Control.Exception as GHC
-- >>> (`GHC.throwTo` DivideByZero) =<< forkFinally (threadDelay 100000) (either (print . isAsyncException) print)
-- False
--
-- @since 1.0.0
raiseTo :: (Primal RW m, GHC.Exception e) => GHC.ThreadId -> e -> m ()
raiseTo tid e =
  liftIO $
  if isAsyncException e
    then GHC.throwTo tid e
    else GHC.throwTo tid $ GHC.SomeAsyncException e
-- {-# INLINEABLE raiseTo #-}

-- | Behaves exactly as `GHC.catch`, except that it works in any `UnliftPrimal`
-- monad. It will catch an exception of any type, regardless how it was thrown
-- asynchonously or synchronously.
--
-- @since 0.3.0
catch ::
     forall e a m. (GHC.Exception e, UnliftPrimal RW m)
  => m a
  -> (e -> m a)
  -> m a
catch action handler =
  runInPrimalState2 (const action) handler $ \action# handler# ->
    let handler'# :: GHC.SomeException -> (State# RW -> (# State# RW, a #))
        handler'# someExc =
          case GHC.fromException someExc of
            Just exc -> handler# exc
            Nothing -> raiseIO# someExc
     in catch# (action# ()) handler'#
-- {-# INLINEABLE catch #-}
--{-# SPECIALIZE catch :: GHC.Exception e => IO a -> (e -> IO a) -> IO a #-}

-- | Same as `catch`, but allows to select which particular exception we care
-- about with the help of a modifying predicate.
--
-- @since 1.0.0
catchJust ::
     (UnliftPrimal RW m, GHC.Exception e)
  => (e -> Maybe b)
  -> m a
  -> (b -> m a)
  -> m a
catchJust p action handler =
  catchAll action $ \e ->
    case GHC.fromException e >>= p of
      Nothing -> raise e
      Just b -> handler b

-- | Catch a synchronous exception.
catchSync ::
     forall e a m. (GHC.Exception e, UnliftPrimal RW m)
  => m a
  -> (e -> m a)
  -> m a
catchSync action = catch action . syncHandler

-- | Catch an asynchronous exception.
catchAsync ::
     forall e a m. (GHC.Exception e, UnliftPrimal RW m)
  => m a
  -> (e -> m a)
  -> m a
catchAsync action = catch action . asyncHandler
--catchAsync = catchJust GHC.asyncExceptionFromException

syncHandler :: (GHC.Exception e, Primal s m) => (e -> m a) -> e -> m a
syncHandler handler exc =
  if isAsyncException exc
    then raise exc
    else handler exc

asyncHandler :: (GHC.Exception e, Primal s m) => (e -> m a) -> e -> m a
asyncHandler handler exc =
  if isAsyncException exc
    then handler exc
    else raise exc

-- | Catch all synchronous and asynchronous exceptions. Make sure to rethrow
-- asynchronous exceptions, since it is very rare that you want to recover from
-- exceptions originated from another thread.
--
-- @since 1.0.0
catchAll ::
     forall a m. UnliftPrimal RW m
  => m a
  -> (GHC.SomeException -> m a)
  -> m a
catchAll action handler =
  runInPrimalState2 (const action) handler $ \action# handler# ->
    catch# (action# ()) handler#
-- {-# INLINEABLE catchAll #-}
--{-# SPECIALIZE catchAll :: IO a -> (GHC.SomeException -> IO a) -> IO a #-}

-- | Catch all synchronous exceptions.
--
-- @since 1.0.0
catchAllSync ::
     forall a m. UnliftPrimal RW m
  => m a
  -> (GHC.SomeException -> m a)
  -> m a
catchAllSync action = catchAll action . syncHandler

-- | Catch all synchronous exceptions.
--
-- @since 1.0.0
catchAllAsync ::
     forall a m. UnliftPrimal RW m
  => m a
  -> (GHC.SomeException -> m a)
  -> m a
catchAllAsync action = catchAll action . asyncHandler


-- catchAny ::
--      forall a m. UnliftPrimal RW m
--   => m a
--   -> (forall e. GHC.Exception e => e -> m a)
--   -> m a
-- catchAny action handler =
--   runInPrimalState2
--     (const action)
--     (\(GHC.SomeException e) -> handler e)
--     (\action# handler# -> catch# (action# ()) handler#)
-- -- {-# INLINEABLE catchAny #-}

-- catchAnySync ::
--      forall a m. UnliftPrimal RW m
--   => m a
--   -> (forall e. GHC.Exception e => e -> m a)
--   -> m a
-- catchAnySync action handler =
--   catchAny action $ \exc ->
--     when (isAsyncException exc) (raise exc) >> handler exc
-- -- {-# INLINEABLE catchAnySync #-}


try :: (GHC.Exception e, UnliftPrimal RW m) => m a -> m (Either e a)
try f = catch (Right <$> f) (pure . Left)
-- {-# INLINEABLE try #-}
--{-# SPECIALIZE try :: GHC.Exception e => IO a -> IO (Either e a) #-}

trySync :: (GHC.Exception e, UnliftPrimal RW m) => m a -> m (Either e a)
trySync f = catchSync (Right <$> f) (pure . Left)

tryAsync :: (GHC.Exception e, UnliftPrimal RW m) => m a -> m (Either e a)
tryAsync f = catchAsync (Right <$> f) (pure . Left)

tryAll :: UnliftPrimal RW m => m a -> m (Either GHC.SomeException a)
tryAll f = catchAll (Right <$> f) (pure . Left)
-- {-# INLINEABLE tryAll #-}

tryAllSync :: UnliftPrimal RW m => m a -> m (Either GHC.SomeException a)
tryAllSync f = catchAllSync (Right <$> f) (pure . Left)

tryAllAsync :: UnliftPrimal RW m => m a -> m (Either GHC.SomeException a)
tryAllAsync f = catchAllAsync (Right <$> f) (pure . Left)


-- | Run an action, while invoking an exception handler when that action fails for some
-- reason. Exception handling function has async exceptions masked, but it is still
-- interruptible, which can be undesired in some scenarios. If you are sure that the
-- cleanup action does not deadlock and you do need hard guarantees that it gets executed
-- you can run it as uninterruptible:
--
-- > uninterruptibleMask $ \restore -> withException (restore action) handler
--
-- @since 0.3.0
withException ::
     (UnliftPrimal RW m, GHC.Exception e) => m a -> (e -> m b) -> m a
withException action handler =
  mask $ \restore -> do
    catch
      (restore action)
      (\exc -> catchAllSync (void $ handler exc) (\_ -> pure ()) >> raise exc)


-- | Same as `withException`, but will invoke exception handling function on all
-- exceptions.
--
-- @since 0.3.0
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
-- @since 0.3.0
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
-- @since 0.3.0
bracket_ :: UnliftPrimal RW m => m a -> m b -> m c -> m c
bracket_ acquire cleanup action = bracket acquire (const cleanup) (const action)


ubracket :: UnliftPrimal RW m => m a -> (a -> m b) -> (a -> m c) -> m c
ubracket acquire cleanup action =
  uninterruptibleMask $ \restore ->
    bracket (restore acquire) cleanup (restore . action)


--
-- @since 0.3.0
ubracket_ :: UnliftPrimal RW m => m a -> m b -> m c -> m c
ubracket_ acquire cleanup action = ubracket acquire (const cleanup) (const action)


ubracketOnError :: UnliftPrimal RW m => m a -> (a -> m b) -> (a -> m c) -> m c
ubracketOnError acquire cleanup action =
  uninterruptibleMask $ \restore ->
    bracketOnError (restore acquire) cleanup (restore . action)

ufinally :: UnliftPrimal RW m => m a -> m b -> m a
ufinally action cleanup =
  uninterruptibleMask $ \restore -> finally (restore action) cleanup


-- | Mask all asychronous exceptions, but keep it interruptible, unless the inherited state
-- was uninterruptible already, in which case this action has no affect. Same as
-- `Control.Exception.mask_`, except that it is polymorphic in state token. Inside a state
-- thread it cannot affect the result of computation, therefore it is safe to use it within
-- `ST` monad.
--
-- @since 0.3.0
mask_ :: forall a m s. UnliftPrimal s m => m a -> m a
mask_ action =
  unsafeIOToPrimal getMaskingState >>= \case
    GHC.Unmasked -> runInPrimalState action maskAsyncExceptionsInternal#
    _ -> action
{-# INLINEABLE mask_  #-}


maskPrimalState_ :: forall a n m s. (Primal s m, PrimalState s n) => n a -> m a
maskPrimalState_ action =
  unsafeIOToPrimal getMaskingState >>= \case
    GHC.Unmasked -> primal (maskAsyncExceptionsInternal# (primalState action))
    _ -> liftPrimalState action
{-# INLINEABLE maskPrimalState_  #-}

-- | Mask all asychronous exceptions, but keep it interruptible, unless the inherited state
-- was uninterruptible already, in which case this action has no affect. Same as
-- `Control.Exception.mask`, except that it is polymorphic in state token. Inside a state
-- thread it cannot affect the result of computation, therefore it is safe to use it within
-- `ST` monad.
--
-- @since 0.3.0
mask ::
     forall a m s. UnliftPrimal s m
  => ((forall b. m b -> m b) -> m a)
  -> m a
mask action = do
  unsafeIOToPrimal getMaskingState >>= \case
    GHC.Unmasked ->
      runInPrimalState
        (action (`runInPrimalState` unmaskAsyncExceptionsInternal#))
        maskAsyncExceptionsInternal#
    GHC.MaskedInterruptible ->
      action (`runInPrimalState` maskAsyncExceptionsInternal#)
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
     forall a m s. UnliftPrimal s m
  => ((forall b. m b -> m b) -> m a)
  -> m a
uninterruptibleMask action = do
  unsafeIOToPrimal getMaskingState >>= \case
    GHC.Unmasked ->
      runInPrimalState
        (action (`runInPrimalState` unmaskAsyncExceptionsInternal#))
        maskAsyncExceptionsInternal#
    GHC.MaskedInterruptible ->
      action (`runInPrimalState` maskAsyncExceptionsInternal#)
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
uninterruptibleMask_ :: forall a m s. UnliftPrimal s m => m a -> m a
uninterruptibleMask_ action = runInPrimalState action maskUninterruptibleInternal#
{-# INLINEABLE uninterruptibleMask_ #-}

uninterruptibleMaskPrimalState_ :: forall a n m s. (PrimalState s n, Primal s m) => n a -> m a
uninterruptibleMaskPrimalState_ action = primal (maskUninterruptibleInternal# (primalState action))
{-# INLINEABLE uninterruptibleMaskPrimalState_ #-}


-- | A direct wrapper around `maskAsyncExceptions#` primop. This is different and more
-- dangerous than `mask_` because it can turn uninterrubtable state into interruptable.
maskAsyncExceptions :: forall a m. UnliftPrimal RW m => m a -> m a
maskAsyncExceptions action = runInPrimalState action maskAsyncExceptions#
{-# INLINEABLE maskAsyncExceptions  #-}


-- | A direct wrapper around `unmaskAsyncExceptions#` primop.
unmaskAsyncExceptions :: forall a m. UnliftPrimal RW m => m a -> m a
unmaskAsyncExceptions action = runInPrimalState action unmaskAsyncExceptions#
{-# INLINEABLE unmaskAsyncExceptions  #-}


-- | A direct wrapper around `maskUninterruptible#` primop.
maskUninterruptible :: forall a m. UnliftPrimal RW m => m a -> m a
maskUninterruptible action = runInPrimalState action maskUninterruptible#
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

-- | Same as `GHC.getMaskingState`, but generalized to `Primal`
--
-- @since 0.3.0
getMaskingState :: Primal RW m => m GHC.MaskingState
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
