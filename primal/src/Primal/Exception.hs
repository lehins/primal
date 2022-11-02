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
-- Copyright   : (c) Alexey Kuleshevich 2020-2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Exception
  (
  -- * Raising
    module Primal.Monad.Raises
  , raise
  , raiseTo
  , StringException(..)
  , raiseString
  , raiseImprecise
  , raiseImpreciseNoCallStack
  , raiseLeftImprecise
  , ImpreciseException(..)
  , errorWithoutStackTrace
  -- * Catching
  , catch
  , catchJust
  , catchSync
  , catchAsync
  , catchAny
  , catchAnySync
  , catchAnyAsync
  , catchAll
  , catchAllSync
  , catchAllAsync
  , try
  , tryJust
  , trySync
  , tryAsync
  , tryAll
  , tryAllSync
  , tryAllAsync
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
import Control.Monad (when)
import Primal.Monad.Internal
import Primal.Monad.Raises
import Primal.Monad.Unsafe
import qualified GHC.Conc as GHC
import GHC.Exts
import GHC.Stack
#if !MIN_VERSION_base(4,9,0)
import Data.List (intercalate)
import GHC.SrcLoc
#endif


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
{-# INLINEABLE raise #-}

newtype StringException = StringException String
  deriving (Eq, Show)

instance GHC.Exception StringException where
  displayException (StringException exc) = "StringException: " ++ exc

-- | Raise a string exception. Very useful when recovery from an exception does not make
-- much sense, but a message with failure still needs to be conveyed. Impossible cases
-- are a good example of this.
raiseString :: forall a m s. Primal s m => String -> m a
raiseString = raise . StringException

data ImpreciseException =
  ImpreciseException
    { impreciseException :: GHC.SomeException
    , impreciseCallStack :: Maybe CallStack
    }

instance Show ImpreciseException where
  showsPrec _ ImpreciseException {impreciseException, impreciseCallStack} =
    ("ImpreciseException '" ++) .
    (GHC.displayException impreciseException ++) .
    case impreciseCallStack of
      Nothing -> ("' was raised" ++)
      Just cs -> ("' was raised at:\n" ++) . (prettyCallStack cs ++)

instance GHC.Exception ImpreciseException


-- | Create a thunk, which, upon evaluation to WHNF, will cause an exception
-- being raised. The actual exception that will be thrown is the
-- `ImpreciseException`, which will contain the supplied exception as well as the
-- callstack.
--
-- @since 1.0.0
raiseImprecise :: (HasCallStack, GHC.Exception e) => e -> a
raiseImprecise e =
  raise# (GHC.toException (ImpreciseException (GHC.toException e) (Just ?callStack)))


-- | Just like `raiseImprecise`, creates a thunk, which, upon evaluation to WHNF, will
-- cause an exception being raised. The actual exception that will be thrown is the
-- `ImpreciseException`, which will contain the supplied exception. However, unlike
-- `raiseImprecise` the callstack is not included.
--
-- @since 1.0.0
raiseImpreciseNoCallStack :: GHC.Exception e => e -> a
raiseImpreciseNoCallStack e =
  raise# (GHC.toException (ImpreciseException (GHC.toException e) Nothing))


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
{-# INLINEABLE raiseTo #-}

-- | Behaves exactly as `GHC.catch`, except that it works in any `UnliftPrimal`
-- monad. It will catch an exception of any type, regardless how it was thrown
-- asynchonously or synchronously.
--
-- @since 1.0.0
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
{-# INLINE catch #-}


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
{-# INLINE catchSync #-}

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
{-# INLINEABLE catchAll #-}
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


catchAny ::
     forall a m. UnliftPrimal RW m
  => m a
  -> (forall e. GHC.Exception e => e -> m a)
  -> m a
catchAny action handler =
  runInPrimalState2
    (const action)
    (\(GHC.SomeException e) -> handler e)
    (\action# handler# -> catch# (action# ()) handler#)
{-# INLINEABLE catchAny #-}

catchAnySync ::
     forall a m. UnliftPrimal RW m
  => m a
  -> (forall e. GHC.Exception e => e -> m a)
  -> m a
catchAnySync action handler =
  catchAny action $ \exc ->
    when (isAsyncException exc) (raise exc) >> handler exc
{-# INLINEABLE catchAnySync #-}


catchAnyAsync ::
     forall a m. UnliftPrimal RW m
  => m a
  -> (forall e. GHC.Exception e => e -> m a)
  -> m a
catchAnyAsync action handler =
  catchAny action $ \exc ->
    when (isSyncException exc) (raise exc) >> handler exc
{-# INLINEABLE catchAnyAsync #-}


try :: (GHC.Exception e, UnliftPrimal RW m) => m a -> m (Either e a)
try f = catch (Right <$> f) (pure . Left)
{-# INLINEABLE try #-}
--{-# SPECIALIZE try :: GHC.Exception e => IO a -> IO (Either e a) #-}

tryJust :: (GHC.Exception e, UnliftPrimal RW m) => (e -> Maybe b) -> m a -> m (Either b a)
tryJust g f = catchJust g (Right <$> f) (pure . Left)
{-# INLINEABLE tryJust #-}


trySync :: (GHC.Exception e, UnliftPrimal RW m) => m a -> m (Either e a)
trySync f = catchSync (Right <$> f) (pure . Left)
{-# INLINE trySync #-}

tryAsync :: (GHC.Exception e, UnliftPrimal RW m) => m a -> m (Either e a)
tryAsync f = catchAsync (Right <$> f) (pure . Left)

tryAll :: UnliftPrimal RW m => m a -> m (Either GHC.SomeException a)
tryAll f = catchAll (Right <$> f) (pure . Left)
{-# INLINEABLE tryAll #-}

tryAllSync :: UnliftPrimal RW m => m a -> m (Either GHC.SomeException a)
tryAllSync f = catchAllSync (Right <$> f) (pure . Left)
{-# INLINEABLE tryAllSync #-}

tryAllAsync :: UnliftPrimal RW m => m a -> m (Either GHC.SomeException a)
tryAllAsync f = catchAllAsync (Right <$> f) (pure . Left)


-- | Same as `GHC.getMaskingState`, but generalized to `Primal`
--
-- @since 1.0.0
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

#if __GLASGOW_HASKELL__ < 800
errorWithoutStackTrace :: String -> a
errorWithoutStackTrace = error
#endif
