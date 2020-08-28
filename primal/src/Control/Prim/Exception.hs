{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Control.Prim.Exception
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Control.Prim.Exception
  ( module Control.Prim.Exception
  ) where

import Control.Exception as GHC
import qualified GHC.Conc as GHC
import Control.Prim.Monad.Internal
import Control.Prim.Monad.Unsafe
import GHC.Exts



----- Exceptions

isSyncException :: Exception e => e -> Bool
isSyncException = not . isAsyncException

isAsyncException :: Exception e => e -> Bool
isAsyncException exc =
  case fromException (toException exc) of
    Just (SomeAsyncException _) -> True
    Nothing -> False


throwPrim :: (Exception e, MonadPrim s m) => e -> m a
throwPrim e = unsafeIOToPrim $ prim (raiseIO# (toException e))

catch ::
     forall e a m. (Exception e, MonadUnliftPrim RW m)
  => m a
  -> (e -> m a)
  -> m a
catch action handler =
  withRunInPrimBase $ \run ->
    let handler# :: SomeException -> (State# RW -> (# State# RW, a #))
        handler# e =
          case fromException e of
            Just e' -> primBase (run (handler e') :: IO a)
            Nothing -> raiseIO# e
     in prim (catch# (primBase (run action :: IO a)) handler#)

catchAny ::
     forall a m. MonadUnliftPrim RW m
  => m a
  -> (SomeException -> m a)
  -> m a
catchAny action handler =
  withRunInPrimBase $ \run ->
    let handler# :: SomeException -> (State# RW -> (# State# RW, a #))
        handler# exc = primBase (run (handler exc) :: IO a)
     in prim (catch# (primBase (run action :: IO a)) handler#)

catchAnySync ::
     forall a m. MonadUnliftPrim RW m
  => m a
  -> (SomeException -> m a)
  -> m a
catchAnySync action handler =
  withRunInPrimBase $ \run ->
    let handler# :: SomeException -> (State# RW -> (# State# RW, a #))
        handler# exc
          | isAsyncException exc = raiseIO# exc
          | otherwise = primBase (run (handler exc) :: IO a)
     in prim (catch# (primBase (run action :: IO a)) handler#)

catchAll ::
     forall a m. MonadUnliftPrim RW m
  => m a
  -> (forall e . Exception e => e -> m a)
  -> m a
catchAll action handler =
  withRunInPrimBase $ \run ->
    let handler# :: SomeException -> (State# RW -> (# State# RW, a #))
        handler# (SomeException e) = primBase (run (handler e) :: IO a)
     in prim (catch# (primBase (run action :: IO a)) handler#)

catchAllSync ::
     forall a m. MonadUnliftPrim RW m
  => m a
  -> (forall e . Exception e => e -> m a)
  -> m a
catchAllSync action handler =
  withRunInPrimBase $ \run ->
    let handler# :: SomeException -> (State# RW -> (# State# RW, a #))
        handler# exc@(SomeException e)
          | isAsyncException exc = raiseIO# exc
          | otherwise = primBase (run (handler e) :: IO a)
     in prim (catch# (primBase (run action :: IO a)) handler#)


maskAsyncExceptions :: forall a m. MonadUnliftPrim RW m => m a -> m a
maskAsyncExceptions action =
  withRunInPrimBase $ \run -> prim (maskAsyncExceptions# (primBase (run action :: IO a)))

unmaskAsyncExceptions :: forall a m. MonadUnliftPrim RW m => m a -> m a
unmaskAsyncExceptions action =
  withRunInPrimBase $ \run -> prim (unmaskAsyncExceptions# (primBase (run action :: IO a)))

maskUninterruptible :: forall a m. MonadUnliftPrim RW m => m a -> m a
maskUninterruptible action =
  withRunInPrimBase $ \run -> prim (maskUninterruptible# (primBase (run action :: IO a)))

-- | Same as `GHC.getMaskingState`, but generalized to `MonadPrim`
getMaskingState :: MonadPrim RW m => m MaskingState
getMaskingState = liftPrimBase GHC.getMaskingState

-- | Similar to @throwTo@ from
-- [unliftio](https://hackage.haskell.org/package/unliftio/docs/UnliftIO-Exception.html#v:throwTo)
-- this will wrap any known non-async exception with `SomeAsyncException`, because
-- otherwise semantics of `throwTo` with respect to asynchronous exceptions are violated.
throwTo :: (MonadPrim RW m, Exception e) => GHC.ThreadId -> e -> m ()
throwTo tid e =
  liftPrimBase $
  GHC.throwTo tid $
  if isAsyncException e
    then toException e
    else toException $ SomeAsyncException e
