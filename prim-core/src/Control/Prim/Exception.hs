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
  -- , withPrimBase
  -- , with#

import Control.Exception
import qualified GHC.Conc as GHC
import Control.Prim.Monad.Internal
import Control.Prim.Monad.Unsafe
import GHC.Exts



----- Exceptions

throwPrim :: (Exception e, MonadPrim s m) => e -> m a
throwPrim e = unsafeIOToPrim $ prim (raiseIO# (toException e))

catchPrim ::
     forall e a m. (Exception e, MonadUnliftPrim RW m)
  => m a
  -> (e -> m a)
  -> m a
catchPrim action handler =
  withRunInPrimBase $ \run ->
    let handler# :: SomeException -> (State# RW -> (# State# RW, a #))
        handler# e =
          case fromException e of
            Just e' -> primBase (run (handler e') :: IO a)
            Nothing -> raiseIO# e
     in prim (catch# (primBase (run action :: IO a)) handler#)

catchAnyPrim ::
     forall a m. (MonadUnliftPrim RW m)
  => m a
  -> (forall e . Exception e => e -> m a)
  -> m a
catchAnyPrim action handler =
  withRunInPrimBase $ \run ->
    let handler# :: SomeException -> (State# RW -> (# State# RW, a #))
        handler# (SomeException e) = primBase (run (handler e) :: IO a)
     in prim (catch# (primBase (run action :: IO a)) handler#)

catchAnySyncPrim ::
     forall a m. (MonadUnliftPrim RW m)
  => m a
  -> (forall e . Exception e => e -> m a)
  -> m a
catchAnySyncPrim action handler =
  withRunInPrimBase $ \run ->
    let handler# :: SomeException -> (State# RW -> (# State# RW, a #))
        handler# exc@(SomeException e) =
          case fromException exc of
            Just (SomeAsyncException _asyncExc) -> raiseIO# exc
            Nothing -> primBase (run (handler e) :: IO a)
     in prim (catch# (primBase (run action :: IO a)) handler#)

maskAsyncExceptionsPrim :: forall a m. MonadUnliftPrim RW m => m a -> m a
maskAsyncExceptionsPrim action =
  withRunInPrimBase $ \run -> prim (maskAsyncExceptions# (primBase (run action :: IO a)))

unmaskAsyncExceptionsPrim :: forall a m. MonadUnliftPrim RW m => m a -> m a
unmaskAsyncExceptionsPrim action =
  withRunInPrimBase $ \run -> prim (unmaskAsyncExceptions# (primBase (run action :: IO a)))

maskUninterruptiblePrim :: forall a m. MonadUnliftPrim RW m => m a -> m a
maskUninterruptiblePrim action =
  withRunInPrimBase $ \run -> prim (maskUninterruptible# (primBase (run action :: IO a)))


getMaskingStatePrim :: MonadPrim s m => m MaskingState
getMaskingStatePrim = unsafeIOToPrim getMaskingState


throwToPrim :: (MonadPrim RW m, Exception e) => GHC.ThreadId -> e -> m ()
throwToPrim tid = liftPrimBase . GHC.throwTo tid
