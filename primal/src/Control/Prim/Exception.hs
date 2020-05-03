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

import Control.Exception as GHC
import qualified GHC.Conc as GHC
import Control.Prim.Monad.Internal
import Control.Prim.Monad.Unsafe
import GHC.Exts



----- Exceptions

throw :: (Exception e, MonadPrim s m) => e -> m a
throw e = unsafeIOToPrim $ prim (raiseIO# (toException e))

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
     forall a m. (MonadUnliftPrim RW m)
  => m a
  -> (forall e . Exception e => e -> m a)
  -> m a
catchAny action handler =
  withRunInPrimBase $ \run ->
    let handler# :: SomeException -> (State# RW -> (# State# RW, a #))
        handler# (SomeException e) = primBase (run (handler e) :: IO a)
     in prim (catch# (primBase (run action :: IO a)) handler#)

catchAnySync ::
     forall a m. (MonadUnliftPrim RW m)
  => m a
  -> (forall e . Exception e => e -> m a)
  -> m a
catchAnySync action handler =
  withRunInPrimBase $ \run ->
    let handler# :: SomeException -> (State# RW -> (# State# RW, a #))
        handler# exc@(SomeException e) =
          case fromException exc of
            Just (SomeAsyncException _asyncExc) -> raiseIO# exc
            Nothing -> primBase (run (handler e) :: IO a)
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


getMaskingState :: MonadPrim RW m => m MaskingState
getMaskingState = liftPrimBase GHC.getMaskingState


throwTo :: (MonadPrim RW m, Exception e) => GHC.ThreadId -> e -> m ()
throwTo tid = liftPrimBase . GHC.throwTo tid
