{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

-- |
-- Module      : Primal.Exception.Unsafe
-- Copyright   : (c) Alexey Kuleshevich 2021-2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module Primal.Exception.Unsafe
  ( maskAsyncExceptions
  , maskUninterruptible
  , unmaskAsyncExceptions
  , maskAsyncExceptionsInternal#
  , maskUninterruptibleInternal#
  , unmaskAsyncExceptionsInternal#
  , blockAsyncExceptions
  , blockUninterruptible
  , unblockAsyncExceptions
  , liftBlockAsyncExceptions
  , liftBlockUninterruptible
  , liftUnblockAsyncExceptions
  ) where

import GHC.Exts
import Primal.Monad.Internal

-- | Version of `maskAsyncExceptions` that works with any state token. Very unsafe.
blockAsyncExceptions :: UnliftPrimal s m => m b -> m b
blockAsyncExceptions action = runInPrimalState action maskAsyncExceptionsInternal#
{-# INLINE blockAsyncExceptions #-}

-- | Version of `maskAsyncExceptions` that works with any state token. Very unsafe.
liftBlockAsyncExceptions :: forall a n m s. (PrimalState s n, Primal s m) => n a -> m a
liftBlockAsyncExceptions action = primal (maskAsyncExceptionsInternal# (primalState action))
{-# INLINE liftBlockAsyncExceptions #-}

-- | Version of `maskUninterruptible` that works with any state token. Very unsafe.
blockUninterruptible :: UnliftPrimal s m => m b -> m b
blockUninterruptible action = runInPrimalState action maskUninterruptibleInternal#
{-# INLINE blockUninterruptible #-}

-- | Version of `blockUninterruptible` that works with different monads. Very unsafe.
liftBlockUninterruptible :: forall a n m s. (PrimalState s n, Primal s m) => n a -> m a
liftBlockUninterruptible action = primal (maskUninterruptibleInternal# (primalState action))
{-# INLINE liftBlockUninterruptible #-}

-- | Version of `unmaskAsyncExceptions` that works with any state token. Very unsafe.
unblockAsyncExceptions :: UnliftPrimal s m => m b -> m b
unblockAsyncExceptions action = runInPrimalState action unmaskAsyncExceptionsInternal#
{-# INLINE unblockAsyncExceptions #-}

-- | Version of `blockAsyncExceptions` that works with different monads. Very unsafe.
liftUnblockAsyncExceptions :: forall a n m s. (PrimalState s n, Primal s m) => n a -> m a
liftUnblockAsyncExceptions action = primal (unmaskAsyncExceptionsInternal# (primalState action))
{-# INLINE liftUnblockAsyncExceptions #-}

-- | A direct wrapper around `maskAsyncExceptions#` primop. This is different
-- and more dangerous than `mask_` because it ignores current masking state,
-- therefire it can turn uninterrubtable state into interruptable.
maskAsyncExceptions :: forall a m. UnliftPrimal RW m => m a -> m a
maskAsyncExceptions action = runInPrimalState action maskAsyncExceptions#
{-# INLINEABLE maskAsyncExceptions #-}

-- | A direct wrapper around `unmaskAsyncExceptions#` primop.
unmaskAsyncExceptions :: forall a m. UnliftPrimal RW m => m a -> m a
unmaskAsyncExceptions action = runInPrimalState action unmaskAsyncExceptions#
{-# INLINEABLE unmaskAsyncExceptions #-}

-- | A direct wrapper around `maskUninterruptible#` primop.
maskUninterruptible :: forall a m. UnliftPrimal RW m => m a -> m a
maskUninterruptible action = runInPrimalState action maskUninterruptible#
{-# INLINEABLE maskUninterruptible #-}

maskAsyncExceptionsInternal# :: (State# s -> (# State# s, a #)) -> State# s -> (# State# s, a #)
maskAsyncExceptionsInternal# = unsafeCoerce# maskAsyncExceptions#
{-# INLINEABLE maskAsyncExceptionsInternal# #-}

unmaskAsyncExceptionsInternal# :: (State# s -> (# State# s, b #)) -> State# s -> (# State# s, b #)
unmaskAsyncExceptionsInternal# = unsafeCoerce# unmaskAsyncExceptions#
{-# INLINEABLE unmaskAsyncExceptionsInternal# #-}

maskUninterruptibleInternal# :: (State# s -> (# State# s, a #)) -> State# s -> (# State# s, a #)
maskUninterruptibleInternal# = unsafeCoerce# maskUninterruptible#
{-# INLINEABLE maskUninterruptibleInternal# #-}
