{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Control.Monad.Prim.Unsafe
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Control.Monad.Prim.Unsafe
  ( unsafePrimBase
  , unsafePrimBase_
  , unsafePrimBaseToPrim
  , unsafePrimBaseToIO
  , unsafePrimBaseToST
  , unsafeIOToPrim
  , unsafeSTToPrim
  , noDuplicatePrim
  , unsafeDupablePerformPrimBase
  -- * Inline
  , unsafeInlineIO
  , unsafeInlineST
  , unsafeInlinePrimBase
  -- * Interleave
  , unsafeInterleavePrimBase
  , unsafeDupableInterleavePrimBase
  -- * Re-exports
  , unsafePerformIO
  , unsafeDupablePerformIO
  , unsafeInterleaveIO
  , unsafeDupableInterleaveIO
  ) where

import System.IO.Unsafe
import Control.Monad.Prim.Internal
import Control.Monad.ST (ST)
import GHC.IO
import GHC.Exts

-- | Unwrap any `MonadPrimBase` action while coercing the state token
--
-- === Highly unsafe!
unsafePrimBase :: MonadPrimBase s' m => m a -> State# s -> (# State# s, a #)
unsafePrimBase m = unsafeCoerce# (primBase m)
{-# INLINE unsafePrimBase #-}

-- | Unwrap any `MonadPrimBase` action that does not return anything, while coercing the
-- state token
--
-- === Highly unsafe!
unsafePrimBase_ :: MonadPrimBase s' m => m () -> State# s -> State# s
unsafePrimBase_ m = unsafeCoerce# (primBase_ m)
{-# INLINE unsafePrimBase_ #-}

-- | Convert a `MonadPrimBase` action to another `MonadPrim` while coercing the state token.
--
-- === Highly unsafe!
unsafePrimBaseToPrim :: (MonadPrimBase sn n, MonadPrim sm m) => n a -> m a
unsafePrimBaseToPrim m = prim (unsafeCoerce# (primBase m))
{-# INLINE unsafePrimBaseToPrim #-}

-- | Convert a `MonadPrimBase` action to `ST` while coercing the state token @s@.
--
-- === Highly unsafe!
unsafePrimBaseToST :: MonadPrimBase sm m => m a -> ST s a
unsafePrimBaseToST = unsafePrimBaseToPrim
{-# INLINE unsafePrimBaseToST #-}

-- | Convert a `MonadPrimBase` action to `IO` while coercing the state token to `RealWorld`.
--
-- === Highly unsafe!
unsafePrimBaseToIO :: MonadPrimBase s m => m a -> IO a
unsafePrimBaseToIO = unsafePrimBaseToPrim
{-# INLINE unsafePrimBaseToIO #-}

-- | Convert an `IO` action to some `MonadPrim` while coercing the state token.
--
-- === Highly unsafe!
--
-- It is similar to `Control.Monad.ST.Unsafe.unsafeSTToIO`, except resulting action can be
-- any other `MonadPrim` action, therefore it is a lot more dangerous.
unsafeIOToPrim :: MonadPrim s m => IO a -> m a
unsafeIOToPrim = unsafePrimBaseToPrim
{-# INLINE unsafeIOToPrim #-}

-- | Convert an `ST` action to some `MonadPrim` while coercing the state token.
--
-- === Highly unsafe!
unsafeSTToPrim :: MonadPrim s m => ST s a -> m a
unsafeSTToPrim = unsafePrimBaseToPrim
{-# INLINE unsafeSTToPrim #-}

-- | Same as `GHC.IO.noDuplicate`, except works in any `MonadPrim`.
noDuplicatePrim :: MonadPrim s m => m ()
#if __GLASGOW_HASKELL__ >= 802
noDuplicatePrim = prim_ noDuplicate#
#else
noDuplicatePrim = unsafeIOToPrim $ prim_ noDuplicate#
#endif


-- | Same as `unsafeDupablePerformIO`, except works not only with `IO`, but with other
-- `MonadPrimBase` actions as well. Reading and writing values into memory is safe, as
-- long as writing action is idempotent. On the other hand things like memory or resource
-- allocation, exceptions handling are not safe at all, since supplied action can be run
-- multiple times and a copy interrupted at will.
unsafeDupablePerformPrimBase :: MonadPrimBase s m => m a -> a
unsafeDupablePerformPrimBase m = unsafeDupablePerformIO (unsafePrimBaseToIO m)

-- | Take an `IO` and compute it as a pure value, while inlining the action itself.
--
-- === Ridiculously unsafe!
--
-- This is even more unsafe then both `unsafePerformIO` and `unsafeDupableInterleaveIO`.
--
-- The only time it is really safe to use is on idempotent action that only read values
-- from memory, but do note do any mutation, allocation and certainly not interaction with
-- real world.
--
-- In
-- [`bytestring`](https://github.com/haskell/bytestring/blob/95fe6bdf13c9cc86c1c880164f7844d61d989574/Data/ByteString/Internal.hs#L566-L592)
-- it is known as `accursedUnutterablePerformIO`. Here are some resources that discuss
-- it's unsafety:
--
-- * [Stack overflow question](https://stackoverflow.com/questions/61021205/what-is-the-difference-between-unsafedupableperformio-and-accursedunutterableper)
-- * [Reddit discussion](https://www.reddit.com/r/haskell/comments/2cbgpz/flee_traveller_flee_or_you_will_be_corrupted_and/)
--
unsafeInlineIO :: IO a -> a
unsafeInlineIO (IO m) = case m realWorld# of (# _, r #) -> r
{-# INLINE unsafeInlineIO #-}

-- | Take an `ST` and compute it as a pure value, while inlining the action itself. Same
-- as `unsafeInlineIO`.
--
-- === Ridiculously unsafe!
unsafeInlineST :: ST s a -> a
unsafeInlineST = unsafeInlinePrimBase
{-# INLINE unsafeInlineST #-}


-- | Take any `MonadPrimBase` action and compute it as a pure value, while inlining the
-- action. Same as `unsafeInlineIO`, but applied to any `MonadPrimBase` action.
--
-- === Ridiculously unsafe!
unsafeInlinePrimBase :: MonadPrimBase s m => m a -> a
unsafeInlinePrimBase m = unsafeInlineIO (unsafePrimBaseToIO m)
{-# INLINE unsafeInlinePrimBase #-}


-- | Same as `unsafeInterleaveIO`, except works in any `MonadPrimBase`
unsafeInterleavePrimBase :: MonadPrimBase s m => m a -> m a
unsafeInterleavePrimBase x = unsafeDupableInterleavePrimBase (noDuplicatePrim >> x)
{-# INLINE unsafeInterleavePrimBase #-}


-- | Same as `unsafeDupableInterleaveIO`, except works in any `MonadPrimBase`
unsafeDupableInterleavePrimBase :: MonadPrimBase s m => m a -> m a
unsafeDupableInterleavePrimBase x =
  prim $ \s ->
    let r =
          case primBase x s of
            (# _, res #) -> res
     in (# s, r #)
{-# NOINLINE unsafeDupableInterleavePrimBase #-}
