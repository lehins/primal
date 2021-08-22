{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Primal.Monad.Unsafe
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Monad.Unsafe
  ( unsafePrimal
  , unsafePrimal_
  , unsafePrimalState
  , unsafePrimalState_
  , unsafePrimalStateToPrimal
  , unsafePrimalStateToIO
  , unsafePrimalStateToST
  , unsafeIOToST
  , unsafeIOToPrimal
  , unsafeSTToPrimal
  , unsafeLiftPrimalState
  , noDuplicate
  , unsafeDupablePerformPrimalState
  -- * Inline
  , unsafeInlineIO
  , unsafeInlineST
  , unsafeInlinePrimalState
  -- * Interleave
  , unsafeInterleavePrimalState
  , unsafeDupableInterleavePrimalState
  -- * Re-exports
  , unsafePerformIO
  , unsafeDupablePerformIO
  , unsafeInterleaveIO
  , unsafeDupableInterleaveIO
  ) where

import System.IO.Unsafe
import Primal.Monad.Internal
import GHC.IO hiding (noDuplicate)
import GHC.Exts

-- | Coerce the state token of primal operation and wrap it into a `Primal` action.
--
-- === Highly unsafe!
--
-- @since 0.3.0
unsafePrimal :: Primal s m => (State# s' -> (# State# s', a #)) -> m a
unsafePrimal m = primal (unsafeCoerce# m)
{-# INLINE unsafePrimal #-}


-- | Coerce the state token of primal operation and wrap it into a `Primal` action.
--
-- === Highly unsafe!
--
-- @since 0.3.0
unsafePrimal_ :: Primal s m => (State# s' -> State# s') -> m ()
unsafePrimal_ m = primal_ (unsafeCoerce# m)
{-# INLINE unsafePrimal_ #-}


-- | Unwrap any `PrimalState` action while coercing the state token
--
-- === Highly unsafe!
unsafePrimalState :: PrimalState s' m => m a -> State# s -> (# State# s, a #)
unsafePrimalState m = unsafeCoerce# (primalState m)
{-# INLINE unsafePrimalState #-}

-- | Unwrap any `PrimalState` action that does not return anything, while coercing the
-- state token
--
-- === Highly unsafe!
unsafePrimalState_ :: PrimalState s' m => m () -> State# s -> State# s
unsafePrimalState_ m = unsafeCoerce# (primalState_ m)
{-# INLINE unsafePrimalState_ #-}

-- | Convert a `PrimalState` action to another `Primal` while coercing the state token.
--
-- === Highly unsafe!
unsafePrimalStateToPrimal :: (PrimalState sn n, Primal sm m) => n a -> m a
unsafePrimalStateToPrimal m = primal (unsafeCoerce# (primalState m))
{-# INLINE unsafePrimalStateToPrimal #-}

-- | Convert a `PrimalState` action to `ST` while coercing the state token @s@.
--
-- === Highly unsafe!
unsafePrimalStateToST :: PrimalState sm m => m a -> ST s a
unsafePrimalStateToST = unsafePrimalStateToPrimal
{-# INLINE unsafePrimalStateToST #-}

-- | Convert a `PrimalState` action to `IO` while coercing the state token to `RealWorld`.
--
-- === Highly unsafe!
unsafePrimalStateToIO :: PrimalState s m => m a -> IO a
unsafePrimalStateToIO = unsafePrimalStateToPrimal
{-# INLINE unsafePrimalStateToIO #-}

-- | Convert an `IO` action to some `Primal` while coercing the state token.
--
-- === Highly unsafe!
--
-- It is similar to `Control.Monad.ST.Unsafe.unsafeSTToIO`, except resulting action can be
-- any other `Primal` action, therefore it is a lot more dangerous.
unsafeIOToPrimal :: Primal s m => IO a -> m a
unsafeIOToPrimal = unsafePrimalStateToPrimal
{-# INLINE unsafeIOToPrimal #-}

-- | Convert an `ST` action to some `Primal` while coercing the state token.
--
-- === Highly unsafe!
unsafeSTToPrimal :: Primal s' m => ST s a -> m a
unsafeSTToPrimal = unsafePrimalStateToPrimal
{-# INLINE unsafeSTToPrimal #-}

-- | Same as `GHC.IO.noDuplicate`, but it also works in any `Primal` monad.
noDuplicate :: Primal s m => m ()
#if __GLASGOW_HASKELL__ >= 802
noDuplicate = primal_ noDuplicate#
#else
noDuplicate = unsafeIOToPrimal $ primal_ noDuplicate#
#endif


-- | Same as `unsafeDupablePerformIO`, except works not only with `IO`, but with other
-- `PrimalState` actions as well. Reading and writing values into memory is safe, as
-- long as writing action is idempotent. On the other hand things like memory or resource
-- allocation, exceptions handling are not safe at all, since supplied action can be run
-- multiple times and a copy interrupted at will.
unsafeDupablePerformPrimalState :: PrimalState s m => m a -> a
unsafeDupablePerformPrimalState m = unsafeDupablePerformIO (unsafePrimalStateToIO m)

-- | Take an `IO` and compute it as a pure value, while inlining the action itself.
--
-- === Ridiculously unsafe!
--
-- This is even more unsafe then both `unsafePerformIO` and `unsafeDupableInterleaveIO`.
--
-- It is only safe to use on idempotent actions that only read values from memory, but do
-- not do any mutation, allocation nor any other interactions with the real world.
--
-- In
-- [`bytestring`](https://github.com/haskell/bytestring/blob/95fe6bdf13c9cc86c1c880164f7844d61d989574/Data/ByteString/Internal.hs#L566-L592)
-- it is known as `accursedUnutterablePerformIO`. Here are some resources that discuss
-- its unsafety:
--
-- * [Stack overflow question](https://stackoverflow.com/questions/61021205/what-is-the-difference-between-unsafedupableperformio-and-accursedunutterableper)
-- * [Reddit discussion](https://www.reddit.com/r/haskell/comments/2cbgpz/flee_traveller_flee_or_you_will_be_corrupted_and/)
--
unsafeInlineIO :: IO a -> a
unsafeInlineIO (IO m) = case m realWorld# of (# _, r #) -> r
{-# INLINE unsafeInlineIO #-}

-- | Take an `ST` and compute it as a pure value, while inlining the action itself. Just as unsafe
-- as `unsafeInlineIO`.
--
-- === Ridiculously unsafe!
unsafeInlineST :: ST s a -> a
unsafeInlineST = unsafeInlinePrimalState
{-# INLINE unsafeInlineST #-}


-- | Take any `PrimalState` action and compute it as a pure value, while inlining the
-- action. Same as `unsafeInlineIO`, but applied to any `PrimalState` action.
--
-- === Ridiculously unsafe!
unsafeInlinePrimalState :: PrimalState s m => m a -> a
unsafeInlinePrimalState m = unsafeInlineIO (unsafePrimalStateToIO m)
{-# INLINE unsafeInlinePrimalState #-}


-- | Same as `unsafeInterleaveIO`, except works in any `PrimalState`
unsafeInterleavePrimalState :: PrimalState s m => m a -> m a
unsafeInterleavePrimalState x = unsafeDupableInterleavePrimalState (noDuplicate >> x)
{-# INLINE unsafeInterleavePrimalState #-}


-- | Same as `unsafeDupableInterleaveIO`, except works in any `PrimalState`
unsafeDupableInterleavePrimalState :: PrimalState s m => m a -> m a
unsafeDupableInterleavePrimalState x =
  primal $ \s ->
    let r = case primalState x s of
              (# _, res #) -> res
     in (# s, r #)
{-# NOINLINE unsafeDupableInterleavePrimalState #-}

-- | A version of `liftPrimalState` that coerce the state token.
--
-- === Highly unsafe!
--
unsafeLiftPrimalState :: forall sn n sm m a. (PrimalState sn n, Primal sm m) => n a -> m a
unsafeLiftPrimalState m = primal (unsafePrimalState m)
{-# INLINE unsafeLiftPrimalState #-}
