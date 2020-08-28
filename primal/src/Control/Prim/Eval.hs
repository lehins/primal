{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Control.Prim.Eval
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Control.Prim.Eval
  ( module Control.Prim.Eval
  ) where

import Control.Prim.Monad.Internal
import Control.Prim.Monad.Unsafe
import GHC.Exts



------- Evaluation


-- | This is an action that ensures that the value is still available and garbage
-- collector has not cleaned it up.
--
-- Make sure not to use it after some computation that doesn't return, like after
-- `forever` for example, otherwise touch will simply be removed by ghc and bad things
-- will happen. If you have a case like that, make sure to use `withAlivePrimBase` or
-- `withAliveUnliftPrim` instead.
--
-- @since 0.1.0
touch :: MonadPrim s m => a -> m ()
touch x = unsafeIOToPrim $ prim_ (touch# x)
{-# INLINE touch #-}

-- | An action that evaluates a value to weak head normal form. Same
-- as `Control.Exception.evaluate`, except it works in a `MonadPrim`
--
-- @since 0.1.0
seqPrim :: MonadPrim s m => a -> m a
seqPrim a = prim (seq# a)


-- | Forward compatible operator that might be introduced in some future ghc version.
--
-- See: [!3131](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3131)
--
-- @since 0.1.0
keepAlive# ::
     a
  -- ^ the value to preserve
  -> (State# s -> (# State# s, r #))
  -- ^ the continuation in which the value will be preserved
  -> State# s
  -> (# State# s, r #)
keepAlive# a m s =
  case m s of
    (# s', r #) -> (# unsafeCoerce# (touch# a) s', r #)
{-# NOINLINE keepAlive# #-}

-- | Similar to `touch`. See `withAlive#` for more info.
--
-- @since 0.1.0
withAlivePrimBase :: (MonadPrimBase s n, MonadPrim s m) => a -> n b -> m b
withAlivePrimBase a m = prim (keepAlive# a (primBase m))
{-# INLINE withAlivePrimBase #-}

-- | Similar to `touch`. See `withAlive#` for more info.
--
-- @since 0.1.0
withAliveUnliftPrim :: MonadUnliftPrim s m => a -> m b -> m b
withAliveUnliftPrim a m = runInPrimBase m (keepAlive# a)
{-# INLINE withAliveUnliftPrim #-}
