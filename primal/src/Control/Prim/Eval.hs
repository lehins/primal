{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
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
  , module Control.DeepSeq
  ) where

import Control.DeepSeq
import Control.Prim.Monad.Internal
import qualified GHC.Exts as GHC


-- | Same as `GHC.Exts.touch#`, except it is not restricted to `RealWorld` state token.
touch# :: a -> GHC.State# s -> GHC.State# s
touch# a = GHC.unsafeCoerce# (GHC.touch# a)
{-# INLINE touch# #-}


------- Evaluation


-- | This is an action that ensures that the value is still available and garbage
-- collector has not cleaned it up.
--
-- Make sure not to use it after some computation that doesn't return, like after
-- `forever` for example, otherwise touch will simply be removed by ghc and bad things
-- will happen. If you have a case like that, make sure to use `withAlivePrimBase` or
-- `keepAlive` instead.
--
-- @since 0.1.0
touch :: MonadPrim s m => a -> m ()
touch x = prim_ (touch# x)
{-# INLINE touch #-}

-- | An action that evaluates a value to weak head normal form. Same as
-- `Control.Exception.evaluate`, except it works in a `MonadPrim`. This provides sightly
-- better guarantees than `seq` with respect to ordering of operations, but it has higher
-- overhead.
--
-- @since 0.3.0
evaluate :: MonadPrim s m => a -> m a
evaluate a = prim (GHC.seq# a)
{-# INLINE evaluate #-}


-- | Forward compatible operator that might be introduced in some future ghc version.
--
-- See: [#17760](https://gitlab.haskell.org/ghc/ghc/-/issues/17760)
--
-- Current version is not as efficient as the version that will be introduced in the
-- future, because it works around the ghc bug by simply preventing inlining and relying
-- on the `touch` function.
--
-- @since 0.1.0
keepAlive# ::
     a
  -- ^ The value to preserve
  -> (GHC.State# s -> (# GHC.State# s, r #))
  -- ^ The continuation in which the value will be preserved
  -> GHC.State# s
  -> (# GHC.State# s, r #)
keepAlive# a m s =
  case m s of
    (# s', r #) -> (# touch# a s', r #)
{-# NOINLINE keepAlive# #-}


-- | Similar to `touch`. See `withAlive#` for more info.
--
-- @since 0.3.0
keepAlive ::
     MonadUnliftPrim s m
  => a
  -- ^ The value to preserve
  -> m b
  -- ^ Action to run in which the value will be preserved
  -> m b
keepAlive a m = runInPrimBase m (keepAlive# a)
{-# INLINE keepAlive #-}
