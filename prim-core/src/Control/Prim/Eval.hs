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
-- will happen. If you have a case like that, make sure to use `withPrimBase` instead.
touch :: MonadPrim s m => a -> m ()
touch x = unsafeIOToPrim $ prim_ (touch# x)
{-# INLINE touch #-}

-- | An action that evaluates a value to weak head normal form. Same
-- as `Control.Exception.evaluate`, except it work in a `MonadPrim`
evaluate :: MonadPrim s m => a -> m a
evaluate a = prim (seq# a)


-- | Forward compatible operator that will be introduced in some future ghc version.
--
-- See: [!2961](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/2961)
with# ::
     a
  -> (State# s -> (# State# s, b #))
  -> State# s
  -> (# State# s#, b #)
with# a m s# =
  case m s# of
    (# s'#, r #) -> (# unsafeCoerce# (touch# a) s'#, r #)
{-# NOINLINE with# #-}


withPrimBase :: (MonadPrimBase s n, MonadPrim s m) => a -> n b -> m b
withPrimBase a m = prim (with# a (primBase m))
{-# INLINE withPrimBase #-}
