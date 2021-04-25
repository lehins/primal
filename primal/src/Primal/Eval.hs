{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Primal.Eval
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Eval
  ( -- * Liveness
    touch
  , touch#
  , keepAlive
  , keepAlive#
    -- * Weak-Head Normal Form
  , seq
  , eval
  , evalM
    -- * Normal Form
  , deepeval
  , deepevalM
  , module Control.DeepSeq
  , rnfMut
  , MutNFData(..)
  , BNF(..)
  ) where

import Control.DeepSeq
import Primal.Monad.Internal
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



-- | An action that evaluates a value to Weak Head Normal Form (WHNF). Same as
-- `Control.Exception.evaluate`, except it works in `MonadPrim`. This function provides
-- stronger guarantees than `seq` with respect to ordering of operations, but it does have a
-- slightly higher overhead.
--
-- @since 0.3.0
eval :: MonadPrim s m => a -> m a
eval a = prim (GHC.seq# a)
{-# INLINE eval #-}

-- | Run the action and then use `eval` to ensure its result is evaluated to Weak Head
-- Normal Form (WHNF)
--
-- @since 0.3.0
evalM :: MonadPrim s m => m a -> m a
evalM m = eval =<< m
{-# INLINE evalM #-}


-- Normal Form


-- | An action that evaluates a value to Normal Form (NF). This function provides stronger
-- guarantees than `deepseq` with respect to ordering of operations.
--
-- @since 0.3.0
deepeval :: (MonadPrim s m, NFData a) => a -> m a
deepeval = eval . force
{-# INLINE deepeval #-}

-- | Run the action and the using `deepeval` ensure its result is evaluated to Normal Form
-- (NF)
--
-- @since 0.3.0
deepevalM :: (MonadPrim s m, NFData a) => m a -> m a
deepevalM m = eval . force =<< m
{-# INLINE deepevalM #-}


-- | Bogus Normal Form. This is useful in places where `NFData` constraint is required,
-- but an instance can't really be created in any meaningful way for the type at
-- hand. Creating environment in benchmarks is one such place where it may come in handy.
--
-- @since 0.3.0
newtype BNF a = BNF a

-- | Unlawful instance that only evaluates its contents to WHNF
--
-- @since 0.3.0
instance NFData (BNF a) where
  rnf (BNF a) = a `seq` ()

-- | Same as `NFData`, but for mutable data types
class MutNFData mut where
  rnfMutST :: mut s -> ST s ()

-- | Force the mutable type to Normal Form
rnfMut :: (MutNFData mut, MonadPrim s m) => mut s -> m ()
rnfMut = liftST . rnfMutST
{-# INLINE rnfMut #-}
