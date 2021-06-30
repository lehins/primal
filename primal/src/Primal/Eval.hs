{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Primal.Eval
-- Copyright   : (c) Alexey Kuleshevich 2020-2021
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
  , evalMaybe
    -- * Normal Form
  , deepeval
  , deepevalM
  , deepevalMaybe
  , rnfMut
  , MutNFData(..)
  , BNF(..)
  , module Control.DeepSeq
  -- * References
  --
  -- $references
  --
  ) where

import Control.DeepSeq
import Primal.Monad.Internal
import Primal.Monad.Unsafe
import qualified GHC.Exts as GHC
import Primal.Exception


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
-- will happen. If you have a case like that, make sure to use `withAlivePrimalState` or
-- `keepAlive` instead.
--
-- @since 0.1.0
touch :: Primal s m => a -> m ()
touch x = primal_ (touch# x)
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
#if __GLASGOW_HASKELL >= 900
keepAlive# = unsafeCoerce# keepAlive#
{-# INLINE keepAlive# #-}
#else
keepAlive# a m s =
  case m s of
    (# s', r #) -> (# touch# a s', r #)
{-# NOINLINE keepAlive# #-}
#endif

-- | Similar to `touch`. See `withAlive#` for more info.
--
-- @since 0.3.0
keepAlive ::
     UnliftPrimal s m
  => a
  -- ^ The value to preserve
  -> m b
  -- ^ Action to run in which the value will be preserved
  -> m b
keepAlive a m = runInPrimalState m (keepAlive# a)
{-# INLINE keepAlive #-}



-- | An action that evaluates a value to Weak Head Normal Form (WHNF). Same as
-- `Control.Exception.evaluate`, except it works in `Primal`. This function provides
-- stronger guarantees than `seq` with respect to ordering of operations, but it does have a
-- slightly higher overhead.
--
-- @since 0.3.0
eval :: Primal s m => a -> m a
eval a = primal (GHC.seq# a)
{-# INLINE eval #-}

-- | Run the action and then use `eval` to ensure its result is evaluated to Weak Head
-- Normal Form (WHNF)
--
-- @since 0.3.0
evalM :: Primal s m => m a -> m a
evalM = (>>= eval)
{-# INLINE evalM #-}


-- | Evalute the supplied value to Weak-Head Normal Form and fail with `Nothing`
-- if an exception was raised.
--
-- __/Note/__ - Supplied argument should not contain non-terminating
-- computation, because order of evaluation is not guaranteed with GHC and in
-- presense of exceptions this function might loop non-deterministically. See
-- ["A semantics for imprecise exceptions" [1\]](#ref_1) for more info.
--
-- ===__Examples__
--
-- In case of successful evaluation we get back Just value
--
-- >>> evalMaybe (4 `div` 2 :: Integer)
-- Just 2
--
-- Reason why we don't have a @evalEither@ function that would also return the
-- exact exception that was raised is because the order of evaluation in GHC is
-- not guaranteed in pure functions. Therefore in example below we can't know
-- ahead of time which exception will be raised first: `undefined` or
-- `DivideByZero`, so we return `Nothing` instead.
--
-- >>> evalMaybe (unsafeined `div` 0 :: Integer)
-- Nothing
--
-- Note that exceptions might lurk deeper, so for more complex types it is best
-- to use `deepevalMaybe` instead.
--
-- >>> evalMaybe ("Partial tuple", 5 `div` 0 :: Integer)
-- Just ("Partial tuple",*** Exception: divide by zero
--
-- @since 1.0.0
evalMaybe :: a -> Maybe a
evalMaybe e = unsafeInlineIO $ do
  tryAnySync (eval e) >>= \case
    Left _ -> pure Nothing
    Right val -> pure $ Just val
{-# INLINE evalMaybe #-}


-- Normal Form


-- | An action that evaluates a value to Normal Form (NF). This function provides stronger
-- guarantees than `deepseq` with respect to ordering of operations.
--
-- @since 0.3.0
deepeval :: (Primal s m, NFData a) => a -> m a
deepeval = eval . force
{-# INLINE deepeval #-}

-- | Run the action and the using `deepeval` ensure its result is evaluated to Normal Form
-- (NF)
--
-- @since 0.3.0
deepevalM :: (Primal s m, NFData a) => m a -> m a
deepevalM m = eval . force =<< m
{-# INLINE deepevalM #-}


-- | Same as `evalMaybe`, except evalute the value to Normal Form and fail with
-- `Nothing` if at any point during evaluation an exception was raised.
--
-- /Note/ - Supplied argument should not contain non-terminating computation,
-- because order of evaluation is not guaranteed with GHC and in presense of
-- exceptions this function might loop non-deterministically. See ["A semantics
-- for imprecise exceptions" [1\]](#ref_1) for more info.
--
-- ===__Examples__
--
-- >>> deepevalMaybe ("Partial tuple", 4 `div` 0 :: Integer)
-- Nothing
-- >>> deepevalMaybe ("Total tuple", 4 `div` 2 :: Integer)
-- Just ("Total tuple",2)
--
-- @since 1.0.0
deepevalMaybe :: NFData a => a -> Maybe a
deepevalMaybe e =
  unsafeInlineIO $ do
    tryAnySync (deepeval e) >>= \case
      Left _ -> pure Nothing
      Right val -> pure $ Just val
{-# INLINE deepevalMaybe #-}


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
rnfMut :: (MutNFData mut, Primal s m) => mut s -> m ()
rnfMut = liftST . rnfMutST
{-# INLINE rnfMut #-}

-- $references
--
-- #ref_1#
--
--   [[1\]] __/A semantics for imprecise exceptions/__ - Simon Peyton Jones, Alastair Reid, Fergus Henderson, Tony Hoare, Simon Marlow - PLDI '99: Proceedings of the ACM SIGPLAN 1999 conference on Programming language design and implementation - May 1999 - Pages 25–36 https://doi.org/10.1145/301618.301637
