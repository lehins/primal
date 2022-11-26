{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- |
-- Module      : Primal.Memory.Weak
-- Copyright   : (c) Alexey Kuleshevich 2020-2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module Primal.Memory.Weak
  ( Weak (..)
  , mkWeak -- TODO: validate pre ghc-8.2 mkWeak#
  , mkWeakNoFinalizer
  , mkWeakPtr
  , mkWeakPtrNoFinalizer
  , addFinalizer
  , addCFinalizer
  , addCFinalizerEnv
  , deRefWeak
  , finalizeWeak
  ) where

import Control.Monad
import qualified Foreign.ForeignPtr as GHC (FinalizerEnvPtr, FinalizerPtr)
import GHC.Weak (Weak (..))
import Primal.Foreign
import Primal.Monad

-- | Same as `System.Mem.Weak.mkWeak`, except it requires a finalizer to be
-- supplied. For a version without finalizers use `mkWeakNoFinalizer`
mkWeak :: UnliftPrimal RW m => k -> v -> m b -> m (Weak v)
mkWeak key val finalizer =
  runInPrimalState finalizer $ \f s ->
    case mkWeak# key val f s of
      (# s', w #) -> (# s', Weak w #)

-- | Similar to `mkWeak`, except it does not require a finalizer.
mkWeakNoFinalizer :: Primal RW m => k -> v -> m (Weak v)
mkWeakNoFinalizer key val =
  primal $ \s ->
    case mkWeakNoFinalizer# key val s of
      (# s', w #) -> (# s', Weak w #)

-- | Same as `System.Mem.Weak.mkWeakPtr`, except it requires a finalizer to be
-- supplied. For a version without finalizers use `mkWeakPtrNoFinalizer`
mkWeakPtr :: UnliftPrimal RW m => k -> m b -> m (Weak k)
mkWeakPtr key = mkWeak key key

-- | Similar to `mkWeakPtr`, except it does not require a finalizer.
mkWeakPtrNoFinalizer :: Primal RW m => k -> m (Weak k)
mkWeakPtrNoFinalizer key = mkWeakNoFinalizer key key

-- | Same as `System.Mem.Weak.addFinalizer`.
addFinalizer :: UnliftPrimal RW m => k -> m b -> m ()
addFinalizer key = void . mkWeakPtr key

-- | Add a foreign function finalizer with a single argument. Returns `True` on success or
-- `False` when weak pointer is already dead.
addCFinalizer
  :: Primal RW m
  => GHC.FinalizerPtr a
  -- ^ Pointer to the C function to be called when finalizers are being invoked
  -> Ptr a
  -- ^ Argument that will be supplied to the finalizer function
  -> Weak v
  -> m Bool
addCFinalizer (FunPtr faddr#) (Ptr addr#) (Weak weak#) =
  primal $ \s ->
    case addCFinalizerToWeak# faddr# addr# 0# nullAddr# weak# s of
      (# s', i# #) -> (# s', isTrue# i# #)

-- | Add a foreign function finalizer with two arguments. Returns `True` on success or
-- `False` when weak pointer is already dead.
addCFinalizerEnv
  :: Primal RW m
  => GHC.FinalizerEnvPtr env a
  -- ^ Pointer to the C function to be called when finalizers are being invoked
  -> Ptr env
  -- ^ First argument that will be supplied to the finalizer function
  -> Ptr a
  -- ^ Second argument that will be supplied to the finalizer function
  -> Weak v
  -> m Bool
addCFinalizerEnv (FunPtr faddr#) (Ptr envAddr#) (Ptr addr#) (Weak weak#) =
  primal $ \s ->
    case addCFinalizerToWeak# faddr# addr# 1# envAddr# weak# s of
      (# s', i# #) -> (# s', isTrue# i# #)

-- | Similar to `System.Mem.Weak.deRefWeak`
deRefWeak :: Primal RW m => Weak v -> m (Maybe v)
deRefWeak (Weak weak#) =
  primal $ \s ->
    case deRefWeak# weak# s of
      (# s', 0#, _ #) -> (# s', Nothing #)
      (# s', _, a #) -> (# s', Just a #)

-- | Runs associated finalizer, same as `System.Mem.Weak.finalize`
finalizeWeak :: Primal RW m => Weak v -> m ()
finalizeWeak (Weak w) =
  primal $ \s ->
    case finalizeWeak# w s of
      (# s1, 0#, _ #) -> (# s1, () #)
      (# s1, _, f #) -> f s1
