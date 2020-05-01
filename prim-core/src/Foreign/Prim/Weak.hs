{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Foreign.Prim.Weak
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Foreign.Prim.Weak
  ( Weak(..)
  , mkWeak
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
import Control.Prim.Monad
import GHC.Weak (Weak(..))
import Foreign.Prim

-- | Same as `System.Mem.Weak.mkWeak`, except it requires a finalizer to be
-- supplied. For a version without finalizers use `mkWeakNoFinalizer`
mkWeak :: MonadUnliftPrim RW m => a -> v -> m b -> m (Weak v)
mkWeak key val finalizer =
  runInPrimBase finalizer $ \f s ->
    case mkWeak# key val f s of
      (# s1, w #) -> (# s1, Weak w #)

-- | Similar to `mkWeak`, except it does not require a finalizer.
mkWeakNoFinalizer :: MonadPrim RW m => a -> v -> m (Weak v)
mkWeakNoFinalizer key val =
  prim $ \s ->
    case mkWeakNoFinalizer# key val s of
      (# s1, w #) -> (# s1, Weak w #)

-- | Same as `System.Mem.Weak.mkWeakPtr`, except it requires a finalizer to be
-- supplied. For a version without finalizers use `mkWeakPtrNoFinalizer`
mkWeakPtr :: MonadUnliftPrim RW m => k -> m b -> m (Weak k)
mkWeakPtr key = mkWeak key key

-- | Similar to `mkWeakPtr`, except it does not require a finalizer.
mkWeakPtrNoFinalizer :: MonadPrim RW m => k -> m (Weak k)
mkWeakPtrNoFinalizer key = mkWeakNoFinalizer key key


-- | Same as `System.Mem.Weak.addFinalizer`.
addFinalizer :: MonadUnliftPrim RW m => k -> m b -> m ()
addFinalizer key = void . mkWeakPtr key

-- | Add a foreign function finalizer with a single argument
addCFinalizer ::
     MonadPrim RW m
  => FunPtr (Ptr a -> IO ())
     -- ^ Pointer to the C function to be called when finalizers are being invoked
  -> Ptr a
     -- ^ Argument that will be supplied to the finalizer function
  -> Weak v
  -> m Bool
addCFinalizer (FunPtr faddr#) (Ptr addr#) (Weak weak#) =
  prim $ \s ->
    case addCFinalizerToWeak# faddr# addr# 0# nullAddr# weak# s of
      (# s', i# #) -> (# s', isTrue# i# #)

-- | Add a foreign function finalizer with two arguments
addCFinalizerEnv ::
     MonadPrim RW m
  => FunPtr (Ptr env -> Ptr a -> IO ())
     -- ^ Pointer to the C function to be called when finalizers are being invoked
  -> Ptr env
     -- ^ First argument that will be supplied to the finalizer function
  -> Ptr a
     -- ^ Second argument that will be supplied to the finalizer function
  -> Weak v
  -> m Bool
addCFinalizerEnv (FunPtr faddr#) (Ptr envAddr#) (Ptr addr#) (Weak weak#) =
  prim $ \s ->
    case addCFinalizerToWeak# faddr# addr# 1# envAddr# weak# s of
      (# s', i# #) -> (# s', isTrue# i# #)

-- | Similar to `System.Mem.Weak.deRefWeak`
deRefWeak :: MonadPrim RW m => Weak v -> m (Maybe v)
deRefWeak (Weak weak#) =
  prim $ \s ->
    case deRefWeak# weak# s of
      (# s', 0#, _ #) -> (# s', Nothing #)
      (# s', _, a #) -> (# s', Just a #)


-- | Similar to `System.Mem.Weak.finalize`
finalizeWeak :: MonadPrim RW m => Weak v -> m ()
finalizeWeak (Weak w) =
  prim $ \s ->
    case finalizeWeak# w s of
      (# s1, 0#, _ #) -> (# s1, () #)
      (# s1, _, f #) -> f s1
