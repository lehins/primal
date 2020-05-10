{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
#if __GLASGOW_HASKELL__ < 800
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif
-- |
-- Module      : Data.Prim.Atomic
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Atomic
  ( Atomic(..)
  , AtomicBits(..)
  , AtomicCount(..)
  , atomicModifyMutableByteArray#
  , atomicModifyMutableByteArray_#
  , atomicFetchModifyMutableByteArray#
  , atomicModifyFetchMutableByteArray#
  , atomicModifyOffAddr#
  , atomicModifyOffAddr_#
  , atomicFetchModifyOffAddr#
  , atomicModifyFetchOffAddr#
  , atomicFetchNotMutableByteArray#
  , atomicNotFetchMutableByteArray#
  , atomicFetchNotOffAddr#
  , atomicNotFetchOffAddr#
  ) where

import Control.Prim.Monad.Unsafe
import Data.Bits
import Data.Functor.Identity
import Data.Monoid
import Data.Prim.Class
import Foreign.C.Error (Errno(..))
import Foreign.Prim hiding (Any)
import GHC.Conc
import GHC.IO.Device
#if __GLASGOW_HASKELL__ >= 800
import Data.Functor.Const
import Data.Semigroup
#endif /* __GLASGOW_HASKELL__ >= 800 */

#include "MachDeps.h"

class (Prim a, Eq a) => Atomic a where
  -- | Read an element from `MutableByteArray#` atomically. Implies full memory barrier.
  atomicReadMutableByteArray# ::
       MutableByteArray# s -- ^ Mutable array to read the element from
    -> Int# -- ^ Offset into the array in number of elements
    -> State# s
    -> (# State# s, a #)
  atomicReadMutableByteArray# mba# i# = withMemBarrier# (readMutableByteArray# mba# i#)
  {-# INLINE atomicReadMutableByteArray# #-}

  -- | Write an element into `MutableByteArray#` atomically. Implies full memory barrier.
  atomicWriteMutableByteArray# ::
       MutableByteArray# s -- ^ Mutable array to write the element into
    -> Int# -- ^ Offset into the array in number of elements
    -> a -- ^ Element to write
    -> State# s
    -> State# s
  atomicWriteMutableByteArray# mba# i# a = withMemBarrier_# (writeMutableByteArray# mba# i# a)
  {-# INLINE atomicWriteMutableByteArray# #-}

  -- | Read an element from memory atomically. Implies full memory barrier.
  atomicReadOffAddr# ::
       Addr# -- ^ Pointer to the beginning of memory
    -> Int# -- ^ Offset in number of elements from the supplied pointer
    -> State# s
    -> (# State# s, a #)
  atomicReadOffAddr# addr# i# = withMemBarrier# (readOffAddr# addr# i#)
  {-# INLINE atomicReadOffAddr# #-}

  -- | Write an element directly into memory atomically. Implies full memory barrier.
  atomicWriteOffAddr# ::
       Addr# -- ^ Pointer to the beginning of memory
    -> Int# -- ^ Offset in number of elements from the supplied pointer
    -> a -- ^ Element to write
    -> State# s
    -> State# s
  atomicWriteOffAddr# addr# i# a = withMemBarrier_# (writeOffAddr# addr# i# a)
  {-# INLINE atomicWriteOffAddr# #-}

  -- | Compare-and-swap (CAS) operation. Given a mutable array, offset in number of
  -- elements, an old value and a new value atomically swap the old value for the new one,
  -- but only if the actual old value was an exact match to the expetced old one that was
  -- supplied. Returns the actual old value, which if compared to supplied expected one
  -- will tell us whether atomic swap occured or not.
  casMutableByteArray# ::
       MutableByteArray# s -- ^ Array to be mutated
    -> Int# -- ^ Offset into the array in number of elements
    -> a -- ^ Expected old value
    -> a -- ^ New value
    -> State# s -- ^ Starting state
    -> (# State# s, a #)
  default casMutableByteArray#
    :: Atomic (PrimBase a)
    => MutableByteArray# s
    -> Int#
    -> a
    -> a
    -> State# s
    -> (# State# s, a #)
  casMutableByteArray# mba# i# old new s =
    case casMutableByteArray# mba# i# (toPrimBase old) (toPrimBase new) s of
      (# s', prev #) -> (# s', fromPrimBase prev #)
  {-# INLINE casMutableByteArray# #-}

  casOffAddr# :: Addr# -> Int# -> a -> a -> State# s -> (# State# s, a #)
  default casOffAddr# ::
    Atomic (PrimBase a) => Addr# -> Int# -> a -> a -> State# s -> (# State# s, a #)
  casOffAddr# addr# i# old new s =
    case casOffAddr# addr# i# (toPrimBase old) (toPrimBase new) s of
      (# s', prev #) -> (# s', fromPrimBase prev #)
  {-# INLINE casOffAddr# #-}



-- | Using `casMutableByteArray#` perform atomic modification of an element in a
-- `MutableByteArray#`. This is essentially an implementation of a spinlock using CAS.
--
-- @since 0.1.0
atomicModifyMutableByteArray# ::
     Atomic a =>
     MutableByteArray# s -- ^ Array to be mutated
  -> Int# -- ^ Index in number of `Int#` elements into the `MutableByteArray#`
  -> (a -> (# a, b #)) -- ^ Function to be applied atomically to the element
  -> State# s -- ^ Starting state
  -> (# State# s, b #)
atomicModifyMutableByteArray# mba# i# f s0 =
  let go s o =
        case f o of
          (# n, artifact #) ->
            case casMutableByteArray# mba# i# o n s of
              (# s', o' #) ->
                if o == o'
                  then (# s', artifact #)
                  else go s o'
   in case atomicReadMutableByteArray# mba# i# s0 of
        (# s', o #) -> go s' o
{-# INLINE atomicModifyMutableByteArray# #-}


-- | Using `casMutableByteArray#` perform atomic modification of an element in a
-- `MutableByteArray#`. Returns the previous value.
--
-- @since 0.1.0
atomicFetchModifyMutableByteArray# ::
     Atomic a =>
     MutableByteArray# s -- ^ Array to be mutated
  -> Int# -- ^ Index in number of `Int#` elements into the `MutableByteArray#`
  -> (a -> a) -- ^ Function to be applied atomically to the element
  -> State# s -- ^ Starting state
  -> (# State# s, a #)
atomicFetchModifyMutableByteArray# mba# i# f =
  atomicModifyMutableByteArray# mba# i# (\a -> let a' = f a in seq a' (# a', a #))
{-# INLINE atomicFetchModifyMutableByteArray# #-}


-- | Using `casMutableByteArray#` perform atomic modification of an element in a
-- `MutableByteArray#`. Returns the new value.
--
-- @since 0.1.0
atomicModifyFetchMutableByteArray# ::
     Atomic a =>
     MutableByteArray# s -- ^ Array to be mutated
  -> Int# -- ^ Index in number of `Int#` elements into the `MutableByteArray#`
  -> (a -> a) -- ^ Function to be applied atomically to the element
  -> State# s -- ^ Starting state
  -> (# State# s, a #)
atomicModifyFetchMutableByteArray# mba# i# f =
  atomicModifyMutableByteArray# mba# i# (\a -> let a' = f a in seq a' (# a', a' #))
{-# INLINE atomicModifyFetchMutableByteArray# #-}

-- | Using `casMutableByteArray#` perform atomic modification of an element in a
-- `MutableByteArray#`.
--
-- @since 0.1.0
atomicModifyMutableByteArray_# ::
     Atomic a =>
     MutableByteArray# s -- ^ Array to be mutated
  -> Int# -- ^ Index in number of `Int#` elements into the `MutableByteArray#`
  -> (a -> a) -- ^ Function to be applied atomically to the element
  -> State# s -- ^ Starting state
  -> State# s
atomicModifyMutableByteArray_# mba# i# f s =
  case atomicModifyMutableByteArray# mba# i# (\a -> let a' = f a in seq a' (# a', () #)) s of
    (# s', () #) -> s'
{-# INLINE atomicModifyMutableByteArray_# #-}


-- | Using `casOffAddr#` perform atomic modification of an element in a
-- `OffAddr#`. This is essentially an implementation of a spinlock using CAS.
--
-- @since 0.1.0
atomicModifyOffAddr# ::
     Atomic a =>
     Addr# -- ^ Array to be mutated
  -> Int# -- ^ Index in number of `Int#` elements into the `OffAddr#`
  -> (a -> (# a, b #)) -- ^ Function to be applied atomically to the element
  -> State# s -- ^ Starting state
  -> (# State# s, b #)
atomicModifyOffAddr# addr# i# f s0 =
  let go s o =
        case f o of
          (# n, artifact #) ->
            case casOffAddr# addr# i# o n s of
              (# s', o' #) ->
                if o == o'
                  then (# s', artifact #)
                  else go s o'
   in case atomicReadOffAddr# addr# i# s0 of
        (# s', o #) -> go s' o
{-# INLINE atomicModifyOffAddr# #-}


-- | Using `casOffAddr#` perform atomic modification of an element in a
-- `OffAddr#`. Returns the previous value.
--
-- @since 0.1.0
atomicFetchModifyOffAddr# ::
     Atomic a =>
     Addr# -- ^ Array to be mutated
  -> Int# -- ^ Index in number of `Int#` elements into the `OffAddr#`
  -> (a -> a) -- ^ Function to be applied atomically to the element
  -> State# s -- ^ Starting state
  -> (# State# s, a #)
atomicFetchModifyOffAddr# addr# i# f =
  atomicModifyOffAddr# addr# i# (\a -> let a' = f a in seq a' (# a', a #))
{-# INLINE atomicFetchModifyOffAddr# #-}


-- | Using `casOffAddr#` perform atomic modification of an element in a
-- `OffAddr#`. Returns the new value.
--
-- @since 0.1.0
atomicModifyFetchOffAddr# ::
     Atomic a =>
     Addr# -- ^ Array to be mutated
  -> Int# -- ^ Index in number of `Int#` elements into the `OffAddr#`
  -> (a -> a) -- ^ Function to be applied atomically to the element
  -> State# s -- ^ Starting state
  -> (# State# s, a #)
atomicModifyFetchOffAddr# addr# i# f =
  atomicModifyOffAddr# addr# i# (\a -> let a' = f a in seq a' (# a', a' #))
{-# INLINE atomicModifyFetchOffAddr# #-}

-- | Using `casOffAddr#` perform atomic modification of an element in a
-- `OffAddr#`.
--
-- @since 0.1.0
atomicModifyOffAddr_# ::
     Atomic a =>
     Addr# -- ^ Array to be mutated
  -> Int# -- ^ Index in number of `Int#` elements into the `OffAddr#`
  -> (a -> a) -- ^ Function to be applied atomically to the element
  -> State# s -- ^ Starting state
  -> State# s
atomicModifyOffAddr_# addr# i# f s =
  case atomicModifyOffAddr# addr# i# (\a -> let a' = f a in seq a' (# a', () #)) s of
    (# s', () #) -> s'
{-# INLINE atomicModifyOffAddr_# #-}



class Atomic a => AtomicCount a where
  atomicFetchAddMutableByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicFetchAddMutableByteArray# ::
    AtomicCount (PrimBase a) => MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  atomicFetchAddMutableByteArray# mba# i# x s =
    case atomicFetchAddMutableByteArray# mba# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicFetchAddMutableByteArray# #-}

  atomicAddFetchMutableByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicAddFetchMutableByteArray# ::
    AtomicCount (PrimBase a) => MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  atomicAddFetchMutableByteArray# mba# i# x s =
    case atomicAddFetchMutableByteArray# mba# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicAddFetchMutableByteArray# #-}

  atomicFetchSubMutableByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicFetchSubMutableByteArray# ::
    AtomicCount (PrimBase a) => MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  atomicFetchSubMutableByteArray# mba# i# x s =
    case atomicFetchSubMutableByteArray# mba# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicFetchSubMutableByteArray# #-}

  atomicSubFetchMutableByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicSubFetchMutableByteArray# ::
    AtomicCount (PrimBase a) => MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  atomicSubFetchMutableByteArray# mba# i# x s =
    case atomicSubFetchMutableByteArray# mba# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicSubFetchMutableByteArray# #-}


  atomicFetchAddOffAddr# :: Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicFetchAddOffAddr# ::
    AtomicCount (PrimBase a) => Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  atomicFetchAddOffAddr# addr# i# x s =
    case atomicFetchAddOffAddr# addr# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicFetchAddOffAddr# #-}

  atomicAddFetchOffAddr# :: Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicAddFetchOffAddr# ::
    AtomicCount (PrimBase a) => Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  atomicAddFetchOffAddr# addr# i# x s =
    case atomicAddFetchOffAddr# addr# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicAddFetchOffAddr# #-}

  atomicFetchSubOffAddr# :: Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicFetchSubOffAddr# ::
    AtomicCount (PrimBase a) => Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  atomicFetchSubOffAddr# addr# i# x s =
    case atomicFetchSubOffAddr# addr# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicFetchSubOffAddr# #-}

  atomicSubFetchOffAddr# :: Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicSubFetchOffAddr# ::
    AtomicCount (PrimBase a) => Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  atomicSubFetchOffAddr# addr# i# x s =
    case atomicSubFetchOffAddr# addr# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicSubFetchOffAddr# #-}



class (Bits a, Atomic a) => AtomicBits a where
  atomicFetchAndMutableByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicFetchAndMutableByteArray# ::
    AtomicBits (PrimBase a) => MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  atomicFetchAndMutableByteArray# mba# i# x s =
    case atomicFetchAndMutableByteArray# mba# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicFetchAndMutableByteArray# #-}

  atomicAndFetchMutableByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicAndFetchMutableByteArray# ::
    AtomicBits (PrimBase a) => MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  atomicAndFetchMutableByteArray# mba# i# x s =
    case atomicAndFetchMutableByteArray# mba# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicAndFetchMutableByteArray# #-}

  atomicFetchNandMutableByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicFetchNandMutableByteArray# ::
    AtomicBits (PrimBase a) => MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  atomicFetchNandMutableByteArray# mba# i# x s =
    case atomicFetchNandMutableByteArray# mba# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicFetchNandMutableByteArray# #-}

  atomicNandFetchMutableByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicNandFetchMutableByteArray# ::
    AtomicBits (PrimBase a) => MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  atomicNandFetchMutableByteArray# mba# i# x s =
    case atomicNandFetchMutableByteArray# mba# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicNandFetchMutableByteArray# #-}

  atomicFetchOrMutableByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicFetchOrMutableByteArray# ::
    AtomicBits (PrimBase a) => MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  atomicFetchOrMutableByteArray# mba# i# x s =
    case atomicFetchOrMutableByteArray# mba# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicFetchOrMutableByteArray# #-}

  atomicOrFetchMutableByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicOrFetchMutableByteArray# ::
    AtomicBits (PrimBase a) => MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  atomicOrFetchMutableByteArray# mba# i# x s =
    case atomicOrFetchMutableByteArray# mba# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicOrFetchMutableByteArray# #-}

  atomicFetchXorMutableByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicFetchXorMutableByteArray# ::
    AtomicBits (PrimBase a) => MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  atomicFetchXorMutableByteArray# mba# i# x s =
    case atomicFetchXorMutableByteArray# mba# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicFetchXorMutableByteArray# #-}

  atomicXorFetchMutableByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicXorFetchMutableByteArray# ::
    AtomicBits (PrimBase a) => MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  atomicXorFetchMutableByteArray# mba# i# x s =
    case atomicXorFetchMutableByteArray# mba# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicXorFetchMutableByteArray# #-}


  atomicFetchAndOffAddr# :: Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicFetchAndOffAddr# ::
    AtomicBits (PrimBase a) => Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  atomicFetchAndOffAddr# addr# i# x s =
    case atomicFetchAndOffAddr# addr# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicFetchAndOffAddr# #-}

  atomicAndFetchOffAddr# :: Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicAndFetchOffAddr# ::
    AtomicBits (PrimBase a) => Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  atomicAndFetchOffAddr# addr# i# x s =
    case atomicAndFetchOffAddr# addr# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicAndFetchOffAddr# #-}

  atomicFetchNandOffAddr# :: Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicFetchNandOffAddr# ::
    AtomicBits (PrimBase a) => Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  atomicFetchNandOffAddr# addr# i# x s =
    case atomicFetchNandOffAddr# addr# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicFetchNandOffAddr# #-}

  atomicNandFetchOffAddr# :: Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicNandFetchOffAddr# ::
    AtomicBits (PrimBase a) => Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  atomicNandFetchOffAddr# addr# i# x s =
    case atomicNandFetchOffAddr# addr# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicNandFetchOffAddr# #-}

  atomicFetchOrOffAddr# :: Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicFetchOrOffAddr# ::
    AtomicBits (PrimBase a) => Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  atomicFetchOrOffAddr# addr# i# x s =
    case atomicFetchOrOffAddr# addr# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicFetchOrOffAddr# #-}

  atomicOrFetchOffAddr# :: Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicOrFetchOffAddr# ::
    AtomicBits (PrimBase a) => Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  atomicOrFetchOffAddr# addr# i# x s =
    case atomicOrFetchOffAddr# addr# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicOrFetchOffAddr# #-}

  atomicFetchXorOffAddr# :: Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicFetchXorOffAddr# ::
    AtomicBits (PrimBase a) => Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  atomicFetchXorOffAddr# addr# i# x s =
    case atomicFetchXorOffAddr# addr# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicFetchXorOffAddr# #-}

  atomicXorFetchOffAddr# :: Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicXorFetchOffAddr# ::
    AtomicBits (PrimBase a) => Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  atomicXorFetchOffAddr# addr# i# x s =
    case atomicXorFetchOffAddr# addr# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicXorFetchOffAddr# #-}



-- | Flip all bits atomically in the element of an array at the supplied
-- offset. Returns the previous value. Implies full memory barrier.
atomicFetchNotMutableByteArray# ::
  forall a s . AtomicBits a => MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
atomicFetchNotMutableByteArray# mba# i# =
  atomicFetchXorMutableByteArray# mba# i# (complement (zeroBits :: a))
{-# INLINE atomicFetchNotMutableByteArray# #-}

-- | Flip all bits atomically in the element of an array at the supplied
-- offset. Returns the new value. Implies full memory barrier.
atomicNotFetchMutableByteArray# ::
  forall a s . AtomicBits a => MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
atomicNotFetchMutableByteArray# mba# i# =
  atomicXorFetchMutableByteArray# mba# i# (complement (zeroBits :: a))
{-# INLINE atomicNotFetchMutableByteArray# #-}


-- | Flip all bits atomically in the element of an array at the supplied
-- offset. Returns the previous value. Implies full memory barrier.
atomicFetchNotOffAddr# ::
  forall a s . AtomicBits a => Addr# -> Int# -> State# s -> (# State# s, a #)
atomicFetchNotOffAddr# addr# i# =
  atomicFetchXorOffAddr# addr# i# (complement (zeroBits :: a))
{-# INLINE atomicFetchNotOffAddr# #-}

-- | Flip all bits atomically in the element of an array at the supplied
-- offset. Returns the new value. Implies full memory barrier.
atomicNotFetchOffAddr# ::
  forall a s . AtomicBits a => Addr# -> Int# -> State# s -> (# State# s, a #)
atomicNotFetchOffAddr# addr# i# =
  atomicXorFetchOffAddr# addr# i# (complement (zeroBits :: a))
{-# INLINE atomicNotFetchOffAddr# #-}



instance Atomic Int8 where
  casMutableByteArray# mba# i# old new = unsafePrimBase (syncCasInt8ArrayIO mba# i# old new)
  {-# INLINE casMutableByteArray# #-}
  casOffAddr# addr# i# old new = unsafePrimBase (syncCasInt8AddrIO addr# i# old new)
  {-# INLINE casOffAddr# #-}

instance AtomicCount Int8 where
  atomicFetchAddMutableByteArray# mba# i# a = unsafePrimBase (syncFetchAddInt8ArrayIO mba# i# a)
  {-# INLINE atomicFetchAddMutableByteArray# #-}
  atomicAddFetchMutableByteArray# mba# i# a = unsafePrimBase (syncAddFetchInt8ArrayIO mba# i# a)
  {-# INLINE atomicAddFetchMutableByteArray# #-}
  atomicFetchSubMutableByteArray# mba# i# a = unsafePrimBase (syncFetchSubInt8ArrayIO mba# i# a)
  {-# INLINE atomicFetchSubMutableByteArray# #-}
  atomicSubFetchMutableByteArray# mba# i# a = unsafePrimBase (syncSubFetchInt8ArrayIO mba# i# a)
  {-# INLINE atomicSubFetchMutableByteArray# #-}
  atomicFetchAddOffAddr# addr# i# a = unsafePrimBase (syncFetchAddInt8AddrIO addr# i# a)
  {-# INLINE atomicFetchAddOffAddr# #-}
  atomicAddFetchOffAddr# addr# i# a = unsafePrimBase (syncAddFetchInt8AddrIO addr# i# a)
  {-# INLINE atomicAddFetchOffAddr# #-}
  atomicFetchSubOffAddr# addr# i# a = unsafePrimBase (syncFetchSubInt8AddrIO addr# i# a)
  {-# INLINE atomicFetchSubOffAddr# #-}
  atomicSubFetchOffAddr# addr# i# a = unsafePrimBase (syncSubFetchInt8AddrIO addr# i# a)
  {-# INLINE atomicSubFetchOffAddr# #-}

instance AtomicBits Int8 where
  atomicFetchAndMutableByteArray# mba# i# a = unsafePrimBase (syncFetchAndInt8ArrayIO mba# i# a)
  {-# INLINE atomicFetchAndMutableByteArray# #-}
  atomicAndFetchMutableByteArray# mba# i# a = unsafePrimBase (syncAndFetchInt8ArrayIO mba# i# a)
  {-# INLINE atomicAndFetchMutableByteArray# #-}
  atomicFetchNandMutableByteArray# mba# i# a = unsafePrimBase (syncFetchNandInt8ArrayIO mba# i# a)
  {-# INLINE atomicFetchNandMutableByteArray# #-}
  atomicNandFetchMutableByteArray# mba# i# a = unsafePrimBase (syncNandFetchInt8ArrayIO mba# i# a)
  {-# INLINE atomicNandFetchMutableByteArray# #-}
  atomicFetchOrMutableByteArray# mba# i# a = unsafePrimBase (syncFetchOrInt8ArrayIO mba# i# a)
  {-# INLINE atomicFetchOrMutableByteArray# #-}
  atomicOrFetchMutableByteArray# mba# i# a = unsafePrimBase (syncOrFetchInt8ArrayIO mba# i# a)
  {-# INLINE atomicOrFetchMutableByteArray# #-}
  atomicFetchXorMutableByteArray# mba# i# a = unsafePrimBase (syncFetchXorInt8ArrayIO mba# i# a)
  {-# INLINE atomicFetchXorMutableByteArray# #-}
  atomicXorFetchMutableByteArray# mba# i# a = unsafePrimBase (syncXorFetchInt8ArrayIO mba# i# a)
  {-# INLINE atomicXorFetchMutableByteArray# #-}
  atomicFetchAndOffAddr# addr# i# a = unsafePrimBase (syncFetchAndInt8AddrIO addr# i# a)
  {-# INLINE atomicFetchAndOffAddr# #-}
  atomicAndFetchOffAddr# addr# i# a = unsafePrimBase (syncAndFetchInt8AddrIO addr# i# a)
  {-# INLINE atomicAndFetchOffAddr# #-}
  atomicFetchNandOffAddr# addr# i# a = unsafePrimBase (syncFetchNandInt8AddrIO addr# i# a)
  {-# INLINE atomicFetchNandOffAddr# #-}
  atomicNandFetchOffAddr# addr# i# a = unsafePrimBase (syncNandFetchInt8AddrIO addr# i# a)
  {-# INLINE atomicNandFetchOffAddr# #-}
  atomicFetchOrOffAddr# addr# i# a = unsafePrimBase (syncFetchOrInt8AddrIO addr# i# a)
  {-# INLINE atomicFetchOrOffAddr# #-}
  atomicOrFetchOffAddr# addr# i# a = unsafePrimBase (syncOrFetchInt8AddrIO addr# i# a)
  {-# INLINE atomicOrFetchOffAddr# #-}
  atomicFetchXorOffAddr# addr# i# a = unsafePrimBase (syncFetchXorInt8AddrIO addr# i# a)
  {-# INLINE atomicFetchXorOffAddr# #-}
  atomicXorFetchOffAddr# addr# i# a = unsafePrimBase (syncXorFetchInt8AddrIO addr# i# a)
  {-# INLINE atomicXorFetchOffAddr# #-}

instance Atomic Int16 where
  casMutableByteArray# mba# i# old new = unsafePrimBase (syncCasInt16ArrayIO mba# i# old new)
  {-# INLINE casMutableByteArray# #-}
  casOffAddr# addr# i# old new = unsafePrimBase (syncCasInt16AddrIO addr# i# old new)
  {-# INLINE casOffAddr# #-}

instance AtomicCount Int16 where
  atomicFetchAddMutableByteArray# mba# i# a = unsafePrimBase (syncFetchAddInt16ArrayIO mba# i# a)
  {-# INLINE atomicFetchAddMutableByteArray# #-}
  atomicAddFetchMutableByteArray# mba# i# a = unsafePrimBase (syncAddFetchInt16ArrayIO mba# i# a)
  {-# INLINE atomicAddFetchMutableByteArray# #-}
  atomicFetchSubMutableByteArray# mba# i# a = unsafePrimBase (syncFetchSubInt16ArrayIO mba# i# a)
  {-# INLINE atomicFetchSubMutableByteArray# #-}
  atomicSubFetchMutableByteArray# mba# i# a = unsafePrimBase (syncSubFetchInt16ArrayIO mba# i# a)
  {-# INLINE atomicSubFetchMutableByteArray# #-}
  atomicFetchAddOffAddr# addr# i# a = unsafePrimBase (syncFetchAddInt16AddrIO addr# i# a)
  {-# INLINE atomicFetchAddOffAddr# #-}
  atomicAddFetchOffAddr# addr# i# a = unsafePrimBase (syncAddFetchInt16AddrIO addr# i# a)
  {-# INLINE atomicAddFetchOffAddr# #-}
  atomicFetchSubOffAddr# addr# i# a = unsafePrimBase (syncFetchSubInt16AddrIO addr# i# a)
  {-# INLINE atomicFetchSubOffAddr# #-}
  atomicSubFetchOffAddr# addr# i# a = unsafePrimBase (syncSubFetchInt16AddrIO addr# i# a)
  {-# INLINE atomicSubFetchOffAddr# #-}

instance AtomicBits Int16 where
  atomicFetchAndMutableByteArray# mba# i# a = unsafePrimBase (syncFetchAndInt16ArrayIO mba# i# a)
  {-# INLINE atomicFetchAndMutableByteArray# #-}
  atomicAndFetchMutableByteArray# mba# i# a = unsafePrimBase (syncAndFetchInt16ArrayIO mba# i# a)
  {-# INLINE atomicAndFetchMutableByteArray# #-}
  atomicFetchNandMutableByteArray# mba# i# a = unsafePrimBase (syncFetchNandInt16ArrayIO mba# i# a)
  {-# INLINE atomicFetchNandMutableByteArray# #-}
  atomicNandFetchMutableByteArray# mba# i# a = unsafePrimBase (syncNandFetchInt16ArrayIO mba# i# a)
  {-# INLINE atomicNandFetchMutableByteArray# #-}
  atomicFetchOrMutableByteArray# mba# i# a = unsafePrimBase (syncFetchOrInt16ArrayIO mba# i# a)
  {-# INLINE atomicFetchOrMutableByteArray# #-}
  atomicOrFetchMutableByteArray# mba# i# a = unsafePrimBase (syncOrFetchInt16ArrayIO mba# i# a)
  {-# INLINE atomicOrFetchMutableByteArray# #-}
  atomicFetchXorMutableByteArray# mba# i# a = unsafePrimBase (syncFetchXorInt16ArrayIO mba# i# a)
  {-# INLINE atomicFetchXorMutableByteArray# #-}
  atomicXorFetchMutableByteArray# mba# i# a = unsafePrimBase (syncXorFetchInt16ArrayIO mba# i# a)
  {-# INLINE atomicXorFetchMutableByteArray# #-}
  atomicFetchAndOffAddr# addr# i# a = unsafePrimBase (syncFetchAndInt16AddrIO addr# i# a)
  {-# INLINE atomicFetchAndOffAddr# #-}
  atomicAndFetchOffAddr# addr# i# a = unsafePrimBase (syncAndFetchInt16AddrIO addr# i# a)
  {-# INLINE atomicAndFetchOffAddr# #-}
  atomicFetchNandOffAddr# addr# i# a = unsafePrimBase (syncFetchNandInt16AddrIO addr# i# a)
  {-# INLINE atomicFetchNandOffAddr# #-}
  atomicNandFetchOffAddr# addr# i# a = unsafePrimBase (syncNandFetchInt16AddrIO addr# i# a)
  {-# INLINE atomicNandFetchOffAddr# #-}
  atomicFetchOrOffAddr# addr# i# a = unsafePrimBase (syncFetchOrInt16AddrIO addr# i# a)
  {-# INLINE atomicFetchOrOffAddr# #-}
  atomicOrFetchOffAddr# addr# i# a = unsafePrimBase (syncOrFetchInt16AddrIO addr# i# a)
  {-# INLINE atomicOrFetchOffAddr# #-}
  atomicFetchXorOffAddr# addr# i# a = unsafePrimBase (syncFetchXorInt16AddrIO addr# i# a)
  {-# INLINE atomicFetchXorOffAddr# #-}
  atomicXorFetchOffAddr# addr# i# a = unsafePrimBase (syncXorFetchInt16AddrIO addr# i# a)
  {-# INLINE atomicXorFetchOffAddr# #-}

instance Atomic Int32 where
  casMutableByteArray# mba# i# old new = unsafePrimBase (syncCasInt32ArrayIO mba# i# old new)
  {-# INLINE casMutableByteArray# #-}
  casOffAddr# addr# i# old new = unsafePrimBase (syncCasInt32AddrIO addr# i# old new)
  {-# INLINE casOffAddr# #-}

instance AtomicCount Int32 where
  atomicFetchAddMutableByteArray# mba# i# a = unsafePrimBase (syncFetchAddInt32ArrayIO mba# i# a)
  {-# INLINE atomicFetchAddMutableByteArray# #-}
  atomicAddFetchMutableByteArray# mba# i# a = unsafePrimBase (syncAddFetchInt32ArrayIO mba# i# a)
  {-# INLINE atomicAddFetchMutableByteArray# #-}
  atomicFetchSubMutableByteArray# mba# i# a = unsafePrimBase (syncFetchSubInt32ArrayIO mba# i# a)
  {-# INLINE atomicFetchSubMutableByteArray# #-}
  atomicSubFetchMutableByteArray# mba# i# a = unsafePrimBase (syncSubFetchInt32ArrayIO mba# i# a)
  {-# INLINE atomicSubFetchMutableByteArray# #-}
  atomicFetchAddOffAddr# addr# i# a = unsafePrimBase (syncFetchAddInt32AddrIO addr# i# a)
  {-# INLINE atomicFetchAddOffAddr# #-}
  atomicAddFetchOffAddr# addr# i# a = unsafePrimBase (syncAddFetchInt32AddrIO addr# i# a)
  {-# INLINE atomicAddFetchOffAddr# #-}
  atomicFetchSubOffAddr# addr# i# a = unsafePrimBase (syncFetchSubInt32AddrIO addr# i# a)
  {-# INLINE atomicFetchSubOffAddr# #-}
  atomicSubFetchOffAddr# addr# i# a = unsafePrimBase (syncSubFetchInt32AddrIO addr# i# a)
  {-# INLINE atomicSubFetchOffAddr# #-}

instance AtomicBits Int32 where
  atomicFetchAndMutableByteArray# mba# i# a = unsafePrimBase (syncFetchAndInt32ArrayIO mba# i# a)
  {-# INLINE atomicFetchAndMutableByteArray# #-}
  atomicAndFetchMutableByteArray# mba# i# a = unsafePrimBase (syncAndFetchInt32ArrayIO mba# i# a)
  {-# INLINE atomicAndFetchMutableByteArray# #-}
  atomicFetchNandMutableByteArray# mba# i# a = unsafePrimBase (syncFetchNandInt32ArrayIO mba# i# a)
  {-# INLINE atomicFetchNandMutableByteArray# #-}
  atomicNandFetchMutableByteArray# mba# i# a = unsafePrimBase (syncNandFetchInt32ArrayIO mba# i# a)
  {-# INLINE atomicNandFetchMutableByteArray# #-}
  atomicFetchOrMutableByteArray# mba# i# a = unsafePrimBase (syncFetchOrInt32ArrayIO mba# i# a)
  {-# INLINE atomicFetchOrMutableByteArray# #-}
  atomicOrFetchMutableByteArray# mba# i# a = unsafePrimBase (syncOrFetchInt32ArrayIO mba# i# a)
  {-# INLINE atomicOrFetchMutableByteArray# #-}
  atomicFetchXorMutableByteArray# mba# i# a = unsafePrimBase (syncFetchXorInt32ArrayIO mba# i# a)
  {-# INLINE atomicFetchXorMutableByteArray# #-}
  atomicXorFetchMutableByteArray# mba# i# a = unsafePrimBase (syncXorFetchInt32ArrayIO mba# i# a)
  {-# INLINE atomicXorFetchMutableByteArray# #-}
  atomicFetchAndOffAddr# addr# i# a = unsafePrimBase (syncFetchAndInt32AddrIO addr# i# a)
  {-# INLINE atomicFetchAndOffAddr# #-}
  atomicAndFetchOffAddr# addr# i# a = unsafePrimBase (syncAndFetchInt32AddrIO addr# i# a)
  {-# INLINE atomicAndFetchOffAddr# #-}
  atomicFetchNandOffAddr# addr# i# a = unsafePrimBase (syncFetchNandInt32AddrIO addr# i# a)
  {-# INLINE atomicFetchNandOffAddr# #-}
  atomicNandFetchOffAddr# addr# i# a = unsafePrimBase (syncNandFetchInt32AddrIO addr# i# a)
  {-# INLINE atomicNandFetchOffAddr# #-}
  atomicFetchOrOffAddr# addr# i# a = unsafePrimBase (syncFetchOrInt32AddrIO addr# i# a)
  {-# INLINE atomicFetchOrOffAddr# #-}
  atomicOrFetchOffAddr# addr# i# a = unsafePrimBase (syncOrFetchInt32AddrIO addr# i# a)
  {-# INLINE atomicOrFetchOffAddr# #-}
  atomicFetchXorOffAddr# addr# i# a = unsafePrimBase (syncFetchXorInt32AddrIO addr# i# a)
  {-# INLINE atomicFetchXorOffAddr# #-}
  atomicXorFetchOffAddr# addr# i# a = unsafePrimBase (syncXorFetchInt32AddrIO addr# i# a)
  {-# INLINE atomicXorFetchOffAddr# #-}

-- TODO: compare with and possibly swap for primops
instance Atomic Int where
  casMutableByteArray# mba# i# old new = unsafePrimBase (syncCasIntArrayIO mba# i# old new)
  {-# INLINE casMutableByteArray# #-}
  casOffAddr# addr# i# old new = unsafePrimBase (syncCasIntAddrIO addr# i# old new)
  {-# INLINE casOffAddr# #-}

instance AtomicCount Int where
  atomicFetchAddMutableByteArray# mba# i# a = unsafePrimBase (syncFetchAddIntArrayIO mba# i# a)
  {-# INLINE atomicFetchAddMutableByteArray# #-}
  atomicAddFetchMutableByteArray# mba# i# a = unsafePrimBase (syncAddFetchIntArrayIO mba# i# a)
  {-# INLINE atomicAddFetchMutableByteArray# #-}
  atomicFetchSubMutableByteArray# mba# i# a = unsafePrimBase (syncFetchSubIntArrayIO mba# i# a)
  {-# INLINE atomicFetchSubMutableByteArray# #-}
  atomicSubFetchMutableByteArray# mba# i# a = unsafePrimBase (syncSubFetchIntArrayIO mba# i# a)
  {-# INLINE atomicSubFetchMutableByteArray# #-}
  atomicFetchAddOffAddr# addr# i# a = unsafePrimBase (syncFetchAddIntAddrIO addr# i# a)
  {-# INLINE atomicFetchAddOffAddr# #-}
  atomicAddFetchOffAddr# addr# i# a = unsafePrimBase (syncAddFetchIntAddrIO addr# i# a)
  {-# INLINE atomicAddFetchOffAddr# #-}
  atomicFetchSubOffAddr# addr# i# a = unsafePrimBase (syncFetchSubIntAddrIO addr# i# a)
  {-# INLINE atomicFetchSubOffAddr# #-}
  atomicSubFetchOffAddr# addr# i# a = unsafePrimBase (syncSubFetchIntAddrIO addr# i# a)
  {-# INLINE atomicSubFetchOffAddr# #-}

instance AtomicBits Int where
  atomicFetchAndMutableByteArray# mba# i# a = unsafePrimBase (syncFetchAndIntArrayIO mba# i# a)
  {-# INLINE atomicFetchAndMutableByteArray# #-}
  atomicAndFetchMutableByteArray# mba# i# a = unsafePrimBase (syncAndFetchIntArrayIO mba# i# a)
  {-# INLINE atomicAndFetchMutableByteArray# #-}
  atomicFetchNandMutableByteArray# mba# i# a = unsafePrimBase (syncFetchNandIntArrayIO mba# i# a)
  {-# INLINE atomicFetchNandMutableByteArray# #-}
  atomicNandFetchMutableByteArray# mba# i# a = unsafePrimBase (syncNandFetchIntArrayIO mba# i# a)
  {-# INLINE atomicNandFetchMutableByteArray# #-}
  atomicFetchOrMutableByteArray# mba# i# a = unsafePrimBase (syncFetchOrIntArrayIO mba# i# a)
  {-# INLINE atomicFetchOrMutableByteArray# #-}
  atomicOrFetchMutableByteArray# mba# i# a = unsafePrimBase (syncOrFetchIntArrayIO mba# i# a)
  {-# INLINE atomicOrFetchMutableByteArray# #-}
  atomicFetchXorMutableByteArray# mba# i# a = unsafePrimBase (syncFetchXorIntArrayIO mba# i# a)
  {-# INLINE atomicFetchXorMutableByteArray# #-}
  atomicXorFetchMutableByteArray# mba# i# a = unsafePrimBase (syncXorFetchIntArrayIO mba# i# a)
  {-# INLINE atomicXorFetchMutableByteArray# #-}
  atomicFetchAndOffAddr# addr# i# a = unsafePrimBase (syncFetchAndIntAddrIO addr# i# a)
  {-# INLINE atomicFetchAndOffAddr# #-}
  atomicAndFetchOffAddr# addr# i# a = unsafePrimBase (syncAndFetchIntAddrIO addr# i# a)
  {-# INLINE atomicAndFetchOffAddr# #-}
  atomicFetchNandOffAddr# addr# i# a = unsafePrimBase (syncFetchNandIntAddrIO addr# i# a)
  {-# INLINE atomicFetchNandOffAddr# #-}
  atomicNandFetchOffAddr# addr# i# a = unsafePrimBase (syncNandFetchIntAddrIO addr# i# a)
  {-# INLINE atomicNandFetchOffAddr# #-}
  atomicFetchOrOffAddr# addr# i# a = unsafePrimBase (syncFetchOrIntAddrIO addr# i# a)
  {-# INLINE atomicFetchOrOffAddr# #-}
  atomicOrFetchOffAddr# addr# i# a = unsafePrimBase (syncOrFetchIntAddrIO addr# i# a)
  {-# INLINE atomicOrFetchOffAddr# #-}
  atomicFetchXorOffAddr# addr# i# a = unsafePrimBase (syncFetchXorIntAddrIO addr# i# a)
  {-# INLINE atomicFetchXorOffAddr# #-}
  atomicXorFetchOffAddr# addr# i# a = unsafePrimBase (syncXorFetchIntAddrIO addr# i# a)
  {-# INLINE atomicXorFetchOffAddr# #-}




instance Atomic Word8 where
  casMutableByteArray# mba# i# old new = unsafePrimBase (syncCasWord8ArrayIO mba# i# old new)
  {-# INLINE casMutableByteArray# #-}
  casOffAddr# addr# i# old new = unsafePrimBase (syncCasWord8AddrIO addr# i# old new)
  {-# INLINE casOffAddr# #-}

instance AtomicCount Word8 where
  atomicFetchAddMutableByteArray# mba# i# a = unsafePrimBase (syncFetchAddWord8ArrayIO mba# i# a)
  {-# INLINE atomicFetchAddMutableByteArray# #-}
  atomicAddFetchMutableByteArray# mba# i# a = unsafePrimBase (syncAddFetchWord8ArrayIO mba# i# a)
  {-# INLINE atomicAddFetchMutableByteArray# #-}
  atomicFetchSubMutableByteArray# mba# i# a = unsafePrimBase (syncFetchSubWord8ArrayIO mba# i# a)
  {-# INLINE atomicFetchSubMutableByteArray# #-}
  atomicSubFetchMutableByteArray# mba# i# a = unsafePrimBase (syncSubFetchWord8ArrayIO mba# i# a)
  {-# INLINE atomicSubFetchMutableByteArray# #-}
  atomicFetchAddOffAddr# addr# i# a = unsafePrimBase (syncFetchAddWord8AddrIO addr# i# a)
  {-# INLINE atomicFetchAddOffAddr# #-}
  atomicAddFetchOffAddr# addr# i# a = unsafePrimBase (syncAddFetchWord8AddrIO addr# i# a)
  {-# INLINE atomicAddFetchOffAddr# #-}
  atomicFetchSubOffAddr# addr# i# a = unsafePrimBase (syncFetchSubWord8AddrIO addr# i# a)
  {-# INLINE atomicFetchSubOffAddr# #-}
  atomicSubFetchOffAddr# addr# i# a = unsafePrimBase (syncSubFetchWord8AddrIO addr# i# a)
  {-# INLINE atomicSubFetchOffAddr# #-}

instance AtomicBits Word8 where
  atomicFetchAndMutableByteArray# mba# i# a = unsafePrimBase (syncFetchAndWord8ArrayIO mba# i# a)
  {-# INLINE atomicFetchAndMutableByteArray# #-}
  atomicAndFetchMutableByteArray# mba# i# a = unsafePrimBase (syncAndFetchWord8ArrayIO mba# i# a)
  {-# INLINE atomicAndFetchMutableByteArray# #-}
  atomicFetchNandMutableByteArray# mba# i# a = unsafePrimBase (syncFetchNandWord8ArrayIO mba# i# a)
  {-# INLINE atomicFetchNandMutableByteArray# #-}
  atomicNandFetchMutableByteArray# mba# i# a = unsafePrimBase (syncNandFetchWord8ArrayIO mba# i# a)
  {-# INLINE atomicNandFetchMutableByteArray# #-}
  atomicFetchOrMutableByteArray# mba# i# a = unsafePrimBase (syncFetchOrWord8ArrayIO mba# i# a)
  {-# INLINE atomicFetchOrMutableByteArray# #-}
  atomicOrFetchMutableByteArray# mba# i# a = unsafePrimBase (syncOrFetchWord8ArrayIO mba# i# a)
  {-# INLINE atomicOrFetchMutableByteArray# #-}
  atomicFetchXorMutableByteArray# mba# i# a = unsafePrimBase (syncFetchXorWord8ArrayIO mba# i# a)
  {-# INLINE atomicFetchXorMutableByteArray# #-}
  atomicXorFetchMutableByteArray# mba# i# a = unsafePrimBase (syncXorFetchWord8ArrayIO mba# i# a)
  {-# INLINE atomicXorFetchMutableByteArray# #-}
  atomicFetchAndOffAddr# addr# i# a = unsafePrimBase (syncFetchAndWord8AddrIO addr# i# a)
  {-# INLINE atomicFetchAndOffAddr# #-}
  atomicAndFetchOffAddr# addr# i# a = unsafePrimBase (syncAndFetchWord8AddrIO addr# i# a)
  {-# INLINE atomicAndFetchOffAddr# #-}
  atomicFetchNandOffAddr# addr# i# a = unsafePrimBase (syncFetchNandWord8AddrIO addr# i# a)
  {-# INLINE atomicFetchNandOffAddr# #-}
  atomicNandFetchOffAddr# addr# i# a = unsafePrimBase (syncNandFetchWord8AddrIO addr# i# a)
  {-# INLINE atomicNandFetchOffAddr# #-}
  atomicFetchOrOffAddr# addr# i# a = unsafePrimBase (syncFetchOrWord8AddrIO addr# i# a)
  {-# INLINE atomicFetchOrOffAddr# #-}
  atomicOrFetchOffAddr# addr# i# a = unsafePrimBase (syncOrFetchWord8AddrIO addr# i# a)
  {-# INLINE atomicOrFetchOffAddr# #-}
  atomicFetchXorOffAddr# addr# i# a = unsafePrimBase (syncFetchXorWord8AddrIO addr# i# a)
  {-# INLINE atomicFetchXorOffAddr# #-}
  atomicXorFetchOffAddr# addr# i# a = unsafePrimBase (syncXorFetchWord8AddrIO addr# i# a)
  {-# INLINE atomicXorFetchOffAddr# #-}

instance Atomic Word16 where
  casMutableByteArray# mba# i# old new = unsafePrimBase (syncCasWord16ArrayIO mba# i# old new)
  {-# INLINE casMutableByteArray# #-}
  casOffAddr# addr# i# old new = unsafePrimBase (syncCasWord16AddrIO addr# i# old new)
  {-# INLINE casOffAddr# #-}

instance AtomicCount Word16 where
  atomicFetchAddMutableByteArray# mba# i# a = unsafePrimBase (syncFetchAddWord16ArrayIO mba# i# a)
  {-# INLINE atomicFetchAddMutableByteArray# #-}
  atomicAddFetchMutableByteArray# mba# i# a = unsafePrimBase (syncAddFetchWord16ArrayIO mba# i# a)
  {-# INLINE atomicAddFetchMutableByteArray# #-}
  atomicFetchSubMutableByteArray# mba# i# a = unsafePrimBase (syncFetchSubWord16ArrayIO mba# i# a)
  {-# INLINE atomicFetchSubMutableByteArray# #-}
  atomicSubFetchMutableByteArray# mba# i# a = unsafePrimBase (syncSubFetchWord16ArrayIO mba# i# a)
  {-# INLINE atomicSubFetchMutableByteArray# #-}
  atomicFetchAddOffAddr# addr# i# a = unsafePrimBase (syncFetchAddWord16AddrIO addr# i# a)
  {-# INLINE atomicFetchAddOffAddr# #-}
  atomicAddFetchOffAddr# addr# i# a = unsafePrimBase (syncAddFetchWord16AddrIO addr# i# a)
  {-# INLINE atomicAddFetchOffAddr# #-}
  atomicFetchSubOffAddr# addr# i# a = unsafePrimBase (syncFetchSubWord16AddrIO addr# i# a)
  {-# INLINE atomicFetchSubOffAddr# #-}
  atomicSubFetchOffAddr# addr# i# a = unsafePrimBase (syncSubFetchWord16AddrIO addr# i# a)
  {-# INLINE atomicSubFetchOffAddr# #-}

instance AtomicBits Word16 where
  atomicFetchAndMutableByteArray# mba# i# a = unsafePrimBase (syncFetchAndWord16ArrayIO mba# i# a)
  {-# INLINE atomicFetchAndMutableByteArray# #-}
  atomicAndFetchMutableByteArray# mba# i# a = unsafePrimBase (syncAndFetchWord16ArrayIO mba# i# a)
  {-# INLINE atomicAndFetchMutableByteArray# #-}
  atomicFetchNandMutableByteArray# mba# i# a = unsafePrimBase (syncFetchNandWord16ArrayIO mba# i# a)
  {-# INLINE atomicFetchNandMutableByteArray# #-}
  atomicNandFetchMutableByteArray# mba# i# a = unsafePrimBase (syncNandFetchWord16ArrayIO mba# i# a)
  {-# INLINE atomicNandFetchMutableByteArray# #-}
  atomicFetchOrMutableByteArray# mba# i# a = unsafePrimBase (syncFetchOrWord16ArrayIO mba# i# a)
  {-# INLINE atomicFetchOrMutableByteArray# #-}
  atomicOrFetchMutableByteArray# mba# i# a = unsafePrimBase (syncOrFetchWord16ArrayIO mba# i# a)
  {-# INLINE atomicOrFetchMutableByteArray# #-}
  atomicFetchXorMutableByteArray# mba# i# a = unsafePrimBase (syncFetchXorWord16ArrayIO mba# i# a)
  {-# INLINE atomicFetchXorMutableByteArray# #-}
  atomicXorFetchMutableByteArray# mba# i# a = unsafePrimBase (syncXorFetchWord16ArrayIO mba# i# a)
  {-# INLINE atomicXorFetchMutableByteArray# #-}
  atomicFetchAndOffAddr# addr# i# a = unsafePrimBase (syncFetchAndWord16AddrIO addr# i# a)
  {-# INLINE atomicFetchAndOffAddr# #-}
  atomicAndFetchOffAddr# addr# i# a = unsafePrimBase (syncAndFetchWord16AddrIO addr# i# a)
  {-# INLINE atomicAndFetchOffAddr# #-}
  atomicFetchNandOffAddr# addr# i# a = unsafePrimBase (syncFetchNandWord16AddrIO addr# i# a)
  {-# INLINE atomicFetchNandOffAddr# #-}
  atomicNandFetchOffAddr# addr# i# a = unsafePrimBase (syncNandFetchWord16AddrIO addr# i# a)
  {-# INLINE atomicNandFetchOffAddr# #-}
  atomicFetchOrOffAddr# addr# i# a = unsafePrimBase (syncFetchOrWord16AddrIO addr# i# a)
  {-# INLINE atomicFetchOrOffAddr# #-}
  atomicOrFetchOffAddr# addr# i# a = unsafePrimBase (syncOrFetchWord16AddrIO addr# i# a)
  {-# INLINE atomicOrFetchOffAddr# #-}
  atomicFetchXorOffAddr# addr# i# a = unsafePrimBase (syncFetchXorWord16AddrIO addr# i# a)
  {-# INLINE atomicFetchXorOffAddr# #-}
  atomicXorFetchOffAddr# addr# i# a = unsafePrimBase (syncXorFetchWord16AddrIO addr# i# a)
  {-# INLINE atomicXorFetchOffAddr# #-}


instance Atomic Word32 where
  casMutableByteArray# mba# i# old new = unsafePrimBase (syncCasWord32ArrayIO mba# i# old new)
  {-# INLINE casMutableByteArray# #-}
  casOffAddr# addr# i# old new = unsafePrimBase (syncCasWord32AddrIO addr# i# old new)
  {-# INLINE casOffAddr# #-}

instance AtomicCount Word32 where
  atomicFetchAddMutableByteArray# mba# i# a = unsafePrimBase (syncFetchAddWord32ArrayIO mba# i# a)
  {-# INLINE atomicFetchAddMutableByteArray# #-}
  atomicAddFetchMutableByteArray# mba# i# a = unsafePrimBase (syncAddFetchWord32ArrayIO mba# i# a)
  {-# INLINE atomicAddFetchMutableByteArray# #-}
  atomicFetchSubMutableByteArray# mba# i# a = unsafePrimBase (syncFetchSubWord32ArrayIO mba# i# a)
  {-# INLINE atomicFetchSubMutableByteArray# #-}
  atomicSubFetchMutableByteArray# mba# i# a = unsafePrimBase (syncSubFetchWord32ArrayIO mba# i# a)
  {-# INLINE atomicSubFetchMutableByteArray# #-}
  atomicFetchAddOffAddr# addr# i# a = unsafePrimBase (syncFetchAddWord32AddrIO addr# i# a)
  {-# INLINE atomicFetchAddOffAddr# #-}
  atomicAddFetchOffAddr# addr# i# a = unsafePrimBase (syncAddFetchWord32AddrIO addr# i# a)
  {-# INLINE atomicAddFetchOffAddr# #-}
  atomicFetchSubOffAddr# addr# i# a = unsafePrimBase (syncFetchSubWord32AddrIO addr# i# a)
  {-# INLINE atomicFetchSubOffAddr# #-}
  atomicSubFetchOffAddr# addr# i# a = unsafePrimBase (syncSubFetchWord32AddrIO addr# i# a)
  {-# INLINE atomicSubFetchOffAddr# #-}

instance AtomicBits Word32 where
  atomicFetchAndMutableByteArray# mba# i# a = unsafePrimBase (syncFetchAndWord32ArrayIO mba# i# a)
  {-# INLINE atomicFetchAndMutableByteArray# #-}
  atomicAndFetchMutableByteArray# mba# i# a = unsafePrimBase (syncAndFetchWord32ArrayIO mba# i# a)
  {-# INLINE atomicAndFetchMutableByteArray# #-}
  atomicFetchNandMutableByteArray# mba# i# a = unsafePrimBase (syncFetchNandWord32ArrayIO mba# i# a)
  {-# INLINE atomicFetchNandMutableByteArray# #-}
  atomicNandFetchMutableByteArray# mba# i# a = unsafePrimBase (syncNandFetchWord32ArrayIO mba# i# a)
  {-# INLINE atomicNandFetchMutableByteArray# #-}
  atomicFetchOrMutableByteArray# mba# i# a = unsafePrimBase (syncFetchOrWord32ArrayIO mba# i# a)
  {-# INLINE atomicFetchOrMutableByteArray# #-}
  atomicOrFetchMutableByteArray# mba# i# a = unsafePrimBase (syncOrFetchWord32ArrayIO mba# i# a)
  {-# INLINE atomicOrFetchMutableByteArray# #-}
  atomicFetchXorMutableByteArray# mba# i# a = unsafePrimBase (syncFetchXorWord32ArrayIO mba# i# a)
  {-# INLINE atomicFetchXorMutableByteArray# #-}
  atomicXorFetchMutableByteArray# mba# i# a = unsafePrimBase (syncXorFetchWord32ArrayIO mba# i# a)
  {-# INLINE atomicXorFetchMutableByteArray# #-}
  atomicFetchAndOffAddr# addr# i# a = unsafePrimBase (syncFetchAndWord32AddrIO addr# i# a)
  {-# INLINE atomicFetchAndOffAddr# #-}
  atomicAndFetchOffAddr# addr# i# a = unsafePrimBase (syncAndFetchWord32AddrIO addr# i# a)
  {-# INLINE atomicAndFetchOffAddr# #-}
  atomicFetchNandOffAddr# addr# i# a = unsafePrimBase (syncFetchNandWord32AddrIO addr# i# a)
  {-# INLINE atomicFetchNandOffAddr# #-}
  atomicNandFetchOffAddr# addr# i# a = unsafePrimBase (syncNandFetchWord32AddrIO addr# i# a)
  {-# INLINE atomicNandFetchOffAddr# #-}
  atomicFetchOrOffAddr# addr# i# a = unsafePrimBase (syncFetchOrWord32AddrIO addr# i# a)
  {-# INLINE atomicFetchOrOffAddr# #-}
  atomicOrFetchOffAddr# addr# i# a = unsafePrimBase (syncOrFetchWord32AddrIO addr# i# a)
  {-# INLINE atomicOrFetchOffAddr# #-}
  atomicFetchXorOffAddr# addr# i# a = unsafePrimBase (syncFetchXorWord32AddrIO addr# i# a)
  {-# INLINE atomicFetchXorOffAddr# #-}
  atomicXorFetchOffAddr# addr# i# a = unsafePrimBase (syncXorFetchWord32AddrIO addr# i# a)
  {-# INLINE atomicXorFetchOffAddr# #-}


instance Atomic Word where
  casMutableByteArray# mba# i# old new = unsafePrimBase (syncCasWordArrayIO mba# i# old new)
  {-# INLINE casMutableByteArray# #-}
  casOffAddr# addr# i# old new = unsafePrimBase (syncCasWordAddrIO addr# i# old new)
  {-# INLINE casOffAddr# #-}

instance AtomicCount Word where
  atomicFetchAddMutableByteArray# mba# i# a = unsafePrimBase (syncFetchAddWordArrayIO mba# i# a)
  {-# INLINE atomicFetchAddMutableByteArray# #-}
  atomicAddFetchMutableByteArray# mba# i# a = unsafePrimBase (syncAddFetchWordArrayIO mba# i# a)
  {-# INLINE atomicAddFetchMutableByteArray# #-}
  atomicFetchSubMutableByteArray# mba# i# a = unsafePrimBase (syncFetchSubWordArrayIO mba# i# a)
  {-# INLINE atomicFetchSubMutableByteArray# #-}
  atomicSubFetchMutableByteArray# mba# i# a = unsafePrimBase (syncSubFetchWordArrayIO mba# i# a)
  {-# INLINE atomicSubFetchMutableByteArray# #-}
  atomicFetchAddOffAddr# addr# i# a = unsafePrimBase (syncFetchAddWordAddrIO addr# i# a)
  {-# INLINE atomicFetchAddOffAddr# #-}
  atomicAddFetchOffAddr# addr# i# a = unsafePrimBase (syncAddFetchWordAddrIO addr# i# a)
  {-# INLINE atomicAddFetchOffAddr# #-}
  atomicFetchSubOffAddr# addr# i# a = unsafePrimBase (syncFetchSubWordAddrIO addr# i# a)
  {-# INLINE atomicFetchSubOffAddr# #-}
  atomicSubFetchOffAddr# addr# i# a = unsafePrimBase (syncSubFetchWordAddrIO addr# i# a)
  {-# INLINE atomicSubFetchOffAddr# #-}

instance AtomicBits Word where
  atomicFetchAndMutableByteArray# mba# i# a = unsafePrimBase (syncFetchAndWordArrayIO mba# i# a)
  {-# INLINE atomicFetchAndMutableByteArray# #-}
  atomicAndFetchMutableByteArray# mba# i# a = unsafePrimBase (syncAndFetchWordArrayIO mba# i# a)
  {-# INLINE atomicAndFetchMutableByteArray# #-}
  atomicFetchNandMutableByteArray# mba# i# a = unsafePrimBase (syncFetchNandWordArrayIO mba# i# a)
  {-# INLINE atomicFetchNandMutableByteArray# #-}
  atomicNandFetchMutableByteArray# mba# i# a = unsafePrimBase (syncNandFetchWordArrayIO mba# i# a)
  {-# INLINE atomicNandFetchMutableByteArray# #-}
  atomicFetchOrMutableByteArray# mba# i# a = unsafePrimBase (syncFetchOrWordArrayIO mba# i# a)
  {-# INLINE atomicFetchOrMutableByteArray# #-}
  atomicOrFetchMutableByteArray# mba# i# a = unsafePrimBase (syncOrFetchWordArrayIO mba# i# a)
  {-# INLINE atomicOrFetchMutableByteArray# #-}
  atomicFetchXorMutableByteArray# mba# i# a = unsafePrimBase (syncFetchXorWordArrayIO mba# i# a)
  {-# INLINE atomicFetchXorMutableByteArray# #-}
  atomicXorFetchMutableByteArray# mba# i# a = unsafePrimBase (syncXorFetchWordArrayIO mba# i# a)
  {-# INLINE atomicXorFetchMutableByteArray# #-}
  atomicFetchAndOffAddr# addr# i# a = unsafePrimBase (syncFetchAndWordAddrIO addr# i# a)
  {-# INLINE atomicFetchAndOffAddr# #-}
  atomicAndFetchOffAddr# addr# i# a = unsafePrimBase (syncAndFetchWordAddrIO addr# i# a)
  {-# INLINE atomicAndFetchOffAddr# #-}
  atomicFetchNandOffAddr# addr# i# a = unsafePrimBase (syncFetchNandWordAddrIO addr# i# a)
  {-# INLINE atomicFetchNandOffAddr# #-}
  atomicNandFetchOffAddr# addr# i# a = unsafePrimBase (syncNandFetchWordAddrIO addr# i# a)
  {-# INLINE atomicNandFetchOffAddr# #-}
  atomicFetchOrOffAddr# addr# i# a = unsafePrimBase (syncFetchOrWordAddrIO addr# i# a)
  {-# INLINE atomicFetchOrOffAddr# #-}
  atomicOrFetchOffAddr# addr# i# a = unsafePrimBase (syncOrFetchWordAddrIO addr# i# a)
  {-# INLINE atomicOrFetchOffAddr# #-}
  atomicFetchXorOffAddr# addr# i# a = unsafePrimBase (syncFetchXorWordAddrIO addr# i# a)
  {-# INLINE atomicFetchXorOffAddr# #-}
  atomicXorFetchOffAddr# addr# i# a = unsafePrimBase (syncXorFetchWordAddrIO addr# i# a)
  {-# INLINE atomicXorFetchOffAddr# #-}

#if WORD_SIZE_IN_BITS == 64
-- | Available only on 64bit architectures

instance Atomic Int64 where
  casMutableByteArray# mba# i# old new = unsafePrimBase (syncCasInt64ArrayIO mba# i# old new)
  {-# INLINE casMutableByteArray# #-}
  casOffAddr# addr# i# old new = unsafePrimBase (syncCasInt64AddrIO addr# i# old new)
  {-# INLINE casOffAddr# #-}

instance AtomicCount Int64 where
  atomicFetchAddMutableByteArray# mba# i# a = unsafePrimBase (syncFetchAddInt64ArrayIO mba# i# a)
  {-# INLINE atomicFetchAddMutableByteArray# #-}
  atomicAddFetchMutableByteArray# mba# i# a = unsafePrimBase (syncAddFetchInt64ArrayIO mba# i# a)
  {-# INLINE atomicAddFetchMutableByteArray# #-}
  atomicFetchSubMutableByteArray# mba# i# a = unsafePrimBase (syncFetchSubInt64ArrayIO mba# i# a)
  {-# INLINE atomicFetchSubMutableByteArray# #-}
  atomicSubFetchMutableByteArray# mba# i# a = unsafePrimBase (syncSubFetchInt64ArrayIO mba# i# a)
  {-# INLINE atomicSubFetchMutableByteArray# #-}
  atomicFetchAddOffAddr# addr# i# a = unsafePrimBase (syncFetchAddInt64AddrIO addr# i# a)
  {-# INLINE atomicFetchAddOffAddr# #-}
  atomicAddFetchOffAddr# addr# i# a = unsafePrimBase (syncAddFetchInt64AddrIO addr# i# a)
  {-# INLINE atomicAddFetchOffAddr# #-}
  atomicFetchSubOffAddr# addr# i# a = unsafePrimBase (syncFetchSubInt64AddrIO addr# i# a)
  {-# INLINE atomicFetchSubOffAddr# #-}
  atomicSubFetchOffAddr# addr# i# a = unsafePrimBase (syncSubFetchInt64AddrIO addr# i# a)
  {-# INLINE atomicSubFetchOffAddr# #-}

instance AtomicBits Int64 where
  atomicFetchAndMutableByteArray# mba# i# a = unsafePrimBase (syncFetchAndInt64ArrayIO mba# i# a)
  {-# INLINE atomicFetchAndMutableByteArray# #-}
  atomicAndFetchMutableByteArray# mba# i# a = unsafePrimBase (syncAndFetchInt64ArrayIO mba# i# a)
  {-# INLINE atomicAndFetchMutableByteArray# #-}
  atomicFetchNandMutableByteArray# mba# i# a = unsafePrimBase (syncFetchNandInt64ArrayIO mba# i# a)
  {-# INLINE atomicFetchNandMutableByteArray# #-}
  atomicNandFetchMutableByteArray# mba# i# a = unsafePrimBase (syncNandFetchInt64ArrayIO mba# i# a)
  {-# INLINE atomicNandFetchMutableByteArray# #-}
  atomicFetchOrMutableByteArray# mba# i# a = unsafePrimBase (syncFetchOrInt64ArrayIO mba# i# a)
  {-# INLINE atomicFetchOrMutableByteArray# #-}
  atomicOrFetchMutableByteArray# mba# i# a = unsafePrimBase (syncOrFetchInt64ArrayIO mba# i# a)
  {-# INLINE atomicOrFetchMutableByteArray# #-}
  atomicFetchXorMutableByteArray# mba# i# a = unsafePrimBase (syncFetchXorInt64ArrayIO mba# i# a)
  {-# INLINE atomicFetchXorMutableByteArray# #-}
  atomicXorFetchMutableByteArray# mba# i# a = unsafePrimBase (syncXorFetchInt64ArrayIO mba# i# a)
  {-# INLINE atomicXorFetchMutableByteArray# #-}
  atomicFetchAndOffAddr# addr# i# a = unsafePrimBase (syncFetchAndInt64AddrIO addr# i# a)
  {-# INLINE atomicFetchAndOffAddr# #-}
  atomicAndFetchOffAddr# addr# i# a = unsafePrimBase (syncAndFetchInt64AddrIO addr# i# a)
  {-# INLINE atomicAndFetchOffAddr# #-}
  atomicFetchNandOffAddr# addr# i# a = unsafePrimBase (syncFetchNandInt64AddrIO addr# i# a)
  {-# INLINE atomicFetchNandOffAddr# #-}
  atomicNandFetchOffAddr# addr# i# a = unsafePrimBase (syncNandFetchInt64AddrIO addr# i# a)
  {-# INLINE atomicNandFetchOffAddr# #-}
  atomicFetchOrOffAddr# addr# i# a = unsafePrimBase (syncFetchOrInt64AddrIO addr# i# a)
  {-# INLINE atomicFetchOrOffAddr# #-}
  atomicOrFetchOffAddr# addr# i# a = unsafePrimBase (syncOrFetchInt64AddrIO addr# i# a)
  {-# INLINE atomicOrFetchOffAddr# #-}
  atomicFetchXorOffAddr# addr# i# a = unsafePrimBase (syncFetchXorInt64AddrIO addr# i# a)
  {-# INLINE atomicFetchXorOffAddr# #-}
  atomicXorFetchOffAddr# addr# i# a = unsafePrimBase (syncXorFetchInt64AddrIO addr# i# a)
  {-# INLINE atomicXorFetchOffAddr# #-}

-- | Available only on 64bit architectures


instance Atomic Word64 where
  casMutableByteArray# mba# i# old new = unsafePrimBase (syncCasWord64ArrayIO mba# i# old new)
  {-# INLINE casMutableByteArray# #-}
  casOffAddr# addr# i# old new = unsafePrimBase (syncCasWord64AddrIO addr# i# old new)
  {-# INLINE casOffAddr# #-}

instance AtomicCount Word64 where
  atomicFetchAddMutableByteArray# mba# i# a = unsafePrimBase (syncFetchAddWord64ArrayIO mba# i# a)
  {-# INLINE atomicFetchAddMutableByteArray# #-}
  atomicAddFetchMutableByteArray# mba# i# a = unsafePrimBase (syncAddFetchWord64ArrayIO mba# i# a)
  {-# INLINE atomicAddFetchMutableByteArray# #-}
  atomicFetchSubMutableByteArray# mba# i# a = unsafePrimBase (syncFetchSubWord64ArrayIO mba# i# a)
  {-# INLINE atomicFetchSubMutableByteArray# #-}
  atomicSubFetchMutableByteArray# mba# i# a = unsafePrimBase (syncSubFetchWord64ArrayIO mba# i# a)
  {-# INLINE atomicSubFetchMutableByteArray# #-}
  atomicFetchAddOffAddr# addr# i# a = unsafePrimBase (syncFetchAddWord64AddrIO addr# i# a)
  {-# INLINE atomicFetchAddOffAddr# #-}
  atomicAddFetchOffAddr# addr# i# a = unsafePrimBase (syncAddFetchWord64AddrIO addr# i# a)
  {-# INLINE atomicAddFetchOffAddr# #-}
  atomicFetchSubOffAddr# addr# i# a = unsafePrimBase (syncFetchSubWord64AddrIO addr# i# a)
  {-# INLINE atomicFetchSubOffAddr# #-}
  atomicSubFetchOffAddr# addr# i# a = unsafePrimBase (syncSubFetchWord64AddrIO addr# i# a)
  {-# INLINE atomicSubFetchOffAddr# #-}

instance AtomicBits Word64 where
  atomicFetchAndMutableByteArray# mba# i# a = unsafePrimBase (syncFetchAndWord64ArrayIO mba# i# a)
  {-# INLINE atomicFetchAndMutableByteArray# #-}
  atomicAndFetchMutableByteArray# mba# i# a = unsafePrimBase (syncAndFetchWord64ArrayIO mba# i# a)
  {-# INLINE atomicAndFetchMutableByteArray# #-}
  atomicFetchNandMutableByteArray# mba# i# a = unsafePrimBase (syncFetchNandWord64ArrayIO mba# i# a)
  {-# INLINE atomicFetchNandMutableByteArray# #-}
  atomicNandFetchMutableByteArray# mba# i# a = unsafePrimBase (syncNandFetchWord64ArrayIO mba# i# a)
  {-# INLINE atomicNandFetchMutableByteArray# #-}
  atomicFetchOrMutableByteArray# mba# i# a = unsafePrimBase (syncFetchOrWord64ArrayIO mba# i# a)
  {-# INLINE atomicFetchOrMutableByteArray# #-}
  atomicOrFetchMutableByteArray# mba# i# a = unsafePrimBase (syncOrFetchWord64ArrayIO mba# i# a)
  {-# INLINE atomicOrFetchMutableByteArray# #-}
  atomicFetchXorMutableByteArray# mba# i# a = unsafePrimBase (syncFetchXorWord64ArrayIO mba# i# a)
  {-# INLINE atomicFetchXorMutableByteArray# #-}
  atomicXorFetchMutableByteArray# mba# i# a = unsafePrimBase (syncXorFetchWord64ArrayIO mba# i# a)
  {-# INLINE atomicXorFetchMutableByteArray# #-}
  atomicFetchAndOffAddr# addr# i# a = unsafePrimBase (syncFetchAndWord64AddrIO addr# i# a)
  {-# INLINE atomicFetchAndOffAddr# #-}
  atomicAndFetchOffAddr# addr# i# a = unsafePrimBase (syncAndFetchWord64AddrIO addr# i# a)
  {-# INLINE atomicAndFetchOffAddr# #-}
  atomicFetchNandOffAddr# addr# i# a = unsafePrimBase (syncFetchNandWord64AddrIO addr# i# a)
  {-# INLINE atomicFetchNandOffAddr# #-}
  atomicNandFetchOffAddr# addr# i# a = unsafePrimBase (syncNandFetchWord64AddrIO addr# i# a)
  {-# INLINE atomicNandFetchOffAddr# #-}
  atomicFetchOrOffAddr# addr# i# a = unsafePrimBase (syncFetchOrWord64AddrIO addr# i# a)
  {-# INLINE atomicFetchOrOffAddr# #-}
  atomicOrFetchOffAddr# addr# i# a = unsafePrimBase (syncOrFetchWord64AddrIO addr# i# a)
  {-# INLINE atomicOrFetchOffAddr# #-}
  atomicFetchXorOffAddr# addr# i# a = unsafePrimBase (syncFetchXorWord64AddrIO addr# i# a)
  {-# INLINE atomicFetchXorOffAddr# #-}
  atomicXorFetchOffAddr# addr# i# a = unsafePrimBase (syncXorFetchWord64AddrIO addr# i# a)
  {-# INLINE atomicXorFetchOffAddr# #-}
#endif

instance Atomic Bool
instance AtomicBits Bool

instance Atomic Char

instance Atomic (Ptr a)

instance Atomic (FunPtr a)

instance Atomic IntPtr
instance AtomicCount IntPtr
instance AtomicBits IntPtr

instance Atomic WordPtr
instance AtomicCount WordPtr
instance AtomicBits WordPtr


instance Atomic CBool
instance AtomicCount CBool
instance AtomicBits CBool

instance Atomic CChar
instance AtomicCount CChar
instance AtomicBits CChar

instance Atomic CSChar
instance AtomicCount CSChar
instance AtomicBits CSChar

instance Atomic CUChar
instance AtomicCount CUChar
instance AtomicBits CUChar

instance Atomic CShort
instance AtomicCount CShort
instance AtomicBits CShort

instance Atomic CUShort
instance AtomicCount CUShort
instance AtomicBits CUShort

instance Atomic CInt
instance AtomicCount CInt
instance AtomicBits CInt

instance Atomic CUInt
instance AtomicCount CUInt
instance AtomicBits CUInt

instance Atomic CLong
instance AtomicCount CLong
instance AtomicBits CLong

instance Atomic CULong
instance AtomicCount CULong
instance AtomicBits CULong

instance Atomic CLLong
instance AtomicCount CLLong
instance AtomicBits CLLong

instance Atomic CULLong
instance AtomicCount CULLong
instance AtomicBits CULLong

instance Atomic CPtrdiff
instance AtomicCount CPtrdiff
instance AtomicBits CPtrdiff

instance Atomic CSize
instance AtomicCount CSize
instance AtomicBits CSize

instance Atomic CWchar
instance AtomicCount CWchar
instance AtomicBits CWchar

instance Atomic CSigAtomic
instance AtomicCount CSigAtomic
instance AtomicBits CSigAtomic

instance Atomic CIntPtr
instance AtomicCount CIntPtr
instance AtomicBits CIntPtr

instance Atomic CUIntPtr
instance AtomicCount CUIntPtr
instance AtomicBits CUIntPtr

instance Atomic CIntMax
instance AtomicCount CIntMax
instance AtomicBits CIntMax

instance Atomic CUIntMax
instance AtomicCount CUIntMax
instance AtomicBits CUIntMax

instance Atomic Fd
instance AtomicCount Fd
instance AtomicBits Fd

instance Atomic Errno
instance AtomicCount Errno



#if defined(HTYPE_DEV_T)
instance Atomic CDev
instance AtomicCount CDev
instance AtomicBits CDev
#endif
#if defined(HTYPE_INO_T)
instance Atomic CIno
instance AtomicCount CIno
instance AtomicBits CIno
#endif
#if defined(HTYPE_MODE_T)
instance Atomic CMode
instance AtomicCount CMode
instance AtomicBits CMode
#endif
#if defined(HTYPE_OFF_T)
instance Atomic COff
instance AtomicCount COff
instance AtomicBits COff
#endif
#if defined(HTYPE_PID_T)
instance Atomic CPid
instance AtomicCount CPid
instance AtomicBits CPid
#endif
#if defined(HTYPE_SSIZE_T)
instance Atomic CSsize
instance AtomicCount CSsize
instance AtomicBits CSsize
#endif
#if defined(HTYPE_GID_T)
instance Atomic CGid
instance AtomicCount CGid
instance AtomicBits CGid
#endif
#if defined(HTYPE_NLINK_T)
instance Atomic CNlink
instance AtomicCount CNlink
instance AtomicBits CNlink
#endif
#if defined(HTYPE_UID_T)
instance Atomic CUid
instance AtomicCount CUid
instance AtomicBits CUid
#endif
#if defined(HTYPE_CC_T)
instance Atomic CCc
instance AtomicCount CCc
instance AtomicBits CCc
#endif
#if defined(HTYPE_SPEED_T)
instance Atomic CSpeed
instance AtomicCount CSpeed
instance AtomicBits CSpeed
#endif
#if defined(HTYPE_TCFLAG_T)
instance Atomic CTcflag
instance AtomicCount CTcflag
instance AtomicBits CTcflag
#endif
#if defined(HTYPE_RLIM_T)
instance Atomic CRLim
instance AtomicCount CRLim
instance AtomicBits CRLim
#endif

#if __GLASGOW_HASKELL__ >= 802

#if defined(HTYPE_BLKSIZE_T)
instance Atomic CBlkSize
instance AtomicCount CBlkSize
instance AtomicBits CBlkSize
#endif
#if defined(HTYPE_BLKCNT_T)
instance Atomic CBlkCnt
instance AtomicCount CBlkCnt
instance AtomicBits CBlkCnt
#endif
#if defined(HTYPE_CLOCKID_T)
instance Atomic CClockId
instance AtomicCount CClockId
instance AtomicBits CClockId
#endif
#if defined(HTYPE_FSBLKCNT_T)
instance Atomic CFsBlkCnt
instance AtomicCount CFsBlkCnt
instance AtomicBits CFsBlkCnt
#endif
#if defined(HTYPE_FSFILCNT_T)
instance Atomic CFsFilCnt
instance AtomicCount CFsFilCnt
instance AtomicBits CFsFilCnt
#endif
#if defined(HTYPE_ID_T)
instance Atomic CId
instance AtomicCount CId
instance AtomicBits CId
#endif
#if defined(HTYPE_KEY_T)
instance Atomic CKey
instance AtomicCount CKey
instance AtomicBits CKey
#endif
#if defined(HTYPE_TIMER_T)
instance Atomic CTimer
instance AtomicCount CTimer
instance AtomicBits CTimer
#endif

#if __GLASGOW_HASKELL__ >= 810

#if defined(HTYPE_SOCKLEN_T)
instance Atomic CSocklen
instance AtomicCount CSocklen
instance AtomicBits CSocklen
#endif
#if defined(HTYPE_NFDS_T)
instance Atomic CNfds
instance AtomicCount CNfds
instance AtomicBIts CNfds
#endif

#endif /* __GLASGOW_HASKELL__ >= 810 */

#endif /* __GLASGOW_HASKELL__ >= 802 */

#if __GLASGOW_HASKELL__ >= 800
instance Atomic a => Atomic (Max a)
instance Atomic a => Atomic (Min a)
instance Atomic a => Atomic (Data.Semigroup.First a)
instance Atomic a => Atomic (Data.Semigroup.Last a)

instance Atomic a => Atomic (Const a b)
instance AtomicCount a => AtomicCount (Const a b)
instance AtomicBits a => AtomicBits (Const a b)

#else

deriving instance Bits a => Bits (Identity a)
#endif /* __GLASGOW_HASKELL__ >= 800 */

instance Atomic a => Atomic (Identity a)
instance AtomicCount a => AtomicCount (Identity a)
instance AtomicBits a => AtomicBits (Identity a)

instance Atomic Ordering

-- instance Atomic IODeviceType

instance Atomic SeekMode

-- instance Atomic BlockReason

instance Atomic a => Atomic (Down a)
instance AtomicCount a => AtomicCount (Down a)
--instance AtomicBits a => AtomicBits (Down a)

instance Atomic a => Atomic (Dual a)
instance AtomicCount a => AtomicCount (Dual a)
--instance AtomicBits a => AtomicBits (Dual a)

instance Atomic a => Atomic (Sum a)
instance AtomicCount a => AtomicCount (Sum a)
--instance AtomicBits a => AtomicBits (Sum a)

instance Atomic a => Atomic (Product a)
instance AtomicCount a => AtomicCount (Product a)
--instance AtomicBits a => AtomicBits (Product a)

instance Atomic All
--instance AtomicBits All

instance Atomic Any
--instance AtomicBits Any
