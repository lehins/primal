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
  , atomicBoolModifyMutableByteArray#
  , atomicModifyMutableByteArray#
  , atomicModifyMutableByteArray_#
  , atomicModifyFetchOldMutableByteArray#
  , atomicModifyFetchNewMutableByteArray#
  , atomicModifyOffAddr#
  , atomicModifyOffAddr_#
  , atomicBoolModifyOffAddr#
  , atomicModifyFetchOldOffAddr#
  , atomicModifyFetchNewOffAddr#
  , atomicNotFetchOldMutableByteArray#
  , atomicNotFetchNewMutableByteArray#
  , atomicNotFetchOldOffAddr#
  , atomicNotFetchNewOffAddr#
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


  casBoolMutableByteArray# ::
       MutableByteArray# s -- ^ Array to be mutated
    -> Int# -- ^ Offset into the array in number of elements
    -> a -- ^ Expected old value
    -> a -- ^ New value
    -> State# s -- ^ Starting state
    -> (# State# s, Bool #)
  default casBoolMutableByteArray#
    :: Atomic (PrimBase a)
    => MutableByteArray# s
    -> Int#
    -> a
    -> a
    -> State# s
    -> (# State# s, Bool #)
  casBoolMutableByteArray# mba# i# old new s =
    case casBoolMutableByteArray# mba# i# (toPrimBase old) (toPrimBase new) s of
      (# s', casSucc #) -> (# s', casSucc #)
  {-# INLINE casBoolMutableByteArray# #-}

  casBoolOffAddr# :: Addr# -> Int# -> a -> a -> State# s -> (# State# s, Bool #)
  default casBoolOffAddr# ::
    Atomic (PrimBase a) => Addr# -> Int# -> a -> a -> State# s -> (# State# s, Bool #)
  casBoolOffAddr# addr# i# old new s =
    case casBoolOffAddr# addr# i# (toPrimBase old) (toPrimBase new) s of
      (# s', casSucc #) -> (# s', casSucc #)
  {-# INLINE casBoolOffAddr# #-}


-- | Using `casBoolMutableByteArray#` perform atomic modification of an element in a
-- `MutableByteArray#`. This is essentially an implementation of a spinlock using CAS.
--
-- @since 0.1.0
atomicBoolModifyMutableByteArray# ::
     Atomic a =>
     MutableByteArray# s -- ^ Array to be mutated
  -> Int# -- ^ Index in number of `Int#` elements into the `MutableByteArray#`
  -> (a -> (# a, b #)) -- ^ Function to be applied atomically to the element
  -> State# s -- ^ Starting state
  -> (# State# s, b #)
atomicBoolModifyMutableByteArray# mba# i# f s0 =
  let go s o =
        case f o of
          (# n, artifact #) ->
            case casBoolMutableByteArray# mba# i# o n s of
              (# s', isCasSucc #) ->
                if isCasSucc
                  then (# s', artifact #)
                  else case atomicReadMutableByteArray# mba# i# s' of
                         (# s'', o' #) -> go s'' o'
   in case atomicReadMutableByteArray# mba# i# s0 of
        (# s', o #) -> go s' o
{-# INLINE atomicBoolModifyMutableByteArray# #-}


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
atomicModifyFetchOldMutableByteArray# ::
     Atomic a =>
     MutableByteArray# s -- ^ Array to be mutated
  -> Int# -- ^ Index in number of `Int#` elements into the `MutableByteArray#`
  -> (a -> a) -- ^ Function to be applied atomically to the element
  -> State# s -- ^ Starting state
  -> (# State# s, a #)
atomicModifyFetchOldMutableByteArray# mba# i# f =
  atomicModifyMutableByteArray# mba# i# (\a -> let a' = f a in seq a' (# a', a #))
{-# INLINE atomicModifyFetchOldMutableByteArray# #-}


-- | Using `casMutableByteArray#` perform atomic modification of an element in a
-- `MutableByteArray#`. Returns the new value.
--
-- @since 0.1.0
atomicModifyFetchNewMutableByteArray# ::
     Atomic a =>
     MutableByteArray# s -- ^ Array to be mutated
  -> Int# -- ^ Index in number of `Int#` elements into the `MutableByteArray#`
  -> (a -> a) -- ^ Function to be applied atomically to the element
  -> State# s -- ^ Starting state
  -> (# State# s, a #)
atomicModifyFetchNewMutableByteArray# mba# i# f =
  atomicModifyMutableByteArray# mba# i# (\a -> let a' = f a in seq a' (# a', a' #))
{-# INLINE atomicModifyFetchNewMutableByteArray# #-}

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


-- | Using `casBoolOffAddr#` perform atomic modification of an element in a
-- `OffAddr#`. This is essentially an implementation of a spinlock using CAS.
--
-- @since 0.1.0
atomicBoolModifyOffAddr# ::
     Atomic a =>
     Addr# -- ^ Array to be mutated
  -> Int# -- ^ Index in number of `Int#` elements into the `OffAddr#`
  -> (a -> (# a, b #)) -- ^ Function to be applied atomically to the element
  -> State# s -- ^ Starting state
  -> (# State# s, b #)
atomicBoolModifyOffAddr# addr# i# f s0 =
  let go s o =
        case f o of
          (# n, artifact #) ->
            case casBoolOffAddr# addr# i# o n s of
              (# s', isCasSucc #) ->
                if isCasSucc
                  then (# s', artifact #)
                  else case atomicReadOffAddr# addr# i# s' of
                         (# s'', o' #) -> go s'' o'
   in case atomicReadOffAddr# addr# i# s0 of
        (# s', o #) -> go s' o
{-# INLINE atomicBoolModifyOffAddr# #-}

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
atomicModifyFetchOldOffAddr# ::
     Atomic a =>
     Addr# -- ^ Array to be mutated
  -> Int# -- ^ Index in number of `Int#` elements into the `OffAddr#`
  -> (a -> a) -- ^ Function to be applied atomically to the element
  -> State# s -- ^ Starting state
  -> (# State# s, a #)
atomicModifyFetchOldOffAddr# addr# i# f =
  atomicModifyOffAddr# addr# i# (\a -> let a' = f a in seq a' (# a', a #))
{-# INLINE atomicModifyFetchOldOffAddr# #-}


-- | Using `casOffAddr#` perform atomic modification of an element in a
-- `OffAddr#`. Returns the new value.
--
-- @since 0.1.0
atomicModifyFetchNewOffAddr# ::
     Atomic a =>
     Addr# -- ^ Array to be mutated
  -> Int# -- ^ Index in number of `Int#` elements into the `OffAddr#`
  -> (a -> a) -- ^ Function to be applied atomically to the element
  -> State# s -- ^ Starting state
  -> (# State# s, a #)
atomicModifyFetchNewOffAddr# addr# i# f =
  atomicModifyOffAddr# addr# i# (\a -> let a' = f a in seq a' (# a', a' #))
{-# INLINE atomicModifyFetchNewOffAddr# #-}

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
  atomicAddFetchOldMutableByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicAddFetchOldMutableByteArray# ::
    AtomicCount (PrimBase a) => MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  atomicAddFetchOldMutableByteArray# mba# i# x s =
    case atomicAddFetchOldMutableByteArray# mba# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicAddFetchOldMutableByteArray# #-}

  atomicAddFetchNewMutableByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicAddFetchNewMutableByteArray# ::
    AtomicCount (PrimBase a) => MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  atomicAddFetchNewMutableByteArray# mba# i# x s =
    case atomicAddFetchNewMutableByteArray# mba# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicAddFetchNewMutableByteArray# #-}

  atomicSubFetchOldMutableByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicSubFetchOldMutableByteArray# ::
    AtomicCount (PrimBase a) => MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  atomicSubFetchOldMutableByteArray# mba# i# x s =
    case atomicSubFetchOldMutableByteArray# mba# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicSubFetchOldMutableByteArray# #-}

  atomicSubFetchNewMutableByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicSubFetchNewMutableByteArray# ::
    AtomicCount (PrimBase a) => MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  atomicSubFetchNewMutableByteArray# mba# i# x s =
    case atomicSubFetchNewMutableByteArray# mba# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicSubFetchNewMutableByteArray# #-}


  atomicAddFetchOldOffAddr# :: Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicAddFetchOldOffAddr# ::
    AtomicCount (PrimBase a) => Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  atomicAddFetchOldOffAddr# addr# i# x s =
    case atomicAddFetchOldOffAddr# addr# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicAddFetchOldOffAddr# #-}

  atomicAddFetchNewOffAddr# :: Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicAddFetchNewOffAddr# ::
    AtomicCount (PrimBase a) => Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  atomicAddFetchNewOffAddr# addr# i# x s =
    case atomicAddFetchNewOffAddr# addr# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicAddFetchNewOffAddr# #-}

  atomicSubFetchOldOffAddr# :: Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicSubFetchOldOffAddr# ::
    AtomicCount (PrimBase a) => Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  atomicSubFetchOldOffAddr# addr# i# x s =
    case atomicSubFetchOldOffAddr# addr# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicSubFetchOldOffAddr# #-}

  atomicSubFetchNewOffAddr# :: Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicSubFetchNewOffAddr# ::
    AtomicCount (PrimBase a) => Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  atomicSubFetchNewOffAddr# addr# i# x s =
    case atomicSubFetchNewOffAddr# addr# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicSubFetchNewOffAddr# #-}



class (Bits a, Atomic a) => AtomicBits a where
  atomicAndFetchOldMutableByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicAndFetchOldMutableByteArray# ::
    AtomicBits (PrimBase a) => MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  atomicAndFetchOldMutableByteArray# mba# i# x s =
    case atomicAndFetchOldMutableByteArray# mba# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicAndFetchOldMutableByteArray# #-}

  atomicAndFetchNewMutableByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicAndFetchNewMutableByteArray# ::
    AtomicBits (PrimBase a) => MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  atomicAndFetchNewMutableByteArray# mba# i# x s =
    case atomicAndFetchNewMutableByteArray# mba# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicAndFetchNewMutableByteArray# #-}

  atomicNandFetchOldMutableByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicNandFetchOldMutableByteArray# ::
    AtomicBits (PrimBase a) => MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  atomicNandFetchOldMutableByteArray# mba# i# x s =
    case atomicNandFetchOldMutableByteArray# mba# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicNandFetchOldMutableByteArray# #-}

  atomicNandFetchNewMutableByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicNandFetchNewMutableByteArray# ::
    AtomicBits (PrimBase a) => MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  atomicNandFetchNewMutableByteArray# mba# i# x s =
    case atomicNandFetchNewMutableByteArray# mba# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicNandFetchNewMutableByteArray# #-}

  atomicOrFetchOldMutableByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicOrFetchOldMutableByteArray# ::
    AtomicBits (PrimBase a) => MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  atomicOrFetchOldMutableByteArray# mba# i# x s =
    case atomicOrFetchOldMutableByteArray# mba# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicOrFetchOldMutableByteArray# #-}

  atomicOrFetchNewMutableByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicOrFetchNewMutableByteArray# ::
    AtomicBits (PrimBase a) => MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  atomicOrFetchNewMutableByteArray# mba# i# x s =
    case atomicOrFetchNewMutableByteArray# mba# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicOrFetchNewMutableByteArray# #-}

  atomicXorFetchOldMutableByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicXorFetchOldMutableByteArray# ::
    AtomicBits (PrimBase a) => MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  atomicXorFetchOldMutableByteArray# mba# i# x s =
    case atomicXorFetchOldMutableByteArray# mba# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicXorFetchOldMutableByteArray# #-}

  atomicXorFetchNewMutableByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicXorFetchNewMutableByteArray# ::
    AtomicBits (PrimBase a) => MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  atomicXorFetchNewMutableByteArray# mba# i# x s =
    case atomicXorFetchNewMutableByteArray# mba# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicXorFetchNewMutableByteArray# #-}


  atomicAndFetchOldOffAddr# :: Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicAndFetchOldOffAddr# ::
    AtomicBits (PrimBase a) => Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  atomicAndFetchOldOffAddr# addr# i# x s =
    case atomicAndFetchOldOffAddr# addr# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicAndFetchOldOffAddr# #-}

  atomicAndFetchNewOffAddr# :: Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicAndFetchNewOffAddr# ::
    AtomicBits (PrimBase a) => Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  atomicAndFetchNewOffAddr# addr# i# x s =
    case atomicAndFetchNewOffAddr# addr# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicAndFetchNewOffAddr# #-}

  atomicNandFetchOldOffAddr# :: Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicNandFetchOldOffAddr# ::
    AtomicBits (PrimBase a) => Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  atomicNandFetchOldOffAddr# addr# i# x s =
    case atomicNandFetchOldOffAddr# addr# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicNandFetchOldOffAddr# #-}

  atomicNandFetchNewOffAddr# :: Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicNandFetchNewOffAddr# ::
    AtomicBits (PrimBase a) => Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  atomicNandFetchNewOffAddr# addr# i# x s =
    case atomicNandFetchNewOffAddr# addr# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicNandFetchNewOffAddr# #-}

  atomicOrFetchOldOffAddr# :: Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicOrFetchOldOffAddr# ::
    AtomicBits (PrimBase a) => Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  atomicOrFetchOldOffAddr# addr# i# x s =
    case atomicOrFetchOldOffAddr# addr# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicOrFetchOldOffAddr# #-}

  atomicOrFetchNewOffAddr# :: Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicOrFetchNewOffAddr# ::
    AtomicBits (PrimBase a) => Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  atomicOrFetchNewOffAddr# addr# i# x s =
    case atomicOrFetchNewOffAddr# addr# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicOrFetchNewOffAddr# #-}

  atomicXorFetchOldOffAddr# :: Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicXorFetchOldOffAddr# ::
    AtomicBits (PrimBase a) => Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  atomicXorFetchOldOffAddr# addr# i# x s =
    case atomicXorFetchOldOffAddr# addr# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicXorFetchOldOffAddr# #-}

  atomicXorFetchNewOffAddr# :: Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  default atomicXorFetchNewOffAddr# ::
    AtomicBits (PrimBase a) => Addr# -> Int# -> a -> State# s -> (# State# s, a #)
  atomicXorFetchNewOffAddr# addr# i# x s =
    case atomicXorFetchNewOffAddr# addr# i# (toPrimBase x) s of
      (# s', y #) -> (# s', fromPrimBase y #)
  {-# INLINE atomicXorFetchNewOffAddr# #-}



-- | Flip all bits atomically in the element of an array at the supplied
-- offset. Returns the previous value. Implies full memory barrier.
atomicNotFetchOldMutableByteArray# ::
  forall a s . AtomicBits a => MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
atomicNotFetchOldMutableByteArray# mba# i# =
  atomicXorFetchOldMutableByteArray# mba# i# (complement (zeroBits :: a))
{-# INLINE atomicNotFetchOldMutableByteArray# #-}

-- | Flip all bits atomically in the element of an array at the supplied
-- offset. Returns the new value. Implies full memory barrier.
atomicNotFetchNewMutableByteArray# ::
  forall a s . AtomicBits a => MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
atomicNotFetchNewMutableByteArray# mba# i# =
  atomicXorFetchNewMutableByteArray# mba# i# (complement (zeroBits :: a))
{-# INLINE atomicNotFetchNewMutableByteArray# #-}


-- | Flip all bits atomically in the element of an array at the supplied
-- offset. Returns the previous value. Implies full memory barrier.
atomicNotFetchOldOffAddr# ::
  forall a s . AtomicBits a => Addr# -> Int# -> State# s -> (# State# s, a #)
atomicNotFetchOldOffAddr# addr# i# =
  atomicXorFetchOldOffAddr# addr# i# (complement (zeroBits :: a))
{-# INLINE atomicNotFetchOldOffAddr# #-}

-- | Flip all bits atomically in the element of an array at the supplied
-- offset. Returns the new value. Implies full memory barrier.
atomicNotFetchNewOffAddr# ::
  forall a s . AtomicBits a => Addr# -> Int# -> State# s -> (# State# s, a #)
atomicNotFetchNewOffAddr# addr# i# =
  atomicXorFetchNewOffAddr# addr# i# (complement (zeroBits :: a))
{-# INLINE atomicNotFetchNewOffAddr# #-}



instance Atomic Int8 where
  casMutableByteArray# mba# i# old new = unsafePrimBase (syncCasInt8ArrayIO mba# i# old new)
  {-# INLINE casMutableByteArray# #-}
  casOffAddr# addr# i# old new = unsafePrimBase (syncCasInt8AddrIO addr# i# old new)
  {-# INLINE casOffAddr# #-}
  casBoolMutableByteArray# mba# i# old new = ioCBoolToBoolBase (syncCasInt8BoolArrayIO mba# i# old new)
  {-# INLINE casBoolMutableByteArray# #-}
  casBoolOffAddr# addr# i# old new = ioCBoolToBoolBase (syncCasInt8BoolAddrIO addr# i# old new)
  {-# INLINE casBoolOffAddr# #-}

instance AtomicCount Int8 where
  atomicAddFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncAddFetchOldInt8ArrayIO mba# i# a)
  {-# INLINE atomicAddFetchOldMutableByteArray# #-}
  atomicAddFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncAddFetchNewInt8ArrayIO mba# i# a)
  {-# INLINE atomicAddFetchNewMutableByteArray# #-}
  atomicSubFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncSubFetchOldInt8ArrayIO mba# i# a)
  {-# INLINE atomicSubFetchOldMutableByteArray# #-}
  atomicSubFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncSubFetchNewInt8ArrayIO mba# i# a)
  {-# INLINE atomicSubFetchNewMutableByteArray# #-}
  atomicAddFetchOldOffAddr# addr# i# a = unsafePrimBase (syncAddFetchOldInt8AddrIO addr# i# a)
  {-# INLINE atomicAddFetchOldOffAddr# #-}
  atomicAddFetchNewOffAddr# addr# i# a = unsafePrimBase (syncAddFetchNewInt8AddrIO addr# i# a)
  {-# INLINE atomicAddFetchNewOffAddr# #-}
  atomicSubFetchOldOffAddr# addr# i# a = unsafePrimBase (syncSubFetchOldInt8AddrIO addr# i# a)
  {-# INLINE atomicSubFetchOldOffAddr# #-}
  atomicSubFetchNewOffAddr# addr# i# a = unsafePrimBase (syncSubFetchNewInt8AddrIO addr# i# a)
  {-# INLINE atomicSubFetchNewOffAddr# #-}

instance AtomicBits Int8 where
  atomicAndFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncAndFetchOldInt8ArrayIO mba# i# a)
  {-# INLINE atomicAndFetchOldMutableByteArray# #-}
  atomicAndFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncAndFetchNewInt8ArrayIO mba# i# a)
  {-# INLINE atomicAndFetchNewMutableByteArray# #-}
  atomicNandFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncNandFetchOldInt8ArrayIO mba# i# a)
  {-# INLINE atomicNandFetchOldMutableByteArray# #-}
  atomicNandFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncNandFetchNewInt8ArrayIO mba# i# a)
  {-# INLINE atomicNandFetchNewMutableByteArray# #-}
  atomicOrFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncOrFetchOldInt8ArrayIO mba# i# a)
  {-# INLINE atomicOrFetchOldMutableByteArray# #-}
  atomicOrFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncOrFetchNewInt8ArrayIO mba# i# a)
  {-# INLINE atomicOrFetchNewMutableByteArray# #-}
  atomicXorFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncXorFetchOldInt8ArrayIO mba# i# a)
  {-# INLINE atomicXorFetchOldMutableByteArray# #-}
  atomicXorFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncXorFetchNewInt8ArrayIO mba# i# a)
  {-# INLINE atomicXorFetchNewMutableByteArray# #-}
  atomicAndFetchOldOffAddr# addr# i# a = unsafePrimBase (syncAndFetchOldInt8AddrIO addr# i# a)
  {-# INLINE atomicAndFetchOldOffAddr# #-}
  atomicAndFetchNewOffAddr# addr# i# a = unsafePrimBase (syncAndFetchNewInt8AddrIO addr# i# a)
  {-# INLINE atomicAndFetchNewOffAddr# #-}
  atomicNandFetchOldOffAddr# addr# i# a = unsafePrimBase (syncNandFetchOldInt8AddrIO addr# i# a)
  {-# INLINE atomicNandFetchOldOffAddr# #-}
  atomicNandFetchNewOffAddr# addr# i# a = unsafePrimBase (syncNandFetchNewInt8AddrIO addr# i# a)
  {-# INLINE atomicNandFetchNewOffAddr# #-}
  atomicOrFetchOldOffAddr# addr# i# a = unsafePrimBase (syncOrFetchOldInt8AddrIO addr# i# a)
  {-# INLINE atomicOrFetchOldOffAddr# #-}
  atomicOrFetchNewOffAddr# addr# i# a = unsafePrimBase (syncOrFetchNewInt8AddrIO addr# i# a)
  {-# INLINE atomicOrFetchNewOffAddr# #-}
  atomicXorFetchOldOffAddr# addr# i# a = unsafePrimBase (syncXorFetchOldInt8AddrIO addr# i# a)
  {-# INLINE atomicXorFetchOldOffAddr# #-}
  atomicXorFetchNewOffAddr# addr# i# a = unsafePrimBase (syncXorFetchNewInt8AddrIO addr# i# a)
  {-# INLINE atomicXorFetchNewOffAddr# #-}

instance Atomic Int16 where
  casMutableByteArray# mba# i# old new = unsafePrimBase (syncCasInt16ArrayIO mba# i# old new)
  {-# INLINE casMutableByteArray# #-}
  casOffAddr# addr# i# old new = unsafePrimBase (syncCasInt16AddrIO addr# i# old new)
  {-# INLINE casOffAddr# #-}
  casBoolMutableByteArray# mba# i# old new =
    ioCBoolToBoolBase (syncCasInt16BoolArrayIO mba# i# old new)
  {-# INLINE casBoolMutableByteArray# #-}
  casBoolOffAddr# addr# i# old new = ioCBoolToBoolBase (syncCasInt16BoolAddrIO addr# i# old new)
  {-# INLINE casBoolOffAddr# #-}

instance AtomicCount Int16 where
  atomicAddFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncAddFetchOldInt16ArrayIO mba# i# a)
  {-# INLINE atomicAddFetchOldMutableByteArray# #-}
  atomicAddFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncAddFetchNewInt16ArrayIO mba# i# a)
  {-# INLINE atomicAddFetchNewMutableByteArray# #-}
  atomicSubFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncSubFetchOldInt16ArrayIO mba# i# a)
  {-# INLINE atomicSubFetchOldMutableByteArray# #-}
  atomicSubFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncSubFetchNewInt16ArrayIO mba# i# a)
  {-# INLINE atomicSubFetchNewMutableByteArray# #-}
  atomicAddFetchOldOffAddr# addr# i# a = unsafePrimBase (syncAddFetchOldInt16AddrIO addr# i# a)
  {-# INLINE atomicAddFetchOldOffAddr# #-}
  atomicAddFetchNewOffAddr# addr# i# a = unsafePrimBase (syncAddFetchNewInt16AddrIO addr# i# a)
  {-# INLINE atomicAddFetchNewOffAddr# #-}
  atomicSubFetchOldOffAddr# addr# i# a = unsafePrimBase (syncSubFetchOldInt16AddrIO addr# i# a)
  {-# INLINE atomicSubFetchOldOffAddr# #-}
  atomicSubFetchNewOffAddr# addr# i# a = unsafePrimBase (syncSubFetchNewInt16AddrIO addr# i# a)
  {-# INLINE atomicSubFetchNewOffAddr# #-}

instance AtomicBits Int16 where
  atomicAndFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncAndFetchOldInt16ArrayIO mba# i# a)
  {-# INLINE atomicAndFetchOldMutableByteArray# #-}
  atomicAndFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncAndFetchNewInt16ArrayIO mba# i# a)
  {-# INLINE atomicAndFetchNewMutableByteArray# #-}
  atomicNandFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncNandFetchOldInt16ArrayIO mba# i# a)
  {-# INLINE atomicNandFetchOldMutableByteArray# #-}
  atomicNandFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncNandFetchNewInt16ArrayIO mba# i# a)
  {-# INLINE atomicNandFetchNewMutableByteArray# #-}
  atomicOrFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncOrFetchOldInt16ArrayIO mba# i# a)
  {-# INLINE atomicOrFetchOldMutableByteArray# #-}
  atomicOrFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncOrFetchNewInt16ArrayIO mba# i# a)
  {-# INLINE atomicOrFetchNewMutableByteArray# #-}
  atomicXorFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncXorFetchOldInt16ArrayIO mba# i# a)
  {-# INLINE atomicXorFetchOldMutableByteArray# #-}
  atomicXorFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncXorFetchNewInt16ArrayIO mba# i# a)
  {-# INLINE atomicXorFetchNewMutableByteArray# #-}
  atomicAndFetchOldOffAddr# addr# i# a = unsafePrimBase (syncAndFetchOldInt16AddrIO addr# i# a)
  {-# INLINE atomicAndFetchOldOffAddr# #-}
  atomicAndFetchNewOffAddr# addr# i# a = unsafePrimBase (syncAndFetchNewInt16AddrIO addr# i# a)
  {-# INLINE atomicAndFetchNewOffAddr# #-}
  atomicNandFetchOldOffAddr# addr# i# a = unsafePrimBase (syncNandFetchOldInt16AddrIO addr# i# a)
  {-# INLINE atomicNandFetchOldOffAddr# #-}
  atomicNandFetchNewOffAddr# addr# i# a = unsafePrimBase (syncNandFetchNewInt16AddrIO addr# i# a)
  {-# INLINE atomicNandFetchNewOffAddr# #-}
  atomicOrFetchOldOffAddr# addr# i# a = unsafePrimBase (syncOrFetchOldInt16AddrIO addr# i# a)
  {-# INLINE atomicOrFetchOldOffAddr# #-}
  atomicOrFetchNewOffAddr# addr# i# a = unsafePrimBase (syncOrFetchNewInt16AddrIO addr# i# a)
  {-# INLINE atomicOrFetchNewOffAddr# #-}
  atomicXorFetchOldOffAddr# addr# i# a = unsafePrimBase (syncXorFetchOldInt16AddrIO addr# i# a)
  {-# INLINE atomicXorFetchOldOffAddr# #-}
  atomicXorFetchNewOffAddr# addr# i# a = unsafePrimBase (syncXorFetchNewInt16AddrIO addr# i# a)
  {-# INLINE atomicXorFetchNewOffAddr# #-}

instance Atomic Int32 where
  casMutableByteArray# mba# i# old new = unsafePrimBase (syncCasInt32ArrayIO mba# i# old new)
  {-# INLINE casMutableByteArray# #-}
  casOffAddr# addr# i# old new = unsafePrimBase (syncCasInt32AddrIO addr# i# old new)
  {-# INLINE casOffAddr# #-}
  casBoolMutableByteArray# mba# i# old new =
    ioCBoolToBoolBase (syncCasInt32BoolArrayIO mba# i# old new)
  {-# INLINE casBoolMutableByteArray# #-}
  casBoolOffAddr# addr# i# old new =
    ioCBoolToBoolBase (syncCasInt32BoolAddrIO addr# i# old new)
  {-# INLINE casBoolOffAddr# #-}

instance AtomicCount Int32 where
  atomicAddFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncAddFetchOldInt32ArrayIO mba# i# a)
  {-# INLINE atomicAddFetchOldMutableByteArray# #-}
  atomicAddFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncAddFetchNewInt32ArrayIO mba# i# a)
  {-# INLINE atomicAddFetchNewMutableByteArray# #-}
  atomicSubFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncSubFetchOldInt32ArrayIO mba# i# a)
  {-# INLINE atomicSubFetchOldMutableByteArray# #-}
  atomicSubFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncSubFetchNewInt32ArrayIO mba# i# a)
  {-# INLINE atomicSubFetchNewMutableByteArray# #-}
  atomicAddFetchOldOffAddr# addr# i# a = unsafePrimBase (syncAddFetchOldInt32AddrIO addr# i# a)
  {-# INLINE atomicAddFetchOldOffAddr# #-}
  atomicAddFetchNewOffAddr# addr# i# a = unsafePrimBase (syncAddFetchNewInt32AddrIO addr# i# a)
  {-# INLINE atomicAddFetchNewOffAddr# #-}
  atomicSubFetchOldOffAddr# addr# i# a = unsafePrimBase (syncSubFetchOldInt32AddrIO addr# i# a)
  {-# INLINE atomicSubFetchOldOffAddr# #-}
  atomicSubFetchNewOffAddr# addr# i# a = unsafePrimBase (syncSubFetchNewInt32AddrIO addr# i# a)
  {-# INLINE atomicSubFetchNewOffAddr# #-}

instance AtomicBits Int32 where
  atomicAndFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncAndFetchOldInt32ArrayIO mba# i# a)
  {-# INLINE atomicAndFetchOldMutableByteArray# #-}
  atomicAndFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncAndFetchNewInt32ArrayIO mba# i# a)
  {-# INLINE atomicAndFetchNewMutableByteArray# #-}
  atomicNandFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncNandFetchOldInt32ArrayIO mba# i# a)
  {-# INLINE atomicNandFetchOldMutableByteArray# #-}
  atomicNandFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncNandFetchNewInt32ArrayIO mba# i# a)
  {-# INLINE atomicNandFetchNewMutableByteArray# #-}
  atomicOrFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncOrFetchOldInt32ArrayIO mba# i# a)
  {-# INLINE atomicOrFetchOldMutableByteArray# #-}
  atomicOrFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncOrFetchNewInt32ArrayIO mba# i# a)
  {-# INLINE atomicOrFetchNewMutableByteArray# #-}
  atomicXorFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncXorFetchOldInt32ArrayIO mba# i# a)
  {-# INLINE atomicXorFetchOldMutableByteArray# #-}
  atomicXorFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncXorFetchNewInt32ArrayIO mba# i# a)
  {-# INLINE atomicXorFetchNewMutableByteArray# #-}
  atomicAndFetchOldOffAddr# addr# i# a = unsafePrimBase (syncAndFetchOldInt32AddrIO addr# i# a)
  {-# INLINE atomicAndFetchOldOffAddr# #-}
  atomicAndFetchNewOffAddr# addr# i# a = unsafePrimBase (syncAndFetchNewInt32AddrIO addr# i# a)
  {-# INLINE atomicAndFetchNewOffAddr# #-}
  atomicNandFetchOldOffAddr# addr# i# a = unsafePrimBase (syncNandFetchOldInt32AddrIO addr# i# a)
  {-# INLINE atomicNandFetchOldOffAddr# #-}
  atomicNandFetchNewOffAddr# addr# i# a = unsafePrimBase (syncNandFetchNewInt32AddrIO addr# i# a)
  {-# INLINE atomicNandFetchNewOffAddr# #-}
  atomicOrFetchOldOffAddr# addr# i# a = unsafePrimBase (syncOrFetchOldInt32AddrIO addr# i# a)
  {-# INLINE atomicOrFetchOldOffAddr# #-}
  atomicOrFetchNewOffAddr# addr# i# a = unsafePrimBase (syncOrFetchNewInt32AddrIO addr# i# a)
  {-# INLINE atomicOrFetchNewOffAddr# #-}
  atomicXorFetchOldOffAddr# addr# i# a = unsafePrimBase (syncXorFetchOldInt32AddrIO addr# i# a)
  {-# INLINE atomicXorFetchOldOffAddr# #-}
  atomicXorFetchNewOffAddr# addr# i# a = unsafePrimBase (syncXorFetchNewInt32AddrIO addr# i# a)
  {-# INLINE atomicXorFetchNewOffAddr# #-}

-- TODO: compare with and possibly swap for primops
instance Atomic Int where
  casMutableByteArray# mba# i# old new = unsafePrimBase (syncCasIntArrayIO mba# i# old new)
  {-# INLINE casMutableByteArray# #-}
  casOffAddr# addr# i# old new = unsafePrimBase (syncCasIntAddrIO addr# i# old new)
  {-# INLINE casOffAddr# #-}
  casBoolMutableByteArray# mba# i# old new = ioCBoolToBoolBase (syncCasIntBoolArrayIO mba# i# old new)
  {-# INLINE casBoolMutableByteArray# #-}
  casBoolOffAddr# addr# i# old new = ioCBoolToBoolBase (syncCasIntBoolAddrIO addr# i# old new)
  {-# INLINE casBoolOffAddr# #-}

instance AtomicCount Int where
  atomicAddFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncAddFetchOldIntArrayIO mba# i# a)
  {-# INLINE atomicAddFetchOldMutableByteArray# #-}
  atomicAddFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncAddFetchNewIntArrayIO mba# i# a)
  {-# INLINE atomicAddFetchNewMutableByteArray# #-}
  atomicSubFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncSubFetchOldIntArrayIO mba# i# a)
  {-# INLINE atomicSubFetchOldMutableByteArray# #-}
  atomicSubFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncSubFetchNewIntArrayIO mba# i# a)
  {-# INLINE atomicSubFetchNewMutableByteArray# #-}
  atomicAddFetchOldOffAddr# addr# i# a = unsafePrimBase (syncAddFetchOldIntAddrIO addr# i# a)
  {-# INLINE atomicAddFetchOldOffAddr# #-}
  atomicAddFetchNewOffAddr# addr# i# a = unsafePrimBase (syncAddFetchNewIntAddrIO addr# i# a)
  {-# INLINE atomicAddFetchNewOffAddr# #-}
  atomicSubFetchOldOffAddr# addr# i# a = unsafePrimBase (syncSubFetchOldIntAddrIO addr# i# a)
  {-# INLINE atomicSubFetchOldOffAddr# #-}
  atomicSubFetchNewOffAddr# addr# i# a = unsafePrimBase (syncSubFetchNewIntAddrIO addr# i# a)
  {-# INLINE atomicSubFetchNewOffAddr# #-}

instance AtomicBits Int where
  atomicAndFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncAndFetchOldIntArrayIO mba# i# a)
  {-# INLINE atomicAndFetchOldMutableByteArray# #-}
  atomicAndFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncAndFetchNewIntArrayIO mba# i# a)
  {-# INLINE atomicAndFetchNewMutableByteArray# #-}
  atomicNandFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncNandFetchOldIntArrayIO mba# i# a)
  {-# INLINE atomicNandFetchOldMutableByteArray# #-}
  atomicNandFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncNandFetchNewIntArrayIO mba# i# a)
  {-# INLINE atomicNandFetchNewMutableByteArray# #-}
  atomicOrFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncOrFetchOldIntArrayIO mba# i# a)
  {-# INLINE atomicOrFetchOldMutableByteArray# #-}
  atomicOrFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncOrFetchNewIntArrayIO mba# i# a)
  {-# INLINE atomicOrFetchNewMutableByteArray# #-}
  atomicXorFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncXorFetchOldIntArrayIO mba# i# a)
  {-# INLINE atomicXorFetchOldMutableByteArray# #-}
  atomicXorFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncXorFetchNewIntArrayIO mba# i# a)
  {-# INLINE atomicXorFetchNewMutableByteArray# #-}
  atomicAndFetchOldOffAddr# addr# i# a = unsafePrimBase (syncAndFetchOldIntAddrIO addr# i# a)
  {-# INLINE atomicAndFetchOldOffAddr# #-}
  atomicAndFetchNewOffAddr# addr# i# a = unsafePrimBase (syncAndFetchNewIntAddrIO addr# i# a)
  {-# INLINE atomicAndFetchNewOffAddr# #-}
  atomicNandFetchOldOffAddr# addr# i# a = unsafePrimBase (syncNandFetchOldIntAddrIO addr# i# a)
  {-# INLINE atomicNandFetchOldOffAddr# #-}
  atomicNandFetchNewOffAddr# addr# i# a = unsafePrimBase (syncNandFetchNewIntAddrIO addr# i# a)
  {-# INLINE atomicNandFetchNewOffAddr# #-}
  atomicOrFetchOldOffAddr# addr# i# a = unsafePrimBase (syncOrFetchOldIntAddrIO addr# i# a)
  {-# INLINE atomicOrFetchOldOffAddr# #-}
  atomicOrFetchNewOffAddr# addr# i# a = unsafePrimBase (syncOrFetchNewIntAddrIO addr# i# a)
  {-# INLINE atomicOrFetchNewOffAddr# #-}
  atomicXorFetchOldOffAddr# addr# i# a = unsafePrimBase (syncXorFetchOldIntAddrIO addr# i# a)
  {-# INLINE atomicXorFetchOldOffAddr# #-}
  atomicXorFetchNewOffAddr# addr# i# a = unsafePrimBase (syncXorFetchNewIntAddrIO addr# i# a)
  {-# INLINE atomicXorFetchNewOffAddr# #-}




instance Atomic Word8 where
  casMutableByteArray# mba# i# old new = unsafePrimBase (syncCasWord8ArrayIO mba# i# old new)
  {-# INLINE casMutableByteArray# #-}
  casOffAddr# addr# i# old new = unsafePrimBase (syncCasWord8AddrIO addr# i# old new)
  {-# INLINE casOffAddr# #-}
  casBoolMutableByteArray# mba# i# old new =
    ioCBoolToBoolBase (syncCasWord8BoolArrayIO mba# i# old new)
  {-# INLINE casBoolMutableByteArray# #-}
  casBoolOffAddr# addr# i# old new = ioCBoolToBoolBase (syncCasWord8BoolAddrIO addr# i# old new)
  {-# INLINE casBoolOffAddr# #-}

instance AtomicCount Word8 where
  atomicAddFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncAddFetchOldWord8ArrayIO mba# i# a)
  {-# INLINE atomicAddFetchOldMutableByteArray# #-}
  atomicAddFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncAddFetchNewWord8ArrayIO mba# i# a)
  {-# INLINE atomicAddFetchNewMutableByteArray# #-}
  atomicSubFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncSubFetchOldWord8ArrayIO mba# i# a)
  {-# INLINE atomicSubFetchOldMutableByteArray# #-}
  atomicSubFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncSubFetchNewWord8ArrayIO mba# i# a)
  {-# INLINE atomicSubFetchNewMutableByteArray# #-}
  atomicAddFetchOldOffAddr# addr# i# a = unsafePrimBase (syncAddFetchOldWord8AddrIO addr# i# a)
  {-# INLINE atomicAddFetchOldOffAddr# #-}
  atomicAddFetchNewOffAddr# addr# i# a = unsafePrimBase (syncAddFetchNewWord8AddrIO addr# i# a)
  {-# INLINE atomicAddFetchNewOffAddr# #-}
  atomicSubFetchOldOffAddr# addr# i# a = unsafePrimBase (syncSubFetchOldWord8AddrIO addr# i# a)
  {-# INLINE atomicSubFetchOldOffAddr# #-}
  atomicSubFetchNewOffAddr# addr# i# a = unsafePrimBase (syncSubFetchNewWord8AddrIO addr# i# a)
  {-# INLINE atomicSubFetchNewOffAddr# #-}

instance AtomicBits Word8 where
  atomicAndFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncAndFetchOldWord8ArrayIO mba# i# a)
  {-# INLINE atomicAndFetchOldMutableByteArray# #-}
  atomicAndFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncAndFetchNewWord8ArrayIO mba# i# a)
  {-# INLINE atomicAndFetchNewMutableByteArray# #-}
  atomicNandFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncNandFetchOldWord8ArrayIO mba# i# a)
  {-# INLINE atomicNandFetchOldMutableByteArray# #-}
  atomicNandFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncNandFetchNewWord8ArrayIO mba# i# a)
  {-# INLINE atomicNandFetchNewMutableByteArray# #-}
  atomicOrFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncOrFetchOldWord8ArrayIO mba# i# a)
  {-# INLINE atomicOrFetchOldMutableByteArray# #-}
  atomicOrFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncOrFetchNewWord8ArrayIO mba# i# a)
  {-# INLINE atomicOrFetchNewMutableByteArray# #-}
  atomicXorFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncXorFetchOldWord8ArrayIO mba# i# a)
  {-# INLINE atomicXorFetchOldMutableByteArray# #-}
  atomicXorFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncXorFetchNewWord8ArrayIO mba# i# a)
  {-# INLINE atomicXorFetchNewMutableByteArray# #-}
  atomicAndFetchOldOffAddr# addr# i# a = unsafePrimBase (syncAndFetchOldWord8AddrIO addr# i# a)
  {-# INLINE atomicAndFetchOldOffAddr# #-}
  atomicAndFetchNewOffAddr# addr# i# a = unsafePrimBase (syncAndFetchNewWord8AddrIO addr# i# a)
  {-# INLINE atomicAndFetchNewOffAddr# #-}
  atomicNandFetchOldOffAddr# addr# i# a = unsafePrimBase (syncNandFetchOldWord8AddrIO addr# i# a)
  {-# INLINE atomicNandFetchOldOffAddr# #-}
  atomicNandFetchNewOffAddr# addr# i# a = unsafePrimBase (syncNandFetchNewWord8AddrIO addr# i# a)
  {-# INLINE atomicNandFetchNewOffAddr# #-}
  atomicOrFetchOldOffAddr# addr# i# a = unsafePrimBase (syncOrFetchOldWord8AddrIO addr# i# a)
  {-# INLINE atomicOrFetchOldOffAddr# #-}
  atomicOrFetchNewOffAddr# addr# i# a = unsafePrimBase (syncOrFetchNewWord8AddrIO addr# i# a)
  {-# INLINE atomicOrFetchNewOffAddr# #-}
  atomicXorFetchOldOffAddr# addr# i# a = unsafePrimBase (syncXorFetchOldWord8AddrIO addr# i# a)
  {-# INLINE atomicXorFetchOldOffAddr# #-}
  atomicXorFetchNewOffAddr# addr# i# a = unsafePrimBase (syncXorFetchNewWord8AddrIO addr# i# a)
  {-# INLINE atomicXorFetchNewOffAddr# #-}

instance Atomic Word16 where
  casMutableByteArray# mba# i# old new = unsafePrimBase (syncCasWord16ArrayIO mba# i# old new)
  {-# INLINE casMutableByteArray# #-}
  casOffAddr# addr# i# old new = unsafePrimBase (syncCasWord16AddrIO addr# i# old new)
  {-# INLINE casOffAddr# #-}
  casBoolMutableByteArray# mba# i# old new =
    ioCBoolToBoolBase (syncCasWord16BoolArrayIO mba# i# old new)
  {-# INLINE casBoolMutableByteArray# #-}
  casBoolOffAddr# addr# i# old new = ioCBoolToBoolBase (syncCasWord16BoolAddrIO addr# i# old new)
  {-# INLINE casBoolOffAddr# #-}

instance AtomicCount Word16 where
  atomicAddFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncAddFetchOldWord16ArrayIO mba# i# a)
  {-# INLINE atomicAddFetchOldMutableByteArray# #-}
  atomicAddFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncAddFetchNewWord16ArrayIO mba# i# a)
  {-# INLINE atomicAddFetchNewMutableByteArray# #-}
  atomicSubFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncSubFetchOldWord16ArrayIO mba# i# a)
  {-# INLINE atomicSubFetchOldMutableByteArray# #-}
  atomicSubFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncSubFetchNewWord16ArrayIO mba# i# a)
  {-# INLINE atomicSubFetchNewMutableByteArray# #-}
  atomicAddFetchOldOffAddr# addr# i# a = unsafePrimBase (syncAddFetchOldWord16AddrIO addr# i# a)
  {-# INLINE atomicAddFetchOldOffAddr# #-}
  atomicAddFetchNewOffAddr# addr# i# a = unsafePrimBase (syncAddFetchNewWord16AddrIO addr# i# a)
  {-# INLINE atomicAddFetchNewOffAddr# #-}
  atomicSubFetchOldOffAddr# addr# i# a = unsafePrimBase (syncSubFetchOldWord16AddrIO addr# i# a)
  {-# INLINE atomicSubFetchOldOffAddr# #-}
  atomicSubFetchNewOffAddr# addr# i# a = unsafePrimBase (syncSubFetchNewWord16AddrIO addr# i# a)
  {-# INLINE atomicSubFetchNewOffAddr# #-}

instance AtomicBits Word16 where
  atomicAndFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncAndFetchOldWord16ArrayIO mba# i# a)
  {-# INLINE atomicAndFetchOldMutableByteArray# #-}
  atomicAndFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncAndFetchNewWord16ArrayIO mba# i# a)
  {-# INLINE atomicAndFetchNewMutableByteArray# #-}
  atomicNandFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncNandFetchOldWord16ArrayIO mba# i# a)
  {-# INLINE atomicNandFetchOldMutableByteArray# #-}
  atomicNandFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncNandFetchNewWord16ArrayIO mba# i# a)
  {-# INLINE atomicNandFetchNewMutableByteArray# #-}
  atomicOrFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncOrFetchOldWord16ArrayIO mba# i# a)
  {-# INLINE atomicOrFetchOldMutableByteArray# #-}
  atomicOrFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncOrFetchNewWord16ArrayIO mba# i# a)
  {-# INLINE atomicOrFetchNewMutableByteArray# #-}
  atomicXorFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncXorFetchOldWord16ArrayIO mba# i# a)
  {-# INLINE atomicXorFetchOldMutableByteArray# #-}
  atomicXorFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncXorFetchNewWord16ArrayIO mba# i# a)
  {-# INLINE atomicXorFetchNewMutableByteArray# #-}
  atomicAndFetchOldOffAddr# addr# i# a = unsafePrimBase (syncAndFetchOldWord16AddrIO addr# i# a)
  {-# INLINE atomicAndFetchOldOffAddr# #-}
  atomicAndFetchNewOffAddr# addr# i# a = unsafePrimBase (syncAndFetchNewWord16AddrIO addr# i# a)
  {-# INLINE atomicAndFetchNewOffAddr# #-}
  atomicNandFetchOldOffAddr# addr# i# a = unsafePrimBase (syncNandFetchOldWord16AddrIO addr# i# a)
  {-# INLINE atomicNandFetchOldOffAddr# #-}
  atomicNandFetchNewOffAddr# addr# i# a = unsafePrimBase (syncNandFetchNewWord16AddrIO addr# i# a)
  {-# INLINE atomicNandFetchNewOffAddr# #-}
  atomicOrFetchOldOffAddr# addr# i# a = unsafePrimBase (syncOrFetchOldWord16AddrIO addr# i# a)
  {-# INLINE atomicOrFetchOldOffAddr# #-}
  atomicOrFetchNewOffAddr# addr# i# a = unsafePrimBase (syncOrFetchNewWord16AddrIO addr# i# a)
  {-# INLINE atomicOrFetchNewOffAddr# #-}
  atomicXorFetchOldOffAddr# addr# i# a = unsafePrimBase (syncXorFetchOldWord16AddrIO addr# i# a)
  {-# INLINE atomicXorFetchOldOffAddr# #-}
  atomicXorFetchNewOffAddr# addr# i# a = unsafePrimBase (syncXorFetchNewWord16AddrIO addr# i# a)
  {-# INLINE atomicXorFetchNewOffAddr# #-}


instance Atomic Word32 where
  casMutableByteArray# mba# i# old new = unsafePrimBase (syncCasWord32ArrayIO mba# i# old new)
  {-# INLINE casMutableByteArray# #-}
  casOffAddr# addr# i# old new = unsafePrimBase (syncCasWord32AddrIO addr# i# old new)
  {-# INLINE casOffAddr# #-}
  casBoolMutableByteArray# mba# i# old new =
    ioCBoolToBoolBase (syncCasWord32BoolArrayIO mba# i# old new)
  {-# INLINE casBoolMutableByteArray# #-}
  casBoolOffAddr# addr# i# old new = ioCBoolToBoolBase (syncCasWord32BoolAddrIO addr# i# old new)
  {-# INLINE casBoolOffAddr# #-}

instance AtomicCount Word32 where
  atomicAddFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncAddFetchOldWord32ArrayIO mba# i# a)
  {-# INLINE atomicAddFetchOldMutableByteArray# #-}
  atomicAddFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncAddFetchNewWord32ArrayIO mba# i# a)
  {-# INLINE atomicAddFetchNewMutableByteArray# #-}
  atomicSubFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncSubFetchOldWord32ArrayIO mba# i# a)
  {-# INLINE atomicSubFetchOldMutableByteArray# #-}
  atomicSubFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncSubFetchNewWord32ArrayIO mba# i# a)
  {-# INLINE atomicSubFetchNewMutableByteArray# #-}
  atomicAddFetchOldOffAddr# addr# i# a = unsafePrimBase (syncAddFetchOldWord32AddrIO addr# i# a)
  {-# INLINE atomicAddFetchOldOffAddr# #-}
  atomicAddFetchNewOffAddr# addr# i# a = unsafePrimBase (syncAddFetchNewWord32AddrIO addr# i# a)
  {-# INLINE atomicAddFetchNewOffAddr# #-}
  atomicSubFetchOldOffAddr# addr# i# a = unsafePrimBase (syncSubFetchOldWord32AddrIO addr# i# a)
  {-# INLINE atomicSubFetchOldOffAddr# #-}
  atomicSubFetchNewOffAddr# addr# i# a = unsafePrimBase (syncSubFetchNewWord32AddrIO addr# i# a)
  {-# INLINE atomicSubFetchNewOffAddr# #-}

instance AtomicBits Word32 where
  atomicAndFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncAndFetchOldWord32ArrayIO mba# i# a)
  {-# INLINE atomicAndFetchOldMutableByteArray# #-}
  atomicAndFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncAndFetchNewWord32ArrayIO mba# i# a)
  {-# INLINE atomicAndFetchNewMutableByteArray# #-}
  atomicNandFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncNandFetchOldWord32ArrayIO mba# i# a)
  {-# INLINE atomicNandFetchOldMutableByteArray# #-}
  atomicNandFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncNandFetchNewWord32ArrayIO mba# i# a)
  {-# INLINE atomicNandFetchNewMutableByteArray# #-}
  atomicOrFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncOrFetchOldWord32ArrayIO mba# i# a)
  {-# INLINE atomicOrFetchOldMutableByteArray# #-}
  atomicOrFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncOrFetchNewWord32ArrayIO mba# i# a)
  {-# INLINE atomicOrFetchNewMutableByteArray# #-}
  atomicXorFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncXorFetchOldWord32ArrayIO mba# i# a)
  {-# INLINE atomicXorFetchOldMutableByteArray# #-}
  atomicXorFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncXorFetchNewWord32ArrayIO mba# i# a)
  {-# INLINE atomicXorFetchNewMutableByteArray# #-}
  atomicAndFetchOldOffAddr# addr# i# a = unsafePrimBase (syncAndFetchOldWord32AddrIO addr# i# a)
  {-# INLINE atomicAndFetchOldOffAddr# #-}
  atomicAndFetchNewOffAddr# addr# i# a = unsafePrimBase (syncAndFetchNewWord32AddrIO addr# i# a)
  {-# INLINE atomicAndFetchNewOffAddr# #-}
  atomicNandFetchOldOffAddr# addr# i# a = unsafePrimBase (syncNandFetchOldWord32AddrIO addr# i# a)
  {-# INLINE atomicNandFetchOldOffAddr# #-}
  atomicNandFetchNewOffAddr# addr# i# a = unsafePrimBase (syncNandFetchNewWord32AddrIO addr# i# a)
  {-# INLINE atomicNandFetchNewOffAddr# #-}
  atomicOrFetchOldOffAddr# addr# i# a = unsafePrimBase (syncOrFetchOldWord32AddrIO addr# i# a)
  {-# INLINE atomicOrFetchOldOffAddr# #-}
  atomicOrFetchNewOffAddr# addr# i# a = unsafePrimBase (syncOrFetchNewWord32AddrIO addr# i# a)
  {-# INLINE atomicOrFetchNewOffAddr# #-}
  atomicXorFetchOldOffAddr# addr# i# a = unsafePrimBase (syncXorFetchOldWord32AddrIO addr# i# a)
  {-# INLINE atomicXorFetchOldOffAddr# #-}
  atomicXorFetchNewOffAddr# addr# i# a = unsafePrimBase (syncXorFetchNewWord32AddrIO addr# i# a)
  {-# INLINE atomicXorFetchNewOffAddr# #-}


instance Atomic Word where
  casMutableByteArray# mba# i# old new = unsafePrimBase (syncCasWordArrayIO mba# i# old new)
  {-# INLINE casMutableByteArray# #-}
  casOffAddr# addr# i# old new = unsafePrimBase (syncCasWordAddrIO addr# i# old new)
  {-# INLINE casOffAddr# #-}
  casBoolMutableByteArray# mba# i# old new =
    ioCBoolToBoolBase (syncCasWordBoolArrayIO mba# i# old new)
  {-# INLINE casBoolMutableByteArray# #-}
  casBoolOffAddr# addr# i# old new = ioCBoolToBoolBase (syncCasWordBoolAddrIO addr# i# old new)
  {-# INLINE casBoolOffAddr# #-}

instance AtomicCount Word where
  atomicAddFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncAddFetchOldWordArrayIO mba# i# a)
  {-# INLINE atomicAddFetchOldMutableByteArray# #-}
  atomicAddFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncAddFetchNewWordArrayIO mba# i# a)
  {-# INLINE atomicAddFetchNewMutableByteArray# #-}
  atomicSubFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncSubFetchOldWordArrayIO mba# i# a)
  {-# INLINE atomicSubFetchOldMutableByteArray# #-}
  atomicSubFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncSubFetchNewWordArrayIO mba# i# a)
  {-# INLINE atomicSubFetchNewMutableByteArray# #-}
  atomicAddFetchOldOffAddr# addr# i# a = unsafePrimBase (syncAddFetchOldWordAddrIO addr# i# a)
  {-# INLINE atomicAddFetchOldOffAddr# #-}
  atomicAddFetchNewOffAddr# addr# i# a = unsafePrimBase (syncAddFetchNewWordAddrIO addr# i# a)
  {-# INLINE atomicAddFetchNewOffAddr# #-}
  atomicSubFetchOldOffAddr# addr# i# a = unsafePrimBase (syncSubFetchOldWordAddrIO addr# i# a)
  {-# INLINE atomicSubFetchOldOffAddr# #-}
  atomicSubFetchNewOffAddr# addr# i# a = unsafePrimBase (syncSubFetchNewWordAddrIO addr# i# a)
  {-# INLINE atomicSubFetchNewOffAddr# #-}

instance AtomicBits Word where
  atomicAndFetchOldMutableByteArray# mba# i# a =
    unsafePrimBase (syncAndFetchOldWordArrayIO mba# i# a)
  {-# INLINE atomicAndFetchOldMutableByteArray# #-}
  atomicAndFetchNewMutableByteArray# mba# i# a =
    unsafePrimBase (syncAndFetchNewWordArrayIO mba# i# a)
  {-# INLINE atomicAndFetchNewMutableByteArray# #-}
  atomicNandFetchOldMutableByteArray# mba# i# a =
    unsafePrimBase (syncNandFetchOldWordArrayIO mba# i# a)
  {-# INLINE atomicNandFetchOldMutableByteArray# #-}
  atomicNandFetchNewMutableByteArray# mba# i# a =
    unsafePrimBase (syncNandFetchNewWordArrayIO mba# i# a)
  {-# INLINE atomicNandFetchNewMutableByteArray# #-}
  atomicOrFetchOldMutableByteArray# mba# i# a =
    unsafePrimBase (syncOrFetchOldWordArrayIO mba# i# a)
  {-# INLINE atomicOrFetchOldMutableByteArray# #-}
  atomicOrFetchNewMutableByteArray# mba# i# a =
    unsafePrimBase (syncOrFetchNewWordArrayIO mba# i# a)
  {-# INLINE atomicOrFetchNewMutableByteArray# #-}
  atomicXorFetchOldMutableByteArray# mba# i# a =
    unsafePrimBase (syncXorFetchOldWordArrayIO mba# i# a)
  {-# INLINE atomicXorFetchOldMutableByteArray# #-}
  atomicXorFetchNewMutableByteArray# mba# i# a =
    unsafePrimBase (syncXorFetchNewWordArrayIO mba# i# a)
  {-# INLINE atomicXorFetchNewMutableByteArray# #-}
  atomicAndFetchOldOffAddr# addr# i# a = unsafePrimBase (syncAndFetchOldWordAddrIO addr# i# a)
  {-# INLINE atomicAndFetchOldOffAddr# #-}
  atomicAndFetchNewOffAddr# addr# i# a = unsafePrimBase (syncAndFetchNewWordAddrIO addr# i# a)
  {-# INLINE atomicAndFetchNewOffAddr# #-}
  atomicNandFetchOldOffAddr# addr# i# a = unsafePrimBase (syncNandFetchOldWordAddrIO addr# i# a)
  {-# INLINE atomicNandFetchOldOffAddr# #-}
  atomicNandFetchNewOffAddr# addr# i# a = unsafePrimBase (syncNandFetchNewWordAddrIO addr# i# a)
  {-# INLINE atomicNandFetchNewOffAddr# #-}
  atomicOrFetchOldOffAddr# addr# i# a = unsafePrimBase (syncOrFetchOldWordAddrIO addr# i# a)
  {-# INLINE atomicOrFetchOldOffAddr# #-}
  atomicOrFetchNewOffAddr# addr# i# a = unsafePrimBase (syncOrFetchNewWordAddrIO addr# i# a)
  {-# INLINE atomicOrFetchNewOffAddr# #-}
  atomicXorFetchOldOffAddr# addr# i# a = unsafePrimBase (syncXorFetchOldWordAddrIO addr# i# a)
  {-# INLINE atomicXorFetchOldOffAddr# #-}
  atomicXorFetchNewOffAddr# addr# i# a = unsafePrimBase (syncXorFetchNewWordAddrIO addr# i# a)
  {-# INLINE atomicXorFetchNewOffAddr# #-}

#if WORD_SIZE_IN_BITS == 64

-- | Available only on 64bit architectures
instance Atomic Int64 where
  casMutableByteArray# mba# i# old new = unsafePrimBase (syncCasInt64ArrayIO mba# i# old new)
  {-# INLINE casMutableByteArray# #-}
  casOffAddr# addr# i# old new = unsafePrimBase (syncCasInt64AddrIO addr# i# old new)
  {-# INLINE casOffAddr# #-}
  casBoolMutableByteArray# mba# i# old new =
    ioCBoolToBoolBase (syncCasInt64BoolArrayIO mba# i# old new)
  {-# INLINE casBoolMutableByteArray# #-}
  casBoolOffAddr# addr# i# old new = ioCBoolToBoolBase (syncCasInt64BoolAddrIO addr# i# old new)
  {-# INLINE casBoolOffAddr# #-}

-- | Available only on 64bit architectures
instance AtomicCount Int64 where
  atomicAddFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncAddFetchOldInt64ArrayIO mba# i# a)
  {-# INLINE atomicAddFetchOldMutableByteArray# #-}
  atomicAddFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncAddFetchNewInt64ArrayIO mba# i# a)
  {-# INLINE atomicAddFetchNewMutableByteArray# #-}
  atomicSubFetchOldMutableByteArray# mba# i# a = unsafePrimBase (syncSubFetchOldInt64ArrayIO mba# i# a)
  {-# INLINE atomicSubFetchOldMutableByteArray# #-}
  atomicSubFetchNewMutableByteArray# mba# i# a = unsafePrimBase (syncSubFetchNewInt64ArrayIO mba# i# a)
  {-# INLINE atomicSubFetchNewMutableByteArray# #-}
  atomicAddFetchOldOffAddr# addr# i# a = unsafePrimBase (syncAddFetchOldInt64AddrIO addr# i# a)
  {-# INLINE atomicAddFetchOldOffAddr# #-}
  atomicAddFetchNewOffAddr# addr# i# a = unsafePrimBase (syncAddFetchNewInt64AddrIO addr# i# a)
  {-# INLINE atomicAddFetchNewOffAddr# #-}
  atomicSubFetchOldOffAddr# addr# i# a = unsafePrimBase (syncSubFetchOldInt64AddrIO addr# i# a)
  {-# INLINE atomicSubFetchOldOffAddr# #-}
  atomicSubFetchNewOffAddr# addr# i# a = unsafePrimBase (syncSubFetchNewInt64AddrIO addr# i# a)
  {-# INLINE atomicSubFetchNewOffAddr# #-}

-- | Available only on 64bit architectures
instance AtomicBits Int64 where
  atomicAndFetchOldMutableByteArray# mba# i# a =
    unsafePrimBase (syncAndFetchOldInt64ArrayIO mba# i# a)
  {-# INLINE atomicAndFetchOldMutableByteArray# #-}
  atomicAndFetchNewMutableByteArray# mba# i# a =
    unsafePrimBase (syncAndFetchNewInt64ArrayIO mba# i# a)
  {-# INLINE atomicAndFetchNewMutableByteArray# #-}
  atomicNandFetchOldMutableByteArray# mba# i# a =
    unsafePrimBase (syncNandFetchOldInt64ArrayIO mba# i# a)
  {-# INLINE atomicNandFetchOldMutableByteArray# #-}
  atomicNandFetchNewMutableByteArray# mba# i# a =
    unsafePrimBase (syncNandFetchNewInt64ArrayIO mba# i# a)
  {-# INLINE atomicNandFetchNewMutableByteArray# #-}
  atomicOrFetchOldMutableByteArray# mba# i# a =
    unsafePrimBase (syncOrFetchOldInt64ArrayIO mba# i# a)
  {-# INLINE atomicOrFetchOldMutableByteArray# #-}
  atomicOrFetchNewMutableByteArray# mba# i# a =
    unsafePrimBase (syncOrFetchNewInt64ArrayIO mba# i# a)
  {-# INLINE atomicOrFetchNewMutableByteArray# #-}
  atomicXorFetchOldMutableByteArray# mba# i# a =
    unsafePrimBase (syncXorFetchOldInt64ArrayIO mba# i# a)
  {-# INLINE atomicXorFetchOldMutableByteArray# #-}
  atomicXorFetchNewMutableByteArray# mba# i# a =
    unsafePrimBase (syncXorFetchNewInt64ArrayIO mba# i# a)
  {-# INLINE atomicXorFetchNewMutableByteArray# #-}
  atomicAndFetchOldOffAddr# addr# i# a = unsafePrimBase (syncAndFetchOldInt64AddrIO addr# i# a)
  {-# INLINE atomicAndFetchOldOffAddr# #-}
  atomicAndFetchNewOffAddr# addr# i# a = unsafePrimBase (syncAndFetchNewInt64AddrIO addr# i# a)
  {-# INLINE atomicAndFetchNewOffAddr# #-}
  atomicNandFetchOldOffAddr# addr# i# a = unsafePrimBase (syncNandFetchOldInt64AddrIO addr# i# a)
  {-# INLINE atomicNandFetchOldOffAddr# #-}
  atomicNandFetchNewOffAddr# addr# i# a = unsafePrimBase (syncNandFetchNewInt64AddrIO addr# i# a)
  {-# INLINE atomicNandFetchNewOffAddr# #-}
  atomicOrFetchOldOffAddr# addr# i# a = unsafePrimBase (syncOrFetchOldInt64AddrIO addr# i# a)
  {-# INLINE atomicOrFetchOldOffAddr# #-}
  atomicOrFetchNewOffAddr# addr# i# a = unsafePrimBase (syncOrFetchNewInt64AddrIO addr# i# a)
  {-# INLINE atomicOrFetchNewOffAddr# #-}
  atomicXorFetchOldOffAddr# addr# i# a = unsafePrimBase (syncXorFetchOldInt64AddrIO addr# i# a)
  {-# INLINE atomicXorFetchOldOffAddr# #-}
  atomicXorFetchNewOffAddr# addr# i# a = unsafePrimBase (syncXorFetchNewInt64AddrIO addr# i# a)
  {-# INLINE atomicXorFetchNewOffAddr# #-}

-- | Available only on 64bit architectures
instance Atomic Word64 where
  casMutableByteArray# mba# i# old new = unsafePrimBase (syncCasWord64ArrayIO mba# i# old new)
  {-# INLINE casMutableByteArray# #-}
  casOffAddr# addr# i# old new = unsafePrimBase (syncCasWord64AddrIO addr# i# old new)
  {-# INLINE casOffAddr# #-}
  casBoolMutableByteArray# mba# i# old new =
    ioCBoolToBoolBase (syncCasWord64BoolArrayIO mba# i# old new)
  {-# INLINE casBoolMutableByteArray# #-}
  casBoolOffAddr# addr# i# old new = ioCBoolToBoolBase (syncCasWord64BoolAddrIO addr# i# old new)
  {-# INLINE casBoolOffAddr# #-}

-- | Available only on 64bit architectures
instance AtomicCount Word64 where
  atomicAddFetchOldMutableByteArray# mba# i# a =
    unsafePrimBase (syncAddFetchOldWord64ArrayIO mba# i# a)
  {-# INLINE atomicAddFetchOldMutableByteArray# #-}
  atomicAddFetchNewMutableByteArray# mba# i# a =
    unsafePrimBase (syncAddFetchNewWord64ArrayIO mba# i# a)
  {-# INLINE atomicAddFetchNewMutableByteArray# #-}
  atomicSubFetchOldMutableByteArray# mba# i# a =
    unsafePrimBase (syncSubFetchOldWord64ArrayIO mba# i# a)
  {-# INLINE atomicSubFetchOldMutableByteArray# #-}
  atomicSubFetchNewMutableByteArray# mba# i# a =
    unsafePrimBase (syncSubFetchNewWord64ArrayIO mba# i# a)
  {-# INLINE atomicSubFetchNewMutableByteArray# #-}
  atomicAddFetchOldOffAddr# addr# i# a = unsafePrimBase (syncAddFetchOldWord64AddrIO addr# i# a)
  {-# INLINE atomicAddFetchOldOffAddr# #-}
  atomicAddFetchNewOffAddr# addr# i# a = unsafePrimBase (syncAddFetchNewWord64AddrIO addr# i# a)
  {-# INLINE atomicAddFetchNewOffAddr# #-}
  atomicSubFetchOldOffAddr# addr# i# a = unsafePrimBase (syncSubFetchOldWord64AddrIO addr# i# a)
  {-# INLINE atomicSubFetchOldOffAddr# #-}
  atomicSubFetchNewOffAddr# addr# i# a = unsafePrimBase (syncSubFetchNewWord64AddrIO addr# i# a)
  {-# INLINE atomicSubFetchNewOffAddr# #-}

-- | Available only on 64bit architectures
instance AtomicBits Word64 where
  atomicAndFetchOldMutableByteArray# mba# i# a =
    unsafePrimBase (syncAndFetchOldWord64ArrayIO mba# i# a)
  {-# INLINE atomicAndFetchOldMutableByteArray# #-}
  atomicAndFetchNewMutableByteArray# mba# i# a =
    unsafePrimBase (syncAndFetchNewWord64ArrayIO mba# i# a)
  {-# INLINE atomicAndFetchNewMutableByteArray# #-}
  atomicNandFetchOldMutableByteArray# mba# i# a =
    unsafePrimBase (syncNandFetchOldWord64ArrayIO mba# i# a)
  {-# INLINE atomicNandFetchOldMutableByteArray# #-}
  atomicNandFetchNewMutableByteArray# mba# i# a =
    unsafePrimBase (syncNandFetchNewWord64ArrayIO mba# i# a)
  {-# INLINE atomicNandFetchNewMutableByteArray# #-}
  atomicOrFetchOldMutableByteArray# mba# i# a =
    unsafePrimBase (syncOrFetchOldWord64ArrayIO mba# i# a)
  {-# INLINE atomicOrFetchOldMutableByteArray# #-}
  atomicOrFetchNewMutableByteArray# mba# i# a =
    unsafePrimBase (syncOrFetchNewWord64ArrayIO mba# i# a)
  {-# INLINE atomicOrFetchNewMutableByteArray# #-}
  atomicXorFetchOldMutableByteArray# mba# i# a =
    unsafePrimBase (syncXorFetchOldWord64ArrayIO mba# i# a)
  {-# INLINE atomicXorFetchOldMutableByteArray# #-}
  atomicXorFetchNewMutableByteArray# mba# i# a =
    unsafePrimBase (syncXorFetchNewWord64ArrayIO mba# i# a)
  {-# INLINE atomicXorFetchNewMutableByteArray# #-}
  atomicAndFetchOldOffAddr# addr# i# a = unsafePrimBase (syncAndFetchOldWord64AddrIO addr# i# a)
  {-# INLINE atomicAndFetchOldOffAddr# #-}
  atomicAndFetchNewOffAddr# addr# i# a = unsafePrimBase (syncAndFetchNewWord64AddrIO addr# i# a)
  {-# INLINE atomicAndFetchNewOffAddr# #-}
  atomicNandFetchOldOffAddr# addr# i# a = unsafePrimBase (syncNandFetchOldWord64AddrIO addr# i# a)
  {-# INLINE atomicNandFetchOldOffAddr# #-}
  atomicNandFetchNewOffAddr# addr# i# a = unsafePrimBase (syncNandFetchNewWord64AddrIO addr# i# a)
  {-# INLINE atomicNandFetchNewOffAddr# #-}
  atomicOrFetchOldOffAddr# addr# i# a = unsafePrimBase (syncOrFetchOldWord64AddrIO addr# i# a)
  {-# INLINE atomicOrFetchOldOffAddr# #-}
  atomicOrFetchNewOffAddr# addr# i# a = unsafePrimBase (syncOrFetchNewWord64AddrIO addr# i# a)
  {-# INLINE atomicOrFetchNewOffAddr# #-}
  atomicXorFetchOldOffAddr# addr# i# a = unsafePrimBase (syncXorFetchOldWord64AddrIO addr# i# a)
  {-# INLINE atomicXorFetchOldOffAddr# #-}
  atomicXorFetchNewOffAddr# addr# i# a = unsafePrimBase (syncXorFetchNewWord64AddrIO addr# i# a)
  {-# INLINE atomicXorFetchNewOffAddr# #-}


-- | Available only on 64bit architectures
instance Atomic CLLong
-- | Available only on 64bit architectures
instance AtomicCount CLLong
-- | Available only on 64bit architectures
instance AtomicBits CLLong

-- | Available only on 64bit architectures
instance Atomic CULLong
-- | Available only on 64bit architectures
instance AtomicCount CULLong
-- | Available only on 64bit architectures
instance AtomicBits CULLong

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

instance Atomic IODeviceType

instance Atomic SeekMode

instance Atomic BlockReason

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
