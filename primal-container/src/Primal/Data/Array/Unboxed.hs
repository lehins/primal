{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

-- |
-- Module      : Primal.Data.Array.Unboxed
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module Primal.Data.Array.Unboxed
  ( UArray (..)
  , UMArray (..)
  , Size (..)

    -- * Immutable
  , makeUArray
  , makeUArrayM
  , sizeOfUArray
  , indexUArray

    -- * Mutable

    -- ** Create
  , newUMArray
  , newRawUMArray
  , makeUMArray
  , createUArrayM
  , createUArrayM_

    -- ** Access
  , readUMArray
  , writeUMArray

    -- *** Atomic

  -- , casUMArray
  -- , atomicModifyFetchNewUMArray
  -- , atomicModifyFetchOldUMArray
  -- , atomicModifyUMArray
  -- , atomicModifyUMArray_
  -- , atomicModifyUMArray2

  , shrinkUMArray
  , resizeUMArray
  , thawUArray
  , thawCloneSliceUArray
  , freezeUMArray
  , freezeCloneSliceUMArray
  , copyUArray
  , moveUMArray
  , cloneUArray
  , cloneUMArray

    -- * PUArray
  , toPUArray
  , fromPUArray
  , toPUMArray
  , fromPUMArray

    -- * List
  , fromListUArray
  , fromListUArrayN
  , toListUArray

    -- * Helpers
  , foldrUArray
  , traverseUArray
  ) where

import Control.Prim.Monad
import Data.Bits
import Data.Prim
import Data.Prim.Array
import qualified Data.Prim.MArray.Internal as I
import Data.Prim.MRef.Atomic
import Data.Prim.MRef.Internal
import Data.Prim.Memory.Bytes
import Data.Prim.Memory.PUArray

instance Prim e => MRef (UMArray e) where
  type Elt (UMArray e) = e
  newRawMRef = newRawUMArray 1
  {-# INLINE newRawMRef #-}
  readMRef uma = readUMArray uma 0
  {-# INLINE readMRef #-}
  writeMRef uma = writeUMArray uma 0
  {-# INLINE writeMRef #-}

instance Atomic e => AtomicMRef (UMArray e) where
  atomicReadMRef mba = atomicReadMBytes (fromUMArrayMBytes mba) (0 :: Off e)
  {-# INLINE atomicReadMRef #-}
  atomicWriteMRef mba = atomicWriteMBytes (fromUMArrayMBytes mba) (0 :: Off e)
  {-# INLINE atomicWriteMRef #-}
  casMRef mba = casBoolFetchMBytes (fromUMArrayMBytes mba) (0 :: Off e)
  {-# INLINE casMRef #-}
  atomicModifyMRef mba = atomicModifyMBytes (fromUMArrayMBytes mba) (0 :: Off e)
  {-# INLINE atomicModifyMRef #-}

instance (Num e, AtomicCount e) => AtomicCountMRef (UMArray e) where
  atomicAddFetchOldMRef mba = atomicAddFetchOldMBytes (fromUMArrayMBytes mba) (0 :: Off e)
  {-# INLINE atomicAddFetchOldMRef #-}
  atomicAddFetchNewMRef mba = atomicAddFetchNewMBytes (fromUMArrayMBytes mba) (0 :: Off e)
  {-# INLINE atomicAddFetchNewMRef #-}
  atomicSubFetchOldMRef mba = atomicSubFetchOldMBytes (fromUMArrayMBytes mba) (0 :: Off e)
  {-# INLINE atomicSubFetchOldMRef #-}
  atomicSubFetchNewMRef mba = atomicSubFetchNewMBytes (fromUMArrayMBytes mba) (0 :: Off e)
  {-# INLINE atomicSubFetchNewMRef #-}

instance (Bits e, AtomicBits e) => AtomicBitsMRef (UMArray e) where
  atomicAndFetchOldMRef mba = atomicAndFetchOldMBytes (fromUMArrayMBytes mba) (0 :: Off e)
  {-# INLINE atomicAndFetchOldMRef #-}
  atomicAndFetchNewMRef mba = atomicAndFetchNewMBytes (fromUMArrayMBytes mba) (0 :: Off e)
  {-# INLINE atomicAndFetchNewMRef #-}
  atomicNandFetchOldMRef mba = atomicNandFetchOldMBytes (fromUMArrayMBytes mba) (0 :: Off e)
  {-# INLINE atomicNandFetchOldMRef #-}
  atomicNandFetchNewMRef mba = atomicNandFetchNewMBytes (fromUMArrayMBytes mba) (0 :: Off e)
  {-# INLINE atomicNandFetchNewMRef #-}
  atomicOrFetchOldMRef mba = atomicOrFetchOldMBytes (fromUMArrayMBytes mba) (0 :: Off e)
  {-# INLINE atomicOrFetchOldMRef #-}
  atomicOrFetchNewMRef mba = atomicOrFetchNewMBytes (fromUMArrayMBytes mba) (0 :: Off e)
  {-# INLINE atomicOrFetchNewMRef #-}
  atomicXorFetchOldMRef mba = atomicXorFetchOldMBytes (fromUMArrayMBytes mba) (0 :: Off e)
  {-# INLINE atomicXorFetchOldMRef #-}
  atomicXorFetchNewMRef mba = atomicXorFetchNewMBytes (fromUMArrayMBytes mba) (0 :: Off e)
  {-# INLINE atomicXorFetchNewMRef #-}
  atomicNotFetchOldMRef mba = atomicNotFetchOldMBytes (fromUMArrayMBytes mba) (0 :: Off e)
  {-# INLINE atomicNotFetchOldMRef #-}
  atomicNotFetchNewMRef mba = atomicNotFetchNewMBytes (fromUMArrayMBytes mba) (0 :: Off e)
  {-# INLINE atomicNotFetchNewMRef #-}

instance Prim e => I.MArray (UMArray e) where
  type Array (UMArray e) = UArray e
  sizeOfArray = sizeOfUArray
  {-# INLINE sizeOfArray #-}
  indexArray = indexUArray
  {-# INLINE indexArray #-}
  getSizeOfMArray = getSizeOfUMArray
  {-# INLINE getSizeOfMArray #-}
  thawArray = thawUArray
  {-# INLINE thawArray #-}
  freezeMArray = freezeUMArray
  {-# INLINE freezeMArray #-}
  newRawMArray = newRawUMArray
  {-# INLINE newRawMArray #-}
  readMArray = readUMArray
  {-# INLINE readMArray #-}
  writeMArray = writeUMArray
  {-# INLINE writeMArray #-}
  copyArray = copyUArray
  {-# INLINE copyArray #-}
  moveMArray = moveUMArray
  {-# INLINE moveMArray #-}
  setMArray = setUMArray
  {-# INLINE setMArray #-}
  shrinkMArray ma sz = ma <$ shrinkUMArray ma sz
  {-# INLINE shrinkMArray #-}
  resizeMArray = resizeUMArray
  {-# INLINE resizeMArray #-}

-- | /O(1)/ - Cast an unboxed array into a `PUArray`
--
-- @since 0.1.0
toPUArray :: UArray e -> PUArray 'Inc e
toPUArray (UArray a#) = PUArray (fromByteArray# a#)
{-# INLINE toPUArray #-}

-- | /O(1)/ - Cast a `PUArray` into an unboxed array
--
-- @since 0.1.0
fromPUArray :: PUArray p e -> UArray e
fromPUArray (PUArray ba) = UArray (toByteArray# ba)
{-# INLINE fromPUArray #-}

-- | /O(1)/ - Cast a mutable unboxed array into a `PUMArray`
--
-- @since 0.1.0
toPUMArray :: UMArray e s -> PUMArray 'Inc e s
toPUMArray (UMArray mba#) = PUMArray (fromMutableByteArray# mba#)
{-# INLINE toPUMArray #-}

-- | /O(1)/ - Cast an `PUMArray` into a mutable unboxed array
--
-- @since 0.1.0
fromPUMArray :: PUMArray p e s -> UMArray e s
fromPUMArray (PUMArray mb) = UMArray (toMutableByteArray# mb)
{-# INLINE fromPUMArray #-}

thawCloneSliceUArray :: (Prim e, Primal s m) => UArray e -> Int -> Size -> m (UMArray e s)
thawCloneSliceUArray = I.thawCloneSliceArray
{-# INLINE thawCloneSliceUArray #-}

freezeCloneSliceUMArray :: (Prim e, Primal s m) => UMArray e s -> Int -> Size -> m (UArray e)
freezeCloneSliceUMArray = I.freezeCloneSliceMArray
{-# INLINE freezeCloneSliceUMArray #-}

-- | Make an exact copy of a subsection of a pure immutable array.

---
-- [Unsafe offset] Offset cannot be negative or larger than the size of an array,
-- otherwise it can result in an unchecked exception
--
-- [Unsafe new size] Number of elements to be copied cannot be larger than the size of an
-- array minus the offset.
--
-- ====__Examples__
--
-- >>> let a = fromListUArray ['a'..'z']
-- >>> a
-- UArray "abcdefghijklmnopqrstuvwxyz"
-- >>> cloneUArray a 23 3
-- UArray "xyz"
--
-- @since 0.1.0
cloneUArray :: Prim e => UArray e -> Int -> Size -> UArray e
cloneUArray = I.cloneArray
{-# INLINE cloneUArray #-}

-- | Same as `cloneUArray`, except it works on mutable arrays
--
-- [Unsafe offset] Offset cannot be negative or larger than the size of an array,
-- otherwise it can result in an unchecked exception
--
-- [Unsafe new size] Number of elements to be copied cannot be larger than the size of an
-- array minus the offset.
--
-- @since 0.1.0
cloneUMArray :: (Prim e, Primal s m) => UMArray e s -> Int -> Size -> m (UMArray e s)
cloneUMArray = I.cloneMArray
{-# INLINE cloneUMArray #-}

-- | Strict right fold
foldrUArray :: Prim e => (e -> b -> b) -> b -> UArray e -> b
foldrUArray = I.foldrArray
{-# INLINE foldrUArray #-}

makeUArray :: Prim e => Size -> (Int -> e) -> UArray e
makeUArray = I.makeArray
{-# INLINE makeUArray #-}

makeUArrayM :: (Prim e, Primal s m) => Size -> (Int -> m e) -> m (UArray e)
makeUArrayM = I.makeArrayM
{-# INLINE makeUArrayM #-}

createUArrayM :: (Prim e, Primal s m) => Size -> (UMArray e s -> m b) -> m (b, UArray e)
createUArrayM = I.createArrayM
{-# INLINE createUArrayM #-}

createUArrayM_ :: (Prim e, Primal s m) => Size -> (UMArray e s -> m b) -> m (UArray e)
createUArrayM_ = I.createArrayM_
{-# INLINE createUArrayM_ #-}

-- | Traverse an array with a monadic action.
--
-- @since 0.1.0
traverseUArray :: (Prim e, Prim b, Primal s m) => (e -> m b) -> UArray e -> m (UArray b)
traverseUArray = I.traverseArray
{-# INLINE traverseUArray #-}
