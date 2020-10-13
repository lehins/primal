{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Data.Prim.MArray.Unboxed
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.MArray.Unboxed
  ( UArray(..)
  , UMArray(..)
  , Size(..)
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
  -- *
  , shrinkUMArray
  , resizeUMArray
  , thawUArray
  , thawCopyUArray
  , freezeUMArray
  , freezeCopyUMArray
  , copyUArray
  , moveUMArray
  , cloneUArray
  , cloneUMArray
  -- * PArray
  , toPArray
  , fromPArray
  , toPMArray
  , fromPMArray
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
import qualified Data.Prim.MArray.Internal as I
import Data.Prim.Memory.PArray
import Data.Prim.Memory.Bytes
import Data.Prim.MRef.Atomic
import Data.Prim.MRef.Internal
import Data.Prim.Array






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


-- | /O(1)/ - Cast an unboxed array into a `PArray`
--
-- @since 0.1.0
toPArray :: UArray e -> PArray 'Inc e
toPArray (UArray a#) = PArray (fromByteArray# a#)
{-# INLINE toPArray #-}

-- | /O(1)/ - Cast a `PArray` into an unboxed array
--
-- @since 0.1.0
fromPArray :: PArray p e -> UArray e
fromPArray (PArray ba) = UArray (toByteArray# ba)
{-# INLINE fromPArray #-}


-- | /O(1)/ - Cast a mutable unboxed array into a `PMArray`
--
-- @since 0.1.0
toPMArray :: UMArray e s -> PMArray 'Inc e s
toPMArray (UMArray mba#) = PMArray (fromMutableByteArray# mba#)
{-# INLINE toPMArray #-}

-- | /O(1)/ - Cast an `PMArray` into a mutable unboxed array
--
-- @since 0.1.0
fromPMArray :: PMArray p e s -> UMArray e s
fromPMArray (PMArray mb) = UMArray (toMutableByteArray# mb)
{-# INLINE fromPMArray #-}







newUMArray :: (Prim e, MonadPrim s m) => Size -> e -> m (UMArray e s)
newUMArray = I.newMArray
{-# INLINE newUMArray #-}




thawCopyUArray :: (Prim e, MonadPrim s m) => UArray e -> Int -> Size -> m (UMArray e s)
thawCopyUArray = I.thawCopyArray
{-# INLINE thawCopyUArray #-}

freezeCopyUMArray :: (Prim e, MonadPrim s m) => UMArray e s -> Int -> Size -> m (UArray e)
freezeCopyUMArray = I.freezeCopyMArray
{-# INLINE freezeCopyUMArray #-}


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
cloneUMArray :: (Prim e, MonadPrim s m) => UMArray e s -> Int -> Size -> m (UMArray e s)
cloneUMArray = I.cloneMArray
{-# INLINE cloneUMArray #-}


-- | Strict right fold
foldrUArray :: Prim e => (e -> b -> b) -> b -> UArray e -> b
foldrUArray = I.foldrArray
{-# INLINE foldrUArray #-}

makeUArray :: Prim e => Size -> (Int -> e) -> UArray e
makeUArray = I.makeArray
{-# INLINE makeUArray #-}

makeUArrayM :: (Prim e, MonadPrim s m) => Size -> (Int -> m e) -> m (UArray e)
makeUArrayM = I.makeArrayM
{-# INLINE makeUArrayM #-}

createUArrayM :: (Prim e, MonadPrim s m) => Size -> (UMArray e s -> m b) -> m (b, UArray e)
createUArrayM = I.createArrayM
{-# INLINE createUArrayM #-}

createUArrayM_ :: (Prim e, MonadPrim s m) => Size -> (UMArray e s -> m b) -> m (UArray e)
createUArrayM_ = I.createArrayM_
{-# INLINE createUArrayM_ #-}


-- | Create a new mutable array of a supplied size by applying a monadic action to indices
-- of each one of the new elements.
--
-- [Unsafe size] Negative or too large of an array size can kill the current thread with
-- `HeapOverflow` asynchronous exception.
--
-- ====__Examples__
--
-- >>> import Control.Monad ((>=>))
-- >>> import Data.Prim.Ref
-- >>> ref <- newRef "Numbers: "
-- >>> ma <- makeUMArray 5 $ \i -> modifyFetchRef ref (\cur -> cur ++ show i ++ ",")
-- >>> mapM_ (readUMArray ma >=> putStrLn) [0 .. 4]
-- Numbers: 0,
-- Numbers: 0,1,
-- Numbers: 0,1,2,
-- Numbers: 0,1,2,3,
-- Numbers: 0,1,2,3,4,
--
-- @since 0.1.0
makeUMArray :: (Prim e, MonadPrim s m) => Size -> (Int -> m e) -> m (UMArray e s)
makeUMArray = I.makeMArray
{-# INLINE makeUMArray #-}

-- | Traverse an array with a monadic action.
--
-- @since 0.1.0
traverseUArray :: (Prim e, Prim b, MonadPrim s m) => (e -> m b) -> UArray e -> m (UArray b)
traverseUArray = I.traverseArray
{-# INLINE traverseUArray #-}
