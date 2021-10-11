{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Primal.Data.Array.Unboxed.Ragged
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Data.Array.Unboxed.Ragged
  ( RArray(..)
  , RMArray(..)
  , Size(..)
  -- * Immutable
  -- , makeRArray
  -- , makeRArrayM
  , sizeOfRArray
  , indexRArray
  -- * Mutable
  -- ** Create
  -- , newRMArray
  , newRawRMArray
  -- , newRMArrayLazy
  -- , makeRMArray
  -- , createRArrayM
  -- , createRArrayM_
  , sizeOfRMArray
  -- ** Access
  , readRMArray
  , readUnboxedRMArray
  , writeRMArray
  , readMBytesRMArray
  , writeMBytesRMArray
  , thawRArray
  -- , thawCloneSliceRArray -- TODO: implement deep copy
  , freezeRMArray
  -- , freezeCloneSliceRMArray
  , copyRArray
  , moveRMArray
  -- , cloneRArray -- TODO: implement deep clone
  -- , cloneRMArray
  -- * List
  -- , fromListRArray
  -- , fromListRArrayN
  -- , toListRArray
  -- -- * Helpers
  -- , foldrRArray
  -- , traverseRArray
  ) where

import GHC.TypeLits
import Primal.Container.Internal
import Primal.Container.Mutable.Array.Internal
import Primal.Container.Mutable.Ref.Internal
import Primal.Data.Array
import Primal.Foreign
import Primal.Memory.Bytes
import Primal.Memory.PArray
import Primal.Monad


-- | Check if both of the arrays refer to the exact same one. None of the elements are
-- evaluated.
instance Eq (RMArray n a s) where
  RMArray ma1# == RMArray ma2# = isTrue# (sameMutableArrayArray# ma1# ma2#)

data RArray (n :: Nat) a = RArray ArrayArray#

data RMArray (n :: Nat) a s = RMArray (MutableArrayArray# s)

-- Viable experiment with ragged array that contains mutable arrays
-- data RMMArray (n :: Nat) (ma :: * -> *) s = RMMArray (MutableArrayArray# s)

type instance Frozen (RMArray n) = (RArray n)

type instance Elt (RArray n) e = ()
type instance Elt (RMArray n) e = ()

instance MRef (RMArray 0) (Bytes 'Inc) where
  newRawMRef = newRawRMArray 1
  {-# INLINE newRawMRef #-}
  readMRef ra = readBytesRMArray ra 0
  {-# INLINE readMRef #-}
  writeMRef ra = writeBytesRMArray ra 0
  {-# INLINE writeMRef #-}

instance MArray (RMArray 0) (Bytes 'Inc) where

  indexArray = indexBytesRArray
  {-# INLINE indexArray #-}
  sizeOfArray = sizeOfRArray
  {-# INLINE sizeOfArray #-}
  getSizeOfMArray = pure . sizeOfRMArray
  {-# INLINE getSizeOfMArray #-}
  thawArray = thawRArray
  {-# INLINE thawArray #-}
  freezeMArray = freezeRMArray
  {-# INLINE freezeMArray #-}
  newRawMArray = newRawRMArray
  {-# INLINE newRawMArray #-}
  readMArray = readBytesRMArray
  {-# INLINE readMArray #-}
  writeMArray = writeBytesRMArray
  {-# INLINE writeMArray #-}
  copyArray = copyRArray
  {-# INLINE copyArray #-}
  moveMArray = moveRMArray
  {-# INLINE moveMArray #-}


instance MRef (RMArray 0) (UArray e) where
  newRawMRef = newRawRMArray 1
  {-# INLINE newRawMRef #-}
  readMRef ra = readUnboxedFrozenRMArray ra 0
  {-# INLINE readMRef #-}
  writeMRef ra = writeUnboxedFrozenRMArray ra 0
  {-# INLINE writeMRef #-}

instance MArray (RMArray 0) (UArray e) where
  indexArray = indexUnboxedRArray
  {-# INLINE indexArray #-}
  sizeOfArray = sizeOfRArray
  {-# INLINE sizeOfArray #-}
  getSizeOfMArray = pure . sizeOfRMArray
  {-# INLINE getSizeOfMArray #-}
  thawArray = thawRArray
  {-# INLINE thawArray #-}
  freezeMArray = freezeRMArray
  {-# INLINE freezeMArray #-}
  newRawMArray = newRawRMArray
  {-# INLINE newRawMArray #-}
  readMArray = readUnboxedFrozenRMArray
  {-# INLINE readMArray #-}
  writeMArray = writeUnboxedFrozenRMArray
  {-# INLINE writeMArray #-}
  copyArray = copyRArray
  {-# INLINE copyArray #-}
  moveMArray = moveRMArray
  {-# INLINE moveMArray #-}

instance MRef (RMArray 0) (PArray 'Inc e) where
  newRawMRef = newRawRMArray 1
  {-# INLINE newRawMRef #-}
  readMRef rma = coerce <$> readBytesRMArray rma 0
  {-# INLINE readMRef #-}
  writeMRef rma ba = writeBytesRMArray rma 0 (coerce ba)
  {-# INLINE writeMRef #-}

instance MArray (RMArray 0) (PArray 'Inc e) where
  indexArray = coerce . indexBytesRArray
  {-# INLINE indexArray #-}
  sizeOfArray = sizeOfRArray
  {-# INLINE sizeOfArray #-}
  getSizeOfMArray = pure . sizeOfRMArray
  {-# INLINE getSizeOfMArray #-}
  thawArray = thawRArray
  {-# INLINE thawArray #-}
  freezeMArray = freezeRMArray
  {-# INLINE freezeMArray #-}
  newRawMArray = newRawRMArray
  {-# INLINE newRawMArray #-}
  readMArray ma = fmap coerce . readBytesRMArray ma
  {-# INLINE readMArray #-}
  writeMArray ma i ba = writeBytesRMArray ma i (coerce ba)
  {-# INLINE writeMArray #-}
  copyArray = copyRArray
  {-# INLINE copyArray #-}
  moveMArray = moveRMArray
  {-# INLINE moveMArray #-}

instance (KnownNat n, 1 <= n, KnownNat k, k ~ (n - 1)) => MRef (RMArray n) (RArray k e) where
  newRawMRef = newRawRMArray 1
  {-# INLINE newRawMRef #-}
  readMRef rma = readFrozenRMArray rma 0
  {-# INLINE readMRef #-}
  writeMRef rma = writeFrozenRMArray rma 0
  {-# INLINE writeMRef #-}

instance (KnownNat n, 1 <= n, KnownNat k, k ~ (n - 1)) => MArray (RMArray n) (RArray k e) where
  sizeOfArray = sizeOfRArray
  {-# INLINE sizeOfArray #-}
  indexArray = indexRArray
  {-# INLINE indexArray #-}
  getSizeOfMArray = pure . sizeOfRMArray
  {-# INLINE getSizeOfMArray #-}
  thawArray = thawRArray
  {-# INLINE thawArray #-}
  freezeMArray = freezeRMArray
  {-# INLINE freezeMArray #-}
  newRawMArray = newRawRMArray
  {-# INLINE newRawMArray #-}
  readMArray = readFrozenRMArray
  {-# INLINE readMArray #-}
  writeMArray = writeFrozenRMArray
  {-# INLINE writeMArray #-}
  copyArray = copyRArray
  {-# INLINE copyArray #-}
  moveMArray = moveRMArray
  {-# INLINE moveMArray #-}


sizeOfRArray :: RArray n a -> Size
sizeOfRArray (RArray a#) = Size (I# (sizeofArrayArray# a#))
{-# INLINE sizeOfRArray #-}

indexUnboxedRArray :: RArray 0 b -> Int -> UArray e
indexUnboxedRArray (RArray a#) (I# i#) = UArray (indexByteArrayArray# a# i#)
{-# INLINE indexUnboxedRArray #-}

indexBytesRArray :: RArray 0 b -> Int -> Bytes 'Inc
indexBytesRArray (RArray a#) (I# i#) = fromByteArray# (indexByteArrayArray# a# i#)
{-# INLINE indexBytesRArray #-}

indexRArray :: (KnownNat n, 1 <= n) => RArray n (RArray (n - 1) e) -> Int -> RArray (n - 1) e
indexRArray (RArray a#) (I# i#) = RArray (indexArrayArrayArray# a# i#)
{-# INLINE indexRArray #-}


-- indexRArrayI :: MArray (RMArray n) e => RArray n e -> Int -> RElt n e
-- indexRArrayI = indexArray
-- {-# INLINE indexRArrayI #-}

-- |
--
-- @since 0.1.0
newRawRMArray :: Primal s m => Size -> m (RMArray n e s)
newRawRMArray (Size (I# n#)) =
  primal $ \s ->
    case newArrayArray# n# s of
      (# s', ma# #) -> (# s', RMArray ma# #)
{-# INLINE newRawRMArray #-}

-- Better interface:
-- newRawRMArrayI :: (MArray (RMArray n) a, Primal s m) => Size -> m (RMArray n a s)
-- newRawRMArrayI = newRawRMArray
-- {-# INLINE newRawRMArrayI #-}


thawRArray :: Primal s m => RArray n e -> m (RMArray n e s)
thawRArray (RArray a#) =
  primal $ \s ->
    case unsafeThawArrayArray# a# s of
      (# s', ma# #) -> (# s', RMArray ma# #)
{-# INLINE thawRArray #-}

freezeRMArray :: Primal s m => RMArray n e s -> m (RArray n e)
freezeRMArray (RMArray ma#) =
  primal $ \s ->
    case unsafeFreezeArrayArray# ma# s of
      (# s', a# #) -> (# s', RArray a# #)
{-# INLINE freezeRMArray #-}


-- | Get the size of a mutable boxed array
--
-- >>> ma <- newRMArray 1024 "Element of each cell"
-- >>> sizeOfRMArray ma
-- Size 1024
--
-- @since 0.1.0
sizeOfRMArray :: RMArray n e s -> Size
sizeOfRMArray (RMArray ma#) = Size (I# (sizeofMutableArrayArray# ma#))
{-# INLINE sizeOfRMArray #-}


-- | Read an element from mutable boxed array at a supplied index.
--
-- [Unsafe index] Negative or larger than array size can fail with unchecked exception
--
-- ==== __Examples__
--
-- >>> ma <- makeRMArray 10 (pure . ("Element ix: " ++) . show)
-- >>> readRMArray ma 5
-- "Element ix: 5"
--
-- @since 0.1.0
readRMArray ::
     (KnownNat n, 1 <= n, Primal s m)
  => RMArray n (RMArray (n - 1) e s) s
  -> Int
  -> m (RMArray (n - 1) e s)
readRMArray (RMArray ma#) (I# i#) =
  primal $ \s ->
    case readMutableArrayArrayArray# ma# i# s of
      (# s', ma'# #) -> (# s', RMArray ma'# #)
{-# INLINE readRMArray #-}

-- readRMMArray ::
--      (KnownNat n, 1 <= n, Primal s m)
--   => RMMArray n (RMMArray (n - 1) e) s
--   -> Int
--   -> m (RMMArray (n - 1) e s)
-- readRMMArray (RMMArray ma#) (I# i#) =
--   primal $ \s ->
--     case readMutableArrayArrayArray# ma# i# s of
--       (# s', ma'# #) -> (# s', RMMArray ma'# #)
-- {-# INLINE readRMMArray #-}

-- readRMMArray' ::
--      (KnownNat n, 1 <= n, Primal s m)
--   => RMMArray n (RMArray (n - 1) e) s
--   -> Int
--   -> m (RMArray (n - 1) e s)
-- readRMMArray' (RMMArray ma#) (I# i#) =
--   primal $ \s ->
--     case readMutableArrayArrayArray# ma# i# s of
--       (# s', ma'# #) -> (# s', RMArray ma'# #)
-- {-# INLINE readRMMArray' #-}


readRMArray_ ::
     (MArray (RMArray n) ma, Primal s m)
  => RMArray n ma s
  -> Int
  -> m ma
readRMArray_ = readMArray
{-# INLINE readRMArray_ #-}

readFrozenRMArray ::
     (KnownNat n, 1 <= n, Primal s m)
  => RMArray n (RArray (n - 1) e) s
  -> Int
  -> m (RArray (n - 1) e)
readFrozenRMArray (RMArray ma#) (I# i#) =
  primal $ \s ->
    case readArrayArrayArray# ma# i# s of
      (# s', a# #) -> (# s', RArray a# #)
{-# INLINE readFrozenRMArray #-}

readUnboxedRMArray :: Primal s m => RMArray 0 e s -> Int -> m (UMArray e s)
readUnboxedRMArray (RMArray ma#) (I# i#) =
  primal $ \s ->
    case readMutableByteArrayArray# ma# i# s of
      (# s', mba# #) -> (# s', UMArray mba# #)
{-# INLINE readUnboxedRMArray #-}

readUnboxedFrozenRMArray :: Primal s m => RMArray 0 b s -> Int -> m (UArray e)
readUnboxedFrozenRMArray (RMArray ma#) (I# i#) =
  primal $ \s ->
    case readByteArrayArray# ma# i# s of
      (# s', ba# #) -> (# s', UArray ba# #)
{-# INLINE readUnboxedFrozenRMArray #-}


readMBytesRMArray :: Primal s m => RMArray 0 e s -> Int -> m (MBytes 'Inc s)
readMBytesRMArray (RMArray ma#) (I# i#) =
  primal $ \s ->
    case readMutableByteArrayArray# ma# i# s of
      (# s', mba# #) -> (# s', fromMutableByteArray# mba# #)
{-# INLINE readMBytesRMArray #-}

readBytesRMArray :: Primal s m => RMArray 0 b s -> Int -> m (Bytes 'Inc)
readBytesRMArray (RMArray ma#) (I# i#) =
  primal $ \s ->
    case readByteArrayArray# ma# i# s of
      (# s', ba# #) -> (# s', fromByteArray# ba# #)
{-# INLINE readBytesRMArray #-}

-- | Write an element into a mutable boxed array at a supplied index strictly. An
-- element will be evaluated to WHNF.
--
-- [Unsafe index] Negative or larger than array size can fail with unchecked exception
--
-- ==== __Examples__
--
-- >>> ma <- newRMArray 4 (Nothing :: Maybe Int)
-- >>> writeRMArray ma 2 (Just 2)
-- >>> freezeRMArray ma
-- Array [Nothing,Nothing,Just 2,Nothing]
--
-- Important to note that an element is evaluated prior to being written into a cell, so
-- it will not overwrite a value with if it evaluates to an exception:
--
-- >>> import Control.Exception
-- >>> writeRMArray ma 2 (impureThrow DivideByZero)
-- *** Exception: divide by zero
-- >>> freezeRMArray ma
-- Array [Nothing,Nothing,Just 2,Nothing]
--
-- But it is evaluated to Normal Form, so it is still possible to write something that
-- eventually evaluates to bottom.
--
-- >>> writeRMArray ma 3 (Just (7 `div` 0 ))
-- >>> freezeRMArray ma
-- Array [Nothing,Nothing,Just 2,Just *** Exception: divide by zero
--
-- Either `deepseq` or `writeRMArrayDeep` can be used to alleviate that.
--
-- @since 0.1.0
writeRMArray ::
     (KnownNat n, 1 <= n, Primal s m)
  => RMArray n e s
  -> Int
  -> RMArray (n - 1) e s
  -> m ()
writeRMArray (RMArray ma#) (I# i#) (RMArray e#) =
  primal_ (writeMutableArrayArrayArray# ma# i# e#)
{-# INLINE writeRMArray #-}

writeFrozenRMArray ::
     (KnownNat n, 1 <= n, Primal s m)
  => RMArray n (RArray (n - 1) e) s
  -> Int
  -> RArray (n - 1) e
  -> m ()
writeFrozenRMArray (RMArray ma#) (I# i#) (RArray e#) =
  primal_ (writeArrayArrayArray# ma# i# e#)
{-# INLINE writeFrozenRMArray #-}

writeUnboxedRMArray ::
     Primal s m => RMArray 0 e s -> Int -> UMArray e s -> m ()
writeUnboxedRMArray (RMArray ma#) (I# i#) (UMArray mba#) =
  primal_ (writeMutableByteArrayArray# ma# i# mba#)
{-# INLINE writeUnboxedRMArray #-}


writeUnboxedFrozenRMArray ::
     Primal s m => RMArray 0 b s -> Int -> UArray e -> m ()
writeUnboxedFrozenRMArray (RMArray ma#) (I# i#) (UArray ba#) =
  primal_ (writeByteArrayArray# ma# i# ba#)
{-# INLINE writeUnboxedFrozenRMArray #-}


writeMBytesRMArray ::
     Primal s m => RMArray 0 e s -> Int -> MBytes 'Inc s -> m ()
writeMBytesRMArray (RMArray ma#) (I# i#) mb =
  primal_ (writeMutableByteArrayArray# ma# i# (toMutableByteArray# mb))
{-# INLINE writeMBytesRMArray #-}


writeBytesRMArray ::
     Primal s m => RMArray 0 b s -> Int -> Bytes 'Inc -> m ()
writeBytesRMArray (RMArray ma#) (I# i#) b =
  primal_ (writeByteArrayArray# ma# i# (toByteArray# b))
{-# INLINE writeBytesRMArray #-}


-- | Copy a subsection of an immutable array into a subsection of another mutable array.
--
-- [Unsafe overlap] The two arrays must not be the same array in different states
--
-- [Unsafe offset] Each offset cannot be negative or larger than the size of a
-- corresponding array, otherwise it can result in an unchecked exception
--
-- [Unsafe new size] Number of elements to be copied cannot be larger than the size of an
-- each array minus their corersponding offsets.
--
-- @since 0.1.0
copyRArray ::
     Primal s m
  => RArray n e -- ^ Source immutable array
  -> Int -- ^ Offset into the source immutable array
  -> RMArray n e s -- ^ Destination mutable array
  -> Int -- ^ Offset into the destination mutable array
  -> Size -- ^ Number of elements to copy over
  -> m ()
copyRArray (RArray src#) (I# srcOff#) (RMArray dst#) (I# dstOff#) (Size (I# n#)) =
  primal_ (copyArrayArray# src# srcOff# dst# dstOff# n#)
{-# INLINE copyRArray #-}

-- | Copy a subsection of a mutable array into a subsection of another or the same
-- mutable array. Therefore, unlike `copyRArray`, memory overlap is allowed.
--
-- [Unsafe offset] Each offset cannot be negative or larger than the size of a
-- corresponding array, otherwise it can result in an unchecked exception
--
-- [Unsafe new size] Number of elements to be copied cannot be larger than the size of an
-- each array minus their corersponding offsets.
--
-- @since 0.1.0
moveRMArray ::
     Primal s m
  => RMArray n e s -- ^ Source mutable array
  -> Int -- ^ Offset into the source mutable array
  -> RMArray n e s -- ^ Destination mutable array
  -> Int -- ^ Offset into the destination mutable array
  -> Size -- ^ Number of elements to copy over
  -> m ()
moveRMArray (RMArray src#) (I# srcOff#) (RMArray dst#) (I# dstOff#) (Size (I# n#)) =
  primal_ (copyMutableArrayArray# src# srcOff# dst# dstOff# n#)
{-# INLINE moveRMArray #-}

