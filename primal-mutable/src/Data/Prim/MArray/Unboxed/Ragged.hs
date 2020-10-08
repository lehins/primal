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
-- Module      : Data.Prim.MArray.Unboxed.Ragged
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.MArray.Unboxed.Ragged
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
  -- , thawCopyRArray -- TODO: implement deep copy
  , freezeRMArray
  -- , freezeCopyRMArray
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

import Control.Prim.Monad
import Data.Prim.Memory.Bytes
import Data.Prim.Memory.PrimArray
import Data.Prim.MRef.Internal
import qualified Data.Prim.MArray.Unboxed as U
import qualified Data.Prim.MArray.Internal as I
import Foreign.Prim
import GHC.TypeLits


-- | Check if both of the arrays refer to the exact same one. None of the elements are
-- evaluated.
instance Eq (RMArray n a s) where
  RMArray ma1# == RMArray ma2# = isTrue# (sameMutableArrayArray# ma1# ma2#)

data RArray (n :: Nat) a = RArray ArrayArray#

data RMArray (n :: Nat) a s = RMArray (MutableArrayArray# s)

type family RElt n a where
  RElt 0 a = a
  RElt n a = RArray (n - 1) a

instance {-# OVERLAPPING #-} MRef (RMArray 0 (Bytes 'Inc)) where
  type Elt (RMArray 0 (Bytes 'Inc)) = RElt 0 (Bytes 'Inc)
  newRawMRef = newRawRMArray 1
  {-# INLINE newRawMRef #-}
  readMRef ra = readBytesRMArray ra 0
  {-# INLINE readMRef #-}
  writeMRef ra = writeBytesRMArray ra 0
  {-# INLINE writeMRef #-}

instance {-# OVERLAPPING #-} I.MArray (RMArray 0 (Bytes 'Inc)) where
  type Array (RMArray 0 (Bytes 'Inc)) = RArray 0 (Bytes 'Inc)

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


instance {-# OVERLAPPING #-} MRef (RMArray 0 (U.UArray e)) where
  type Elt (RMArray 0 (U.UArray e)) = RElt 0 (U.UArray e)
  newRawMRef = newRawRMArray 1
  {-# INLINE newRawMRef #-}
  readMRef ra = readUnboxedFrozenRMArray ra 0
  {-# INLINE readMRef #-}
  writeMRef ra = writeUnboxedFrozenRMArray ra 0
  {-# INLINE writeMRef #-}

instance {-# OVERLAPPING #-} I.MArray (RMArray 0 (U.UArray e)) where
  type Array (RMArray 0 (U.UArray e)) = RArray 0 (U.UArray e)

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

instance {-# OVERLAPPING #-} MRef (RMArray 0 (PArray 'Inc e)) where
  type Elt (RMArray 0 (PArray 'Inc e)) = RElt 0 (PArray 'Inc e)
  newRawMRef = newRawRMArray 1
  {-# INLINE newRawMRef #-}
  readMRef rma = coerce <$> readBytesRMArray rma 0
  {-# INLINE readMRef #-}
  writeMRef rma ba = writeBytesRMArray rma 0 (coerce ba)
  {-# INLINE writeMRef #-}

instance {-# OVERLAPPING #-} I.MArray (RMArray 0 (PArray 'Inc e)) where
  type Array (RMArray 0 (PArray 'Inc e)) = RArray 0 (PArray 'Inc e)

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

instance (KnownNat n, 1 <= n, RElt n e ~ RArray (n - 1) e) => MRef (RMArray n e) where
  type Elt (RMArray n e) = RElt n e
  newRawMRef = newRawRMArray 1
  {-# INLINE newRawMRef #-}
  readMRef rma = readFrozenRMArray rma 0
  {-# INLINE readMRef #-}
  writeMRef rma = writeFrozenRMArray rma 0
  {-# INLINE writeMRef #-}

instance (KnownNat n, 1 <= n, RElt n e ~ RArray (n - 1) e) => I.MArray (RMArray n e) where
  type Array (RMArray n e) = RArray n e
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

indexUnboxedRArray :: RArray 0 b -> Int -> U.UArray e
indexUnboxedRArray (RArray a#) (I# i#) = U.UArray (indexByteArrayArray# a# i#)
{-# INLINE indexUnboxedRArray #-}

indexBytesRArray :: RArray 0 b -> Int -> Bytes 'Inc
indexBytesRArray (RArray a#) (I# i#) = fromByteArray# (indexByteArrayArray# a# i#)
{-# INLINE indexBytesRArray #-}

indexRArray :: (KnownNat n, 1 <= n) => RArray n e -> Int -> RArray (n - 1) e
indexRArray (RArray a#) (I# i#) = RArray (indexArrayArrayArray# a# i#)
{-# INLINE indexRArray #-}


indexRArrayI :: I.MArray (RMArray n e) => RArray n e -> Int -> RElt n e
indexRArrayI = I.indexArray
{-# INLINE indexRArrayI #-}

-- |
--
-- @since 0.1.0
newRawRMArray :: MonadPrim s m => Size -> m (RMArray n e s)
newRawRMArray (Size (I# n#)) =
  prim $ \s ->
    case newArrayArray# n# s of
      (# s', ma# #) -> (# s', RMArray ma# #)
{-# INLINE newRawRMArray #-}

-- Better interface:
-- newRawRMArrayI :: (I.MArray (RMArray n) a, MonadPrim s m) => Size -> m (RMArray n a s)
-- newRawRMArrayI = I.newRawRMArray
-- {-# INLINE newRawRMArrayI #-}


thawRArray :: MonadPrim s m => RArray n e -> m (RMArray n e s)
thawRArray (RArray a#) =
  prim $ \s ->
    case unsafeThawArrayArray# a# s of
      (# s', ma# #) -> (# s', RMArray ma# #)
{-# INLINE thawRArray #-}

freezeRMArray :: MonadPrim s m => RMArray n e s -> m (RArray n e)
freezeRMArray (RMArray ma#) =
  prim $ \s ->
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
     (KnownNat n, 1 <= n, marr ~ RMArray (n - 1) e s, MonadPrim s m)
  => RMArray n marr s
  -> Int
  -> m marr
readRMArray (RMArray ma#) (I# i#) =
  prim $ \s ->
    case readMutableArrayArrayArray# ma# i# s of
      (# s', ma'# #) -> (# s', RMArray ma'# #)
{-# INLINE readRMArray #-}

readFrozenRMArray ::
     (KnownNat n, 1 <= n, MonadPrim s m)
  => RMArray n e s
  -> Int
  -> m (RArray (n - 1) e)
readFrozenRMArray (RMArray ma#) (I# i#) =
  prim $ \s ->
    case readArrayArrayArray# ma# i# s of
      (# s', a# #) -> (# s', RArray a# #)
{-# INLINE readFrozenRMArray #-}

readUnboxedRMArray :: MonadPrim s m => RMArray 0 e s -> Int -> m (U.UMArray e s)
readUnboxedRMArray (RMArray ma#) (I# i#) =
  prim $ \s ->
    case readMutableByteArrayArray# ma# i# s of
      (# s', mba# #) -> (# s', U.UMArray mba# #)
{-# INLINE readUnboxedRMArray #-}

readUnboxedFrozenRMArray :: MonadPrim s m => RMArray 0 b s -> Int -> m (U.UArray e)
readUnboxedFrozenRMArray (RMArray ma#) (I# i#) =
  prim $ \s ->
    case readByteArrayArray# ma# i# s of
      (# s', ba# #) -> (# s', U.UArray ba# #)
{-# INLINE readUnboxedFrozenRMArray #-}


readMBytesRMArray :: MonadPrim s m => RMArray 0 e s -> Int -> m (MBytes 'Inc s)
readMBytesRMArray (RMArray ma#) (I# i#) =
  prim $ \s ->
    case readMutableByteArrayArray# ma# i# s of
      (# s', mba# #) -> (# s', fromMutableByteArray# mba# #)
{-# INLINE readMBytesRMArray #-}

readBytesRMArray :: MonadPrim s m => RMArray 0 b s -> Int -> m (Bytes 'Inc)
readBytesRMArray (RMArray ma#) (I# i#) =
  prim $ \s ->
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
-- >>> writeRMArray ma 2 (throw DivideByZero)
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
     (KnownNat n, 1 <= n, MonadPrim s m)
  => RMArray n e s
  -> Int
  -> RMArray (n - 1) e s
  -> m ()
writeRMArray (RMArray ma#) (I# i#) (RMArray e#) =
  prim_ (writeMutableArrayArrayArray# ma# i# e#)
{-# INLINE writeRMArray #-}

writeFrozenRMArray ::
     (KnownNat n, 1 <= n, MonadPrim s m)
  => RMArray n e s
  -> Int
  -> RArray (n - 1) e
  -> m ()
writeFrozenRMArray (RMArray ma#) (I# i#) (RArray e#) =
  prim_ (writeArrayArrayArray# ma# i# e#)
{-# INLINE writeFrozenRMArray #-}

writeUnboxedRMArray ::
     MonadPrim s m => RMArray 0 e s -> Int -> U.UMArray e s -> m ()
writeUnboxedRMArray (RMArray ma#) (I# i#) (U.UMArray mba#) =
  prim_ (writeMutableByteArrayArray# ma# i# mba#)
{-# INLINE writeUnboxedRMArray #-}


writeUnboxedFrozenRMArray ::
     MonadPrim s m => RMArray 0 b s -> Int -> U.UArray e -> m ()
writeUnboxedFrozenRMArray (RMArray ma#) (I# i#) (U.UArray ba#) =
  prim_ (writeByteArrayArray# ma# i# ba#)
{-# INLINE writeUnboxedFrozenRMArray #-}


writeMBytesRMArray ::
     MonadPrim s m => RMArray 0 e s -> Int -> MBytes 'Inc s -> m ()
writeMBytesRMArray (RMArray ma#) (I# i#) mb =
  prim_ (writeMutableByteArrayArray# ma# i# (toMutableByteArray# mb))
{-# INLINE writeMBytesRMArray #-}


writeBytesRMArray ::
     MonadPrim s m => RMArray 0 b s -> Int -> Bytes 'Inc -> m ()
writeBytesRMArray (RMArray ma#) (I# i#) b =
  prim_ (writeByteArrayArray# ma# i# (toByteArray# b))
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
     MonadPrim s m
  => RArray n e -- ^ Source immutable array
  -> Int -- ^ Offset into the source immutable array
  -> RMArray n e s -- ^ Destination mutable array
  -> Int -- ^ Offset into the destination mutable array
  -> Size -- ^ Number of elements to copy over
  -> m ()
copyRArray (RArray src#) (I# srcOff#) (RMArray dst#) (I# dstOff#) (Size (I# n#)) =
  prim_ (copyArrayArray# src# srcOff# dst# dstOff# n#)
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
     MonadPrim s m
  => RMArray n e s -- ^ Source mutable array
  -> Int -- ^ Offset into the source mutable array
  -> RMArray n e s -- ^ Destination mutable array
  -> Int -- ^ Offset into the destination mutable array
  -> Size -- ^ Number of elements to copy over
  -> m ()
moveRMArray (RMArray src#) (I# srcOff#) (RMArray dst#) (I# dstOff#) (Size (I# n#)) =
  prim_ (copyMutableArrayArray# src# srcOff# dst# dstOff# n#)
{-# INLINE moveRMArray #-}

