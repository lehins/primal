{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
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
  , MRArray(..)
  , Size(..)
  -- * Immutable
  -- , makeArray
  -- , makeArrayM
  , sizeOfArray
  , indexArray
  -- * Mutable
  -- ** Create
  -- , newMArray
  , newRawMArray
  -- , newMArrayLazy
  -- , makeMArray
  -- , createArrayM
  -- , createArrayM_
  , sizeOfMArray
  -- ** Access
  , readMArray
  , writeMArray
  -- -- *** Atomic
  -- , casMArray
  -- , atomicModifyFetchNewMArray
  -- , atomicModifyFetchOldMArray
  -- , atomicModifyMArray
  -- , atomicModifyMArray_
  -- , atomicModifyMArray2
  -- -- *
  -- , thawArray
  -- , thawCopyArray
  -- , freezeMArray
  -- , freezeCopyMArray
  -- , copyArray
  -- , moveMArray
  -- , cloneArray
  -- , cloneMArray
  -- -- * List
  -- , fromListArray
  -- , fromListArrayN
  -- , toListArray
  -- -- * Helpers
  -- , foldrArray
  -- , traverseArray
  ) where

import Control.DeepSeq
import Control.Exception (ArrayException(UndefinedElement), throw)
import Control.Monad.ST
import Control.Prim.Monad
import Data.Prim.Memory.Bytes
import Data.Prim.Memory.ByteArray
import Data.Prim
import qualified Data.Prim.MArray.Unboxed as U
import qualified Data.Prim.MArray.Internal as I
import Foreign.Prim
import GHC.TypeLits


-- | Check if both of the arrays refer to the exact same one. None of the elements are
-- evaluated.
instance Eq (MRArray n a s) where
  MRArray ma1# == MRArray ma2# = isTrue# (sameMutableArrayArray# ma1# ma2#)

data RArray (n :: Nat) a = RArray ArrayArray#

data MRArray (n :: Nat) a s = MRArray (MutableArrayArray# s)

type family RElt n a where
  RElt 0 a = a
  RElt n a = RArray (n - 1) a

instance {-# OVERLAPPING #-} I.MRef (MRArray 0 (Bytes 'Inc)) where
  type Elt (MRArray 0 (Bytes 'Inc)) = RElt 0 (Bytes 'Inc)
  newRawMRef = newRawMArray 1
  {-# INLINE newRawMRef #-}
  readMRef ra = readBytesMArray ra 0
  {-# INLINE readMRef #-}
  writeMRef ra = writeBytesMArray ra 0
  {-# INLINE writeMRef #-}

instance {-# OVERLAPPING #-} I.MArray (MRArray 0 (Bytes 'Inc)) where
  type Array (MRArray 0 (Bytes 'Inc)) = RArray 0 (Bytes 'Inc)

  indexArray = indexBytesArray
  {-# INLINE indexArray #-}
  sizeOfArray = sizeOfArray
  {-# INLINE sizeOfArray #-}
  getSizeOfMArray = pure . sizeOfMArray
  {-# INLINE getSizeOfMArray #-}
  thawArray = thawArray
  {-# INLINE thawArray #-}
  freezeMArray = freezeMArray
  {-# INLINE freezeMArray #-}
  newRawMArray = newRawMArray
  {-# INLINE newRawMArray #-}
  readMArray = readBytesMArray
  {-# INLINE readMArray #-}
  writeMArray = writeBytesMArray
  {-# INLINE writeMArray #-}
  copyArray = copyArray
  {-# INLINE copyArray #-}
  moveMArray = moveMArray
  {-# INLINE moveMArray #-}


instance {-# OVERLAPPING #-} I.MRef (MRArray 0 (U.UArray e)) where
  type Elt (MRArray 0 (U.UArray e)) = RElt 0 (U.UArray e)
  newRawMRef = newRawMArray 1
  {-# INLINE newRawMRef #-}
  readMRef ra = readUnboxedFrozenMArray ra 0
  {-# INLINE readMRef #-}
  writeMRef ra = writeUnboxedFrozenMArray ra 0
  {-# INLINE writeMRef #-}

instance {-# OVERLAPPING #-} I.MArray (MRArray 0 (U.UArray e)) where
  type Array (MRArray 0 (U.UArray e)) = RArray 0 (U.UArray e)

  indexArray = indexUnboxedArray
  {-# INLINE indexArray #-}
  sizeOfArray = sizeOfArray
  {-# INLINE sizeOfArray #-}
  getSizeOfMArray = pure . sizeOfMArray
  {-# INLINE getSizeOfMArray #-}
  thawArray = thawArray
  {-# INLINE thawArray #-}
  freezeMArray = freezeMArray
  {-# INLINE freezeMArray #-}
  newRawMArray = newRawMArray
  {-# INLINE newRawMArray #-}
  readMArray = readUnboxedFrozenMArray
  {-# INLINE readMArray #-}
  writeMArray = writeUnboxedFrozenMArray
  {-# INLINE writeMArray #-}
  copyArray = copyArray
  {-# INLINE copyArray #-}
  moveMArray = moveMArray
  {-# INLINE moveMArray #-}

instance {-# OVERLAPPING #-} I.MRef (MRArray 0 (ByteArray 'Inc e)) where
  type Elt (MRArray 0 (ByteArray 'Inc e)) = RElt 0 (ByteArray 'Inc e)
  newRawMRef = newRawMArray 1
  {-# INLINE newRawMRef #-}
  readMRef rma = coerce <$> readBytesMArray rma 0
  {-# INLINE readMRef #-}
  writeMRef rma ba = writeBytesMArray rma 0 (coerce ba)
  {-# INLINE writeMRef #-}

instance {-# OVERLAPPING #-} I.MArray (MRArray 0 (ByteArray 'Inc e)) where
  type Array (MRArray 0 (ByteArray 'Inc e)) = RArray 0 (ByteArray 'Inc e)

  indexArray = coerce . indexBytesArray
  {-# INLINE indexArray #-}
  sizeOfArray = sizeOfArray
  {-# INLINE sizeOfArray #-}
  getSizeOfMArray = pure . sizeOfMArray
  {-# INLINE getSizeOfMArray #-}
  thawArray = thawArray
  {-# INLINE thawArray #-}
  freezeMArray = freezeMArray
  {-# INLINE freezeMArray #-}
  newRawMArray = newRawMArray
  {-# INLINE newRawMArray #-}
  readMArray ma = fmap coerce . readBytesMArray ma
  {-# INLINE readMArray #-}
  writeMArray ma i ba = writeBytesMArray ma i (coerce ba)
  {-# INLINE writeMArray #-}
  copyArray = copyArray
  {-# INLINE copyArray #-}
  moveMArray = moveMArray
  {-# INLINE moveMArray #-}

instance (KnownNat n, 1 <= n, RElt n e ~ RArray (n - 1) e) => I.MRef (MRArray n e) where
  type Elt (MRArray n e) = RElt n e
  newRawMRef = newRawMArray 1
  {-# INLINE newRawMRef #-}
  readMRef rma = readFrozenMArray rma 0
  {-# INLINE readMRef #-}
  writeMRef rma = writeFrozenMArray rma 0
  {-# INLINE writeMRef #-}

instance (KnownNat n, 1 <= n, RElt n e ~ RArray (n - 1) e) => I.MArray (MRArray n e) where
  type Array (MRArray n e) = RArray n e
  sizeOfArray = sizeOfArray
  {-# INLINE sizeOfArray #-}
  indexArray = indexArray
  {-# INLINE indexArray #-}
  getSizeOfMArray = pure . sizeOfMArray
  {-# INLINE getSizeOfMArray #-}
  thawArray = thawArray
  {-# INLINE thawArray #-}
  freezeMArray = freezeMArray
  {-# INLINE freezeMArray #-}
  newRawMArray = newRawMArray
  {-# INLINE newRawMArray #-}
  readMArray = readFrozenMArray
  {-# INLINE readMArray #-}
  writeMArray = writeFrozenMArray
  {-# INLINE writeMArray #-}
  copyArray = copyArray
  {-# INLINE copyArray #-}
  moveMArray = moveMArray
  {-# INLINE moveMArray #-}


sizeOfArray :: RArray n a -> Size
sizeOfArray (RArray a#) = Size (I# (sizeofArrayArray# a#))
{-# INLINE sizeOfArray #-}

indexUnboxedArray :: RArray 0 b -> Int -> U.UArray e
indexUnboxedArray (RArray a#) (I# i#) = U.UArray (indexByteArrayArray# a# i#)
{-# INLINE indexUnboxedArray #-}

indexBytesArray :: RArray 0 b -> Int -> Bytes 'Inc
indexBytesArray (RArray a#) (I# i#) = fromByteArray# (indexByteArrayArray# a# i#)
{-# INLINE indexBytesArray #-}

indexArray :: (KnownNat n, 1 <= n) => RArray n e -> Int -> RArray (n - 1) e
indexArray (RArray a#) (I# i#) = RArray (indexArrayArrayArray# a# i#)
{-# INLINE indexArray #-}


indexArrayI :: I.MArray (MRArray n e) => RArray n e -> Int -> RElt n e
indexArrayI = I.indexArray
{-# INLINE indexArrayI #-}

-- |
--
-- @since 0.1.0
newRawMArray :: MonadPrim s m => Size -> m (MRArray n e s)
newRawMArray (Size (I# n#)) =
  prim $ \s ->
    case newArrayArray# n# s of
      (# s', ma# #) -> (# s', MRArray ma# #)
{-# INLINE newRawMArray #-}

-- Better interface:
-- newRawMArrayI :: (I.MArray (MRArray n) a, MonadPrim s m) => Size -> m (MRArray n a s)
-- newRawMArrayI = I.newRawMArray
-- {-# INLINE newRawMArrayI #-}


thawArray :: MonadPrim s m => RArray n e -> m (MRArray n e s)
thawArray (RArray a#) =
  prim $ \s ->
    case unsafeThawArrayArray# a# s of
      (# s', ma# #) -> (# s', MRArray ma# #)
{-# INLINE thawArray #-}

freezeMArray :: MonadPrim s m => MRArray n e s -> m (RArray n e)
freezeMArray (MRArray ma#) =
  prim $ \s ->
    case unsafeFreezeArrayArray# ma# s of
      (# s', a# #) -> (# s', RArray a# #)
{-# INLINE freezeMArray #-}


-- | Get the size of a mutable boxed array
--
-- >>> ma <- newMRArray 1024 "Element of each cell"
-- >>> sizeOfMRArray ma
-- Size 1024
--
-- @since 0.1.0
sizeOfMArray :: MRArray n e s -> Size
sizeOfMArray (MRArray ma#) = Size (I# (sizeofMutableArrayArray# ma#))
{-# INLINE sizeOfMArray #-}


-- | Read an element from mutable boxed array at a supplied index.
--
-- [Unsafe index] Negative or larger than array size can fail with unchecked exception
--
-- ==== __Examples__
--
-- >>> ma <- makeMRArray 10 (pure . ("Element ix: " ++) . show)
-- >>> readMRArray ma 5
-- "Element ix: 5"
--
-- @since 0.1.0
readMArray ::
     (KnownNat n, 1 <= n, marr ~ MRArray (n - 1) e s, MonadPrim s m)
  => MRArray n marr s
  -> Int
  -> m marr
readMArray (MRArray ma#) (I# i#) =
  prim $ \s ->
    case readMutableArrayArrayArray# ma# i# s of
      (# s', ma'# #) -> (# s', MRArray ma'# #)
{-# INLINE readMArray #-}

readFrozenMArray ::
     (KnownNat n, 1 <= n, MonadPrim s m)
  => MRArray n e s
  -> Int
  -> m (RArray (n - 1) e)
readFrozenMArray (MRArray ma#) (I# i#) =
  prim $ \s ->
    case readArrayArrayArray# ma# i# s of
      (# s', a# #) -> (# s', RArray a# #)
{-# INLINE readFrozenMArray #-}

readUnboxedMArray :: MonadPrim s m => MRArray 0 e s -> Int -> m (U.MUArray e s)
readUnboxedMArray (MRArray ma#) (I# i#) =
  prim $ \s ->
    case readMutableByteArrayArray# ma# i# s of
      (# s', mba# #) -> (# s', U.MUArray mba# #)
{-# INLINE readUnboxedMArray #-}

readUnboxedFrozenMArray :: MonadPrim s m => MRArray 0 b s -> Int -> m (U.UArray e)
readUnboxedFrozenMArray (MRArray ma#) (I# i#) =
  prim $ \s ->
    case readByteArrayArray# ma# i# s of
      (# s', ba# #) -> (# s', U.UArray ba# #)
{-# INLINE readUnboxedFrozenMArray #-}


readMBytesMArray :: MonadPrim s m => MRArray 0 e s -> Int -> m (MBytes 'Inc s)
readMBytesMArray (MRArray ma#) (I# i#) =
  prim $ \s ->
    case readMutableByteArrayArray# ma# i# s of
      (# s', mba# #) -> (# s', fromMutableByteArray# mba# #)
{-# INLINE readMBytesMArray #-}

readBytesMArray :: MonadPrim s m => MRArray 0 b s -> Int -> m (Bytes 'Inc)
readBytesMArray (MRArray ma#) (I# i#) =
  prim $ \s ->
    case readByteArrayArray# ma# i# s of
      (# s', ba# #) -> (# s', fromByteArray# ba# #)
{-# INLINE readBytesMArray #-}

-- | Write an element into a mutable boxed array at a supplied index strictly. An
-- element will be evaluated to WHNF.
--
-- [Unsafe index] Negative or larger than array size can fail with unchecked exception
--
-- ==== __Examples__
--
-- >>> ma <- newMRArray 4 (Nothing :: Maybe Int)
-- >>> writeMRArray ma 2 (Just 2)
-- >>> freezeMRArray ma
-- Array [Nothing,Nothing,Just 2,Nothing]
--
-- Important to note that an element is evaluated prior to being written into a cell, so
-- it will not overwrite a value with if it evaluates to an exception:
--
-- >>> import Control.Exception
-- >>> writeMRArray ma 2 (throw DivideByZero)
-- *** Exception: divide by zero
-- >>> freezeMRArray ma
-- Array [Nothing,Nothing,Just 2,Nothing]
--
-- But it is evaluated to Normal Form, so it is still possible to write something that
-- eventually evaluates to bottom.
--
-- >>> writeMRArray ma 3 (Just (7 `div` 0 ))
-- >>> freezeMRArray ma
-- Array [Nothing,Nothing,Just 2,Just *** Exception: divide by zero
--
-- Either `deepseq` or `writeMArrayDeep` can be used to alleviate that.
--
-- @since 0.1.0
writeMArray ::
     (KnownNat n, 1 <= n, MonadPrim s m)
  => MRArray n e s
  -> Int
  -> MRArray (n - 1) e s
  -> m ()
writeMArray (MRArray ma#) (I# i#) (MRArray e#) =
  prim_ (writeMutableArrayArrayArray# ma# i# e#)
{-# INLINE writeMArray #-}

writeFrozenMArray ::
     (KnownNat n, 1 <= n, MonadPrim s m)
  => MRArray n e s
  -> Int
  -> RArray (n - 1) e
  -> m ()
writeFrozenMArray (MRArray ma#) (I# i#) (RArray e#) =
  prim_ (writeArrayArrayArray# ma# i# e#)
{-# INLINE writeFrozenMArray #-}

writeUnboxedMArray ::
     MonadPrim s m => MRArray 0 e s -> Int -> U.MUArray e s -> m ()
writeUnboxedMArray (MRArray ma#) (I# i#) (U.MUArray mba#) =
  prim_ (writeMutableByteArrayArray# ma# i# mba#)
{-# INLINE writeUnboxedMArray #-}


writeUnboxedFrozenMArray ::
     MonadPrim s m => MRArray 0 b s -> Int -> U.UArray e -> m ()
writeUnboxedFrozenMArray (MRArray ma#) (I# i#) (U.UArray ba#) =
  prim_ (writeByteArrayArray# ma# i# ba#)
{-# INLINE writeUnboxedFrozenMArray #-}


writeMBytesMArray ::
     MonadPrim s m => MRArray 0 e s -> Int -> MBytes 'Inc s -> m ()
writeMBytesMArray (MRArray ma#) (I# i#) mb =
  prim_ (writeMutableByteArrayArray# ma# i# (toMutableByteArray# mb))
{-# INLINE writeMBytesMArray #-}


writeBytesMArray ::
     MonadPrim s m => MRArray 0 b s -> Int -> Bytes 'Inc -> m ()
writeBytesMArray (MRArray ma#) (I# i#) b =
  prim_ (writeByteArrayArray# ma# i# (toByteArray# b))
{-# INLINE writeBytesMArray #-}


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
copyArray ::
     MonadPrim s m
  => RArray n e -- ^ Source immutable array
  -> Int -- ^ Offset into the source immutable array
  -> MRArray n e s -- ^ Destination mutable array
  -> Int -- ^ Offset into the destination mutable array
  -> Size -- ^ Number of elements to copy over
  -> m ()
copyArray (RArray src#) (I# srcOff#) (MRArray dst#) (I# dstOff#) (Size (I# n#)) =
  prim_ (copyArrayArray# src# srcOff# dst# dstOff# n#)
{-# INLINE copyArray #-}

-- | Copy a subsection of a mutable array into a subsection of another or the same
-- mutable array. Therefore, unlike `copyArray`, memory overlap is allowed.
--
-- [Unsafe offset] Each offset cannot be negative or larger than the size of a
-- corresponding array, otherwise it can result in an unchecked exception
--
-- [Unsafe new size] Number of elements to be copied cannot be larger than the size of an
-- each array minus their corersponding offsets.
--
-- @since 0.1.0
moveMArray ::
     MonadPrim s m
  => MRArray n e s -- ^ Source mutable array
  -> Int -- ^ Offset into the source mutable array
  -> MRArray n e s -- ^ Destination mutable array
  -> Int -- ^ Offset into the destination mutable array
  -> Size -- ^ Number of elements to copy over
  -> m ()
moveMArray (MRArray src#) (I# srcOff#) (MRArray dst#) (I# dstOff#) (Size (I# n#)) =
  prim_ (copyMutableArrayArray# src# srcOff# dst# dstOff# n#)
{-# INLINE moveMArray #-}

