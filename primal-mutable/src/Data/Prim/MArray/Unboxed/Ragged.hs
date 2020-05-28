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
  -- , makeRArray
  -- , makeRArrayM
  , sizeOfRArray
  , indexRArray
  -- * Mutable
  -- ** Create
  -- , newMRArray
  , newRawMRArray
  -- , newMRArrayLazy
  -- , makeMRArray
  -- , createRArrayM
  -- , createRArrayM_
  , sizeOfMRArray
  -- ** Access
  , readMRArray
  , writeMRArray
  , readMBytesMRArray
  , writeMBytesMRArray
  , thawRArray
  -- , thawCopyRArray -- TODO: implement deep copy
  , freezeMRArray
  -- , freezeCopyMRArray
  , copyRArray
  , moveMRArray
  -- , cloneRArray -- TODO: implement deep clone
  -- , cloneMRArray
  -- * List
  -- , fromListRArray
  -- , fromListRArrayN
  -- , toListRArray
  -- -- * Helpers
  -- , foldrRArray
  -- , traverseRArray
  ) where

import Control.DeepSeq
import Control.Exception (ArrayException(UndefinedElement), throw)
import Control.Monad.ST
import Control.Prim.Monad
import Data.Prim.Memory.Bytes
import Data.Prim.Memory.ByteArray
import Data.Prim.MRef.Atomic
import Data.Prim.MRef.Internal
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

instance {-# OVERLAPPING #-} MRef (MRArray 0 (Bytes 'Inc)) where
  type Elt (MRArray 0 (Bytes 'Inc)) = RElt 0 (Bytes 'Inc)
  newRawMRef = newRawMRArray 1
  {-# INLINE newRawMRef #-}
  readMRef ra = readBytesMRArray ra 0
  {-# INLINE readMRef #-}
  writeMRef ra = writeBytesMRArray ra 0
  {-# INLINE writeMRef #-}

instance {-# OVERLAPPING #-} I.MArray (MRArray 0 (Bytes 'Inc)) where
  type Array (MRArray 0 (Bytes 'Inc)) = RArray 0 (Bytes 'Inc)

  indexArray = indexBytesRArray
  {-# INLINE indexArray #-}
  sizeOfArray = sizeOfRArray
  {-# INLINE sizeOfArray #-}
  getSizeOfMArray = pure . sizeOfMRArray
  {-# INLINE getSizeOfMArray #-}
  thawArray = thawRArray
  {-# INLINE thawArray #-}
  freezeMArray = freezeMRArray
  {-# INLINE freezeMArray #-}
  newRawMArray = newRawMRArray
  {-# INLINE newRawMArray #-}
  readMArray = readBytesMRArray
  {-# INLINE readMArray #-}
  writeMArray = writeBytesMRArray
  {-# INLINE writeMArray #-}
  copyArray = copyRArray
  {-# INLINE copyArray #-}
  moveMArray = moveMRArray
  {-# INLINE moveMArray #-}


instance {-# OVERLAPPING #-} MRef (MRArray 0 (U.UArray e)) where
  type Elt (MRArray 0 (U.UArray e)) = RElt 0 (U.UArray e)
  newRawMRef = newRawMRArray 1
  {-# INLINE newRawMRef #-}
  readMRef ra = readUnboxedFrozenMRArray ra 0
  {-# INLINE readMRef #-}
  writeMRef ra = writeUnboxedFrozenMRArray ra 0
  {-# INLINE writeMRef #-}

instance {-# OVERLAPPING #-} I.MArray (MRArray 0 (U.UArray e)) where
  type Array (MRArray 0 (U.UArray e)) = RArray 0 (U.UArray e)

  indexArray = indexUnboxedRArray
  {-# INLINE indexArray #-}
  sizeOfArray = sizeOfRArray
  {-# INLINE sizeOfArray #-}
  getSizeOfMArray = pure . sizeOfMRArray
  {-# INLINE getSizeOfMArray #-}
  thawArray = thawRArray
  {-# INLINE thawArray #-}
  freezeMArray = freezeMRArray
  {-# INLINE freezeMArray #-}
  newRawMArray = newRawMRArray
  {-# INLINE newRawMArray #-}
  readMArray = readUnboxedFrozenMRArray
  {-# INLINE readMArray #-}
  writeMArray = writeUnboxedFrozenMRArray
  {-# INLINE writeMArray #-}
  copyArray = copyRArray
  {-# INLINE copyArray #-}
  moveMArray = moveMRArray
  {-# INLINE moveMArray #-}

instance {-# OVERLAPPING #-} MRef (MRArray 0 (ByteArray 'Inc e)) where
  type Elt (MRArray 0 (ByteArray 'Inc e)) = RElt 0 (ByteArray 'Inc e)
  newRawMRef = newRawMRArray 1
  {-# INLINE newRawMRef #-}
  readMRef rma = coerce <$> readBytesMRArray rma 0
  {-# INLINE readMRef #-}
  writeMRef rma ba = writeBytesMRArray rma 0 (coerce ba)
  {-# INLINE writeMRef #-}

instance {-# OVERLAPPING #-} I.MArray (MRArray 0 (ByteArray 'Inc e)) where
  type Array (MRArray 0 (ByteArray 'Inc e)) = RArray 0 (ByteArray 'Inc e)

  indexArray = coerce . indexBytesRArray
  {-# INLINE indexArray #-}
  sizeOfArray = sizeOfRArray
  {-# INLINE sizeOfArray #-}
  getSizeOfMArray = pure . sizeOfMRArray
  {-# INLINE getSizeOfMArray #-}
  thawArray = thawRArray
  {-# INLINE thawArray #-}
  freezeMArray = freezeMRArray
  {-# INLINE freezeMArray #-}
  newRawMArray = newRawMRArray
  {-# INLINE newRawMArray #-}
  readMArray ma = fmap coerce . readBytesMRArray ma
  {-# INLINE readMArray #-}
  writeMArray ma i ba = writeBytesMRArray ma i (coerce ba)
  {-# INLINE writeMArray #-}
  copyArray = copyRArray
  {-# INLINE copyArray #-}
  moveMArray = moveMRArray
  {-# INLINE moveMArray #-}

instance (KnownNat n, 1 <= n, RElt n e ~ RArray (n - 1) e) => MRef (MRArray n e) where
  type Elt (MRArray n e) = RElt n e
  newRawMRef = newRawMRArray 1
  {-# INLINE newRawMRef #-}
  readMRef rma = readFrozenMRArray rma 0
  {-# INLINE readMRef #-}
  writeMRef rma = writeFrozenMRArray rma 0
  {-# INLINE writeMRef #-}

instance (KnownNat n, 1 <= n, RElt n e ~ RArray (n - 1) e) => I.MArray (MRArray n e) where
  type Array (MRArray n e) = RArray n e
  sizeOfArray = sizeOfRArray
  {-# INLINE sizeOfArray #-}
  indexArray = indexRArray
  {-# INLINE indexArray #-}
  getSizeOfMArray = pure . sizeOfMRArray
  {-# INLINE getSizeOfMArray #-}
  thawArray = thawRArray
  {-# INLINE thawArray #-}
  freezeMArray = freezeMRArray
  {-# INLINE freezeMArray #-}
  newRawMArray = newRawMRArray
  {-# INLINE newRawMArray #-}
  readMArray = readFrozenMRArray
  {-# INLINE readMArray #-}
  writeMArray = writeFrozenMRArray
  {-# INLINE writeMArray #-}
  copyArray = copyRArray
  {-# INLINE copyArray #-}
  moveMArray = moveMRArray
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


indexRArrayI :: I.MArray (MRArray n e) => RArray n e -> Int -> RElt n e
indexRArrayI = I.indexArray
{-# INLINE indexRArrayI #-}

-- |
--
-- @since 0.1.0
newRawMRArray :: MonadPrim s m => Size -> m (MRArray n e s)
newRawMRArray (Size (I# n#)) =
  prim $ \s ->
    case newArrayArray# n# s of
      (# s', ma# #) -> (# s', MRArray ma# #)
{-# INLINE newRawMRArray #-}

-- Better interface:
-- newRawMRArrayI :: (I.MArray (MRArray n) a, MonadPrim s m) => Size -> m (MRArray n a s)
-- newRawMRArrayI = I.newRawMRArray
-- {-# INLINE newRawMRArrayI #-}


thawRArray :: MonadPrim s m => RArray n e -> m (MRArray n e s)
thawRArray (RArray a#) =
  prim $ \s ->
    case unsafeThawArrayArray# a# s of
      (# s', ma# #) -> (# s', MRArray ma# #)
{-# INLINE thawRArray #-}

freezeMRArray :: MonadPrim s m => MRArray n e s -> m (RArray n e)
freezeMRArray (MRArray ma#) =
  prim $ \s ->
    case unsafeFreezeArrayArray# ma# s of
      (# s', a# #) -> (# s', RArray a# #)
{-# INLINE freezeMRArray #-}


-- | Get the size of a mutable boxed array
--
-- >>> ma <- newMRArray 1024 "Element of each cell"
-- >>> sizeOfMRArray ma
-- Size 1024
--
-- @since 0.1.0
sizeOfMRArray :: MRArray n e s -> Size
sizeOfMRArray (MRArray ma#) = Size (I# (sizeofMutableArrayArray# ma#))
{-# INLINE sizeOfMRArray #-}


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
readMRArray ::
     (KnownNat n, 1 <= n, marr ~ MRArray (n - 1) e s, MonadPrim s m)
  => MRArray n marr s
  -> Int
  -> m marr
readMRArray (MRArray ma#) (I# i#) =
  prim $ \s ->
    case readMutableArrayArrayArray# ma# i# s of
      (# s', ma'# #) -> (# s', MRArray ma'# #)
{-# INLINE readMRArray #-}

readFrozenMRArray ::
     (KnownNat n, 1 <= n, MonadPrim s m)
  => MRArray n e s
  -> Int
  -> m (RArray (n - 1) e)
readFrozenMRArray (MRArray ma#) (I# i#) =
  prim $ \s ->
    case readArrayArrayArray# ma# i# s of
      (# s', a# #) -> (# s', RArray a# #)
{-# INLINE readFrozenMRArray #-}

readUnboxedMRArray :: MonadPrim s m => MRArray 0 e s -> Int -> m (U.MUArray e s)
readUnboxedMRArray (MRArray ma#) (I# i#) =
  prim $ \s ->
    case readMutableByteArrayArray# ma# i# s of
      (# s', mba# #) -> (# s', U.MUArray mba# #)
{-# INLINE readUnboxedMRArray #-}

readUnboxedFrozenMRArray :: MonadPrim s m => MRArray 0 b s -> Int -> m (U.UArray e)
readUnboxedFrozenMRArray (MRArray ma#) (I# i#) =
  prim $ \s ->
    case readByteArrayArray# ma# i# s of
      (# s', ba# #) -> (# s', U.UArray ba# #)
{-# INLINE readUnboxedFrozenMRArray #-}


readMBytesMRArray :: MonadPrim s m => MRArray 0 e s -> Int -> m (MBytes 'Inc s)
readMBytesMRArray (MRArray ma#) (I# i#) =
  prim $ \s ->
    case readMutableByteArrayArray# ma# i# s of
      (# s', mba# #) -> (# s', fromMutableByteArray# mba# #)
{-# INLINE readMBytesMRArray #-}

readBytesMRArray :: MonadPrim s m => MRArray 0 b s -> Int -> m (Bytes 'Inc)
readBytesMRArray (MRArray ma#) (I# i#) =
  prim $ \s ->
    case readByteArrayArray# ma# i# s of
      (# s', ba# #) -> (# s', fromByteArray# ba# #)
{-# INLINE readBytesMRArray #-}

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
-- Either `deepseq` or `writeMRArrayDeep` can be used to alleviate that.
--
-- @since 0.1.0
writeMRArray ::
     (KnownNat n, 1 <= n, MonadPrim s m)
  => MRArray n e s
  -> Int
  -> MRArray (n - 1) e s
  -> m ()
writeMRArray (MRArray ma#) (I# i#) (MRArray e#) =
  prim_ (writeMutableArrayArrayArray# ma# i# e#)
{-# INLINE writeMRArray #-}

writeFrozenMRArray ::
     (KnownNat n, 1 <= n, MonadPrim s m)
  => MRArray n e s
  -> Int
  -> RArray (n - 1) e
  -> m ()
writeFrozenMRArray (MRArray ma#) (I# i#) (RArray e#) =
  prim_ (writeArrayArrayArray# ma# i# e#)
{-# INLINE writeFrozenMRArray #-}

writeUnboxedMRArray ::
     MonadPrim s m => MRArray 0 e s -> Int -> U.MUArray e s -> m ()
writeUnboxedMRArray (MRArray ma#) (I# i#) (U.MUArray mba#) =
  prim_ (writeMutableByteArrayArray# ma# i# mba#)
{-# INLINE writeUnboxedMRArray #-}


writeUnboxedFrozenMRArray ::
     MonadPrim s m => MRArray 0 b s -> Int -> U.UArray e -> m ()
writeUnboxedFrozenMRArray (MRArray ma#) (I# i#) (U.UArray ba#) =
  prim_ (writeByteArrayArray# ma# i# ba#)
{-# INLINE writeUnboxedFrozenMRArray #-}


writeMBytesMRArray ::
     MonadPrim s m => MRArray 0 e s -> Int -> MBytes 'Inc s -> m ()
writeMBytesMRArray (MRArray ma#) (I# i#) mb =
  prim_ (writeMutableByteArrayArray# ma# i# (toMutableByteArray# mb))
{-# INLINE writeMBytesMRArray #-}


writeBytesMRArray ::
     MonadPrim s m => MRArray 0 b s -> Int -> Bytes 'Inc -> m ()
writeBytesMRArray (MRArray ma#) (I# i#) b =
  prim_ (writeByteArrayArray# ma# i# (toByteArray# b))
{-# INLINE writeBytesMRArray #-}


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
  -> MRArray n e s -- ^ Destination mutable array
  -> Int -- ^ Offset into the destination mutable array
  -> Size -- ^ Number of elements to copy over
  -> m ()
copyRArray (RArray src#) (I# srcOff#) (MRArray dst#) (I# dstOff#) (Size (I# n#)) =
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
moveMRArray ::
     MonadPrim s m
  => MRArray n e s -- ^ Source mutable array
  -> Int -- ^ Offset into the source mutable array
  -> MRArray n e s -- ^ Destination mutable array
  -> Int -- ^ Offset into the destination mutable array
  -> Size -- ^ Number of elements to copy over
  -> m ()
moveMRArray (MRArray src#) (I# srcOff#) (MRArray dst#) (I# dstOff#) (Size (I# n#)) =
  prim_ (copyMutableArrayArray# src# srcOff# dst# dstOff# n#)
{-# INLINE moveMRArray #-}

