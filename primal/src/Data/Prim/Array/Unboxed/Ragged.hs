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
-- Module      : Data.Prim.Array.Unboxed.Ragged
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Array.Unboxed.Ragged
  ( Array
  , pattern Array
  , RaggedArray
  , MArray
  , pattern MArray
  , RaggedMArray
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
  -- , atomicModifyFetchMArray
  -- , atomicFetchModifyMArray
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
import Data.Prim.Array.Internal (Size(..))
import qualified Data.Prim.Array.Unboxed as U
import qualified Data.Prim.Array.Internal as I
import Foreign.Prim
import GHC.TypeLits


-- | Check if both of the arrays refer to the exact same one. None of the elements are
-- evaluated.
instance Eq (RaggedMArray n a s) where
  MArray ma1# == MArray ma2# = isTrue# (sameMutableArrayArray# ma1# ma2#)

type Array = RaggedArray

data RaggedArray (n :: Nat) a = Array ArrayArray#

type MArray = RaggedMArray

data RaggedMArray (n :: Nat) a s = MArray (MutableArrayArray# s)


instance {-# OVERLAPPING #-} I.MArray (RaggedMArray 0 (U.UnboxedArray a)) where
  type IArray (RaggedMArray 0 (U.UnboxedArray a)) = RaggedArray 0 (U.UnboxedArray a)
  type Elt (RaggedMArray 0 (U.UnboxedArray a)) = RaggedElt 0 (U.UnboxedArray a)

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

type family RaggedElt n a where
  RaggedElt 0 a = a
  RaggedElt n a = RaggedArray (n - 1) a

instance (KnownNat n, 1 <= n, RaggedElt n a ~ RaggedArray (n - 1) a) =>
         I.MArray (RaggedMArray n a) where
  type IArray (RaggedMArray n a) = RaggedArray n a
  type Elt (RaggedMArray n a) = RaggedElt n a
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


sizeOfArray :: Array n a -> Size
sizeOfArray (Array a#) = Size (I# (sizeofArrayArray# a#))
{-# INLINE sizeOfArray #-}

indexUnboxedArray :: Array 0 b -> Int -> U.Array a
indexUnboxedArray (Array a#) (I# i#) = U.Array (indexByteArrayArray# a# i#)
{-# INLINE indexUnboxedArray #-}

indexArray :: (KnownNat n, 1 <= n) => Array n a -> Int -> Array (n - 1) a
indexArray (Array a#) (I# i#) = Array (indexArrayArrayArray# a# i#)
{-# INLINE indexArray #-}


indexArrayI :: I.MArray (RaggedMArray n a) => RaggedArray n a -> Int -> RaggedElt n a
indexArrayI = I.indexArray
{-# INLINE indexArrayI #-}

-- |
--
-- @since 0.1.0
newRawMArray :: MonadPrim s m => Size -> m (MArray n a s)
newRawMArray (Size (I# n#)) =
  prim $ \s ->
    case newArrayArray# n# s of
      (# s', ma# #) -> (# s', MArray ma# #)
{-# INLINE newRawMArray #-}

-- Better interface:
-- newRawMArrayI :: (I.MArray (MArray n) a, MonadPrim s m) => Size -> m (MArray n a s)
-- newRawMArrayI = I.newRawMArray
-- {-# INLINE newRawMArrayI #-}


thawArray :: MonadPrim s m => Array n a -> m (MArray n a s)
thawArray (Array a#) =
  prim $ \s ->
    case unsafeThawArrayArray# a# s of
      (# s', ma# #) -> (# s', MArray ma# #)
{-# INLINE thawArray #-}

freezeMArray :: MonadPrim s m => MArray n a s -> m (Array n a)
freezeMArray (MArray ma#) =
  prim $ \s ->
    case unsafeFreezeArrayArray# ma# s of
      (# s', a# #) -> (# s', Array a# #)
{-# INLINE freezeMArray #-}


-- | Get the size of a mutable boxed array
--
-- >>> ma <- newMArray 1024 "Element of each cell"
-- >>> sizeOfMArray ma
-- Size 1024
--
-- @since 0.1.0
sizeOfMArray :: MArray n a s -> Size
sizeOfMArray (MArray ma#) = Size (I# (sizeofMutableArrayArray# ma#))
{-# INLINE sizeOfMArray #-}


-- | Read an element from mutable boxed array at a supplied index.
--
-- [Unsafe index] Negative or larger than array size can fail with unchecked exception
--
-- ==== __Examples__
--
-- >>> ma <- makeMArray 10 (pure . ("Element ix: " ++) . show)
-- >>> readMArray ma 5
-- "Element ix: 5"
--
-- @since 0.1.0
readMArray ::
     (KnownNat n, 1 <= n, marr ~ MArray (n - 1) a s, MonadPrim s m)
  => MArray n marr s
  -> Int
  -> m marr
readMArray (MArray ma#) (I# i#) =
  prim $ \s ->
    case readMutableArrayArrayArray# ma# i# s of
      (# s', ma'# #) -> (# s', MArray ma'# #)
{-# INLINE readMArray #-}

readFrozenMArray ::
     (KnownNat n, 1 <= n, MonadPrim s m)
  => MArray n a s
  -> Int
  -> m (Array (n - 1) a)
readFrozenMArray (MArray ma#) (I# i#) =
  prim $ \s ->
    case readArrayArrayArray# ma# i# s of
      (# s', a# #) -> (# s', Array a# #)
{-# INLINE readFrozenMArray #-}

readUnboxedMArray :: MonadPrim s m => MArray 0 a s -> Int -> m (U.MArray a s)
readUnboxedMArray (MArray ma#) (I# i#) =
  prim $ \s ->
    case readMutableByteArrayArray# ma# i# s of
      (# s', mba# #) -> (# s', U.MArray mba# #)
{-# INLINE readUnboxedMArray #-}

readUnboxedFrozenMArray :: MonadPrim s m => MArray 0 b s -> Int -> m (U.Array a)
readUnboxedFrozenMArray (MArray ma#) (I# i#) =
  prim $ \s ->
    case readByteArrayArray# ma# i# s of
      (# s', ba# #) -> (# s', U.Array ba# #)
{-# INLINE readUnboxedFrozenMArray #-}

-- | Write an element into a mutable boxed array at a supplied index strictly. An
-- element will be evaluated to WHNF.
--
-- [Unsafe index] Negative or larger than array size can fail with unchecked exception
--
-- ==== __Examples__
--
-- >>> ma <- newMArray 4 (Nothing :: Maybe Int)
-- >>> writeMArray ma 2 (Just 2)
-- >>> freezeMArray ma
-- Array [Nothing,Nothing,Just 2,Nothing]
--
-- Important to note that an element is evaluated prior to being written into a cell, so
-- it will not overwrite a value with if it evaluates to an exception:
--
-- >>> import Control.Exception
-- >>> writeMArray ma 2 (throw DivideByZero)
-- *** Exception: divide by zero
-- >>> freezeMArray ma
-- Array [Nothing,Nothing,Just 2,Nothing]
--
-- But it is evaluated to Normal Form, so it is still possible to write something that
-- eventually evaluates to bottom.
--
-- >>> writeMArray ma 3 (Just (7 `div` 0 ))
-- >>> freezeMArray ma
-- Array [Nothing,Nothing,Just 2,Just *** Exception: divide by zero
--
-- Either `deepseq` or `writeMArrayDeep` can be used to alleviate that.
--
-- @since 0.1.0
writeMArray ::
     (KnownNat n, 1 <= n, MonadPrim s m)
  => MArray n a s
  -> Int
  -> MArray (n - 1) a s
  -> m ()
writeMArray (MArray ma#) (I# i#) (MArray e#) =
  prim_ (writeMutableArrayArrayArray# ma# i# e#)
{-# INLINE writeMArray #-}

writeFrozenMArray ::
     (KnownNat n, 1 <= n, MonadPrim s m)
  => MArray n a s
  -> Int
  -> Array (n - 1) a
  -> m ()
writeFrozenMArray (MArray ma#) (I# i#) (Array e#) =
  prim_ (writeArrayArrayArray# ma# i# e#)
{-# INLINE writeFrozenMArray #-}

writeUnboxedMArray ::
     MonadPrim s m => MArray n a s -> Int -> U.MArray a s -> m ()
writeUnboxedMArray (MArray ma#) (I# i#) (U.MArray mba#) =
  prim_ (writeMutableByteArrayArray# ma# i# mba#)
{-# INLINE writeUnboxedMArray #-}


writeUnboxedFrozenMArray ::
     MonadPrim s m => MArray 0 b s -> Int -> U.Array a -> m ()
writeUnboxedFrozenMArray (MArray ma#) (I# i#) (U.Array ba#) =
  prim_ (writeByteArrayArray# ma# i# ba#)
{-# INLINE writeUnboxedFrozenMArray #-}


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
  => Array n a -- ^ Source immutable array
  -> Int -- ^ Offset into the source immutable array
  -> MArray n a s -- ^ Destination mutable array
  -> Int -- ^ Offset into the destination mutable array
  -> Size -- ^ Number of elements to copy over
  -> m ()
copyArray (Array src#) (I# srcOff#) (MArray dst#) (I# dstOff#) (Size (I# n#)) =
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
  => MArray n a s -- ^ Source mutable array
  -> Int -- ^ Offset into the source mutable array
  -> MArray n a s -- ^ Destination mutable array
  -> Int -- ^ Offset into the destination mutable array
  -> Size -- ^ Number of elements to copy over
  -> m ()
moveMArray (MArray src#) (I# srcOff#) (MArray dst#) (I# dstOff#) (Size (I# n#)) =
  prim_ (copyMutableArrayArray# src# srcOff# dst# dstOff# n#)
{-# INLINE moveMArray #-}

