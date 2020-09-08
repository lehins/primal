{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide, not-home #-}
-- |
-- Module      : Data.Prim.Memory.Internal
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Memory.Internal
  ( module Data.Prim.Memory.Internal
  , module Data.Prim.Memory.Bytes.Internal
  ) where

import Control.Exception
import Control.Monad.ST
import Control.Prim.Monad
import Control.Prim.Monad.Unsafe
import Data.Foldable as Foldable
import Data.Kind
import Data.List as List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Monoid as Monoid
import Data.Prim
import Data.Prim.Memory.Bytes.Internal
import Data.Prim.Memory.ByteString
import Data.Prim.Memory.ForeignPtr
import Data.Prim.Memory.Ptr
import qualified Data.Semigroup as Semigroup
import Foreign.Prim
import Numeric (showHex)

-- | Type class that can be implemented for an immutable data type that provides
-- direct read-only access to memory
class MemRead mr where

  -- | Number of bytes allocated by the data type available for reading. This is primarily
  -- used for cloning and conversion.
  --
  -- @since 0.1.0
  byteCountMem :: mr -> Count Word8

  -- | Read an element with an offset in number of elements, rather than bytes as it is
  -- the case with `indexByteOffMem`.
  --
  -- [Unsafe] Bounds are not checked. When precondition for @off@ argument is violated the
  -- result is either unpredictable output or failure with a segfault.
  --
  -- @since 0.1.0
  indexOffMem :: Prim e
    => mr -- ^ /memRead/ - Memory to read an element from
    -> Off e
    -- ^ /off/ - Offset in number of elements from the beginning of @memRead@
    --
    -- /__Preconditions:__/
    --
    -- > 0 <= off
    --
    -- > unOff (toByteOff off) <= unCount (byteCountMem memRead - byteCountType @e)
    --
    -> e
  indexOffMem mr off = indexByteOffMem mr (toByteOff off)
  {-# INLINE indexOffMem #-}

  -- | Read an element with an offset in number of bytes. Bounds are not checked.
  --
  -- [Unsafe] When precondition for @off@ argument is violated the result is either
  -- unpredictable output or failure with a segfault.
  --
  -- @since 0.1.0
  indexByteOffMem :: Prim e
    => mr -- ^ /memRead/ - Memory to read an element from
    -> Off Word8
    -- ^ /off/ - Offset in number of elements from the beginning of @memRead@
    --
    -- /__Preconditions:__/
    --
    -- > 0 <= unOff off
    --
    -- > unOff off <= unCount (byteCountMem memRead - byteCountType @e)
    --
    -> e

  -- | Copy contiguous chunk of memory from the read only memory into the target mutable
  -- `MBytes`. Source and target can't refer to the same memory regions, that would of
  -- course mean that the source is not immutable thus imply a violation of some other
  -- invariant elsewhere in the code.
  --
  -- [Unsafe] When a precondition for either of the offsets @memSourceOff@, @memTargetOff@
  -- or the element count @memCount@ is violated the result is either unpredictable output or
  -- failure with a segfault.
  --
  -- @since 0.1.0
  copyByteOffToMBytesMem ::
       (MonadPrim s m, Prim e)
    => mr -- ^ /memSourceRead/ - Source from where to copy
    -> Off Word8
    -- ^ /memSourceOff/ - Offset into source memory in number of bytes
    --
    -- /__Preconditions:__/
    --
    -- > 0 <= memSourceOff
    --
    -- > unOff memSourceOff <= unCount (byteCountMem memSourceRead - byteCountType @e)
    -> MBytes p s -- ^ /memTargetWrite/ - Target mutable memory
    -> Off Word8
    -- ^ /memTargetOff/ -  Offset into target memory in number of bytes
    --
    -- /__Preconditions:__/
    --
    -- > 0 <= memTargetOff
    --
    -- > unOff memTargetOff <= unCount (byteCountMem memTargetWrite - byteCountType @e)
    -> Count e
    -- ^ /memCount/ - Number of elements of type @e@ to copy
    --
    -- /__Preconditions:__/
    --
    -- > 0 <= memCount
    --
    -- > fromCount memCount + unOff memSourceOff <= unCount (byteCountMem memSourceRead - byteCountType @e)
    --
    -- > fromCount memCount + unOff memTargetOff <= unCount (byteCountMem memTargetRead - byteCountType @e)
    -> m ()

  -- | Copy contiguous chunk of memory from the read only memory into the target mutable
  -- `Ptr`. Source and target can't refer to the same memory regions, that would of
  -- course mean that the source is not immutable thus imply a violation of some other
  -- invariant elsewhere in the code.
  --
  -- [Unsafe] When a precondition for either of the offsets @memSourceOff@, @memTargetOff@
  -- or the element count @memCount@ is violated the result is either unpredictable output or
  -- failure with a segfault.
  --
  -- @since 0.1.0
  copyByteOffToPtrMem ::
       (MonadPrim s m, Prim e)
    => mr -- ^ /memSourceRead/ - Source from where to copy
    -> Off Word8
    -- ^ /memSourceOff/ - Offset into source memory in number of bytes
    --
    -- /__Preconditions:__/
    --
    -- > 0 <= memSourceOff
    --
    -- > unOff memSourceOff <= unCount (byteCountMem memSourceRead - byteCountType @e)
    -> Ptr e
    -- ^ /memTargetWrite/ - Pointer to the target mutable memory
    --
    -- /__Preconditions:__/
    --
    -- Once the pointer is advanced by @memTargetOff@ the next @fromCount memCount@ bytes must
    -- still belong to the same region of memory @memTargetWrite@
    -> Off Word8
    -- ^ /memTargetOff/ - Number of bytes to advance the pointer @memTargetWrite@ forward
    --
    -- /__Precondition:__/
    --
    -- Once the pointer is advanced by @memTargetOff@ it must still refer to the same
    -- memory region @memTargetWrite@
    -> Count e
    -- ^ /memCount/ - Number of elements of type @e@ to copy
    --
    -- /__Preconditions:__/
    --
    -- > 0 <= memCount
    --
    -- > fromCount memCount + unOff memSourceOff <= unCount (byteCountMem memSourceRead - byteCountType @e)
    --
    -- > fromCount memCount + unOff memTargetOff <= unCount (byteCountMem memTargetRead - byteCountType @e)
    -> m ()

  -- | Same as `compareByteOffMem`, but compare inside of a `MonadPrim` the read-only
  -- memory region to a region addressed by a `Ptr`.
  --
  -- [Unsafe] When any precondition for either of the offsets @memOff1@, @memOff2@, the
  -- pointer @memRead2@ or the element count @memCount@ is violated the result is either
  -- unpredictable output or failure with a segfault.
  --
  -- @since 0.1.0
  compareByteOffToPtrMem ::
       (MonadPrim s m, Prim e)
    => mr -- ^ /memRead1/ - First memory region
    -> Off Word8
    -- ^ /memOff1/ - Offset for @memRead1@ in number of bytes
    --
    -- /__Preconditions:__/
    --
    -- > 0 <= memOff1
    --
    -- > unOff memOff1 <= unCount (byteCountMem memRead1 - byteCountType @e)
    -> Ptr e
    -- ^ /memRead2/- Second memory region that can be accessed by a pointer
    --
    -- /__Preconditions__/
    --
    -- Once the pointer is advanced by @memOff2@ the next @fromCount memCount@ bytes must
    -- still belong to the same region of memory @memRead2@
    -> Off Word8
    -- ^ /memOff2/ - Number of bytes to advance the pointer @memRead2@ forward
    --
    -- /__Precondition:__/
    --
    -- Once the pointer is advanced by @memOff2@ it must still refer to the same memory
    -- region @memRead2@
    -> Count e -- ^ /memCount/ - Number of elements of type @e@ to compare as binary
    -- ^ /memCount/ - Number of elements of type @e@ to compare as binary
    --
    -- /__Preconditions:__/
    --
    -- > 0 <= memCount
    --
    -- > fromCount memCount + unOff memOff1 <= unCount (byteCountMem memRead1 - byteCountType @e)
    -> m Ordering

  -- | Same as `compareByteOffMem`, but compare the read-only memory region to `Bytes`
  -- inside of a `MonadPrim`.
  --
  -- [Unsafe] When any precondition for either of the offsets @memOff1@, @memOff2@ or the
  -- element count @memCount@ is violated the result is either unpredictable output or
  -- failure with a segfault.
  --
  -- @since 0.1.0
  compareByteOffToBytesMem ::
       (MonadPrim s m, Prim e)
    => mr -- ^ /memRead1/ - First memory region
    -> Off Word8
    -- ^ /memOff1/ - Offset for @memRead1@ in number of bytes
    --
    -- /__Preconditions:__/
    --
    -- > 0 <= memOff1
    --
    -- > unOff memOff1 <= unCount (byteCountMem memRead1 - byteCountType @e)
    -> Bytes p -- ^ /memRead2/- Second memory region that is backed by `Bytes`
    -> Off Word8
    -- ^ /memOff2/ - Offset for @memRead2@ in number of bytes
    --
    -- /__Preconditions:__/
    --
    -- > 0 <= memOff2
    --
    -- > unOff memOff2 <= unCount (byteCountMem memRead2 - byteCountType @e)
    -> Count e
    -- ^ /memCount/ - Number of elements of type @e@ to compare as binary
    --
    -- /__Preconditions:__/
    --
    -- > 0 <= memCount
    --
    -- > fromCount memCount + unOff memOff1 <= unCount (byteCountMem memRead1 - byteCountType @e)
    --
    -- > fromCount memCount + unOff memOff2 <= unCount (byteCountMem memRead2 - byteCountType @e)
    -> m Ordering

  -- | Compare two read-only regions of memory byte-by-byte. The very first mismatched
  -- byte will cause this function to produce `LT` if the byte in @memRead1@ is smaller
  -- than the one in @memRead2@ and `GT` if it is bigger. It is not a requirement to
  -- short-circuit on the first mismatch, but it is a good optimization to have for
  -- non-sensitive data. Memory regions that store security critical data may choose to
  -- implement this function to work in constant time.
  --
  -- This function is usually implemented by either one of `compareByteOffToPtrMem` or
  -- `compareByteOffToBytesMem`, depending on the nature of @mr@ type. However it differs
  -- from the aforementioned functions with a fact that it is pure non-monadic
  -- computation.
  --
  -- [Unsafe] When any precondition for either of the offsets @memOff1@, @memOff2@ or the
  -- element count @memCount@ is violated the result is either unpredictable output or
  -- failure with a segfault.
  --
  -- @since 0.1.0
  compareByteOffMem ::
       (MemRead mr', Prim e)
    => mr' -- ^ /memRead1/ - First memory region
    -> Off Word8
    -- ^ /memOff1/ - Offset for @memRead1@ in number of bytes
    --
    -- /__Preconditions:__/
    --
    -- > 0 <= memOff1
    --
    -- > unOff memOff1 <= unCount (byteCountMem memRead1 - byteCountType @e)
    -> mr -- ^ /memRead2/ - Second memory region
    -> Off Word8
    -- ^ /memOff2/ - Offset for @memRead2@ in number of bytes
    --
    -- /__Preconditions:__/
    --
    -- > 0 <= memOff2
    --
    -- > unOff memOff2 <= unCount (byteCountMem memRead2 - byteCountType @e)
    -> Count e
    -- ^ /memCount/ - Number of elements of type @e@ to compare as binary
    --
    -- /__Preconditions:__/
    --
    -- > 0 <= memCount
    --
    -- > fromCount memCount + unOff memOff1 <= unCount (byteCountMem memRead1 - byteCountType @e)
    --
    -- > fromCount memCount + unOff memOff2 <= unCount (byteCountMem memRead2 - byteCountType @e)
    -> Ordering

-- | Type class that can be implemented for a mutable data type that provides direct read
-- and write access to memory
class MemWrite mw where
  -- | Read an element with an offset in number of elements, rather than bytes as it is
  -- the case with `readByteOffMem`.
  --
  -- [Unsafe] Bounds are not checked. When precondition for @off@ argument is violated the
  -- result is either unpredictable output or failure with a segfault.
  --
  -- @since 0.1.0
  readOffMem :: (MonadPrim s m, Prim e)
    => mw s -- ^ /memRead/ - Memory region to read an element from
    -> Off e
    -- ^ /off/ - Offset in number of elements from the beginning of @memRead@
    --
    -- /__Preconditions:__/
    --
    -- > 0 <= off
    --
    -- Offset should refer to the same memory region. For types that also implement
    -- `MemAlloc` this can be described as:
    --
    -- > count <- getByteCountMem memRead
    -- > unOff (toByteOff off) <= unCount (count - byteCountType @e)
    --
    -> m e
  readOffMem mw off = readByteOffMem mw (toByteOff off)
  {-# INLINE readOffMem #-}

  -- | Read an element with an offset in number of bytes.
  --
  -- [Unsafe] Bounds are not checked. When precondition for @off@ argument is violated the
  -- result is either unpredictable output or failure with a segfault.
  --
  -- @since 0.1.0
  readByteOffMem :: (MonadPrim s m, Prim e)
    => mw s -- ^ /memRead/ - Memory region to read an element from
    -> Off Word8
    -- ^ /off/ - Offset in number of elements from the beginning of @memRead@
    --
    -- /__Preconditions:__/
    --
    -- > 0 <= off
    --
    -- Offset should refer to the same memory region. For types that also implement
    -- `MemAlloc` this can be described as:
    --
    -- > count <- getByteCountMem memRead
    -- > unOff (toByteOff off) <= unCount (count - byteCountType @e)
    --
    -> m e

  -- | Write an element with an offset in number of elements, rather than bytes as it is
  -- the case with `writeByteOffMem`.
  --
  -- [Unsafe] Bounds are not checked. When precondition for @off@ argument is violated the
  -- result is either memory corruption or failure with a segfault.
  --
  -- @since 0.1.0
  writeOffMem :: (MonadPrim s m, Prim e)
    => mw s -- ^ /memWrite/ - Memory region to write an element into
    -> Off e
    -- ^ /off/ - Offset in number of elements from the beginning of @memWrite@
    --
    -- /__Preconditions:__/
    --
    -- > 0 <= off
    --
    -- Offset should refer to the same memory region. For types that also implement
    -- `MemAlloc` this can be described as:
    --
    -- > count <- getByteCountMem memWrite
    -- > unOff (toByteOff off) <= unCount (count - byteCountType @e)
    --
    -> e -- ^ /elt/ - Element to write
    -> m ()
  writeOffMem mw off = writeByteOffMem mw (toByteOff off)
  {-# INLINE writeOffMem #-}

  -- | Write an element with an offset in number of bytes.
  --
  -- [Unsafe] Bounds are not checked. When precondition for @off@ argument is violated the
  -- result is either memory corruption or failure with a segfault.
  --
  -- @since 0.1.0
  writeByteOffMem :: (MonadPrim s m, Prim e)
    => mw s -- ^ /memWrite/ - Memory region to write an element into
    -> Off Word8
    -- ^ /off/ - Offset in number of elements from the beginning of @memWrite@
    --
    -- /__Preconditions:__/
    --
    -- > 0 <= off
    --
    -- Offset should refer to the same memory region. For types that also implement
    -- `MemAlloc` this can be described as:
    --
    -- > count <- getByteCountMem memWrite
    -- > unOff (toByteOff off) <= unCount (count - byteCountType @e)
    --
    -> e -> m ()

  -- | Source and target can be overlapping memory chunks
  moveByteOffToMBytesMem ::
    (MonadPrim s m, Prim e) => mw s -> Off Word8 -> MBytes p s -> Off Word8 -> Count e -> m ()

  -- | Source and target can be overlapping memory chunks
  moveByteOffToPtrMem ::
    (MonadPrim s m, Prim e) => mw s -> Off Word8 -> Ptr e -> Off Word8 -> Count e -> m ()

  copyByteOffMem ::
    (MonadPrim s m, MemRead mr, Prim e) => mr -> Off Word8 -> mw s -> Off Word8 -> Count e -> m ()

  moveByteOffMem ::
    (MonadPrim s m, MemWrite mw', Prim e) => mw' s -> Off Word8 -> mw s -> Off Word8 -> Count e -> m ()

  -- TODO: Potential feature for the future implementation. Will require extra function in `Prim`.
  --setByteOffMem :: (MonadPrim s m, Prim e) => w s -> Off Word8 -> Count e -> e -> m ()

  -- | Write the same value into each cell starting at an offset.
  setMem
    :: (MonadPrim s m, Prim e)
    => mw s -- ^ Writable memory. Must have enough bytes, at least: (off+count)*(sizeOf e)
    -> Off e -- ^ An offset into writable memory at which element setting should start.
    -> Count e -- ^ Numer of cells to write the elemnt into
    -> e -- ^ Element to write into all memory cells specified by offset and count. Even
         -- if the count is @0@ this element might be still fully evaluated.
    -> m ()

-- | Generalized memory allocation and pure/mutable state conversion.
class (MemRead (FrozenMem ma), MemWrite ma) => MemAlloc ma where
  type FrozenMem ma = (fm :: Type) | fm -> ma

  getByteCountMem :: MonadPrim s m => ma s -> m (Count Word8)

  allocByteCountMem :: MonadPrim s m => Count Word8 -> m (ma s)

  thawMem :: MonadPrim s m => FrozenMem ma -> m (ma s)

  freezeMem :: MonadPrim s m => ma s -> m (FrozenMem ma)

  resizeMem :: (MonadPrim s m, Prim e) => ma s -> Count e -> m (ma s)
  resizeMem = defaultResizeMem


instance MemRead ByteString where
  byteCountMem (PS _ _ c) = Count c
  {-# INLINE byteCountMem #-}
  indexOffMem bs i = unsafeInlineIO $ withPtrAccess bs (`readOffPtr` i)
  {-# INLINE indexOffMem #-}
  indexByteOffMem bs i = unsafeInlineIO $ withPtrAccess bs (`readByteOffPtr` i)
  {-# INLINE indexByteOffMem #-}
  copyByteOffToMBytesMem bs srcOff mb dstOff c =
    withPtrAccess bs $ \srcPtr -> copyByteOffPtrToMBytes srcPtr srcOff mb dstOff c
  {-# INLINE copyByteOffToMBytesMem #-}
  copyByteOffToPtrMem bs srcOff dstPtr dstOff c =
    withPtrAccess bs $ \srcPtr -> copyByteOffPtrToPtr srcPtr srcOff dstPtr dstOff c
  {-# INLINE copyByteOffToPtrMem #-}
  compareByteOffToPtrMem bs off1 ptr2 off2 c =
    withPtrAccess bs $ \ptr1 -> pure $ compareByteOffPtrToPtr ptr1 off1 ptr2 off2 c
  {-# INLINE compareByteOffToPtrMem #-}
  compareByteOffToBytesMem bs off1 bytes off2 c =
    withPtrAccess bs $ \ptr1 -> pure $ compareByteOffPtrToBytes ptr1 off1 bytes off2 c
  {-# INLINE compareByteOffToBytesMem #-}
  compareByteOffMem mem1 off1 bs off2 c =
    unsafeInlineIO $ withPtrAccess bs $ \ptr2 -> compareByteOffToPtrMem mem1 off1 ptr2 off2 c
  {-# INLINE compareByteOffMem #-}


instance MemAlloc MByteString where
  type FrozenMem MByteString = ByteString
  getByteCountMem (MByteString (PS _ _ c)) = pure $ Count c
  {-# INLINE getByteCountMem #-}
  allocByteCountMem c = do
    fp <- mallocByteCountPlainForeignPtr c
    pure $ MByteString (PS fp 0 (coerce c))
  {-# INLINE allocByteCountMem #-}
  thawMem bs = pure $ MByteString bs
  {-# INLINE thawMem #-}
  freezeMem (MByteString bs) = pure bs
  {-# INLINE freezeMem #-}
  resizeMem bsm@(MByteString (PS fp o n)) newc
    | newn > n = defaultResizeMem bsm newc
    | otherwise = pure $ MByteString (PS fp o newn)
    where -- constant slice if we need to reduce the size
      Count newn = toByteCount newc
  {-# INLINE resizeMem #-}

instance MemWrite MByteString where
  readOffMem (MByteString mbs) i = withPtrAccess mbs (`readOffPtr` i)
  {-# INLINE readOffMem #-}
  readByteOffMem (MByteString mbs) i = withPtrAccess mbs (`readByteOffPtr` i)
  {-# INLINE readByteOffMem #-}
  writeOffMem (MByteString mbs) i a = withPtrAccess mbs $ \ptr -> writeOffPtr ptr i a
  {-# INLINE writeOffMem #-}
  writeByteOffMem (MByteString mbs) i a = withPtrAccess mbs $ \ptr -> writeByteOffPtr ptr i a
  {-# INLINE writeByteOffMem #-}
  moveByteOffToPtrMem (MByteString fsrc) srcOff dstPtr dstOff c =
    withPtrAccess fsrc $ \srcPtr -> moveByteOffPtrToPtr srcPtr srcOff dstPtr dstOff c
  {-# INLINE moveByteOffToPtrMem #-}
  moveByteOffToMBytesMem (MByteString fsrc) srcOff dst dstOff c =
    withPtrAccess fsrc $ \srcPtr -> moveByteOffPtrToMBytes srcPtr srcOff dst dstOff c
  {-# INLINE moveByteOffToMBytesMem #-}
  copyByteOffMem src srcOff (MByteString fdst) dstOff c =
    withPtrAccess fdst $ \dstPtr -> copyByteOffToPtrMem src srcOff dstPtr dstOff c
  {-# INLINE copyByteOffMem #-}
  moveByteOffMem src srcOff (MByteString fdst) dstOff c =
    withPtrAccess fdst $ \dstPtr -> moveByteOffToPtrMem src srcOff dstPtr dstOff c
  {-# INLINE moveByteOffMem #-}
  setMem (MByteString mbs) off c a = withPtrAccess mbs $ \ptr -> setOffPtr ptr off c a
  {-# INLINE setMem #-}


instance MemRead ShortByteString where
  byteCountMem = byteCountMem . fromShortByteStringBytes
  {-# INLINE byteCountMem #-}
  indexOffMem sbs = indexOffMem (fromShortByteStringBytes sbs)
  {-# INLINE indexOffMem #-}
  indexByteOffMem sbs = indexByteOffMem (fromShortByteStringBytes sbs)
  {-# INLINE indexByteOffMem #-}
  copyByteOffToMBytesMem sbs = copyByteOffToMBytesMem (fromShortByteStringBytes sbs)
  {-# INLINE copyByteOffToMBytesMem #-}
  copyByteOffToPtrMem sbs = copyByteOffToPtrMem (fromShortByteStringBytes sbs)
  {-# INLINE copyByteOffToPtrMem #-}
  compareByteOffToPtrMem sbs = compareByteOffToPtrMem (fromShortByteStringBytes sbs)
  {-# INLINE compareByteOffToPtrMem #-}
  compareByteOffToBytesMem sbs = compareByteOffToBytesMem (fromShortByteStringBytes sbs)
  {-# INLINE compareByteOffToBytesMem #-}
  compareByteOffMem mem off1 sbs = compareByteOffMem mem off1 (fromShortByteStringBytes sbs)
  {-# INLINE compareByteOffMem #-}

-- | A wrapper that adds a phantom state token. It can be used with types that either
-- doesn't have such state token or are designed to work in `IO` and therefore restricted
-- to `RW`. Using this wrapper is very much unsafe, so make sure you know what you are
-- doing.
newtype MemState a s = MemState { unMemState :: a }

instance MemWrite (MemState (ForeignPtr a)) where
  readOffMem (MemState fptr) i = withForeignPtr fptr $ \ptr -> readOffPtr (castPtr ptr) i
  {-# INLINE readOffMem #-}
  readByteOffMem (MemState fptr) i =
    withForeignPtr fptr $ \ptr -> readByteOffPtr (castPtr ptr) i
  {-# INLINE readByteOffMem #-}
  writeOffMem (MemState fptr) i a = withForeignPtr fptr $ \ptr -> writeOffPtr (castPtr ptr) i a
  {-# INLINE writeOffMem #-}
  writeByteOffMem (MemState fptr) i a =
    withForeignPtr fptr $ \ptr -> writeByteOffPtr (castPtr ptr) i a
  {-# INLINE writeByteOffMem #-}
  moveByteOffToPtrMem (MemState fsrc) srcOff dstPtr dstOff c =
    withForeignPtr fsrc $ \srcPtr -> moveByteOffPtrToPtr (castPtr srcPtr) srcOff dstPtr dstOff c
  {-# INLINE moveByteOffToPtrMem #-}
  moveByteOffToMBytesMem (MemState fsrc) srcOff dst dstOff c =
    withForeignPtr fsrc $ \srcPtr -> moveByteOffPtrToMBytes (castPtr srcPtr) srcOff dst dstOff c
  {-# INLINE moveByteOffToMBytesMem #-}
  copyByteOffMem src srcOff (MemState fdst) dstOff c =
    withForeignPtr fdst $ \dstPtr ->
       copyByteOffToPtrMem src srcOff (castPtr dstPtr) dstOff c
  {-# INLINE copyByteOffMem #-}
  moveByteOffMem src srcOff (MemState fdst) dstOff c =
    withForeignPtr fdst $ \dstPtr ->
       moveByteOffToPtrMem src srcOff (castPtr dstPtr) dstOff c
  {-# INLINE moveByteOffMem #-}
  setMem (MemState fptr) off c a = withForeignPtr fptr $ \ptr -> setOffPtr (castPtr ptr) off c a
  {-# INLINE setMem #-}

modifyFetchOldMem ::
     (MemWrite mw, MonadPrim s m, Prim b) => mw s -> Off b -> (b -> b) -> m b
modifyFetchOldMem mem o f = modifyFetchOldMemM mem o (pure . f)
{-# INLINE modifyFetchOldMem #-}


modifyFetchNewMem ::
     (MemWrite mw, MonadPrim s m, Prim b) => mw s -> Off b -> (b -> b) -> m b
modifyFetchNewMem mem o f = modifyFetchNewMemM mem o (pure . f)
{-# INLINE modifyFetchNewMem #-}


modifyFetchOldMemM ::
     (MemWrite mw, MonadPrim s m, Prim b) => mw s -> Off b -> (b -> m b) -> m b
modifyFetchOldMemM mem o f = do
  a <- readOffMem mem o
  a <$ (writeOffMem mem o =<< f a)
{-# INLINE modifyFetchOldMemM #-}


modifyFetchNewMemM ::
     (MemWrite mw, MonadPrim s m, Prim b) => mw s -> Off b -> (b -> m b) -> m b
modifyFetchNewMemM mem o f = do
  a <- readOffMem mem o
  a' <- f a
  a' <$ writeOffMem mem o a'
{-# INLINE modifyFetchNewMemM #-}


defaultResizeMem ::
     (Prim e, MemAlloc ma, MonadPrim s m) => ma s -> Count e -> m (ma s)
defaultResizeMem mem c = do
  let newByteCount = toByteCount c
  oldByteCount <- getByteCountMem mem
  if oldByteCount == newByteCount
    then pure mem
    else do
      newMem <- allocByteCountMem newByteCount
      newMem <$ moveMem mem 0 newMem 0 oldByteCount


-- | Make @n@ copies of supplied region of memory into a contiguous chunk of memory.
cycleMemN :: (MemAlloc ma, MemRead mr) => Int -> mr -> FrozenMem ma
cycleMemN n r
  | n <= 0 = emptyMem
  | otherwise =
    runST $ do
      let bc@(Count chunk) = byteCountMem r
          c@(Count c8) = Count n * bc
      mem <- allocByteCountMem c
      let go i = when (i < c8) $ copyByteOffMem r 0 mem (Off i) bc >> go (i + chunk)
      go 0
      freezeMem mem
{-# INLINE cycleMemN #-}


-- | Chunk of empty memory.
emptyMem :: MemAlloc ma => FrozenMem ma
emptyMem = createMemST_ (0 :: Count Word8) (\_ -> pure ())
{-# INLINE emptyMem #-}

-- | A region of memory that hold a single element.
singletonMem ::
     forall e ma. (MemAlloc ma, Prim e)
  => e
  -> FrozenMem ma
singletonMem a = createMemST_ (1 :: Count e) $ \mem -> writeOffMem mem 0 a
{-# INLINE singletonMem #-}

-- | Allocate enough memory for number of elements. Memory is not initialized and may
-- contain garbage. Use `allocZeroMem` if clean memory is needed.
--
-- [Unsafe Count] Negative element count will result in unpredictable behavior
--
-- @since 0.1.0
allocMem :: (MemAlloc ma, MonadPrim s m, Prim e) => Count e -> m (ma s)
allocMem n = allocByteCountMem (toByteCount n)
{-# INLINE allocMem #-}


-- | Same as `allocMem`, but also use @memset@ to initialize all the new memory to zeros.
--
-- [Unsafe Count] Negative element count will result in unpredictable behavior
--
-- @since 0.1.0
allocZeroMem ::
     (MemAlloc ma, MonadPrim s m, Prim e) => Count e -> m (ma s)
allocZeroMem n = do
  m <- allocMem n
  m <$ setMem m 0 (toByteCount n) (0 :: Word8)
{-# INLINE allocZeroMem #-}


createMemST ::
     forall e b ma. (MemAlloc ma, Prim e)
  => Count e
  -> (forall s. ma s -> ST s b)
  -> (b, FrozenMem ma)
createMemST n f = runST $ do
  m <- allocMem n
  res <- f m
  i <- freezeMem m
  pure (res, i)
{-# INLINE createMemST #-}

createMemST_ :: (MemAlloc ma, Prim e) => Count e -> (forall s . ma s -> ST s b) -> FrozenMem ma
createMemST_ n f = runST (allocMem n >>= \m -> f m >> freezeMem m)
{-# INLINE createMemST_ #-}

createZeroMemST :: (MemAlloc ma, Prim e) => Count e -> (forall s . ma s -> ST s b) -> (b, FrozenMem ma)
createZeroMemST n f = runST $ do
  m <- allocZeroMem n
  res <- f m
  i <- freezeMem m
  pure (res, i)
{-# INLINE createZeroMemST #-}

createZeroMemST_ :: (MemAlloc ma, Prim e) => Count e -> (forall s . ma s -> ST s b) -> FrozenMem ma
createZeroMemST_ n f = runST (allocZeroMem n >>= \m -> f m >> freezeMem m)
{-# INLINE createZeroMemST_ #-}


copyMem ::
     (MonadPrim s m, MemRead mr, MemWrite mw, Prim e)
  => mr -- ^ Source memory region
  -> Off e -- ^ Offset into the source in number of elements
  -> mw s -- ^ Destination memory region
  -> Off e -- ^ Offset into destination in number of elements
  -> Count e -- ^ Number of elements to copy over
  -> m ()
copyMem src srcOff dst dstOff = copyByteOffMem src (toByteOff srcOff) dst (toByteOff dstOff)
{-# INLINE copyMem #-}


moveMem ::
     (MonadPrim s m, MemWrite mw1, MemWrite mw2, Prim e)
  => mw1 s -- ^ Source memory region
  -> Off e -- ^ Offset into the source in number of elements
  -> mw2 s -- ^ Destination memory region
  -> Off e -- ^ Offset into destination in number of elements
  -> Count e -- ^ Number of elements to copy over
  -> m ()
moveMem src srcOff dst dstOff = moveByteOffMem src (toByteOff srcOff) dst (toByteOff dstOff)
{-# INLINE moveMem #-}


appendMem ::
     forall mr1 mr2 ma. (MemRead mr1, MemRead mr2, MemAlloc ma)
  => mr1
  -> mr2
  -> FrozenMem ma
appendMem r1 r2 =
  createMemST_ (n1 + n2) $ \mem -> do
    copyMem r1 0 mem 0 n1
    copyMem r2 (coerce n1) mem (coerce n1) n2
  where
    n1 = byteCountMem r1
    n2 = byteCountMem r2
{-# INLINABLE appendMem #-}

concatMem ::
     forall mr ma. (MemRead mr, MemAlloc ma)
  => [mr]
  -> FrozenMem ma
concatMem xs = do
  let c = Foldable.foldl' (\ !acc b -> acc + byteCountMem b) 0 xs
  createMemST_ c $ \mb -> do
    let load i b = do
          let cb@(Count n) = byteCountMem b :: Count Word8
          (i + Off n) <$ copyMem b 0 mb i cb
    foldM_ load 0 xs
{-# INLINABLE concatMem #-}


thawCopyMem ::
     forall e mr ma m s. (Prim e, MemRead mr, MemAlloc ma, MonadPrim s m)
  => mr
  -> Off e
  -> Count e
  -> m (ma s)
thawCopyMem a off c = do
  mem <- allocMem c
  mem <$ copyMem a off mem 0 c
{-# INLINE thawCopyMem #-}

freezeCopyMem ::
     forall e ma m s. (Prim e, MemAlloc ma, MonadPrim s m)
  => ma s
  -> Off e
  -> Count e
  -> m (FrozenMem ma)
freezeCopyMem mem off c = freezeMem mem >>= \r -> thawCopyMem r off c >>= freezeMem
{-# INLINE freezeCopyMem #-}


thawCloneMem ::
     forall mr ma m s. (MemRead mr, MemAlloc ma, MonadPrim s m)
  => mr
  -> m (ma s)
thawCloneMem a = thawCopyMem a 0 (byteCountMem a)
{-# INLINE thawCloneMem #-}

freezeCloneMem ::
     forall ma m s. (MemAlloc ma, MonadPrim s m)
  => ma s
  -> m (FrozenMem ma)
freezeCloneMem = freezeMem >=> thawCloneMem >=> freezeMem
{-# INLINE freezeCloneMem #-}

-- | /O(n)/ - Convert a read-only memory region into a newly allocated other type of
-- memory region
--
-- >>> import Data.ByteString
-- >>> bs = pack [0x10 .. 0x20]
-- >>> bs
-- "\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\EM\SUB\ESC\FS\GS\RS\US "
-- >>> convertMem bs :: Bytes 'Inc
-- [0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18,0x19,0x1a,0x1b,0x1c,0x1d,0x1e,0x1f,0x20]
--
-- @since 0.1.0
convertMem :: (MemRead mr, MemAlloc ma) => mr -> FrozenMem ma
convertMem a = runST $ thawCloneMem a >>= freezeMem
{-# INLINE convertMem #-}

-- | Figure out how many elements can fit into the region of memory. It is possible that
-- there is a remainder of bytes left, see `countRemMem` for getting that too.
--
-- ====__Examples__
--
-- >>> b = fromListMem [0 .. 5 :: Word8] :: Bytes 'Pin
-- >>> b
-- [0x00,0x01,0x02,0x03,0x04,0x05]
-- >>> countMem b :: Count Word16
-- Count {unCount = 3}
-- >>> countMem b :: Count Word32
-- Count {unCount = 1}
--
-- @since 0.1.0
countMem ::
     forall e mr. (MemRead mr, Prim e)
  => mr -- ^ Read-only memory type
  -> Count e
countMem = fromByteCount . byteCountMem
{-# INLINE countMem #-}

-- | Compute how many elements and a byte size remainder that can fit into the region of memory.
--
-- ====__Examples__
--
-- >>> b = fromListMem [0 .. 5 :: Word8] :: Bytes 'Pin
-- >>> b
-- [0x00,0x01,0x02,0x03,0x04,0x05]
-- >>> countRemMem @Word16 b
-- (Count {unCount = 3},0)
-- >>> countRemMem @Word32 b
-- (Count {unCount = 1},2)
--
-- @since 0.1.0
countRemMem :: forall e mr. (MemRead mr, Prim e) => mr -> (Count e, Count Word8)
countRemMem = fromByteCountRem . byteCountMem
{-# INLINE countRemMem #-}

getCountMem :: forall e ma m s. (MemAlloc ma, MonadPrim s m, Prim e) => ma s -> m (Count e)
getCountMem = fmap (fromByteCount . coerce) . getByteCountMem
{-# INLINE getCountMem #-}


getCountRemMem ::
     forall e ma m s. (MemAlloc ma, MonadPrim s m, Prim e)
  => ma s
  -> m (Count e, Count Word8)
getCountRemMem = fmap (fromByteCountRem . coerce) . getByteCountMem
{-# INLINE getCountRemMem #-}


clone ::
     forall ma m s. (MemAlloc ma, MonadPrim s m)
  => ma s
  -> m (ma s)
clone mb = do
  n <- getByteCountMem mb
  mb' <- allocMem n
  mb' <$ moveMem mb 0 mb' 0 n
{-# INLINE clone #-}

-- | Compare two memory regions byte-by-byte. False is returned immendiately if sizes
-- reported by `byteCountMem` do not match. Computation may be short-circuited on the
-- first mismatch, but it is `MemRead` implementation specific.
--
-- @since 0.1.0
eqMem :: (MemRead mr1, MemRead mr2) => mr1 -> mr2 -> Bool
eqMem b1 b2 = n == byteCountMem b2 && compareByteOffMem b1 0 b2 0 n == EQ
  where
    n = byteCountMem b1
{-# INLINE eqMem #-}

-- | Compare two regions of memory byte-by-byte. It will return `EQ` whenever both regions
-- are exactly the same and `LT` or `GT` as soon as the first byte is reached that is less
-- than or greater than respectfully in the first region when compared to the second
-- one. It is safe for both regions to refer to the same part of memory, since this is a
-- pure function and both regions of memory are read-only.
compareMem ::
     forall e mr1 mr2. (MemRead mr1, MemRead mr2, Prim e)
  => mr1 -- ^ First region of memory
  -> Off e -- ^ Offset in number of elements into the first region
  -> mr2 -- ^ Second region of memory
  -> Off e -- ^ Offset in number of elements into the second region
  -> Count e -- ^ Number of elements to compare
  -> Ordering
compareMem r1 off1 r2 off2 = compareByteOffMem r1 (toByteOff off1) r2 (toByteOff off2)
{-# INLINE compareMem #-}

-- | It is only guaranteed to convert the whole memory to a list whenever the size of
-- allocated memory is exactly divisible by the size of the element, otherwise there will
-- be some slack left unaccounted for.
toListMem :: forall e mr. (MemRead mr, Prim e) => mr -> [e]
toListMem ba = build (\ c n -> foldrCountMem (countMem ba) c n ba)
{-# INLINE toListMem #-}
{-# SPECIALIZE toListMem :: Prim e => Bytes p -> [e] #-}

-- | Same as `toListMem`, except if there is some slack at the end of the memory that
-- didn't fit in a list it will be returned as a list of bytes
--
-- ====__Examples__
--
-- >>> import Data.Word
-- >>> :set -XDataKinds
-- >>> a = fromListMem [0 .. 10 :: Word8] :: Bytes 'Pin
-- >>> a
-- [0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0a]
-- >>> toListSlackMem a :: ([Word8], [Word8])
-- ([0,1,2,3,4,5,6,7,8,9,10],[])
-- >>> toListSlackMem a :: ([Word16], [Word8])
-- ([256,770,1284,1798,2312],[10])
-- >>> toListSlackMem a :: ([Word32], [Word8])
-- ([50462976,117835012],[8,9,10])
-- >>> toListSlackMem a :: ([Word64], [Word8])
-- ([506097522914230528],[8,9,10])
--
-- @since 0.1.0
toListSlackMem ::
     forall e mr. (MemRead mr, Prim e)
  => mr
  -> ([e], [Word8])
toListSlackMem mem =
  (build (\c n -> foldrCountMem k c n mem), getSlack (k8 + r8) [])
  where
    (k, Count r8) = countRemMem mem
    Count k8 = toByteCount k
    getSlack i !acc
      | i == k8 = acc
      | otherwise =
        let i' = i - 1
         in getSlack i' (indexByteOffMem mem (Off i') : acc)
{-# INLINABLE toListSlackMem #-}

-- | Right fold that is useful for converting to list while tapping into list fusion.
foldrCountMem :: forall e b mr. (MemRead mr, Prim e) => Count e -> (e -> b -> b) -> b -> mr -> b
foldrCountMem (Count k) c nil bs = go 0
  where
    go i
      | i == k = nil
      | otherwise =
        let !v = indexOffMem bs (Off i)
         in v `c` go (i + 1)
{-# INLINE[0] foldrCountMem #-}


loadListMemN ::
     (MemWrite mw, MonadPrim s m, Prim e)
  => Count e
  -> Count Word8
  -> [e]
  -> mw s
  -> m Ordering
loadListMemN (Count n) (Count slack) ys mb = do
  let go [] !i = pure (compare i n <> compare 0 slack)
      go (x:xs) !i
        | i < n = writeOffMem mb (Off i) x >> go xs (i + 1)
        | otherwise = pure GT
  go ys 0
{-# INLINABLE loadListMemN #-}

loadListMemN_ ::
     forall e mw m s. (Prim e, MemWrite mw, MonadPrim s m)
  => Count e
  -> [e]
  -> mw s
  -> m ()
loadListMemN_ (Count n) ys mb =
  let go [] _     = pure ()
      go (x:xs) i = when (i < n) $ writeOffMem mb (Off i) x >> go xs (i + 1)
   in go ys 0
{-# INLINABLE loadListMemN_ #-}

-- | Returns `EQ` if the full list did fit into the supplied memory chunk exactly.
-- Otherwise it will return either `LT` if the list was smaller than allocated memory or
-- `GT` if the list was bigger than the available memory and did not fit into `MBytes`.
loadListMem ::
     forall e ma m s. (Prim e, MemAlloc ma, MonadPrim s m)
  => [e]
  -> ma s
  -> m Ordering
loadListMem ys mb = do
  (c, slack) <- getCountRemMem mb
  loadListMemN (c `countForProxyTypeOf` ys) slack ys mb
{-# INLINE loadListMem #-}

loadListMem_ ::
     forall e ma m s. (Prim e, MemAlloc ma, MonadPrim s m)
  => [e]
  -> ma s
  -> m ()
loadListMem_ ys mb = do
  c <- getCountMem mb
  loadListMemN_ (c `countForProxyTypeOf` ys) ys mb
{-# INLINE loadListMem_ #-}


fromListMemN ::
     forall e ma. (Prim e, MemAlloc ma)
  => Count e
  -> [e]
  -> (Ordering, FrozenMem ma)
fromListMemN n xs = createMemST n (loadListMemN n 0 xs)
{-# INLINE fromListMemN #-}

fromListMemN_ ::
     forall e ma. (Prim e, MemAlloc ma)
  => Count e
  -> [e]
  -> FrozenMem ma
fromListMemN_ !n xs = createMemST_ n (loadListMemN_ n xs)
{-# INLINE fromListMemN_ #-}

fromListMem ::
     forall e ma. (Prim e, MemAlloc ma)
  => [e]
  -> FrozenMem ma
fromListMem xs = fromListMemN_ (coerce (length xs) `countForProxyTypeOf` xs) xs
{-# INLINE fromListMem #-}


-- | Load a list of bytes into a newly allocated memory region. Equivalent to
-- `Data.ByteString.pack` for `Data.ByteString.ByteString`
--
-- ====__Examples__
--
-- >>> fromByteListMem [0..10] :: Bytes 'Pin
-- [0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0a]
--
-- @since 0.1.0
fromByteListMem ::
     forall ma. MemAlloc ma
  => [Word8]
  -> FrozenMem ma
fromByteListMem = fromListMem
{-# INLINE fromByteListMem #-}

-- | Convert a memory region to a list of bytes. Equivalent to `Data.ByteString.unpack`
-- for `Data.ByteString.ByteString`
--
-- >>> toByteListMem (fromByteListMem [0..10] :: Bytes 'Pin)
-- [0,1,2,3,4,5,6,7,8,9,10]
--
-- @since 0.1.0
toByteListMem ::
     forall ma. MemAlloc ma
  => FrozenMem ma
  -> [Word8]
toByteListMem = toListMem
{-# INLINE toByteListMem #-}


mapByteMem ::
     forall e mr ma. (MemRead mr, MemAlloc ma, Prim e)
  => (Word8 -> e)
  -> mr
  -> FrozenMem ma
mapByteMem f = mapByteOffMem (const f)

-- | Map an index aware function over memory region
--
-- >>> a = fromListMem [1 .. 10 :: Word8] :: Bytes 'Inc
-- >>> a
-- [0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0a]
-- >>> imapMem (\i e -> (fromIntegral i :: Int8, e + 0xf0)) a :: Bytes 'Pin
-- [0x00,0xf1,0x01,0xf2,0x02,0xf3,0x03,0xf4,0x04,0xf5,0x05,0xf6,0x06,0xf7,0x07,0xf8,0x08,0xf9,0x09,0xfa]
--
-- @since 0.1.0
mapByteOffMem ::
     (MemRead mr, MemAlloc ma, Prim e) => (Off Word8 -> Word8 -> e) -> mr -> FrozenMem ma
mapByteOffMem f r = runST $ mapByteOffMemM (\i -> pure . f i) r

-- @since 0.1.0
mapByteMemM ::
     (MemRead mr, MemAlloc ma, MonadPrim s m, Prim e)
  => (Word8 -> m e)
  -> mr
  -> m (FrozenMem ma)
mapByteMemM f = mapByteOffMemM (const f)


-- @since 0.1.0
mapByteOffMemM ::
     forall e mr ma m s. (MemRead mr, MemAlloc ma, MonadPrim s m, Prim e)
  => (Off Word8 -> Word8 -> m e)
  -> mr
  -> m (FrozenMem ma)
mapByteOffMemM f r = do
  let bc@(Count n) = byteCountMem r
      c = Count n `countForProxyTypeOf` f 0 0
  mem <- allocMem c
  _ <- forByteOffMemM_ r 0 bc f
  -- let go i =
  --       when (i < n) $ do
  --         f i (indexByteOffMem r (Off i)) >>=
  --           writeOffMem mem (offAsProxy c (Off i))
  --         go (i + 1)
  -- go 0
  freezeMem mem


-- | Iterate over a region of memory
forByteOffMemM_ ::
     (MemRead mr, MonadPrim s m, Prim e)
  => mr
  -> Off Word8
  -> Count e
  -> (Off Word8 -> e -> m b)
  -> m (Off Word8)
forByteOffMemM_ r (Off byteOff) c f =
  let n = coerce (toByteCount c) + byteOff
      Count k = byteCountProxy c
      go i
        | i < n = f (Off i) (indexByteOffMem r (Off i)) >> go (i + k)
        | otherwise = pure $ Off i
   in go byteOff

loopShortM :: Monad m => Int -> (Int -> a -> Bool) -> (Int -> Int) -> a -> (Int -> a -> m a) -> m a
loopShortM !startAt condition increment !initAcc f = go startAt initAcc
  where
    go !step !acc
      | condition step acc = f step acc >>= go (increment step)
      | otherwise = pure acc
{-# INLINE loopShortM #-}

loopShortM' :: Monad m => Int -> (Int -> a -> m Bool) -> (Int -> Int) -> a -> (Int -> a -> m a) -> m a
loopShortM' !startAt condition increment !initAcc f = go startAt initAcc
  where
    go !step !acc =
      condition step acc >>= \cont ->
        if cont
          then f step acc >>= go (increment step)
          else pure acc
{-# INLINE loopShortM' #-}

-- -- | Iterate over a region of memory
-- loopMemM_ ::
--      (MemRead mr, MonadPrim s m, Prim e)
--   => r
--   -> Off Word8
--   -> Count e
--   -> (Count Word8 -> a -> Bool)
--   -> (Off Word8 -> e -> m b)
--   -> m (Off Word8)
-- foldlByteOffMemM_ r (Off byteOff) c f =
--   loopShortM byteOff (\i -> f (coerce i))
--   let n = coerce (toByteCount c) + byteOff
--       Count k = byteCountProxy c
--       go i
--         | i < n = f (Off i) (indexByteOffMem r (Off i)) >> go (i + k)
--         | otherwise = pure $ Off i
--    in go byteOff


data MemView a = MemView
  { mvOffset :: {-# UNPACK #-} !(Off Word8)
  , mvCount  :: {-# UNPACK #-} !(Count Word8)
  , mvMem    :: !a
  }

data MMemView a s = MMemView
  { mmvOffset :: {-# UNPACK #-} !(Off Word8)
  , mmvCount  :: {-# UNPACK #-} !(Count Word8)
  , mmvMem    :: !(a s)
  }

izipWithByteOffMemM_ ::
     (MemRead mr1, MemRead mr2, MonadPrim s m, Prim e)
  => mr1
  -> Off Word8
  -> mr2
  -> Off Word8
  -> Count e
  -> (Off Word8 -> e -> Off Word8 -> e -> m b)
  -> m (Off Word8)
izipWithByteOffMemM_ r1 (Off byteOff1) r2 off2 c f =
  let n = coerce (toByteCount c) + byteOff1
      Count k = byteCountProxy c
      go i
        | i < n =
          let o1 = Off i
              o2 = Off i + off2
           in f o1 (indexByteOffMem r1 o1) o2 (indexByteOffMem r2 o2) >>
              go (i + k)
        | otherwise = pure $ Off i
   in go byteOff1


izipWithOffMemM_ ::
     (MemRead mr1, MemRead mr2, MonadPrim s m, Prim e1, Prim e2)
  => mr1
  -> Off e1
  -> mr2
  -> Off e2
  -> Int
  -> (Off e1 -> e1 -> Off e2 -> e2 -> m b)
  -> m ()
izipWithOffMemM_ r1 off1 r2 off2 nc f =
  let n = nc + coerce off1
      go o1@(Off i) o2 =
        when (i < n) $
        f o1 (indexOffMem r1 o1) o2 (indexOffMem r2 o2) >> go (o1 + 1) (o2 + 1)
   in go off1 off2


-- class Mut f => MFunctor f where
--   mmap :: (Elt f a, Elt f b, MonadPrim s m) => (a -> b) -> f a s -> m (f b s)

-- class Mut f => MTraverse f where
--   mmapM :: (Elt f a, Elt f b, MonadPrim s m) => (a -> m b) -> f a s -> m (f b s)

-- class MFunctor f => MApplicative f where
--   pureMut :: (Elt f a, MonadPrim s m) => a -> m (f a s)
--   liftMut ::
--     (Elt f a, Elt f b, Elt f c, MonadPrim s m) => (a -> b -> m c) -> f a s -> f b s -> m (f c s)

-- class MApplicative f => MMonad f where
--   bindMut ::
--     (Elt f a, Elt f b, MonadPrim s m) => f a s -> (a -> m b) -> f b s -> m (f c s)

-- instance MFunctor MAddr where
--   mmap f maddr = do
--     Count n <- getCountMAddr maddr
--     maddr' <- allocMAddr (Count n)
--     let go i =
--           when (i < n) $ do
--             writeOffMAddr maddr' (Off i) . f =<< readOffMAddr maddr (Off i)
--             go (i + 1)
--     maddr' <$ go 0

-- instance MTraverse MAddr where
--   mmapM f maddr = do
--     Count n <- getCountMAddr maddr
--     maddr' <- allocMAddr (Count n)
--     let go i =
--           when (i < n) $ do
--             writeOffMAddr maddr' (Off i) =<< f =<< readOffMAddr maddr (Off i)
--             go (i + 1)
--     maddr' <$ go 0


---------------------
-- Bytes instances --
---------------------

instance MemRead (Bytes p) where
  byteCountMem = byteCountBytes
  {-# INLINE byteCountMem #-}
  indexOffMem = indexOffBytes
  {-# INLINE indexOffMem #-}
  indexByteOffMem = indexByteOffBytes
  {-# INLINE indexByteOffMem #-}
  copyByteOffToMBytesMem = copyByteOffBytesToMBytes
  {-# INLINE copyByteOffToMBytesMem #-}
  copyByteOffToPtrMem = copyByteOffBytesToPtr
  {-# INLINE copyByteOffToPtrMem #-}
  compareByteOffToPtrMem bytes1 off1 ptr2 off2 c =
    pure $ compareByteOffBytesToPtr bytes1 off1 ptr2 off2 c
  {-# INLINE compareByteOffToPtrMem #-}
  compareByteOffToBytesMem bytes1 off1 bytes2 off2 c =
    pure $ compareByteOffBytes bytes1 off1 bytes2 off2 c
  {-# INLINE compareByteOffToBytesMem #-}
  compareByteOffMem mem1 off1 bs off2 c =
    unsafeInlineIO $ compareByteOffToBytesMem mem1 off1 bs off2 c
  {-# INLINE compareByteOffMem #-}

instance Typeable p => MemAlloc (MBytes p) where
  type FrozenMem (MBytes p) = Bytes p
  getByteCountMem = getByteCountMBytes
  {-# INLINE getByteCountMem #-}
  allocByteCountMem = allocMBytes
  {-# INLINE allocByteCountMem #-}
  thawMem = thawBytes
  {-# INLINE thawMem #-}
  freezeMem = freezeMBytes
  {-# INLINE freezeMem #-}
  resizeMem = reallocMBytes
  {-# INLINE resizeMem #-}

instance MemWrite (MBytes p) where
  readOffMem = readOffMBytes
  {-# INLINE readOffMem #-}
  readByteOffMem = readByteOffMBytes
  {-# INLINE readByteOffMem #-}
  writeOffMem = writeOffMBytes
  {-# INLINE writeOffMem #-}
  writeByteOffMem = writeByteOffMBytes
  {-# INLINE writeByteOffMem #-}
  moveByteOffToPtrMem = moveByteOffMBytesToPtr
  {-# INLINE moveByteOffToPtrMem #-}
  moveByteOffToMBytesMem = moveByteOffMBytesToMBytes
  {-# INLINE moveByteOffToMBytesMem #-}
  moveByteOffMem = moveByteOffToMBytesMem
  {-# INLINE moveByteOffMem #-}
  copyByteOffMem = copyByteOffToMBytesMem
  {-# INLINE copyByteOffMem #-}
  setMem = setMBytes
  {-# INLINE setMem #-}


instance Show (Bytes p) where
  show b =
    Foldable.foldr' ($) "]" $
    ('[' :) : List.intersperse (',' :) (map (("0x" ++) .) (showsHexMem b))

instance Typeable p => IsList (Bytes p) where
  type Item (Bytes p) = Word8
  fromList = fromListMem
  fromListN n = fromListMemN_ (Count n)
  toList = toListMem

instance Eq (Bytes p) where
  b1 == b2 = isSameBytes b1 b2 || eqMem b1 b2

instance Ord (Bytes p) where
  compare b1 b2 =
    compare n (byteCountBytes b2) <> compareByteOffBytes b1 0 b2 0 n
    where
      n = byteCountBytes b1

instance Typeable p => Semigroup.Semigroup (Bytes p) where
  (<>) = appendMem
  sconcat (x :| xs) = concatMem (x:xs)
  stimes i = cycleMemN (fromIntegral i)

instance Typeable p => Monoid.Monoid (Bytes p) where
  mappend = appendMem
  mconcat = concatMem
  mempty = emptyMem


-- | A list of `ShowS` that covert bytes to base16 encoded strings. Each element of the list
-- is a function that will convert one byte.
--
-- >>> mb <- newPinnedMBytes (Count 5 :: Count Int)
-- >>> mapM_ (\i -> writeOffMBytes mb (pred i) i) [1 .. 5]
-- >>> foldr ($) "" . showsBytesHex <$> freezeMBytes mb
-- "01000000000000000200000000000000030000000000000004000000000000000500000000000000"
--
showsHexMem :: MemRead mr => mr -> [ShowS]
showsHexMem b = map toHex (toListMem b :: [Word8])
  where
    toHex b8 =
      (if b8 <= 0x0f
         then ('0' :)
         else id) .
      showHex b8

-- | Ensure that memory is filled with zeros before and after it is used.
withScrubbedMem ::
     (MonadUnliftPrim RW m, Prim e, MemAlloc ma)
  => Count e
  -> (ma RW -> m a)
  -> m a
withScrubbedMem c f = do
  mem <- allocZeroMem c
  f mem `finallyPrim` setMem mem 0 (toByteCount c) 0
  where
    finallyPrim m1 m2 = withRunInPrimBase $ \run -> finally (run m1) (run m2)
{-# INLINE withScrubbedMem #-}
