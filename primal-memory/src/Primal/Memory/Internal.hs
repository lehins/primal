{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide, not-home #-}
-- |
-- Module      : Primal.Memory.Internal
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Memory.Internal
  ( module Primal.Memory.Internal
  , module Primal.Memory.Bytes.Internal
  ) where

import qualified Data.ByteString as BS
import Data.Foldable as Foldable
import Data.List as List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Monoid as Monoid
import qualified Data.Semigroup as Semigroup
import Numeric (showHex)
import Primal.Array
import Primal.Exception
import Primal.Foreign
import Primal.Memory.ByteString
import Primal.Memory.Bytes.Internal
import Primal.Memory.ForeignPtr
import Primal.Memory.Ptr
import Primal.Monad.Unsafe
import Primal.Mutable.Freeze

--TODO: implement:
-- - cloneSliceMutMem, cloneByteOffSliceMutMem
-- - thawByteOffCopyMem, freezeByteOffCopyMutMem

-- | Copy contiguous chunk of memory from the read only memory into the target mutable
-- `Ptr`. Source and target /must not/ refer to the same memory region, otherwise that
-- would imply that the source is not immutable which would be a violation of some other
-- invariant elsewhere in the code.
--
-- [Unsafe] When any precondition for one of the offsets @memSourceOff@, @memTargetOff@
-- or the element count @memCount@ is violated a call to this function can result in:
-- copy of data that doesn't belong to @memSourceRead@, heap corruption or failure with
-- a segfault.
--
--
-- @since 0.1.0
copyByteOffToPtrMem ::
     forall e mr m s. (MonadPrim s m, MemRead mr, Unbox e)
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
  -- Once the pointer is advanced by @memTargetOff@ the next @unCountBytes memCount@ bytes must
  -- still belong to the same region of memory @memTargetWrite@
  -> Off Word8
  -- ^ /memTargetOff/ - Number of bytes to advance the pointer @memTargetWrite@ forward
  --
  -- /__Precondition:__/
  --
  -- Once the pointer is advanced by @memTargetOff@ it must still refer to the same
  -- memory region @memTargetWrite@
  -> Count e
  -- ^ /memCount/ - Number of elements of type __@e@__ to copy
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memCount
  --
  -- > unCountBytes memCount + unOff memSourceOff <= unCount (byteCountMem memSourceRead - byteCountType @e)
  -> m ()
copyByteOffToPtrMem src srcOff dst dstOff c =
  liftST $ copyByteOffToPtrMemST src srcOff dst dstOff c
{-# INLINE copyByteOffToPtrMem #-}

-- | Same as `compareByteOffMem`, but compare the read-only
-- memory region to a region addressed by a `Ptr` inside of a `MonadPrim`.
--
-- [Unsafe] When any precondition for either of the offsets @memOff1@, @memOff2@, the
-- pointer @memRead2@ or the element count @memCount@ is violated the result is either
-- unpredictable output or failure with a segfault.
--
-- @since 0.1.0
compareByteOffToPtrMem ::
     forall e mr m s. (MonadPrim s m, MemRead mr, Unbox e)
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
  -- Once the pointer is advanced by @memOff2@ the next @unCountBytes memCount@ bytes must
  -- still belong to the same region of memory @memRead2@
  -> Off Word8
  -- ^ /memOff2/ - Number of bytes to advance the pointer @memRead2@ forward
  --
  -- /__Precondition:__/
  --
  -- Once the pointer is advanced by @memOff2@ it must still refer to the same memory
  -- region @memRead2@
  -> Count e -- ^ /memCount/ - Number of elements of type __@e@__ to compare as binary
  -- ^ /memCount/ - Number of elements of type __@e@__ to compare as binary
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memCount
  --
  -- > unCountBytes memCount + unOff memOff1 <= unCount (byteCountMem memRead1 - byteCountType @e)
  -> m Ordering
compareByteOffToPtrMem src srcOff dst dstOff c =
  liftST $ compareByteOffToPtrMemST src srcOff dst dstOff c
{-# INLINE compareByteOffToPtrMem #-}


-- | Copy contiguous chunk of memory from the read only memory into the target mutable
-- `MBytes`. Source and target /must not/ refer to the same memory region, otherwise
-- that would imply that the source is not immutable which would be a violation of some
-- other invariant elsewhere in the code.
--
-- [Unsafe] When a precondition for either of the offsets @memSourceOff@, @memTargetOff@
-- or the element count @memCount@ is violated the result is either unpredictable output or
-- failure with a segfault.
--
-- @since 0.1.0
copyByteOffToMBytesMem ::
     forall e mr p m s. (MonadPrim s m, MemRead mr, Unbox e)
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
  -- ^ /memCount/ - Number of elements of type __@e@__ to copy
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memCount
  --
  -- > unCountBytes memCount + unOff memSourceOff <= unCount (byteCountMem memSourceRead - byteCountType @e)
  --
  -- > unCountBytes memCount + unOff memTargetOff <= unCount (byteCountMem memTargetRead - byteCountType @e)
  -> m ()
copyByteOffToMBytesMem src srcOff dst dstOff c =
  liftST $ copyByteOffToMBytesMemST src srcOff dst dstOff c
{-# INLINE copyByteOffToMBytesMem #-}

-- | Read an element with an offset in number of elements, rather than bytes as it is
-- the case with `readByteOffMutMem`.
--
-- [Unsafe] Bounds are not checked. When precondition for @off@ argument is violated the
-- result is either unpredictable output or failure with a segfault.
--
-- @since 0.3.0
readOffMutMem ::
     forall e mw m s. (MonadPrim s m, MemWrite mw, Unbox e)
  => mw s -- ^ /memRead/ - Memory region to read an element from
  -> Off e
  -- ^ /off/ - Offset in number of elements from the beginning of @memRead@
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= off
  --
  -- With offset applied it should still refer to the same memory region. For types that
  -- also implement `MemAlloc` this can be described as:
  --
  -- > count <- getByteCountMutMem memRead
  -- > unOff (toByteOff off) <= unCount (count - byteCountType @e)
  --
  -> m e
readOffMutMem mw = liftST . readOffMutMemST mw
{-# INLINE readOffMutMem #-}

-- | Read an element with an offset in number of bytes.
--
-- [Unsafe] Bounds are not checked. When precondition for @off@ argument is violated the
-- result is either unpredictable output or failure with a segfault.
--
-- @since 0.3.0
readByteOffMutMem ::
     forall e mw m s. (MonadPrim s m, MemWrite mw, Unbox e)
  => mw s -- ^ /memRead/ - Memory region to read an element from
  -> Off Word8
  -- ^ /off/ - Offset in number of elements from the beginning of @memRead@
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= off
  --
  -- With offset applied it should still refer to the same memory region. For types that
  -- also implement `MemAlloc` this can be described as:
  --
  -- > count <- getByteCountMutMem memRead
  -- > unOff (toByteOff off) <= unCount (count - byteCountType @e)
  --
  -> m e
readByteOffMutMem mw = liftST . readByteOffMutMemST mw
{-# INLINE readByteOffMutMem #-}

-- | Write an element with an offset in number of elements, rather than bytes as it is
-- the case with `writeByteOffMutMem`.
--
-- [Unsafe] Bounds are not checked. When precondition for @off@ argument is violated the
-- outcome is either heap corruption or failure with a segfault.
--
-- @since 0.3.0
writeOffMutMem ::
     forall e mw m s. (MonadPrim s m, MemWrite mw, Unbox e)
  => mw s -- ^ /memWrite/ - Memory region to write an element into
  -> Off e
  -- ^ /off/ - Offset in number of elements from the beginning of @memWrite@
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= off
  --
  -- With offset applied it should still refer to the same memory region. For types that
  -- also implement `MemAlloc` this can be described as:
  --
  -- > count <- getByteCountMutMem memWrite
  -- > unOff (toByteOff off) <= unCount (count - byteCountType @e)
  --
  -> e -- ^ /elt/ - Element to write
  -> m ()
writeOffMutMem mw off = liftST . writeOffMutMemST mw off
{-# INLINE writeOffMutMem #-}

-- | Write an element with an offset in number of bytes.
--
-- [Unsafe] Bounds are not checked. When precondition for @off@ argument is violated the
-- outcome is either heap corruption or failure with a segfault.
--
-- @since 0.3.0
writeByteOffMutMem ::
     forall e mw m s. (MonadPrim s m, MemWrite mw, Unbox e)
  => mw s -- ^ /memWrite/ - Memory region to write an element into
  -> Off Word8
  -- ^ /off/ - Offset in number of bytes from the beginning of @memWrite@
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= off
  --
  -- With offset applied it should still refer to the same memory region. For types that
  -- also implement `MemAlloc` this can be described as:
  --
  -- > count <- getByteCountMutMem memWrite
  -- > unOff (toByteOff off) <= unCount (count - byteCountType @e)
  --
  -> e
  -- ^ /elt/ - Element to write into memory at the supplied offset
  -> m ()
writeByteOffMutMem mw off = liftST . writeByteOffMutMemST mw off
{-# INLINE writeByteOffMutMem #-}

-- | Copy contiguous chunk of memory from the source mutable memory into the target
-- mutable `MBytes`. Source and target /may/ refer to overlapping memory regions.
--
-- [Unsafe] When any precondition for one of the offsets @memSourceOff@, @memTargetOff@
-- or the element count @memCount@ is violated a call to this function can result in:
-- copy of data that doesn't belong to @memSource@, heap corruption or failure with
-- a segfault.
--
-- @since 0.3.0
moveByteOffToMBytesMutMem ::
     forall e mw p m s. (MonadPrim s m, MemWrite mw, Unbox e)
  => mw s -- ^ /memSource/ - Source memory from where to copy
  -> Off Word8
  -- ^ /memSourceOff/ - Offset in number of bytes into source memory
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memSourceOff
  --
  -- With offset applied it should still refer to the same memory region. For types that
  -- also implement `MemAlloc` this can be described as:
  --
  -- > sourceByteCount <- getByteCountMutMem memSource
  -- > unOff (toByteOff memSourceOff) <= unCount (sourceByteCount - byteCountType @e)
  -> MBytes p s -- ^ /memTarget/ - Target memory into where to copy
  -> Off Word8
  -- ^ /memTargetOff/ - Offset in number of bytes into target memory where writing will start
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memTargetOff
  --
  -- With offset applied it should still refer to the same memory region. For types that
  -- also implement `MemAlloc` this can be described as:
  --
  -- > targetByteCount <- getByteCountMutMem memTarget
  -- > unOffBytes memTargetOff <= unCount (targetByteCount - byteCountType @e)
  -> Count e
  -- ^ /memCount/ - Number of elements of type __@e@__ to copy
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memCount
  --
  -- Both source and target memory regions must have enough memory to perform a copy
  -- of @memCount@ elements starting at their respective offsets. For types that also
  -- implement `MemAlloc` this can be described as:
  --
  -- > sourceByteCount <- getByteCountMutMem memSource
  -- > unOff memSourceOff + unCountBytes memCount <= unCount (sourceByteCount - byteCountType @e)
  --
  -- > targetByteCount <- getByteCountMutMem memTarget
  -- > unOff memTargetOff + unCountBytes memCount <= unCount (targetByteCount - byteCountType @e)
  -> m ()
moveByteOffToMBytesMutMem src srcOff dst dstOff c =
  liftST $ moveByteOffToMBytesMutMemST src srcOff dst dstOff c
{-# INLINE moveByteOffToMBytesMutMem #-}


-- | Copy contiguous chunk of memory from the source mutable memory into the target
-- `Ptr`. Source and target /may/ refer to overlapping memory regions.
--
-- [Unsafe] When any precondition for one of the offsets @memSourceOff@ or
-- @memTargetOff@, a target pointer @memTarget@ or the element count @memCount@ is
-- violated a call to this function can result in: copy of data that doesn't belong to
-- @memSource@, heap corruption or failure with a segfault.
--
-- @since 0.3.0
moveByteOffToPtrMutMem ::
     forall e mw m s. (MonadPrim s m, MemWrite mw, Unbox e)
  => mw s -- ^ /memSource/ - Source memory from where to copy
  -> Off Word8
  -- ^ /memSourceOff/ - Offset in number of bytes into source memory
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memSourceOff
  --
  -- With offset applied it should still refer to the same memory region. For types that
  -- also implement `MemAlloc` this can be described as:
  --
  -- > sourceByteCount <- getByteCountMutMem memSource
  -- > unOff (toByteOff memSourceOff) <= unCount (sourceByteCount - byteCountType @e)
  -> Ptr e
  -- ^ /memTarget/ - Target memory into where to copy
  --
  -- /__Precondition:__/
  --
  -- Once the pointer is advanced by @memTargetOff@ the next @unCountBytes memCount@ bytes must
  -- still belong to the same region of memory @memTargetWrite@
  -> Off Word8
  -- ^ /memTargetOff/ - Offset in number of bytes into target memory where writing will start
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memTargetOff
  --
  -- Once the pointer is advanced by @memTargetOff@ it must still refer to the same
  -- memory region @memTarget@
  -> Count e
  -- ^ /memCount/ - Number of elements of type __@e@__ to copy
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memCount
  --
  -- Both source and target memory regions must have enough memory to perform a copy
  -- of @memCount@ elements starting at their respective offsets. For /memSource/ that also
  -- implements `MemAlloc` this can be described as:
  --
  -- > sourceByteCount <- getByteCountMutMem memSource
  -- > unOff memSourceOff + unCountBytes memCount <= unCount (sourceByteCount - byteCountType @e)
  -> m ()
moveByteOffToPtrMutMem src srcOff dst dstOff c =
  liftST $ moveByteOffToPtrMutMemST src srcOff dst dstOff c
{-# INLINE moveByteOffToPtrMutMem #-}

-- | Copy contiguous chunk of memory from the read only memory region into the target
-- mutable memory region. Source and target /must not/ refer to the same memory region,
-- otherwise that would imply that the source is not immutable which would be a
-- violation of some other invariant elsewhere in the code.
--
-- [Unsafe] When any precondition for one of the offsets @memSourceOff@, @memTargetOff@
-- or the element count @memCount@ is violated a call to this function can result in:
-- copy of data that doesn't belong to @memSourceRead@, heap corruption or failure with
-- a segfault.
--
-- @since 0.1.0
copyByteOffMutMem ::
     forall e mr mw m s. (MonadPrim s m, MemWrite mw, MemRead mr, Unbox e)
  => mr -- ^ /memSourceRead/ - Read-only source memory region from where to copy
  -> Off Word8
  -- ^ /memSourceOff/ - Offset into source memory in number of bytes
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memSourceOff
  --
  -- > unOff memSourceOff <= unCount (byteCountMem memSourceRead - byteCountType @e)
  -> mw s -- ^ /memTargetWrite/ - Target mutable memory
  -> Off Word8
  -- ^ /memTargetOff/ -  Offset into target memory in number of bytes
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memTargetOff
  --
  -- With offset applied it should still refer to the same memory region. For types that
  -- also implement `MemAlloc` this can be described as:
  --
  -- > targetByteCount <- getByteCountMutMem memTargetWrite
  -- > unOffBytes memTargetOff <= unCount (targetByteCount - byteCountType @e)
  -> Count e
  -- ^ /memCount/ - Number of elements of type __@e@__ to copy
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memCount
  --
  -- Both source and target memory regions must have enough memory to perform a copy
  -- of @memCount@ elements starting at their respective offsets. For @memSourceRead@:
  --
  -- > unOff memSourceOff + unCountBytes memCount <= unCount (byteCountMem memSourceRead - byteCountType @e)
  --
  -- and for @memTargetWrite@ that also implements `MemAlloc` this can be described as:
  --
  -- > targetByteCount <- getByteCountMutMem memTargetWrite
  -- > unOff memTargetOff + unCountBytes memCount <= unCount (targetByteCount - byteCountType @e)
  -> m ()
copyByteOffMutMem src srcOff dst dstOff c =
  liftST $ copyByteOffMutMemST src srcOff dst dstOff c
{-# INLINE copyByteOffMutMem #-}

-- | Copy contiguous chunk of memory from a mutable memory region into the target
-- mutable memory region. Source and target /may/ refer to the same memory region.
--
-- [Unsafe] When any precondition for one of the offsets @memSourceOff@, @memTargetOff@
-- or the element count @memCount@ is violated a call to this function can result in:
-- copy of data that doesn't belong to @memSourceRead@, heap corruption or failure with
-- a segfault.
--
-- @since 0.3.0
moveByteOffMutMem ::
     forall e mw' mw m s. (MonadPrim s m, MemWrite mw, MemWrite mw', Unbox e)
  => mw' s -- ^ /memSource/ - Source memory from where to copy
  -> Off Word8
  -- ^ /memSourceOff/ - Offset in number of bytes into source memory
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memSourceOff
  --
  -- With offset applied it should still refer to the same memory region. For types that
  -- also implement `MemAlloc` this can be described as:
  --
  -- > sourceByteCount <- getByteCountMutMem memSource
  -- > unOffBytes memSourceOff <= unCount (sourceByteCount - byteCountType @e)
  -> mw s -- ^ /memTarget/ - Target memory into where to copy
  -> Off Word8
  -- ^ /memTargetOff/ -  Offset into target memory in number of bytes
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memTargetOff
  --
  -- With offset applied it should still refer to the same memory region. For types that
  -- also implement `MemAlloc` this can be described as:
  --
  -- > targetByteCount <- getByteCountMutMem memTarget
  -- > unOffBytes (toByteOff memTargetOff) <= unCount (targetByteCount - byteCountType @e)
  -> Count e
  -- ^ /memCount/ - Number of elements of type __@e@__ to copy
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memCount
  --
  -- Both source and target memory regions must have enough memory to perform a copy
  -- of @memCount@ elements starting at their respective offsets. For types that also
  -- implement `MemAlloc` this can be described as:
  --
  -- > sourceByteCount <- getByteCountMutMem memSource
  -- > unOff memSourceOff + unCountBytes memCount <= unCount (sourceByteCount - byteCountType @e)
  --
  -- > targetByteCount <- getByteCountMutMem memTarget
  -- > unOff memTargetOff + unCountBytes memCount <= unCount (targetByteCount - byteCountType @e)
  -> m ()
moveByteOffMutMem src srcOff dst dstOff c =
  liftST $ moveByteOffMutMemST src srcOff dst dstOff c
{-# INLINE moveByteOffMutMem #-}

-- TODO: Potential feature for the future implementation. Will require extra function in `Prim`.
--setByteOffMutMem :: (MonadPrim s m, Unbox e) => w s -> Off Word8 -> Count e -> e -> m ()

-- | Write the same value @memCount@ times into each cell of @memTarget@ starting at an
-- offset @memTargetOff@.
--
-- [Unsafe] Bounds are not checked. When precondition for @memTargetOff@ argument is
-- violated the outcome is either heap corruption or failure with a segfault.
--
-- @since 0.3.0
setMutMem ::
     forall e mw m s. (MonadPrim s m, MemWrite mw, Unbox e)
  => mw s -- ^ /memTarget/ - Target memory into where to write the element
  -> Off e
  -- ^ /memTargetOff/ - Offset into target memory in number of elements at which element
  -- setting should start.
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memTargetOff
  --
  -- With offset applied it should still refer to the same memory region. For types that
  -- also implement `MemAlloc` this can be described as:
  --
  -- > targetByteCount <- getByteCountMutMem memTarget
  -- > unOffBytes memTargetOff <= unCount (targetByteCount - byteCountType @e)
  -> Count e
  -- ^ /memCount/ - Number of times the element @elt@ should be written
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memCount
  --
  -- Target memory region should have enough memory to perform a set operation of the
  -- supplied element @memCount@ number of times starting at the supplied offset. For
  -- types that also implement `MemAlloc` this can be described as:
  --
  -- > targetByteCount <- getByteCountMutMem memTarget
  -- > unCountBytes memCount + unOff memTargetOff <= unCount (targetByteCount - byteCountType @e)
  -> e
  -- ^ /elt/ - Element to write into memory cells. This function is strict with
  -- respect to element, which means that the even @memCount = 0@ it might be still
  -- fully evaluated.
  -> m ()
setMutMem dst dstOff c = liftST . setMutMemST dst dstOff c
{-# INLINE setMutMem #-}


-- | Extract the number of bytes a mutable memory region can hold, i.e. what is the
-- total allocated size for this region. The size of a region can be changes and in some
-- circuimstances even in place without copy, see `reallocMutMem` for more info.
--
-- ====__Examples__
--
-- >>> m <- allocMutMem (10 :: Count Int64) :: IO (MBytes 'Pin RW)
-- >>> getByteCountMutMem m
-- Count {unCount = 80}
--
-- @since 0.3.0
getByteCountMutMem ::
     forall ma m s. (MonadPrim s m, MemAlloc ma)
  => ma s
  -> m (Count Word8)
getByteCountMutMem = liftST . getByteCountMutMemST
{-# INLINE getByteCountMutMem #-}


-- | Allocate a mutable memory region for specified number of elements. Memory is not
-- reset and will likely hold some garbage data, therefore prefer to use `allocZeroMutMem`,
-- unless it is guaranteed that all of allocated memory will be overwritten.
--
-- [Unsafe] When any of preconditions for @memCount@ argument is violated the outcome is
-- unpredictable. One possible outcome is termination with
-- `Control.Exception.HeapOverflow` async exception. In a pure setting, such as when
-- executed within `runST`, if allocated memory is not fully overwritten it can lead to
-- violation of referential transparency, because contents of newly allocated region is
-- non-determinstic.
--
-- @since 0.3.0
allocMutMem ::
     forall e ma m s. (MonadPrim s m, MemAlloc ma, Unbox e)
  => Count e
  -- ^ /memCount/ - Amount of memory to allocate for the region in number of elements of
  -- type __@e@__
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memCount
  --
  -- Possibility of overflow:
  --
  -- > memCount <= fromByteCount maxBound
  --
  -- When converted to bytes the value should be less then available physical memory
  -> m (ma s)
allocMutMem = liftST . allocMutMemST
{-# INLINE allocMutMem #-}


-- | Either grow or shrink currently allocated mutable region of memory. For some
-- implementations it might be possible to change the size of the allocated region
-- in-place, i.e. without copy. However in all implementations there is a good chance
-- that the memory region has to be allocated anew, in which case all of the contents
-- up to the minimum of new and old sizes will get copied over. After the resize
-- operation is complete the supplied @memSource@ region must not be used
-- anymore. Moreover, no reference to the old one should be kept in order to allow
-- garbage collection of the original in case a new one had to be allocated.
--
-- Default implementation is `defaultReallocMutMem`
--
-- [Unsafe] Undefined behavior when @memSource@ is used afterwards. The same unsafety
-- notice from `allocMutMem` with regards to @memCount@ is applicable here as well.
--
-- @since 0.3.0
reallocMutMem ::
     forall e ma m s. (MonadPrim s m, MemAlloc ma, Unbox e)
  => ma s
  -- ^ /memSource/ - Source memory region to resize
  -> Count e
  -- ^ /memCount/ - Number of elements for the reallocated memory region
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memCount
  --
  -- Should be less then available physical memory
  -> m (ma s)
reallocMutMem ma = liftST . reallocMutMemST ma
{-# INLINE reallocMutMem #-}


-- -- | A wrapper that adds a phantom state token. It can be used with types that either
-- -- don't have such state token or are designed to work in `IO` and therefore restricted
-- -- to `RealWorld`. Using this wrapper is very much unsafe, so make sure you know what you are
-- -- doing.
-- newtype MemState a s = MemState { unMemState :: a }

--
-- @since 0.3.0
modifyFetchOldMutMem ::
     forall e mw m s. (MemWrite mw, MonadPrim s m, Unbox e)
  => mw s
  -> Off e
  -> (e -> e)
  -> m e
modifyFetchOldMutMem mem o f = modifyFetchOldMutMemM mem o (pure . f)
{-# INLINE modifyFetchOldMutMem #-}


--
-- @since 0.3.0
modifyFetchNewMutMem ::
     forall e mw m s. (MemWrite mw, MonadPrim s m, Unbox e)
  => mw s
  -> Off e
  -> (e -> e)
  -> m e
modifyFetchNewMutMem mem o f = modifyFetchNewMutMemM mem o (pure . f)
{-# INLINE modifyFetchNewMutMem #-}


--
-- @since 0.3.0
modifyFetchOldMutMemM ::
     forall e mw m s. (MemWrite mw, MonadPrim s m, Unbox e)
  => mw s
  -> Off e
  -> (e -> m e)
  -> m e
modifyFetchOldMutMemM mem o f = do
  a <- readOffMutMem mem o
  a <$ (writeOffMutMem mem o =<< f a)
{-# INLINE modifyFetchOldMutMemM #-}


--
-- @since 0.3.0
modifyFetchNewMutMemM ::
     forall e mw m s. (MemWrite mw, MonadPrim s m, Unbox e)
  => mw s
  -> Off e
  -> (e -> m e)
  -> m e
modifyFetchNewMutMemM mem o f = do
  a <- readOffMutMem mem o
  a' <- f a
  a' <$ writeOffMutMem mem o a'
{-# INLINE modifyFetchNewMutMemM #-}


-- | Place @n@ copies of supplied region of memory one after another in a newly allocated
-- contiguous chunk of memory. Similar to `stimes`, but the source memory @memRead@ does
-- not have to match the type of `Frozen` ma.
--
-- ====__Example__
--
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> import Primal.Memory
-- >>> let b = fromListMem @Word8 @(MBytes 'Inc) [0xde, 0xad, 0xbe, 0xef]
-- >>> cycleMemN @(MBytes 'Inc) 2 b
-- [0xde,0xad,0xbe,0xef,0xde,0xad,0xbe,0xef]
--
-- @since 0.1.0
cycleMemN ::
     forall ma mr. (MemAlloc ma, MemRead mr)
  => Int
  -> mr
  -> Frozen ma
cycleMemN n r
  | n <= 0 = emptyMem
  | otherwise =
    runST $ do
      let bc@(Count chunk) = byteCountMem r
          c@(Count c8) = Count n * bc
      mem <- allocMutMem c
      let go i = when (i < c8) $ copyByteOffMutMem r 0 mem (Off i) bc >> go (i + chunk)
      go 0
      freezeMut mem
{-# INLINE cycleMemN #-}


-- | Construct an immutable memory region that can't hold any data. Same as @`mempty` ::
-- `Frozen` ma@
--
-- ====__Example__
--
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> import Primal.Memory
-- >>> toListMem (emptyMem @(MBytes 'Inc)) :: [Int]
-- []
--
-- @since 0.1.0
emptyMem ::
     forall ma. MemAlloc ma
  => Frozen ma
emptyMem = createMemST_ (0 :: Count Word8) (\_ -> pure ())
{-# INLINE emptyMem #-}

-- | Allocate a region of immutable memory that holds a single element.
--
-- ====__Example__
--
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> import Primal.Memory
-- >>> toListMem (singletonMem @Word16 @(MBytes 'Inc) 0xffff) :: [Word8]
-- [255,255]
--
-- @since 0.1.0
singletonMem ::
     forall e ma. (MemAlloc ma, Unbox e)
  => e -- ^ The single element that will be stored in the newly allocated region of memory
  -> Frozen ma
singletonMem a = createMemST_ (1 :: Count e) $ \mem -> writeOffMutMem mem 0 a
{-# INLINE singletonMem #-}

-- | Same as `allocMutMem`, but also use `setMutMem` to reset all of newly allocated memory to
-- zeros.
--
-- [Unsafe] When precondition for @memCount@ argument is violated the outcome is
-- unpredictable. One possible outcome is termination with `Control.Exception.HeapOverflow`
-- async exception.
--
-- ====__Example__
--
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> import Primal.Memory
-- >>> mb <- allocZeroMutMem @Int @(MBytes 'Inc) 10
-- >>> b <- freezeMutMem mb
-- >>> toListMem b :: [Int]
-- [0,0,0,0,0,0,0,0,0,0]
--
-- @since 0.3.0
allocZeroMutMem ::
     forall e ma m s. (MemAlloc ma, MonadPrim s m, Unbox e)
  => Count e
  -- ^ /memCount/ - Amount of memory to allocate for the region in number of elements of
  -- type __@e@__
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memCount
  --
  -- Possibility of overflow:
  --
  -- > memCount <= fromByteCount maxBound
  --
  -- When converted to bytes the value should be less then available physical memory
  -> m (ma s)
allocZeroMutMem n = do
  m <- allocMutMem n
  m <$ setMutMem m 0 (toByteCount n) (0 :: Word8)
{-# INLINE allocZeroMutMem #-}

-- | Allocate a mutable region of memory and fill it with the supplied `ST` action. Besides
-- the newly filled frozen memory this function also returns the result produced by the
-- filling action. See `createMemST_` for the version that discards it. Also see
-- `createZeroMemST` for a safer alternative.
--
-- [Unsafe] Same caviats as in `allocMutMem`
--
-- @since 0.1.0
createMemST ::
     forall e b ma. (MemAlloc ma, Unbox e)
  => Count e
  -- ^ /memCount/ - Amount of memory to allocate for the region in number of elements of
  -- type __@e@__
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memCount
  --
  -- Possibility of overflow:
  --
  -- > memCount <= fromByteCount maxBound
  --
  -- When converted to bytes the value should be less then available physical memory
  -> (forall s. ma s -> ST s b)
  -- ^ /memFillAction/ - This action will be used to modify the contents of newly
  -- allocated memory. Make sure to overwrite all of it, otherwise it might lead to
  -- breaking referential transparency.
  -> (b, Frozen ma)
createMemST n f = runST $ allocMutMem n >>= \m -> (,) <$> f m <*> freezeMut m
{-# INLINE createMemST #-}


createMemST_ ::
     forall e b ma. (MemAlloc ma, Unbox e)
  => Count e
  -- ^ /memCount/ - Amount of memory to allocate for the region in number of elements of
  -- type __@e@__
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memCount
  --
  -- Possibility of overflow:
  --
  -- > memCount <= fromByteCount maxBound
  --
  -- When converted to bytes the value should be less then available physical memory
  -> (forall s. ma s -> ST s b)
  -- ^ /memFillAction/ - This action will be used to modify the contents of newly
  -- allocated memory. Make sure to overwrite all of it, otherwise it might lead to
  -- breaking referential transparency.
  -> Frozen ma
createMemST_ n f = runST (allocMutMem n >>= \m -> f m >> freezeMut m)
{-# INLINE createMemST_ #-}

-- | Same as `createMemST`, except it in ensures that the memory is reset to zeros right
-- after allocation
--
-- [Unsafe] Same caviats as in `allocZeroMutMem`: violation of precondition for @memCount@ may
-- result in undefined behavior or `Control.Exception.HeapOverflow` async exception.
--
-- @since 0.1.0
createZeroMemST ::
     forall e ma b. (MemAlloc ma, Unbox e)
  => Count e
  -- ^ /memCount/ - Amount of memory to allocate for the region in number of elements of
  -- type __@e@__
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memCount
  --
  -- Possibility of overflow:
  --
  -- > memCount <= fromByteCount maxBound
  --
  -- When converted to bytes the value should be less then available physical memory
  -> (forall s. ma s -> ST s b)
  -- ^ /fillAction/ -- Action that will be used to modify contents of newly allocated
  -- memory. It is not required to overwrite the full region, since the whole thing will
  -- be reset to zeros before applying this action.
  -> (b, Frozen ma)
createZeroMemST n f = runST $ allocZeroMutMem n >>= \m -> (,) <$> f m <*> freezeMut m
{-# INLINE createZeroMemST #-}

-- | Same as `createMemST_`, except it ensures that the memory gets reset with zeros right
-- after allocation and prior applying the @ST@ filling action @fillAction@.
--
-- [Unsafe] Same reasons as `allocZeroMutMem`: violation of precondition for @memCount@ may
-- result in undefined behavior or `Control.Exception.HeapOverflow` async exception.
--
-- ====__Example__
--
-- Note that this example will work correctly only on little-endian machines:
--
-- >>> :set -XTypeApplications
-- >>> import Primal.Monad
-- >>> let ibs = zip [0, 4 ..] [0x48,0x61,0x73,0x6b,0x65,0x6c,0x6c] :: [(Off Word8, Word8)]
-- >>> let c = Count (length ibs) :: Count Char
-- >>> let bc = createZeroMemST_ @_ @(MBytes 'Inc) c $ \m -> forM_ ibs $ \(i, b) -> writeByteOffMutMem m i b
-- >>> toListMem bc :: String
-- "Haskell"
--
-- @since 0.1.0
createZeroMemST_ ::
     forall e ma b. (MemAlloc ma, Unbox e)
  => Count e
  -- ^ /memCount/ - Amount of memory to allocate for the region in number of elements of
  -- type __@e@__
  --
  -- /__Precoditions:__/
  --
  -- Size should be non-negative, but smaller than amount of available memory. Note that the
  -- second condition simply describes overflow.
  --
  -- > 0 <= memCount
  --
  -- Possibility of overflow:
  --
  -- > memCount <= fromByteCount maxBound
  --
  -> (forall s. ma s -> ST s b)
  -- ^ /fillAction/ -- Action that will be used to modify contents of newly allocated
  -- memory. It is not required to overwrite the full region, since the whole thing will
  -- be reset to zeros before applying this action.
  -> Frozen ma
createZeroMemST_ n f = runST (allocZeroMutMem n >>= \m -> f m >> freezeMut m)
{-# INLINE createZeroMemST_ #-}

-- | Copy all of the data from the source into a newly allocated memory region of identical
-- size.
--
-- ====__Examples__
--
-- >>> :set -XDataKinds
-- >>> import Primal.Memory
-- >>> import Primal.Memory.Bytes
-- >>> let xs = fromByteListMem @(MBytes 'Pin) [0..15] :: Bytes 'Pin
-- >>> let ys = cloneMem xs
-- >>> let report bEq pEq = print $ "Bytes equal: " ++ show bEq ++ ", their pointers equal: " ++ show pEq
-- >>> withPtrBytes xs $ \ xsPtr -> withPtrBytes ys $ \ ysPtr -> report (xs == ys) (xsPtr == ysPtr)
-- "Bytes equal: True, their pointers equal: False"
-- >>> report (eqByteMem xs ys) (isSameBytes xs ys)
-- "Bytes equal: True, their pointers equal: False"
--
-- @since 0.2.0
cloneMem ::
     forall ma. MemAlloc ma
  => Frozen ma -- ^ /memSource/ - immutable source memory.
  -> Frozen ma
cloneMem = clone
{-# INLINE cloneMem #-}

-- | Similar to `copyByteOffMutMem`, but supply offsets in number of elements instead of
-- bytes. Copy contiguous chunk of memory from the read only memory region into the target
-- mutable memory region. Source and target /must not/ refer to the same memory region,
-- otherwise that would imply that the source is not immutable which would be a violation
-- of some other invariant elsewhere in the code.
--
-- [Unsafe] When any precondition for one of the offsets @memSourceOff@, @memTargetOff@
-- or the element count @memCount@ is violated a call to this function can result in:
-- copy of data that doesn't belong to @memSourceRead@, heap corruption or failure with
-- a segfault.
--
-- @since 0.1.0
copyMem ::
     forall e mr mw m s. (MonadPrim s m, MemRead mr, MemWrite mw, Unbox e)
  => mr -- ^ /memSourceRead/ - Read-only source memory region from which the data will
        -- copied
  -> Off e
  -- ^ /memSourceOff/ - Offset into source memory in number of elements of type __@e@__
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memSourceOff
  --
  -- > unOff memSourceOff < unCount (countMem memSourceRead)
  -> mw s -- ^ /memTargetWrite/ - Target mutable memory
  -> Off e
  -- ^ /memTargetOff/ -  Offset into target memory in number of elements
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memTargetOff
  --
  -- With offset applied it should still refer to the same memory region. For types that
  -- also implement `MemAlloc` this can be described as:
  --
  -- > targetCount <- getCountMutMem memTargetWrite
  -- > unOff memTargetOff < unCount targetCount
  -> Count e
  -- ^ /memCount/ - Number of elements of type __@e@__ to copy
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memCount
  --
  -- Both source and target memory regions must have enough memory to perform a copy
  -- of @memCount@ elements starting at their respective offsets. For @memSourceRead@:
  --
  -- > unOff memSourceOff + unCount memCount < unCount (countMem memSourceRead)
  --
  -- and for @memTargetWrite@ that also implements `MemAlloc` this can be described as:
  --
  -- > targetCount <- getCountMutMem memTargetWrite
  -- > unOff memTargetOff + unCount memCount < unCount targetCount
  -> m ()
copyMem src srcOff dst dstOff = copyByteOffMutMem src (toByteOff srcOff) dst (toByteOff dstOff)
{-# INLINE copyMem #-}


--
-- @since 0.3.0
moveMutMem ::
     forall e mw1 mw2 m s. (MonadPrim s m, MemWrite mw1, MemWrite mw2, Unbox e)
  => mw1 s -- ^ Source memory region
  -> Off e -- ^ Offset into the source in number of elements
  -> mw2 s -- ^ Destination memory region
  -> Off e -- ^ Offset into destination in number of elements
  -> Count e -- ^ Number of elements to copy over
  -> m ()
moveMutMem src srcOff dst dstOff = moveByteOffMutMem src (toByteOff srcOff) dst (toByteOff dstOff)
{-# INLINE moveMutMem #-}


appendMem ::
     forall mr1 mr2 ma. (MemRead mr1, MemRead mr2, MemAlloc ma)
  => mr1
  -> mr2
  -> Frozen ma
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
  -> Frozen ma
concatMem xs = do
  let c = Foldable.foldl' (\ !acc b -> acc + byteCountMem b) 0 xs
  createMemST_ c $ \mb -> do
    let load i b = do
          let cb@(Count n) = byteCountMem b :: Count Word8
          (i + Off n) <$ copyMem b 0 mb i cb
    foldM_ load 0 xs
{-# INLINABLE concatMem #-}

-- | Convert the state of an immutable memory region to the mutable one. This is a no
-- copy operation, as such it is fast, but extremely dangerous. See `thawCloneMutMem` for a safe
-- alternative.
--
-- [Extremely Unsafe] This function makes it possible to break referential transparency,
-- because any subsequent destructive operation to the mutable region of memory will also
-- be reflected in the frozen immutable type as well. The most dangerous part about this
-- function is that the argument suppplied can be floated out by the compiler and thus
-- shared by many invocations to `thawMem`.
--
-- @since 0.1.0
thawMem ::
     forall ma m s. (MemAlloc ma, MonadPrim s m)
  => Frozen ma
  -> m (ma s)
thawMem = liftST . thawST
{-# INLINE thawMem #-}


-- | Convert the state of a mutable memory region to the immutable one. This is a no
-- copy operation, as such it is fast, but dangerous. See `freezeCopyMem` for a safe alternative.
--
-- [Unsafe] It makes it possible to break referential transparency, because any
-- subsequent destructive operation to the mutable region of memory will also be
-- reflected in the frozen immutable type as well.
--
-- @since 0.3.0
freezeMutMem ::
     forall ma m s. (MemAlloc ma, MonadPrim s m)
  => ma s
  -> m (Frozen ma)
freezeMutMem = liftST . freezeMutST
{-# INLINE freezeMutMem #-}


-- | This is a safe version of `thawMem`. It first makes an exact copy of the supplied
-- memory region and only then thaws it, thus yielding a mutable region of memory. This
-- means any mutation, will only affect the newly allocated region that was returned and
-- not the source region.
--
-- ====__Examples__
--
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> import Primal.Memory
-- >>> let fm = fromListMem @Word8 @(MBytes 'Inc) [1,2,3,4]
-- >>> mm <- thawCloneMem fm
-- >>> writeOffMutMem mm 1 (0xadde :: Word16)
-- >>> freezeMutMem mm
-- [0x01,0x02,0xde,0xad]
-- >>> fm
-- [0x01,0x02,0x03,0x04]
--
-- @since 0.1.0
thawCloneMem ::
     forall ma m s. (MemAlloc ma, MonadPrim s m)
  => Frozen ma
  -> m (ma s)
thawCloneMem a = thawCopyMem a 0 (byteCountMem a)
{-# INLINE thawCloneMem #-}


-- | Similar to `thawCloneMem`, except it is possible to specify which portion of the
-- frozen region will be copied over and thawed.
--
-- [Unsafe] When any precondition for eihter an offset @memSourceOff@ or the element count
-- @memCount@ is violated a call to this function can result in: copy of data that doesn't
-- belong to @memSource@ or failure with a segfault.
--
-- ====__Examples__
--
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> import Primal.Memory
-- >>> let fm = fromListMem @Word8 @(MBytes 'Inc) [1,2,3,4,5]
-- >>> mm <- thawCopyMem fm 1 (3 :: Count Word8)
-- >>> writeOffMutMem mm 1 (0 :: Word8)
-- >>> freezeMutMem mm
-- [0x02,0x00,0x04]
-- >>> fm
-- [0x01,0x02,0x03,0x04,0x05]
--
-- @since 0.1.0
thawCopyMem ::
     forall e ma m s. (Unbox e, MemAlloc ma, MonadPrim s m)
  => Frozen ma -- ^ /memSource/ - Read-only source memory region from which the data
                  -- will copied and thawed
  -> Off e
  -- ^ /memSourceOff/ - Offset into source memory in number of elements of type __@e@__
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memSourceOff
  --
  -- > unOff memSourceOff < unCount (countMem memSource)
  -> Count e
  -- ^ /memCount/ - Number of elements of type __@e@__ to copy
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memCount
  --
  -- > unOff memSourceOff + unCount memCount < unCount (countMem memSource)
  -> m (ma s)
thawCopyMem a off c = do
  mem <- allocMutMem c
  mem <$ copyMem a off mem 0 c
{-# INLINE thawCopyMem #-}



--
-- @since 0.3.0
freezeCopyMutMem ::
     forall e ma m s. (Unbox e, MemAlloc ma, MonadPrim s m)
  => ma s
  -> Off e
  -> Count e
  -> m (Frozen ma)
freezeCopyMutMem mem off c = freezeMut mem >>= \r -> thawCopyMem r off c >>= freezeMut
{-# INLINE freezeCopyMutMem #-}

-- | Safe version of `freezeMutMem`. Yields an immutable copy of the supplied mutable
-- memory region. Further mutation of the source memory region will not affect the
-- produced copy.
---
-- ====__Example__
--
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> import Primal.Memory
-- >>> mb <- allocZeroMutMem @Word8 @(MBytes 'Pin) 4
-- >>> writeOffMutMem mb 2 (0xff :: Word8)
-- >>> b <- freezeCloneMutMem mb
-- >>> writeOffMutMem mb 1 (0xab :: Word8)
-- >>> b
-- [0x00,0x00,0xff,0x00]
-- >>> freezeMutMem mb
-- [0x00,0xab,0xff,0x00]
--
-- @since 0.3.0
freezeCloneMutMem ::
     forall ma m s. (MemAlloc ma, MonadPrim s m)
  => ma s
  -> m (Frozen ma)
freezeCloneMutMem = freezeCloneMut
{-# INLINE freezeCloneMutMem #-}




-- | /O(n)/ - Convert a read-only memory region into a newly allocated other type of
-- memory region
--
-- >>> import Data.ByteString (pack)
-- >>> let bs = pack [0x10 .. 0x20]
-- >>> bs
-- "\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\EM\SUB\ESC\FS\GS\RS\US "
-- >>> convertMem bs :: Bytes 'Inc
-- [0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18,0x19,0x1a,0x1b,0x1c,0x1d,0x1e,0x1f,0x20]
--
-- @since 0.1.0
convertMem :: (MemRead mr, MemAlloc ma) => mr -> Frozen ma
convertMem m =
  let c = byteCountMem m
   in createMemST_ c (\mm -> copyMem m 0 mm 0 c)
{-# INLINE convertMem #-}

-- | Figure out how many elements fits into the immutable region of memory. It is
-- possible that there is a remainder of bytes left, see `countRemMem` for getting that
-- too.
--
-- ====__Examples__
--
-- >>> let b = fromListMem [0 .. 5 :: Word8] :: Bytes 'Pin
-- >>> b
-- [0x00,0x01,0x02,0x03,0x04,0x05]
-- >>> countMem b :: Count Word16
-- Count {unCount = 3}
-- >>> countMem b :: Count Word32
-- Count {unCount = 1}
-- >>> countMem b :: Count Word64
-- Count {unCount = 0}
--
-- @since 0.1.0
countMem ::
     forall e mr. (MemRead mr, Unbox e)
  => mr
  -> Count e
countMem = fromByteCount . byteCountMem
{-# INLINE countMem #-}

-- | Figure out how many elements and a byte size remainder can fit into the immutable
-- region of memory.
--
-- ====__Examples__
--
-- >>> let b = fromListMem [0 .. 5 :: Word8] :: Bytes 'Pin
-- >>> b
-- [0x00,0x01,0x02,0x03,0x04,0x05]
-- >>> countRemMem @Word16 b
-- (Count {unCount = 3},Count {unCount = 0})
-- >>> countRemMem @Word32 b
-- (Count {unCount = 1},Count {unCount = 2})
-- >>> countRemMem @Word64 b
-- (Count {unCount = 0},Count {unCount = 6})
--
-- @since 0.1.0
countRemMem :: forall e mr. (MemRead mr, Unbox e) => mr -> (Count e, Count Word8)
countRemMem = fromByteCountRem . byteCountMem
{-# INLINE countRemMem #-}

-- | Figure out how many elements fits into the mutable region of memory. Similar to
-- `countMem`, except that it is not a pure funciton, since the size of mutable memory can
-- change throuhout its lifetime. It is possible that there is a remainder of bytes left,
-- see `getCountRemMem` for getting that too.
--
-- ====__Examples__
--
-- >>> mb <- thawMem (fromListMem [0 .. 5 :: Word8] :: Bytes 'Pin)
-- >>> getCountMutMem mb :: IO (Count Word16)
-- Count {unCount = 3}
-- >>> getCountMutMem mb :: IO (Count Word32)
-- Count {unCount = 1}
-- >>> getCountMutMem mb :: IO (Count Word64)
-- Count {unCount = 0}
-- >>> mb' <- reallocMutMem mb (6 :: Count Word64)
-- >>> getCountMutMem mb' :: IO (Count Word32)
-- Count {unCount = 12}
--
-- @since 0.3.0
getCountMutMem :: forall e ma m s. (MemAlloc ma, MonadPrim s m, Unbox e) => ma s -> m (Count e)
getCountMutMem = fmap (fromByteCount . coerce) . getByteCountMutMem
{-# INLINE getCountMutMem #-}


-- | Figure out how many elements and a byte size remainder can fit into the mutable
-- region of memory. Similar to `countRemMem`, except it is a monadic action for mutable
-- regions instead of a pure function for immutable memory. See `getCountMutMem` for getting
-- the element count only.
--
-- ====__Examples__
--
-- >>> b <- thawMem (fromListMem [0 .. 5 :: Word8] :: Bytes 'Pin)
-- >>> getCountRemMutMem @Word16 b
-- (Count {unCount = 3},Count {unCount = 0})
-- >>> getCountRemMutMem @Word32 b
-- (Count {unCount = 1},Count {unCount = 2})
-- >>> getCountRemMutMem @Word64 b
-- (Count {unCount = 0},Count {unCount = 6})
--
-- @since 0.3.0
getCountRemMutMem ::
     forall e ma m s. (MemAlloc ma, MonadPrim s m, Unbox e)
  => ma s
  -> m (Count e, Count Word8)
getCountRemMutMem = fmap (fromByteCountRem . coerce) . getByteCountMutMem
{-# INLINE getCountRemMutMem #-}

-- | Allocate the same amount of memory as the source memory region and copy all of its
-- data over.
--
-- @since 0.3.0
cloneMutMem ::
     forall ma m s. (MemAlloc ma, MonadPrim s m)
  => ma s
  -> m (ma s)
cloneMutMem = cloneMut
{-# INLINE cloneMutMem #-}

-- | Compare two memory regions byte-by-byte. Computation may be short-circuited on the
-- first mismatch, but it is `MemRead` implementation specific.
--
-- @since 0.3.0
eqByteOffMem ::
     (MemRead mr1, MemRead mr2)
  => mr1 -- ^ /memRead1/ - First region of memory
  -> Off Word8
  -- ^ /memOff1/ - Offset for @memRead1@ in number of bytes
  --
  -- /__Precondition:__/
  --
  -- > 0 <= memOff1
  -> mr2 -- ^ /memRead2/ - Second region of memory
  -> Off Word8
  -- ^ /memOff2/ - Offset for @memRead1@ in number of bytes
  --
  -- /__Precondition:__/
  --
  -- > 0 <= memOff2
  -> Count Word8
  -- ^ /memCount/ - Number of bytes compare
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memCount
  --
  -- > offToCount memOff1 + memCount < countMem memRead1
  --
  -- > offToCount memOff2 + memCount < countMem memRead2
  -> Bool
eqByteOffMem b1 off1 b2 off2 n = compareByteOffMem b1 off1 b2 off2 n == EQ
{-# INLINE eqByteOffMem #-}

-- | Compare two memory regions for equality byte-by-byte. `False` is returned immediately
-- when sizes reported by `byteCountMem` do not match.
--
-- @since 0.3.0
eqByteMem :: (MemRead mr1, MemRead mr2) => mr1 -> mr2 -> Bool
eqByteMem b1 b2 = n == byteCountMem b2 && eqByteOffMem b1 0 b2 0 n
  where
    n = byteCountMem b1
{-# INLINE eqByteMem #-}


-- | Compare two memory regions byte-by-byte.
--
-- @since 0.3.0
compareByteMem :: (MemRead mr1, MemRead mr2) => mr1 -> mr2 -> Ordering
compareByteMem b1 b2 = compare n (byteCountMem b2) <> compareByteOffMem b1 0 b2 0 n
  where
    n = byteCountMem b1
{-# INLINE compareByteMem #-}

-- =============== --
-- List conversion --
-- =============== --

-------------
-- To List --
-------------

-- | Convert an immutable memory region to a list. Whenever memory byte count is not
-- exactly divisible by the size of the element there will be some slack left unaccounted
-- for. In order to get a hold of this slack use `toListSlackMem` instead.
--
-- ====__Examples__
--
-- >>> import Primal.Memory
-- >>> import Numeric (showHex)
-- >>> let b = fromByteListMem [0x48,0x61,0x73,0x6b,0x65,0x6c,0x6c] :: Bytes 'Inc
-- >>> toListMem b :: [Int8]
-- [72,97,115,107,101,108,108]
-- >>> let xs = toListMem b :: [Word32]
-- >>> xs
-- [1802723656]
-- >>> showHex (head xs) ""
-- "6b736148"
--
-- @since 0.1.0
toListMem :: forall e mr. (MemRead mr, Unbox e) => mr -> [e]
toListMem ba = build (\ c n -> foldrCountMem (countMem ba) c n ba)
{-# INLINE toListMem #-}
{-# SPECIALIZE toListMem :: Unbox e => Bytes p -> [e] #-}

-- | Same as `toListMem`, except when there is some slack towards the end of the memory
-- region that didn't fit into a list it will be returned as a list of bytes.
--
-- ====__Examples__
--
-- >>> import Data.Word
-- >>> :set -XDataKinds
-- >>> let a = fromListMem [0 .. 10 :: Word8] :: Bytes 'Pin
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
     forall e mr. (MemRead mr, Unbox e)
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

-- | Right fold that is useful for converting to a list while tapping into list fusion.
--
-- [Unsafe] Supplying Count larger than memory holds will result in reading out of bounds
-- and a potential segfault.
--
-- @since 0.1.0
foldrCountMem :: forall e b mr. (MemRead mr, Unbox e) => Count e -> (e -> b -> b) -> b -> mr -> b
foldrCountMem (Count k) c nil bs = go 0
  where
    go i
      | i == k = nil
      | otherwise =
        let !v = indexOffMem bs (Off i)
         in v `c` go (i + 1)
{-# INLINE[0] foldrCountMem #-}

---------------
-- From List --
---------------

-- Pure immutable conversion --

-- | Just like `fromListMemN`, except it ensures safety by using the length of the
-- list for allocation. Because it has to figure out the length of the list first it
-- will be just a little bit slower, but that much safer.
--
-- ====__Examples__
--
-- >>> import Primal.Memory
-- >>> :set -XDataKinds
-- >>> fromListMem "Hi" :: Bytes 'Inc
-- [0x48,0x00,0x00,0x00,0x69,0x00,0x00,0x00]
--
-- @since 0.1.0
fromListMem ::
     forall e ma. (Unbox e, MemAlloc ma)
  => [e]
  -> Frozen ma
fromListMem xs =
  let count = coerce (length xs) `countForProxyTypeOf` xs
   in createMemST_ count (loadListMutMemN_ count xs)
{-# INLINE fromListMem #-}


-- | Same as `fromListMem` but restricted to a list of `Word8`. Load a list of bytes into
-- a newly allocated memory region. Equivalent to `Data.ByteString.pack` for
-- `Data.ByteString.ByteString`
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
  -> Frozen ma
fromByteListMem = fromListMem
{-# INLINE fromByteListMem #-}


-- | Similarly to `fromListMem` load a list into a newly allocated memory region, but
-- unlike the aforementioned function it also accepts a hint of how many elements is
-- expected to be in the list. Because the number of expected an actual elements might
-- not match we return not only the frozen memory region, but also:
--
-- * either a list with leftover elements from the input @list@, if it did not fully fit
--   into the allocated region. An empty list would indicate that it did fit exactly.
--
--     @
--     unCount memCount <= length list
--     @
--
-- * or an exact count of how many elements have been loaded when there was no
--   enough elements in the list
--
--
-- In the latter case a zero value would indicate that the list did fit into the newly
-- allocated memory region exactly, which is perfectly fine. But a positive value would
-- mean that the tail of the memory region is still unset and might contain garbage
-- data. Make sure to overwrite the surplus memory yourself or use the safe version
-- `fromListZeroMemN` that fills the surplus with zeros.
--
-- [Unsafe] Whenever @memCount@ precodition is violated, because on each call with the
-- same input it can produce different output therefore it will break referential
-- transparency.
--
-- ====__Examples__
--
-- >>> :set -XTypeApplications
-- >>> fromListMemN @Char @(MBytes 'Inc) 3 "Hello"
-- (Left "lo",[0x48,0x00,0x00,0x00,0x65,0x00,0x00,0x00,0x6c,0x00,0x00,0x00])
-- >>> fromListMemN @Char @(MBytes 'Inc) 2 "Hi"
-- (Left "",[0x48,0x00,0x00,0x00,0x69,0x00,0x00,0x00])
-- >>> fst $ fromListMemN @Char @(MBytes 'Inc) 5 "Hi"
-- Right (Count {unCount = 2})
--
-- @since 0.2.0
fromListMemN ::
     forall e ma. (Unbox e, MemAlloc ma)
  => Count e
  -- ^ /memCount/ - Expected number of elements in the list, which exactly how much
  -- memory will be allocated.
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memCount
  -- > unCount memCount <= length list
  -> [e]
  -- ^ /list/ - A list of elements to load into the newly allocated memory region.
  -> (Either [e] (Count e), Frozen ma)
fromListMemN count xs =
  createMemST count $ \mm -> do
    (ys, loadedCount) <- loadListOffMutMemN count xs mm 0
    pure $
      if loadedCount /= count && null ys
        then Right loadedCount
        else Left ys
{-# INLINE fromListMemN #-}


-- | Just like `fromListMemN`, except it ensures safety by filling tail with zeros,
-- whenever the list is not long enough.
--
-- ====__Examples__
--
-- >>> import Primal.Memory
-- >>> :set -XTypeApplications
-- >>> fromListZeroMemN @Char @(MBytes 'Inc) 3 "Hi"
-- (Right (Count {unCount = 2}),[0x48,0x00,0x00,0x00,0x69,0x00,0x00,0x00,0x00,0x00,0x00,0x00])
--
-- @since 0.2.0
fromListZeroMemN ::
     forall e ma. (Unbox e, MemAlloc ma)
  => Count e -- ^ /memCount/ - Number of elements to load from the list.
  -> [e]
  -> (Either [e] (Count e), Frozen ma)
fromListZeroMemN count xs =
  createMemST (max 0 count) $ \mm -> do
    (ys, loadedCount) <- loadListOffMutMemN count xs mm 0
    let loadedByteCount = toByteCount loadedCount
        surplusByteCount = toByteCount count - loadedByteCount
    when (surplusByteCount > 0) $ setMutMem mm (countToOff loadedByteCount) surplusByteCount 0
    pure $
      if loadedCount /= count && null ys
        then Right loadedCount
        else Left ys
{-# INLINE fromListZeroMemN #-}

-- | Same as `fromListZeroMemN`, but ignore the extra information about how the loading went.
--
-- ====__Examples__
--
-- >>> import Primal.Memory
-- >>> fromListZeroMemN_ 3 "Hi" :: Bytes 'Inc
-- [0x48,0x00,0x00,0x00,0x69,0x00,0x00,0x00,0x00,0x00,0x00,0x00]
--
-- @since 0.2.0
fromListZeroMemN_ ::
     forall e ma. (Unbox e, MemAlloc ma)
  => Count e
  -> [e]
  -> Frozen ma
fromListZeroMemN_ !n = snd . fromListZeroMemN n
{-# INLINE fromListZeroMemN_ #-}



-- Mutable loading --


loadListByteOffHelper ::
     (MemWrite mw, MonadPrim s m, Unbox e)
  => [e]
  -> mw s
  -> Off Word8 -- ^ Offset
  -> Off Word8 -- ^ Upper bound
  -> Off Word8 -- ^ Element size
  -> m ([e], Count e)
loadListByteOffHelper ys mw byteOff k step =
  let go []       i = pure ([], toLoadedCount i)
      go a@(x:xs) i
        | i < k = writeByteOffMutMem mw i x >> go xs (i + step)
        | otherwise = pure (a, toLoadedCount i)
      toLoadedCount i = fromByteCount (offToCount (i - byteOff))
   in go ys byteOff
{-# INLINE loadListByteOffHelper #-}


-- | Load elements from the supplied list into a mutable memory region. Loading will
-- start at the supplied offset in number of bytes and will stop when either supplied
-- @elemCount@ number is reached or there are no more elements left in the list to
-- load. This action returns a list of elements that did not get loaded and the count of
-- how many elements did get loaded.
--
-- [Unsafe] When any precondition for either the offset @memTargetOff@ or the element
-- count @memCount@ is violated then a call to this function can result in heap corruption
-- or failure with a segfault.
--
-- ====__Examples__
--
-- For example load the @"Hell"@ somewhere in the middle of `MBytes`:
--
-- >>> ma <- allocZeroMutMem (6 :: Count Char) :: IO (MBytes 'Inc RW)
-- >>> loadListByteOffMutMemN 4 "Hello!" ma (toByteOff (1 :: Off Char))
-- ("o!",Count {unCount = 4})
-- >>> freezeMutMem ma
-- [0x00,0x00,0x00,0x00,0x48,0x00,0x00,0x00,0x65,0x00,0x00,0x00,0x6c,0x00,0x00,0x00,0x6c,0x00,0x00,0x00,0x00,0x00,0x00,0x00]
--
-- Or something more useful like loading prefixes from nested lists:
--
-- >>> import Control.Monad
-- >>> foldM_ (\o xs -> (+ o) . countToByteOff . snd <$> loadListByteOffMutMemN 4 xs ma o) 2 [[x..] | x <- [1..5] :: [Word8]]
-- >>> freezeMutMem ma
-- [0x00,0x00,0x01,0x02,0x03,0x04,0x02,0x03,0x04,0x05,0x03,0x04,0x05,0x06,0x04,0x05,0x06,0x07,0x05,0x06,0x07,0x08,0x00,0x00]
--
-- @since 0.2.0
loadListByteOffMutMemN ::
     (MemWrite mw, MonadPrim s m, Unbox e)
  => Count e
  -- ^ /elemCount/ - Maximum number of elements to load from list into the memory region
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memCount
  --
  -- Target memory region must have enough memory to perform loading of @elemCount@
  -- elements starting at the @memTargetOff@ offset. For types that also implement
  -- `MemAlloc` this can be described as:
  --
  -- > targetByteCount <- getByteCountMutMem memTarget
  -- > unOff memTargetOff + unCountBytes elemCount <= unCount (targetByteCount - byteCountType @e)
  -> [e] -- ^ /listSource/ - List with elements that should be loaded
  -> mw s -- ^ /memTarget/ - Memory region where to load the elements into
  -> Off Word8
  -- ^ /memTargetOff/ - Offset in number of bytes into target memory where writing will start
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memTargetOff
  --
  -- Once the pointer is advanced by @memTargetOff@ it must still refer to the same memory
  -- region @memTarget@. For types that also implement `MemAlloc` this can be described
  -- as:
  --
  -- > targetByteCount <- getByteCountMutMem memTarget
  -- > unOff memTargetOff <= unCount (targetByteCount - byteCountType @e)
  -> m ([e], Count e)
  -- ^ Leftover part of the @listSource@ if any and the exact count of elements that have been loaded.
loadListByteOffMutMemN count ys mw byteOff = loadListByteOffHelper ys mw byteOff k step
  where
    k = byteOff + countToOff (toByteCount count)
    step = countToOff $ byteCountProxy ys
{-# INLINABLE loadListByteOffMutMemN #-}

-- | Same as `loadListByteOffMutMemN`, but infer the count from number of bytes that is
-- available in the target memory region.
--
-- [Unsafe] When a precondition for the element count @memCount@ is violated then a call
-- to this function can result in heap corruption or failure with a segfault.
--
-- ====__Examples__
--
-- >>> :set -XDataKinds
-- >>> import Primal.Memory
-- >>> ma <- allocZeroMutMem (5 :: Count Char) :: IO (MBytes 'Inc RW)
-- >>> freezeMutMem ma
-- [0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00]
-- >>> loadListByteOffMutMem "Hello World" ma 0
-- (" World",Count {unCount = 5})
-- >>> freezeMutMem ma
-- [0x48,0x00,0x00,0x00,0x65,0x00,0x00,0x00,0x6c,0x00,0x00,0x00,0x6c,0x00,0x00,0x00,0x6f,0x00,0x00,0x00]
-- >>> loadListByteOffMutMem ([0xff,0xff,0xff] :: [Word8]) ma 1
-- ([],Count {unCount = 3})
-- >>> freezeMutMem ma
-- [0x48,0xff,0xff,0xff,0x65,0x00,0x00,0x00,0x6c,0x00,0x00,0x00,0x6c,0x00,0x00,0x00,0x6f,0x00,0x00,0x00]
--
-- @since 0.3.0
loadListByteOffMutMem ::
     (MemAlloc ma, MonadPrim s m, Unbox e)
  => [e] -- ^ /listSource/ - List with elements that should be loaded
  -> ma s -- ^ /memTarget/ - Memory region where to load the elements into
  -> Off Word8
  -- ^ /memTargetOff/ - Offset in number of bytes into target memory where writing will start
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memTargetOff
  --
  -- Once the pointer is advanced by @memTargetOff@ it must still refer to the same memory
  -- region @memTarget@. For types that also implement `MemAlloc` this can be described
  -- as:
  --
  -- > targetByteCount <- getByteCountMutMem memTarget
  -- > unOff memTargetOff <= unCount (targetByteCount - byteCountType @e)
  -> m ([e], Count e)
  -- ^ Leftover part of the @listSource@ if any and the exact count of elements that have been loaded.
loadListByteOffMutMem ys ma byteOff = do
  bCount <- getByteCountMutMem ma
  let k = countToOff bCount - byteOff
      step = countToOff $ byteCountProxy ys
  loadListByteOffHelper ys ma byteOff k step
{-# INLINABLE loadListByteOffMutMem #-}

-- | Same as `loadListByteOffMutMemN`, but works with offset in number of elements instead of
-- bytes.
--
-- [Unsafe] When preconditions for either the offset @memTargetOff@ or the element count
-- @memCount@ is violated then a call to this function can result in heap corruption or
-- failure with a segfault.
--
-- @since 0.2.0
loadListOffMutMemN ::
     (MemWrite mw, MonadPrim s m, Unbox e)
  => Count e
  -- ^ /elemCount/ - Maximum number of elements to load from list into the memory region
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memCount
  --
  -- Target memory region must have enough memory to perform loading of @elemCount@
  -- elements starting at the @memTargetOff@ offset. For types that also implement
  -- `MemAlloc` this can be described as:
  --
  -- > targetCount <- getCountMutMem memTarget
  -- > unOff memTargetOff + unCount elemCount < unCount targetCount
  -> [e] -- ^ /listSource/ - List with elements that should be loaded
  -> mw s -- ^ /memTarget/ - Memory region where to load the elements into
  -> Off e
  -- ^ /memTargetOff/ - Offset in number of elements into target memory where writing will start
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memTargetOff
  --
  -- Once the pointer is advanced by @memTargetOff@ it must still refer to the same memory
  -- region @memTarget@. For types that also implement `MemAlloc` this can be described
  -- as:
  --
  -- > targetCount <- getByteCountMutMem memTarget
  -- > unOff memTargetOff < unCount targetCount
  -> m ([e], Count e)
  -- ^ Leftover part of the @listSource@ if any and the exact count of elements that have been loaded.
loadListOffMutMemN count ys mw off =
  let go []       i = pure ([], toLoadedCount i)
      go a@(x:xs) i
        | i < k = writeOffMutMem mw i x >> go xs (i + 1)
        | otherwise = pure (a, toLoadedCount i)
      k = off + countToOff count
      toLoadedCount i = offToCount (i - off)
  in go ys off
{-# INLINABLE loadListOffMutMemN #-}


-- | Same as `loadListOffMutMemN`, but start loading at @0@ offset.
--
-- [Unsafe] When any precondition for the element count @memCount@ is violated then a call to
-- this function can result in heap corruption or failure with a segfault.
--
-- @since 0.2.0
loadListMutMemN ::
     forall e mw m s. (MemWrite mw, MonadPrim s m, Unbox e)
  => Count e
  -- ^ /elemCount/ - Maximum number of elements to load from list into the memory region
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memCount
  --
  -- Target memory region must have enough memory to perform loading of @elemCount@
  -- elements. For types that also implement `MemAlloc` this can be described as:
  --
  -- > targetCount <- getCountMutMem memTarget
  -- > elemCount <= targetCount
  -> [e] -- ^ /listSource/ - List with elements that should be loaded
  -> mw s -- ^ /memTarget/ - Memory region where to load the elements into
  -> m ([e], Count e)
  -- ^ Leftover part of the @listSource@ if any and the exact count of elements that have been loaded.
loadListMutMemN count xs mw = loadListOffMutMemN count xs mw 0
{-# INLINABLE loadListMutMemN #-}



-- | Same as `loadListMutMemN`, but ignores the result.
--
-- [Unsafe] When any precondition for the element count @memCount@ is violated then a call
-- to this function can result in heap corruption or failure with a segfault.
--
-- @since 0.2.0
loadListMutMemN_ ::
     forall e mw m s. (Unbox e, MemWrite mw, MonadPrim s m)
  => Count e
  -- ^ /elemCount/ - Maximum number of elements to load from list into the memory region
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memCount
  --
  -- Target memory region must have enough memory to perform loading of @elemCount@
  -- elements. For types that also implement `MemAlloc` this can be described as:
  --
  -- > targetCount <- getCountMutMem memTarget
  -- > elemCount <= targetCount
  -> [e] -- ^ /listSource/ - List with elements that should be loaded
  -> mw s -- ^ /memTarget/ - Memory region where to load the elements into
  -> m ()
loadListMutMemN_ (Count n) ys mb =
  let go []     _ = pure ()
      go (x:xs) i = when (i < n) $ writeOffMutMem mb (Off i) x >> go xs (i + 1)
   in go ys 0
{-# INLINABLE loadListMutMemN_ #-}




-- | Same as `loadListOffMutMemN`, but infer the count from number of bytes that is available
-- in the target memory region.
--
-- [Unsafe] When a precondition for the element count @memCount@ is violated then a call
-- to this function can result in heap corruption or failure with a segfault.
--
-- @since 0.3.0
loadListOffMutMem ::
     forall e ma m s. (Unbox e, MemAlloc ma, MonadPrim s m)
  => [e] -- ^ /listSource/ - List with elements that should be loaded
  -> ma s -- ^ /memTarget/ - Memory region where to load the elements into
  -> Off e
  -- ^ /memTargetOff/ - Offset in number of elements into target memory where writing will
  -- start
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memTargetOff
  --
  -- Once the pointer is advanced by @memTargetOff@ it must still refer to the same memory
  -- region @memTarget@. For types that also implement `MemAlloc` this can be described
  -- as:
  --
  -- > targetCount <- getCountMutMem memTarget
  -- > unOff memTargetOff < unCount targetCount
  -> m ([e], Count e)
  -- ^ Leftover part of the @listSource@ if any and the exact count of elements that have been loaded.
loadListOffMutMem ys ma off = getCountMutMem ma >>= \c -> loadListOffMutMemN (c - offToCount off) ys ma off
{-# INLINE loadListOffMutMem #-}


-- | Same as `loadListMutMemN`, but tries to fit as many elements as possible into the mutable
-- memory region starting at the beginning. This operation is always safe.
--
-- ====__Examples__
--
-- >>> import Primal.Memory
-- >>> ma <- allocMutMem (5 :: Count Char) :: IO (MBytes 'Inc RW)
-- >>> loadListMutMem "HelloWorld" ma
-- ("World",Count {unCount = 5})
-- >>> freezeMutMem ma
-- [0x48,0x00,0x00,0x00,0x65,0x00,0x00,0x00,0x6c,0x00,0x00,0x00,0x6c,0x00,0x00,0x00,0x6f,0x00,0x00,0x00]
-- >>> loadListMutMem (replicate 6 (0xff :: Word8)) ma
-- ([],Count {unCount = 6})
-- >>> freezeMutMem ma
-- [0xff,0xff,0xff,0xff,0xff,0xff,0x00,0x00,0x6c,0x00,0x00,0x00,0x6c,0x00,0x00,0x00,0x6f,0x00,0x00,0x00]
--
-- @since 0.3.0
loadListMutMem ::
     forall e ma m s. (Unbox e, MemAlloc ma, MonadPrim s m)
  => [e] -- ^ /listSource/ - List with elements to load
  -> ma s -- ^ /memTarget/ - Mutable region where to load elements from the list
  -> m ([e], Count e)
  -- ^ Leftover part of the @listSource@ if any and the exact count of elements that have been loaded.
loadListMutMem ys ma = getCountMutMem ma >>= \c -> loadListOffMutMemN (c `countForProxyTypeOf` ys) ys ma 0
{-# INLINE loadListMutMem #-}

-- | Same as `loadListMutMem`, but ignores the result. Equivalence as property:
--
-- prop> let c = fromInteger (abs i) :: Count Int in (createZeroMemST_ c (loadListMutMem_ (xs :: [Int])) :: Bytes 'Inc) == createZeroMemST_ c (void . loadListMutMem xs)
--
-- @since 0.2.0
loadListMutMem_ ::
     forall e ma m s. (Unbox e, MemAlloc ma, MonadPrim s m)
  => [e] -- ^ /listSource/ - List with elements to load
  -> ma s -- ^ /memTarget/ - Mutable region where to load elements from the list
  -> m ()
loadListMutMem_ ys mb = getCountMutMem mb >>= \c -> loadListMutMemN_ (c `countForProxyTypeOf` ys) ys mb
{-# INLINE loadListMutMem_ #-}


-- | Convert a memory region to a list of bytes. Equivalent to `Data.ByteString.unpack`
-- for `Data.ByteString.ByteString`
--
-- ====__Example__
--
-- >>> toByteListMem (fromByteListMem [0..10] :: Bytes 'Pin)
-- [0,1,2,3,4,5,6,7,8,9,10]
--
-- @since 0.1.0
toByteListMem ::
     forall ma. MemAlloc ma
  => Frozen ma
  -> [Word8]
toByteListMem = toListMem
{-# INLINE toByteListMem #-}

-- mapMem ::
--      forall e e' mr ma. (MemRead mr, MemAlloc ma, Unbox e, Unbox e')
--   => (e -> e')
--   -> mr
--   -> (Frozen ma, [Word8])
-- mapMem f = undefined


mapByteMem ::
     forall e mr ma. (MemRead mr, MemAlloc ma, Unbox e)
  => (Word8 -> e)
  -> mr
  -> Frozen ma
mapByteMem f = imapByteOffMem (const f)

-- Map an index aware function over memory region
--
-- >>> import Primal.Memory
-- >>> a = fromListMem [1 .. 10 :: Word8] :: Bytes 'Inc
-- >>> a
-- [0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0a]
-- >>> imapByteOffMem (\i e -> (fromIntegral i :: Int8, e + 0xf0)) a :: Bytes 'Pin
-- [0x00,0xf1,0x01,0xf2,0x02,0xf3,0x03,0xf4,0x04,0xf5,0x05,0xf6,0x06,0xf7,0x07,0xf8,0x08,0xf9,0x09,0xfa]
--
-- @since 0.1.0
imapByteOffMem ::
     (MemRead mr, MemAlloc ma, Unbox e) => (Off Word8 -> Word8 -> e) -> mr -> Frozen ma
imapByteOffMem f r = runST $ mapByteOffMemM (\i -> pure . f i) r

-- @since 0.1.0
mapByteMemM ::
     (MemRead mr, MemAlloc ma, MonadPrim s m, Unbox e)
  => (Word8 -> m e)
  -> mr
  -> m (Frozen ma)
mapByteMemM f = mapByteOffMemM (const f)


-- @since 0.1.0
mapByteOffMemM ::
     forall e mr ma m s. (MemRead mr, MemAlloc ma, MonadPrim s m, Unbox e)
  => (Off Word8 -> Word8 -> m e)
  -> mr
  -> m (Frozen ma)
mapByteOffMemM f r = do
  let bc@(Count n) = byteCountMem r
      c = Count n `countForProxyTypeOf` f 0 0
  mem <- allocMutMem c
  _ <- forByteOffMemM_ r 0 bc f
  -- let go i =
  --       when (i < n) $ do
  --         f i (indexByteOffMem r (Off i)) >>=
  --           writeOffMem mem (offAsProxy c (Off i))
  --         go (i + 1)
  -- go 0
  freezeMut mem


-- | Iterate over a region of memory
forByteOffMemM_ ::
     (MemRead mr, MonadPrim s m, Unbox e)
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

{-

loopShort :: Int -> (Int -> a -> Bool) -> (Int -> Int) -> a -> (Int -> a -> a) -> a
loopShort !startAt condition increment !initAcc f = go startAt initAcc
  where
    go !step !acc =
      if condition step acc
        then go (increment step) (f step acc)
        else acc
{-# INLINE loopShort #-}

loopShortM :: Monad m => Int -> (Int -> a -> m Bool) -> (Int -> Int) -> a -> (Int -> a -> m a) -> m a
loopShortM !startAt condition increment !initAcc f = go startAt initAcc
  where
    go !step !acc = do
      shouldContinue <- condition step acc
      if shouldContinue
        then f step acc >>= go (increment step)
        else return acc
{-# INLINE loopShortM #-}



ifoldlShortBytes ::
     (Unbox e)
  => Off e
  -- ^ Initial offset to start at
  -> Count e
  -- ^ Total number of elements to iterate through
  -> (a -> Bool)
  -- ^ Continuation condition applied to an accumulator. When `False` it will terminate
  -- early
  -> (a -> Off e -> e -> a)
  -- ^ Folding function
  -> a
  -- ^ Initial accumulator
  -> Bytes p
  -- ^ Memory region to iterate over
  -> a
ifoldlShortBytes off count g f initAcc mem =
  loopShort (coerce off) cond (+ 1) initAcc inner
  where
    inner !i !acc =
      let ioff = coerce i
       in f acc ioff $! indexOffBytes mem ioff
    {-# INLINE inner #-}
    cond i a = g a && i < k
    {-# INLINE cond #-}
    k = coerce count
{-# INLINE ifoldlShortBytes #-}
-}







{-




ifoldlShortMem ::
     (Unbox e, MemRead mr)
  => Off e
  -- ^ Initial offset to start at
  -> Count e
  -- ^ Total number of elements to iterate through
  -> (a -> Bool)
  -- ^ Continuation condition applied to an accumulator. When `False` it will terminate
  -- early
  -> (a -> Off e -> e -> a)
  -- ^ Folding function
  -> a
  -- ^ Initial accumulator
  -> mr
  -- ^ Memory region to iterate over
  -> a
ifoldlShortMem off count g f initAcc mem = loop initAcc off
  where
    k = countToOff count + off
    loop !acc !i
      | g acc && i < k = loop (f acc i (indexOffMem mem i)) (i + 1)
      | otherwise = acc

  -- loopShort (coerce off) (\i a -> g a && i < k) (+ 1) initAcc $ \ !i !acc ->
  --   let ioff = coerce i
  --    in f acc ioff (indexOffMem mem ioff)
  -- where
  --   k = coerce count
{-# INLINE ifoldlShortMem #-}

ifoldlShortMemM ::
     (Unbox e, MemRead mr, Monad m)
  => Off e
  -- ^ Initial offset to start at
  -> Count e
  -- ^ Total number of elements to iterate through
  -> (a -> m Bool)
  -- ^ Continuation condition applied to an accumulator. When `False` it will terminate
  -- early
  -> (a -> Off e -> e -> m a)
  -- ^ Folding function
  -> a
  -- ^ Initial accumulator
  -> mr
  -- ^ Memory region to iterate over
  -> m a
ifoldlShortMemM off count g f initAcc mem =
  loopShortM (coerce off) (\i a -> (\p -> p && i < k) <$> g a) (+ 1) initAcc $ \ !i !acc ->
    let ioff = coerce i
     in f acc ioff (indexOffMem mem ioff)
  where
    k = coerce count
{-# INLINE ifoldlShortMemM #-}


foldlShortMem ::
     (Unbox e, MemRead mr)
  => Off e
  -- ^ Initial offset to start at
  -> Count e
  -- ^ Total number of elements to iterate through
  -> (a -> Bool)
  -- ^ Continuation condition applied to an accumulator. When `False` it will terminate
  -- early
  -> (a -> e -> a)
  -- ^ Folding function
  -> a
  -- ^ Initial accumulator
  -> mr
  -- ^ Memory region to iterate over
  -> a
foldlShortMem off count g f = ifoldlShortMem off count g (\a _ -> f a)
{-# INLINE foldlShortMem #-}
-}




-- -- | Iterate over a region of memory
-- loopMemM_ ::
--      (MemRead mr, MonadPrim s m, Unbox e)
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
     (MemRead mr1, MemRead mr2, MonadPrim s m, Unbox e)
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
     (MemRead mr1, MemRead mr2, MonadPrim s m, Unbox e1, Unbox e2)
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
--     (Elt f a, Elt f b, MonadPrim s m) => f a s -> (a -> m b) -> m (f b s)

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

instance Show (Bytes p) where
  show b =
    Foldable.foldr' ($) "]" $
    ('[' :) : List.intersperse (',' :) (map (("0x" ++) .) (showsHexMem b))

instance Typeable p => IsList (Bytes p) where
  type Item (Bytes p) = Word8
  fromList = fromListMem
  {-# INLINE fromList #-}
  fromListN n = fromListZeroMemN_ (Count n)
  {-# INLINE fromListN #-}
  toList = toListMem
  {-# INLINE toList #-}

instance Eq (Bytes p) where
  b1 == b2 = isSameBytes b1 b2 || eqByteMem b1 b2
  {-# INLINE (==) #-}


instance Ord (Bytes p) where
  compare b1 b2 =
    compare n (byteCountBytes b2) <> compareByteOffBytes b1 0 b2 0 n
    where
      n = byteCountBytes b1
  {-# INLINE compare #-}

instance Typeable p => Semigroup.Semigroup (Bytes p) where
  (<>) = appendMem
  {-# INLINE (<>) #-}
  sconcat (x :| xs) = concatMem (x:xs)
  {-# INLINE sconcat #-}
  stimes i = cycleMemN (fromIntegral i)
  {-# INLINE stimes #-}

instance Typeable p => Monoid.Monoid (Bytes p) where
  mappend = appendMem
  {-# INLINE mappend #-}
  mconcat = concatMem
  {-# INLINE mconcat #-}
  mempty = emptyMem
  {-# INLINE mempty #-}


-- | A list of `ShowS` which covert bytes to base16 encoded strings. Each element of the list
-- is a function that will convert one byte.
--
-- ====__Example__
--
-- >>> :set -XDataKinds
-- >>> import Primal.Memory
-- >>> concatMap ($ " ") $ showsHexMem (fromListMem [1 :: Int16 .. 15] :: Bytes 'Inc)
-- "01 00 02 00 03 00 04 00 05 00 06 00 07 00 08 00 09 00 0a 00 0b 00 0c 00 0d 00 0e 00 0f 00 "
--
-- @since 0.1.0
showsHexMem :: MemRead mr => mr -> [ShowS]
showsHexMem b = map toHex (toListMem b :: [Word8])
  where
    toHex b8 =
      (if b8 <= 0x0f
         then ('0' :)
         else id) .
      showHex b8

-- | Allocate a new region of memory and Ensure that it is filled with zeros before and
-- after it gets used. `PtrAccess` is not used directly, but instead is used to guarantee
-- that the memory is pinned and its contents will not get moved around by the garbage
-- collector.
--
-- @since 0.3.0
withScrubbedMutMem ::
     forall e ma m a.
     (MonadUnliftPrim RW m, Unbox e, MemAlloc ma, MemPtr ma)
  => Count e
  -> (ma RW -> m a)
  -> m a
withScrubbedMutMem c f = do
  mem <- allocZeroMutMem c
  let _fptr = toMForeignPtrMem mem :: MForeignPtr e RW -- Enforce the `PtrAccess` constraint.
  f mem `ufinally` setMutMem mem 0 (toByteCount c) 0
{-# INLINE withScrubbedMutMem #-}
