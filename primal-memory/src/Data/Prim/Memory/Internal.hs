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
import Data.List.NonEmpty (NonEmpty(..))
import Control.Monad.ST
import Control.Prim.Monad
import Control.Prim.Monad.Unsafe
import Data.Foldable as Foldable
import Data.Prim
import Data.Prim.Memory.Bytes.Internal
import Data.List as List
import Data.Prim.Memory.ByteString
import Data.Prim.Memory.ForeignPtr
import Data.Prim.Memory.Ptr
import Foreign.Prim
import Numeric (showHex)
import qualified Data.Semigroup as Semigroup
import qualified Data.Monoid as Monoid
import Data.Kind

-- | Type class that can be implemented for a read-only immutable data type that provides
-- direct access to memory
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
    -- > 0 <= unOff (toByteOff off)
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
  -- `MBytes`. Source and target can't refer to the same memory chunks, that would of
  -- course mean that the source is not immutable thus imply a violation of some other
  -- invariant elsewhere in the code.
  --
  -- [Unsafe] When a precondition for either of the offsets @memSourceOff@, @memTargetOff@
  -- or the byte count @memCount@ is violated the result is either unpredictable output or
  -- failure with a segfault.
  --
  -- @since 0.1.0
  copyByteOffToMBytesMem ::
       (MonadPrim s m, Prim e)
    => mr -- ^ /memSourceRead/ - Source from where to copy
    -> Off Word8
    -- ^ /memSourceOff/ - Offset into source memory in number of bytes
    --
    -- /__Precondition:__/
    --
    -- > 0 <= unOff memSourceOff <= unCount (byteCountMem memSourceRead - byteCountType @e)
    -> MBytes p s -- ^ /memTargetWrite/ - Target mutable memory
    -> Off Word8 -- ^ /memTargetOff/ -  Offset into target memory in number of bytes
    --
    -- /__Precondition:__/
    --
    -- > 0 <= unOff memTargetOff <= unCount (byteCountMem memTargetWrite - byteCountType @e)
    -> Count e
    -- ^ /memCount/ - Number of elements to copy
    --
    -- /__Precondition:__/
    --
    -- > 0 <= unCount memCount
    --
    -- > fromCount memCount + unOff memSourceOff <= unCount (byteCountMem memSourceRead - byteCountType @e)
    --
    -- > fromCount memCount + unOff memTargetOff <= unCount (byteCountMem memTargetRead - byteCountType @e)
    -> m ()

  -- | Copy contiguous chunk of memory from the read only memory into the target mutable
  -- `MBytes`. Source and target can't refer to the same memory chunks, that would of
  -- course mean that the source is not immutable thus imply a violation of some other
  -- invariant elsewhere in the code.
  --
  -- [Unsafe] When a precondition for either of the offsets @memSourceOff@, @memTargetOff@
  -- or the byte count @memCount@ is violated the result is either unpredictable output or
  -- failure with a segfault.
  --
  -- @since 0.1.0
  copyByteOffToPtrMem ::
       (MonadPrim s m, Prim e)
    => mr
    -> Off Word8
    -> Ptr e
    -> Off Word8
    -> Count e
    -> m ()
  --
  -- @since 0.1.0
  compareByteOffToPtrMem ::
       (MonadPrim s m, Prim e)
    => mr
    -> Off Word8
    -> Ptr e
    -> Off Word8
    -> Count e
    -> m Ordering
  --
  -- @since 0.1.0
  compareByteOffToBytesMem ::
       (MonadPrim s m, Prim e)
    => mr
    -> Off Word8
    -> Bytes p
    -> Off Word8
    -> Count e
    -> m Ordering
  --
  -- @since 0.1.0
  compareByteOffMem ::
       (MemRead mr', Prim e)
    => mr'
    -> Off Word8
    -> mr
    -> Off Word8
    -> Count e
    -> Ordering

-- | Generalized memory allocation and pure/mutable state conversion.
class (MemRead (FrozenMem a), MemWrite a) => MemAlloc a where
  type FrozenMem a = (fa :: Type) | fa -> a

  getByteCountMem :: MonadPrim s m => a s -> m (Count Word8)

  allocByteCountMem :: MonadPrim s m => Count Word8 -> m (a s)

  thawMem :: MonadPrim s m => FrozenMem a -> m (a s)

  freezeMem :: MonadPrim s m => a s -> m (FrozenMem a)

  resizeMem :: (MonadPrim s m, Prim e) => a s -> Count e -> m (a s)
  resizeMem = defaultResizeMem


class MemWrite w where
  readOffMem :: (MonadPrim s m, Prim e) => w s -> Off e -> m e

  readByteOffMem :: (MonadPrim s m, Prim e) => w s -> Off Word8 -> m e

  writeOffMem :: (MonadPrim s m, Prim e) => w s -> Off e -> e -> m ()

  writeByteOffMem :: (MonadPrim s m, Prim e) => w s -> Off Word8 -> e -> m ()

  -- | Source and target can be overlapping memory chunks
  moveByteOffToMBytesMem ::
    (MonadPrim s m, Prim e) => w s -> Off Word8 -> MBytes p s -> Off Word8 -> Count e -> m ()

  -- | Source and target can be overlapping memory chunks
  moveByteOffToPtrMem ::
    (MonadPrim s m, Prim e) => w s -> Off Word8 -> Ptr e -> Off Word8 -> Count e -> m ()

  copyByteOffMem ::
    (MonadPrim s m, MemRead r, Prim e) => r -> Off Word8 -> w s -> Off Word8 -> Count e -> m ()

  moveByteOffMem ::
    (MonadPrim s m, MemWrite w', Prim e) => w' s -> Off Word8 -> w s -> Off Word8 -> Count e -> m ()

  -- TODO: Potential feature for the future implementation. Will require extra function in `Prim`.
  --setByteOffMem :: (MonadPrim s m, Prim e) => w s -> Off Word8 -> Count e -> e -> m ()

  -- | Write the same value into each cell starting at an offset.
  setMem
    :: (MonadPrim s m, Prim e)
    => w s -- ^ Writable memory. Must have enough bytes, at least: (off+count)*(sizeOf e)
    -> Off e -- ^ An offset into writable memory at which element setting should start.
    -> Count e -- ^ Numer of cells to write the elemnt into
    -> e -- ^ Element to write into all memory cells specified by offset and count. Even
         -- if the count is @0@ this element might be still fully evaluated.
    -> m ()


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

-- | A wrapper that adds a phantom state token. It can be use with types that either
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
     (MemWrite w, MonadPrim s m, Prim b) => w s -> Off b -> (b -> b) -> m b
modifyFetchOldMem mem o f = modifyFetchOldMemM mem o (pure . f)
{-# INLINE modifyFetchOldMem #-}


modifyFetchNewMem ::
     (MemWrite w, MonadPrim s m, Prim b) => w s -> Off b -> (b -> b) -> m b
modifyFetchNewMem mem o f = modifyFetchNewMemM mem o (pure . f)
{-# INLINE modifyFetchNewMem #-}


modifyFetchOldMemM ::
     (MemWrite w, MonadPrim s m, Prim b) => w s -> Off b -> (b -> m b) -> m b
modifyFetchOldMemM mem o f = do
  a <- readOffMem mem o
  a <$ (writeOffMem mem o =<< f a)
{-# INLINE modifyFetchOldMemM #-}


modifyFetchNewMemM ::
     (MemWrite w, MonadPrim s m, Prim b) => w s -> Off b -> (b -> m b) -> m b
modifyFetchNewMemM mem o f = do
  a <- readOffMem mem o
  a' <- f a
  a' <$ writeOffMem mem o a'
{-# INLINE modifyFetchNewMemM #-}


defaultResizeMem ::
     (Prim e, MemAlloc a, MonadPrim s m) => a s -> Count e -> m (a s)
defaultResizeMem mem c = do
  let newByteCount = toByteCount c
  oldByteCount <- getByteCountMem mem
  if oldByteCount == newByteCount
    then pure mem
    else do
      newMem <- allocByteCountMem newByteCount
      newMem <$ moveMem mem 0 newMem 0 oldByteCount


-- | Make @n@ copies of supplied region of memory into a contiguous chunk of memory.
cycleMemN :: (MemAlloc a, MemRead r) => Int -> r -> FrozenMem a
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
emptyMem :: MemAlloc a => FrozenMem a
emptyMem = createMemST_ (0 :: Count Word8) (\_ -> pure ())
{-# INLINE emptyMem #-}

-- | A region of memory that hold a single element.
singletonMem ::
     forall e a. (MemAlloc a, Prim e)
  => e
  -> FrozenMem a
singletonMem a = createMemST_ (1 :: Count e) $ \mem -> writeOffMem mem 0 a
{-# INLINE singletonMem #-}

-- | Allocate enough memory for number of elements. Memory is not initialized and may
-- contain garbage. Use `allocZeroMem` if clean memory is needed.
--
-- [Unsafe Count] Negative element count will result in unpredictable behavior
--
-- @since 0.1.0
allocMem :: (MemAlloc a, MonadPrim s m, Prim e) => Count e -> m (a s)
allocMem n = allocByteCountMem (toByteCount n)
{-# INLINE allocMem #-}


-- | Same as `allocMem`, but also use @memset@ to initialize all the new memory to zeros.
--
-- [Unsafe Count] Negative element count will result in unpredictable behavior
--
-- @since 0.1.0
allocZeroMem ::
     (MemAlloc a, MonadPrim s m, Prim e) => Count e -> m (a s)
allocZeroMem n = do
  m <- allocMem n
  m <$ setMem m 0 (toByteCount n) (0 :: Word8)
{-# INLINE allocZeroMem #-}


createMemST :: (MemAlloc a, Prim e) => Count e -> (forall s . a s -> ST s b) -> (b, FrozenMem a)
createMemST n f = runST $ do
  m <- allocMem n
  res <- f m
  i <- freezeMem m
  pure (res, i)
{-# INLINE createMemST #-}

createMemST_ :: (MemAlloc a, Prim e) => Count e -> (forall s . a s -> ST s b) -> FrozenMem a
createMemST_ n f = runST (allocMem n >>= \m -> f m >> freezeMem m)
{-# INLINE createMemST_ #-}

createZeroMemST :: (MemAlloc a, Prim e) => Count e -> (forall s . a s -> ST s b) -> (b, FrozenMem a)
createZeroMemST n f = runST $ do
  m <- allocZeroMem n
  res <- f m
  i <- freezeMem m
  pure (res, i)
{-# INLINE createZeroMemST #-}

createZeroMemST_ :: (MemAlloc a, Prim e) => Count e -> (forall s . a s -> ST s b) -> FrozenMem a
createZeroMemST_ n f = runST (allocZeroMem n >>= \m -> f m >> freezeMem m)
{-# INLINE createZeroMemST_ #-}


copyMem ::
     (MonadPrim s m, MemRead r, MemWrite w, Prim e)
  => r -- ^ Source memory region
  -> Off e -- ^ Offset into the source in number of elements
  -> w s -- ^ Destination memory region
  -> Off e -- ^ Offset into destination in number of elements
  -> Count e -- ^ Number of elements to copy over
  -> m ()
copyMem src srcOff dst dstOff = copyByteOffMem src (toByteOff srcOff) dst (toByteOff dstOff)
{-# INLINE copyMem #-}


moveMem ::
     (MonadPrim s m, MemWrite w1, MemWrite w2, Prim e)
  => w1 s -- ^ Source memory region
  -> Off e -- ^ Offset into the source in number of elements
  -> w2 s -- ^ Destination memory region
  -> Off e -- ^ Offset into destination in number of elements
  -> Count e -- ^ Number of elements to copy over
  -> m ()
moveMem src srcOff dst dstOff = moveByteOffMem src (toByteOff srcOff) dst (toByteOff dstOff)
{-# INLINE moveMem #-}


appendMem :: (MemRead r1, MemRead r2, MemAlloc a) => r1 -> r2 -> FrozenMem a
appendMem r1 r2 =
  createMemST_ (n1 + n2) $ \mem -> do
    copyMem r1 0 mem 0 n1
    copyMem r2 (coerce n1) mem (coerce n1) n2
  where
    n1 = byteCountMem r1
    n2 = byteCountMem r2
{-# INLINABLE appendMem #-}

concatMem :: (MemRead r, MemAlloc a) => [r] -> FrozenMem a
concatMem xs = do
  let c = Foldable.foldl' (\ !acc b -> acc + byteCountMem b) 0 xs
  createMemST_ c $ \mb -> do
    let load i b = do
          let cb@(Count n) = byteCountMem b :: Count Word8
          (i + Off n) <$ copyMem b 0 mb i cb
    foldM_ load 0 xs
{-# INLINABLE concatMem #-}


thawCopyMem ::
     (MemRead r, MemAlloc a, MonadPrim s m, Prim e) => r -> Off e -> Count e -> m (a s)
thawCopyMem a off c = do
  mem <- allocMem c
  mem <$ copyMem a off mem 0 c
{-# INLINE thawCopyMem #-}

freezeCopyMem ::
     (MemAlloc a, MonadPrim s m, Prim e)
  => a s
  -> Off e
  -> Count e
  -> m (FrozenMem a)
freezeCopyMem mem off c = freezeMem mem >>= \r -> thawCopyMem r off c >>= freezeMem
{-# INLINE freezeCopyMem #-}


thawCloneMem :: (MemRead r, MemAlloc a, MonadPrim s m) => r -> m (a s)
thawCloneMem a = thawCopyMem a 0 (byteCountMem a)
{-# INLINE thawCloneMem #-}

freezeCloneMem :: (MemAlloc a, MonadPrim s m) => a s -> m (FrozenMem a)
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
convertMem :: (MemRead r, MemAlloc a) => r -> FrozenMem a
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
     forall e r. (MemRead r, Prim e)
  => r -- ^ Read-only memory type
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
countRemMem :: forall e r. (MemRead r, Prim e) => r -> (Count e, Count Word8)
countRemMem = fromByteCountRem . byteCountMem
{-# INLINE countRemMem #-}

getCountMem :: (MemAlloc r, MonadPrim s m, Prim e) => r s -> m (Count e)
getCountMem = fmap (fromByteCount . coerce) . getByteCountMem
{-# INLINE getCountMem #-}


getCountRemMem :: (MemAlloc r, MonadPrim s m, Prim e) => r s -> m (Count e, Count Word8)
getCountRemMem = fmap (fromByteCountRem . coerce) . getByteCountMem
{-# INLINE getCountRemMem #-}


clone :: (MemAlloc r, MonadPrim s m) => r s -> m (r s)
clone mb = do
  n <- getByteCountMem mb
  mb' <- allocMem n
  mb' <$ moveMem mb 0 mb' 0 n
{-# INLINE clone #-}

eqMem :: (MemRead r1, MemRead r2) => r1 -> r2 -> Bool
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
     (MemRead r1, MemRead r2, Prim e)
  => r1 -- ^ First region of memory
  -> Off e -- ^ Offset in number of elements into the first region
  -> r2 -- ^ Second region of memory
  -> Off e -- ^ Offset in number of elements into the second region
  -> Count e -- ^ Number of elements to compare
  -> Ordering
compareMem r1 off1 r2 off2 = compareByteOffMem r1 (toByteOff off1) r2 (toByteOff off2)
{-# INLINE compareMem #-}

-- | It is only guaranteed to convert the whole memory to a list whenever the size of
-- allocated memory is exactly divisible by the size of the element, otherwise there will
-- be some slack left unaccounted for.
toListMem :: (MemRead r, Prim e) => r -> [e]
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
     forall e r. (MemRead r, Prim e)
  => r
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
foldrCountMem :: (MemRead r, Prim e) => Count e -> (e -> b -> b) -> b -> r -> b
foldrCountMem (Count k) c nil bs = go 0
  where
    go i
      | i == k = nil
      | otherwise =
        let !v = indexOffMem bs (Off i)
         in v `c` go (i + 1)
{-# INLINE[0] foldrCountMem #-}


loadListMemN ::
     (MemWrite r, MonadPrim s m, Prim e)
  => Count e
  -> Count Word8
  -> [e]
  -> r s
  -> m Ordering
loadListMemN (Count n) (Count slack) ys mb = do
  let go [] !i = pure (compare i n <> compare 0 slack)
      go (x:xs) !i
        | i < n = writeOffMem mb (Off i) x >> go xs (i + 1)
        | otherwise = pure GT
  go ys 0
{-# INLINABLE loadListMemN #-}

loadListMemN_ :: (MemWrite r, MonadPrim s m, Prim e) => Count e -> [e] -> r s -> m ()
loadListMemN_ (Count n) ys mb =
  let go [] _     = pure ()
      go (x:xs) i = when (i < n) $ writeOffMem mb (Off i) x >> go xs (i + 1)
   in go ys 0
{-# INLINABLE loadListMemN_ #-}

-- | Returns `EQ` if the full list did fit into the supplied memory chunk exactly.
-- Otherwise it will return either `LT` if the list was smaller than allocated memory or
-- `GT` if the list was bigger than the available memory and did not fit into `MBytes`.
loadListMem :: (MonadPrim s m, MemAlloc r, Prim e) => [e] -> r s -> m Ordering
loadListMem ys mb = do
  (c, slack) <- getCountRemMem mb
  loadListMemN (c `countForProxyTypeOf` ys) slack ys mb
{-# INLINE loadListMem #-}

loadListMem_ :: (MonadPrim s m, MemAlloc r, Prim e) => [e] -> r s -> m ()
loadListMem_ ys mb = do
  c <- getCountMem mb
  loadListMemN_ (c `countForProxyTypeOf` ys) ys mb
{-# INLINE loadListMem_ #-}


fromListMemN :: (MemAlloc a, Prim e) => Count e -> [e] -> (Ordering, FrozenMem a)
fromListMemN n xs = createMemST n (loadListMemN n 0 xs)
{-# INLINE fromListMemN #-}

fromListMemN_ :: (MemAlloc a, Prim e) => Count e -> [e] -> FrozenMem a
fromListMemN_ !n xs = createMemST_ n (loadListMemN_ n xs)
{-# INLINE fromListMemN_ #-}

fromListMem :: (MemAlloc a, Prim e) => [e] -> FrozenMem a
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
fromByteListMem :: MemAlloc a => [Word8] -> FrozenMem a
fromByteListMem = fromListMem
{-# INLINE fromByteListMem #-}

-- | Convert a memory region to a list of bytes. Equivalent to `Data.ByteString.unpack`
-- for `Data.ByteString.ByteString`
--
-- >>> toByteListMem (fromByteListMem [0..10] :: Bytes 'Pin)
-- [0,1,2,3,4,5,6,7,8,9,10]
--
-- @since 0.1.0
toByteListMem :: MemAlloc a => FrozenMem a -> [Word8]
toByteListMem = toListMem
{-# INLINE toByteListMem #-}


mapByteMem :: (MemRead r, MemAlloc a, Prim e) => (Word8 -> e) -> r -> FrozenMem a
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
     (MemRead r, MemAlloc a, Prim e) => (Off Word8 -> Word8 -> e) -> r -> FrozenMem a
mapByteOffMem f r = runST $ mapByteOffMemM (\i -> pure . f i) r

-- @since 0.1.0
mapByteMemM ::
     (MemRead r, MemAlloc a, MonadPrim s m, Prim e)
  => (Word8 -> m e)
  -> r
  -> m (FrozenMem a)
mapByteMemM f = mapByteOffMemM (const f)


-- @since 0.1.0
mapByteOffMemM ::
     (MemRead r, MemAlloc a, MonadPrim s m, Prim e)
  => (Off Word8 -> Word8 -> m e)
  -> r
  -> m (FrozenMem a)
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
     (MemRead r, MonadPrim s m, Prim e)
  => r
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
--      (MemRead r, MonadPrim s m, Prim e)
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
  , mvCount :: {-# UNPACK #-} !(Count Word8)
  , mvMem :: !a
  }

data MMemView a s = MMemView
  { mmvOffset :: {-# UNPACK #-} !(Off Word8)
  , mmvCount :: {-# UNPACK #-} !(Count Word8)
  , mmvMem :: !(a s)
  }

izipWithByteOffMemM_ ::
     (MemRead r1, MemRead r2, MonadPrim s m, Prim e)
  => r1
  -> Off Word8
  -> r2
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
     (MemRead r1, MemRead r2, MonadPrim s m, Prim e1, Prim e2)
  => r1
  -> Off e1
  -> r2
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


-------------------
-- Bytes orphans --
-------------------

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
showsHexMem :: MemRead r => r -> [ShowS]
showsHexMem b = map toHex (toListMem b :: [Word8])
  where
    toHex b8 =
      (if b8 <= 0x0f
         then ('0' :)
         else id) .
      showHex b8

-- | Ensure that memory is filled with zeros before and after it is used.
withScrubbedMem ::
     (MonadUnliftPrim RW m, Prim e, MemAlloc mem)
  => Count e
  -> (mem RW -> m a)
  -> m a
withScrubbedMem c f = do
  mem <- allocZeroMem c
  f mem `finallyPrim` setMem mem 0 (toByteCount c) 0
  where
    finallyPrim m1 m2 = withRunInPrimBase $ \run -> finally (run m1) (run m2)
