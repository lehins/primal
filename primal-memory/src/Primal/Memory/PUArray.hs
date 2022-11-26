{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Primal.Memory.PUArray
-- Copyright   : (c) Alexey Kuleshevich 2020-2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module Primal.Memory.PUArray (
  PUArray (..),
  PUMArray (..),
  Pinned (..),
  singletonPUArray,
  fromBytesPUArray,
  toBytesPUArray,
  fromUArrayPUArray,
  toUArrayPUArray,
  castPUArray,
  fromMBytesPUMArray,
  toMBytesPUMArray,
  fromUMArrayPUMArray,
  toUMArrayPUMArray,
  castPUMArray,
  allocPUMArray,
  allocPinnedPUMArray,
  allocAlignedPUMArray,
  allocUnpinnedPUMArray,
  shrinkPUMArray,
  resizePUMArray,
  reallocPUMArray,
  isPinnedPUArray,
  isPinnedPUMArray,
  thawPUArray,
  freezePUMArray,
  sizePUArray,
  getSizeOfPUMArray,
  readPUMArray,
  writePUMArray,
  setPUMArray,
  copyPUArrayToPUMArray,
  movePUMArrayToPUMArray,
  module Primal.Element.Unbox,
) where

import Primal.Array (Size (..), UArray (..), UMArray (..), compareWithST, eqWithST)
import Primal.Element.Unbox
import Primal.Element.Unlift
import Primal.Eval
import Primal.Foreign
import Primal.Memory.Bytes
import Primal.Memory.Fold
import Primal.Memory.ForeignPtr
import Primal.Memory.Internal
import Primal.Monad
import Primal.Mutable.Eq
import Primal.Mutable.Freeze
import Primal.Mutable.Ord

-- | An immutable array with elements of type @e@. This is a very similar array
-- to `UArray`, except it allows tracking pinnedness at the type level in the
-- same way `Bytes` does it.
newtype PUArray (p :: Pinned) e = PUArray (Bytes p)
  deriving (NFData, Semigroup, Monoid, MemRead)

type role PUArray nominal nominal

instance Unlift (PUArray p e) where
  type UnliftIso (PUArray p e) = Bytes p

instance (Unbox e, Eq e) => Eq (PUArray p e) where
  (==) = eqMem @e
  {-# INLINE (==) #-}

instance (Unbox e, Eq e) => MutEq (PUMArray p e) where
  eqMutST m1 m2 = eqWithST isSamePUMArray getSizeOfPUMArray readPUMArray m1 m2
  {-# INLINE eqMutST #-}

instance (Unbox e, Ord e) => Ord (PUArray p e) where
  compare = compareMem @e
  {-# INLINE compare #-}

instance (Unbox e, Ord e) => MutOrd (PUMArray p e) where
  compareMutST m1 m2 = compareWithST isSamePUMArray getSizeOfPUMArray readPUMArray m1 m2
  {-# INLINE compareMutST #-}

-- | A mutable array with elements of type @e@
newtype PUMArray (p :: Pinned) e s = PUMArray (MBytes p s)
  deriving (NFData, MutNFData, MutFreeze, MemWrite)

type role PUMArray nominal nominal nominal

instance MutUnlift (PUMArray p e) where
  type MutUnliftIso (PUMArray p e) = MBytes p

type instance Frozen (PUMArray p e) = PUArray p e

instance MemForeignPtr (PUMArray 'Pin e) where
  toMForeignPtrMem = toMForeignPtrMBytes . toMBytesPUMArray
  {-# INLINE toMForeignPtrMem #-}

instance MemPtr (PUMArray 'Pin e) where
  withPtrMutMemST mb = withPtrMBytes (toMBytesPUMArray mb)
  {-# INLINE withPtrMutMemST #-}
  withNoHaltPtrMutMemST mb = withNoHaltPtrMBytes (toMBytesPUMArray mb)
  {-# INLINE withNoHaltPtrMutMemST #-}

instance Typeable p => MemAlloc (PUMArray p e) where
  allocMutMemST = fmap fromMBytesPUMArray . allocMBytes
  {-# INLINE allocMutMemST #-}
  allocPinnedMutMemST = fmap fromMBytesPUMArray . allocPinnedMutMemST
  {-# INLINE allocPinnedMutMemST #-}
  allocAlignedPinnedMutMemST = fmap fromMBytesPUMArray . allocAlignedPinnedMutMemST
  {-# INLINE allocAlignedPinnedMutMemST #-}

instance Typeable p => MemFreeze (PUMArray p e) where
  getByteCountMutMemST = getByteCountMutMem . toMBytesPUMArray
  {-# INLINE getByteCountMutMemST #-}
  reallocMutMemST mba = fmap fromMBytesPUMArray . reallocMBytes (toMBytesPUMArray mba)
  {-# INLINE reallocMutMemST #-}

instance (Typeable p, Unbox e) => IsList (PUArray p e) where
  type Item (PUArray p e) = e
  fromList = fromListMem
  fromListN n = fromListZeroMemN_ (Count n)
  toList = toListMem

instance Typeable p => IsString (PUArray p Char) where
  fromString = fromListMem

instance (Show e, Unbox e) => Show (PUArray p e) where
  show = show . toListPUArray

toListPUArray :: Unbox e => PUArray p e -> [e]
toListPUArray = toListMem

castPUArray :: PUArray p e' -> PUArray p e
castPUArray = coerce

-- | /O(1)/ - Cast `PUArray` to `UArray`
--
-- @since 1.0.0
toUArrayPUArray :: PUArray p e -> UArray e
toUArrayPUArray pa = UArray (toByteArray# (toBytesPUArray pa))
{-# INLINE toUArrayPUArray #-}

-- | /O(1)/ - Cast `UArray` to `PUArray`
--
-- @since 1.0.0
fromUArrayPUArray :: UArray e -> PUArray 'Inc e
fromUArrayPUArray (UArray ba#) = fromBytesPUArray (fromByteArray# ba#)
{-# INLINE fromUArrayPUArray #-}

fromBytesPUArray :: Bytes p -> PUArray p e
fromBytesPUArray = coerce

toBytesPUArray :: PUArray p e -> Bytes p
toBytesPUArray = coerce

castPUMArray :: PUMArray p e' s -> PUMArray p e s
castPUMArray = coerce

fromMBytesPUMArray :: MBytes p s -> PUMArray p e s
fromMBytesPUMArray = coerce

toMBytesPUMArray :: PUMArray p e s -> MBytes p s
toMBytesPUMArray = coerce

-- | /O(1)/ - Cast `PUMArray` to `UMArray`
--
-- @since 1.0.0
toUMArrayPUMArray :: PUMArray p e s -> UMArray e s
toUMArrayPUMArray pa = UMArray (toMutableByteArray# (toMBytesPUMArray pa))
{-# INLINE toUMArrayPUMArray #-}

-- | /O(1)/ - Cast `UMArray` to `PUMArray`
--
-- @since 1.0.0
fromUMArrayPUMArray :: UMArray e s -> PUMArray 'Inc e s
fromUMArrayPUMArray (UMArray mba#) = fromMBytesPUMArray (fromMutableByteArray# mba#)
{-# INLINE fromUMArrayPUMArray #-}

-- | /O(1)/ - Compare pointers for two mutable arrays and see if they refer to the exact same one.
--
-- Documentation for utilized primop: `sameMutableByteArray#`.
--
-- @since 1.0.0
isSamePUMArray :: forall a b p1 p2 s. PUMArray p1 a s -> PUMArray p2 b s -> Bool
isSamePUMArray (PUMArray mb1) (PUMArray mb2) = isSameMBytes mb1 mb2
{-# INLINE isSamePUMArray #-}

sizePUArray :: forall e p. Unbox e => PUArray p e -> Size
sizePUArray = (coerce :: Count e -> Size) . countBytes . toBytesPUArray
{-# INLINE sizePUArray #-}

getSizeOfPUMArray :: forall e p m s. (Primal s m, Unbox e) => PUMArray p e s -> m Size
getSizeOfPUMArray = fmap (coerce :: Count e -> Size) . getCountMBytes . toMBytesPUMArray
{-# INLINE getSizeOfPUMArray #-}

singletonPUArray
  :: forall e p
   . (Typeable p, Unbox e)
  => e
  -> PUArray p e
singletonPUArray e = singletonMem e
{-# INLINE singletonPUArray #-}

allocPUMArray
  :: forall e p m s. (Typeable p, Unbox e, Primal s m) => Size -> m (PUMArray p e s)
allocPUMArray sz = fromMBytesPUMArray <$> allocMBytes (coerce sz :: Count e)
{-# INLINE allocPUMArray #-}

allocUnpinnedPUMArray :: forall e m s. (Primal s m, Unbox e) => Size -> m (PUMArray 'Inc e s)
allocUnpinnedPUMArray sz = fromMBytesPUMArray <$> allocUnpinnedMBytes (coerce sz :: Count e)
{-# INLINE allocUnpinnedPUMArray #-}

allocPinnedPUMArray :: forall e m s. (Primal s m, Unbox e) => Size -> m (PUMArray 'Pin e s)
allocPinnedPUMArray sz = fromMBytesPUMArray <$> allocPinnedMBytes (coerce sz :: Count e)
{-# INLINE allocPinnedPUMArray #-}

allocAlignedPUMArray
  :: (Primal s m, Unbox e)
  => Count e
  -- ^ Size in number of bytes
  -> m (PUMArray 'Pin e s)
allocAlignedPUMArray = fmap fromMBytesPUMArray . allocAlignedPinnedMBytes
{-# INLINE allocAlignedPUMArray #-}

freezePUMArray :: Primal s m => PUMArray p e s -> m (PUArray p e)
freezePUMArray = fmap fromBytesPUArray . freezeMBytes . toMBytesPUMArray
{-# INLINE freezePUMArray #-}

thawPUArray :: Primal s m => PUArray p e -> m (PUMArray p e s)
thawPUArray = fmap fromMBytesPUMArray . thawBytes . toBytesPUArray
{-# INLINE thawPUArray #-}

-- | Shrink mutable bytes to new specified count of elements. The new count must be less
-- than or equal to the current count as reported by `getCountPUMArray`.
shrinkPUMArray
  :: forall e p m s
   . (Primal s m, Unbox e)
  => PUMArray p e s
  -> Size
  -> m ()
shrinkPUMArray mba sz = shrinkMBytes (toMBytesPUMArray mba) (coerce sz :: Count e)
{-# INLINE shrinkPUMArray #-}

-- | Attempt to resize mutable bytes in place.
--
-- * New bytes might be allocated, with the copy of an old one.
-- * Old references should not be kept around to allow GC to claim it
-- * Old references should not be used to avoid undefined behavior
resizePUMArray
  :: forall e p m s
   . (Primal s m, Unbox e)
  => PUMArray p e s
  -> Size
  -> m (PUMArray 'Inc e s)
resizePUMArray mba sz =
  fromMBytesPUMArray
    <$> resizeMBytes (toMBytesPUMArray mba) (coerce sz :: Count e)
{-# INLINE resizePUMArray #-}

reallocPUMArray
  :: forall e p m s
   . (Primal s m, Typeable p, Unbox e)
  => PUMArray p e s
  -> Size
  -> m (PUMArray p e s)
reallocPUMArray mba sz =
  fromMBytesPUMArray
    <$> reallocMBytes (toMBytesPUMArray mba) (coerce sz :: Count e)
{-# INLINEABLE reallocPUMArray #-}

isPinnedPUArray :: PUArray p e -> Bool
isPinnedPUArray (PUArray b) = isPinnedBytes b
{-# INLINE isPinnedPUArray #-}

isPinnedPUMArray :: PUMArray p e s -> Bool
isPinnedPUMArray (PUMArray mb) = isPinnedMBytes mb
{-# INLINE isPinnedPUMArray #-}

readPUMArray :: (Primal s m, Unbox e) => PUMArray p e s -> Int -> m e
readPUMArray (PUMArray mb) = readOffMBytes mb . coerce
{-# INLINE readPUMArray #-}

writePUMArray :: (Primal s m, Unbox e) => PUMArray p e s -> Int -> e -> m ()
writePUMArray (PUMArray mb) o = writeOffMBytes mb (coerce o)
{-# INLINE writePUMArray #-}

setPUMArray
  :: forall e p m s
   . (Primal s m, Unbox e)
  => PUMArray p e s
  -- ^ Chunk of memory to fill
  -> Int
  -- ^ Offset in number of elements
  -> Size
  -- ^ Number of cells to fill
  -> e
  -- ^ A value to fill the cells with
  -> m ()
setPUMArray (PUMArray mb) off sz = setMBytes mb (coerce off) (coerce sz)
{-# INLINE setPUMArray #-}

copyPUArrayToPUMArray
  :: forall e p m s
   . (Primal s m, Unbox e)
  => PUArray p e
  -> Int
  -> PUMArray p e s
  -> Int
  -> Size
  -> m ()
copyPUArrayToPUMArray ba srcOff mba dstOff sz =
  copyMem ba (coerce srcOff) mba (coerce dstOff) (coerce sz `countForProxyTypeOf` ba)
{-# INLINE copyPUArrayToPUMArray #-}

movePUMArrayToPUMArray
  :: forall e p m s
   . (Primal s m, Unbox e)
  => PUMArray p e s
  -> Int
  -> PUMArray p e s
  -> Int
  -> Size
  -> m ()
movePUMArrayToPUMArray ba srcOff mba dstOff sz =
  moveMutMem ba (coerce srcOff) mba (coerce dstOff) (coerce sz :: Count e)
{-# INLINE movePUMArrayToPUMArray #-}

-- toPtrPUArray :: PUArray Pin e -> Ptr e
-- toPtrPUArray (PUArray ba#) = Ptr (byteArrayContents# ba#)
-- {-# INLINE toPtrPUArray #-}

-- toPtrPUMArray :: PUMArray Pin e s -> Ptr e
-- toPtrPUMArray (PUMArray mba#) = Ptr (mutablePUArrayContents# mba#)
-- {-# INLINE toPtrPUMArray #-}

-- -- | Pointer access to immutable `PUArray` should be for read only purposes, but it is
-- -- not enforced. Any mutation will break referential transparency
-- withPtrPUArray :: Primal s m => PUArray Pin e -> (Ptr e -> m b) -> m b
-- withPtrPUArray b f = do
--   res <- f (toPtrPUArray b)
--   res <$ touch b
-- {-# INLINE withPtrPUArray #-}

-- -- | Same as `withPtrPUArray`, but is suitable for actions that don't terminate
-- withNoHaltPtrPUArray :: UnliftPrimal s m => PUArray Pin e -> (Ptr e -> m b) -> m b
-- withNoHaltPtrPUArray b f = withAliveUnliftPrim b $ f (toPtrPUArray b)
-- {-# INLINE withNoHaltPtrPUArray #-}

-- withPtrPUMArray :: Primal s m => PUMArray Pin e s -> (Ptr e -> m b) -> m b
-- withPtrPUMArray mb f = do
--   res <- f (toPtrPUMArray mb)
--   res <$ touch mb
-- {-# INLINE withPtrPUMArray #-}

-- withNoHaltPtrPUMArray :: UnliftPrimal s m => PUMArray Pin e s -> (Ptr e -> m b) -> m b
-- withNoHaltPtrPUMArray mb f = withAliveUnliftPrim mb $ f (toPtrPUMArray mb)
-- {-# INLINE withNoHaltPtrPUMArray #-}

-- -- -- | Check if two byte arrays refer to pinned memory and compare their pointers.
-- -- isSamePUArray :: PUArray p1 e -> PUArray p2 e -> Bool
-- -- isSamePUArray (PUArray b1#) (PUArray b2#) = isTrue# (isSameByteArray# b1# b2#)
-- -- {-# INLINE[0] isSamePUArray #-}
-- -- {-# RULES
-- -- "isSamePinnedPUArray" isSamePUArray = isSamePinnedPUArray
-- --   #-}

-- -- -- | Perform pointer equality on pinned `PUArray`.
-- -- isSamePinnedPUArray :: PUArray Pin e -> PUArray Pin e -> Bool
-- -- isSamePinnedPUArray pb e1 pb2 = toPtrPUArray pb e1 == toPtrPUArray pb e2
-- -- {-# INLINE isSamePinnedPUArray #-}

-- -- byteStringConvertError :: String -> a
-- -- byteStringConvertError msg = error $ "Cannot convert 'ByteString'. " ++ msg
-- -- {-# NOINLINE byteStringConvertError #-}
