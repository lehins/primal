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
-- Module      : Primal.Memory.UVector
-- Copyright   : (c) Alexey Kuleshevich 2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module Primal.Memory.UVector
  ( UVector (..)
  , UMVector (..)
  , Pinned (..)
  -- , fromBytesUVector
  -- , toBytesUVector
  -- , fromUArrayUVector
  -- , toUArrayUVector
  -- , castUVector
  -- , fromMBytesUMVector
  -- , toMBytesUMVector
  -- , fromUMArrayUMVector
  -- , toUMArrayUMVector
  -- , castUMVector
  -- , allocUMVector
  -- , allocPinnedUMVector
  -- , allocAlignedUMVector
  -- , allocUnpinnedUMVector
  -- , shrinkUMVector
  -- , resizeUMVector
  -- , reallocUMVector
  -- , isPinnedUVector
  -- , isPinnedUMVector

  -- , thawUVector
  -- , freezeUMVector
  -- , sizeUVector
  -- , getSizeOfUMVector
  -- , readUMVector
  -- , writeUMVector

  -- , setUMVector
  -- , copyUVectorToUMVector
  -- , moveUMVectorToUMVector
  , module Primal.Element.Unbox
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

-- | An immutable vector with unboxed elements of type @e@ and support for constant time slicing.
data UVector (p :: Pinned) e = UVector
  { uvOff :: {-# UNPACK #-} !(Off Word8)
  -- ^ Offset in number of bytes into the memory region where vector begins
  , uvSize :: {-# UNPACK #-} !(Count e)
  -- ^ Number of elements of type __e__ in the vector
  , uvBytes :: {-# UNPACK #-} !(Bytes p)
  -- ^ Region of memory with the actual data
  }

type role UVector nominal nominal

instance MemRead (UVector p e)

instance (Unbox e, Eq e) => Eq (UVector p e) where
  (==) = eqMem @e
  {-# INLINE (==) #-}

-- instance (Unbox e, Eq e) => MutEq (UMVector p e) where
--   eqMutST m1 m2 = eqWithST isSameUMVector getSizeOfUMVector readUMVector m1 m2
--   {-# INLINE eqMutST #-}

instance (Unbox e, Ord e) => Ord (UVector p e) where
  compare = compareMem @e
  {-# INLINE compare #-}

-- instance (Unbox e, Ord e) => MutOrd (UMVector p e) where
--   compareMutST m1 m2 = compareWithST isSameUMVector getSizeOfUMVector readUMVector m1 m2
--   {-# INLINE compareMutST #-}

-- | A mutable vector with unboxed elements of type @e@ and support for constant time slicing.
data UMVector (p :: Pinned) e s = UMVector
  { umvOff :: {-# UNPACK #-} !(Off Word8)
  , umvMBytes :: {-# UNPACK #-} !(MBytes p s)
  }

type role UMVector nominal nominal nominal

type instance Frozen (UMVector p e) = UVector p e

-- instance MemPtr (UMVector 'Pin e) where
--   toMForeignPtrMem = toMForeignPtrMBytes . toMBytesUMVector
--   {-# INLINE toMForeignPtrMem #-}
--   withPtrMutMemST mb = withPtrMBytes (toMBytesUMVector mb)
--   {-# INLINE withPtrMutMemST #-}
--   withNoHaltPtrMutMemST mb = withNoHaltPtrMBytes (toMBytesUMVector mb)
--   {-# INLINE withNoHaltPtrMutMemST #-}

-- instance Typeable p => MemAlloc (UMVector p e) where
--   getByteCountMutMemST = getByteCountMutMem . toMBytesUMVector
--   {-# INLINE getByteCountMutMemST #-}
--   allocMutMemST = fmap fromMBytesUMVector . allocMBytes
--   {-# INLINE allocMutMemST #-}
--   allocPinnedMutMemST = fmap fromMBytesUMVector . allocPinnedMutMemST
--   {-# INLINE allocPinnedMutMemST #-}
--   allocAlignedPinnedMutMemST = fmap fromMBytesUMVector . allocAlignedPinnedMutMemST
--   {-# INLINE allocAlignedPinnedMutMemST #-}
--   reallocMutMemST mba = fmap fromMBytesUMVector . reallocMBytes (toMBytesUMVector mba)
--   {-# INLINE reallocMutMemST #-}

-- instance (Typeable p, Unbox e) => IsList (UVector p e) where
--   type Item (UVector p e) = e
--   fromList = fromListMem
--   fromListN n = fromListZeroMemN_ (Count n)
--   toList = toListMem

-- instance Typeable p => IsString (UVector p Char) where
--   fromString = fromListMem

-- instance (Show e, Unbox e) => Show (UVector p e) where
--   show = show . toListUVector

-- toListUVector :: Unbox e => UVector p e -> [e]
-- toListUVector = toListMem

-- castUVector :: UVector p e' -> UVector p e
-- castUVector = coerce

-- -- | /O(1)/ - Cast `UVector` to `UArray`
-- --
-- -- @since 0.3.0
-- toUArrayUVector :: UVector p e -> UArray e
-- toUArrayUVector pa = UArray (toByteArray# (toBytesUVector pa))
-- {-# INLINE toUArrayUVector #-}

-- -- | /O(1)/ - Cast `UArray` to `UVector`
-- --
-- -- @since 0.3.0
-- fromUArrayUVector :: UArray e -> UVector 'Inc e
-- fromUArrayUVector (UArray ba#) = fromBytesUVector (fromByteArray# ba#)
-- {-# INLINE fromUArrayUVector #-}

fromBytesUVector :: Unbox e => Bytes p -> UVector p e
fromBytesUVector b = UVector 0 (countMem b) b

fromMBytesUMVector :: MBytes p s -> UMVector p e s
fromMBytesUMVector = UMVector 0

-- -- | /O(1)/ - Cast `UMVector` to `UMArray`
-- --
-- -- @since 0.3.0
-- toUMArrayUMVector :: UMVector p e s -> UMArray e s
-- toUMArrayUMVector pa = UMArray (toMutableByteArray# (toMBytesUMVector pa))
-- {-# INLINE toUMArrayUMVector #-}

-- -- | /O(1)/ - Cast `UMArray` to `UMVector`
-- --
-- -- @since 0.3.0
-- fromUMArrayUMVector :: UMArray e s -> UMVector 'Inc e s
-- fromUMArrayUMVector (UMArray mba#) = fromMBytesUMVector (fromMutableByteArray# mba#)
-- {-# INLINE fromUMArrayUMVector #-}

-- | /O(1)/ - Compare pointers for two mutable arrays and see if they refer to
-- the exact same one. Also ensures that the slicing of the underlying memory
-- region matches exactly.
--
-- @since 1.0.0
isSameUMVector
  :: forall e1 e2 p1 p2 s
   . (Unbox e1, Unbox e2)
  => UMVector p1 e1 s
  -> UMVector p2 e2 s
  -> Bool
isSameUMVector (UMVector o1 mb1) (UMVector o2 mb2) = isSameMBytes mb1 mb2 && o1 == o2
{-# INLINE isSameUMVector #-}

sizeUVector :: forall e p. Unbox e => UVector p e -> Size
sizeUVector = (coerce :: Count e -> Size) . uvSize
{-# INLINE sizeUVector #-}

getSizeOfUMVector :: forall e p m s. (Unbox e, Primal s m) => UMVector p e s -> m Size
getSizeOfUMVector umv = do
  bc <- getByteCountMBytes $ umvMBytes umv
  pure $ (coerce :: Count e -> Size) $ fromByteCount (bc - offToCount (umvOff umv))
{-# INLINE getSizeOfUMVector #-}

allocUMVector
  :: forall e p m s. (Typeable p, Unbox e, Primal s m) => Size -> m (UMVector p e s)
allocUMVector sz = fromMBytesUMVector <$> allocMBytes (coerce sz :: Count e)
{-# INLINE allocUMVector #-}

newRawUnpinnedUMVector :: forall e m s. (Primal s m, Unbox e) => Size -> m (UMVector 'Inc e s)
newRawUnpinnedUMVector sz = UMVector 0 <$> allocUnpinnedMBytes (coerce sz :: Count e)
{-# INLINE newRawUnpinnedUMVector #-}

newRawPinnedUMVector :: forall e m s. (Primal s m, Unbox e) => Size -> m (UMVector 'Pin e s)
newRawPinnedUMVector sz = UMVector 0 <$> allocPinnedMBytes (coerce sz :: Count e)
{-# INLINE newRawPinnedUMVector #-}

newRawAlignedPinnedUMVector
  :: forall e m s
   . (Primal s m, Unbox e)
  => Size
  -> m (UMVector 'Pin e s)
newRawAlignedPinnedUMVector sz = UMVector 0 <$> allocAlignedPinnedMBytes (coerce sz :: Count e)
{-# INLINE newRawAlignedPinnedUMVector #-}

freezeUMVector
  :: forall e p m s
   . (Unbox e, Primal s m)
  => UMVector p e s
  -> m (UVector p e)
freezeUMVector umv = do
  sz <- getSizeOfUMVector umv
  UVector (umvOff umv) (coerce sz) <$> freezeMBytes (umvMBytes umv)
{-# INLINE freezeUMVector #-}

thawCloneUVector
  :: forall e p m s
   . (Unbox e, Typeable p, Primal s m)
  => UVector p e
  -> m (UMVector p e s)
thawCloneUVector (UVector o c b) = UMVector 0 <$> thawCloneSliceBytes b o (toByteCount c)
{-# INLINE thawCloneUVector #-}

-- thawCloneUVector ::
--      forall e p m s. (Unbox e, Typeable p, Primal s m)
--   => UVector p e
--   -> m (UMVector p e s)
-- thawCloneUVector (UVector o c b) = UMVector 0 <$> thawCloneSliceMem b o (toByteCount c)
-- {-# INLINE thawCloneUVector #-}

-- -- | Shrink mutable bytes to new specified count of elements. The new count must be less
-- -- than or equal to the current count as reported by `getCountUMVector`.
-- shrinkUMVector ::
--      forall e p m s. (Primal s m, Unbox e)
--   => UMVector p e s
--   -> Size
--   -> m ()
-- shrinkUMVector mba sz = shrinkMBytes (toMBytesUMVector mba) (coerce sz :: Count e)
-- {-# INLINE shrinkUMVector #-}

-- -- | Attempt to resize mutable bytes in place.
-- --
-- -- * New bytes might be allocated, with the copy of an old one.
-- -- * Old references should not be kept around to allow GC to claim it
-- -- * Old references should not be used to avoid undefined behavior
-- resizeUMVector ::
--      forall e p m s. (Primal s m, Unbox e)
--   => UMVector p e s
--   -> Size
--   -> m (UMVector 'Inc e s)
-- resizeUMVector mba sz =
--   fromMBytesUMVector <$>
--   resizeMBytes (toMBytesUMVector mba) (coerce sz :: Count e)
-- {-# INLINE resizeUMVector #-}

-- reallocUMVector ::
--      forall e p m s. (Primal s m, Typeable p,  Unbox e)
--   => UMVector p e s
--   -> Size
--   -> m (UMVector p e s)
-- reallocUMVector mba sz =
--   fromMBytesUMVector <$>
--   reallocMBytes (toMBytesUMVector mba) (coerce sz :: Count e)
-- {-# INLINABLE reallocUMVector #-}

-- isPinnedUVector :: UVector p e -> Bool
-- isPinnedUVector (UVector b) = isPinnedBytes b
-- {-# INLINE isPinnedUVector #-}

-- isPinnedUMVector :: UMVector p e s -> Bool
-- isPinnedUMVector (UMVector mb) = isPinnedMBytes mb
-- {-# INLINE isPinnedUMVector #-}

-- readUMVector :: (Primal s m, Unbox e) => UMVector p e s -> Int -> m e
-- readUMVector (UMVector mb) = readOffMBytes mb . coerce
-- {-# INLINE readUMVector #-}

-- writeUMVector :: (Primal s m, Unbox e) => UMVector p e s -> Int -> e -> m ()
-- writeUMVector (UMVector mb) o = writeOffMBytes mb (coerce o)
-- {-# INLINE writeUMVector #-}

-- setUMVector ::
--      forall e p m s. (Primal s m, Unbox e)
--   => UMVector p e s -- ^ Chunk of memory to fill
--   -> Int -- ^ Offset in number of elements
--   -> Size -- ^ Number of cells to fill
--   -> e -- ^ A value to fill the cells with
--   -> m ()
-- setUMVector (UMVector mb) off sz = setMBytes mb (coerce off) (coerce sz)
-- {-# INLINE setUMVector #-}

-- copyUVectorToUMVector ::
--      forall e p m s. (Primal s m, Unbox e)
--   => UVector p e
--   -> Int
--   -> UMVector p e s
--   -> Int
--   -> Size
--   -> m ()
-- copyUVectorToUMVector ba srcOff mba dstOff sz =
--   copyMem ba (coerce srcOff) mba (coerce dstOff) (coerce sz `countForProxyTypeOf` ba)
-- {-# INLINE copyUVectorToUMVector #-}

-- moveUMVectorToUMVector ::
--      forall e p m s. (Primal s m, Unbox e)
--   => UMVector p e s
--   -> Int
--   -> UMVector p e s
--   -> Int
--   -> Size
--   -> m ()
-- moveUMVectorToUMVector ba srcOff mba dstOff sz =
--   moveMutMem ba (coerce srcOff) mba (coerce dstOff) (coerce sz :: Count e)
-- {-# INLINE moveUMVectorToUMVector #-}

-- toPtrUVector :: UVector Pin e -> Ptr e
-- toPtrUVector (UVector ba#) = Ptr (byteArrayContents# ba#)
-- {-# INLINE toPtrUVector #-}

-- toPtrUMVector :: UMVector Pin e s -> Ptr e
-- toPtrUMVector (UMVector mba#) = Ptr (mutableUVectorContents# mba#)
-- {-# INLINE toPtrUMVector #-}

-- -- | Pointer access to immutable `UVector` should be for read only purposes, but it is
-- -- not enforced. Any mutation will break referential transparency
-- withPtrUVector :: Primal s m => UVector Pin e -> (Ptr e -> m b) -> m b
-- withPtrUVector b f = do
--   res <- f (toPtrUVector b)
--   res <$ touch b
-- {-# INLINE withPtrUVector #-}

-- -- | Same as `withPtrUVector`, but is suitable for actions that don't terminate
-- withNoHaltPtrUVector :: UnliftPrimal s m => UVector Pin e -> (Ptr e -> m b) -> m b
-- withNoHaltPtrUVector b f = withAliveUnliftPrim b $ f (toPtrUVector b)
-- {-# INLINE withNoHaltPtrUVector #-}

-- withPtrUMVector :: Primal s m => UMVector Pin e s -> (Ptr e -> m b) -> m b
-- withPtrUMVector mb f = do
--   res <- f (toPtrUMVector mb)
--   res <$ touch mb
-- {-# INLINE withPtrUMVector #-}

-- withNoHaltPtrUMVector :: UnliftPrimal s m => UMVector Pin e s -> (Ptr e -> m b) -> m b
-- withNoHaltPtrUMVector mb f = withAliveUnliftPrim mb $ f (toPtrUMVector mb)
-- {-# INLINE withNoHaltPtrUMVector #-}

-- -- -- | Check if two byte arrays refer to pinned memory and compare their pointers.
-- -- isSameUVector :: UVector p1 e -> UVector p2 e -> Bool
-- -- isSameUVector (UVector b1#) (UVector b2#) = isTrue# (isSameByteArray# b1# b2#)
-- -- {-# INLINE[0] isSameUVector #-}
-- -- {-# RULES
-- -- "isSamePinnedUVector" isSameUVector = isSamePinnedUVector
-- --   #-}

-- -- -- | Perform pointer equality on pinned `UVector`.
-- -- isSamePinnedUVector :: UVector Pin e -> UVector Pin e -> Bool
-- -- isSamePinnedUVector pb e1 pb2 = toPtrUVector pb e1 == toPtrUVector pb e2
-- -- {-# INLINE isSamePinnedUVector #-}

-- -- byteStringConvertError :: String -> a
-- -- byteStringConvertError msg = error $ "Cannot convert 'ByteString'. " ++ msg
-- -- {-# NOINLINE byteStringConvertError #-}
