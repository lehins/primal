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
-- Module      : Data.Prim.Memory.PArray
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Memory.PArray
  ( PArray(..)
  , PMArray(..)
  , Pinned(..)
  , fromBytesPArray
  , toBytesPArray
  , fromUArrayPArray
  , toUArrayPArray
  , castPArray
  , fromMBytesPMArray
  , toMBytesPMArray
  , fromUMArrayPMArray
  , toUMArrayPMArray
  , castPMArray
  , allocPMArray
  , allocPinnedPMArray
  , allocAlignedPMArray
  , allocUnpinnedPMArray
  , shrinkPMArray
  , resizePMArray
  , reallocPMArray
  , isPinnedPArray
  , isPinnedPMArray

  , thawPArray
  , freezePMArray
  , sizePArray
  , getSizePMArray
  , readPMArray
  , writePMArray

  , setPMArray
  , copyPArrayToPMArray
  , movePMArrayToPMArray
  ) where

import Control.DeepSeq
import Control.Prim.Monad
import Data.Prim
import Data.Prim.Array (Size(..), UArray(..), UMArray(..))
import Data.Prim.Memory.Bytes
import Data.Prim.Memory.Fold
import Data.Prim.Memory.ForeignPtr
import Data.Prim.Memory.Internal
import Foreign.Prim

-- | An immutable array with elements of type @e@
newtype PArray (p :: Pinned) e = PArray (Bytes p)
  deriving (NFData, Semigroup, Monoid, MemRead)
type role PArray nominal nominal


instance (Prim e, Eq e) => Eq (PArray p e) where
  (==) = eqMem @e
  {-# INLINE (==) #-}

instance (Prim e, Ord e) => Ord (PArray p e) where
  compare = compareMem @e
  {-# INLINE compare #-}

-- | A mutable array with elements of type @e@
newtype PMArray (p :: Pinned) e s = PMArray (MBytes p s)
  deriving (NFData, MemWrite)
type role PMArray nominal nominal nominal

-- | Read-only access, but it is not enforced.
instance PtrAccess s (PArray 'Pin e) where
  toForeignPtr = pure . toForeignPtrBytes . toBytesPArray
  {-# INLINE toForeignPtr #-}
  withPtrAccess b = withPtrBytes (toBytesPArray b)
  {-# INLINE withPtrAccess #-}
  withNoHaltPtrAccess b = withNoHaltPtrBytes (toBytesPArray b)
  {-# INLINE withNoHaltPtrAccess #-}

instance PtrAccess s (PMArray 'Pin e s) where
  toForeignPtr = pure . toForeignPtrMBytes . toMBytesPMArray
  {-# INLINE toForeignPtr #-}
  withPtrAccess mb = withPtrMBytes (toMBytesPMArray mb)
  {-# INLINE withPtrAccess #-}
  withNoHaltPtrAccess mb = withNoHaltPtrMBytes (toMBytesPMArray mb)
  {-# INLINE withNoHaltPtrAccess #-}

instance Typeable p => MemAlloc (PMArray p e) where
  type FrozenMem (PMArray p e) = PArray p e
  getByteCountMutMem = getByteCountMutMem . toMBytesPMArray
  {-# INLINE getByteCountMutMem #-}
  allocMutMem = fmap fromMBytesPMArray . allocMBytes
  {-# INLINE allocMutMem #-}
  thawMem = thawPArray
  {-# INLINE thawMem #-}
  freezeMutMem = freezePMArray
  {-# INLINE freezeMutMem #-}
  reallocMutMem mba = fmap fromMBytesPMArray . reallocMBytes (toMBytesPMArray mba)
  {-# INLINE reallocMutMem #-}

instance (Typeable p, Prim e) => IsList (PArray p e) where
  type Item (PArray p e) = e
  fromList = fromListMem
  fromListN n = fromListZeroMemN_ (Count n)
  toList = toListMem

instance Typeable p => IsString (PArray p Char) where
  fromString = fromListMem

instance (Show e, Prim e) => Show (PArray p e) where
  show = show . toListPArray


toListPArray :: Prim e => PArray p e -> [e]
toListPArray = toListMem

castPArray :: PArray p e' -> PArray p e
castPArray = coerce

-- | /O(1)/ - Cast `PArray` to `UArray`
--
-- @since 0.3.0
toUArrayPArray :: PArray p e -> UArray e
toUArrayPArray pa = UArray (toByteArray# (toBytesPArray pa))
{-# INLINE toUArrayPArray #-}

-- | /O(1)/ - Cast `UArray` to `PArray`
--
-- @since 0.3.0
fromUArrayPArray :: UArray e -> PArray 'Inc e
fromUArrayPArray (UArray ba#) = fromBytesPArray (fromByteArray# ba#)
{-# INLINE fromUArrayPArray #-}

fromBytesPArray :: Bytes p -> PArray p e
fromBytesPArray = coerce

toBytesPArray :: PArray p e -> Bytes p
toBytesPArray = coerce

castPMArray :: PMArray p e' s -> PMArray p e s
castPMArray = coerce

fromMBytesPMArray :: MBytes p s -> PMArray p e s
fromMBytesPMArray = coerce

toMBytesPMArray :: PMArray p e s -> MBytes p s
toMBytesPMArray = coerce

-- | /O(1)/ - Cast `PMArray` to `UMArray`
--
-- @since 0.3.0
toUMArrayPMArray :: PMArray p e s -> UMArray e s
toUMArrayPMArray pa = UMArray (toMutableByteArray# (toMBytesPMArray pa))
{-# INLINE toUMArrayPMArray #-}

-- | /O(1)/ - Cast `UMArray` to `PMArray`
--
-- @since 0.3.0
fromUMArrayPMArray :: UMArray e s -> PMArray 'Inc e s
fromUMArrayPMArray (UMArray mba#) = fromMBytesPMArray (fromMutableByteArray# mba#)
{-# INLINE fromUMArrayPMArray #-}


sizePArray :: forall e p. Prim e => PArray p e -> Size
sizePArray = (coerce :: Count e -> Size) . countBytes . toBytesPArray
{-# INLINE sizePArray #-}

getSizePMArray :: forall e p m s. (MonadPrim s m, Prim e) => PMArray p e s -> m Size
getSizePMArray = fmap (coerce :: Count e -> Size) . getCountMBytes . toMBytesPMArray
{-# INLINE getSizePMArray #-}

allocPMArray ::
     forall e p m s . (Typeable p, Prim e, MonadPrim s m) => Size -> m (PMArray p e s)
allocPMArray sz = fromMBytesPMArray <$> allocMBytes (coerce sz :: Count e)
{-# INLINE allocPMArray #-}

allocUnpinnedPMArray :: forall e m s . (MonadPrim s m, Prim e) => Size -> m (PMArray 'Inc e s)
allocUnpinnedPMArray sz = fromMBytesPMArray <$> allocUnpinnedMBytes (coerce sz :: Count e)
{-# INLINE allocUnpinnedPMArray #-}

allocPinnedPMArray :: forall e m s . (MonadPrim s m, Prim e) => Size -> m (PMArray 'Pin e s)
allocPinnedPMArray sz = fromMBytesPMArray <$> allocPinnedMBytes (coerce sz :: Count e)
{-# INLINE allocPinnedPMArray #-}

allocAlignedPMArray ::
     (MonadPrim s m, Prim e)
  => Count e -- ^ Size in number of bytes
  -> m (PMArray 'Pin e s)
allocAlignedPMArray = fmap fromMBytesPMArray . allocAlignedMBytes
{-# INLINE allocAlignedPMArray #-}

freezePMArray :: MonadPrim s m => PMArray p e s -> m (PArray p e)
freezePMArray = fmap fromBytesPArray . freezeMBytes . toMBytesPMArray
{-# INLINE freezePMArray #-}

thawPArray :: MonadPrim s m => PArray p e -> m (PMArray p e s)
thawPArray = fmap fromMBytesPMArray . thawBytes . toBytesPArray
{-# INLINE thawPArray #-}

-- | Shrink mutable bytes to new specified count of elements. The new count must be less
-- than or equal to the current count as reported by `getCountPMArray`.
shrinkPMArray ::
     forall e p m s. (MonadPrim s m, Prim e)
  => PMArray p e s
  -> Size
  -> m ()
shrinkPMArray mba sz = shrinkMBytes (toMBytesPMArray mba) (coerce sz :: Count e)
{-# INLINE shrinkPMArray #-}


-- | Attempt to resize mutable bytes in place.
--
-- * New bytes might be allocated, with the copy of an old one.
-- * Old references should not be kept around to allow GC to claim it
-- * Old references should not be used to avoid undefined behavior
resizePMArray ::
     forall e p m s. (MonadPrim s m, Prim e)
  => PMArray p e s
  -> Size
  -> m (PMArray 'Inc e s)
resizePMArray mba sz =
  fromMBytesPMArray <$>
  resizeMBytes (toMBytesPMArray mba) (coerce sz :: Count e)
{-# INLINE resizePMArray #-}

reallocPMArray ::
     forall e p m s. (MonadPrim s m, Typeable p,  Prim e)
  => PMArray p e s
  -> Size
  -> m (PMArray p e s)
reallocPMArray mba sz =
  fromMBytesPMArray <$>
  reallocMBytes (toMBytesPMArray mba) (coerce sz :: Count e)
{-# INLINABLE reallocPMArray #-}


isPinnedPArray :: PArray p e -> Bool
isPinnedPArray (PArray b) = isPinnedBytes b
{-# INLINE isPinnedPArray #-}

isPinnedPMArray :: PMArray p e s -> Bool
isPinnedPMArray (PMArray mb) = isPinnedMBytes mb
{-# INLINE isPinnedPMArray #-}

readPMArray :: (MonadPrim s m, Prim e) => PMArray p e s -> Int -> m e
readPMArray (PMArray mb) = readOffMBytes mb . coerce
{-# INLINE readPMArray #-}

writePMArray :: (MonadPrim s m, Prim e) => PMArray p e s -> Int -> e -> m ()
writePMArray (PMArray mb) o = writeOffMBytes mb (coerce o)
{-# INLINE writePMArray #-}



setPMArray ::
     forall e p m s. (MonadPrim s m, Prim e)
  => PMArray p e s -- ^ Chunk of memory to fill
  -> Int -- ^ Offset in number of elements
  -> Size -- ^ Number of cells to fill
  -> e -- ^ A value to fill the cells with
  -> m ()
setPMArray (PMArray mb) off sz = setMBytes mb (coerce off) (coerce sz)
{-# INLINE setPMArray #-}

copyPArrayToPMArray ::
     forall e p m s. (MonadPrim s m, Prim e)
  => PArray p e
  -> Int
  -> PMArray p e s
  -> Int
  -> Size
  -> m ()
copyPArrayToPMArray ba srcOff mba dstOff sz =
  copyMem ba (coerce srcOff) mba (coerce dstOff) (coerce sz `countForProxyTypeOf` ba)
{-# INLINE copyPArrayToPMArray #-}

movePMArrayToPMArray ::
     forall e p m s. (MonadPrim s m, Prim e)
  => PMArray p e s
  -> Int
  -> PMArray p e s
  -> Int
  -> Size
  -> m ()
movePMArrayToPMArray ba srcOff mba dstOff sz =
  moveMutMem ba (coerce srcOff) mba (coerce dstOff) (coerce sz :: Count e)
{-# INLINE movePMArrayToPMArray #-}



-- toPtrPArray :: PArray Pin e -> Ptr e
-- toPtrPArray (PArray ba#) = Ptr (byteArrayContents# ba#)
-- {-# INLINE toPtrPArray #-}

-- toPtrPMArray :: PMArray Pin e s -> Ptr e
-- toPtrPMArray (PMArray mba#) = Ptr (mutablePArrayContents# mba#)
-- {-# INLINE toPtrPMArray #-}

-- -- | Pointer access to immutable `PArray` should be for read only purposes, but it is
-- -- not enforced. Any mutation will break referential transparency
-- withPtrPArray :: MonadPrim s m => PArray Pin e -> (Ptr e -> m b) -> m b
-- withPtrPArray b f = do
--   res <- f (toPtrPArray b)
--   res <$ touch b
-- {-# INLINE withPtrPArray #-}

-- -- | Same as `withPtrPArray`, but is suitable for actions that don't terminate
-- withNoHaltPtrPArray :: MonadUnliftPrim s m => PArray Pin e -> (Ptr e -> m b) -> m b
-- withNoHaltPtrPArray b f = withAliveUnliftPrim b $ f (toPtrPArray b)
-- {-# INLINE withNoHaltPtrPArray #-}

-- withPtrPMArray :: MonadPrim s m => PMArray Pin e s -> (Ptr e -> m b) -> m b
-- withPtrPMArray mb f = do
--   res <- f (toPtrPMArray mb)
--   res <$ touch mb
-- {-# INLINE withPtrPMArray #-}

-- withNoHaltPtrPMArray :: MonadUnliftPrim s m => PMArray Pin e s -> (Ptr e -> m b) -> m b
-- withNoHaltPtrPMArray mb f = withAliveUnliftPrim mb $ f (toPtrPMArray mb)
-- {-# INLINE withNoHaltPtrPMArray #-}


-- -- -- | Check if two byte arrays refer to pinned memory and compare their pointers.
-- -- isSamePArray :: PArray p1 e -> PArray p2 e -> Bool
-- -- isSamePArray (PArray b1#) (PArray b2#) = isTrue# (isSameByteArray# b1# b2#)
-- -- {-# INLINE[0] isSamePArray #-}
-- -- {-# RULES
-- -- "isSamePinnedPArray" isSamePArray = isSamePinnedPArray
-- --   #-}

-- -- -- | Perform pointer equality on pinned `PArray`.
-- -- isSamePinnedPArray :: PArray Pin e -> PArray Pin e -> Bool
-- -- isSamePinnedPArray pb e1 pb2 = toPtrPArray pb e1 == toPtrPArray pb e2
-- -- {-# INLINE isSamePinnedPArray #-}



-- -- byteStringConvertError :: String -> a
-- -- byteStringConvertError msg = error $ "Cannot convert 'ByteString'. " ++ msg
-- -- {-# NOINLINE byteStringConvertError #-}

