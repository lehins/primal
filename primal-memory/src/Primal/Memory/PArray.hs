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
-- Module      : Primal.Memory.PArray
-- Copyright   : (c) Alexey Kuleshevich 2020-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Memory.PArray
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
  , getSizeOfPMArray
  , readPMArray
  , writePMArray

  , setPMArray
  , copyPArrayToPMArray
  , movePMArrayToPMArray
  , module Primal.Element.Unbox
  ) where

import Primal.Array (Size(..), UArray(..), UMArray(..), eqWithST, compareWithST)
import Primal.Eval
import Primal.Foreign
import Primal.Memory.Bytes
import Primal.Memory.Fold
import Primal.Memory.ForeignPtr
import Primal.Memory.Internal
import Primal.Mutable.Eq
import Primal.Mutable.Freeze
import Primal.Mutable.Ord
import Primal.Element.Unbox
import Primal.Element.Unlift

-- | An immutable array with elements of type @e@
newtype PArray (p :: Pinned) e = PArray (Bytes p)
  deriving (NFData, Semigroup, Monoid, MemRead)
type role PArray nominal nominal

instance Unlift (PArray p e) where
  type UnliftIso (PArray p e) = Bytes p

instance (Unbox e, Eq e) => Eq (PArray p e) where
  (==) = eqMem @e
  {-# INLINE (==) #-}

instance (Unbox e, Eq e) => MutEq (PMArray p e) where
  eqMutST m1 m2 = eqWithST isSamePMArray getSizeOfPMArray readPMArray m1 m2
  {-# INLINE eqMutST #-}

instance (Unbox e, Ord e) => Ord (PArray p e) where
  compare = compareMem @e
  {-# INLINE compare #-}

instance (Unbox e, Ord e) => MutOrd (PMArray p e) where
  compareMutST m1 m2 = compareWithST isSamePMArray getSizeOfPMArray readPMArray m1 m2
  {-# INLINE compareMutST #-}

-- | A mutable array with elements of type @e@
newtype PMArray (p :: Pinned) e s = PMArray (MBytes p s)
  deriving (NFData, MutNFData, MutFreeze, MemWrite)
type role PMArray nominal nominal nominal


instance MutUnlift (PMArray p e) where
  type MutUnliftIso (PMArray p e) = MBytes p


type instance Frozen (PMArray p e) = PArray p e

instance MemPtr (PMArray 'Pin e) where
  toMForeignPtrMem = toMForeignPtrMBytes . toMBytesPMArray
  {-# INLINE toMForeignPtrMem #-}
  withPtrMemST mb = withPtrMBytes (toMBytesPMArray mb)
  {-# INLINE withPtrMemST #-}
  withNoHaltPtrMemST mb = withNoHaltPtrMBytes (toMBytesPMArray mb)
  {-# INLINE withNoHaltPtrMemST #-}

instance Typeable p => MemAlloc (PMArray p e) where
  getByteCountMutMemST = getByteCountMutMem . toMBytesPMArray
  {-# INLINE getByteCountMutMemST #-}
  allocMutMemST = fmap fromMBytesPMArray . allocMBytes
  {-# INLINE allocMutMemST #-}
  reallocMutMemST mba = fmap fromMBytesPMArray . reallocMBytes (toMBytesPMArray mba)
  {-# INLINE reallocMutMemST #-}

instance (Typeable p, Unbox e) => IsList (PArray p e) where
  type Item (PArray p e) = e
  fromList = fromListMem
  fromListN n = fromListZeroMemN_ (Count n)
  toList = toListMem

instance Typeable p => IsString (PArray p Char) where
  fromString = fromListMem

instance (Show e, Unbox e) => Show (PArray p e) where
  show = show . toListPArray

toListPArray :: Unbox e => PArray p e -> [e]
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

-- | /O(1)/ - Compare pointers for two mutable arrays and see if they refer to the exact same one.
--
-- Documentation for utilized primop: `sameMutableByteArray#`.
--
-- @since 1.0.0
isSamePMArray :: forall a b p1 p2 s. PMArray p1 a s -> PMArray p2 b s -> Bool
isSamePMArray (PMArray mb1) (PMArray mb2) = isSameMBytes mb1 mb2
{-# INLINE isSamePMArray #-}

sizePArray :: forall e p. Unbox e => PArray p e -> Size
sizePArray = (coerce :: Count e -> Size) . countBytes . toBytesPArray
{-# INLINE sizePArray #-}

getSizeOfPMArray :: forall e p m s. (Primal s m, Unbox e) => PMArray p e s -> m Size
getSizeOfPMArray = fmap (coerce :: Count e -> Size) . getCountMBytes . toMBytesPMArray
{-# INLINE getSizeOfPMArray #-}

allocPMArray ::
     forall e p m s . (Typeable p, Unbox e, Primal s m) => Size -> m (PMArray p e s)
allocPMArray sz = fromMBytesPMArray <$> allocMBytes (coerce sz :: Count e)
{-# INLINE allocPMArray #-}

allocUnpinnedPMArray :: forall e m s . (Primal s m, Unbox e) => Size -> m (PMArray 'Inc e s)
allocUnpinnedPMArray sz = fromMBytesPMArray <$> allocUnpinnedMBytes (coerce sz :: Count e)
{-# INLINE allocUnpinnedPMArray #-}

allocPinnedPMArray :: forall e m s . (Primal s m, Unbox e) => Size -> m (PMArray 'Pin e s)
allocPinnedPMArray sz = fromMBytesPMArray <$> allocPinnedMBytes (coerce sz :: Count e)
{-# INLINE allocPinnedPMArray #-}

allocAlignedPMArray ::
     (Primal s m, Unbox e)
  => Count e -- ^ Size in number of bytes
  -> m (PMArray 'Pin e s)
allocAlignedPMArray = fmap fromMBytesPMArray . allocAlignedMBytes
{-# INLINE allocAlignedPMArray #-}

freezePMArray :: Primal s m => PMArray p e s -> m (PArray p e)
freezePMArray = fmap fromBytesPArray . freezeMBytes . toMBytesPMArray
{-# INLINE freezePMArray #-}

thawPArray :: Primal s m => PArray p e -> m (PMArray p e s)
thawPArray = fmap fromMBytesPMArray . thawBytes . toBytesPArray
{-# INLINE thawPArray #-}

-- | Shrink mutable bytes to new specified count of elements. The new count must be less
-- than or equal to the current count as reported by `getCountPMArray`.
shrinkPMArray ::
     forall e p m s. (Primal s m, Unbox e)
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
     forall e p m s. (Primal s m, Unbox e)
  => PMArray p e s
  -> Size
  -> m (PMArray 'Inc e s)
resizePMArray mba sz =
  fromMBytesPMArray <$>
  resizeMBytes (toMBytesPMArray mba) (coerce sz :: Count e)
{-# INLINE resizePMArray #-}

reallocPMArray ::
     forall e p m s. (Primal s m, Typeable p,  Unbox e)
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

readPMArray :: (Primal s m, Unbox e) => PMArray p e s -> Int -> m e
readPMArray (PMArray mb) = readOffMBytes mb . coerce
{-# INLINE readPMArray #-}

writePMArray :: (Primal s m, Unbox e) => PMArray p e s -> Int -> e -> m ()
writePMArray (PMArray mb) o = writeOffMBytes mb (coerce o)
{-# INLINE writePMArray #-}



setPMArray ::
     forall e p m s. (Primal s m, Unbox e)
  => PMArray p e s -- ^ Chunk of memory to fill
  -> Int -- ^ Offset in number of elements
  -> Size -- ^ Number of cells to fill
  -> e -- ^ A value to fill the cells with
  -> m ()
setPMArray (PMArray mb) off sz = setMBytes mb (coerce off) (coerce sz)
{-# INLINE setPMArray #-}

copyPArrayToPMArray ::
     forall e p m s. (Primal s m, Unbox e)
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
     forall e p m s. (Primal s m, Unbox e)
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
-- withPtrPArray :: Primal s m => PArray Pin e -> (Ptr e -> m b) -> m b
-- withPtrPArray b f = do
--   res <- f (toPtrPArray b)
--   res <$ touch b
-- {-# INLINE withPtrPArray #-}

-- -- | Same as `withPtrPArray`, but is suitable for actions that don't terminate
-- withNoHaltPtrPArray :: UnliftPrimal s m => PArray Pin e -> (Ptr e -> m b) -> m b
-- withNoHaltPtrPArray b f = withAliveUnliftPrim b $ f (toPtrPArray b)
-- {-# INLINE withNoHaltPtrPArray #-}

-- withPtrPMArray :: Primal s m => PMArray Pin e s -> (Ptr e -> m b) -> m b
-- withPtrPMArray mb f = do
--   res <- f (toPtrPMArray mb)
--   res <$ touch mb
-- {-# INLINE withPtrPMArray #-}

-- withNoHaltPtrPMArray :: UnliftPrimal s m => PMArray Pin e s -> (Ptr e -> m b) -> m b
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

