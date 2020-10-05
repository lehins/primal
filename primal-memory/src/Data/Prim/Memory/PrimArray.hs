{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.Prim.Memory.PrimArray
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Memory.PrimArray
  ( PrimArray(..)
  , MPrimArray(..)
  , Pinned(..)
  , fromBytesPrimArray
  , toBytesPrimArray
  , castPrimArray
  , fromMBytesMPrimArray
  , toMBytesMPrimArray
  , castMPrimArray
  , allocMPrimArray
  , allocPinnedMPrimArray
  , allocAlignedMPrimArray
  , allocUnpinnedMPrimArray
  , shrinkMPrimArray
  , resizeMPrimArray
  , reallocMPrimArray
  , isPinnedPrimArray
  , isPinnedMPrimArray

  , thawPrimArray
  , freezeMPrimArray
  , sizePrimArray
  , getSizeMPrimArray
  , readMPrimArray
  , writeMPrimArray

  , setMPrimArray
  , copyPrimArrayToMPrimArray
  , moveMPrimArrayToMPrimArray
  ) where

import Control.DeepSeq
import Control.Prim.Monad
import Foreign.Prim
import Data.Prim
import Data.Prim.Memory.Bytes
import Data.Prim.Memory.Internal
import Data.Prim.Memory.ForeignPtr


-- | An immutable array of bytes of type @e@
newtype PrimArray (p :: Pinned) e = PrimArray (Bytes p)
  deriving (NFData, Semigroup, Monoid, MemRead)
type role PrimArray nominal nominal

-- | A mutable array of bytes of type @e@
newtype MPrimArray (p :: Pinned) e s = MPrimArray (MBytes p s)
  deriving (NFData, MemWrite)
type role MPrimArray nominal nominal nominal

-- | Read-only access, but it is not enforced.
instance PtrAccess s (PrimArray 'Pin e) where
  toForeignPtr = pure . toForeignPtrBytes . toBytesPrimArray
  {-# INLINE toForeignPtr #-}
  withPtrAccess b = withPtrBytes (toBytesPrimArray b)
  {-# INLINE withPtrAccess #-}
  withNoHaltPtrAccess b = withNoHaltPtrBytes (toBytesPrimArray b)
  {-# INLINE withNoHaltPtrAccess #-}

instance PtrAccess s (MPrimArray 'Pin e s) where
  toForeignPtr = pure . toForeignPtrMBytes . toMBytesMPrimArray
  {-# INLINE toForeignPtr #-}
  withPtrAccess mb = withPtrMBytes (toMBytesMPrimArray mb)
  {-# INLINE withPtrAccess #-}
  withNoHaltPtrAccess mb = withNoHaltPtrMBytes (toMBytesMPrimArray mb)
  {-# INLINE withNoHaltPtrAccess #-}

instance Typeable p => MemAlloc (MPrimArray p e) where
  type FrozenMem (MPrimArray p e) = PrimArray p e
  getByteCountMem = getByteCountMem . toMBytesMPrimArray
  {-# INLINE getByteCountMem #-}
  allocMem = fmap fromMBytesMPrimArray . allocMBytes
  {-# INLINE allocMem #-}
  thawMem = thawPrimArray
  {-# INLINE thawMem #-}
  freezeMem = freezeMPrimArray
  {-# INLINE freezeMem #-}
  reallocMem mba = fmap fromMBytesMPrimArray . reallocMBytes (toMBytesMPrimArray mba)
  {-# INLINE reallocMem #-}

instance (Typeable p, Prim e) => IsList (PrimArray p e) where
  type Item (PrimArray p e) = e
  fromList = fromListMem
  fromListN n = fromListZeroMemN_ (Count n)
  toList = toListMem

instance Typeable p => IsString (PrimArray p Char) where
  fromString = fromListMem

instance (Show e, Prim e) => Show (PrimArray p e) where
  show = show . toListPrimArray


toListPrimArray :: Prim e => PrimArray p e -> [e]
toListPrimArray = toListMem

castPrimArray :: PrimArray p e' -> PrimArray p e
castPrimArray = coerce

fromBytesPrimArray :: Bytes p -> PrimArray p e
fromBytesPrimArray = coerce

toBytesPrimArray :: PrimArray p e -> Bytes p
toBytesPrimArray = coerce

castMPrimArray :: MPrimArray p e' s -> MPrimArray p e s
castMPrimArray = coerce

fromMBytesMPrimArray :: MBytes p s -> MPrimArray p e s
fromMBytesMPrimArray = coerce

toMBytesMPrimArray :: MPrimArray p e s -> MBytes p s
toMBytesMPrimArray = coerce

sizePrimArray :: forall e p. Prim e => PrimArray p e -> Size
sizePrimArray = (coerce :: Count e -> Size) . countBytes . toBytesPrimArray
{-# INLINE sizePrimArray #-}

getSizeMPrimArray :: forall e p m s. (MonadPrim s m, Prim e) => MPrimArray p e s -> m Size
getSizeMPrimArray = fmap (coerce :: Count e -> Size) . getCountMBytes . toMBytesMPrimArray
{-# INLINE getSizeMPrimArray #-}

allocMPrimArray ::
     forall e p m s . (Typeable p, Prim e, MonadPrim s m) => Size -> m (MPrimArray p e s)
allocMPrimArray sz = fromMBytesMPrimArray <$> allocMBytes (coerce sz :: Count e)
{-# INLINE allocMPrimArray #-}

allocUnpinnedMPrimArray :: forall e m s . (MonadPrim s m, Prim e) => Size -> m (MPrimArray 'Inc e s)
allocUnpinnedMPrimArray sz = fromMBytesMPrimArray <$> allocUnpinnedMBytes (coerce sz :: Count e)
{-# INLINE allocUnpinnedMPrimArray #-}

allocPinnedMPrimArray :: forall e m s . (MonadPrim s m, Prim e) => Size -> m (MPrimArray 'Pin e s)
allocPinnedMPrimArray sz = fromMBytesMPrimArray <$> allocPinnedMBytes (coerce sz :: Count e)
{-# INLINE allocPinnedMPrimArray #-}

allocAlignedMPrimArray ::
     (MonadPrim s m, Prim e)
  => Count e -- ^ Size in number of bytes
  -> m (MPrimArray 'Pin e s)
allocAlignedMPrimArray = fmap fromMBytesMPrimArray . allocAlignedMBytes
{-# INLINE allocAlignedMPrimArray #-}

freezeMPrimArray :: MonadPrim s m => MPrimArray p e s -> m (PrimArray p e)
freezeMPrimArray = fmap fromBytesPrimArray . freezeMBytes . toMBytesMPrimArray
{-# INLINE freezeMPrimArray #-}

thawPrimArray :: MonadPrim s m => PrimArray p e -> m (MPrimArray p e s)
thawPrimArray = fmap fromMBytesMPrimArray . thawBytes . toBytesPrimArray
{-# INLINE thawPrimArray #-}

-- | Shrink mutable bytes to new specified count of elements. The new count must be less
-- than or equal to the current count as reported by `getCountMPrimArray`.
shrinkMPrimArray ::
     forall e p m s. (MonadPrim s m, Prim e)
  => MPrimArray p e s
  -> Size
  -> m ()
shrinkMPrimArray mba sz = shrinkMBytes (toMBytesMPrimArray mba) (coerce sz :: Count e)
{-# INLINE shrinkMPrimArray #-}


-- | Attempt to resize mutable bytes in place.
--
-- * New bytes might be allocated, with the copy of an old one.
-- * Old references should not be kept around to allow GC to claim it
-- * Old references should not be used to avoid undefined behavior
resizeMPrimArray ::
     forall e p m s. (MonadPrim s m, Prim e)
  => MPrimArray p e s
  -> Size
  -> m (MPrimArray 'Inc e s)
resizeMPrimArray mba sz =
  fromMBytesMPrimArray <$>
  resizeMBytes (toMBytesMPrimArray mba) (coerce sz :: Count e)
{-# INLINE resizeMPrimArray #-}

reallocMPrimArray ::
     forall e p m s. (MonadPrim s m, Typeable p,  Prim e)
  => MPrimArray p e s
  -> Size
  -> m (MPrimArray p e s)
reallocMPrimArray mba sz =
  fromMBytesMPrimArray <$>
  reallocMBytes (toMBytesMPrimArray mba) (coerce sz :: Count e)
{-# INLINABLE reallocMPrimArray #-}


isPinnedPrimArray :: PrimArray p e -> Bool
isPinnedPrimArray (PrimArray b) = isPinnedBytes b
{-# INLINE isPinnedPrimArray #-}

isPinnedMPrimArray :: MPrimArray p e s -> Bool
isPinnedMPrimArray (MPrimArray mb) = isPinnedMBytes mb
{-# INLINE isPinnedMPrimArray #-}

readMPrimArray :: (MonadPrim s m, Prim e) => MPrimArray p e s -> Int -> m e
readMPrimArray (MPrimArray mb) = readOffMBytes mb . coerce
{-# INLINE readMPrimArray #-}

writeMPrimArray :: (MonadPrim s m, Prim e) => MPrimArray p e s -> Int -> e -> m ()
writeMPrimArray (MPrimArray mb) o = writeOffMBytes mb (coerce o)
{-# INLINE writeMPrimArray #-}



setMPrimArray ::
     forall e p m s. (MonadPrim s m, Prim e)
  => MPrimArray p e s -- ^ Chunk of memory to fill
  -> Int -- ^ Offset in number of elements
  -> Size -- ^ Number of cells to fill
  -> e -- ^ A value to fill the cells with
  -> m ()
setMPrimArray (MPrimArray mb) off sz = setMBytes mb (coerce off) (coerce sz)
{-# INLINE setMPrimArray #-}

copyPrimArrayToMPrimArray ::
     forall e p m s. (MonadPrim s m, Prim e)
  => PrimArray p e
  -> Int
  -> MPrimArray p e s
  -> Int
  -> Size
  -> m ()
copyPrimArrayToMPrimArray ba srcOff mba dstOff sz =
  copyMem ba (coerce srcOff) mba (coerce dstOff) (coerce sz `countForProxyTypeOf` ba)
{-# INLINE copyPrimArrayToMPrimArray #-}

moveMPrimArrayToMPrimArray ::
     forall e p m s. (MonadPrim s m, Prim e)
  => MPrimArray p e s
  -> Int
  -> MPrimArray p e s
  -> Int
  -> Size
  -> m ()
moveMPrimArrayToMPrimArray ba srcOff mba dstOff sz =
  moveMem ba (coerce srcOff) mba (coerce dstOff) (coerce sz :: Count e)
{-# INLINE moveMPrimArrayToMPrimArray #-}



-- toPtrPrimArray :: PrimArray Pin e -> Ptr e
-- toPtrPrimArray (PrimArray ba#) = Ptr (byteArrayContents# ba#)
-- {-# INLINE toPtrPrimArray #-}

-- toPtrMPrimArray :: MPrimArray Pin e s -> Ptr e
-- toPtrMPrimArray (MPrimArray mba#) = Ptr (mutablePrimArrayContents# mba#)
-- {-# INLINE toPtrMPrimArray #-}

-- -- | Pointer access to immutable `PrimArray` should be for read only purposes, but it is
-- -- not enforced. Any mutation will break referential transparency
-- withPtrPrimArray :: MonadPrim s m => PrimArray Pin e -> (Ptr e -> m b) -> m b
-- withPtrPrimArray b f = do
--   res <- f (toPtrPrimArray b)
--   res <$ touch b
-- {-# INLINE withPtrPrimArray #-}

-- -- | Same as `withPtrPrimArray`, but is suitable for actions that don't terminate
-- withNoHaltPtrPrimArray :: MonadUnliftPrim s m => PrimArray Pin e -> (Ptr e -> m b) -> m b
-- withNoHaltPtrPrimArray b f = withAliveUnliftPrim b $ f (toPtrPrimArray b)
-- {-# INLINE withNoHaltPtrPrimArray #-}

-- withPtrMPrimArray :: MonadPrim s m => MPrimArray Pin e s -> (Ptr e -> m b) -> m b
-- withPtrMPrimArray mb f = do
--   res <- f (toPtrMPrimArray mb)
--   res <$ touch mb
-- {-# INLINE withPtrMPrimArray #-}

-- withNoHaltPtrMPrimArray :: MonadUnliftPrim s m => MPrimArray Pin e s -> (Ptr e -> m b) -> m b
-- withNoHaltPtrMPrimArray mb f = withAliveUnliftPrim mb $ f (toPtrMPrimArray mb)
-- {-# INLINE withNoHaltPtrMPrimArray #-}


-- -- -- | Check if two byte arrays refer to pinned memory and compare their pointers.
-- -- isSamePrimArray :: PrimArray p1 e -> PrimArray p2 e -> Bool
-- -- isSamePrimArray (PrimArray b1#) (PrimArray b2#) = isTrue# (isSameByteArray# b1# b2#)
-- -- {-# INLINE[0] isSamePrimArray #-}
-- -- {-# RULES
-- -- "isSamePinnedPrimArray" isSamePrimArray = isSamePinnedPrimArray
-- --   #-}

-- -- -- | Perform pointer equality on pinned `PrimArray`.
-- -- isSamePinnedPrimArray :: PrimArray Pin e -> PrimArray Pin e -> Bool
-- -- isSamePinnedPrimArray pb e1 pb2 = toPtrPrimArray pb e1 == toPtrPrimArray pb e2
-- -- {-# INLINE isSamePinnedPrimArray #-}



-- -- byteStringConvertError :: String -> a
-- -- byteStringConvertError msg = error $ "Cannot convert 'ByteString'. " ++ msg
-- -- {-# NOINLINE byteStringConvertError #-}

