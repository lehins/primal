{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.Prim.Memory.ByteArray
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Memory.ByteArray
  ( ByteArray(..)
  , MByteArray(..)
  , Pinned(..)
  , toByteArray
  , fromByteArray
  , castByteArray
  , toMByteArray
  , fromMByteArray
  , castMByteArray
  , allocMByteArray
  , allocPinnedMByteArray
  , allocAlignedMByteArray
  , allocUnpinnedMByteArray
  , shrinkMByteArray
  , resizeMByteArray
  , reallocMByteArray
  , isPinnedByteArray
  , isPinnedMByteArray

  , thawByteArray
  , freezeMByteArray
  , sizeByteArray
  , getSizeMByteArray
  , readMByteArray
  , writeMByteArray

  , setMByteArray
  , copyByteArrayToMByteArray
  , moveMByteArrayToMByteArray
  ) where

import Control.DeepSeq
import Control.Prim.Monad
import Foreign.Prim
import Data.Prim
import Data.Prim.Memory.Bytes.Internal
import Data.Prim.Memory.Internal
import Data.Prim.Memory.ForeignPtr


-- | An immutable array of bytes of type @e@
newtype ByteArray (p :: Pinned) e = ByteArray (Bytes p)
  deriving (NFData, Semigroup, Monoid, MemRead)
type role ByteArray phantom representational

-- | A mutable array of bytes of type @e@
newtype MByteArray (p :: Pinned) e s = MByteArray (MBytes p s)
  deriving (NFData, MemWrite)
type role MByteArray phantom representational nominal

-- | Read-only access, but it is not enforced.
instance PtrAccess s (ByteArray 'Pin e) where
  toForeignPtr = pure . toForeignPtrBytes . fromByteArray
  {-# INLINE toForeignPtr #-}
  withPtrAccess b = withPtrBytes (fromByteArray b)
  {-# INLINE withPtrAccess #-}
  withNoHaltPtrAccess b = withNoHaltPtrBytes (fromByteArray b)
  {-# INLINE withNoHaltPtrAccess #-}

instance PtrAccess s (MByteArray 'Pin e s) where
  toForeignPtr = pure . toForeignPtrMBytes . fromMByteArray
  {-# INLINE toForeignPtr #-}
  withPtrAccess mb = withPtrMBytes (fromMByteArray mb)
  {-# INLINE withPtrAccess #-}
  withNoHaltPtrAccess mb = withNoHaltPtrMBytes (fromMByteArray mb)
  {-# INLINE withNoHaltPtrAccess #-}

instance Typeable p => MemAlloc (MByteArray p e) where
  type FrozenMem (MByteArray p e) = ByteArray p e
  getByteCountMem = getByteCountMem . fromMByteArray
  {-# INLINE getByteCountMem #-}
  allocByteCountMem = fmap toMByteArray . allocMBytes
  {-# INLINE allocByteCountMem #-}
  thawMem = thawByteArray
  {-# INLINE thawMem #-}
  freezeMem = freezeMByteArray
  {-# INLINE freezeMem #-}
  resizeMem mba = fmap toMByteArray . reallocMBytes (fromMByteArray mba)
  {-# INLINE resizeMem #-}

instance (Typeable p, Prim e) => IsList (ByteArray p e) where
  type Item (ByteArray p e) = e
  fromList = fromListMem
  fromListN n = fromListMemN_ (Count n)
  toList = toListMem

instance Typeable p => IsString (ByteArray p Char) where
  fromString = fromListMem

instance (Show e, Prim e) => Show (ByteArray p e) where
  show = show . toListByteArray


toListByteArray :: Prim e => ByteArray p e -> [e]
toListByteArray = toListMem

castByteArray :: ByteArray p e' -> ByteArray p e
castByteArray = coerce

toByteArray :: Bytes p -> ByteArray p e
toByteArray = coerce

fromByteArray :: ByteArray p e -> Bytes p
fromByteArray = coerce

castMByteArray :: MByteArray p e' s -> MByteArray p e s
castMByteArray = coerce

toMByteArray :: MBytes p s -> MByteArray p e s
toMByteArray = coerce

fromMByteArray :: MByteArray p e s -> MBytes p s
fromMByteArray = coerce

sizeByteArray :: forall e p. Prim e => ByteArray p e -> Size
sizeByteArray = (coerce :: Count e -> Size) . countBytes . fromByteArray
{-# INLINE sizeByteArray #-}

getSizeMByteArray :: forall e p m s. (MonadPrim s m, Prim e) => MByteArray p e s -> m Size
getSizeMByteArray = fmap (coerce :: Count e -> Size) . getCountMBytes . fromMByteArray
{-# INLINE getSizeMByteArray #-}

allocMByteArray ::
     forall e p m s . (Typeable p, Prim e, MonadPrim s m) => Size -> m (MByteArray p e s)
allocMByteArray sz = toMByteArray <$> allocMBytes (coerce sz :: Count e)
{-# INLINE allocMByteArray #-}

allocUnpinnedMByteArray :: forall e m s . (MonadPrim s m, Prim e) => Size -> m (MByteArray 'Inc e s)
allocUnpinnedMByteArray sz = toMByteArray <$> allocUnpinnedMBytes (coerce sz :: Count e)
{-# INLINE allocUnpinnedMByteArray #-}

allocPinnedMByteArray :: forall e m s . (MonadPrim s m, Prim e) => Size -> m (MByteArray 'Pin e s)
allocPinnedMByteArray sz = toMByteArray <$> allocPinnedMBytes (coerce sz :: Count e)
{-# INLINE allocPinnedMByteArray #-}

allocAlignedMByteArray ::
     (MonadPrim s m, Prim e)
  => Count e -- ^ Size in number of bytes
  -> m (MByteArray 'Pin e s)
allocAlignedMByteArray = fmap toMByteArray . allocAlignedMBytes
{-# INLINE allocAlignedMByteArray #-}

freezeMByteArray :: MonadPrim s m => MByteArray p e s -> m (ByteArray p e)
freezeMByteArray = fmap toByteArray . freezeMBytes . fromMByteArray
{-# INLINE freezeMByteArray #-}

thawByteArray :: MonadPrim s m => ByteArray p e -> m (MByteArray p e s)
thawByteArray = fmap toMByteArray . thawBytes . fromByteArray
{-# INLINE thawByteArray #-}

-- | Shrink mutable bytes to new specified count of elements. The new count must be less
-- than or equal to the current count as reported by `getCountMByteArray`.
shrinkMByteArray :: (MonadPrim s m, Prim e) => MByteArray p e s -> Count e -> m ()
shrinkMByteArray mba c = shrinkMBytes (fromMByteArray mba) (toByteCount c)
{-# INLINE shrinkMByteArray #-}


-- | Attempt to resize mutable bytes in place.
--
-- * New bytes might be allocated, with the copy of an old one.
-- * Old references should not be kept around to allow GC to claim it
-- * Old references should not be used to avoid undefined behavior
resizeMByteArray ::
     (MonadPrim s m, Prim e) => MByteArray p e s -> Count e -> m (MByteArray 'Inc e s)
resizeMByteArray mba = fmap toMByteArray . resizeMBytes (fromMByteArray mba)
{-# INLINE resizeMByteArray #-}

reallocMByteArray ::
     forall e p m s. (MonadPrim s m, Typeable p,  Prim e)
  => MByteArray p e s
  -> Count e
  -> m (MByteArray p e s)
reallocMByteArray mba = fmap toMByteArray . reallocMBytes (fromMByteArray mba)
{-# INLINABLE reallocMByteArray #-}


isPinnedByteArray :: ByteArray p e -> Bool
isPinnedByteArray (ByteArray b) = isPinnedBytes b
{-# INLINE isPinnedByteArray #-}

isPinnedMByteArray :: MByteArray p e s -> Bool
isPinnedMByteArray (MByteArray mb) = isPinnedMBytes mb
{-# INLINE isPinnedMByteArray #-}

readMByteArray :: (MonadPrim s m, Prim e) => MByteArray p e s -> Int -> m e
readMByteArray (MByteArray mb) = readOffMBytes mb . coerce
{-# INLINE readMByteArray #-}

writeMByteArray :: (MonadPrim s m, Prim e) => MByteArray p e s -> Int -> e -> m ()
writeMByteArray (MByteArray mb) o = writeOffMBytes mb (coerce o)
{-# INLINE writeMByteArray #-}



setMByteArray ::
     (MonadPrim s m, Prim e)
  => MByteArray p e s -- ^ Chunk of memory to fill
  -> Int -- ^ Offset in number of elements
  -> Size -- ^ Number of cells to fill
  -> e -- ^ A value to fill the cells with
  -> m ()
setMByteArray (MByteArray mb) off sz = setMBytes mb (coerce off) (coerce sz)
{-# INLINE setMByteArray #-}

copyByteArrayToMByteArray ::
     (MonadPrim s m, Prim e)
  => ByteArray p e
  -> Int
  -> MByteArray p e s
  -> Int
  -> Size
  -> m ()
copyByteArrayToMByteArray ba srcOff mba dstOff sz =
  copyMem ba (coerce srcOff) mba (coerce dstOff) (countAsProxy ba (coerce sz))
{-# INLINE copyByteArrayToMByteArray #-}

moveMByteArrayToMByteArray ::
     forall e p m s. (MonadPrim s m, Prim e)
  => MByteArray p e s
  -> Int
  -> MByteArray p e s
  -> Int
  -> Size
  -> m ()
moveMByteArrayToMByteArray ba srcOff mba dstOff sz =
  moveMem ba (coerce srcOff) mba (coerce dstOff) (coerce sz :: Count e)
{-# INLINE moveMByteArrayToMByteArray #-}



-- toPtrByteArray :: ByteArray Pin e -> Ptr e
-- toPtrByteArray (ByteArray ba#) = Ptr (byteArrayContents# ba#)
-- {-# INLINE toPtrByteArray #-}

-- toPtrMByteArray :: MByteArray Pin e s -> Ptr e
-- toPtrMByteArray (MByteArray mba#) = Ptr (mutableByteArrayContents# mba#)
-- {-# INLINE toPtrMByteArray #-}

-- -- | Pointer access to immutable `ByteArray` should be for read only purposes, but it is
-- -- not enforced. Any mutation will break referential transparency
-- withPtrByteArray :: MonadPrim s m => ByteArray Pin e -> (Ptr e -> m b) -> m b
-- withPtrByteArray b f = do
--   res <- f (toPtrByteArray b)
--   res <$ touch b
-- {-# INLINE withPtrByteArray #-}

-- -- | Same as `withPtrByteArray`, but is suitable for actions that don't terminate
-- withNoHaltPtrByteArray :: MonadUnliftPrim s m => ByteArray Pin e -> (Ptr e -> m b) -> m b
-- withNoHaltPtrByteArray b f = withUnliftPrim b $ f (toPtrByteArray b)
-- {-# INLINE withNoHaltPtrByteArray #-}

-- withPtrMByteArray :: MonadPrim s m => MByteArray Pin e s -> (Ptr e -> m b) -> m b
-- withPtrMByteArray mb f = do
--   res <- f (toPtrMByteArray mb)
--   res <$ touch mb
-- {-# INLINE withPtrMByteArray #-}

-- withNoHaltPtrMByteArray :: MonadUnliftPrim s m => MByteArray Pin e s -> (Ptr e -> m b) -> m b
-- withNoHaltPtrMByteArray mb f = withUnliftPrim mb $ f (toPtrMByteArray mb)
-- {-# INLINE withNoHaltPtrMByteArray #-}


-- -- -- | Check if two byte arrays refer to pinned memory and compare their pointers.
-- -- isSameByteArray :: ByteArray p1 e -> ByteArray p2 e -> Bool
-- -- isSameByteArray (ByteArray b1#) (ByteArray b2#) = isTrue# (isSameByteArray# b1# b2#)
-- -- {-# INLINE[0] isSameByteArray #-}
-- -- {-# RULES
-- -- "isSamePinnedByteArray" isSameByteArray = isSamePinnedByteArray
-- --   #-}

-- -- -- | Perform pointer equality on pinned `ByteArray`.
-- -- isSamePinnedByteArray :: ByteArray Pin e -> ByteArray Pin e -> Bool
-- -- isSamePinnedByteArray pb e1 pb2 = toPtrByteArray pb e1 == toPtrByteArray pb e2
-- -- {-# INLINE isSamePinnedByteArray #-}



-- -- byteStringConvertError :: String -> a
-- -- byteStringConvertError msg = error $ "Cannot convert 'ByteString'. " ++ msg
-- -- {-# NOINLINE byteStringConvertError #-}

