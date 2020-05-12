{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
-- |
-- Module      : Data.Prim.Memory.Bytes
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Memory.Bytes
  ( Bytes(..)
  , MBytes(..)
  , Pinned(..)
  , allocMBytes
  , allocUnpinnedMBytes
  , allocPinnedMBytes
  , allocAlignedMBytes
  , callocAlignedMBytes
  , freezeMBytes
  , thawBytes
  , indexOffBytes
  , indexByteOffBytes
  , compareByteOffBytes
  , byteCountBytes
  , countBytes
  , getCountMBytes
  , getByteCountMBytes
  , setMBytes
  , moveMBytesToMBytes
  , copyBytesToMBytes
  , readOffMBytes
  , readByteOffMBytes
  , writeOffMBytes
  , writeByteOffMBytes
  , toPtrBytes
  , toPtrMBytes
  , withPtrBytes
  , withPtrMBytes
  , withNoHaltPtrBytes
  , withNoHaltPtrMBytes
  , toForeignPtrBytes
  , toForeignPtrMBytes
  ) where

import Data.Prim
import Control.Prim.Monad
import GHC.Exts (ByteArray#, MutableByteArray#)

data Bytes (p :: Pinned) = Bytes ByteArray#
instance Eq (Bytes p)

data MBytes (p :: Pinned) s = MBytes (MutableByteArray# s)

data Pinned = Pin | Inc

allocMBytes :: (Typeable p, Prim a, MonadPrim s m) => Count a -> m (MBytes p s)
allocUnpinnedMBytes :: (MonadPrim s m, Prim a) => Count a -> m (MBytes 'Inc s)
allocPinnedMBytes :: (MonadPrim s m, Prim a) => Count a -> m (MBytes 'Pin s)
allocAlignedMBytes :: (MonadPrim s m, Prim a) => Count a -> m (MBytes 'Pin s)
callocAlignedMBytes :: (MonadPrim s m, Prim a) => Count a -> m (MBytes 'Pin s)

thawBytes :: MonadPrim s m => Bytes p -> m (MBytes p s)
freezeMBytes :: MonadPrim s m => MBytes p s -> m (Bytes p)

indexOffBytes :: Prim a => Bytes p -> Off a -> a
indexByteOffBytes :: Prim a => Bytes p -> Off Word8 -> a
compareByteOffBytes :: Prim a => Bytes p1 -> Off Word8 -> Bytes p2 -> Off Word8 -> Count a -> Ordering

countBytes :: Prim a => Bytes p -> Count a
byteCountBytes :: Bytes p -> Count Word8
getCountMBytes :: forall a p s m. (MonadPrim s m, Prim a) => MBytes p s -> m (Count a)
getByteCountMBytes :: MonadPrim s m => MBytes p s -> m (Count Word8)

setMBytes :: (MonadPrim s m, Prim a) => MBytes p s -> Off a -> Count a -> a -> m ()
copyBytesToMBytes ::
     (MonadPrim s m, Prim a) => Bytes ps -> Off a -> MBytes pd s -> Off a -> Count a -> m ()
moveMBytesToMBytes ::
     (MonadPrim s m, Prim a) => MBytes ps s-> Off a -> MBytes pd s -> Off a -> Count a -> m ()

readOffMBytes :: (MonadPrim s m, Prim a) => MBytes p s -> Off a -> m a
readByteOffMBytes :: (MonadPrim s m, Prim a) => MBytes p s -> Off Word8 -> m a

writeOffMBytes :: (MonadPrim s m, Prim a) => MBytes p s -> Off a -> a -> m ()
writeByteOffMBytes :: (MonadPrim s m, Prim a) => MBytes p s -> Off Word8 -> a -> m ()

toPtrBytes :: Bytes 'Pin -> Ptr a
toPtrMBytes :: MBytes 'Pin s -> Ptr a
withPtrBytes :: MonadPrim s m => Bytes 'Pin -> (Ptr a -> m b) -> m b
withPtrMBytes :: MonadPrim s m => MBytes 'Pin s -> (Ptr a -> m b) -> m b
withNoHaltPtrBytes :: MonadUnliftPrim s m => Bytes 'Pin -> (Ptr a -> m b) -> m b
withNoHaltPtrMBytes :: MonadUnliftPrim s m => MBytes 'Pin s -> (Ptr a -> m b) -> m b

toForeignPtrBytes :: Bytes 'Pin -> ForeignPtr a
toForeignPtrMBytes :: MBytes 'Pin s -> ForeignPtr a