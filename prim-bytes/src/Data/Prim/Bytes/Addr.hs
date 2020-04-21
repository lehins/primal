{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Data.Prim.Bytes
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Bytes.Addr
  ( Addr(..)
  , castAddr
  , toAddr
  , curOffAddr
  , countOfAddr
  , plusOffAddr
  , indexAddr
  , indexOffAddr
  , readAddr
  , readOffAddr

  , thawAddr
  , freezeMAddr
  , withPtrAddr
  , withAddrAddr#

  , MAddr(..)
  , castMAddr
  , allocMAddr
  , callocMAddr
  , curOffMAddr
  , getCountOfMAddr
  , plusOffMAddr
  , readMAddr
  , readOffMAddr
  , writeMAddr
  , writeOffMAddr

  , withPtrMAddr
  ) where

import Control.DeepSeq
import Control.Monad.ST
import Control.Prim.Monad
import Control.Prim.Monad.Unsafe
import Data.Prim
import Data.Prim.Bytes
import Data.Prim.Class
import Data.Proxy
import Data.Typeable
import Foreign.Prim (getSizeofMutableByteArray#, isByteArrayPinned#,
                     isMutableByteArrayPinned#, isSameByteArray#, memcmpAddr#,
                     memmoveMutableByteArray#, toOrdering#)
import Foreign.Ptr
import GHC.Exts hiding (getSizeofMutableByteArray#, isByteArrayPinned#,
                 isMutableByteArrayPinned#)


data Addr a = Addr
  { addrAddr# :: Addr#
  , addrBytes :: {-# UNPACK #-}!(Bytes 'Pin)
  }

data MAddr a s = MAddr
  { mAddrAddr# :: Addr#
  , mAddrMBytes :: {-# UNPACK #-}!(MBytes 'Pin s)
  }

instance Eq (Addr a) where
  a1 == a2 =
    isSameAddr a1 a2 || (c1' == c2' && memcmpAddr a1' 0 a2' 0 c1' == EQ)
    where
      a1' = castAddr a1 :: Addr Word8
      a2' = castAddr a2 :: Addr Word8
      c1' = countOfAddr a1'
      c2' = countOfAddr a2'

castAddr :: Addr a -> Addr b
castAddr = coerce

castMAddr :: MAddr a s -> MAddr b s
castMAddr = coerce

memcmpAddr :: Prim a => Addr a -> Off a -> Addr a -> Off a -> Count a -> Ordering
memcmpAddr a1 o1 a2 o2 c = unsafeInlineIO $
  withAddrAddr# a1 $ \ addr1# ->
    withAddrAddr# a2 $ \ addr2# ->
      pure $ toOrdering# (memcmpAddr# addr1# (fromOff# o1) addr2# (fromOff# o2) (fromCount# c))

isSameAddr :: Addr a -> Addr a -> Bool
isSameAddr (Addr a1# _) (Addr a2# _) = isTrue# (a1# `eqAddr#` a2#)

instance NFData (Addr a) where
  rnf (Addr _ _) = ()

instance NFData (MAddr a s) where
  rnf (MAddr _ _) = ()

toAddr :: Bytes 'Pin -> Addr a
toAddr b@(Bytes b#) = Addr (byteArrayContents# b#) b

toMAddr :: MBytes 'Pin s -> MAddr a s
toMAddr mb =
  case getPtrMBytes mb of
    Ptr addr# -> MAddr addr# mb

allocMAddr :: (MonadPrim s m, Prim a) => Count a -> m (MAddr a s)
allocMAddr c = toMAddr <$> allocAlignedMBytes c

callocMAddr :: (MonadPrim s m, Prim a) => Count a -> m (MAddr a s)
callocMAddr c = toMAddr <$> callocAlignedMBytes c

plusOffAddr :: Prim a => Addr a -> Off a -> Addr a
plusOffAddr (Addr addr# b) off = Addr (addr# `plusAddr#` fromOff# off) b

plusOffMAddr :: Prim a => MAddr a s -> Off a -> MAddr a s
plusOffMAddr (MAddr addr# mb) off = MAddr (addr# `plusAddr#` fromOff# off) mb

curOffAddr :: Prim a => Addr a -> Off a
curOffAddr (Addr addr# (Bytes b#)) =
  let count = countSize (I# (addr# `minusAddr#` byteArrayContents# b#))
  in offAsProxy count (Off (unCount count))

countOfAddr :: forall a .Prim a => Addr a -> Count a
countOfAddr addr@(Addr _ b) = countOfBytes b - coerce (curOffAddr addr)

getCountOfMAddr :: (MonadPrim s m, Prim a) => MAddr a s -> m (Count a)
getCountOfMAddr maddr@(MAddr _ mb) =
  subtract (coerce (curOffMAddr maddr)) <$> getCountOfMBytes mb


indexAddr :: Prim a => Addr a -> a
indexAddr addr = indexOffAddr addr 0

indexOffAddr :: Prim a => Addr a -> Off a -> a
indexOffAddr addr off = unsafeInlineIO $ readOffAddr addr off

withPtrAddr :: MonadPrim s m => Addr a -> (Ptr a -> m b) -> m b
withPtrAddr addr f = withAddrAddr# addr (\addr# -> f (Ptr addr#))

withAddrAddr# :: MonadPrim s m => Addr a -> (Addr# -> m b) -> m b
withAddrAddr# (Addr addr# mb) f = do
  a <- f addr#
  a <$ touch mb


curOffMAddr :: Prim a => MAddr a s -> Off a
curOffMAddr (MAddr addr# mb) =
  let count = countSize (Ptr addr# `minusPtr` getPtrMBytes mb)
  in offAsProxy count (Off (unCount count))

withPtrMAddr :: MonadPrim s m => MAddr a s -> (Ptr a -> m b) -> m b
withPtrMAddr (MAddr addr# mb) f = do
  a <- f (Ptr addr#)
  a <$ touch mb

thawAddr :: MonadPrim s m => Addr a -> m (MAddr a s)
thawAddr (Addr addr# b) = MAddr addr# <$> thawBytes b

freezeMAddr :: MonadPrim s m => MAddr a s -> m (Addr a)
freezeMAddr (MAddr addr# mb) = Addr addr# <$> freezeMBytes mb


readAddr :: (MonadPrim s m, Prim a) => Addr a -> m a
readAddr addr = readOffAddr addr 0

readOffAddr :: (MonadPrim s m, Prim a) => Addr a -> Off a -> m a
readOffAddr (Addr addr# b) off = do
  a <- prim (seq# (indexOffAddr# addr# (fromOff# off)))
  a <$ touch b


readMAddr :: (MonadPrim s m, Prim a) => MAddr a s -> m a
readMAddr maddr = readOffMAddr maddr 0

readOffMAddr :: (MonadPrim s m, Prim a) => MAddr a s -> Off a -> m a
readOffMAddr (MAddr addr# mb) off = do
  a <- prim (readOffAddr# addr# (fromOff# off))
  a <$ touch mb

writeMAddr :: (MonadPrim s m, Prim a) => MAddr a s -> a -> m ()
writeMAddr maddr = writeOffMAddr maddr 0

writeOffMAddr :: (MonadPrim s m, Prim a) => MAddr a s -> Off a -> a -> m ()
writeOffMAddr (MAddr addr# mb) off a = do
  prim_ (writeOffAddr# addr# (fromOff# off) a)
  touch mb


