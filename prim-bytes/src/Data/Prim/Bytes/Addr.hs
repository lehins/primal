{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
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
  , indexByteOffAddr
  , readAddr
  , readOffAddr
  , readByteOffAddr
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
  , readByteOffMAddr
  , writeMAddr
  , writeOffMAddr
  , writeByteOffMAddr

  , withPtrMAddr
  -- * Prefetch
  -- ** Directly
  , prefetchAddr0
  , prefetchMAddr0
  , prefetchAddr1
  , prefetchMAddr1
  , prefetchAddr2
  , prefetchMAddr2
  , prefetchAddr3
  , prefetchMAddr3
  -- ** With offset
  , prefetchOffAddr0
  , prefetchOffMAddr0
  , prefetchOffAddr1
  , prefetchOffMAddr1
  , prefetchOffAddr2
  , prefetchOffMAddr2
  , prefetchOffAddr3
  , prefetchOffMAddr3

  ) where

import Control.DeepSeq
import Control.Prim.Monad
import Control.Prim.Monad.Unsafe
import Data.Prim
import Data.Prim.Bytes
import Data.Prim.Class
import Foreign.Prim
import Foreign.Ptr


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

indexByteOffAddr :: Prim a => Addr a -> Off Word8 -> a
indexByteOffAddr addr off = unsafeInlineIO $ readByteOffAddr addr off

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
readOffAddr (Addr addr# b) (Off (I# off#)) = do
  a <- prim (seq# (indexOffAddr# addr# off#))
  a <$ touch b

readByteOffAddr :: (MonadPrim s m, Prim a) => Addr a -> Off Word8 -> m a
readByteOffAddr (Addr addr# b) (Off (I# off#)) = do
  a <- prim (seq# (indexOffAddr# (addr# `plusAddr#` off#) 0#))
  a <$ touch b


readMAddr :: (MonadPrim s m, Prim a) => MAddr a s -> m a
readMAddr maddr = readOffMAddr maddr 0

readOffMAddr :: (MonadPrim s m, Prim a) => MAddr a s -> Off a -> m a
readOffMAddr (MAddr addr# mb) (Off (I# off#)) = do
  a <- prim (readOffAddr# addr# off#)
  a <$ touch mb

readByteOffMAddr :: (MonadPrim s m, Prim a) => MAddr a s -> Off Word8 -> m a
readByteOffMAddr (MAddr addr# mb) (Off (I# off#)) = do
  a <- prim (readOffAddr# (addr# `plusAddr#` off#) 0#)
  a <$ touch mb

writeMAddr :: (MonadPrim s m, Prim a) => MAddr a s -> a -> m ()
writeMAddr maddr = writeOffMAddr maddr 0

writeOffMAddr :: (MonadPrim s m, Prim a) => MAddr a s -> Off a -> a -> m ()
writeOffMAddr (MAddr addr# mb) (Off (I# off#)) a = do
  prim_ (writeOffAddr# addr# off# a)
  touch mb


writeByteOffMAddr :: (MonadPrim s m, Prim a) => MAddr a s -> Off Word8 -> a -> m ()
writeByteOffMAddr (MAddr addr# mb) (Off (I# off#)) a = do
  prim_ (writeOffAddr# (addr# `plusAddr#` off#) 0# a)
  touch mb



prefetchAddr0 :: MonadPrim s m => Addr a -> m ()
prefetchAddr0 (Addr addr# _) = prim_ (prefetchAddr0# addr# 0#)
{-# INLINE prefetchAddr0 #-}

prefetchMAddr0 :: MonadPrim s m => MAddr a s -> m ()
prefetchMAddr0 (MAddr maddr# _) = prim_ (prefetchAddr0# maddr# 0#)
{-# INLINE prefetchMAddr0 #-}

prefetchAddr1 :: MonadPrim s m => Addr a -> m ()
prefetchAddr1 (Addr addr# _) = prim_ (prefetchAddr1# addr# 0#)
{-# INLINE prefetchAddr1 #-}

prefetchMAddr1 :: MonadPrim s m => MAddr a s -> m ()
prefetchMAddr1 (MAddr maddr# _) = prim_ (prefetchAddr1# maddr# 0#)
{-# INLINE prefetchMAddr1 #-}

prefetchAddr2 :: MonadPrim s m => Addr a -> m ()
prefetchAddr2 (Addr addr# _) = prim_ (prefetchAddr2# addr# 0#)
{-# INLINE prefetchAddr2 #-}

prefetchMAddr2 :: MonadPrim s m => MAddr a s -> m ()
prefetchMAddr2 (MAddr maddr# _) = prim_ (prefetchAddr2# maddr# 0#)
{-# INLINE prefetchMAddr2 #-}

prefetchAddr3 :: MonadPrim s m => Addr a -> m ()
prefetchAddr3 (Addr addr# _) = prim_ (prefetchAddr3# addr# 0#)
{-# INLINE prefetchAddr3 #-}

prefetchMAddr3 :: MonadPrim s m => MAddr a s -> m ()
prefetchMAddr3 (MAddr maddr# _) = prim_ (prefetchAddr3# maddr# 0#)
{-# INLINE prefetchMAddr3 #-}


prefetchOffAddr0 :: (MonadPrim s m, Prim a) => Addr a -> Off a -> m ()
prefetchOffAddr0 (Addr addr# _) off = prim_ (prefetchAddr0# addr# (fromOff# off))
{-# INLINE prefetchOffAddr0 #-}

prefetchOffMAddr0 :: (MonadPrim s m, Prim a) => MAddr a s -> Off a -> m ()
prefetchOffMAddr0 (MAddr maddr# _) off = prim_ (prefetchAddr0# maddr# (fromOff# off))
{-# INLINE prefetchOffMAddr0 #-}

prefetchOffAddr1 :: (MonadPrim s m, Prim a) => Addr a -> Off a -> m ()
prefetchOffAddr1 (Addr addr# _) off = prim_ (prefetchAddr1# addr# (fromOff# off))
{-# INLINE prefetchOffAddr1 #-}

prefetchOffMAddr1 :: (MonadPrim s m, Prim a) => MAddr a s -> Off a -> m ()
prefetchOffMAddr1 (MAddr maddr# _) off = prim_ (prefetchAddr1# maddr# (fromOff# off))
{-# INLINE prefetchOffMAddr1 #-}

prefetchOffAddr2 :: (MonadPrim s m, Prim a) => Addr a -> Off a -> m ()
prefetchOffAddr2 (Addr addr# _) off = prim_ (prefetchAddr2# addr# (fromOff# off))
{-# INLINE prefetchOffAddr2 #-}

prefetchOffMAddr2 :: (MonadPrim s m, Prim a) => MAddr a s -> Off a -> m ()
prefetchOffMAddr2 (MAddr maddr# _) off = prim_ (prefetchAddr2# maddr# (fromOff# off))
{-# INLINE prefetchOffMAddr2 #-}

prefetchOffAddr3 :: (MonadPrim s m, Prim a) => Addr a -> Off a -> m ()
prefetchOffAddr3 (Addr addr# _) off = prim_ (prefetchAddr3# addr# (fromOff# off))
{-# INLINE prefetchOffAddr3 #-}

prefetchOffMAddr3 :: (MonadPrim s m, Prim a) => MAddr a s -> Off a -> m ()
prefetchOffMAddr3 (MAddr maddr# _) off = prim_ (prefetchAddr3# maddr# (fromOff# off))
{-# INLINE prefetchOffMAddr3 #-}

