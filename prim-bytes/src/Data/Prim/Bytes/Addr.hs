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
  , curOffAddr
  , curCountOfAddr
  , plusOffAddr
  , indexAddr
  , indexOffAddr
  , readAddr
  , readOffAddr

  , thawAddr
  , freezeMAddr

  , MAddr(..)
  , curOffMAddr
  , plusOffMAddr
  , readMAddr
  , readOffMAddr
  , writeMAddr
  , writeOffMAddr

  , withPtrMAddr
  ) where

import Control.DeepSeq
import Control.Monad.Prim
import Control.Monad.Prim.Unsafe
import Control.Monad.ST
import Data.Foldable as Foldable
import Data.List as List
import Data.Prim
import Data.Proxy
import Data.Typeable
import Data.Prim.Bytes
import Data.Prim.Class
import Data.Prim.Foreign (getSizeofMutableByteArray#, isByteArrayPinned#,
                          isMutableByteArrayPinned#, isSameByteArray#,
                          memmoveMutableByteArray#)
import GHC.Exts hiding (getSizeofMutableByteArray#, isByteArrayPinned#,
                 isMutableByteArrayPinned#)
import Foreign.Ptr


data Addr a = Addr Addr# {-# UNPACK #-} !(Bytes 'Pin)

data MAddr a s = MAddr Addr# {-# UNPACK #-} !(MBytes 'Pin s)

class NoConstraint a
instance NoConstraint a

class PFunctor f where
  type Elt f :: * -> Constraint
  type Elt f = NoConstraint
  mapPrim :: (Elt f a, Elt f b, MonadPrim s m) => (a -> m b) -> f a s -> m (f b s)

instance PFunctor MAddr where
  type Elt MAddr = Prim
  mapPrim f maddr = do
    Count n <- getCurrentCountOfMAddr maddr
    maddr' <- allocMAddr (Count n)
    forM_ [0 .. n - 1] $ \i ->
      readOffMAddr maddr (Off i) >>= f >>= writeOffMAddr maddr' (Off i)
    pure maddr'

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
allocMAddr c = toMAddr <$> allocPinnedMBytes c

plusOffAddr :: Prim a => Addr a -> Off a -> Addr a
plusOffAddr (Addr addr# b) off = Addr (addr# `plusAddr#` fromOff# off) b

plusOffMAddr :: Prim a => MAddr a s -> Off a -> MAddr a s
plusOffMAddr (MAddr addr# mb) off = MAddr (addr# `plusAddr#` fromOff# off) mb

curOffAddr :: Prim a => Addr a -> Off a
curOffAddr (Addr addr# (Bytes b#)) =
  let count = countSize (I# (addr# `minusAddr#` byteArrayContents# b#))
  in offAsProxy count (Off (unCount count))

curCountOfAddr :: forall a .Prim a => Addr a -> Count a
curCountOfAddr addr@(Addr _ b) = countOfBytes b - coerce (curOffAddr addr)

getCurrentCountOfMAddr :: (MonadPrim s m, Prim a) => MAddr a s -> m (Count a)
getCurrentCountOfMAddr maddr@(MAddr _ mb) = do
  c <- getCountOfMBytes mb
  pure (c - coerce (curOffMAddr maddr))


indexAddr :: Prim a => Addr a -> a
indexAddr addr = indexOffAddr addr 0

indexOffAddr :: Prim a => Addr a -> Off a -> a
indexOffAddr addr off = unsafeInlineIO $ readOffAddr addr off


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


