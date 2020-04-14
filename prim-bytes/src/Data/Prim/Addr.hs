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
  , MAddr(..)
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

data MAddr s a = MAddr Addr# {-# UNPACK #-} !(MBytes 'Pin s)

instance NFData (Addr a) where
  rnf (Addr _ _) = ()

instance NFData (MAddr s a) where
  rnf (MAddr _ _) = ()

plusOffAddr :: Prim a => Addr a -> Off a -> Addr a
plusOffAddr (Addr addr# b) off = Addr (addr# `plusAddr#` fromOff# off) b

plusOffMAddr :: Prim a => MAddr s a -> Off a -> MAddr s a
plusOffMAddr (MAddr addr# mb) off = MAddr (addr# `plusAddr#` fromOff# off) mb

curAddrOff :: Prim a => Addr a -> Off a
curAddrOff (Addr addr# (Bytes b#)) =
  let count = countSize (I# (addr# `minusAddr#` byteArrayContents# b#))
  in offAsProxy count (Off (unCount count))

curMAddrOff :: Prim a => MAddr s a -> Off a
curMAddrOff (MAddr addr# mb) =
  let count = countSize (Ptr addr# `minusPtr` getMBytesPtr mb)
  in offAsProxy count (Off (unCount count))

withPtrMAddr :: MonadPrim s m => MAddr s a -> (Ptr a -> m b) -> m b
withPtrMAddr (MAddr addr# mb) f = do
  a <- f (Ptr addr#)
  a <$ touch mb

thawAddr :: MonadPrim s m => Addr a -> m (MAddr s a)
thawAddr (Addr addr# b) = MAddr addr# <$> thawBytes b

readAddr :: (MonadPrim s m, Prim a) => Addr a -> m a
readAddr addr = readOffAddr addr 0

readOffAddr :: (MonadPrim s m, Prim a) => Addr a -> Off a -> m a
readOffAddr (Addr addr# b) off = do
  a <- prim (seq# (indexOffAddr# addr# (fromOff# off)))
  a <$ touch b

indexAddr :: Prim a => Addr a -> a
indexAddr addr = indexOffAddr addr 0


indexOffAddr :: Prim a => Addr a -> Off a -> a
indexOffAddr addr off = unsafeInlineIO $ readOffAddr addr off


readOffMAddr :: (MonadPrim s m, Prim a) => MAddr s a -> Off a -> m a
readOffMAddr (MAddr addr# mb) off = do
  a <- prim (readOffAddr# addr# (fromOff# off))
  a <$ touch mb

writeOffMAddr :: (MonadPrim s m, Prim a) => MAddr s a -> Off a -> a -> m ()
writeOffMAddr (MAddr addr# mb) off a = do
  prim_ (writeOffAddr# addr# (fromOff# off) a)
  touch mb


freezeMAddr :: MonadPrim s m => MAddr s a -> m (Addr a)
freezeMAddr (MAddr addr# mb) = Addr addr# <$> freezeMBytes mb
