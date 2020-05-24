{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.Prim.MRef.Internal
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.MRef.Internal
  ( MRef(..)
  ) where

import Control.Prim.Monad
import Data.Prim.Memory
import Data.Prim.Memory.Addr
import Data.Prim.Memory.ByteArray
import Data.Prim.Memory.Bytes


class MRef mut where
  type Elt mut :: *

  newMRef :: MonadPrim s m => Elt mut -> m (mut s)
  newMRef a = newRawMRef >>= \mut -> mut <$ writeMRef mut a
  {-# INLINE newMRef #-}

  newRawMRef :: MonadPrim s m => m (mut s)

  readMRef :: MonadPrim s m => mut s -> m (Elt mut)

  writeMRef :: MonadPrim s m => mut s -> Elt mut -> m ()


instance Typeable p => MRef (MBytes p) where
  type Elt (MBytes p) = Word8

  newRawMRef = allocByteCountMem 1
  {-# INLINE newRawMRef #-}

  writeMRef mb = writeOffMBytes mb 0
  {-# INLINE writeMRef #-}

  readMRef mb = readOffMBytes mb 0
  {-# INLINE readMRef #-}



instance Prim e => MRef (MAddr e) where
  type Elt (MAddr e) = e

  newRawMRef = allocMAddr 1
  {-# INLINE newRawMRef #-}

  writeMRef = writeMAddr
  {-# INLINE writeMRef #-}

  readMRef = readMAddr
  {-# INLINE readMRef #-}



instance (Typeable p, Prim e) => MRef (MByteArray p e) where
  type Elt (MByteArray p e) = e

  newRawMRef = allocMByteArray 1
  {-# INLINE newRawMRef #-}

  writeMRef mba = writeMByteArray mba 0
  {-# INLINE writeMRef #-}

  readMRef mba = readMByteArray mba 0
  {-# INLINE readMRef #-}
