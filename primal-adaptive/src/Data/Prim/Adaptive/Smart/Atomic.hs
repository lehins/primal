{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Prim.Array.Adaptive.Atomic
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Array.Adaptive.Atomic
  (
    AtomicArray(..)
  , AtomicMArray(..)
  , AtomicRep
  , createAtomicArray
  ) where

import Control.Prim.Monad
import Data.Prim.MArray.Internal
import qualified Data.Prim.MArray.Boxed as Boxed
import qualified Data.Prim.MArray.Adaptive.Rep as Rep

newtype AtomicArray a = AtomicArray (Frozen (Rep.AtomicRep Boxed.BoxedMArray a a))

newtype AtomicArray a = AtomicArray (Frozen (Rep.AtomicRep Boxed.BoxedMArray a a))

newtype AtomicMArray a s = AtomicMArray (Rep.AtomicRep Boxed.BoxedMArray a a s)

class Mutable (Rep.AtomicRep Boxed.BoxedMArray e e) => AtomicRep e
instance Mutable (Rep.AtomicRep Boxed.BoxedMArray e e) => AtomicRep e

instance AtomicRep e => Mutable (AtomicMArray e) where
  type Frozen (AtomicMArray e) = AtomicArray e
  type Elt (AtomicMArray e) = Elt (Rep.AtomicRep Boxed.BoxedMArray e e)
  sizeOfArray (AtomicArray a) = sizeOfArray a
  {-# INLINE sizeOfArray #-}
  indexArray (AtomicArray a) = indexArray a
  {-# INLINE indexArray #-}
  getSizeOfMArray (AtomicMArray ma) = getSizeOfMArray ma
  {-# INLINE getSizeOfMArray #-}
  thawArray (AtomicArray a) = AtomicMArray <$> thawArray a
  {-# INLINE thawArray #-}
  freezeMArray (AtomicMArray ma) = AtomicArray <$> freezeMArray ma
  {-# INLINE freezeMArray #-}
  newRawMArray = fmap AtomicMArray . newRawMArray
  {-# INLINE newRawMArray #-}
  readMArray (AtomicMArray ma) = readMArray ma
  {-# INLINE readMArray #-}
  writeMArray (AtomicMArray ma) = writeMArray ma
  {-# INLINE writeMArray #-}
  copyArray (AtomicArray as) os (AtomicMArray mad) = copyArray as os mad
  {-# INLINE copyArray #-}
  moveMArray (AtomicMArray mas) os (AtomicMArray mad) = moveMArray mas os mad
  {-# INLINE moveMArray #-}
  setMArray (AtomicMArray ma) = setMArray ma
  {-# INLINE setMArray #-}



createAtomicArray ::
     (MonadPrim s m, AtomicRep e)
  => Size
  -> (AtomicMArray e s -> m b)
  -> m (b, AtomicArray e)
createAtomicArray = createArrayM
