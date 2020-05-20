-- |
-- Module      : Data.Prim.Array
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Prim.Array
  (
    PrimArray(..)
  , PrimMArray(..)
  , PrimRep
  , createPrimArray
  ) where

import Control.Prim.Monad
import Data.Prim.Array.Internal
import qualified Data.Prim.Array.Boxed as Boxed
import Data.Prim.ByteRep

newtype PrimArray a = PrimArray (Frozen (UnboxRep Boxed.BoxedMArray a a))

newtype PrimMArray a s = PrimMArray (UnboxRep Boxed.BoxedMArray a a s)

class Mutable (UnboxRep Boxed.BoxedMArray e e) => PrimRep e
instance Mutable (UnboxRep Boxed.BoxedMArray e e) => PrimRep e

instance PrimRep e => Mutable (PrimMArray e) where
  type Frozen (PrimMArray e) = PrimArray e
  type Elt (PrimMArray e) = Elt (UnboxRep Boxed.BoxedMArray e e)
  sizeOfArray (PrimArray a) = sizeOfArray a
  {-# INLINE sizeOfArray #-}
  indexArray (PrimArray a) = indexArray a
  {-# INLINE indexArray #-}
  getSizeOfMArray (PrimMArray ma) = getSizeOfMArray ma
  {-# INLINE getSizeOfMArray #-}
  thawArray (PrimArray a) = PrimMArray <$> thawArray a
  {-# INLINE thawArray #-}
  freezeMArray (PrimMArray ma) = PrimArray <$> freezeMArray ma
  {-# INLINE freezeMArray #-}
  newRawMArray = fmap PrimMArray . newRawMArray
  {-# INLINE newRawMArray #-}
  readMArray (PrimMArray ma) = readMArray ma
  {-# INLINE readMArray #-}
  writeMArray (PrimMArray ma) = writeMArray ma
  {-# INLINE writeMArray #-}
  copyArray (PrimArray as) os (PrimMArray mad) = copyArray as os mad
  {-# INLINE copyArray #-}
  moveMArray (PrimMArray mas) os (PrimMArray mad) = moveMArray mas os mad
  {-# INLINE moveMArray #-}
  setMArray (PrimMArray ma) = setMArray ma
  {-# INLINE setMArray #-}


createPrimArray ::
     (MonadPrim s m, PrimRep e)
  => Size
  -> (PrimMArray e s -> m b)
  -> m (b, PrimArray e)
createPrimArray = createArrayM
