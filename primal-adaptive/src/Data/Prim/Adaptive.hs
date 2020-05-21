{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Prim.Array.Adaptive
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Array.Adaptive
  (
    AArray(..)
  , AMArray(..)
  , AdaptiveRep
  , createAArray
  ) where

import Control.Prim.Monad
import Data.Prim.Array.Internal
import qualified Data.Prim.Array.Boxed as Boxed
import qualified Data.Prim.Array.Adaptive.Rep as Rep


newtype AArray a = AArray (Frozen (Rep.AdaptRep Boxed.BoxedMArray a a))

newtype AMArray a s = AMArray (Rep.AdaptRep Boxed.BoxedMArray a a s)

class Mutable (Rep.AdaptRep Boxed.BoxedMArray e e) => AdaptRep e
instance Mutable (Rep.AdaptRep Boxed.BoxedMArray e e) => AdaptRep e


instance AdaptRep e => Mutable (AMArray e) where
  type Frozen (AMArray e) = AArray e
  type Elt (AMArray e) = Elt (Rep.AdaptRep Boxed.BoxedMArray e e)
  sizeOfArray (AArray a) = sizeOfArray a
  {-# INLINE sizeOfArray #-}
  indexArray (AArray a) = indexArray a
  {-# INLINE indexArray #-}
  getSizeOfMArray (AMArray ma) = getSizeOfMArray ma
  {-# INLINE getSizeOfMArray #-}
  thawArray (AArray a) = AMArray <$> thawArray a
  {-# INLINE thawArray #-}
  freezeMArray (AMArray ma) = AArray <$> freezeMArray ma
  {-# INLINE freezeMArray #-}
  newRawMArray = fmap AMArray . newRawMArray
  {-# INLINE newRawMArray #-}
  readMArray (AMArray ma) = readMArray ma
  {-# INLINE readMArray #-}
  writeMArray (AMArray ma) = writeMArray ma
  {-# INLINE writeMArray #-}
  copyArray (AArray as) os (AMArray mad) = copyArray as os mad
  {-# INLINE copyArray #-}
  moveMArray (AMArray mas) os (AMArray mad) = moveMArray mas os mad
  {-# INLINE moveMArray #-}
  setMArray (AMArray ma) = setMArray ma
  {-# INLINE setMArray #-}


createAArray ::
     (MonadPrim s m, AdaptRep e)
  => Size
  -> (AMArray e s -> m b)
  -> m (b, AArray e)
createAArray = createArrayM
