{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Prim.Adaptive.MArray
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Adaptive.MArray
  (
    AArray(..)
  , AMArray(..)
  , AdaptRep
  , createAArray
  ) where

import Control.Prim.Monad
import Data.Prim.MArray
import Data.Prim.MRef
import qualified Data.Prim.MArray.Boxed as Boxed
import qualified Data.Prim.Adaptive.Rep as Rep


newtype AArray a = AArray (Array (Rep.AdaptRep Boxed.MBArray a a))

newtype AMArray a s = AMArray (Rep.AdaptRep Boxed.MBArray a a s)

class MArray (Rep.AdaptRep Boxed.MBArray e e) => AdaptRep e
instance MArray (Rep.AdaptRep Boxed.MBArray e e) => AdaptRep e

instance AdaptRep e => MRef (AMArray e) where
  type Elt (AMArray e) = Elt (Rep.AdaptRep Boxed.MBArray e e)
  newRawMRef = AMArray <$> newRawMRef
  {-# INLINE newRawMRef #-}
  readMRef (AMArray ma) = readMRef ma
  {-# INLINE readMRef #-}
  writeMRef (AMArray ma) = writeMRef ma
  {-# INLINE writeMRef #-}


instance AdaptRep e => MArray (AMArray e) where
  type Array (AMArray e) = AArray e
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
