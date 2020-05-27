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
  , MAArray(..)
  , Adaptive
  , newMArray
  , createArrayM
  , Atom(..)
  ) where

import Control.Prim.Monad
import Data.Coerce
import Data.Prim
import Data.Prim.Adaptive.Rep
import qualified Data.Prim.MArray as M
import qualified Data.Prim.MArray.Boxed as B
import Data.Prim.MRef

type ABWrap e = AWrap (AdaptRep B.MBArray e) (IsAtomic e) e

type ABRep e = AdaptRep B.MBArray e (ABWrap e)

newtype AArray e = AArray (M.Array (ABRep e))

newtype MAArray e s = MAArray (ABRep e s)


class (Coercible e (Elt (ABRep e)), M.MArray (ABRep e)) => Adaptive e where
  wrap :: e -> Elt (ABRep e)
  unwrap :: Elt (ABRep e) -> e

instance (Coercible e (Elt (ABRep e)), M.MArray (ABRep e)) => Adaptive e where
  wrap = coerce
  unwrap = coerce

instance Adaptive e => MRef (MAArray e) where
  type Elt (MAArray e) = e
  newRawMRef = MAArray <$> newRawMRef
  {-# INLINE newRawMRef #-}
  newMRef e = MAArray <$> newMRef (wrap e)
  {-# INLINE newMRef #-}
  readMRef (MAArray ma) = unwrap <$> readMRef ma
  {-# INLINE readMRef #-}
  writeMRef (MAArray ma) e = writeMRef ma (wrap e)
  {-# INLINE writeMRef #-}


instance Adaptive e => M.MArray (MAArray e) where
  type Array (MAArray e) = AArray e
  sizeOfArray (AArray a) = M.sizeOfArray a
  {-# INLINE sizeOfArray #-}
  indexArray (AArray a) i = unwrap (M.indexArray a i)
  {-# INLINE indexArray #-}
  getSizeOfMArray (MAArray ma) = M.getSizeOfMArray ma
  {-# INLINE getSizeOfMArray #-}
  thawArray (AArray a) = MAArray <$> M.thawArray a
  {-# INLINE thawArray #-}
  freezeMArray (MAArray ma) = AArray <$> M.freezeMArray ma
  {-# INLINE freezeMArray #-}
  newRawMArray = fmap MAArray . M.newRawMArray
  {-# INLINE newRawMArray #-}
  readMArray (MAArray ma) i = unwrap <$> M.readMArray ma i
  {-# INLINE readMArray #-}
  writeMArray (MAArray ma) i e = M.writeMArray ma i (wrap e)
  {-# INLINE writeMArray #-}
  copyArray (AArray as) os (MAArray mad) = M.copyArray as os mad
  {-# INLINE copyArray #-}
  moveMArray (MAArray mas) os (MAArray mad) = M.moveMArray mas os mad
  {-# INLINE moveMArray #-}
  setMArray (MAArray ma) i sz e = M.setMArray ma i sz (wrap e)
  {-# INLINE setMArray #-}


newMArray :: (MonadPrim s m, Adaptive e) => Size -> e -> m (MAArray e s)
newMArray = M.newMArray

createArrayM ::
     (MonadPrim s m, Adaptive e)
  => Size
  -> (MAArray e s -> m b)
  -> m (b, AArray e)
createArrayM = M.createArrayM
