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
  , Adaptive
  , newMArray
  , createArrayM
  , Atom(..)
  ) where

import Control.Prim.Monad
import Data.Coerce
import Data.Prim
import Data.Prim.Adaptive.Rep
import Data.Prim.Array (Size(..))
import qualified Data.Prim.MArray as M
import qualified Data.Prim.MArray.Boxed as B
import Data.Prim.MRef

type ABWrap e = AWrap (AdaptRep B.BMArray e) (IsAtomic e) e

type ABRep e = AdaptRep B.BMArray e (ABWrap e)

newtype AArray e = AArray (M.Array (ABRep e))

newtype AMArray e s = AMArray (ABRep e s)


class (Coercible e (Elt (ABRep e)), M.MArray (ABRep e)) => Adaptive e where
  wrap :: e -> Elt (ABRep e)
  unwrap :: Elt (ABRep e) -> e

instance (Coercible e (Elt (ABRep e)), M.MArray (ABRep e)) => Adaptive e where
  wrap = coerce
  unwrap = coerce

instance Adaptive e => MRef (AMArray e) where
  type Elt (AMArray e) = e
  newRawMRef = AMArray <$> newRawMRef
  {-# INLINE newRawMRef #-}
  newMRef e = AMArray <$> newMRef (wrap e)
  {-# INLINE newMRef #-}
  readMRef (AMArray ma) = unwrap <$> readMRef ma
  {-# INLINE readMRef #-}
  writeMRef (AMArray ma) e = writeMRef ma (wrap e)
  {-# INLINE writeMRef #-}


instance Adaptive e => M.MArray (AMArray e) where
  type Array (AMArray e) = AArray e
  sizeOfArray (AArray a) = M.sizeOfArray a
  {-# INLINE sizeOfArray #-}
  indexArray (AArray a) i = unwrap (M.indexArray a i)
  {-# INLINE indexArray #-}
  getSizeOfMArray (AMArray ma) = M.getSizeOfMArray ma
  {-# INLINE getSizeOfMArray #-}
  thawArray (AArray a) = AMArray <$> M.thawArray a
  {-# INLINE thawArray #-}
  freezeMArray (AMArray ma) = AArray <$> M.freezeMArray ma
  {-# INLINE freezeMArray #-}
  newRawMArray = fmap AMArray . M.newRawMArray
  {-# INLINE newRawMArray #-}
  readMArray (AMArray ma) i = unwrap <$> M.readMArray ma i
  {-# INLINE readMArray #-}
  writeMArray (AMArray ma) i e = M.writeMArray ma i (wrap e)
  {-# INLINE writeMArray #-}
  copyArray (AArray as) os (AMArray mad) = M.copyArray as os mad
  {-# INLINE copyArray #-}
  moveMArray (AMArray mas) os (AMArray mad) = M.moveMArray mas os mad
  {-# INLINE moveMArray #-}
  setMArray (AMArray ma) i sz e = M.setMArray ma i sz (wrap e)
  {-# INLINE setMArray #-}


newMArray :: (MonadPrim s m, Adaptive e) => Size -> e -> m (AMArray e s)
newMArray = M.newMArray

createArrayM ::
     (MonadPrim s m, Adaptive e)
  => Size
  -> (AMArray e s -> m b)
  -> m (b, AArray e)
createArrayM = M.createArrayM
