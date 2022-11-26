{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- |
-- Module      : Data.Prim.Adaptive.MArray
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module Data.Prim.Adaptive.MArray (
  AArray (..),
  AMArray (..),
  newAMArray,
  createAArrayM,
  Atom (..),
) where

import Data.Coerce
import Data.Prim.Adaptive.Rep
import Primal.Container.Internal
import Primal.Container.Mutable.Array
import Primal.Container.Mutable.Ref
import Primal.Data.Array
import Primal.Monad
import Primal.Prim

type ABWrap e = AWrap (AdaptRep BMArray e) (IsAtomic e) e

newtype AArray e = AArray (Frozen (AdaptRep BMArray e) (ABWrap e))

newtype AMArray e s = AMArray (AdaptRep BMArray e (ABWrap e) s)

class (Coercible e (ABWrap e), MArray (AdaptRep BMArray e) (ABWrap e)) => AdaptMArray e where
  wrap :: e -> ABWrap e
  unwrap :: ABWrap e -> e

instance (Coercible e (ABWrap e), MArray (AdaptRep BMArray e) (ABWrap e)) => AdaptMArray e where
  wrap = coerce
  {-# INLINE wrap #-}
  unwrap = coerce
  {-# INLINE unwrap #-}

type instance Elt AArray e = (AdaptMArray e)
type instance Elt AMArray e = (AdaptMArray e)

instance AdaptMArray e => MRef AMArray e where
  newRawMRef = AMArray <$> newRawMRef
  {-# INLINE newRawMRef #-}
  newMRef e = AMArray <$> newMRef (wrap e)
  {-# INLINE newMRef #-}
  readMRef (AMArray ma) = unwrap <$> readMRef ma
  {-# INLINE readMRef #-}
  writeMRef (AMArray ma) e = writeMRef ma (wrap e)
  {-# INLINE writeMRef #-}

type instance Frozen AMArray = AArray

instance AdaptMArray e => MArray AMArray e where
  sizeOfArray (AArray a) = sizeOfArray a
  {-# INLINE sizeOfArray #-}
  indexArray (AArray a) i = unwrap (indexArray a i)
  {-# INLINE indexArray #-}
  getSizeOfMArray (AMArray ma) = getSizeOfMArray ma
  {-# INLINE getSizeOfMArray #-}
  thawArray (AArray a) = AMArray <$> thawArray a
  {-# INLINE thawArray #-}
  freezeMArray (AMArray ma) = AArray <$> freezeMArray ma
  {-# INLINE freezeMArray #-}
  newRawMArray = fmap AMArray . newRawMArray
  {-# INLINE newRawMArray #-}
  readMArray (AMArray ma) i = unwrap <$> readMArray ma i
  {-# INLINE readMArray #-}
  writeMArray (AMArray ma) i e = writeMArray ma i (wrap e)
  {-# INLINE writeMArray #-}
  copyArray (AArray as) os (AMArray mad) = copyArray as os mad
  {-# INLINE copyArray #-}
  moveMArray (AMArray mas) os (AMArray mad) = moveMArray mas os mad
  {-# INLINE moveMArray #-}
  setMArray (AMArray ma) i sz e = setMArray ma i sz (wrap e)
  {-# INLINE setMArray #-}

newAMArray :: (Primal s m, AdaptMArray e) => Size -> e -> m (AMArray e s)
newAMArray = newMArray

createAArrayM
  :: (Primal s m, AdaptMArray e)
  => Size
  -> (AMArray e s -> m b)
  -> m (b, AArray e)
createAArrayM = createArrayM
