{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Prim.Adaptive.MRef
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Adaptive.MRef
  (
    ARef(..)
  , Adapt(..)
  , Atom(..)
  , module Data.Prim.Adaptive.MRef
  ) where

import Control.Prim.Monad
import Data.Prim
import Data.Prim.MArray.Internal
import Data.Prim.MRef
import Data.Prim.MRef.Atomic
import Data.Prim.MRef.Ref
import Data.Prim.Adaptive.Rep


type ABWrap e = AWrap (AdaptRep Ref e) (IsAtomic e) e

type ABRep e = AdaptRep Ref e (ABWrap e)

newtype ARef e s = ARef (ABRep e s)


class (Coercible e (Elt (ABRep e)), MRef (ABRep e)) => Adapt e where
  wrap :: e -> Elt (ABRep e)
  unwrap :: Elt (ABRep e) -> e

instance (Coercible e (Elt (ABRep e)), MRef (ABRep e)) => Adapt e where
  wrap = coerce
  unwrap = coerce

class (Adapt e, AtomicMRef (ABRep e)) => AdaptAtomic e

instance (Adapt e, AtomicMRef (ABRep e)) => AdaptAtomic e


instance Adapt e => MRef (ARef e) where
  type Elt (ARef e) = e
  newRawMRef = ARef <$> newRawMRef
  {-# INLINE newRawMRef #-}
  newMRef e = ARef <$> newMRef (wrap e)
  {-# INLINE newMRef #-}
  readMRef (ARef ma) = unwrap <$> readMRef ma
  {-# INLINE readMRef #-}
  writeMRef (ARef ma) e = writeMRef ma (wrap e)
  {-# INLINE writeMRef #-}

instance AdaptAtomic e => AtomicMRef (ARef e) where
  atomicReadMRef (ARef mut) = unwrap <$> atomicReadMRef mut
  {-# INLINE atomicReadMRef #-}
  atomicWriteMRef (ARef mut) = atomicWriteMRef mut . wrap
  {-# INLINE atomicWriteMRef #-}


newARef :: (MonadPrim s m, Adapt e) => e -> m (ARef e s)
newARef = newMRef

foo :: (MonadPrim s m, AdaptAtomic b) => b -> m b
foo i = do
  ref <- newARef i
  atomicReadMRef ref
