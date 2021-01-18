{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-- |
-- Module      : Data.Prim.Adaptive.MRef
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Adaptive.MRef
  ( ARef(..)
  , AdaptMRef(..)
  , AdaptAtomicMRef
  , module Data.Prim.Adaptive.MRef
  ) where

import Primal.Monad
import Primal.Prim
import Primal.Container.Mutable.Ref
import Primal.Container.Mutable.Ref.Atomic
import Primal.Data.Ref
import Data.Prim.Adaptive.Rep
import Primal.Container.Internal

type ABWrap e = AWrap (AdaptRep Ref e) (IsAtomic e) e

type ABRep e = AdaptRep Ref e (ABWrap e)

newtype ARef e s = ARef (ABRep e s)


class (Coercible e (ABWrap e), MRef (AdaptRep Ref e) (ABWrap e)) => AdaptMRef e where
  wrap :: e -> ABWrap e
  unwrap :: ABWrap e -> e

instance (Coercible e (ABWrap e), MRef (AdaptRep Ref e) (ABWrap e)) => AdaptMRef e where
  wrap = coerce
  {-# INLINE wrap #-}
  unwrap = coerce
  {-# INLINE unwrap #-}

class (AdaptMRef e, AtomicMRef (AdaptRep Ref e) (ABWrap e)) => AdaptAtomicMRef e

instance (AdaptMRef e, AtomicMRef (AdaptRep Ref e) (ABWrap e)) => AdaptAtomicMRef e

type instance Elt ARef e = ()

instance AdaptMRef e => MRef ARef e where
  newRawMRef = ARef <$> newRawMRef
  {-# INLINE newRawMRef #-}
  newMRef e = ARef <$> newMRef (wrap e)
  {-# INLINE newMRef #-}
  readMRef (ARef ma) = unwrap <$> readMRef ma
  {-# INLINE readMRef #-}
  writeMRef (ARef ma) e = writeMRef ma (wrap e)
  {-# INLINE writeMRef #-}

instance AdaptAtomicMRef e => AtomicMRef ARef e where
  atomicReadMRef (ARef mut) = unwrap <$> atomicReadMRef mut
  {-# INLINE atomicReadMRef #-}
  atomicWriteMRef (ARef mut) = atomicWriteMRef mut . wrap
  {-# INLINE atomicWriteMRef #-}
  casMRef (ARef mut) old new = coerce <$> casMRef mut (wrap old) (wrap new)
  {-# INLINE casMRef #-}

newARef :: (MonadPrim s m, AdaptMRef e) => e -> m (ARef e s)
newARef = newMRef

foo :: (MonadPrim s m, AdaptAtomicMRef b) => b -> m b
foo i = do
  ref <- newARef i
  atomicReadMRef ref
