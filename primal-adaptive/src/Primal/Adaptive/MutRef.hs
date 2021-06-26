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
  ( AMRef(..)
  , AdaptMRef(..)
  , AdaptAtomicMRef
  , newAMRef
  ) where

import Primal.Monad
import Primal.Prim
import Primal.Container.Mutable.Ref
import Primal.Container.Mutable.Ref.Atomic
import Primal.Data.Ref
import Data.Prim.Adaptive.Rep
import Primal.Container.Internal

type ABWrap e = AWrap (AdaptRep Ref e) (IsAtomic e) e

newtype AMRef e s = AMRef (AdaptRep Ref e (ABWrap e) s)


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

type instance Elt AMRef e = ()

instance AdaptMRef e => MRef AMRef e where
  newRawMRef = AMRef <$> newRawMRef
  {-# INLINE newRawMRef #-}
  newMRef e = AMRef <$> newMRef (wrap e)
  {-# INLINE newMRef #-}
  readMRef (AMRef ma) = unwrap <$> readMRef ma
  {-# INLINE readMRef #-}
  writeMRef (AMRef ma) e = writeMRef ma (wrap e)
  {-# INLINE writeMRef #-}

instance AdaptAtomicMRef e => AtomicMRef AMRef e where
  atomicReadMRef (AMRef mut) = unwrap <$> atomicReadMRef mut
  {-# INLINE atomicReadMRef #-}
  atomicWriteMRef (AMRef mut) = atomicWriteMRef mut . wrap
  {-# INLINE atomicWriteMRef #-}
  casMRef (AMRef mut) old new = coerce <$> casMRef mut (wrap old) (wrap new)
  {-# INLINE casMRef #-}

newAMRef :: (MonadPrim s m, AdaptMRef e) => e -> m (AMRef e s)
newAMRef = newMRef


foo :: (MonadPrim s m, AdaptAtomicMRef b) => b -> m b
foo i = do
  ref <- newAMRef i
  atomicReadMRef ref
