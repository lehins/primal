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
  (
    ARef(..)
  , Adapt(..)
  , Atom(..)
  , module Data.Prim.Adaptive.MRef
  ) where

import Primal.Monad
import Primal.Prim
import Primal.Prim.Class
import Primal.Container.Mutable.Ref
import Primal.Container.Mutable.Ref.Atomic
import Primal.Data.Ref
import Data.Prim.Adaptive.Rep
import Primal.Container.Internal


newtype ABWrap mc e = ABWrap (AWrap (AdaptRep (DefaultRep mc) e) (IsAtomic e) e)

instance Prim (AWrap (AdaptRep (DefaultRep mc) e) (IsAtomic e) e) => Prim (ABWrap mc e) where
  type PrimBase (ABWrap mc e) = AWrap (AdaptRep (DefaultRep mc) e) (IsAtomic e) e

type ABRep mc e s = AdaptRep (DefaultRep mc) e (ABWrap mc e) s

type family DefaultRep (mc :: * -> * -> *) :: * -> * -> *

type instance DefaultRep ARef = Ref

newtype ARef e s = ARef (ABRep ARef e s)


class (Coercible e (ABWrap mc e), MRef (AdaptRep (DefaultRep mc) e) (ABWrap mc e)) => Adapt mc e where
  wrap :: e -> ABWrap mc e
  wrap = coerce
  {-# INLINE wrap #-}
  unwrap :: ABWrap mc e -> e
  unwrap = coerce
  {-# INLINE unwrap #-}

instance ( Coercible e (ABWrap ARef e)
         , MRef (AdaptRep (DefaultRep ARef) e) (ABWrap ARef e)
         ) =>
         Adapt ARef e where


class (Adapt mc e, AtomicMRef (AdaptRep (DefaultRep mc) e) (ABWrap mc e)) => AdaptAtomic mc e

instance ( Adapt ARef e
         , AtomicMRef (AdaptRep (DefaultRep ARef) e) (ABWrap ARef e)
         ) =>
         AdaptAtomic ARef e

type instance Elt ARef e = ()

instance Adapt ARef e => MRef ARef e where
  newRawMRef = ARef <$> newRawMRef
  {-# INLINE newRawMRef #-}
  newMRef e = ARef <$> newMRef (wrap e)
  {-# INLINE newMRef #-}
  readMRef (ARef ma) = unwrap <$> readMRef ma
  {-# INLINE readMRef #-}
  writeMRef (ARef ma) e = writeMRef ma (wrap e)
  {-# INLINE writeMRef #-}

instance AdaptAtomic ARef e => AtomicMRef ARef e where
  atomicReadMRef (ARef mut) = unwrap <$> atomicReadMRef mut
  {-# INLINE atomicReadMRef #-}
  atomicWriteMRef (ARef mut) = atomicWriteMRef mut . wrap
  {-# INLINE atomicWriteMRef #-}
  -- casMRef (ARef mut) f = casMRef mut f
  -- {-# INLINE casMRef #-}

newARef :: (MonadPrim s m, Adapt ARef e) => e -> m (ARef e s)
newARef = newMRef


inc :: (MonadPrim s m, Enum e, Adapt ARef e) => e -> m (ARef e s)
inc e = do
  ref <- newARef e
  readMRef ref >>= writeMRef ref . succ
  pure ref
