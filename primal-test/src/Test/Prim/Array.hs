{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
-- |
-- Module      : Test.Prim.Atomic
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--

module Test.Prim.Array where

import Control.Prim.Monad
import Data.Prim
import Data.Coerce
import Data.Prim.Atomic
import Test.Hspec
import Test.QuickCheck
import Data.Kind
import Data.Prim.Array.Boxed (Size(..))
import qualified Data.Prim.Ref as Ref
import qualified Data.Prim.Array.Boxed as Boxed
import qualified Data.Prim.Bytes.Addr as Addr

class NoConstraint a
instance NoConstraint a

class Mut mut where
  type Elt mut :: * -> Constraint
  type Elt mut = NoConstraint

  newMut :: (Elt mut a, MonadPrim s m) => a -> m (mut a s)
  newMut a = do
    mut <- newRawMut
    mut <$ writeMut mut a

  newRawMut :: (Elt mut a, MonadPrim s m) => m (mut a s)

  readMut :: (Elt mut a, MonadPrim s m) => mut a s -> m a

  writeMut :: (Elt mut a, MonadPrim s m) => mut a s -> a -> m ()


class Mut mut => MutAtomic mut where
  type EltAtomic mut :: * -> Constraint
  type EltAtomic mut = NoConstraint

  atomicReadMut :: (EltAtomic mut a, MonadPrim s m) => mut a s -> m a

  atomicWriteMut :: (EltAtomic mut a, MonadPrim s m) => mut a s -> a -> m ()

  casMut :: MonadPrim s m => mut a s -> a -> a -> m (Bool, a)

class Mut mut => MutArray mut where
  type Frozen mut = (r :: Type -> Type) | r -> mut

  getSizeOfMArray :: (Elt mut a, MonadPrim s m) => mut a s -> m Size

  thawArray :: (Elt mut a, MonadPrim s m) => Frozen mut a -> m (mut a s)

  -- thawCopyArray
  -- freezeCopyMArray

  freezeMArray :: (Elt mut a, MonadPrim s m) => mut a s -> m (Frozen mut a)

  newMArray :: (Elt mut a, MonadPrim s m) => Size -> a -> m (mut a s)
  newMArray n a = do
    ma <- newRawMArray n
    ma <$ setMArray ma 0 n a

  newRawMArray :: (Elt mut a, MonadPrim s m) => Size -> m (mut a s)

  readMArray :: (Elt mut a, MonadPrim s m) => mut a s -> Int -> m a

  writeMArray :: (Elt mut a, MonadPrim s m) => mut a s -> Int -> a -> m ()

  setMArray :: (Elt mut a, MonadPrim s m) => mut a s -> Int -> Size -> a -> m ()
  setMArray ma i0 (Size n0) x =
    let n = n0 + i0
        go i | i < n = writeMArray ma i x >> go (i + 1)
             | otherwise = pure ()
    in go i0

  -- copyArray
  -- copyMArray
  -- cloneArray
  -- cloneMArray




instance Mut Boxed.MArray where
  newMut = Boxed.newMArray 1
  newRawMut = Boxed.newRawMArray 1
  readMut ma = Boxed.readMArray ma 0
  writeMut ma = Boxed.writeMArray ma 0

instance MutArray Boxed.MArray where
  type Frozen Boxed.MArray = Boxed.Array

  getSizeOfMArray = pure . Boxed.sizeOfMArray

  thawArray = Boxed.thawArray

  freezeMArray = Boxed.freezeMArray

  newMArray = Boxed.newMArray

  newRawMArray = Boxed.newRawMArray

  readMArray = Boxed.readMArray

  writeMArray = Boxed.writeMArray


instance Mut Addr.MAddr where
  type Elt Addr.MAddr = Prim
  newRawMut = Addr.allocMAddr 1
  readMut = Addr.readMAddr
  writeMut = Addr.writeMAddr

instance MutArray Addr.MAddr where
  type Frozen Addr.MAddr = Addr.Addr

  getSizeOfMArray = fmap coerce . Addr.getCountOfMAddr

  thawArray = Addr.thawAddr

  freezeMArray = Addr.freezeMAddr

  newRawMArray sz = Addr.allocMAddr (coerce sz)

  readMArray m i = Addr.readOffMAddr m (coerce i)

  writeMArray m i = Addr.writeOffMAddr m (coerce i)

  setMArray m i o x = Addr.setMAddr m (coerce i) (coerce o) x


--data PRef a s where

data NEMArrayIx ma a s = MArrayIx !Int !(ma a s)


instance MutArray ma => Mut (NEMArrayIx ma) where
  type Elt (NEMArrayIx ma) = Elt ma
  newMut a = MArrayIx 0 <$> newMut a
  newRawMut = MArrayIx 0 <$> newRawMut
  readMut (MArrayIx i ma) = readMArray ma i
  writeMut (MArrayIx i ma) = writeMArray ma i



