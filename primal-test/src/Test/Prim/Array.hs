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
import Data.Prim.Atomic
import Test.Hspec
import Test.QuickCheck
import Data.Kind
import Data.Prim.Array.Boxed (Size(..))
import qualified Data.Prim.Array.Boxed as Boxed

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

  newRawMArray :: (Elt mut a, MonadPrim s m) => Size -> m (mut a s)

  readMArray :: (Elt mut a, MonadPrim s m) => mut a s -> Int -> m a

  writeMArray :: (Elt mut a, MonadPrim s m) => mut a s -> Int -> a -> m ()

  -- setMArray
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



data NEMArrayIx ma a s = MArrayIx !Int !(ma a s)


instance MutArray ma => Mut (NEMArrayIx ma) where
  type Elt (NEMArrayIx ma) = Elt ma
  newMut a = MArrayIx 0 <$> newMut a
  newRawMut = MArrayIx 0 <$> newRawMut
  readMut (MArrayIx i ma) = readMArray ma i
  writeMut (MArrayIx i ma) = writeMArray ma i



