{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module      : Test.Prim.Atomic
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--

module Test.Prim.Array
  ( module Test.Prim.Array
  ) where

import Control.Prim.Monad
import Data.Prim
import Data.Kind
import Data.Prim.Array.Internal
import qualified Data.Prim.Ref as Ref
import qualified Data.Prim.Array.Boxed as Boxed
import qualified Data.Prim.Array.Unboxed as Unboxed
import qualified Data.Prim.Memory.Addr as Addr


class MRef mut a where

  newMRef :: MonadPrim s m => a -> m (mut a s)
  newMRef a = newRawMRef >>= \mut -> mut <$ writeMRef mut a

  newRawMRef :: MonadPrim s m => m (mut a s)

  readMRef :: MonadPrim s m => mut a s -> m a

  writeMRef :: MonadPrim s m => mut a s -> a -> m ()

class MRefLazy mut a where

  newMRefLazy :: MonadPrim s m => a -> m (mut a s)

  writeMRefLazy :: MonadPrim s m => mut a s -> a -> m ()


class MRef mut a => AtomicMRef mut a where

  atomicReadMRef :: MonadPrim s m => mut a s -> m a

  atomicWriteMRef :: MonadPrim s m => mut a s -> a -> m ()

  casMRef :: MonadPrim s m => mut a s -> a -> a -> m (Bool, a)




instance MRef Boxed.BoxedMArray a where
  newMRef = Boxed.newMArray 1
  newRawMRef = Boxed.newRawMArray 1
  readMRef ma = Boxed.readMArray ma 0
  writeMRef ma = Boxed.writeMArray ma 0

instance Prim a => MRef Unboxed.UnboxedMArray a where
  newMRef = Unboxed.newMArray 1
  newRawMRef = Unboxed.newRawMArray 1
  readMRef ma = Unboxed.readMArray ma 0
  writeMRef ma = Unboxed.writeMArray ma 0


instance Prim a => MRef Addr.MAddr a where
  newRawMRef = Addr.allocMAddr 1
  readMRef = Addr.readMAddr
  writeMRef = Addr.writeMAddr

type family BestPRep r1 r2 where
  BestPRep Addr.MAddr Addr.MAddr = Addr.MAddr
  BestPRep r1 r2 = r1

type family PRep a :: Type -> Type -> Type where
  PRep Int = Addr.MAddr
  PRep Word = Addr.MAddr
  PRep (Maybe a) = PRep a
  PRep (Either a b) = BestPRep (PRep a) (PRep b)
  PRep (a, b) = BestPRep (PRep a) (PRep b)
  PRep (a, b, c) = BestPRep (BestPRep (PRep a) (PRep b)) (PRep c)
  PRep (a, b, c, d) = BestPRep (PRep (a, b)) (PRep (c, d))
  PRep a = Ref.Ref

newtype PRef a s = PRef (PRep a a s)

instance MRef (PRep a) a => MRef PRef a where
  newRawMRef = PRef <$> newRawMRef
  readMRef (PRef ref) = readMRef ref
  writeMRef (PRef ref) = writeMRef ref

class MRef (PRep a) a => PrimRep a

instance MRef (PRep a) a => PrimRep a


newPRef ::
     forall a m s. (MonadPrim s m, PrimRep a)
  => a -- ^ A value to initialize the PRef with
  -> m (PRef a s)
newPRef = newMRef

data NEMArrayIx ma a s = MArrayIx !Int !(ma a s)


instance (a ~ Elt (ma a), MArray (ma a)) => MRef (NEMArrayIx ma) a where
  newMRef a = MArrayIx 0 <$> newMArray 1 a
  newRawMRef = MArrayIx 0 <$> newRawMArray 1
  readMRef (MArrayIx i ma) = readMArray ma i
  writeMRef (MArrayIx i ma) = writeMArray ma i



