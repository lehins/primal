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
import Data.Coerce
import Data.Prim.Atomic
import Test.Hspec
import Test.QuickCheck
import Data.Kind
import Data.Prim.Array.Internal
import qualified Data.Prim.Ref as Ref
import qualified Data.Prim.Array.Boxed as Boxed
import qualified Data.Prim.Bytes.Addr as Addr


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


instance Prim a => MRef Addr.MAddr a where
  newRawMRef = Addr.allocMAddr 1
  readMRef = Addr.readMAddr
  writeMRef = Addr.writeMAddr

instance Prim a => MArray (Addr.MAddr a) where
  type IArray (Addr.MAddr a) = Addr.Addr a
  type Elt (Addr.MAddr a) = a

  getSizeOfMArray = fmap coerce . Addr.getCountOfMAddr

  thawArray = Addr.thawAddr

  freezeMArray = Addr.freezeMAddr

  newRawMArray sz = Addr.allocMAddr (coerce sz)

  readMArray m i = Addr.readOffMAddr m (coerce i)

  writeMArray m i = Addr.writeOffMAddr m (coerce i)

  setMArray m i o x = Addr.setMAddr m (coerce i) (coerce o) x

type family BestRep r1 r2 where
  BestRep Addr.MAddr Addr.MAddr = Addr.MAddr
  BestRep r1 r2 = r1

type family Rep a :: Type -> Type -> Type where
  Rep Int = Addr.MAddr
  Rep Word = Addr.MAddr
  Rep (Maybe a) = Rep a
  Rep (Either a b) = BestRep (Rep a) (Rep b)
  Rep (a, b) = BestRep (Rep a) (Rep b)
  Rep (a, b, c) = BestRep (BestRep (Rep a) (Rep b)) (Rep c)
  Rep (a, b, c, d) = BestRep (Rep (a, b)) (Rep (c, d))
  Rep a = Ref.Ref

newtype PRef a s = PRef (Rep a a s)

instance MRef (Rep a) a => MRef PRef a where
  newRawMRef = PRef <$> newRawMRef
  readMRef (PRef ref) = readMRef ref
  writeMRef (PRef ref) = writeMRef ref

class MRef (Rep a) a => PrimRef a

instance MRef (Rep a) a => PrimRef a


newPRef ::
     forall a m s. (MonadPrim s m, PrimRef a)
  => a -- ^ A value to initialize the PRef with
  -> m (PRef a s)
newPRef = newMRef

data NEMArrayIx ma a s = MArrayIx !Int !(ma a s)


instance (a ~ Elt (ma a), MArray (ma a)) => MRef (NEMArrayIx ma) a where
  newMRef a = MArrayIx 0 <$> newMArray 1 a
  newRawMRef = MArrayIx 0 <$> newRawMArray 1
  readMRef (MArrayIx i ma) = readMArray ma i
  writeMRef (MArrayIx i ma) = writeMArray ma i



