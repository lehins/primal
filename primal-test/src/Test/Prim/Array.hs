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
--import qualified Data.Prim.Memory.ByteArray as ByteArray


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

type family BestRep r1 r2 where
  BestRep r1 Addr.MAddr = r1
  BestRep r1 r2 = r2

type family ARep def a :: Type -> Type -> Type where
  ARep def Int = Addr.MAddr
  ARep def Word = Addr.MAddr
  ARep def a = def


type family PRep def a :: Type -> Type -> Type where
  PRep def Int = Addr.MAddr
  PRep def Word = Addr.MAddr
  PRep def (Maybe a) = PRep def a
  PRep def (Either a b) = BestRep (PRep def a) (PRep def b)
  PRep def (a, b) = BestRep (PRep def a) (PRep def b)
  PRep def (a, b, c) = BestRep (BestRep (PRep def a) (PRep def b)) (PRep def c)
  PRep def (a, b, c, d) = BestRep (PRep def (a, b)) (PRep def (c, d))
  PRep def (a, b, c, d, e) = BestRep (PRep def (a, b, c)) (PRep def (d, e))
  PRep def (a, b, c, d, e, f) = BestRep (PRep def (a, b, c)) (PRep def (d, e, f))
  PRep def (a, b, c, d, e, f, g) = BestRep (PRep def (a, b, c, d)) (PRep def (e, f, g))
  PRep def (a, b, c, d, e, f, g, h) = BestRep (PRep def (a, b, c, d)) (PRep def (e, f, g, h))
  PRep def (a, b, c, d, e, f, g, h, i) = BestRep (PRep def (a, b, c, d, e)) (PRep def (f, g, h, i))
  PRep def a = def


instance MRef Ref.Ref a where
  newRawMRef = Ref.newRefLazy undefined
  readMRef = Ref.readRef
  writeMRef = Ref.writeRef


newtype PRef a s = PRef (PRep Ref.Ref a a s)

-- instance MRef (PRep a) a => MRef PRef a where
--   newRawMRef = PRef <$> newRawMRef
--   readMRef (PRef ref) = readMRef ref
--   writeMRef (PRef ref) = writeMRef ref

class MRef (PRep Ref.Ref a) a => PRefRep a

instance MRef (PRep Ref.Ref a) a => PRefRep a



-- newPRef ::
--      (MonadPrim s m, PRefRep a)
--   => a -- ^ A value to initialize the PRef with
--   -> m (PRef a s)
-- newPRef = newMRef

newPRef ::
     (MonadPrim s m, PRefRep a)
  => a -- ^ A value to initialize the PRef with
  -> m (PRef a s)
newPRef = newPRef

readPRef ::
     (MonadPrim s m, PRefRep a)
  => PRef a s
  -> m a
readPRef (PRef ref) = readMRef ref

bar :: (Num a, MonadPrim s m, PRefRep (Maybe a)) => m (Maybe a)
bar = do
  r' <- newPRef Nothing
  readPRef r'


instance AtomicMRef Ref.Ref a where
  atomicReadMRef = Ref.atomicReadRef

  atomicWriteMRef = Ref.atomicWriteRef

  casMRef = Ref.casRef


instance Atomic a => AtomicMRef Addr.MAddr a where
  atomicReadMRef addr = Addr.atomicReadOffMAddr addr 0

  atomicWriteMRef addr = Addr.atomicWriteOffMAddr addr 0

  casMRef addr old new = do
    actual <- Addr.casOffMAddr addr 0 old new
    return (actual == old, actual)



data NEMArrayIx ma a s = MArrayIx !Int !(ma a s)


instance (a ~ Elt (ma a), MArray (ma a)) => MRef (NEMArrayIx ma) a where
  newMRef a = MArrayIx 0 <$> newMArray 1 a
  newRawMRef = MArrayIx 0 <$> newRawMArray 1
  readMRef (MArrayIx i ma) = readMArray ma i
  writeMRef (MArrayIx i ma) = writeMArray ma i



