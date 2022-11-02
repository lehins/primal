{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
#if __GLASGOW_HASKELL__ >= 801
{-# LANGUAGE UndecidableSuperClasses #-}
#endif
-- |
-- Module      : Primal.Container.Foldable
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Container.Foldable where

import Primal.Monad
import Primal.Monad.Throw
import Primal.Ref
import Primal.Foreign
import Primal.Unbox
import Primal.Array



type family Elt (f :: k) a :: Constraint


-- | Corresponds to a `pure` from applicative
class Elt f a => Singleton (f :: * -> *) a where
  singleton :: a -> f a

-- | Corresponds to a Functor
class Elt f a => Lift (f :: * -> *) a where
  -- app :: Lift f b => [a -> b] -> f a -> f b

  lift :: Lift f b => (a -> b) -> f a -> f b

  lift2 :: (Lift f b, Lift f c) => (a -> b -> c) -> f a -> f b -> f c

  lift3 :: (Lift f b, Lift f c, Lift f d) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d

type instance Elt Maybe a = ()

instance Singleton Maybe a where
  singleton = Just

instance Lift Maybe a where
  lift _ Nothing = Nothing
  lift f (Just a) = Just (f a)

  lift2 f (Just a) (Just b) = Just (f a b)
  lift2 _ _ _ = Nothing

  lift3 f (Just a) (Just b) (Just c) = Just (f a b c)
  lift3 _ _ _ _ = Nothing



type instance Elt UArray a = Unbox a

instance Unbox a => Singleton UArray a where
  singleton = fromListUArray . pure

instance Unbox a => Lift UArray a where
  lift f = fromListUArray . map f . toListUArray

  lift2 f a b = fromListUArray $ zipWith f (toListUArray a) (toListUArray b)

  lift3 f a b c = fromListUArray $ zipWith3 f (toListUArray a) (toListUArray b) (toListUArray c)


type instance Elt [] a = ()

instance Singleton [] a where
  singleton = pure

instance Lift [] a where
  lift = map
  lift2 = zipWith
  lift3 = zipWith3


class Elt f a => MSingleton (f :: * -> * -> *) a where
  msingleton :: Primal s m => a -> m (f a s)

class Elt f a => MLift (f :: * -> * -> *) a where
  liftInSituM :: Primal s m => (a -> m a) -> f a s -> m ()

  mliftM :: (Primal s m, MLift f b)
        => (a -> m b) -> f a s -> m (f b s)
  mlift2M :: (Primal s m, MLift f b, MLift f c)
         => (a -> b -> m c) -> f a s -> f b s -> m (f c s)
  mlift3M :: (Primal s m, MLift f b, MLift f c, MLift f d)
         => (a -> b -> c -> m d) -> f a s -> f b s -> f c s -> m (f d s)

class Elt f e => ReduceInhab (f :: * -> *) e where
  reduceInhab :: Semigroup e => f e -> e
  mapReduceInhab :: Semigroup a => (e -> a) -> f e -> a
  reducelInhab :: (e -> e -> e) -> f e -> e
  reducerInhab :: (e -> e -> e) -> f e -> e


class Elt f e => Reduce (f :: * -> *) e where
  reduce :: Monoid e => f e -> e
  mapReduce :: Monoid a => (e -> a) -> f e -> a
  reducel :: (a -> e -> a) -> a -> f e -> a
  reducer :: (e -> a -> a) -> a -> f e -> a
  reducel2 :: Throws m => (a -> e -> c -> a) -> a -> f e -> f c -> m a

class Elt f e => MutReduce (f :: * -> * -> *) e where
  reduceMut :: Monoid e => f e s -> m e
  mapReduceMut :: Monoid a => (e -> a) -> f e s -> m a
  mapReduceMutM :: Monoid a => (e -> m a) -> f e s -> m a
  reducelMutM :: Primal s m => (a -> e -> m a) -> a -> f e s -> m a
  reducerMutM :: Primal s m => (e -> a -> m a) -> a -> f e s -> m a


class Elt f e => MReduce (f :: * -> * -> *) e where
  mreduceM :: Monoid e => f e s -> m e
  mmapReduceM :: Monoid a => (e -> a) -> f e s -> m a
  mreducelM :: Primal s m => (a -> e -> m a) -> a -> f e s -> m a
  mreducerM :: Primal s m => (e -> a -> m a) -> a -> f e s -> m a

  mliftM_ :: Primal s m
          => (e -> m b) -> f a s -> m ()
  mlift2M_ :: (Primal s m, MReduce f b)
           => (e -> b -> m c) -> f e s -> f b s -> m ()
  mlift3M_ :: (Primal s m, MReduce f b, MReduce f c)
           => (e -> b -> c -> m d) -> f e s -> f b s -> f c s -> m ()


class Elt f e => Filter (f :: * -> *) e where
  liftMaybe :: (e -> Maybe a) -> f e -> f a

  filter :: (e -> Bool) -> f e -> f e
  filter f = liftMaybe (\x -> if f x then Just x else Nothing)



class Elt f e => MFilter (f :: * -> * -> *) e where
  mliftMaybeM :: Primal s m => (e -> m (Maybe a)) -> f e s -> m (f a s)

  mfilterM :: Primal s m => (e -> m Bool) -> f e s -> m (f e s)
  mfilterM f = mliftMaybeM (\x -> (\y -> if y then Just x else Nothing) <$> f x)

-- instance FoldMut BRef e where
--   foldlMutM f acc ref = readBRef ref >>= f acc


-- instance KeyPFunctor [] Int a b where
--   pilift f = zipWith f [0..]




-- type FunctorF f = forall a b. PFunctor f a b

-- class PFunctor (f :: * -> *) a b where
--   plift :: (a -> b) -> f a -> f b

-- class PPure (f :: * -> *) a where
--   ppure :: a -> f a

-- -- class (PPure f a, PPure f b, PFunctor f a b) => PApplicative (f :: * -> *) a b where
-- --   plift2 :: (a -> b -> c) -> f a -> f b -> f c

-- instance PFunctor Maybe a b where
--   plift _ Nothing = Nothing
--   plift f (Just x) = Just (f x)
-- class PFunctor f a b => KeyPFunctor (f :: * -> *) k a b | f -> k where
--   pilift :: (k -> a -> b) -> f a -> f b

-- instance (Unbox a, Unbox b) => PFunctor UArray a b where
--   plift f = fromListUArray . map f . toListUArray

-- instance (Unbox a, Unbox b) => KeyPFunctor UArray Int a b where
--   pilift f = fromListUArray . zipWith f [0..] . toListUArray

-- instance PFunctor [] a b where
--   plift = map

-- instance KeyPFunctor [] Int a b where
--   pilift f = zipWith f [0..]

-- instance PFunctor IO a b where
--   plift = fmap

class MFunctor (f :: * -> * -> *) a b where
  pliftMut :: (a -> m b) -> f a s -> f b s
class MFunctor f a b => KeyMFunctor (f :: * -> * -> *) k a b | f -> k where
  piliftMut :: (k -> a -> m b) -> f a s -> f b s


-- class MutFoldable (f :: * -> * -> *) e where
-- class MFoldable (f :: * -> * -> *) e where

class FoldMut (f :: * -> * -> *) e where
  foldlMutM :: Primal s m => (a -> e -> m a) -> a -> f e s -> m a


instance FoldMut BRef e where
  foldlMutM f acc ref = readBRef ref >>= f acc


instance Unbox e => FoldMut UMArray e where
  foldlMutM f initAcc marr = do
    Size k <- getSizeOfUMArray marr
    let go acc i
          | i < k = do
            acc' <- f acc =<< readUMArray marr i
            go acc' (i + 1)
          | otherwise = pure acc
    go initAcc 0


foldlMut :: (FoldMut f e, Primal s m) => (a -> e -> a) -> a -> f e s -> m a
foldlMut f = foldlMutM (\a x -> pure $! f a x)

toListMut :: (FoldMut f e, Primal s m) => f e s -> m [e]
toListMut = foldlMut (flip (:)) []
