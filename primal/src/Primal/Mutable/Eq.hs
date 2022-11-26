{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}

-- |
-- Module      : Primal.Mutable.Eq
-- Copyright   : (c) Alexey Kuleshevich 2020-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module Primal.Mutable.Eq (
  MutEq (..),
  eqMut,
  eqFrozen,

  -- * Helpers
  eqMutWithST,
) where

import qualified Data.Text.Array as T
import Primal.Array
import Primal.Monad
import Primal.Mutable.Freeze
import Primal.Ref

class MutEq mut where
  eqMutST :: mut s -> mut s -> ST s Bool
  default eqMutST :: (Eq (Frozen mut), MutFreeze mut) => mut s -> mut s -> ST s Bool
  eqMutST ma mb = (==) <$> freezeMut ma <*> freezeMut mb
  {-# INLINE eqMutST #-}

instance Eq e => MutEq (BRef e) where
  eqMutST m1 m2 = (==) <$> readBRef m1 <*> readBRef m2
  {-# INLINE eqMutST #-}

instance (Unbox e, Eq e) => MutEq (URef e) where
  eqMutST m1 m2 = (==) <$> readURef m1 <*> readURef m2
  {-# INLINE eqMutST #-}

instance Eq e => MutEq (BMArray e) where
  eqMutST m1 m2 = eqWithST isSameBMArray getSizeOfBMArray readBMArray m1 m2
  {-# INLINE eqMutST #-}

instance Eq e => MutEq (SBMArray e) where
  eqMutST m1 m2 = eqWithST isSameSBMArray getSizeOfSBMArray readSBMArray m1 m2
  {-# INLINE eqMutST #-}

instance (Unbox e, Eq e) => MutEq (UMArray e) where
  eqMutST m1 m2 = eqWithST isSameUMArray getSizeOfUMArray readUMArray m1 m2
  {-# INLINE eqMutST #-}

instance (Unlift e, Eq e) => MutEq (UBMArray e) where
  eqMutST m1 m2 = eqWithST isSameUBMArray getSizeOfUBMArray readUBMArray m1 m2
  {-# INLINE eqMutST #-}

instance (MutUnlift e, MutEq e) => MutEq (UBMArray e) where
  eqMutST m1 m2 = eqMutWithST isSameUBMArray getSizeOfUBMArray readMutUBMArray m1 m2
  {-# INLINE eqMutST #-}

instance MutEq T.MArray where
  eqMutST ma1 ma2 = eqMutST (fromTextMArray ma1) (fromTextMArray ma2)
  {-# INLINE eqMutST #-}

-- | Check two mutable data types for equality.
--
-- @since 1.0.0
eqMut
  :: forall mut m s
   . (MutEq mut, Primal s m)
  => mut s
  -> mut s
  -> m Bool
eqMut m1 m2 = liftST $ eqMutST m1 m2
{-# INLINE eqMut #-}

-- | Check two frozen versions of mutable data types for equality. Useful for creating
-- `Eq` instances for immutable tpyes that have corresponding mutable versions.
--
-- @since 1.0.0
eqFrozen :: (MutFreeze f, MutEq f) => Frozen f -> Frozen f -> Bool
eqFrozen fa fb =
  runST $ do
    ma <- thaw fa
    mb <- thaw fb
    eqMutST ma mb
{-# INLINE eqFrozen #-}

-- | Check for equality of two mutable arrays with mutable contents
--
-- @since 1.0.0
eqMutWithST
  :: MutEq e
  => (a e s -> a e s -> Bool)
  -- ^ Pointer equality
  -> (a e s -> ST s Size)
  -- ^ Get the size of array
  -> (a e s -> Int -> ST s (e s))
  -- ^ Read an element from the mutable array
  -> a e s
  -- ^ First array
  -> a e s
  -- ^ Second array
  -> ST s Bool
eqMutWithST isSameMut getSizeOfMut readMut ma1 ma2
  | isSameMut ma1 ma2 = pure True
  | otherwise = do
      sz1@(Size n) <- getSizeOfMut ma1
      sz2 <- getSizeOfMut ma2
      let loop i
            | i < n = do
                x1 <- readMut ma1 i
                x2 <- readMut ma2 i
                eltEq <- eqMutST x1 x2
                if eltEq
                  then loop (i + 1)
                  else pure False
            | otherwise = pure True
      if sz1 /= sz2
        then pure False
        else loop 0
{-# INLINE eqMutWithST #-}
