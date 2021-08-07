{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
-- |
-- Module      : Primal.Mutable.Ord
-- Copyright   : (c) Alexey Kuleshevich 2020-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Mutable.Ord where

import qualified Data.Text.Array as T
import Primal.Array
import Primal.Monad
import Primal.Mutable.Eq
import Primal.Mutable.Freeze
import Primal.Ref


class MutEq mut => MutOrd mut where
  compareMutST :: mut s -> mut s -> ST s Ordering
  default compareMutST :: (Ord (Frozen mut), MutFreeze mut) => mut s -> mut s -> ST s Ordering
  compareMutST ma mb = compare <$> freezeMut ma <*> freezeMut mb
  {-# INLINE compareMutST #-}


instance Ord e => MutOrd (BRef e) where
  compareMutST m1 m2 = compare <$> readBRef m1 <*> readBRef m2
  {-# INLINE compareMutST #-}

instance (Unbox e, Ord e) => MutOrd (URef e) where
  compareMutST m1 m2 = compare <$> readURef m1 <*> readURef m2
  {-# INLINE compareMutST #-}


instance Ord e => MutOrd (BMArray e) where
  compareMutST m1 m2 = compareWithST isSameBMArray getSizeOfBMArray readBMArray m1 m2
  {-# INLINE compareMutST #-}

instance Ord e => MutOrd (SBMArray e) where
  compareMutST m1 m2 = compareWithST isSameSBMArray getSizeOfSBMArray readSBMArray m1 m2
  {-# INLINE compareMutST #-}

instance (Unbox e, Ord e) => MutOrd (UMArray e) where
  compareMutST m1 m2 = compareWithST isSameUMArray getSizeOfUMArray readUMArray m1 m2
  {-# INLINE compareMutST #-}

instance (Unlift e, Ord e) => MutOrd (UBMArray e) where
  compareMutST m1 m2 = compareWithST isSameUBMArray getSizeOfUBMArray readUBMArray m1 m2
  {-# INLINE compareMutST #-}

instance (MutUnlift e, MutOrd e) => MutOrd (UBMArray e) where
  compareMutST m1 m2 = compareMutWithST isSameUBMArray getSizeOfUBMArray readMutUBMArray m1 m2
  {-# INLINE compareMutST #-}

instance MutOrd T.MArray where
  compareMutST ma1 ma2 = compareMutST (fromTextMArray ma1) (fromTextMArray ma2)
  {-# INLINE compareMutST #-}


compareFrozen :: (MutFreeze f, MutOrd f) => Frozen f -> Frozen f -> Ordering
compareFrozen fa fb =
  runST $ do
    ma <- thaw fa
    mb <- thaw fb
    compareMut ma mb
{-# INLINE compareFrozen #-}


compareMut ::
     forall mut m s. (MutOrd mut, Primal s m)
  => mut s
  -> mut s
  -> m Ordering
compareMut ma mb = liftST $ compareMutST ma mb
{-# INLINE compareMut #-}


-- | Compare two mutable arrays using supplied functions, where contents themselves are mutable.
--
-- @since 1.0.0
compareMutWithST ::
     MutOrd e
  => (a e s -> a e s -> Bool) -- ^ Pointer equality
  -> (a e s -> ST s Size) -- ^ Get the size of array
  -> (a e s -> Int -> ST s (e s)) -- ^ Read an element from the mutable array
  -> a e s -- ^ First array
  -> a e s -- ^ Second array
  -> ST s Ordering
compareMutWithST isSameMut getSizeOfMut readMut ma1 ma2
  | isSameMut ma1 ma2 = pure EQ
  | otherwise = do
    sz1@(Size n) <- getSizeOfMut ma1
    sz2 <- getSizeOfMut ma2
    let loop i
          | i < n = do
            x1 <- readMut ma1 i
            x2 <- readMut ma2 i
            compareMut x1 x2 >>= \case
              EQ -> loop (i + 1)
              c -> pure c
          | otherwise = pure $ compare sz1 sz2
    loop 0
{-# INLINE compareMutWithST #-}
