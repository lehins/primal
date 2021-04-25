{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      : Primal.Mutable.Ord
-- Copyright   : (c) Alexey Kuleshevich 2020-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Mutable.Ord where

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


compareFrozen :: (MutFreeze f, MutOrd f) => Frozen f -> Frozen f -> Ordering
compareFrozen fa fb =
  runST $ do
    ma <- thaw fa
    mb <- thaw fb
    compareMut ma mb
{-# INLINE compareFrozen #-}


compareMut ::
     forall mut m s. (MutOrd mut, MonadPrim s m)
  => mut s
  -> mut s
  -> m Ordering
compareMut ma mb = liftST $ compareMutST ma mb
{-# INLINE compareMut #-}
