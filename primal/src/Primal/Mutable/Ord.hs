{-# LANGUAGE DefaultSignatures #-}
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

import Primal.Mutable.Eq
import Primal.Mutable.Freeze
import Primal.Monad


class MutEq mut => MutOrd mut where
  compareMut :: MonadPrim s m => mut s -> mut s -> m Ordering
  default compareMut :: (Ord (Frozen mut), MutFreeze mut, MonadPrim s m) =>
    mut s -> mut s -> m Ordering
  compareMut ma mb = compare <$> freezeMut ma <*> freezeMut mb
  {-# INLINE compareMut #-}

compareFrozen :: (MutFreeze f, MutOrd f) => Frozen f -> Frozen f -> Ordering
compareFrozen fa fb =
  runST $ do
    ma <- thaw fa
    mb <- thaw fb
    compareMut ma mb
{-# INLINE compareFrozen #-}
