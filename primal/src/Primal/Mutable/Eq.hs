{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      : Primal.Mutable.Eq
-- Copyright   : (c) Alexey Kuleshevich 2020-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Mutable.Eq where

import Primal.Array
import Primal.Ref
import Primal.Monad
import Primal.Mutable.Freeze


class MutEq mut where
  eqMut :: MonadPrim s m => mut s -> mut s -> m Bool
  default eqMut :: (Eq (Frozen mut), MutFreeze mut, MonadPrim s m) => mut s -> mut s -> m Bool
  eqMut ma mb = (==) <$> freezeMut ma <*> freezeMut mb
  {-# INLINE eqMut #-}

instance Eq e => MutEq (BRef e) where
  eqMut m1 m2 = (==) <$> readBRef m1 <*> readBRef m2
  {-# INLINE eqMut #-}


instance Eq e => MutEq (BMArray e) where
  eqMut m1 m2 = liftST $ eqWithST isSameBMArray getSizeOfBMArray readBMArray m1 m2
  {-# INLINE eqMut #-}

instance Eq e => MutEq (SBMArray e) where
  eqMut m1 m2 = liftST $ eqWithST isSameSBMArray getSizeOfSBMArray readSBMArray m1 m2
  {-# INLINE eqMut #-}

instance (Prim e, Eq e) => MutEq (UMArray e) where
  eqMut m1 m2 = liftST $ eqWithST isSameUMArray getSizeOfUMArray readUMArray m1 m2
  {-# INLINE eqMut #-}

eqFrozen :: (MutFreeze f, MutEq f) => Frozen f -> Frozen f -> Bool
eqFrozen fa fb =
  runST $ do
    ma <- thaw fa
    mb <- thaw fb
    eqMut ma mb
{-# INLINE eqFrozen #-}
