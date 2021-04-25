{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      : Primal.Mutable.Eq
-- Copyright   : (c) Alexey Kuleshevich 2020-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Mutable.Eq
  ( MutEq(..)
  , eqMut
  , eqFrozen
  ) where

import Primal.Array
import Primal.Ref
import Primal.Monad
import Primal.Mutable.Freeze

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

-- | Check two mutable data types for equality.
--
-- @since 1.0.0
eqMut ::
     forall mut m s. (MutEq mut, MonadPrim s m)
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
