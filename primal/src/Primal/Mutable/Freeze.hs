{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
-- |
-- Module      : Primal.Mutable.Freeze
-- Copyright   : (c) Alexey Kuleshevich 2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Mutable.Freeze where

import Primal.Monad
import Primal.Array

type family Frozen (mut :: k -> *) = (frozen :: k) | frozen -> mut

class MutFreeze mut where
  {-# MINIMAL thaw, freezeMut, (clone|thawClone) #-}

  -- | Convert a pure immutable type into the corresponding mutable one.  Most likely
  -- it will be implemented as type cast without any data copy.
  --
  -- [Unsafe] This function might make it possible to break referential transparency,
  -- because any subsequent destructive operation to the returned mutable type will
  -- also be reflected in the source immutable type as well. Use `thawClone` instead,
  -- which avoids this problem with fresh allocation and efficient data copy.
  --
  -- @since 1.0.0
  thaw :: MonadPrim s m => Frozen mut -> m (mut s)

  -- | Convert a mutable type into the corresponding immutable one. Most likely it
  -- will be implemented as type cast without any data copy.
  --
  -- [Unsafe] This function might make it possible to break referential transparency,
  -- because any subsequent destructive operation to the source mutable type will also
  -- be reflected in the result immutable type as well. Use `freezeCloneMut` instead,
  -- which avoids this problem with fresh allocation and efficient data copy.
  --
  -- @since 1.0.0
  freezeMut :: MonadPrim s m => mut s -> m (Frozen mut)

  -- | Make an exact copy of the immutable type.
  --
  -- @since 1.0.0
  clone :: Frozen mut -> Frozen mut
  clone frozen = runST $ thawClone frozen >>= freezeMut
  {-# INLINE clone #-}

  -- | Make an exact copy of the mutable type.
  --
  -- @since 1.0.0
  cloneMut :: MonadPrim s m => mut s -> m (mut s)
  cloneMut mut = thaw . clone =<< freezeMut mut
  {-# INLINE cloneMut #-}

  -- | Convert an exact copy of an immutable type into the corresponding mutable
  -- one. Unlike `thaw`, this function does copy all of the data.
  --
  -- @since 1.0.0
  thawClone :: MonadPrim s m => Frozen mut -> m (mut s)
  thawClone = thaw . clone
  {-# INLINE thawClone #-}

  -- | Convert an exact copy of a mutable type into the corresponding immutable
  -- one. Unlike `freezeMut`, this function does copy all of the data.
  --
  -- @since 1.0.0
  freezeCloneMut :: MonadPrim s m => mut s -> m (Frozen mut)
  freezeCloneMut = cloneMut >=> freezeMut
  {-# INLINE freezeCloneMut #-}

type instance Frozen (BMArray e) = BArray e

instance MutFreeze (BMArray e) where
  thaw = thawBArray
  {-# INLINE thaw #-}
  clone arr = cloneSliceBArray arr 0 (sizeOfBArray arr)
  {-# INLINE clone #-}
  thawClone arr = thawCopyBArray arr 0 (sizeOfBArray arr)
  {-# INLINE thawClone #-}
  freezeMut = freezeBMArray
  {-# INLINE freezeMut #-}
  cloneMut marr = getSizeOfBMArray marr >>= cloneSliceBMArray marr 0
  {-# INLINE cloneMut #-}
  freezeCloneMut marr = getSizeOfBMArray marr >>= freezeCopyBMArray marr 0
  {-# INLINE freezeCloneMut #-}

type instance Frozen (SBMArray e) = SBArray e

instance MutFreeze (SBMArray e) where
  thaw = thawSBArray
  {-# INLINE thaw #-}
  clone arr = cloneSliceSBArray arr 0 (sizeOfSBArray arr)
  {-# INLINE clone #-}
  thawClone arr = thawCopySBArray arr 0 (sizeOfSBArray arr)
  {-# INLINE thawClone #-}
  freezeMut = freezeSBMArray
  {-# INLINE freezeMut #-}
  cloneMut marr = getSizeOfSBMArray marr >>= cloneSliceSBMArray marr 0
  {-# INLINE cloneMut #-}
  freezeCloneMut marr = getSizeOfSBMArray marr >>= freezeCopySBMArray marr 0
  {-# INLINE freezeCloneMut #-}


type instance Frozen (UMArray e) = UArray e

instance Prim e => MutFreeze (UMArray e) where
  thaw = thawUArray
  {-# INLINE thaw #-}
  -- clone arr = cloneSliceUArray arr 0 (sizeOfUArray arr)
  -- {-# INLINE clone #-}
  thawClone arr = thawCopyUArray arr 0 (sizeOfUArray arr)
  {-# INLINE thawClone #-}
  freezeMut = freezeUMArray
  {-# INLINE freezeMut #-}
  -- cloneMut marr = getSizeOfUMArray marr >>= cloneSliceUMArray marr 0
  -- {-# INLINE cloneMut #-}
  -- freezeCloneMut marr = getSizeOfUMArray marr >>= freezeCopyUMArray marr 0
  -- {-# INLINE freezeCloneMut #-}
