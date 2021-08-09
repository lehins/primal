{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Primal.Mutable.Freeze
-- Copyright   : (c) Alexey Kuleshevich 2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Mutable.Freeze
  ( thaw
  , thawClone
  , freezeMut
  , freezeCloneMut
  , clone
  , cloneMut
  , Frozen
  , MutFreeze
    ( thawST
    , freezeMutST
    , cloneMutST
    , thawCloneST
    , freezeCloneMutST
    )
  ) where

import Primal.Monad
import Primal.Array
import Primal.Foreign
import Data.Text.Array as T

-- | Injective type family that relates the frozen and thawed types together into
-- one-to-one correspondance. Their kind should be the same, except additional type
-- argument for the state token for the mutable data type.
--
-- @since 1.0.0
type family Frozen (thawed :: k -> *) = (frozen :: k) | frozen -> thawed

-- | A type class that allows for going between frozen and thawed states for a mutable
-- data structure.
class MutFreeze mut where
  {-# MINIMAL thawST, freezeMutST, (clone|thawCloneST) #-}

  -- | See `thawMut` for documentation.
  --
  -- @since 1.0.0
  thawST :: Frozen mut -> ST s (mut s)

  -- | See `freezeMut` for documentation.
  --
  -- @since 1.0.0
  freezeMutST :: mut s -> ST s (Frozen mut)

  -- | Make an exact copy of the immutable type.
  --
  -- @since 1.0.0
  clone :: Frozen mut -> Frozen mut
  clone frozen = runST $ thawCloneST frozen >>= freezeMutST
  {-# INLINE clone #-}

  -- | See `cloneMut` for documentation.
  --
  -- @since 1.0.0
  cloneMutST :: mut s -> ST s (mut s)
  cloneMutST mut = thawST . clone =<< freezeMutST mut
  {-# INLINE cloneMutST #-}

  -- | See `thawClone` for documentation.
  --
  -- @since 1.0.0
  thawCloneST :: Frozen mut -> ST s (mut s)
  thawCloneST = thawST . clone
  {-# INLINE thawCloneST #-}

  -- | See `freezeCloneMut` for documentation.
  --
  -- @since 1.0.0
  freezeCloneMutST :: mut s -> ST s (Frozen mut)
  freezeCloneMutST = cloneMutST >=> freezeMutST
  {-# INLINE freezeCloneMutST #-}



type instance Frozen (BMArray e) = BArray e

instance MutFreeze (BMArray e) where
  thawST = thawBArray
  {-# INLINE thawST #-}
  clone arr = cloneSliceBArray arr 0 (sizeOfBArray arr)
  {-# INLINE clone #-}
  thawCloneST arr = thawCopyBArray arr 0 (sizeOfBArray arr)
  {-# INLINE thawCloneST #-}
  freezeMutST = freezeBMArray
  {-# INLINE freezeMutST #-}
  cloneMutST marr = getSizeOfBMArray marr >>= cloneSliceBMArray marr 0
  {-# INLINE cloneMutST #-}
  freezeCloneMutST marr = getSizeOfBMArray marr >>= freezeCopyBMArray marr 0
  {-# INLINE freezeCloneMutST #-}

type instance Frozen (SBMArray e) = SBArray e

instance MutFreeze (SBMArray e) where
  thawST = thawSBArray
  {-# INLINE thawST #-}
  clone arr = cloneSliceSBArray arr 0 (sizeOfSBArray arr)
  {-# INLINE clone #-}
  thawCloneST arr = thawCopySBArray arr 0 (sizeOfSBArray arr)
  {-# INLINE thawCloneST #-}
  freezeMutST = freezeSBMArray
  {-# INLINE freezeMutST #-}
  cloneMutST marr = getSizeOfSBMArray marr >>= cloneSliceSBMArray marr 0
  {-# INLINE cloneMutST #-}
  freezeCloneMutST marr = getSizeOfSBMArray marr >>= freezeCopySBMArray marr 0
  {-# INLINE freezeCloneMutST #-}


type instance Frozen (UMArray e) = UArray e

instance MutFreeze (UMArray e) where
  thawST = thawUArray
  {-# INLINE thawST #-}
  thawCloneST (UArray arr#) = do
    let n# = sizeofByteArray# arr#
    ST $ \s ->
      case newByteArray# n# s of
        (# s', marr# #) -> (# copyByteArray# arr# 0# marr# 0# n# s', UMArray marr# #)
  {-# INLINE thawCloneST #-}
  freezeMutST = freezeUMArray
  {-# INLINE freezeMutST #-}


type instance Frozen (UBMArray e) = UBArray (Frozen e)

instance (MutFreeze e, MutUnlift e, Unlift (Frozen e)) => MutFreeze (UBMArray e) where
  thawST a = makeMutUBMArray (sizeOfUBArray a) (thawST . indexUBArray a)
  {-# INLINE thawST #-}
  thawCloneST a = makeMutUBMArray (sizeOfUBArray a) (thawCloneST . indexUBArray a)
  {-# INLINE thawCloneST #-}
  freezeMutST ma = do
    sz <- getSizeOfUBMArray ma
    ma' <- makeUBMArray sz (readMutUBMArray ma >=> freezeMutST)
    freezeUBMArray ma'
  {-# INLINE freezeMutST #-}
  freezeCloneMutST ma = do
    sz <- getSizeOfUBMArray ma
    ma' <- makeUBMArray sz (readMutUBMArray ma >=> freezeCloneMutST)
    freezeUBMArray ma'
  {-# INLINE freezeCloneMutST #-}
  clone a =
    runST (makeUBMArray (sizeOfUBArray a) (pure . clone . indexUBArray a) >>= freezeUBMArray)
  {-# INLINE clone #-}
  cloneMutST ma = do
    sz <- getSizeOfUBMArray ma
    makeMutUBMArray sz (readMutUBMArray ma)
  {-# INLINE cloneMutST #-}


-- | Convert a pure immutable type into the corresponding mutable one. Most likely
-- it will be implemented as type cast without any data copy.
--
-- [Unsafe] This function is extremly unsafe, because not only it can make it possible to
-- break referential transparency, but also ghc can float thawing operation in such a way
-- that same buffer is used in in different thawing operations. Also any subsequent
-- destructive operation to the returned mutable type will also be reflected in the source
-- immutable type as well. Use `thawClone` instead, which avoids these problem with fresh
-- allocation and efficient data copy.
--
-- @since 1.0.0
thaw ::
     forall mut m s. (MutFreeze mut, Primal s m)
  => Frozen mut
  -> m (mut s)
thaw = liftST . thawST
{-# INLINE thaw #-}

-- | Convert a mutable type into the corresponding immutable one. Most likely it
-- will be implemented as type cast without any data copy.
--
-- [Unsafe] This function can make it possible to break referential transparency,
-- because any subsequent destructive operation to the source mutable type will also
-- be reflected in the result immutable type as well. Use `freezeCloneMut` instead,
-- which avoids this problem with fresh allocation and efficient data copy.
--
-- @since 1.0.0
freezeMut ::
     forall mut m s. (MutFreeze mut, Primal s m)
  => mut s
  -> m (Frozen mut)
freezeMut = liftST . freezeMutST
{-# INLINE freezeMut #-}


-- | Make an exact copy of the mutable type.
--
-- @since 1.0.0
cloneMut ::
     forall mut m s. (MutFreeze mut, Primal s m)
  => mut s
  -> m (mut s)
cloneMut = liftST . cloneMutST
{-# INLINE cloneMut #-}

-- | Convert an exact copy of an immutable type into the corresponding mutable
-- one. Unlike `thaw`, this function does copy all of the data.
--
-- @since 1.0.0
thawClone ::
     forall mut m s. (MutFreeze mut, Primal s m)
  => Frozen mut
  -> m (mut s)
thawClone = liftST . thawCloneST
{-# INLINE thawClone #-}

-- | Convert an exact copy of a mutable type into the corresponding immutable
-- one. Unlike `freezeMut`, this function does copy all of the data.
--
-- @since 1.0.0
freezeCloneMut ::
     forall mut m s. (MutFreeze mut, Primal s m)
  => mut s
  -> m (Frozen mut)
freezeCloneMut = liftST . freezeCloneMutST
{-# INLINE freezeCloneMut #-}


type instance Frozen T.MArray = T.Array

instance MutFreeze T.MArray where
  thawST a = toTextMArray <$> thawST (fromTextArray a)
  {-# INLINE thawST #-}
  thawCloneST a = toTextMArray <$> thawCloneST (fromTextArray a)
  {-# INLINE thawCloneST #-}
  freezeMutST = T.unsafeFreeze
  {-# INLINE freezeMutST #-}
