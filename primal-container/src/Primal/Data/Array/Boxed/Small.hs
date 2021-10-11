{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Primal.Data.Array.Boxed.Small
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Data.Array.Boxed.Small
  ( SBArray(..)
  , SBMArray(..)
  , Size(..)
  -- * Immutable
  , makeSBArray
  , makeSBArrayM
  , sizeOfSBArray
  , indexSBArray
  -- * Mutable
  -- ** Create
  , newSBMArray
  , newRawSBMArray
  , newLazySBMArray
  , makeSBMArray
  , createSBArrayM
  , createSBArrayM_
  , getSizeOfSBMArray
  -- ** Access
  , readSBMArray
  , writeSBMArray
  , writeLazySBMArray
  , writeDeepSBMArray
  -- *** Atomic
  , casSBMArray
  , atomicModifyFetchNewSBMArray
  , atomicModifyFetchOldSBMArray
  , atomicModifySBMArray
  , atomicModifySBMArray_
  , atomicModifySBMArray2
  -- *
  , thawSBArray
  , thawCloneSliceSBArray
  , freezeSBMArray
  , freezeCloneSliceSBMArray
  , copySBArray
  , moveSBMArray
  , cloneSBArray
  , cloneSBMArray
  -- * List
  , fromListSBArray
  , fromListSBArrayN
  , toListSBArray
  -- * Helpers
  , foldrSBArray
  , traverseSBArray
  ) where

import Primal.Monad
import Data.Bits
import Primal.Data.Array
import qualified Primal.Container.Mutable.Array.Internal as I
import Primal.Container.Mutable.Ref.Atomic
import Primal.Container.Mutable.Ref.Internal
import Primal.Foreign







atomicModifySBMArray# :: Primal s m => SBMArray e s -> Int -> (e -> (# e, b #)) -> m b
atomicModifySBMArray# ma@(SBMArray ma#) i@(I# i#) f = do
  current0 <- readSBMArray ma i
  primal $
    let go expected s =
          case f expected of
            (# new, artifact #) ->
              case casSmallArray# ma# i# expected new s of
                (# s', 0#, _ #)     -> (# s', artifact #)
                (# s', _, actual #) -> go actual s'
     in go current0
{-# INLINE atomicModifySBMArray# #-}


atomicModifyFetchNewSBMArray :: Primal s m => SBMArray e s -> Int -> (e -> e) -> m e
atomicModifyFetchNewSBMArray ma i f =
  atomicModifySBMArray# ma i (\a -> let a' = f a in (# a', a' #))
{-# INLINE atomicModifyFetchNewSBMArray #-}

atomicModifyFetchOldSBMArray :: Primal s m => SBMArray e s -> Int -> (e -> e) -> m e
atomicModifyFetchOldSBMArray ma i f =
  atomicModifySBMArray# ma i (\a -> (# f a, a #))
{-# INLINE atomicModifyFetchOldSBMArray #-}


atomicModifySBMArray :: Primal s m => SBMArray e s -> Int -> (e -> (e, b)) -> m b
atomicModifySBMArray ma i f =
  atomicModifySBMArray# ma i (\a -> let (a', b) = f a in (# a', b #))
{-# INLINE atomicModifySBMArray #-}


atomicModifySBMArray_ :: Primal s m => SBMArray e s -> Int -> (e -> e) -> m ()
atomicModifySBMArray_ ma i f =
  atomicModifySBMArray# ma i (\a -> let a' = f a in (# a', () #))
{-# INLINE atomicModifySBMArray_ #-}


atomicModifySBMArray2 :: Primal s m => SBMArray e s -> Int -> (e -> (e, b)) -> m (e, e, b)
atomicModifySBMArray2 ma i f =
  atomicModifySBMArray# ma i (\a -> let (a', b) = f a in (# a', (a, a', b) #))
{-# INLINE atomicModifySBMArray2 #-}


-- | Strict right fold
foldrSBArray :: (e -> b -> b) -> b -> SBArray e -> b
foldrSBArray = I.foldrArray
{-# INLINE foldrSBArray #-}

makeSBArray :: Size -> (Int -> e) -> SBArray e
makeSBArray = I.makeArray
{-# INLINE makeSBArray #-}

makeSBArrayM :: Primal s m => Size -> (Int -> m e) -> m (SBArray e)
makeSBArrayM = I.makeArrayM
{-# INLINE makeSBArrayM #-}

createSBArrayM :: Primal s m => Size -> (SBMArray e s -> m b) -> m (b, SBArray e)
createSBArrayM = I.createArrayM
{-# INLINE createSBArrayM #-}

createSBArrayM_ :: Primal s m => Size -> (SBMArray e s -> m b) -> m (SBArray e)
createSBArrayM_ = I.createArrayM_
{-# INLINE createSBArrayM_ #-}


-- | Traverse an array with a monadic action.
--
-- @since 0.1.0
traverseSBArray :: Primal s m => (e -> m b) -> SBArray e -> m (SBArray b)
traverseSBArray = I.traverseArray
{-# INLINE traverseSBArray #-}
