{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

-- |
-- Module      : Primal.Data.Array.Boxed
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module Primal.Data.Array.Boxed (
  BArray (..),
  BMArray (..),
  Size (..),

  -- * Immutable
  makeBArray,
  makeBArrayM,
  sizeOfBArray,
  indexBArray,

  -- * Mutable

  -- ** Create
  newBMArray,
  newRawBMArray,
  newLazyBMArray,
  makeBMArray,
  createBArrayM,
  createBArrayM_,
  getSizeOfBMArray,

  -- ** Access
  readBMArray,
  writeBMArray,
  writeLazyBMArray,
  writeDeepBMArray,

  -- *** Atomic
  casBMArray,
  atomicModifyFetchNewBMArray,
  atomicModifyFetchOldBMArray,
  atomicModifyBMArray,
  atomicModifyBMArray_,
  atomicModifyBMArray2,

  -- * Mutable
  thawBArray,
  thawCloneSliceBArray,
  freezeBMArray,
  freezeCloneSliceBMArray,
  copyBArray,
  moveBMArray,
  cloneBArray,
  cloneBMArray,

  -- * List
  fromListBArray,
  fromListBArrayN,
  toListBArray,

  -- * Helpers
  foldrBArray,
  traverseBArray,
) where

import Data.Bits
import qualified Primal.Container.Mutable.Array.Internal as I
import Primal.Container.Mutable.Ref.Atomic
import Primal.Container.Mutable.Ref.Internal
import Primal.Data.Array
import Primal.Foreign
import Primal.Monad

atomicModifyBMArray# :: Primal s m => BMArray e s -> Int -> (e -> (# e, b #)) -> m b
atomicModifyBMArray# ma@(BMArray ma#) i@(I# i#) f = do
  current0 <- readBMArray ma i
  primal $
    let go expected s =
          case f expected of
            (# new, artifact #) ->
              case casArray# ma# i# expected new s of
                (# s', 0#, _ #) -> (# s', artifact #)
                (# s', _, actual #) -> go actual s'
     in go current0
{-# INLINE atomicModifyBMArray# #-}

atomicModifyFetchNewBMArray :: Primal s m => BMArray e s -> Int -> (e -> e) -> m e
atomicModifyFetchNewBMArray ma i f =
  atomicModifyBMArray# ma i (\a -> let a' = f a in (# a', a' #))
{-# INLINE atomicModifyFetchNewBMArray #-}

-- atomicModifyFetchNewBMArray ma@(BMArray ma#) i@(I# i#) f = do
--   current0 <- readBMArray ma i
--   primal $ \s0 ->
--     let go expected s =
--           case casBArray# ma# i# expected (f expected) s of
--             (# s', 0#, actual #) -> go actual s'
--             (# s', _, current #) -> (# s', current #)
--     in go current0 s0
-- let go e =
--       casBMArray ma i e (f e) >>= \case
--         (True, new) -> pure new
--         (_, current) -> go current
--  in readBMArray ma i >>= go

atomicModifyFetchOldBMArray :: Primal s m => BMArray e s -> Int -> (e -> e) -> m e
atomicModifyFetchOldBMArray ma i f =
  atomicModifyBMArray# ma i (\a -> (# f a, a #))
{-# INLINE atomicModifyFetchOldBMArray #-}

-- let go e =
--       casBMArray ma i e (f e) >>= \case
--         (True, _new) -> pure e
--         (_, current) -> go current
--  in readBMArray ma i >>= go

atomicModifyBMArray :: Primal s m => BMArray e s -> Int -> (e -> (e, b)) -> m b
atomicModifyBMArray ma i f =
  atomicModifyBMArray# ma i (\a -> let (a', b) = f a in (# a', b #))
{-# INLINE atomicModifyBMArray #-}

-- let go e =
--       let (new, artifact) = f e
--        in casBMArray ma i e new >>= \case
--             (True, _new) -> pure artifact
--             (_, current) -> go current
--  in readBMArray ma i >>= go

atomicModifyBMArray_ :: Primal s m => BMArray e s -> Int -> (e -> e) -> m ()
atomicModifyBMArray_ ma i f =
  atomicModifyBMArray# ma i (\a -> let a' = f a in (# a', () #))
{-# INLINE atomicModifyBMArray_ #-}

atomicModifyBMArray2 :: Primal s m => BMArray e s -> Int -> (e -> (e, b)) -> m (e, e, b)
atomicModifyBMArray2 ma i f =
  atomicModifyBMArray# ma i (\a -> let (a', b) = f a in (# a', (a, a', b) #))
{-# INLINE atomicModifyBMArray2 #-}

-- | Strict right fold
foldrBArray :: (e -> b -> b) -> b -> BArray e -> b
foldrBArray = I.foldrArray
{-# INLINE foldrBArray #-}

makeBArray :: Size -> (Int -> e) -> BArray e
makeBArray = I.makeArray
{-# INLINE makeBArray #-}

makeBArrayM :: Primal s m => Size -> (Int -> m e) -> m (BArray e)
makeBArrayM = I.makeArrayM
{-# INLINE makeBArrayM #-}

createBArrayM :: Primal s m => Size -> (BMArray e s -> m b) -> m (b, BArray e)
createBArrayM = I.createArrayM
{-# INLINE createBArrayM #-}

createBArrayM_ :: Primal s m => Size -> (BMArray e s -> m b) -> m (BArray e)
createBArrayM_ = I.createArrayM_
{-# INLINE createBArrayM_ #-}

-- | Traverse an array with a monadic action.
--
-- @since 0.1.0
traverseBArray :: Primal s m => (e -> m b) -> BArray e -> m (BArray b)
traverseBArray = I.traverseArray
{-# INLINE traverseBArray #-}
