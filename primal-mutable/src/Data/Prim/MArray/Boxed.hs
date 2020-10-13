{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Data.Prim.MArray.Boxed
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.MArray.Boxed
  ( BArray(..)
  , BMArray(..)
  , Size(..)
  -- * Immutable
  , makeBArray
  , makeBArrayM
  , sizeOfBArray
  , indexBArray
  -- * Mutable
  -- ** Create
  , newBMArray
  , newRawBMArray
  , newLazyBMArray
  , makeBMArray
  , createBArrayM
  , createBArrayM_
  , sizeOfBMArray
  -- ** Access
  , readBMArray
  , writeBMArray
  , writeLazyBMArray
  , writeDeepBMArray
  -- *** Atomic
  , casBMArray
  , atomicModifyFetchNewBMArray
  , atomicModifyFetchOldBMArray
  , atomicModifyBMArray
  , atomicModifyBMArray_
  , atomicModifyBMArray2
  -- *
  , thawBArray
  , thawCopyBArray
  , freezeBMArray
  , freezeCopyBMArray
  , copyBArray
  , moveBMArray
  , cloneBArray
  , cloneBMArray
  -- * List
  , fromListBArray
  , fromListBArrayN
  , toListBArray
  -- * Helpers
  , foldrBArray
  , traverseBArray
  ) where

import Control.Monad.ST
import Control.Prim.Monad
import Data.Prim
import Data.Bits
import qualified Data.Prim.MArray.Internal as I
import Data.Prim.MRef.Atomic
import Data.Prim.MRef.Internal
import Foreign.Prim
import Data.Prim.Array


instance MRef (BMArray e) where
  type Elt (BMArray e) = e
  newRawMRef = newRawBMArray 1
  {-# INLINE newRawMRef #-}
  readMRef mba = readBMArray mba 0
  {-# INLINE readMRef #-}
  writeMRef mba = writeBMArray mba 0
  {-# INLINE writeMRef #-}
  newMRef = newBMArray 1
  {-# INLINE newMRef #-}


instance AtomicMRef (BMArray e) where
  casMRef mba = casBMArray mba 0
  {-# INLINE casMRef #-}

instance Num e => AtomicCountMRef (BMArray e)
instance Bits e => AtomicBitsMRef (BMArray e)

instance I.MArray (BMArray e) where
  type Array (BMArray e) = BArray e
  indexArray = indexBArray
  {-# INLINE indexArray #-}
  sizeOfArray = sizeOfBArray
  {-# INLINE sizeOfArray #-}
  getSizeOfMArray = pure . sizeOfBMArray
  {-# INLINE getSizeOfMArray #-}
  thawArray = thawBArray
  {-# INLINE thawArray #-}
  thawCopyArray = thawCopyBArray
  {-# INLINE thawCopyArray #-}
  freezeMArray = freezeBMArray
  {-# INLINE freezeMArray #-}
  freezeCopyMArray = freezeCopyBMArray
  {-# INLINE freezeCopyMArray #-}
  newRawMArray = newRawBMArray
  {-# INLINE newRawMArray #-}
  readMArray = readBMArray
  {-# INLINE readMArray #-}
  writeMArray = writeBMArray
  {-# INLINE writeMArray #-}
  newMArray = newBMArray
  {-# INLINE newMArray #-}
  copyArray = copyBArray
  {-# INLINE copyArray #-}
  moveMArray = moveBMArray
  {-# INLINE moveMArray #-}
  cloneArray = cloneBArray
  {-# INLINE cloneArray #-}
  cloneMArray = cloneBMArray
  {-# INLINE cloneMArray #-}








-- | /O(1)/ - Compare-and-swap operation that can be used as a concurrency primitive for
-- implementing atomic operations on mutable boxed arrays. Returns a boolean value, which
-- indicates `True` for success and `False` otherwise for the update, as well as the current
-- value at the supplied index. In case of success current value returned will be the newly
-- supplied one, otherwise it will still be the old one. Note that there is no `Eq`
-- constraint on the element, that is because compare operation is done on a reference,
-- rather than on the value itself, in other words the expected value must be the exact same
-- one.
--
-- Documentation for utilized primop: `casArray#`.
--
-- [Unsafe] Violation of @ix@ preconditions can result in heap corruption or a failure
-- with a segfault
--
-- ====__Examples__
--
-- >>> ma <- makeBMArray 5 (pure . (*10))
-- >>> freezeBMArray ma
-- Array [0,10,20,30,40]
--
-- A possible mistake is to try and pass the expected value, instead of an actual element:
--
-- >>> casBMArray ma 2 20 1000
-- (False,20)
-- >>> freezeBMArray ma
-- Array [0,10,20,30,40]
--
-- But this will get us nowhere, since what we really need is the actual reference to the
-- value currently in the array cell
--
-- >>> expected <- readBMArray ma 2
-- >>> r@(_, currentValue) <- casBMArray ma 2 expected 1000
-- >>> freezeBMArray ma
-- Array [0,10,1000,30,40]
-- >>> r
-- (True,1000)
--
-- In a concurrent setting current value can potentially be modified by some other
-- thread, therefore returned value can be immediately used as the expected one to the
-- next call, if we want to retry the atomic swap:
--
-- >>> casBMArray ma 2 currentValue 2000
-- (True,2000)
-- >>> freezeBMArray ma
-- Array [0,10,2000,30,40]
--
-- @since 0.3.0
casBMArray ::
     MonadPrim s m
  => BMArray e s
  -- ^ /dstMutArray/ - Mutable array that will have an atomic swap operation applied to
  -> Int
  -- ^ /ix/ - Index of a cell which should be set to the new value
  --
  -- /__Precoditions:__/
  --
  -- > 0 <= ix
  --
  -- > ix < unSize (sizeOfMBArray dstMutArray)
  -> e -- ^ /expElt/ - Reference to the expected boxed value
  -> e -- ^ /elt/ - New value to update the cell with
  -> m (Bool, e)
casBMArray (BMArray ma#) (I# i#) expected new =
  prim $ \s ->
    case casArray# ma# i# expected new s of
      (# s', failed#, actual #) -> (# s', (isTrue# (failed# ==# 0#), actual) #)
{-# INLINE casBMArray #-}




atomicModifyBMArray# :: MonadPrim s m => BMArray e s -> Int -> (e -> (# e, b #)) -> m b
atomicModifyBMArray# ma@(BMArray ma#) i@(I# i#) f = do
  current0 <- readBMArray ma i
  prim $
    let go expected s =
          case f expected of
            (# new, artifact #) ->
              case casArray# ma# i# expected new s of
                (# s', 0#, _ #)     -> (# s', artifact #)
                (# s', _, actual #) -> go actual s'
     in go current0
{-# INLINE atomicModifyBMArray# #-}


atomicModifyFetchNewBMArray :: MonadPrim s m => BMArray e s -> Int -> (e -> e) -> m e
atomicModifyFetchNewBMArray ma i f =
  atomicModifyBMArray# ma i (\a -> let a' = f a in (# a', a' #))
{-# INLINE atomicModifyFetchNewBMArray #-}

-- atomicModifyFetchNewBMArray ma@(BMArray ma#) i@(I# i#) f = do
--   current0 <- readBMArray ma i
--   prim $ \s0 ->
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

atomicModifyFetchOldBMArray :: MonadPrim s m => BMArray e s -> Int -> (e -> e) -> m e
atomicModifyFetchOldBMArray ma i f =
  atomicModifyBMArray# ma i (\a -> (# f a, a #))
{-# INLINE atomicModifyFetchOldBMArray #-}
  -- let go e =
  --       casBMArray ma i e (f e) >>= \case
  --         (True, _new) -> pure e
  --         (_, current) -> go current
  --  in readBMArray ma i >>= go



atomicModifyBMArray :: MonadPrim s m => BMArray e s -> Int -> (e -> (e, b)) -> m b
atomicModifyBMArray ma i f =
  atomicModifyBMArray# ma i (\a -> let (a', b) = f a in (# a', b #))
{-# INLINE atomicModifyBMArray #-}
  -- let go e =
  --       let (new, artifact) = f e
  --        in casBMArray ma i e new >>= \case
  --             (True, _new) -> pure artifact
  --             (_, current) -> go current
  --  in readBMArray ma i >>= go


atomicModifyBMArray_ :: MonadPrim s m => BMArray e s -> Int -> (e -> e) -> m ()
atomicModifyBMArray_ ma i f =
  atomicModifyBMArray# ma i (\a -> let a' = f a in (# a', () #))
{-# INLINE atomicModifyBMArray_ #-}


atomicModifyBMArray2 :: MonadPrim s m => BMArray e s -> Int -> (e -> (e, b)) -> m (e, e, b)
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

makeBArrayM :: MonadPrim s m => Size -> (Int -> m e) -> m (BArray e)
makeBArrayM = I.makeArrayM
{-# INLINE makeBArrayM #-}

createBArrayM :: MonadPrim s m => Size -> (BMArray e s -> m b) -> m (b, BArray e)
createBArrayM = I.createArrayM
{-# INLINE createBArrayM #-}

createBArrayM_ :: MonadPrim s m => Size -> (BMArray e s -> m b) -> m (BArray e)
createBArrayM_ = I.createArrayM_
{-# INLINE createBArrayM_ #-}


-- | Traverse an array with a monadic action.
--
-- @since 0.1.0
traverseBArray :: MonadPrim s m => (e -> m b) -> BArray e -> m (BArray b)
traverseBArray = I.traverseArray
{-# INLINE traverseBArray #-}
