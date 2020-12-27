{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Data.Prim.MArray.Boxed.Small
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.MArray.Boxed.Small
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
  , thawCopySBArray
  , freezeSBMArray
  , freezeCopySBMArray
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

import Control.Prim.Monad
import Data.Bits
import Data.Prim.Array
import qualified Data.Prim.MArray.Internal as I
import Data.Prim.MRef.Atomic
import Data.Prim.MRef.Internal
import Foreign.Prim

instance MRef (SBMArray e) where
  type Elt (SBMArray e) = e
  newRawMRef = newRawSBMArray 1
  {-# INLINE newRawMRef #-}
  readMRef ma = readSBMArray ma 0
  {-# INLINE readMRef #-}
  writeMRef ma = writeSBMArray ma 0
  {-# INLINE writeMRef #-}
  newMRef = newSBMArray 1
  {-# INLINE newMRef #-}

instance AtomicMRef (SBMArray e) where
  casMRef msba = casSBMArray msba 0
  {-# INLINE casMRef #-}

instance Num e => AtomicCountMRef (SBMArray e)
instance Bits e => AtomicBitsMRef (SBMArray e)


instance I.MArray (SBMArray e) where
  type Array (SBMArray e) = SBArray e
  indexArray = indexSBArray
  {-# INLINE indexArray #-}
  sizeOfArray = sizeOfSBArray
  {-# INLINE sizeOfArray #-}
  getSizeOfMArray = getSizeOfSBMArray
  {-# INLINE getSizeOfMArray #-}
  thawArray = thawSBArray
  {-# INLINE thawArray #-}
  thawCopyArray = thawCopySBArray
  {-# INLINE thawCopyArray #-}
  freezeMArray = freezeSBMArray
  {-# INLINE freezeMArray #-}
  freezeCopyMArray = freezeCopySBMArray
  {-# INLINE freezeCopyMArray #-}
  newRawMArray = newRawSBMArray
  {-# INLINE newRawMArray #-}
  readMArray = readSBMArray
  {-# INLINE readMArray #-}
  writeMArray = writeSBMArray
  {-# INLINE writeMArray #-}
  newMArray = newSBMArray
  {-# INLINE newMArray #-}
  copyArray = copySBArray
  {-# INLINE copyArray #-}
  moveMArray = moveSBMArray
  {-# INLINE moveMArray #-}
  cloneArray = cloneSBArray
  {-# INLINE cloneArray #-}
  cloneMArray = cloneSBMArray
  {-# INLINE cloneMArray #-}





-- | Compare-and-swap operation that can be used as a concurrency primitive for
-- implementing atomic operations on the mutable array. Returns a boolean value, which
-- indicates `True` for success and `False` otherwise for the update, as well as the
-- current value at the supplied index. In case of success current value returned will
-- be the newly supplied one, otherwise it will still be the old one. Note that there is
-- no `Eq` constraint on the element, that is because compare operation is done on a
-- thunk reference reference, not the value itself, in other words the expected value
-- must be the exact same one.
--
-- [Unsafe index] Negative or larger than array size can fail with unchecked exception
--
-- ====__Examples__
--
-- >>> ma <- makeSBMArray 5 (pure . (*10))
-- >>> freezeSBMArray ma
-- SBArray [0,10,20,30,40]
--
-- A possible mistake is to try and pass the expected value, instead of an actual element:
--
-- >>> casSBMArray ma 2 20 1000
-- (False,20)
-- >>> freezeSBMArray ma
-- SBArray [0,10,20,30,40]
--
-- But this will get us nowhere, since what we really need is the actual reference to the
-- value currently in the array cell
--
-- >>> expected <- readSBMArray ma 2
-- >>> r@(_, currentValue) <- casSBMArray ma 2 expected 1000
-- >>> freezeSBMArray ma
-- SBArray [0,10,1000,30,40]
-- >>> r
-- (True,1000)
--
-- In a concurrent setting current value can potentially be modified by some other
-- thread, therefore returned value can be immedieately used as the expected one to the
-- next call, if we don want to retry the atomic modification:
--
-- >>> casSBMArray ma 2 currentValue 2000
-- (True,2000)
-- >>> freezeSBMArray ma
-- SBArray [0,10,2000,30,40]
--
-- @since 0.1.0
casSBMArray ::
     MonadPrim s m
  => SBMArray e s -- ^ Mutable array to mutate
  -> Int -- ^ Index at which the cell should be set to the new value
  -> e -- ^ Reference to the expected boxed value
  -> e -- ^ New value to update the cell with
  -> m (Bool, e)
casSBMArray (SBMArray ma#) (I# i#) expected new =
  prim $ \s ->
    case casSmallArray# ma# i# expected new s of
      (# s', failed#, actual #) -> (# s', (isTrue# (failed# ==# 0#), actual) #)
{-# INLINE casSBMArray #-}


atomicModifySBMArray# :: MonadPrim s m => SBMArray e s -> Int -> (e -> (# e, b #)) -> m b
atomicModifySBMArray# ma@(SBMArray ma#) i@(I# i#) f = do
  current0 <- readSBMArray ma i
  prim $
    let go expected s =
          case f expected of
            (# new, artifact #) ->
              case casSmallArray# ma# i# expected new s of
                (# s', 0#, _ #)     -> (# s', artifact #)
                (# s', _, actual #) -> go actual s'
     in go current0
{-# INLINE atomicModifySBMArray# #-}


atomicModifyFetchNewSBMArray :: MonadPrim s m => SBMArray e s -> Int -> (e -> e) -> m e
atomicModifyFetchNewSBMArray ma i f =
  atomicModifySBMArray# ma i (\a -> let a' = f a in (# a', a' #))
{-# INLINE atomicModifyFetchNewSBMArray #-}

atomicModifyFetchOldSBMArray :: MonadPrim s m => SBMArray e s -> Int -> (e -> e) -> m e
atomicModifyFetchOldSBMArray ma i f =
  atomicModifySBMArray# ma i (\a -> (# f a, a #))
{-# INLINE atomicModifyFetchOldSBMArray #-}


atomicModifySBMArray :: MonadPrim s m => SBMArray e s -> Int -> (e -> (e, b)) -> m b
atomicModifySBMArray ma i f =
  atomicModifySBMArray# ma i (\a -> let (a', b) = f a in (# a', b #))
{-# INLINE atomicModifySBMArray #-}


atomicModifySBMArray_ :: MonadPrim s m => SBMArray e s -> Int -> (e -> e) -> m ()
atomicModifySBMArray_ ma i f =
  atomicModifySBMArray# ma i (\a -> let a' = f a in (# a', () #))
{-# INLINE atomicModifySBMArray_ #-}


atomicModifySBMArray2 :: MonadPrim s m => SBMArray e s -> Int -> (e -> (e, b)) -> m (e, e, b)
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

makeSBArrayM :: MonadPrim s m => Size -> (Int -> m e) -> m (SBArray e)
makeSBArrayM = I.makeArrayM
{-# INLINE makeSBArrayM #-}

createSBArrayM :: MonadPrim s m => Size -> (SBMArray e s -> m b) -> m (b, SBArray e)
createSBArrayM = I.createArrayM
{-# INLINE createSBArrayM #-}

createSBArrayM_ :: MonadPrim s m => Size -> (SBMArray e s -> m b) -> m (SBArray e)
createSBArrayM_ = I.createArrayM_
{-# INLINE createSBArrayM_ #-}


-- | Traverse an array with a monadic action.
--
-- @since 0.1.0
traverseSBArray :: MonadPrim s m => (e -> m b) -> SBArray e -> m (SBArray b)
traverseSBArray = I.traverseArray
{-# INLINE traverseSBArray #-}
