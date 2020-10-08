{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
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
  , newSBMArrayLazy
  , makeSBMArray
  , createSBArrayM
  , createSBArrayM_
  , sizeOfSBMArray
  -- ** Access
  , readSBMArray
  , writeSBMArray
  , writeSBMArrayLazy
  , writeSBMArrayDeep
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

import Control.DeepSeq
import Control.Monad.ST
import Control.Prim.Monad
import Data.Bits
import Data.Prim
import qualified Data.Prim.MArray.Internal as I
import Data.Prim.MRef.Atomic
import Data.Prim.MRef.Internal
import Foreign.Prim

instance Show e => Show (SBArray e) where
  showsPrec n arr
    | n > 1 = ('(' :) . inner . (')' :)
    | otherwise = inner
    where
      inner = ("Array " ++) . shows (toList arr)

instance IsList (SBArray e) where
  type Item (SBArray e) = e
  fromList = fromListSBArray
  fromListN n = fromListSBArrayN (Size n)
  toList = toListSBArray


data SBMArray e s = SBMArray (SmallMutableArray# s e)

-- | Does not force the content of mutable array
instance NFData (SBMArray s e) where
  rnf (SBMArray _) = ()

-- | Check if both of the arrays refer to the exact same one. None of the elements are
-- evaluated.
instance Eq (SBMArray e s) where
  SBMArray ma1# == SBMArray ma2# = isTrue# (sameSmallMutableArray# ma1# ma2#)

data SBArray e = SBArray (SmallArray# e)

instance Functor SBArray where
  fmap f a = runST $ traverseSBArray (pure . f) a

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
  getSizeOfMArray = pure . sizeOfSBMArray
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



sizeOfSBArray :: SBArray e -> Size
sizeOfSBArray (SBArray a#) = Size (I# (sizeofSmallArray# a#))
{-# INLINE sizeOfSBArray #-}

-- | Index an element of a pure boxed array.
--
-- [Unsafe index] Negative or larger than array size can fail with unchecked exception
--
-- ==== __Examples__
--
-- >>> import Data.Prim.Array.Boxed
-- >>> let a = makeSBArray 1024 (\i -> [0 .. i])
-- >>> print $ indexSBArray a 1
-- [0,1]
-- >>> print $ indexSBArray a 5
-- [0,1,2,3,4,5]
--
-- @since 0.1.0
indexSBArray :: SBArray e -> Int -> e
indexSBArray (SBArray a#) (I# i#) =
  case indexSmallArray# a# i# of
    (# x #) -> x
{-# INLINE indexSBArray #-}

-- | Create a mutable boxed array where each element is set to the supplied initial
-- value, which is evaluated immediately before that. See `newSBMArrayLazy` for an ability
-- to initialize with a thunk or `newRawSBArray` that will set each element to an
-- `UndefinedElement` exception.
--
-- [Unsafe size] Negative or too large of an array size can kill the current thread with
-- `HeapOverflow` asynchronous exception.
--
-- ====__Examples__
--
-- >>> newSBMArray 10 'A' >>= freezeSBMArray
-- SBArray "AAAAAAAAAA"
--
-- @since 0.1.0
newSBMArray :: MonadPrim s m => Size -> e -> m (SBMArray e s)
newSBMArray sz a = seqPrim a >>= newSBMArrayLazy sz
{-# INLINE newSBMArray #-}

-- | Same as `newSBMArray`, except initial element is allowed to be a thunk. Prefer using
-- `newRawSBMArray`, instead of supplying an `error`, whenever initial element is not
-- known ahead of time. Or even better try using other creation functions that iterate
-- over an array and overwrite each element, such as `makeSBMArray`.
--
-- [Unsafe size] Negative or too large of an array size can kill the current thread with `HeapOverflow`
-- asynchronous exception.
--
-- @since 0.1.0
newSBMArrayLazy :: MonadPrim s m => Size -> e -> m (SBMArray e s)
newSBMArrayLazy (Size (I# n#)) a =
  prim $ \s ->
    case newSmallArray# n# a s of
      (# s', ma# #) -> (# s', SBMArray ma# #)
{-# INLINE newSBMArrayLazy #-}

-- | Create new mutable array, where each element is set to a thunk that throws an error
-- when evaluated. This is useful when there is a plan to iterate over the whole array
-- and write values into each cell monadically or in some index aware fashion.
--
-- [Unsafe size] Negative or too large of an array size can kill the current thread with
-- `HeapOverflow` asynchronous exception.
--
-- ==== __Examples__
--
-- >>> import Control.Prim.Monad
-- >>> ma <- newRawSBMArray 10 :: IO (SBMArray Int RW)
-- >>> sizeOfSBMArray ma
-- Size 10
-- >>> readSBMArray ma 1
-- *** Exception: undefined array element: Data.Prim.Array.Boxed.Small.newRawSBMArray
--
-- @since 0.1.0
newRawSBMArray :: MonadPrim s m => Size -> m (SBMArray e s)
newRawSBMArray sz = newSBMArrayLazy sz (uninitialized "Data.Prim.MAray.Boxed.Small" "newRawSBMArray")
{-# INLINE newRawSBMArray #-}

-- | Get the size of a mutable boxed array
--
-- >>> ma <- newSBMArray 1024 "Element of each cell"
-- >>> sizeOfSBMArray ma
-- Size 1024
--
-- @since 0.1.0
sizeOfSBMArray :: SBMArray e s -> Size
sizeOfSBMArray (SBMArray ma#) = Size (I# (sizeofSmallMutableArray# ma#))
{-# INLINE sizeOfSBMArray #-}


-- | Read an element from mutable boxed array at a supplied index.
--
-- [Unsafe index] Negative or larger than array size can fail with unchecked exception
--
-- ==== __Examples__
--
-- >>> ma <- makeSBMArray 10 (pure . ("Element ix: " ++) . show)
-- >>> readSBMArray ma 5
-- "Element ix: 5"
--
-- @since 0.1.0
readSBMArray :: MonadPrim s m => SBMArray e s -> Int -> m e
readSBMArray (SBMArray ma#) (I# i#) = prim (readSmallArray# ma# i#)
{-# INLINE readSBMArray #-}

-- | Write an element into a mutable boxed array at a supplied index strictly. An
-- element will be evaluated to WHNF.
--
-- [Unsafe index] Negative or larger than array size can fail with unchecked exception
--
-- ==== __Examples__
--
-- >>> ma <- newSBMArray 4 (Nothing :: Maybe Int)
-- >>> writeSBMArray ma 2 (Just 2)
-- >>> freezeSBMArray ma
-- SBArray [Nothing,Nothing,Just 2,Nothing]
--
-- Important to note that an element is evaluated prior to being written into a cell, so
-- it will not overwrite a value with if it evaluates to an exception:
--
-- >>> import Control.Exception
-- >>> writeSBMArray ma 2 (throw DivideByZero)
-- *** Exception: divide by zero
-- >>> freezeSBMArray ma
-- SBArray [Nothing,Nothing,Just 2,Nothing]
--
-- But it is evaluated to Normal Form, so it is still possible to write something that
-- eventually evaluates to bottom.
--
-- >>> writeSBMArray ma 3 (Just (7 `div` 0 ))
-- >>> freezeSBMArray ma
-- SBArray [Nothing,Nothing,Just 2,Just *** Exception: divide by zero
--
-- Either `deepseq` or `writeSBMArrayDeep` can be used to alleviate that.
--
-- @since 0.1.0
writeSBMArray :: MonadPrim s m => SBMArray e s -> Int -> e -> m ()
writeSBMArray ma i x = seqPrim x >>= writeSBMArrayLazy ma i
{-# INLINE writeSBMArray #-}


-- | Same as `writeSBMArray` but write a thunk into an array instead of an actual
-- element. Careful with memory leaks and thunks that can evaluate to exceptions.
--
-- [Unsafe index] Negative or larger than array size can fail with unchecked exception
--
-- @since 0.1.0
writeSBMArrayLazy :: MonadPrim s m => SBMArray e s -> Int -> e -> m ()
writeSBMArrayLazy (SBMArray ma#) (I# i#) a = prim_ (writeSmallArray# ma# i# a)
{-# INLINE writeSBMArrayLazy #-}


-- | Same as `writeSBMArray` but ensure that the value being written is fully evaluated.
--
-- [Unsafe index] Negative or larger than array size can fail with unchecked exception
--
-- @since 0.1.0
writeSBMArrayDeep :: (MonadPrim s m, NFData e) => SBMArray e s -> Int -> e -> m ()
writeSBMArrayDeep ma i x = x `deepseq` writeSBMArrayLazy ma i x
{-# INLINE writeSBMArrayDeep #-}

-- | Convert a pure immutable boxed array into a mutable boxed array. See
-- `thawCopySBArray` for a safer alternative.
--
-- [Unsafe array] There is no copying being done, therefore any modification done to the
-- mutable array will be reflected in the immutable as well.
--
-- ====__Examples__
--
-- The correct way to use it is with combining it with something like `cloneSBArray` or
-- `copySBArray`, in order to preserve referential transperancy
--
-- >>> let a = fromListSBArray [1 .. 5 :: Int]
-- >>> ma <- thawSBArray $ cloneSBArray a 0 (sizeOfSBArray a)
-- >>> writeSBMArray ma 1 10
-- >>> freezeSBMArray ma
-- SBArray [1,10,3,4,5]
-- >>> print a
-- SBArray [1,2,3,4,5]
--
-- Be careful not mutate pure immutable arrays, that are still being referenced in some pure
-- setting.
--
-- >>> ma' <- thawSBArray a
-- >>> writeSBMArray ma' 0 100000
-- >>> print a
-- SBArray [100000,2,3,4,5]
--
-- @since 0.1.0
thawSBArray :: MonadPrim s m => SBArray e -> m (SBMArray e s)
thawSBArray (SBArray a#) = prim $ \s ->
  case unsafeThawSmallArray# a# s of
    (# s', ma# #) -> (# s', SBMArray ma# #)
{-# INLINE thawSBArray #-}

-- | Make a copy of a subsection of an immutable array and then convert the result into
-- a mutable array.
--
-- > ma' <- thawCopySBArray a i n
--
-- Is equivalent to:
--
-- > ma' <- newRawSBMArray n >>= \ma -> ma <$ copySBArray a i ma 0 n
--
-- ====__Examples__
--
-- >>> let a = fromListSBArray [1 .. 5 :: Int]
-- >>> ma <- thawCopySBArray a 1 3
-- >>> writeSBMArray ma 1 10
-- >>> freezeSBMArray ma
-- SBArray [2,10,4]
-- >>> print a
-- SBArray [1,2,3,4,5]
--
-- @since 0.1.0
thawCopySBArray ::
     MonadPrim s m
  => SBArray e
  -> Int -- ^ [offset] Offset cannot be negative or larger than the size of an
         -- array, otherwise it can result in an unchecked exception
  -> Size -- ^ [new size] Number of elements to be copied cannot be larger than the
          -- size of an array minus the offset.
  -> m (SBMArray e s)
thawCopySBArray (SBArray a#) (I# i#) (Size (I# n#)) = prim $ \s ->
  case thawSmallArray# a# i# n# s of
    (# s', ma# #) -> (# s', SBMArray ma# #)
{-# INLINE thawCopySBArray #-}

-- | Convert a mutable boxed array into an immutable.
--
-- [Unsafe further mutation] After the mutable array is frozen, it is unsafe to mutate
-- it, because the changes will be reflected in the newly created immutable array as well.
--
-- See `freezeCopySBMArray` for a safer alternative or use `cloneSBMArray` prior to
-- freezing if further mutation of an array is still needed.
--
-- @since 0.1.0
freezeSBMArray :: MonadPrim s m => SBMArray e s -> m (SBArray e)
freezeSBMArray (SBMArray ma#) = prim $ \s ->
  case unsafeFreezeSmallArray# ma# s of
    (# s', a# #) -> (# s', SBArray a# #)
{-# INLINE freezeSBMArray #-}


-- | Same as `freezeSBMArray`, but makes a copy of a subsection of a mutable array before
-- converting it into an immutable.
--
-- [Unsafe offset] Offset cannot be negative or larger than the size of an array,
-- otherwise it can result in an unchecked exception
--
-- [Unsafe new size] Number of elements to be copied cannot be larger than the size of an
-- array minus the offset.
--
-- @since 0.1.0
freezeCopySBMArray :: MonadPrim s m => SBMArray e s -> Int -> Size -> m (SBArray e)
freezeCopySBMArray (SBMArray ma#) (I# i#) (Size (I# n#)) = prim $ \s ->
  case freezeSmallArray# ma# i# n# s of
    (# s', a# #) -> (# s', SBArray a# #)
{-# INLINE freezeCopySBMArray #-}


-- | Make an exact copy of a subsection of a pure immutable array.
---
-- [Unsafe offset] Offset cannot be negative or larger than the size of an array,
-- otherwise it can result in an unchecked exception
--
-- [Unsafe new size] Number of elements to be copied cannot be larger than the size of an
-- array minus the offset.
--
-- ====__Examples__
--
-- >>> let a = fromListSBArray ['a'..'z']
-- >>> a
-- SBArray "abcdefghijklmnopqrstuvwxyz"
-- >>> cloneSBArray a 23 3
-- SBArray "xyz"
--
-- @since 0.1.0
cloneSBArray :: SBArray e -> Int -> Size -> SBArray e
cloneSBArray (SBArray a#) (I# i#) (Size (I# n#)) = SBArray (cloneSmallArray# a# i# n#)
{-# INLINE cloneSBArray #-}

-- | Same as `cloneSBArray`, except it works on mutable arrays
--
-- [Unsafe offset] Offset cannot be negative or larger than the size of an array,
-- otherwise it can result in an unchecked exception
--
-- [Unsafe new size] Number of elements to be copied cannot be larger than the size of an
-- array minus the offset.
--
-- @since 0.1.0
cloneSBMArray :: MonadPrim s m => SBMArray e s -> Int -> Size -> m (SBMArray e s)
cloneSBMArray (SBMArray ma#) (I# i#) (Size (I# n#)) =
  prim $ \s ->
    case cloneSmallMutableArray# ma# i# n# s of
      (# s', ma'# #) -> (# s', SBMArray ma'# #)
{-# INLINE cloneSBMArray #-}


-- | Copy a subsection of an immutable array into a subsection of another mutable array.
--
-- [Unsafe overlap] The two arrays must not be the same array in different states
--
-- [Unsafe offset] Each offset cannot be negative or larger than the size of a
-- corresponding array, otherwise it can result in an unchecked exception
--
-- [Unsafe new size] Number of elements to be copied cannot be larger than the size of an
-- each array minus their corersponding offsets.
--
-- @since 0.1.0
copySBArray ::
     MonadPrim s m
  => SBArray e -- ^ Source immutable array
  -> Int -- ^ Offset into the source immutable array
  -> SBMArray e s -- ^ Destination mutable array
  -> Int -- ^ Offset into the destination mutable array
  -> Size -- ^ Number of elements to copy over
  -> m ()
copySBArray (SBArray src#) (I# srcOff#) (SBMArray dst#) (I# dstOff#) (Size (I# n#)) =
  prim_ (copySmallArray# src# srcOff# dst# dstOff# n#)
{-# INLINE copySBArray #-}

-- | Copy a subsection of a mutable array into a subsection of another or the same
-- mutable array. Therefore, unlike `copySBArray`, memory overlap is allowed.
--
-- [Unsafe offset] Each offset cannot be negative or larger than the size of a
-- corresponding array, otherwise it can result in an unchecked exception
--
-- [Unsafe new size] Number of elements to be copied cannot be larger than the size of an
-- each array minus their corersponding offsets.
--
-- @since 0.1.0
moveSBMArray ::
     MonadPrim s m
  => SBMArray e s -- ^ Source mutable array
  -> Int -- ^ Offset into the source mutable array
  -> SBMArray e s -- ^ Destination mutable array
  -> Int -- ^ Offset into the destination mutable array
  -> Size -- ^ Number of elements to copy over
  -> m ()
moveSBMArray (SBMArray src#) (I# srcOff#) (SBMArray dst#) (I# dstOff#) (Size (I# n#)) =
  prim_ (copySmallMutableArray# src# srcOff# dst# dstOff# n#)
{-# INLINE moveSBMArray #-}


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


-- | Convert a list into an array strictly, i.e. each element is evaluated to WHNF prior
-- to being written into the newly created array. In order to allocate the array ahead
-- of time, the spine of a list will be evaluated first, in order to get the total
-- number of elements. Infinite lists will cause the program to halt. On the other hand
-- if the length of a list is known ahead of time, `fromListSBArrayN` can be used instead as
-- optimization.
--
-- @since 0.1.0
fromListSBArray :: [e] -> SBArray e
fromListSBArray xs = fromListSBArrayN (Size (length xs)) xs
{-# INLINE fromListSBArray #-}

-- | Same as `fromListSBArray`, except it will allocate an array exactly of @n@ size, as
-- such it will not convert any portion of the list that doesn't fit into the newly
-- created array.
--
-- [Unsafe size] if the length of supplied list is actually smaller then the expected
-- size, thunks with `UndefinedElement` will be left in the tail of the array.
--
-- ====__Examples__
--
-- >>> fromListSBArrayN 3 [1 :: Int, 2, 3]
-- SBArray [1,2,3]
-- >>> fromListSBArrayN 3 [1 :: Int ..]
-- SBArray [1,2,3]
-- >>> fromListSBArrayN 3 [1 :: Int, 2]
-- SBArray [1,2*** Exception: undefined array element: Data.Prim.Array.Boxed.uninitialized
--
-- @since 0.1.0
fromListSBArrayN ::
     Size -- ^ Expected @n@ size of a list
  -> [e]
  -> SBArray e
fromListSBArrayN = I.fromListArrayN
{-# INLINE fromListSBArrayN #-}

-- | Convert a pure boxed array into a list. It should work fine with GHC built-in list
-- fusion.
--
-- @since 0.1.0
toListSBArray :: SBArray e -> [e]
toListSBArray = I.toListArray
{-# INLINE toListSBArray #-}

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


-- | Create a new mutable array of a supplied size by applying a monadic action to indices
-- of each one of the new elements.
--
-- [Unsafe size] Negative or too large of an array size can kill the current thread with
-- `HeapOverflow` asynchronous exception.
--
-- ====__Examples__
--
-- >>> import Control.Monad ((>=>))
-- >>> import Data.Prim.Ref
-- >>> ref <- newRef "Numbers: "
-- >>> ma <- makeSBMArray 5 $ \i -> modifyFetchRef ref (\cur -> cur ++ show i ++ ",")
-- >>> mapM_ (readSBMArray ma >=> putStrLn) [0 .. 4]
-- Numbers: 0,
-- Numbers: 0,1,
-- Numbers: 0,1,2,
-- Numbers: 0,1,2,3,
-- Numbers: 0,1,2,3,4,
--
-- @since 0.1.0
makeSBMArray :: MonadPrim s m => Size -> (Int -> m e) -> m (SBMArray e s)
makeSBMArray = I.makeMArray
{-# INLINE makeSBMArray #-}

-- | Traverse an array with a monadic action.
--
-- @since 0.1.0
traverseSBArray :: MonadPrim s m => (e -> m b) -> SBArray e -> m (SBArray b)
traverseSBArray = I.traverseArray
{-# INLINE traverseSBArray #-}
