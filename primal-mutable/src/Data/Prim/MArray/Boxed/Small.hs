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
  , MSBArray(..)
  , Size(..)
  -- * Immutable
  , makeSBArray
  , makeSBArrayM
  , sizeOfSBArray
  , indexSBArray
  -- * Mutable
  -- ** Create
  , newMSBArray
  , newRawMSBArray
  , newMSBArrayLazy
  , makeMSBArray
  , createSBArrayM
  , createSBArrayM_
  , sizeOfMSBArray
  -- ** Access
  , readMSBArray
  , writeMSBArray
  , writeMSBArrayLazy
  , writeMSBArrayDeep
  -- *** Atomic
  , casMSBArray
  , atomicModifyFetchNewMSBArray
  , atomicModifyFetchOldMSBArray
  , atomicModifyMSBArray
  , atomicModifyMSBArray_
  , atomicModifyMSBArray2
  -- *
  , thawSBArray
  , thawCopySBArray
  , freezeMSBArray
  , freezeCopyMSBArray
  , copySBArray
  , moveMSBArray
  , cloneSBArray
  , cloneMSBArray
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


data MSBArray e s = MSBArray (SmallMutableArray# s e)

-- | Check if both of the arrays refer to the exact same one. None of the elements are
-- evaluated.
instance Eq (MSBArray e s) where
  MSBArray ma1# == MSBArray ma2# = isTrue# (sameSmallMutableArray# ma1# ma2#)

data SBArray e = SBArray (SmallArray# e)

instance Functor SBArray where
  fmap f a = runST $ traverseSBArray (pure . f) a

instance MRef (MSBArray e) where
  type Elt (MSBArray e) = e
  newRawMRef = newRawMSBArray 1
  {-# INLINE newRawMRef #-}
  readMRef ma = readMSBArray ma 0
  {-# INLINE readMRef #-}
  writeMRef ma = writeMSBArray ma 0
  {-# INLINE writeMRef #-}
  newMRef = newMSBArray 1
  {-# INLINE newMRef #-}

instance AtomicMRef (MSBArray e) where
  casMRef msba = casMSBArray msba 0
  {-# INLINE casMRef #-}

instance Num e => AtomicCountMRef (MSBArray e)
instance Bits e => AtomicBitsMRef (MSBArray e)


instance I.MArray (MSBArray e) where
  type Array (MSBArray e) = SBArray e
  indexArray = indexSBArray
  {-# INLINE indexArray #-}
  sizeOfArray = sizeOfSBArray
  {-# INLINE sizeOfArray #-}
  getSizeOfMArray = pure . sizeOfMSBArray
  {-# INLINE getSizeOfMArray #-}
  thawArray = thawSBArray
  {-# INLINE thawArray #-}
  thawCopyArray = thawCopySBArray
  {-# INLINE thawCopyArray #-}
  freezeMArray = freezeMSBArray
  {-# INLINE freezeMArray #-}
  freezeCopyMArray = freezeCopyMSBArray
  {-# INLINE freezeCopyMArray #-}
  newRawMArray = newRawMSBArray
  {-# INLINE newRawMArray #-}
  readMArray = readMSBArray
  {-# INLINE readMArray #-}
  writeMArray = writeMSBArray
  {-# INLINE writeMArray #-}
  newMArray = newMSBArray
  {-# INLINE newMArray #-}
  copyArray = copySBArray
  {-# INLINE copyArray #-}
  moveMArray = moveMSBArray
  {-# INLINE moveMArray #-}
  cloneArray = cloneSBArray
  {-# INLINE cloneArray #-}
  cloneMArray = cloneMSBArray
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
-- value, which is evaluated immediately before that. See `newMSBArrayLazy` for an ability
-- to initialize with a thunk or `newRawSBArray` that will set each element to an
-- `UndefinedElement` exception.
--
-- [Unsafe size] Negative or too large of an array size can kill the current thread with
-- `HeapOverflow` asynchronous exception.
--
-- ====__Examples__
--
-- >>> newMSBArray 10 'A' >>= freezeMSBArray
-- SBArray "AAAAAAAAAA"
--
-- @since 0.1.0
newMSBArray :: MonadPrim s m => Size -> e -> m (MSBArray e s)
newMSBArray sz a = seqPrim a >>= newMSBArrayLazy sz
{-# INLINE newMSBArray #-}

-- | Same as `newMSBArray`, except initial element is allowed to be a thunk. Prefer using
-- `newRawMSBArray`, instead of supplying an `error`, whenever initial element is not
-- known ahead of time. Or even better try using other creation functions that iterate
-- over an array and overwrite each element, such as `makeMSBArray`.
--
-- [Unsafe size] Negative or too large of an array size can kill the current thread with `HeapOverflow`
-- asynchronous exception.
--
-- @since 0.1.0
newMSBArrayLazy :: MonadPrim s m => Size -> e -> m (MSBArray e s)
newMSBArrayLazy (Size (I# n#)) a =
  prim $ \s ->
    case newSmallArray# n# a s of
      (# s', ma# #) -> (# s', MSBArray ma# #)
{-# INLINE newMSBArrayLazy #-}

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
-- >>> ma <- newRawMSBArray 10 :: IO (MSBArray Int RW)
-- >>> sizeOfMSBArray ma
-- Size 10
-- >>> readMSBArray ma 1
-- *** Exception: undefined array element: Data.Prim.Array.Boxed.Small.newRawMSBArray
--
-- @since 0.1.0
newRawMSBArray :: MonadPrim s m => Size -> m (MSBArray e s)
newRawMSBArray sz = newMSBArrayLazy sz (uninitialized "Data.Prim.MAray.Boxed.Small" "newRawMSBArray")
{-# INLINE newRawMSBArray #-}

-- | Get the size of a mutable boxed array
--
-- >>> ma <- newMSBArray 1024 "Element of each cell"
-- >>> sizeOfMSBArray ma
-- Size 1024
--
-- @since 0.1.0
sizeOfMSBArray :: MSBArray e s -> Size
sizeOfMSBArray (MSBArray ma#) = Size (I# (sizeofSmallMutableArray# ma#))
{-# INLINE sizeOfMSBArray #-}


-- | Read an element from mutable boxed array at a supplied index.
--
-- [Unsafe index] Negative or larger than array size can fail with unchecked exception
--
-- ==== __Examples__
--
-- >>> ma <- makeMSBArray 10 (pure . ("Element ix: " ++) . show)
-- >>> readMSBArray ma 5
-- "Element ix: 5"
--
-- @since 0.1.0
readMSBArray :: MonadPrim s m => MSBArray e s -> Int -> m e
readMSBArray (MSBArray ma#) (I# i#) = prim (readSmallArray# ma# i#)
{-# INLINE readMSBArray #-}

-- | Write an element into a mutable boxed array at a supplied index strictly. An
-- element will be evaluated to WHNF.
--
-- [Unsafe index] Negative or larger than array size can fail with unchecked exception
--
-- ==== __Examples__
--
-- >>> ma <- newMSBArray 4 (Nothing :: Maybe Int)
-- >>> writeMSBArray ma 2 (Just 2)
-- >>> freezeMSBArray ma
-- SBArray [Nothing,Nothing,Just 2,Nothing]
--
-- Important to note that an element is evaluated prior to being written into a cell, so
-- it will not overwrite a value with if it evaluates to an exception:
--
-- >>> import Control.Exception
-- >>> writeMSBArray ma 2 (throw DivideByZero)
-- *** Exception: divide by zero
-- >>> freezeMSBArray ma
-- SBArray [Nothing,Nothing,Just 2,Nothing]
--
-- But it is evaluated to Normal Form, so it is still possible to write something that
-- eventually evaluates to bottom.
--
-- >>> writeMSBArray ma 3 (Just (7 `div` 0 ))
-- >>> freezeMSBArray ma
-- SBArray [Nothing,Nothing,Just 2,Just *** Exception: divide by zero
--
-- Either `deepseq` or `writeMSBArrayDeep` can be used to alleviate that.
--
-- @since 0.1.0
writeMSBArray :: MonadPrim s m => MSBArray e s -> Int -> e -> m ()
writeMSBArray ma i x = seqPrim x >>= writeMSBArrayLazy ma i
{-# INLINE writeMSBArray #-}


-- | Same as `writeMSBArray` but write a thunk into an array instead of an actual
-- element. Careful with memory leaks and thunks that can evaluate to exceptions.
--
-- [Unsafe index] Negative or larger than array size can fail with unchecked exception
--
-- @since 0.1.0
writeMSBArrayLazy :: MonadPrim s m => MSBArray e s -> Int -> e -> m ()
writeMSBArrayLazy (MSBArray ma#) (I# i#) a = prim_ (writeSmallArray# ma# i# a)
{-# INLINE writeMSBArrayLazy #-}


-- | Same as `writeMSBArray` but ensure that the value being written is fully evaluated.
--
-- [Unsafe index] Negative or larger than array size can fail with unchecked exception
--
-- @since 0.1.0
writeMSBArrayDeep :: (MonadPrim s m, NFData e) => MSBArray e s -> Int -> e -> m ()
writeMSBArrayDeep ma i x = x `deepseq` writeMSBArrayLazy ma i x
{-# INLINE writeMSBArrayDeep #-}

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
-- >>> writeMSBArray ma 1 10
-- >>> freezeMSBArray ma
-- SBArray [1,10,3,4,5]
-- >>> print a
-- SBArray [1,2,3,4,5]
--
-- Be careful not mutate pure immutable arrays, that are still being referenced in some pure
-- setting.
--
-- >>> ma' <- thawSBArray a
-- >>> writeMSBArray ma' 0 100000
-- >>> print a
-- SBArray [100000,2,3,4,5]
--
-- @since 0.1.0
thawSBArray :: MonadPrim s m => SBArray e -> m (MSBArray e s)
thawSBArray (SBArray a#) = prim $ \s ->
  case unsafeThawSmallArray# a# s of
    (# s', ma# #) -> (# s', MSBArray ma# #)
{-# INLINE thawSBArray #-}

-- | Make a copy of a subsection of an immutable array and then convert the result into
-- a mutable array.
--
-- > ma' <- thawCopySBArray a i n
--
-- Is equivalent to:
--
-- > ma' <- newRawMSBArray n >>= \ma -> ma <$ copySBArray a i ma 0 n
--
-- ====__Examples__
--
-- >>> let a = fromListSBArray [1 .. 5 :: Int]
-- >>> ma <- thawCopySBArray a 1 3
-- >>> writeMSBArray ma 1 10
-- >>> freezeMSBArray ma
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
  -> m (MSBArray e s)
thawCopySBArray (SBArray a#) (I# i#) (Size (I# n#)) = prim $ \s ->
  case thawSmallArray# a# i# n# s of
    (# s', ma# #) -> (# s', MSBArray ma# #)
{-# INLINE thawCopySBArray #-}

-- | Convert a mutable boxed array into an immutable.
--
-- [Unsafe further mutation] After the mutable array is frozen, it is unsafe to mutate
-- it, because the changes will be reflected in the newly created immutable array as well.
--
-- See `freezeCopyMSBArray` for a safer alternative or use `cloneMSBArray` prior to
-- freezing if further mutation of an array is still needed.
--
-- @since 0.1.0
freezeMSBArray :: MonadPrim s m => MSBArray e s -> m (SBArray e)
freezeMSBArray (MSBArray ma#) = prim $ \s ->
  case unsafeFreezeSmallArray# ma# s of
    (# s', a# #) -> (# s', SBArray a# #)
{-# INLINE freezeMSBArray #-}


-- | Same as `freezeMSBArray`, but makes a copy of a subsection of a mutable array before
-- converting it into an immutable.
--
-- [Unsafe offset] Offset cannot be negative or larger than the size of an array,
-- otherwise it can result in an unchecked exception
--
-- [Unsafe new size] Number of elements to be copied cannot be larger than the size of an
-- array minus the offset.
--
-- @since 0.1.0
freezeCopyMSBArray :: MonadPrim s m => MSBArray e s -> Int -> Size -> m (SBArray e)
freezeCopyMSBArray (MSBArray ma#) (I# i#) (Size (I# n#)) = prim $ \s ->
  case freezeSmallArray# ma# i# n# s of
    (# s', a# #) -> (# s', SBArray a# #)
{-# INLINE freezeCopyMSBArray #-}


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
cloneMSBArray :: MonadPrim s m => MSBArray e s -> Int -> Size -> m (MSBArray e s)
cloneMSBArray (MSBArray ma#) (I# i#) (Size (I# n#)) =
  prim $ \s ->
    case cloneSmallMutableArray# ma# i# n# s of
      (# s', ma'# #) -> (# s', MSBArray ma'# #)
{-# INLINE cloneMSBArray #-}


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
  -> MSBArray e s -- ^ Destination mutable array
  -> Int -- ^ Offset into the destination mutable array
  -> Size -- ^ Number of elements to copy over
  -> m ()
copySBArray (SBArray src#) (I# srcOff#) (MSBArray dst#) (I# dstOff#) (Size (I# n#)) =
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
moveMSBArray ::
     MonadPrim s m
  => MSBArray e s -- ^ Source mutable array
  -> Int -- ^ Offset into the source mutable array
  -> MSBArray e s -- ^ Destination mutable array
  -> Int -- ^ Offset into the destination mutable array
  -> Size -- ^ Number of elements to copy over
  -> m ()
moveMSBArray (MSBArray src#) (I# srcOff#) (MSBArray dst#) (I# dstOff#) (Size (I# n#)) =
  prim_ (copySmallMutableArray# src# srcOff# dst# dstOff# n#)
{-# INLINE moveMSBArray #-}


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
-- >>> ma <- makeMSBArray 5 (pure . (*10))
-- >>> freezeMSBArray ma
-- SBArray [0,10,20,30,40]
--
-- A possible mistake is to try and pass the expected value, instead of an actual element:
--
-- >>> casMSBArray ma 2 20 1000
-- (False,20)
-- >>> freezeMSBArray ma
-- SBArray [0,10,20,30,40]
--
-- But this will get us nowhere, since what we really need is the actual reference to the
-- value currently in the array cell
--
-- >>> expected <- readMSBArray ma 2
-- >>> r@(_, currentValue) <- casMSBArray ma 2 expected 1000
-- >>> freezeMSBArray ma
-- SBArray [0,10,1000,30,40]
-- >>> r
-- (True,1000)
--
-- In a concurrent setting current value can potentially be modified by some other
-- thread, therefore returned value can be immedieately used as the expected one to the
-- next call, if we don want to retry the atomic modification:
--
-- >>> casMSBArray ma 2 currentValue 2000
-- (True,2000)
-- >>> freezeMSBArray ma
-- SBArray [0,10,2000,30,40]
--
-- @since 0.1.0
casMSBArray ::
     MonadPrim s m
  => MSBArray e s -- ^ Mutable array to mutate
  -> Int -- ^ Index at which the cell should be set to the new value
  -> e -- ^ Reference to the expected boxed value
  -> e -- ^ New value to update the cell with
  -> m (Bool, e)
casMSBArray (MSBArray ma#) (I# i#) expected new =
  prim $ \s ->
    case casSmallArray# ma# i# expected new s of
      (# s', failed#, actual #) -> (# s', (isTrue# (failed# ==# 0#), actual) #)
{-# INLINE casMSBArray #-}


atomicModifyMSBArray# :: MonadPrim s m => MSBArray e s -> Int -> (e -> (# e, b #)) -> m b
atomicModifyMSBArray# ma@(MSBArray ma#) i@(I# i#) f = do
  current0 <- readMSBArray ma i
  prim $
    let go expected s =
          case f expected of
            (# new, artifact #) ->
              case casSmallArray# ma# i# expected new s of
                (# s', 0#, _ #)     -> (# s', artifact #)
                (# s', _, actual #) -> go actual s'
     in go current0
{-# INLINE atomicModifyMSBArray# #-}


atomicModifyFetchNewMSBArray :: MonadPrim s m => MSBArray e s -> Int -> (e -> e) -> m e
atomicModifyFetchNewMSBArray ma i f =
  atomicModifyMSBArray# ma i (\a -> let a' = f a in (# a', a' #))
{-# INLINE atomicModifyFetchNewMSBArray #-}

atomicModifyFetchOldMSBArray :: MonadPrim s m => MSBArray e s -> Int -> (e -> e) -> m e
atomicModifyFetchOldMSBArray ma i f =
  atomicModifyMSBArray# ma i (\a -> (# f a, a #))
{-# INLINE atomicModifyFetchOldMSBArray #-}


atomicModifyMSBArray :: MonadPrim s m => MSBArray e s -> Int -> (e -> (e, b)) -> m b
atomicModifyMSBArray ma i f =
  atomicModifyMSBArray# ma i (\a -> let (a', b) = f a in (# a', b #))
{-# INLINE atomicModifyMSBArray #-}


atomicModifyMSBArray_ :: MonadPrim s m => MSBArray e s -> Int -> (e -> e) -> m ()
atomicModifyMSBArray_ ma i f =
  atomicModifyMSBArray# ma i (\a -> let a' = f a in (# a', () #))
{-# INLINE atomicModifyMSBArray_ #-}


atomicModifyMSBArray2 :: MonadPrim s m => MSBArray e s -> Int -> (e -> (e, b)) -> m (e, e, b)
atomicModifyMSBArray2 ma i f =
  atomicModifyMSBArray# ma i (\a -> let (a', b) = f a in (# a', (a, a', b) #))
{-# INLINE atomicModifyMSBArray2 #-}


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

createSBArrayM :: MonadPrim s m => Size -> (MSBArray e s -> m b) -> m (b, SBArray e)
createSBArrayM = I.createArrayM
{-# INLINE createSBArrayM #-}

createSBArrayM_ :: MonadPrim s m => Size -> (MSBArray e s -> m b) -> m (SBArray e)
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
-- >>> ma <- makeMSBArray 5 $ \i -> modifyFetchRef ref (\cur -> cur ++ show i ++ ",")
-- >>> mapM_ (readMSBArray ma >=> putStrLn) [0 .. 4]
-- Numbers: 0,
-- Numbers: 0,1,
-- Numbers: 0,1,2,
-- Numbers: 0,1,2,3,
-- Numbers: 0,1,2,3,4,
--
-- @since 0.1.0
makeMSBArray :: MonadPrim s m => Size -> (Int -> m e) -> m (MSBArray e s)
makeMSBArray = I.makeMArray
{-# INLINE makeMSBArray #-}

-- | Traverse an array with a monadic action.
--
-- @since 0.1.0
traverseSBArray :: MonadPrim s m => (e -> m b) -> SBArray e -> m (SBArray b)
traverseSBArray = I.traverseArray
{-# INLINE traverseSBArray #-}
