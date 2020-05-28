{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
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
  , MBArray(..)
  , Size(..)
  -- * Immutable
  , makeBArray
  , makeBArrayM
  , sizeOfBArray
  , indexBArray
  -- * Mutable
  -- ** Create
  , newMBArray
  , newRawMBArray
  , newMBArrayLazy
  , makeMBArray
  , createBArrayM
  , createBArrayM_
  , sizeOfMBArray
  -- ** Access
  , readMBArray
  , writeMBArray
  , writeMBArrayLazy
  , writeMBArrayDeep
  -- *** Atomic
  , casMBArray
  , atomicModifyFetchNewMBArray
  , atomicModifyFetchOldMBArray
  , atomicModifyMBArray
  , atomicModifyMBArray_
  , atomicModifyMBArray2
  -- *
  , thawBArray
  , thawCopyBArray
  , freezeMBArray
  , freezeCopyMBArray
  , copyBArray
  , moveMBArray
  , cloneBArray
  , cloneMBArray
  -- * List
  , fromListBArray
  , fromListBArrayN
  , toListBArray
  -- * Helpers
  , foldrBArray
  , traverseBArray
  ) where

import Control.DeepSeq
import Control.Monad.ST
import Control.Prim.Monad
import Data.Prim
import Data.Bits
import qualified Data.Prim.MArray.Internal as I
import Data.Prim.MRef.Atomic
import Data.Prim.MRef.Internal
import Foreign.Prim

type Array e = BArray e

instance Show e => Show (BArray e) where
  showsPrec n arr
    | n > 1 = ('(' :) . inner . (')' :)
    | otherwise = inner
    where
      inner = ("Array " ++) . shows (toList arr)

instance IsList (BArray e) where
  type Item (Array e) = e
  fromList = fromListBArray
  fromListN n = fromListBArrayN (Size n)
  toList = toListBArray

data MBArray e s = MBArray (MutableArray# s e)

-- | Check if both of the arrays refer to the exact same one. None of the elements are
-- evaluated.
instance Eq (MBArray e s) where
  MBArray ma1# == MBArray ma2# = isTrue# (sameMutableArray# ma1# ma2#)


data BArray e = Array (Array# e)

instance Functor BArray where
  fmap f a = runST $ traverseBArray (pure . f) a

instance MRef (MBArray e) where
  type Elt (MBArray e) = e
  newRawMRef = newRawMBArray 1
  {-# INLINE newRawMRef #-}
  readMRef mba = readMBArray mba 0
  {-# INLINE readMRef #-}
  writeMRef mba = writeMBArray mba 0
  {-# INLINE writeMRef #-}
  newMRef = newMBArray 1
  {-# INLINE newMRef #-}


instance AtomicMRef (MBArray e) where
  casMRef mba = casMBArray mba 0
  {-# INLINE casMRef #-}

instance Num e => AtomicCountMRef (MBArray e)
instance Bits e => AtomicBitsMRef (MBArray e)

instance I.MArray (MBArray e) where
  type Array (MBArray e) = BArray e
  indexArray = indexBArray
  {-# INLINE indexArray #-}
  sizeOfArray = sizeOfBArray
  {-# INLINE sizeOfArray #-}
  getSizeOfMArray = pure . sizeOfMBArray
  {-# INLINE getSizeOfMArray #-}
  thawArray = thawBArray
  {-# INLINE thawArray #-}
  thawCopyArray = thawCopyBArray
  {-# INLINE thawCopyArray #-}
  freezeMArray = freezeMBArray
  {-# INLINE freezeMArray #-}
  freezeCopyMArray = freezeCopyMBArray
  {-# INLINE freezeCopyMArray #-}
  newRawMArray = newRawMBArray
  {-# INLINE newRawMArray #-}
  readMArray = readMBArray
  {-# INLINE readMArray #-}
  writeMArray = writeMBArray
  {-# INLINE writeMArray #-}
  newMArray = newMBArray
  {-# INLINE newMArray #-}
  copyArray = copyBArray
  {-# INLINE copyArray #-}
  moveMArray = moveMBArray
  {-# INLINE moveMArray #-}
  cloneArray = cloneBArray
  {-# INLINE cloneArray #-}
  cloneMArray = cloneMBArray
  {-# INLINE cloneMArray #-}



sizeOfBArray :: Array e -> Size
sizeOfBArray (Array a#) = Size (I# (sizeofArray# a#))
{-# INLINE sizeOfBArray #-}

-- | Index an element of a pure boxed array.
--
-- [Unsafe index] Negative or larger than array size can fail with unchecked exception
--
-- ==== __Examples__
--
-- >>> import Data.Prim.Array.Boxed
-- >>> let a = makeBArray 1024 (\i -> [0 .. i])
-- >>> print $ indexBArray a 1
-- [0,1]
-- >>> print $ indexBArray a 5
-- [0,1,2,3,4,5]
--
-- @since 0.1.0
indexBArray :: Array e -> Int -> e
indexBArray (Array a#) (I# i#) =
  case indexArray# a# i# of
    (# x #) -> x
{-# INLINE indexBArray #-}

-- | Create a mutable boxed array where each element is set to the supplied initial
-- value, which is evaluated immediately before that. See `newMBArrayLazy` for an ability
-- to initialize with a thunk or `newRawBArray` that will set each element to an
-- `UndefinedElement` exception.
--
-- [Unsafe size] Negative or too large of an array size can kill the current thread with
-- `HeapOverflow` asynchronous exception.
--
-- ====__Examples__
--
-- >>> newMBArray 10 'A' >>= freezeMBArray
-- Array "AAAAAAAAAA"
--
-- @since 0.1.0
newMBArray :: MonadPrim s m => Size -> e -> m (MBArray e s)
newMBArray sz a = seqPrim a >>= newMBArrayLazy sz
{-# INLINE newMBArray #-}

-- | Same as `newMBArray`, except initial element is allowed to be a thunk. Prefer using
-- `newRawMBArray`, instead of supplying an `error`, whenever initial element is not
-- known ahead of time. Or even better try using other creation functions that iterate
-- over an array and overwrite each element, such as `makeMBArray`.
--
-- [Unsafe size] Negative or too large of an array size can kill the current thread with `HeapOverflow`
-- asynchronous exception.
--
-- @since 0.1.0
newMBArrayLazy :: MonadPrim s m => Size -> e -> m (MBArray e s)
newMBArrayLazy (Size (I# n#)) a =
  prim $ \s ->
    case newArray# n# a s of
      (# s', ma# #) -> (# s', MBArray ma# #)
{-# INLINE newMBArrayLazy #-}

-- | Create new mutable array, where each element is set to a thunk that throws an error
-- when evaluated. This is useful when there is a plan to iterate over the whole array
-- and write values into each cell monadically or in some index aware fashion.
--
-- [Unsafe size] Negative or too large of an array size can kill the current thread with `HeapOverflow`
-- asynchronous exception.
--
-- ==== __Examples__
--
-- >>> import Control.Prim.Monad
-- >>> ma <- newRawMBArray 10 :: IO (MBArray Int RW)
-- >>> sizeOfMBArray ma
-- Size 10
-- >>> readMBArray ma 1
-- *** Exception: undefined array element: Data.Prim.MAray.Boxed.newRawMBArray
--
-- @since 0.1.0
newRawMBArray :: MonadPrim s m => Size -> m (MBArray e s)
newRawMBArray sz = newMBArrayLazy sz (uninitialized "Data.Prim.MAray.Boxed" "newRawMBArray")
{-# INLINE newRawMBArray #-}

-- | Get the size of a mutable boxed array
--
-- >>> ma <- newMBArray 1024 "Element of each cell"
-- >>> sizeOfMBArray ma
-- Size 1024
--
-- @since 0.1.0
sizeOfMBArray :: MBArray e s -> Size
sizeOfMBArray (MBArray ma#) = Size (I# (sizeofMutableArray# ma#))
{-# INLINE sizeOfMBArray #-}


-- | Read an element from mutable boxed array at a supplied index.
--
-- [Unsafe index] Negative or larger than array size can fail with unchecked exception
--
-- ==== __Examples__
--
-- >>> ma <- makeMBArray 10 (pure . ("Element ix: " ++) . show)
-- >>> readMBArray ma 5
-- "Element ix: 5"
--
-- @since 0.1.0
readMBArray :: MonadPrim s m => MBArray e s -> Int -> m e
readMBArray (MBArray ma#) (I# i#) = prim (readArray# ma# i#)
{-# INLINE readMBArray #-}

-- | Write an element into a mutable boxed array at a supplied index strictly. An
-- element will be evaluated to WHNF.
--
-- [Unsafe index] Negative or larger than array size can fail with unchecked exception
--
-- ==== __Examples__
--
-- >>> ma <- newMBArray 4 (Nothing :: Maybe Int)
-- >>> writeMBArray ma 2 (Just 2)
-- >>> freezeMBArray ma
-- Array [Nothing,Nothing,Just 2,Nothing]
--
-- Important to note that an element is evaluated prior to being written into a cell, so
-- it will not overwrite a value with if it evaluates to an exception:
--
-- >>> import Control.Exception
-- >>> writeMBArray ma 2 (throw DivideByZero)
-- *** Exception: divide by zero
-- >>> freezeMBArray ma
-- Array [Nothing,Nothing,Just 2,Nothing]
--
-- But it is evaluated to Normal Form, so it is still possible to write something that
-- eventually evaluates to bottom.
--
-- >>> writeMBArray ma 3 (Just (7 `div` 0 ))
-- >>> freezeMBArray ma
-- Array [Nothing,Nothing,Just 2,Just *** Exception: divide by zero
--
-- Either `deepseq` or `writeMBArrayDeep` can be used to alleviate that.
--
-- @since 0.1.0
writeMBArray :: MonadPrim s m => MBArray e s -> Int -> e -> m ()
writeMBArray ma i x = seqPrim x >>= writeMBArrayLazy ma i
{-# INLINE writeMBArray #-}


-- | Same as `writeMBArray` but write a thunk into an array instead of an actual
-- element. Careful with memory leaks and thunks that can evaluate to exceptions.
--
-- [Unsafe index] Negative or larger than array size can fail with unchecked exception
--
-- @since 0.1.0
writeMBArrayLazy :: MonadPrim s m => MBArray e s -> Int -> e -> m ()
writeMBArrayLazy (MBArray ma#) (I# i#) a = prim_ (writeArray# ma# i# a)
{-# INLINE writeMBArrayLazy #-}


-- | Same as `writeMBArray` but ensure that the value being written is fully evaluated.
--
-- [Unsafe index] Negative or larger than array size can fail with unchecked exception
--
-- @since 0.1.0
writeMBArrayDeep :: (MonadPrim s m, NFData e) => MBArray e s -> Int -> e -> m ()
writeMBArrayDeep ma i x = x `deepseq` writeMBArrayLazy ma i x
{-# INLINE writeMBArrayDeep #-}

-- | Convert a pure immutable boxed array into a mutable boxed array. See
-- `thawCopyBArray` for a safer alternative.
--
-- [Unsafe array] There is no copying being done, therefore any modification done to the
-- mutable array will be reflected in the immutable as well.
--
-- ====__Examples__
--
-- The correct way to use it is with combining it with something like `cloneBArray` or
-- `copyBArray`, in order to preserve referential transperancy
--
-- >>> let a = fromListBArray [1 .. 5 :: Int]
-- >>> ma <- thawBArray $ cloneBArray a 0 (sizeOfBArray a)
-- >>> writeMBArray ma 1 10
-- >>> freezeMBArray ma
-- Array [1,10,3,4,5]
-- >>> print a
-- Array [1,2,3,4,5]
--
-- Be careful not mutate pure immutable arrays, that are still being referenced in some pure
-- setting.
--
-- >>> ma' <- thawBArray a
-- >>> writeMBArray ma' 0 100000
-- >>> print a
-- Array [100000,2,3,4,5]
--
-- @since 0.1.0
thawBArray :: MonadPrim s m => Array e -> m (MBArray e s)
thawBArray (Array a#) = prim $ \s ->
  case unsafeThawArray# a# s of
    (# s', ma# #) -> (# s', MBArray ma# #)
{-# INLINE thawBArray #-}

-- | Make a copy of a subsection of an immutable array and then convert the result into
-- a mutable array.
--
-- > ma' <- thawCopyBArray a i n
--
-- Is equivalent to:
--
-- > ma' <- newRawMBArray n >>= \ma -> ma <$ copyBArray a i ma 0 n
--
-- [Unsafe offset] Offset cannot be negative or larger than the size of an array,
-- otherwise it can result in an unchecked exception
--
-- [Unsafe new size] Number of elements to be copied cannot be larger than the size of an
-- array minus the offset.
--
-- ====__Examples__
--
-- >>> let a = fromListBArray [1 .. 5 :: Int]
-- >>> ma <- thawCopyBArray a 1 3
-- >>> writeMBArray ma 1 10
-- >>> freezeMBArray ma
-- Array [2,10,4]
-- >>> print a
-- Array [1,2,3,4,5]
--
-- @since 0.1.0
thawCopyBArray :: MonadPrim s m => Array e -> Int -> Size -> m (MBArray e s)
thawCopyBArray (Array a#) (I# i#) (Size (I# n#)) = prim $ \s ->
  case thawArray# a# i# n# s of
    (# s', ma# #) -> (# s', MBArray ma# #)
{-# INLINE thawCopyBArray #-}

-- | Convert a mutable boxed array into an immutable.
--
-- [Unsafe further mutation] After the mutable array is frozen, it is unsafe to mutate
-- it, because the changes will be reflected in the newly created immutable array as well.
--
-- See `freezeCopyMBArray` for a safer alternative or use `cloneMBArray` prior to
-- freezing if further mutation of an array is still needed.
--
-- @since 0.1.0
freezeMBArray :: MonadPrim s m => MBArray e s -> m (Array e)
freezeMBArray (MBArray ma#) = prim $ \s ->
  case unsafeFreezeArray# ma# s of
    (# s', a# #) -> (# s', Array a# #)
{-# INLINE freezeMBArray #-}


-- | Same as `freezeMBArray`, but makes a copy of a subsection of a mutable array before
-- converting it into an immutable.
--
-- [Unsafe offset] Offset cannot be negative or larger than the size of an array,
-- otherwise it can result in an unchecked exception
--
-- [Unsafe new size] Number of elements to be copied cannot be larger than the size of an
-- array minus the offset.
--
-- @since 0.1.0
freezeCopyMBArray :: MonadPrim s m => MBArray e s -> Int -> Size -> m (Array e)
freezeCopyMBArray (MBArray ma#) (I# i#) (Size (I# n#)) = prim $ \s ->
  case freezeArray# ma# i# n# s of
    (# s', a# #) -> (# s', Array a# #)
{-# INLINE freezeCopyMBArray #-}


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
-- >>> let a = fromListBArray ['a'..'z']
-- >>> a
-- Array "abcdefghijklmnopqrstuvwxyz"
-- >>> cloneBArray a 23 3
-- Array "xyz"
--
-- @since 0.1.0
cloneBArray :: Array e -> Int -> Size -> Array e
cloneBArray (Array a#) (I# i#) (Size (I# n#)) = Array (cloneArray# a# i# n#)
{-# INLINE cloneBArray #-}

-- | Same as `cloneBArray`, except it works on mutable arrays
--
-- [Unsafe offset] Offset cannot be negative or larger than the size of an array,
-- otherwise it can result in an unchecked exception
--
-- [Unsafe new size] Number of elements to be copied cannot be larger than the size of an
-- array minus the offset.
--
-- @since 0.1.0
cloneMBArray :: MonadPrim s m => MBArray e s -> Int -> Size -> m (MBArray e s)
cloneMBArray (MBArray ma#) (I# i#) (Size (I# n#)) =
  prim $ \s ->
    case cloneMutableArray# ma# i# n# s of
      (# s', ma'# #) -> (# s', MBArray ma'# #)
{-# INLINE cloneMBArray #-}


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
copyBArray ::
     MonadPrim s m
  => Array e -- ^ Source immutable array
  -> Int -- ^ Offset into the source immutable array
  -> MBArray e s -- ^ Destination mutable array
  -> Int -- ^ Offset into the destination mutable array
  -> Size -- ^ Number of elements to copy over
  -> m ()
copyBArray (Array src#) (I# srcOff#) (MBArray dst#) (I# dstOff#) (Size (I# n#)) =
  prim_ (copyArray# src# srcOff# dst# dstOff# n#)
{-# INLINE copyBArray #-}

-- | Copy a subsection of a mutable array into a subsection of another or the same
-- mutable array. Therefore, unlike `copyBArray`, memory overlap is allowed.
--
-- [Unsafe offset] Each offset cannot be negative or larger than the size of a
-- corresponding array, otherwise it can result in an unchecked exception
--
-- [Unsafe new size] Number of elements to be copied cannot be larger than the size of an
-- each array minus their corersponding offsets.
--
-- @since 0.1.0
moveMBArray ::
     MonadPrim s m
  => MBArray e s -- ^ Source mutable array
  -> Int -- ^ Offset into the source mutable array
  -> MBArray e s -- ^ Destination mutable array
  -> Int -- ^ Offset into the destination mutable array
  -> Size -- ^ Number of elements to copy over
  -> m ()
moveMBArray (MBArray src#) (I# srcOff#) (MBArray dst#) (I# dstOff#) (Size (I# n#)) =
  prim_ (copyMutableArray# src# srcOff# dst# dstOff# n#)
{-# INLINE moveMBArray #-}


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
-- >>> ma <- makeMBArray 5 (pure . (*10))
-- >>> freezeMBArray ma
-- Array [0,10,20,30,40]
--
-- A possible mistake is to try and pass the expected value, instead of an actual element:
--
-- >>> casMBArray ma 2 20 1000
-- (False,20)
-- >>> freezeMBArray ma
-- Array [0,10,20,30,40]
--
-- But this will get us nowhere, since what we really need is the actual reference to the
-- value currently in the array cell
--
-- >>> expected <- readMBArray ma 2
-- >>> r@(_, currentValue) <- casMBArray ma 2 expected 1000
-- >>> freezeMBArray ma
-- Array [0,10,1000,30,40]
-- >>> r
-- (True,1000)
--
-- In a concurrent setting current value can potentially be modified by some other
-- thread, therefore returned value can be immedieately used as the expected one to the
-- next call, if we don want to retry the atomic modification:
--
-- >>> casMBArray ma 2 currentValue 2000
-- (True,2000)
-- >>> freezeMBArray ma
-- Array [0,10,2000,30,40]
--
-- @since 0.1.0
casMBArray ::
     MonadPrim s m
  => MBArray e s -- ^ Mutable array to mutate
  -> Int -- ^ Index at which the cell should be set to the new value
  -> e -- ^ Reference to the expected boxed value
  -> e -- ^ New value to update the cell with
  -> m (Bool, e)
casMBArray (MBArray ma#) (I# i#) expected new =
  prim $ \s ->
    case casArray# ma# i# expected new s of
      (# s', failed#, actual #) -> (# s', (isTrue# (failed# ==# 0#), actual) #)
{-# INLINE casMBArray #-}


atomicModifyMBArray# :: MonadPrim s m => MBArray e s -> Int -> (e -> (# e, b #)) -> m b
atomicModifyMBArray# ma@(MBArray ma#) i@(I# i#) f = do
  current0 <- readMBArray ma i
  prim $
    let go expected s =
          case f expected of
            (# new, artifact #) ->
              case casArray# ma# i# expected new s of
                (# s', 0#, _ #)     -> (# s', artifact #)
                (# s', _, actual #) -> go actual s'
     in go current0
{-# INLINE atomicModifyMBArray# #-}


atomicModifyFetchNewMBArray :: MonadPrim s m => MBArray e s -> Int -> (e -> e) -> m e
atomicModifyFetchNewMBArray ma i f =
  atomicModifyMBArray# ma i (\a -> let a' = f a in (# a', a' #))
{-# INLINE atomicModifyFetchNewMBArray #-}

-- atomicModifyFetchNewMBArray ma@(MBArray ma#) i@(I# i#) f = do
--   current0 <- readMBArray ma i
--   prim $ \s0 ->
--     let go expected s =
--           case casBArray# ma# i# expected (f expected) s of
--             (# s', 0#, actual #) -> go actual s'
--             (# s', _, current #) -> (# s', current #)
--     in go current0 s0
  -- let go e =
  --       casMBArray ma i e (f e) >>= \case
  --         (True, new) -> pure new
  --         (_, current) -> go current
  --  in readMBArray ma i >>= go

atomicModifyFetchOldMBArray :: MonadPrim s m => MBArray e s -> Int -> (e -> e) -> m e
atomicModifyFetchOldMBArray ma i f =
  atomicModifyMBArray# ma i (\a -> (# f a, a #))
{-# INLINE atomicModifyFetchOldMBArray #-}
  -- let go e =
  --       casMBArray ma i e (f e) >>= \case
  --         (True, _new) -> pure e
  --         (_, current) -> go current
  --  in readMBArray ma i >>= go



atomicModifyMBArray :: MonadPrim s m => MBArray e s -> Int -> (e -> (e, b)) -> m b
atomicModifyMBArray ma i f =
  atomicModifyMBArray# ma i (\a -> let (a', b) = f a in (# a', b #))
{-# INLINE atomicModifyMBArray #-}
  -- let go e =
  --       let (new, artifact) = f e
  --        in casMBArray ma i e new >>= \case
  --             (True, _new) -> pure artifact
  --             (_, current) -> go current
  --  in readMBArray ma i >>= go


atomicModifyMBArray_ :: MonadPrim s m => MBArray e s -> Int -> (e -> e) -> m ()
atomicModifyMBArray_ ma i f =
  atomicModifyMBArray# ma i (\a -> let a' = f a in (# a', () #))
{-# INLINE atomicModifyMBArray_ #-}


atomicModifyMBArray2 :: MonadPrim s m => MBArray e s -> Int -> (e -> (e, b)) -> m (e, e, b)
atomicModifyMBArray2 ma i f =
  atomicModifyMBArray# ma i (\a -> let (a', b) = f a in (# a', (a, a', b) #))
{-# INLINE atomicModifyMBArray2 #-}


-- | Convert a list into an array strictly, i.e. each element is evaluated to WHNF prior
-- to being written into the newly created array. In order to allocate the array ahead
-- of time, the spine of a list will be evaluated first, in order to get the total
-- number of elements. Infinite lists will cause the program to halt. On the other hand
-- if the length of a list is known ahead of time, `fromListBArrayN` can be used instead as
-- optimization.
--
-- @since 0.1.0
fromListBArray :: [e] -> Array e
fromListBArray xs = fromListBArrayN (Size (length xs)) xs
{-# INLINE fromListBArray #-}

-- | Same as `fromListBArray`, except it will allocate an array exactly of @n@ size, as
-- such it will not convert any portion of the list that doesn't fit into the newly
-- created array.
--
-- [Unsafe size] if the length of supplied list is actually smaller then the expected
-- size, thunks with `UndefinedElement` will be left in the tail of the array.
--
-- ====__Examples__
--
-- >>> fromListBArrayN 3 [1 :: Int, 2, 3]
-- Array [1,2,3]
-- >>> fromListBArrayN 3 [1 :: Int ..]
-- Array [1,2,3]
-- >>> fromListBArrayN 3 [1 :: Int, 2]
-- Array [1,2*** Exception: undefined array element: Data.Prim.Array.Boxed.uninitialized
--
-- @since 0.1.0
fromListBArrayN ::
     Size -- ^ Expected @n@ size of a list
  -> [e]
  -> Array e
fromListBArrayN = I.fromListArrayN
{-# INLINE fromListBArrayN #-}

-- | Convert a pure boxed array into a list. It should work fine with GHC built-in list
-- fusion.
--
-- @since 0.1.0
toListBArray :: Array e -> [e]
toListBArray = I.toListArray
{-# INLINE toListBArray #-}

-- | Strict right fold
foldrBArray :: (e -> b -> b) -> b -> Array e -> b
foldrBArray = I.foldrArray
{-# INLINE foldrBArray #-}

makeBArray :: Size -> (Int -> e) -> Array e
makeBArray = I.makeArray
{-# INLINE makeBArray #-}

makeBArrayM :: MonadPrim s m => Size -> (Int -> m e) -> m (Array e)
makeBArrayM = I.makeArrayM
{-# INLINE makeBArrayM #-}

createBArrayM :: MonadPrim s m => Size -> (MBArray e s -> m b) -> m (b, Array e)
createBArrayM = I.createArrayM
{-# INLINE createBArrayM #-}

createBArrayM_ :: MonadPrim s m => Size -> (MBArray e s -> m b) -> m (Array e)
createBArrayM_ = I.createArrayM_
{-# INLINE createBArrayM_ #-}


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
-- >>> ma <- makeMBArray 5 $ \i -> modifyFetchRef ref (\cur -> cur ++ show i ++ ",")
-- >>> mapM_ (readMBArray ma >=> putStrLn) [0 .. 4]
-- Numbers: 0,
-- Numbers: 0,1,
-- Numbers: 0,1,2,
-- Numbers: 0,1,2,3,
-- Numbers: 0,1,2,3,4,
--
-- @since 0.1.0
makeMBArray :: MonadPrim s m => Size -> (Int -> m e) -> m (MBArray e s)
makeMBArray = I.makeMArray
{-# INLINE makeMBArray #-}

-- | Traverse an array with a monadic action.
--
-- @since 0.1.0
traverseBArray :: MonadPrim s m => (e -> m b) -> BArray e -> m (BArray b)
traverseBArray = I.traverseArray
{-# INLINE traverseBArray #-}
