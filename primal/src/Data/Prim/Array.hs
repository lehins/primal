{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Data.Prim.Array
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Array
  ( -- * Arrays
    -- $arrays
      Size(..)
    -- ** Boxed Array
    -- $boxedArray
    --
    -- *** Immutable
    , BArray(..)
    , sizeOfBArray
    , indexBArray
    , cloneBArray
    , thawBArray
    , thawCopyBArray
    -- *** Mutable
    , BMArray(..)
    , sizeOfBMArray
    , readBMArray
    , writeBMArray
    , writeLazyBMArray
    , writeDeepBMArray
    , casBMArray
    , newBMArray
    , newLazyBMArray
    , freezeBMArray
    , freezeCopyBMArray
    -- ** Small Boxed Array
    -- ** Unboxed Array
  ) where

import Data.Prim
import Foreign.Prim
import Control.Prim.Monad
import Control.DeepSeq

-- $arrays
--
-- Minimal interface
--
-- Indexing and Size type
--
-- As in the rest of the library majority of the functions are unsafe.
--
-- no fusion
--
-- Boxed vs unboxed concept
--
-- Mutable vs Immutable
--
-- Note more features in primal-memory and primal-mutable

-----------------
-- Boxed Array --
-- =========== --


-- Immutable Boxed Array --
---------------------------

-- $boxedArray A boxed array is essentially a contiguous chunk of memory that holds
-- pointers to actual elements that are being stored somwhere else on the heap. Therefore
-- it is more efficient to use `UArray` if the element being stored has a `Prim` instance
-- or can have created for it, because this avoids an extra level of indirection. However
-- this is not always possible and for this reason we have boxed arrays.


-- | Immutable array with boxed elements.
--
-- @since 0.3.0
data BArray e = BArray (Array# e)

-- instance Show e => Show (BArray e) where
--   showsPrec n arr
--     | n > 1 = ('(' :) . inner . (')' :)
--     | otherwise = inner
--     where
--       inner = ("Array " ++) . shows (toList arr)

-- instance IsList (BArray e) where
--   type Item (Array e) = e
--   fromList = fromListBArray
--   fromListN n = fromListBArrayN (Size n)
--   toList = toListBArray

-- | /O(1)/ - Get the number of elements in an immutable array
--
-- @since 0.3.0
sizeOfBArray :: BArray e -> Size
sizeOfBArray (BArray a#) = Size (I# (sizeofArray# a#))
{-# INLINE sizeOfBArray #-}

-- | /O(1)/ - Index an element in the immutable boxed array.
--
-- [Unsafe] Bounds are not checked. When a precondition for @ix@ argument is violated the
-- result is either unpredictable output or failure with a segfault.
--
-- ==== __Examples__
--
-- >>> import Data.Prim.Array
-- >>> let a = makeBArray 1024 (\i -> [0 .. i])
-- >>> indexBArray a 1
-- [0,1]
-- >>> indexBArray a 5
-- [0,1,2,3,4,5]
--
-- @since 0.3.0
indexBArray ::
     BArray e
  -- ^ /array/ - Array where to lookup an element from
  -> Int
  -- ^ /ix/ - Position of the element within the @array@
  --
  -- /__Precoditions:__/
  --
  -- > 0 <= ix
  --
  -- > ix < unSize (sizeOfBArray array)
  --
  -> e
indexBArray (BArray a#) (I# i#) =
  case indexArray# a# i# of
    (# x #) -> x
{-# INLINE indexBArray #-}


-- | Make an exact copy of a subsection of a pure immutable array.
--
-- [Unsafe] When any of the preconditions for @startIx@ or @sz@ is violated this function
-- can result in a copy of some data that doesn't belong to @srcArray@ or more likely a
-- failure with a segfault. Failure with out of memory is also possibility.
--
-- ====__Examples__
--
-- >>> let a = fromListBArray ['a'..'z']
-- >>> a
-- Array "abcdefghijklmnopqrstuvwxyz"
-- >>> cloneBArray a 23 3
-- Array "xyz"
--
-- @since 0.3.0
cloneBArray ::
     BArray e
  -- ^ /srcArray/ - Immutable source array
  -> Int
  -- ^ /startIx/ - Location within @srcArray@ where the copy of elements should start from
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= startIx
  --
  -- > startIx < unSize (sizeOfBArray srcArray)
  -> Size
  -- ^ /sz/ - Size of the returned immutable array. Also this is the number of elements that
  -- will be copied over into the destionation array starting at the beginning.
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- > startIx + unSize sz < unSize (sizeOfBArray srcArray)
  --
  -- Should be less then the actual available memory
  -> BArray e
cloneBArray (BArray a#) (I# i#) (Size (I# n#)) = BArray (cloneArray# a# i# n#)
{-# INLINE cloneBArray #-}


-- | /O(1)/ - Convert a pure immutable boxed array into a mutable boxed array. Use
-- `freezeBMArray` in order to go in the opposite direction.
--
-- [Unsafe] This function makes it possible to break referential transparency, because any
-- subsequent destructive operation to the mutable boxed array will also be reflected in
-- the source immutable array as well. See `thawCopyBArray` that avoids this problem with
-- a fresh allocation.
--
-- ====__Examples__
--
-- >>> ma <- thawBArray $ fromListBArray [1 .. 5 :: Int]
-- >>> writeBMArray ma 1 10
-- >>> freezeBMArray ma
-- Array [1,10,3,4,5]
--
-- Be careful not to retain a reference to the pure immutable source array after the
-- thawed version gets mutated.
--
-- >>> let a = fromListBArray [1 .. 5 :: Int]
-- >>> ma' <- thawBArray a
-- >>> writeBMArray ma' 0 100000
-- >>> a
-- Array [100000,2,3,4,5]
--
-- @since 0.3.0
thawBArray ::
     MonadPrim s m
  => BArray e
  -- ^ /array/ - Source immutable array that will be thawed
  -> m (BMArray e s)
thawBArray (BArray a#) = prim $ \s ->
  case unsafeThawArray# a# s of
    (# s', ma# #) -> (# s', BMArray ma# #)
{-# INLINE thawBArray #-}

-- TODO: add a test case for the properties
-- > ma' <- thawCopyBArray a i n
--
-- Is equivalent to:
--
-- > ma' <- newRawBMArray n >>= \ma -> ma <$ copyBArray a i ma 0 n
--
-- > thawCopyBArray a i n === thawBArray $ cloneBArray a i n
--
-- | /O(sz)/ - Create a new mutable array with specified size @sz@ and copy that many
-- elements from the source immutable @srcArray@ starting at an offset @startIx@ into the
-- created @dstArray@. This function can help avoid an issue with referential transparency
-- that is inherent to `thawBArray`.
--
-- [Unsafe] When any of the preconditions for @startIx@ or @sz@ is violated this function
-- can result in a copy of some data that doesn't belong to @srcArray@ or more likely a
-- failure with a segfault. Failure with out of memory is also possibility.
--
-- ====__Examples__
--
-- >>> let a = fromListBArray [1 .. 5 :: Int]
-- >>> ma <- thawCopyBArray a 1 3
-- >>> writeBMArray ma 1 10
-- >>> freezeBMArray ma
-- Array [2,10,4]
-- >>> a
-- Array [1,2,3,4,5]
--
-- @since 0.3.0
thawCopyBArray ::
     MonadPrim s m
  => BArray e
  -- ^ /srcArray/ - Immutable source array
  -> Int
  -- ^ /startIx/ - Location within @srcArray@ where the copy of elements should start from
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= startIx
  --
  -- > startIx < unSize (sizeOfBArray srcArray)
  -> Size
  -- ^ /sz/ - Size of the returned mutable array. Also this is the number of elements that
  -- will be copied over into the destionation array starting at the beginning.
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- > startIx + unSize sz < unSize (sizeOfBArray srcArray)
  --
  -- Should be less then the actual available memory
  -> m (BMArray e s)
  -- ^ /dstArray/ - Newly created destination mutable boxed array
thawCopyBArray (BArray a#) (I# i#) (Size (I# n#)) = prim $ \s ->
  case thawArray# a# i# n# s of
    (# s', ma# #) -> (# s', BMArray ma# #)
{-# INLINE thawCopyBArray #-}


-- Mutable Boxed Array --
-------------------------



-- | Mutable array with boxed elements.
--
-- @since 0.3.0
data BMArray e s = BMArray (MutableArray# s e)

-- | Check if both of the arrays refer to the exact same one. None of the elements are
-- evaluated.
instance Eq (BMArray e s) where
  BMArray ma1# == BMArray ma2# = isTrue# (sameMutableArray# ma1# ma2#)

-- | /O(1)/ - Get the size of a mutable boxed array
--
-- ====__Example__
--
-- >>> ma <- newBMArray 1024 "Element of each cell"
-- >>> sizeOfBMArray ma
-- Size 1024
--
-- @since 0.3.0
sizeOfBMArray :: BMArray e s -> Size
sizeOfBMArray (BMArray ma#) = Size (I# (sizeofMutableArray# ma#))
{-# INLINE sizeOfBMArray #-}

-- | /O(1)/ - Read an element from a mutable boxed array at a supplied index.
--
-- [Unsafe] Violation of @ix@ preconditions can result in undefined behavior or a failure
-- with a segfault
--
-- ==== __Example__
--
-- >>> ma <- makeBMArray 10 (pure . ("Element ix: " ++) . show)
-- >>> readBMArray ma 5
-- "Element ix: 5"
--
-- @since 0.1.0
readBMArray ::
     MonadPrim s m
  => BMArray e s -- ^ /srcArray/ - An array to read an element from
  -> Int
  -- ^ /ix/ - Index that refers to an element we need within the the @srcArray@
  --
  -- /__Precoditions:__/
  --
  -- > 0 <= ix
  --
  -- > ix < unSize (sizeOfMBArray srcArray)
  --
  -> m e
readBMArray (BMArray ma#) (I# i#) = prim (readArray# ma# i#)
{-# INLINE readBMArray #-}



-- | /O(1)/ - Write an element @elt@ into the mutable boxed array @dstArray@ at the supplied index
-- @ix@. The actual element will be evaluated to WHNF prior to mutation.
--
-- [Unsafe] Violation of @ix@ preconditions can result in heap corruption or a failure
-- with a segfault
--
-- ==== __Examples__
--
-- >>> ma <- newBMArray 4 (Nothing :: Maybe Int)
-- >>> writeBMArray ma 2 (Just 2)
-- >>> freezeBMArray ma
-- Array [Nothing,Nothing,Just 2,Nothing]
--
-- It is important to note that an element is evaluated prior to being written into a
-- cell, so it will not overwrite the value of an array's cell if it evaluates to an
-- exception:
--
-- >>> import Control.Exception
-- >>> writeBMArray ma 2 (throw DivideByZero)
-- *** Exception: divide by zero
-- >>> freezeBMArray ma
-- Array [Nothing,Nothing,Just 2,Nothing]
--
-- However, it is evaluated only to Weak Head Normal Form (WHNF), so it is still possible
-- to write something that eventually evaluates to bottom.
--
-- >>> writeBMArray ma 3 (Just (7 `div` 0 ))
-- >>> freezeBMArray ma
-- Array [Nothing,Nothing,Just 2,Just *** Exception: divide by zero
--
-- Either `deepseq` or `writeDeepBMArray` can be used to alleviate that.
--
-- @since 0.3.0
writeBMArray ::
     MonadPrim s m
  => BMArray e s -- ^ /dstArray/ - An array to have the element writtent to
  -> Int
  -- ^ /ix/ - Index within the the @dstArray@ that a refernce to the supplied element
  -- @elt@ will be written to.
  --
  -- /__Precoditions:__/
  --
  -- > 0 <= ix
  --
  -- > ix < unSize (sizeOfMBArray srcArray)
  -> e
  -- ^ /elt/ - Element to be written into @dstArray@
  -> m ()
writeBMArray ma i x = x `seq` writeLazyBMArray ma i x
{-# INLINE writeBMArray #-}


-- | /O(1)/ - Same as `writeBMArray` but allows to write a thunk into an array instead of an
-- evaluated element. Careful with memory leaks and thunks that evaluate to exceptions.
--
-- [Unsafe] Same reasons as `writeBMArray`
--
-- @since 0.3.0
writeLazyBMArray :: MonadPrim s m => BMArray e s -> Int -> e -> m ()
writeLazyBMArray (BMArray ma#) (I# i#) a = prim_ (writeArray# ma# i# a)
{-# INLINE writeLazyBMArray #-}


-- | /O(1)/ - Same as `writeBMArray`, except it ensures that the value being written is
-- fully evaluated, i.e. to Normal Form (NF).
--
-- [Unsafe] Same reasons as `writeBMArray`
--
-- @since 0.3.0
writeDeepBMArray :: (MonadPrim s m, NFData e) => BMArray e s -> Int -> e -> m ()
writeDeepBMArray ma i x = x `deepseq` writeLazyBMArray ma i x
{-# INLINE writeDeepBMArray #-}



-- | Compare-and-swap operation that can be used as a concurrency primitive for
-- implementing atomic operations on mutable boxed arrays. Returns a boolean value, which
-- indicates `True` for success and `False` otherwise for the update, as well as the
-- current value at the supplied index. In case of success current value returned will be
-- the newly supplied one, otherwise it will still be the old one. Note that there is no
-- `Eq` constraint on the element, that is because compare operation is done on a
-- reference, rather than on the value itself, in other words the expected value must be
-- the exact same one.
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
  -- ^ /dstArray/ - Mutable array that will have an atomic swap operation applied to
  -> Int
  -- ^ /ix/ - Index of a cell which should be set to the new value
  --
  -- /__Precoditions:__/
  --
  -- > 0 <= ix
  --
  -- > ix < unSize (sizeOfMBArray srcArray)
  -> e -- ^ /expElt/ - Reference to the expected boxed value
  -> e -- ^ /elt/ - New value to update the cell with
  -> m (Bool, e)
casBMArray (BMArray ma#) (I# i#) expected new =
  prim $ \s ->
    case casArray# ma# i# expected new s of
      (# s', failed#, actual #) -> (# s', (isTrue# (failed# ==# 0#), actual) #)
{-# INLINE casBMArray #-}


-- | Create a mutable boxed array where each element is set to the supplied initial value
-- @elt@, which is evaluated before array allocation happens. See `newLazyBMArrayLazy` for
-- an ability to initialize with a thunk.
--
-- [Unsafe size] Violation of precondition for the @sz@ argument can result in the current
-- thread being killed with `HeapOverflow` asynchronous exception or death of the whole
-- process with some unchecked exception from RTS.
--
-- ====__Examples__
--
-- >>> newBMArray 10 'A' >>= freezeBMArray
-- Array "AAAAAAAAAA"
--
-- @since 0.3.0
newBMArray ::
     MonadPrim s m
  => Size
  -- ^ /sz/ - Size of the array
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- Should be below some pper limit that is dictated by the operating system and amount
  -- of available memory
  -> e -- ^ /elt/ - Value to use for all array cells
  -> m (BMArray e s)
newBMArray sz x = x `seq` newLazyBMArray sz x
{-# INLINE newBMArray #-}

-- | Same as `newBMArray`, except initial element is allowed to be a thunk.
--
-- [Unsafe] Same reasons as `writeBMArray`
--
-- @since 0.3.0
newLazyBMArray :: MonadPrim s m => Size -> e -> m (BMArray e s)
newLazyBMArray (Size (I# n#)) a =
  prim $ \s ->
    case newArray# n# a s of
      (# s', ma# #) -> (# s', BMArray ma# #)
{-# INLINE newLazyBMArray #-}


-- | Convert a mutable boxed array into an immutable one. Use `thawBArray` in order to go
-- in the opposite direction.
--
--
-- [Unsafe] This function makes it possible to break referential transparency, because any
-- subsequent destructive operation to the source mutable boxed array will also be
-- reflected in the resulting immutable array as well. See `freezeCopyBMArray` that avoids
-- this problem with fresh allocation.
--
-- @since 0.3.0
freezeBMArray :: MonadPrim s m => BMArray e s -> m (BArray e)
freezeBMArray (BMArray ma#) = prim $ \s ->
  case unsafeFreezeArray# ma# s of
    (# s', a# #) -> (# s', BArray a# #)
{-# INLINE freezeBMArray #-}



-- | Similar to `freezeBMArray`, except it creates a new array with the copy of a
-- subsection of a mutable array before converting it into an immutable.
--
-- [Unsafe] When any of the preconditions for @startIx@ or @sz@ is violated this function
-- can result in a copy of some data that doesn't belong to @srcArray@ or more likely a
-- failure with a segfault or out of memory exception.
--
-- @since 0.3.0
freezeCopyBMArray ::
     MonadPrim s m
  => BMArray e s
  -- ^ /srcArray/ - Source mutable array
  -> Int
  -- ^ /startIx/ - Location within @array@ where the copy of elements should start from
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= startIx
  --
  -- > startIx < unSize (sizeOfBArray srcArray)
  -> Size
  -- ^ /sz/ - Size of the returned immutable array. Also this is the number of elements that
  -- will be copied over into the destionation array starting at the beginning.
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- > startIx + unSize sz < unSize (sizeOfBArray srcArray)
  --
  -- Should be less then actual available memory
  -> m (BArray e)
freezeCopyBMArray (BMArray ma#) (I# i#) (Size (I# n#)) = prim $ \s ->
  case freezeArray# ma# i# n# s of
    (# s', a# #) -> (# s', BArray a# #)
{-# INLINE freezeCopyBMArray #-}




-----------------------
-- Small Boxed Array --
-- ================= --


-- Immutable Small Boxed Array --
---------------------------------

-- Mmutable Small Boxed Array --
---------------------------------



-------------------
-- Unboxed Array --
-- ============= --



-- Immutable Unboxed Array --
-----------------------------


-- Mutable Unboxed Array --
---------------------------
