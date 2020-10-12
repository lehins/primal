{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
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

    -- *** Immutable
    , BArray(..)
    , sizeOfBArray
    , indexBArray
    , copyBArray
    , cloneBArray
    , thawBArray
    , thawCopyBArray
    , toListBArray
    , fromListBArray
    , fromListBArrayN
    -- *** Mutable
    , BMArray(..)
    , sizeOfBMArray
    , readBMArray
    , writeBMArray
    , writeLazyBMArray
    , writeDeepBMArray
    , casBMArray
    , isSameBMArray
    , newBMArray
    , newLazyBMArray
    , newRawBMArray
    , makeBMArray
    , moveBMArray
    , cloneBMArray
    , freezeBMArray
    , freezeCopyBMArray
    -- ** Small Boxed Array
    -- ** Unboxed Array
    -- * Helper functions
    , uninitialized
    , makeMutWith
    , fromListMutWith
    , foldrWithFB
  ) where

import Control.Exception
import Control.DeepSeq
import Control.Monad.ST
import Control.Prim.Monad
import Data.Prim
import Foreign.Prim
import GHC.Stack

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

-- | @since 0.3.0
instance Functor BArray where
  fmap f a =
    runST $
    makeBMArray
      (sizeOfBArray a)
      (pure . f . indexBArray a) >>= freezeBMArray
  {-# INLINE fmap #-}
  (<$) x a = runST $ newLazyBMArray (sizeOfBArray a) x >>= freezeBMArray
  {-# INLINE (<$) #-}

instance Show e => Show (BArray e) where
  showsPrec n arr
    | n > 1 = ('(' :) . inner . (')' :)
    | otherwise = inner
    where
      inner = ("Array " ++) . shows (toList arr)

instance IsList (BArray e) where
  type Item (BArray e) = e
  fromList = fromListBArray
  {-# INLINE fromList #-}
  fromListN n = fromListBArrayN (coerce n)
  {-# INLINE fromListN #-}
  toList = toListBArray
  {-# INLINE toList #-}

instance e ~ Char => IsString (BArray e) where
  fromString = fromListBArray
  {-# INLINE fromString #-}


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
-- >>> let a = fromListBArray [[0 .. i] | i <- [0 .. 10 :: Int]]
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


-- | /O(sz)/ - Make an exact copy of a subsection of a pure immutable array.
--
-- [Unsafe] When any of the preconditions for @startIx@ or @sz@ is violated this function
-- can result in a copy of some data that doesn't belong to @srcArray@ or more likely a
-- failure with a segfault. Failure with out of memory is also possibility when the @sz is
-- too large.
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



-- | /O(sz)/ - Copy a subsection of an immutable array into a subsection of another
-- mutable array. The two arrays must not be the same array in different states.
--
-- [Unsafe] When any of the preconditions for @srcStartIx@, @dstStartIx@ or @sz@ is violated
-- this function can result in a copy of some data that doesn't belong to @srcArray@ or more
-- likely a failure with a segfault.
--
-- @since 0.3.0
copyBArray ::
     MonadPrim s m
  => BArray e
  -- ^ /srcArray/ - Source immutable array
  --
  -- /__Precondition:__/
  --
  -- > srcMutArray <- thawBArray srcArray
  -- > srcMutArray /= dstMutArray
  -> Int
  -- ^ /srcStartIx/ - Offset into the source immutable array where copy should start from
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= srcStartIx
  --
  -- > srcStartIx < unSize (sizeOfBArray srcArray)
  -> BMArray e s -- ^ /dstMutArray/ - Destination mutable array
  -> Int
  -- ^ /dstStartIx/ - Offset into the destination mutable array where copy should start to
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= dstStartIx
  --
  -- > dstStartIx < unSize (sizeOfBMArray dstMutArray)
  -> Size
  -- ^ /sz/ - Number of elements to copy over
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- > srcStartIx + unSize sz < unSize (sizeOfBArray srcArray)
  --
  -- > dstStartIx + unSize sz < unSize (sizeOfBMArray dstMutArray)
  --
  -> m ()
copyBArray (BArray src#) (I# srcOff#) (BMArray dst#) (I# dstOff#) (Size (I# n#)) =
  prim_ (copyArray# src# srcOff# dst# dstOff# n#)
{-# INLINE copyBArray #-}


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
-- created @dstMutArray@. This function can help avoid an issue with referential transparency
-- that is inherent to `thawBArray`.
--
-- [Unsafe] When any of the preconditions for @startIx@ or @sz@ is violated this function
-- can result in a copy of some data that doesn't belong to @srcArray@ or more likely a
-- failure with a segfault. Failure with out of memory is also possibility when the @sz is
-- too large.
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
  -- ^ /dstMutArray/ - Newly created destination mutable boxed array
thawCopyBArray (BArray a#) (I# i#) (Size (I# n#)) = prim $ \s ->
  case thawArray# a# i# n# s of
    (# s', ma# #) -> (# s', BMArray ma# #)
{-# INLINE thawCopyBArray #-}



-- | Convert a pure boxed array into a list. It should work fine with GHC built-in list
-- fusion.
--
-- @since 0.1.0
toListBArray :: BArray e -> [e]
toListBArray ba = build (\ c n -> foldrWithFB sizeOfBArray indexBArray c n ba)
{-# INLINE toListBArray #-}



-- | Same as `fromListBArray`, except it will allocate an array exactly of @n@ size, as
-- such it will not convert any portion of the list that doesn't fit into the newly
-- created array.
--
-- [Partial] When length of supplied list is in fact smaller then the expected size @sz@,
-- thunks with `UndefinedElement` exception throwing function will be placed in the tail
-- portion of the array.
--
-- [Unsafe] When a precondition @sz@ is violated this function can result in critical
-- failure with out of memory or `HeapOverflow` async exception.
--
-- ====__Examples__
--
-- >>> fromListBArrayN 3 [1 :: Int, 2, 3]
-- Array [1,2,3]
-- >>> fromListBArrayN 3 [1 :: Int ..]
-- Array [1,2,3]
--
-- @since 0.1.0
fromListBArrayN ::
     HasCallStack
  => Size -- ^ Expected @n@ size of a list
  -> [e]
  -> BArray e
fromListBArrayN sz xs =
  runST $ fromListMutWith newRawBMArray writeBMArray sz xs >>= freezeBMArray
{-# INLINE fromListBArrayN #-}


-- | Convert a list into an immutable boxed array. It is more efficient to use
-- `fromListBArrayN` when the number of elements is known ahead of time. The reason for this
-- is that it is necessary to iterate the whole list twice: once to count how many elements
-- there is in order to create large enough array that can fit them; and the second time to
-- load the actual elements. Naturally, infinite lists will grind the program to a halt.
--
-- ====__Example__
--
-- >>> fromListBArray "Hello Haskell"
-- Array "Hello Haskell"
--
-- @since 0.3.0
fromListBArray :: [e] -> BArray e
fromListBArray xs = fromListBArrayN (coerce (length xs)) xs
{-# INLINE fromListBArray #-}


-- Mutable Boxed Array --
-------------------------


-- | Mutable array with boxed elements.
--
-- @since 0.3.0
data BMArray e s = BMArray (MutableArray# s e)

-- | Check if both of the arrays refer to the exact same one. None of the elements are
-- evaluated.
instance Eq (BMArray e s) where
  (==) = isSameBMArray
  {-# INLINE (==) #-}


-- | Compare pointers for two mutable arrays and see if they refer to the exact same one.
--
-- @since 0.3.0
isSameBMArray :: BMArray a s -> BMArray a s -> Bool
isSameBMArray (BMArray ma1#) (BMArray ma2#) =
  isTrue# (sameMutableArray# ma1# ma2#)
{-# INLINE isSameBMArray #-}

-- | /O(1)/ - Get the size of a mutable boxed array
--
-- ====__Example__
--
-- >>> ma <- newBMArray 1024 "Element of each cell"
-- >>> sizeOfBMArray ma
-- Size {unSize = 1024}
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



-- | /O(1)/ - Write an element @elt@ into the mutable boxed array @dstMutArray@ at the supplied index
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
  => BMArray e s -- ^ /dstMutArray/ - An array to have the element writtent to
  -> Int
  -- ^ /ix/ - Index within the the @dstMutArray@ that a refernce to the supplied element
  -- @elt@ will be written to.
  --
  -- /__Precoditions:__/
  --
  -- > 0 <= ix
  --
  -- > ix < unSize (sizeOfMBArray srcArray)
  -> e
  -- ^ /elt/ - Element to be written into @dstMutArray@
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



-- | /O(1)/ - Compare-and-swap operation that can be used as a concurrency primitive for
-- implementing atomic operations on mutable boxed arrays. Returns a boolean value, which
-- indicates `True` for success and `False` otherwise for the update, as well as the current
-- value at the supplied index. In case of success current value returned will be the newly
-- supplied one, otherwise it will still be the old one. Note that there is no `Eq`
-- constraint on the element, that is because compare operation is done on a reference,
-- rather than on the value itself, in other words the expected value must be the exact same
-- one.
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
-- [Unsafe] Same reasons as `newBMArray`
--
-- @since 0.3.0
newLazyBMArray :: MonadPrim s m => Size -> e -> m (BMArray e s)
newLazyBMArray (Size (I# n#)) a =
  prim $ \s ->
    case newArray# n# a s of
      (# s', ma# #) -> (# s', BMArray ma# #)
{-# INLINE newLazyBMArray #-}




-- | Create new mutable array, where each element is initilized to a thunk that throws an
-- error when evaluated. This is useful when there is a plan to iterate over the whole array
-- and write values into each cell later in some index aware fashion.
--
-- [Partial] All array cells a initialized with thunks that throw `UndefinedElement`
-- exception.
--
-- [Unsafe] Same reasons as `newBMArray`
--
-- ==== __Examples__
--
-- >>> import Control.Prim.Monad
-- >>> ma <- newRawBMArray 10 :: IO (BMArray Int RW)
-- >>> sizeOfBMArray ma
-- Size {unSize = 10}
--
-- @since 0.3.0
newRawBMArray :: (HasCallStack, MonadPrim s m) => Size -> m (BMArray e s)
newRawBMArray sz = newLazyBMArray sz (uninitialized "Data.Prim.Aray" "newRawBMArray")
{-# INLINE newRawBMArray #-}



-- | Create new mutable boxed array of the supplied size and fill it with a monadic action
-- that is applied to indices of each array cell.
--
-- [Unsafe] Same reasons as `newBMArray`
--
-- ====__Examples__
--
-- >>> ma <- makeBMArray 5 $ \i -> (toEnum (i + 97) :: Char) <$ putStrLn ("Handling index: " ++ show i)
-- Handling index: 0
-- Handling index: 1
-- Handling index: 2
-- Handling index: 3
-- Handling index: 4
-- >>> freezeBMArray ma
-- Array "abcde"
--
-- @since 0.3.0
makeBMArray :: MonadPrim s m => Size -> (Int -> m e) -> m (BMArray e s)
makeBMArray = makeMutWith newRawBMArray writeBMArray
{-# INLINE makeBMArray #-}


-- | /O(1)/ - Convert a mutable boxed array into an immutable one. Use `thawBArray` in order
-- to go in the opposite direction.
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



-- | /O(sz)/ - Similar to `freezeBMArray`, except it creates a new array with the copy of a
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
  -- Should be less then actual available memory
  -> m (BArray e)
freezeCopyBMArray (BMArray ma#) (I# i#) (Size (I# n#)) = prim $ \s ->
  case freezeArray# ma# i# n# s of
    (# s', a# #) -> (# s', BArray a# #)
{-# INLINE freezeCopyBMArray #-}

-- TODO:
-- prop> cloneBMArray ma i n === freezeCopyBMArray ma i n >>= thawBArray
-- prop> cloneBMArray ma i n === newBMArray n undefined >>= \mb -> mb <$ moveBMArray ma i mb 0 n
-- | /O(sz)/ - Allocate a new mutable array of size @sz@ and copy that number of the
-- elements over from the @srcArray@ starting at index @ix@. Similar to `cloneBArray`,
-- except it works on mutable arrays.
--
-- [Unsafe] When any of the preconditions for @startIx@ or @sz@ is violated this function
-- can result in a copy of some data that doesn't belong to @srcArray@ or more likely a
-- failure with a segfault. Failure with out of memory is also possibility when the @sz is
-- too large.
--
-- @since 0.3.0
cloneBMArray ::
     MonadPrim s m
  => BMArray e s
  -- ^ /srcArray/ - Source mutable array
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
  -- Should be less then actual available memory
  -> m (BMArray e s)
cloneBMArray (BMArray ma#) (I# i#) (Size (I# n#)) =
  prim $ \s ->
    case cloneMutableArray# ma# i# n# s of
      (# s', ma'# #) -> (# s', BMArray ma'# #)
{-# INLINE cloneBMArray #-}


-- | /O(sz)/ - Copy a subsection of a mutable array into a subsection of another or the same
-- mutable array. Therefore, unlike `copyBArray`, memory overlap is allowed between source
-- and destination.
--
-- [Unsafe offset] Each offset cannot be negative or larger than the size of a
-- corresponding array, otherwise it can result in an unchecked exception
--
-- [Unsafe new size] Number of elements to be copied cannot be larger than the size of an
-- each array minus their corersponding offsets.
--
-- @since 0.3.0
moveBMArray ::
     MonadPrim s m
  => BMArray e s -- ^ /srcMutArray/ - Source mutable array
  -> Int
  -- ^ /srcStartIx/ - Offset into the source mutable array where copy should start from
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= srcStartIx
  --
  -- > srcStartIx < unSize (sizeOfBMArray srcArray)
  -> BMArray e s -- ^ /dstMutArray/ - Destination mutable array
  -> Int
  -- ^ /dstStartIx/ - Offset into the destination mutable array where copy should start to
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= dstStartIx
  --
  -- > dstStartIx < unSize (sizeOfBMArray dstMutArray)
  -> Size
  -- ^ /sz/ - Number of elements to copy over
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- > srcStartIx + unSize sz < unSize (sizeOfBMArray srcMutArray)
  --
  -- > dstStartIx + unSize sz < unSize (sizeOfBMArray dstMutArray)
  --
  -> m ()
moveBMArray (BMArray src#) (I# srcOff#) (BMArray dst#) (I# dstOff#) (Size (I# n#)) =
  prim_ (copyMutableArray# src# srcOff# dst# dstOff# n#)
{-# INLINE moveBMArray #-}


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


-------------
-- Helpers --
-- ======= --


uninitialized ::
     HasCallStack
  => String -- ^ Module name
  -> String -- ^ Function name
  -> a
uninitialized mname fname =
  throw $
  UndefinedElement $ mname ++ "." ++ fname ++ "\n" ++ prettyCallStack callStack
{-# NOINLINE uninitialized #-}


fromListMutWith ::
     Monad m
  => (Size -> m b) -- ^ Function for array creation
  -> (b -> Int -> a -> m ()) -- ^ Function for writing elements
  -> Size -- ^ Size for the created array
  -> [a] -- ^ Function for generating elements from array index
  -> m b
fromListMutWith new write sz@(Size n) ls = do
  ma <- new sz
  let go i =
        \case
          x:xs
            | i < n -> write ma i x >> go (i + 1) xs
          _ -> pure ()
  ma <$ go 0 ls
{-# INLINE fromListMutWith #-}


-- | Helper for generating mutable arrays
--
-- @since 0.3.0
makeMutWith ::
     Monad m
  => (Size -> m b) -- ^ Function for array creation
  -> (b -> Int -> a -> m ()) -- ^ Function for writing elements
  -> Size -- ^ Size for the created array
  -> (Int -> m a) -- ^ Function for generating elements from array index
  -> m b
makeMutWith new write sz@(Size n) f = do
  ma <- new sz
  let go i = when (i < n) $ f i >>= write ma i >> go (i + 1)
  ma <$ go 0
{-# INLINE makeMutWith #-}


-- | Right fold that is strict on the element. The key feature of this function is that it
--  can be used to convert an array to a list by integrating with list fusion using `build`.
--
-- @since 0.3.0
foldrWithFB ::
     (a e -> Size) -- ^ Function that produces the size of an array
  -> (a e -> Int -> e) -- ^ Indexing function
  -> (e -> b -> b) -- ^ Folding functions
  -> b -- ^ Initial accumulator
  -> a e -- ^ Array to fold over
  -> b
foldrWithFB size index c nil a = go 0
  where
    k = coerce (size a)
    go i
      | i >= k = nil
      | otherwise =
        let v = index a i
         in v `seq` (v `c` go (i + 1))
{-# INLINE[0] foldrWithFB #-}
