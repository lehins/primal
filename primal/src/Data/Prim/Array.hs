{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  ( -- $arrays
      Size(..)
    -- * Boxed Array
    -- $boxedArray

    -- ** Immutable
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
    -- ** Mutable
    , BMArray(..)
    , sizeOfBMArray
    , readBMArray
    , writeBMArray
    , writeLazyBMArray
    , writeDeepBMArray
    , isSameBMArray
    , newBMArray
    , newLazyBMArray
    , newRawBMArray
    , makeBMArray
    , moveBMArray
    , cloneBMArray
    , freezeBMArray
    , freezeCopyBMArray

    -- * Small Boxed Array
    -- ** Immutable
    -- ** Mutable

    -- * Unboxed Array
    -- ** Immutable
    , UArray(..)
    , sizeOfUArray
    , indexUArray
    , copyUArray
    , thawUArray
    -- ** Mutable
    , UMArray(..)
    , isSameUMArray
    , getSizeOfUMArray
    , readUMArray
    , writeUMArray
    , newRawUMArray
    , newPinnedRawUMArray
    , newAlignedPinnedRawUMArray
    , moveUMArray
    , setUMArray
    , shrinkUMArray
    , resizeUMArray
    , freezeUMArray
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
import Data.Prim.Class
import Foreign.Prim
import GHC.Stack

-- $arrays
--
-- Minimal interface, wrappers around primops
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


newtype Size = Size { unSize :: Int }
  deriving (Show, Eq, Ord, Num, Real, Integral, Bounded, Enum)

instance Prim Size where
  type PrimBase Size = Int


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
      inner = ("BArray " ++) . shows (toList arr)

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


-- | /O(1)/ - Get number of elements in an immutable array
--
-- Documentation for utilized primop: `sizeofArray#`.
--
-- @since 0.3.0
sizeOfBArray :: BArray e -> Size
sizeOfBArray (BArray a#) = Size (I# (sizeofArray# a#))
{-# INLINE sizeOfBArray #-}

-- | /O(1)/ - Index an element in the immutable boxed array.
--
-- Documentation for utilized primop: `indexArray#`.
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
-- Documentation for utilized primop: `cloneArray#`.
--
-- ====__Examples__
--
-- >>> let a = fromListBArray ['a'..'z']
-- >>> a
-- BArray "abcdefghijklmnopqrstuvwxyz"
-- >>> cloneBArray a 23 3
-- BArray "xyz"
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



-- | /O(sz)/ - Copy a subsection of an immutable array into a subsection of another mutable
-- array. Source and destination arrays must not be the same array in different states.
--
-- Documentation for utilized primop: `copyArray#`.
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
  -- ^ /dstStartIx/ - Offset into the destination mutable array where the copy should start
  -- at
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
  -> m ()
copyBArray (BArray src#) (I# srcOff#) (BMArray dst#) (I# dstOff#) (Size (I# n#)) =
  prim_ (copyArray# src# srcOff# dst# dstOff# n#)
{-# INLINE copyBArray #-}


-- | /O(1)/ - Convert a pure immutable boxed array into a mutable boxed array. Use
-- `freezeBMArray` in order to go in the opposite direction.
--
-- Documentation for utilized primop: `unsafeThawArray#`.
--
-- [Unsafe] This function makes it possible to break referential transparency, because any
-- subsequent destructive operation to the mutable boxed array will also be reflected in
-- the source immutable array as well. See `thawCopyBArray` that avoids this problem with
-- a fresh allocation and data copy.
--
-- ====__Examples__
--
-- >>> ma <- thawBArray $ fromListBArray [1 .. 5 :: Integer]
-- >>> writeBMArray ma 1 10
-- >>> freezeBMArray ma
-- Array [1,10,3,4,5]
--
-- Be careful not to retain a reference to the pure immutable source array after the
-- thawed version gets mutated.
--
-- >>> let a = fromListBArray [1 .. 5 :: Integer]
-- >>> ma' <- thawBArray a
-- >>> writeBMArray ma' 0 100000
-- >>> a
-- BArray [100000,2,3,4,5]
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
-- Documentation for utilized primop: `thawArray#`.
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



-- | /O(min(length list, sz))/ - Same as `fromListBArray`, except it will allocate an array exactly of @n@ size, as
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
  => Size -- ^ /sz/ - Expected number of elements in the @list@
  -> [e] -- ^ /list/ - A list to bew loaded into the array
  -> BArray e
fromListBArrayN sz xs =
  runST $ fromListMutWith newRawBMArray writeBMArray sz xs >>= freezeBMArray
{-# INLINE fromListBArrayN #-}


-- | /O(length list)/ - Convert a list into an immutable boxed array. It is more efficient to use
-- `fromListBArrayN` when the number of elements is known ahead of time. The reason for this
-- is that it is necessary to iterate the whole list twice: once to count how many elements
-- there is in order to create large enough array that can fit them; and the second time to
-- load the actual elements. Naturally, infinite lists will grind the program to a halt.
--
-- ====__Example__
--
-- >>> fromListBArray "Hello Haskell"
-- BArray "Hello Haskell"
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
-- Documentation for utilized primop: `sameMutableArray#`.
--
-- @since 0.3.0
isSameBMArray :: BMArray a s -> BMArray a s -> Bool
isSameBMArray (BMArray ma1#) (BMArray ma2#) =
  isTrue# (sameMutableArray# ma1# ma2#)
{-# INLINE isSameBMArray #-}

-- | /O(1)/ - Get the size of a mutable boxed array
--
-- Documentation for utilized primop: `sizeofMutableArray#`.
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

-- | /O(1)/ - Read an element from a mutable boxed array at the supplied index.
--
-- Documentation for utilized primop: `readArray#`.
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
  => BMArray e s -- ^ /srcMutArray/ - Array to read an element from
  -> Int
  -- ^ /ix/ - Index that refers to an element we need within the the @srcMutArray@
  --
  -- /__Precoditions:__/
  --
  -- > 0 <= ix
  --
  -- > ix < unSize (sizeOfMBArray srcMutArray)
  -> m e
readBMArray (BMArray ma#) (I# i#) = prim (readArray# ma# i#)
{-# INLINE readBMArray #-}



-- | /O(1)/ - Write an element @elt@ into the mutable boxed array @dstMutArray@ at the
-- supplied index @ix@. The actual element will be evaluated to WHNF prior to mutation.
--
-- [Unsafe] Violation of @ix@ preconditions can result in heap corruption or a failure
-- with a segfault
--
-- ==== __Examples__
--
-- >>> ma <- newBMArray 4 (Nothing :: Maybe Integer)
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
-- Documentation for utilized primop: `writeArray#`.
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
-- BArray "AAAAAAAAAA"
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
  -- Should be below some upper limit that is dictated by the operating system and the total
  -- amount of available memory
  -> e -- ^ /elt/ - Value to use for all array cells
  -> m (BMArray e s)
newBMArray sz x = x `seq` newLazyBMArray sz x
{-# INLINE newBMArray #-}

-- | Same as `newBMArray`, except initial element is allowed to be a thunk.
--
-- Documentation for utilized primop: `newArray#`.
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
-- error when evaluated. This is useful when there is a plan to later iterate over the whole
-- array and write values into each cell in some index aware fashion. Consider `makeBMArray`
-- as an alternative.
--
-- [Partial] All array cells are initialized with thunks that throw `UndefinedElement`
-- exception.
--
-- [Unsafe] Same reasons as `newBMArray`
--
-- ==== __Examples__
--
-- >>> let xs = "Hello Haskell"
-- >>> ma <- newRawBMArray (Size (length xs)) :: IO (BMArray Char RealWorld)
-- >>> mapM_ (\(i, x) -> writeBMArray ma i x) (zip [0..] xs)
-- >>> freezeBMArray ma
-- BArray "Hello Haskell"
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
-- BArray "abcde"
--
-- @since 0.3.0
makeBMArray :: MonadPrim s m => Size -> (Int -> m e) -> m (BMArray e s)
makeBMArray = makeMutWith newRawBMArray writeBMArray
{-# INLINE makeBMArray #-}


-- | /O(1)/ - Convert a mutable boxed array into an immutable one. Use `thawBArray` in order
-- to go in the opposite direction.
--
-- Documentation for utilized primop: `unsafeFreezeArray#`.
--
-- [Unsafe] This function makes it possible to break referential transparency, because any
-- subsequent destructive operation to the source mutable boxed array will also be reflected
-- in the resulting immutable array. See `freezeCopyBMArray` that avoids this problem with
-- fresh allocation.
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
-- Documentation for utilized primop: `freezeArray#`.
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
-- Documentation for utilized primop: `cloneMutableArray#`.
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
-- mutable array. Therefore, unlike `copyBArray`, memory ia allowed to overlap between source
-- and destination.
--
-- Documentation for utilized primop: `copyMutableArray#`.
--
-- [Unsafe] When any of the preconditions for @srcStartIx@, @dstStartIx@ or @sz@ is violated
-- this function can result in a copy of some data that doesn't belong to @srcArray@ or more
-- likely a failure with a segfault.
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

data UArray e = UArray ByteArray#
type role UArray nominal


-- | /O(1)/ - Get number of elements in an immutable array
--
-- Documentation for utilized primop: `sizeofByteArray#`.
--
-- @since 0.3.0
sizeOfUArray :: UArray e -> Size
sizeOfUArray (UArray a#) = Size (I# (sizeofByteArray# a#))
{-# INLINE sizeOfUArray #-}


-- | Index an element of a pure unboxed array.
--
-- Documentation for utilized primop: `indexByteArray#`.
--
-- [Unsafe] Bounds are not checked. When a precondition for @ix@ argument is violated the
-- result is either unpredictable output or failure with a segfault.
--
-- ==== __Examples__
--
-- >>> import Data.Prim.Array.Unboxed
-- >>> let a = makeUArray 1024 (\i -> [0 .. i])
-- >>> indexUArray a 1
-- [0,1]
-- >>> indexUArray a 5
-- [0,1,2,3,4,5]
--
-- @since 0.3.0
indexUArray ::
     Prim e
  => UArray e
  -- ^ /array/ - Array where to lookup an element from
  -> Int
  -- ^ /ix/ - Position of the element within the @array@
  --
  -- /__Precoditions:__/
  --
  -- > 0 <= ix
  --
  -- > ix < unSize (sizeOfUArray array)
  -> e
indexUArray (UArray a#) (I# i#) = indexByteArray# a# i#
{-# INLINE indexUArray #-}


-- | /O(sz)/ - Copy a subsection of an immutable array into a subsection of another mutable
-- array. Source and destination arrays must not be the same array in different states.
--
-- Documentation for utilized primop: `copyByteArray#`.
--
-- [Unsafe] When any of the preconditions for @srcStartIx@, @dstStartIx@ or @sz@ is violated
-- this function can result in a copy of some data that doesn't belong to @srcArray@ or
-- failure with a segfault.
--
-- @since 0.3.0
copyUArray ::
     forall e m s. (Prim e, MonadPrim s m)
  => UArray e
  -- ^ /srcArray/ - Source immutable array
  --
  -- /__Precondition:__/
  --
  -- > srcMutArray <- thawUArray srcArray
  -- > srcMutArray /= dstMutArray
  -> Int
  -- ^ /srcStartIx/ - Offset into the source immutable array where copy should start from
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= srcStartIx
  --
  -- > srcStartIx < unSize (sizeOfUArray srcArray)
  -> UMArray e s -- ^ /dstMutArray/ - Destination mutable array
  -> Int
  -- ^ /dstStartIx/ - Offset into the destination mutable array where the copy should start
  -- at
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= dstStartIx
  --
  -- > dstSize <- getSizeOfMUArray dstMutArray
  -- > dstStartIx < unSize dstSize
  -> Size
  -- ^ /sz/ - Number of elements to copy over
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- > srcStartIx + unSize sz < unSize (sizeOfUArray srcArray)
  --
  -- > dstSize <- getSizeOfMUArray dstMutArray
  -- > dstStartIx + unSize sz < unSize dstSize
  -> m ()
copyUArray (UArray src#) srcOff (UMArray dst#) dstOff n =
  let srcOff# = unOffBytes# (coerce srcOff :: Off e)
      dstOff# = unOffBytes# (coerce dstOff :: Off e)
      n# = unCountBytes# (coerce n :: Count e)
  in prim_ (copyByteArray# src# srcOff# dst# dstOff# n#)
{-# INLINE copyUArray #-}


-- | Convert a pure immutable unboxed array into a mutable unboxed array. Use
-- `freezeUMArray` in order to go in the opposite direction.
--
-- Documentation for utilized primop: `unsafeThawByteArray#`.
--
-- [Unsafe] This function makes it possible to break referential transparency, because any
-- subsequent destructive operation to the mutable unboxed array will also be reflected in
-- the source immutable array as well.
--
-- ====__Examples__
--
-- >>> ma <- thawUArray $ fromListUArray [1 .. 5 :: Int]
-- >>> writeUMArray ma 1 10
-- >>> freezeUMArray ma
-- Array [1,10,3,4,5]
--
-- Be careful not to retain a reference to the pure immutable source array after the
-- thawed version gets mutated.
--
-- >>> let a = fromListUArray [1 .. 5 :: Int]
-- >>> ma' <- thawUArray a
-- >>> writeUMArray ma' 0 100000
-- >>> a
-- UArray [100000,2,3,4,5]
--
-- @since 0.3.0
thawUArray :: MonadPrim s m => UArray e -> m (UMArray e s)
thawUArray (UArray a#) =
  prim $ \s ->
    case unsafeThawByteArray# a# s of
      (# s', ma# #) -> (# s', UMArray ma# #)
{-# INLINE thawUArray #-}


-- Mutable Unboxed Array --
---------------------------

data UMArray e s = UMArray (MutableByteArray# s)
type role UMArray nominal nominal

-- | Check if both of the arrays refer to the exact same one through poiner equality. None
-- of the elements are evaluated.
instance Eq (UMArray e s) where
  (==) = isSameUMArray
  {-# INLINE (==) #-}

-- | Compare pointers for two mutable arrays and see if they refer to the exact same one.
--
-- Documentation for utilized primop: `sameMutableByteArray#`.
--
-- @since 0.3.0
isSameUMArray :: forall a b s. UMArray a s -> UMArray b s -> Bool
isSameUMArray (UMArray ma1#) (UMArray ma2#) = isTrue# (sameMutableByteArray# ma1# ma2#)
{-# INLINE isSameUMArray #-}


-- | /O(1)/ - Get the size of a mutable unboxed array
--
-- Documentation for utilized primop: `getSizeofMutableByteArray#`.
--
-- ====__Example__
--
-- >>> ma <- thawUArray $ fromListUArray ['a' .. 'z']
-- >>> sizeOfUMArray ma
-- Size 26
--
-- @since 0.3.0
getSizeOfUMArray ::
     forall e m s. (Prim e, MonadPrim s m)
  => UMArray e s
  -> m Size
getSizeOfUMArray (UMArray ma#) =
  prim $ \s ->
    case getSizeofMutableByteArray# ma# s of
      (# s', n# #) -> (# s', coerce (fromByteCount (Count (I# n#)) :: Count e) #)
{-# INLINE getSizeOfUMArray #-}



-- | /O(1)/ - Read an element from a mutable unboxed array at the supplied index.
--
-- Documentation for utilized primop: `readMutableByteArray#`.
--
-- [Unsafe] Violation of @ix@ preconditions can result in value that doesn't belong to
-- @srcMutArray@ or a failure with a segfault
--
-- ==== __Examples__
--
-- >>> ma <- thawUArray $ fromListUArray "Hi!"
-- >>> readUMArray ma 2
-- '!'
--
-- @since 0.3.0
readUMArray ::
     (Prim e, MonadPrim s m)
  => UMArray e s -- ^ /srcMutArray/ - Array to read an element from
  -> Int
  -- ^ /ix/ - Index for the element we need within the the @srcMutArray@
  --
  -- /__Precoditions:__/
  --
  -- > 0 <= ix
  --
  -- > srcSize <- getSizeOfMUArray srcMutArray
  -- > ix < unSize srcSize
  -> m e
readUMArray (UMArray ma#) (I# i#) = prim (readMutableByteArray# ma# i#)
{-# INLINE readUMArray #-}


-- | Write an element into an unboxed mutable array at a supplied index.
--
-- Documentation for utilized primop: `writeMutableByteArray#`.
--
-- [Unsafe] Violation of @ix@ preconditions can result in heap corruption or a failure
-- with a segfault
--
-- ==== __Examples__
--
-- >>> ma <- newUMArray 4 (Nothing :: Maybe Int)
-- >>> writeUMArray ma 2 (Just 2)
-- >>> freezeUMArray ma
-- UArray [Nothing,Nothing,Just 2,Nothing]
--
-- @since 0.3.0
writeUMArray :: (Prim e, MonadPrim s m) => UMArray e s -> Int -> e -> m ()
writeUMArray (UMArray ma#) (I# i#) a = prim_ (writeMutableByteArray# ma# i# a)
{-# INLINE writeUMArray #-}


-- | /O(1)/ - Allocate new mutable unboxed array. None of the elements are initialized so
-- expect it to contain some random garbage.
--
-- Documentation for utilized primop: `newByteArray#`.
--
-- [Unsafe] When any of preconditions for @sz@ argument is violated the outcome is
-- unpredictable. One possible outcome is termination with `HeapOverflow` async
-- exception. In a pure setting, such as when executed within `runST`, if each cell in new
-- array is not overwritten it can lead to violation of referential transparency, because
-- contents of newly allocated unboxed array is non-determinstic.
--
-- ==== __Examples__
--
-- >>> let xs = "Hello Haskell"
-- >>> ma <- newRawUMArray (Size (length xs)) :: IO (UMArray Char RealWorld)
-- >>> mapM_ (\(i, x) -> writeUMArray ma i x) (zip [0..] xs)
-- >>> freezeUMArray ma
-- UArray "Hello Haskell"
--
-- @since 0.3.0
newRawUMArray ::
     forall e m s. (Prim e, MonadPrim s m)
  => Size
  -- ^ /sz/ - Size of the array in number of elements.
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- Susceptible to overflow:
  --
  -- > 0 <= toByteCount (Count (unSize n) :: Count e)
  --
  -- Should be below some upper limit that is dictated by the operating system and the total
  -- amount of available memory
  -> m (UMArray e s)
newRawUMArray n =
  prim $ \s ->
    case newByteArray# (unCountBytes# (coerce n :: Count e)) s of
      (# s', ma# #) -> (# s', UMArray ma# #)
{-# INLINE newRawUMArray #-}

-- | /O(1)/ - Same as `newRawUMArray` except allocate new mutable unboxed array as pinned
--
-- Documentation for utilized primop: `newPinnedByteArray#`.
--
-- [Unsafe] Same reasons as in `newRawUMArray`.
--
-- @since 0.3.0
newPinnedRawUMArray ::
     forall e m s. (Prim e, MonadPrim s m)
  => Size
  -> m (UMArray e s)
newPinnedRawUMArray n =
  prim $ \s ->
    case newPinnedByteArray# (unCountBytes# (coerce n :: Count e)) s of
      (# s', ma# #) -> (# s', UMArray ma# #)
{-# INLINE newPinnedRawUMArray #-}

-- | /O(1)/ - Same as `newPinnedRawUMArray` except allocate new mutable unboxed array as
-- pinned and aligned according to the `Prim` instance for the type of element @__e__@
--
-- Documentation for utilized primop: `newAlignedPinnedByteArray#`.
--
-- [Unsafe] Same reasons as in `newRawUMArray`.
--
-- @since 0.3.0
newAlignedPinnedRawUMArray ::
     forall e m s. (Prim e, MonadPrim s m)
  => Size
  -> m (UMArray e s)
newAlignedPinnedRawUMArray n =
  prim $ \s ->
    let c# = unCountBytes# (coerce n :: Count e)
        a# = alignment# (proxy# :: Proxy# e)
     in case newAlignedPinnedByteArray# c# a# s of
          (# s', ma# #) -> (# s', UMArray ma# #)
{-# INLINE newAlignedPinnedRawUMArray #-}


-- | /O(sz)/ - Copy a subsection of a mutable array into a subsection of another or the same
-- mutable array. Therefore, unlike `copyBArray`, memory ia allowed to overlap between
-- source and destination.
--
-- Documentation for utilized primop: `copyMutableByteArray#`.
--
-- [Unsafe] When any of the preconditions for @srcStartIx@, @dstStartIx@ or @sz@ is violated
-- this function can result in a copy of some data that doesn't belong to @srcArray@ or
-- failure with a segfault.
--
-- @since 0.3.0
moveUMArray ::
     forall e m s. (Prim e, MonadPrim s m)
  => UMArray e s -- ^ /srcMutArray/ - Source mutable array
  -> Int
  -- ^ /srcStartIx/ - Offset into the source mutable array where copy should start from
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= srcStartIx
  --
  -- > srcSize <- getSizeOfMUArray srcMutArray
  -- > srcStartIx < unSize srcSize
  -> UMArray e s -- ^ /dstMutArray/ - Destination mutable array
  -> Int
  -- ^ /dstStartIx/ - Offset into the destination mutable array where copy should start to
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= dstStartIx
  --
  -- > dstSize <- getSizeOfMUArray dstMutArray
  -- > dstStartIx < unSize dstSize
  -> Size
  -- ^ /sz/ - Number of elements to copy over
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- > srcSize <- getSizeOfMUArray srcMutArray
  -- > srcStartIx + unSize sz < unSize srcSize
  --
  -- > dstSize <- getSizeOfMUArray dstMutArray
  -- > dstStartIx + unSize sz < unSize dstSize
  -> m ()
moveUMArray (UMArray src#) srcOff (UMArray dst#) dstOff n =
  let srcOff# = unOffBytes# (coerce srcOff :: Off e)
      dstOff# = unOffBytes# (coerce dstOff :: Off e)
      n# = unCountBytes# (coerce n :: Count e)
  in prim_ (copyMutableByteArray# src# srcOff# dst# dstOff# n#)
{-# INLINE moveUMArray #-}


-- | /O(n)/ - Write the same element into the @dstMutArray@ mutable array @n@ times starting
-- at @dstStartIx@ offset.
--
-- [Unsafe]
--
-- @since 0.3.0
setUMArray ::
     (Prim e, MonadPrim s m)
  => UMArray e s -- ^ /dstMutArray/ - Mutable array
  -> Int
  -- ^ /dstStartIx/ - Offset into the mutable array
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= dstStartIx
  --
  -- > dstSize <- getSizeOfMUArray dstMutArray
  -- > dstStartIx < unSize dstSize
  -> Size
  -- ^ /n/ - Number of elements to overwrite
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= n
  --
  -- > dstSize <- getSizeOfMUArray dstMutArray
  -- > dstStartIx + unSize n < unSize dstSize
  -> e -- ^ /elt/ - Value to overwrite the cells with in the specified block
  -> m ()
setUMArray (UMArray ma#) (I# o#) (Size (I# n#)) a =
  prim_ (setMutableByteArray# ma# o# n# a)
{-# INLINE setUMArray #-}


-- | /O(1)/ - Reduce the size of a mutable unboxed array.
--
-- Documentation for utilized primop: `shrinkMutableByteArray#`.
--
-- [Unsafe] - Violation of preconditions for @sz@ leads to undefined behavior
--
-- 0.3.0
shrinkUMArray ::
     forall e m s. (MonadPrim s m, Prim e)
  => UMArray e s -- ^ /mutArray/ - Mutable unboxed array to be shrunk
  -> Size
  -- ^ /sz/ - New size for the array in number of elements
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- > curSize <- getSizeOfUMArray mutArray
  -- > sz <= curSize
  -> m ()
shrinkUMArray (UMArray mb#) sz =
  prim_ (shrinkMutableByteArray# mb# (unCountBytes# (coerce sz :: Count e)))
{-# INLINE shrinkUMArray #-}

-- | /O(1)/ - Either grow or shrink the size of a mutable unboxed array, possibly without
-- new allocation and data copy. Source array @srcArray@ should no longer be used after the
-- resize operation completes. In case when in place resize was not possible the new array
-- is allocated as unpinned.
--
-- Documentation on the utilized primop: `resizeMutableByteArray#`.
--
-- [Unsafe] - Same reasons as in `newRawUMArray`. When size @sz@ is larger then the size of
-- @srcMutArray@ then @dstMutArray@ will contain uninitialized memory at its end, hence a
-- potential problem for referential transparency.
--
-- 0.3.0
resizeUMArray ::
     forall e m s. (MonadPrim s m, Prim e)
  => UMArray e s -- ^ /srcMutArray/ - Mutable unboxed array to be shrunk
  -> Size
  -- ^ /sz/ - New size for the array in number of elements
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- Susceptible to overflow:
  --
  -- > 0 <= toByteCount (Count (unSize n) :: Count e)
  --
  -- Should be below some upper limit that is dictated by the operating system and the total
  -- amount of available memory
  -> m (UMArray e s) -- ^ /dstMutArray/ - produces a resized version of /srcMutArray/.
resizeUMArray (UMArray mb#) sz =
  prim $ \s ->
    case resizeMutableByteArray# mb# (unCountBytes# (coerce sz :: Count e)) s of
      (# s', mb'# #) -> (# s', UMArray mb'# #)
{-# INLINE resizeUMArray #-}



-- | /O(1)/ - Convert a mutable unboxed array into an immutable one. Use `thawUArray` in order
-- to go in the opposite direction.
--
-- Documentation on the utilized primop: `unsafeFreezeByteArray#`.
--
-- [Unsafe] This function makes it possible to break referential transparency, because any
-- subsequent destructive operation to the source mutable boxed array will also be reflected
-- in the resulting immutable array. See `freezeCopyBMArray` that avoids this problem with
-- fresh allocation.
--
-- @since 0.3.0
freezeUMArray :: MonadPrim s m => UMArray e s -> m (UArray e)
freezeUMArray (UMArray ma#) = prim $ \s ->
  case unsafeFreezeByteArray# ma# s of
    (# s', a# #) -> (# s', UArray a# #)
{-# INLINE freezeUMArray #-}

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