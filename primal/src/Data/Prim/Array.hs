{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
#if __GLASGOW_HASKELL__ >= 800
  {-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif
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
    , isSameBArray
    , sizeOfBArray
    , indexBArray
    , copyBArray
    , cloneBArray
    , thawBArray
    , thawCopyBArray
    , toListBArray
    , fromListBArray
    , fromListBArrayN
    , fromBaseBArray
    , toBaseBArray
    -- ** Mutable
    , BMArray(..)
    , getSizeOfBMArray
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
    , shrinkBMArray
    , resizeBMArray
    , resizeRawBMArray
    , freezeBMArray
    , freezeCopyBMArray

    -- * Small Boxed Array
    -- ** Immutable
    , SBArray(..)
    , isSameSBArray
    , sizeOfSBArray
    , indexSBArray
    , copySBArray
    , cloneSBArray
    , thawSBArray
    , thawCopySBArray
    , toListSBArray
    , fromListSBArray
    , fromListSBArrayN
    -- ** Mutable
    , SBMArray(..)
    , isSameSBMArray
    , getSizeOfSBMArray
    , readSBMArray
    , writeSBMArray
    , writeLazySBMArray
    , writeDeepSBMArray
    , newSBMArray
    , newLazySBMArray
    , newRawSBMArray
    , makeSBMArray
    , moveSBMArray
    , cloneSBMArray
    , shrinkSBMArray
    , resizeSBMArray
    , resizeRawSBMArray
    , freezeSBMArray
    , freezeCopySBMArray
    -- * Unboxed Array
    -- ** Immutable
    , UArray(..)
    , isSameUArray
    , isPinnedUArray
    , sizeOfUArray
    , indexUArray
    , copyUArray
    , thawUArray
    , toListUArray
    , fromListUArray
    , fromListUArrayN
    , fromBaseUArray
    , toBaseUArray
    -- ** Mutable
    , UMArray(..)
    , isSameUMArray
    , isPinnedUMArray
    , getSizeOfUMArray
    , readUMArray
    , writeUMArray
    , newUMArray
    , newRawUMArray
    , makeUMArray

    , newPinnedUMArray
    , newRawPinnedUMArray
    , makePinnedUMArray
    , newAlignedPinnedUMArray
    , newRawAlignedPinnedUMArray
    , makeAlignedPinnedUMArray
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
    , eqWith
    , compareWith
    , appendWith
    , concatWith
  ) where

import Control.DeepSeq
import Control.Prim.Exception
import qualified Data.Foldable as F
import Data.Functor.Classes
import qualified Data.List.NonEmpty as NE (toList)
import Data.Prim
import Data.Prim.Class
import Foreign.Prim
import qualified Data.Array.Base as A
import qualified GHC.Arr as A

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
-- pointers to actual elements that are being stored somewhere else on the heap. Therefore
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

-- | @since 0.3.0
instance Foldable BArray where
  null = (== 0) . sizeOfBArray
  {-# INLINE null #-}
  length = coerce . sizeOfBArray
  {-# INLINE length #-}
  foldr = foldrWithFB sizeOfBArray indexBArray
  {-# INLINE foldr #-}

instance Show1 BArray where
#if MIN_VERSION_transformers(0,5,0)
  liftShowsPrec _ = liftShowsPrecArray "BArray"
#else
  showsPrec1 = liftShowsPrecArray "BArray" showList
#endif

instance Show e => Show (BArray e) where
  showsPrec = showsPrec1

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

instance NFData e => NFData (BArray e) where
  rnf = foldrWithFB sizeOfBArray indexBArray deepseq ()
  {-# INLINE rnf #-}

instance Eq e => Eq (BArray e) where
  (==) = eqWith isSameBArray sizeOfBArray indexBArray
  {-# INLINE (==) #-}

instance Ord e => Ord (BArray e) where
  compare = compareWith isSameBArray sizeOfBArray indexBArray
  {-# INLINE compare #-}

instance Eq1 BArray where
#if MIN_VERSION_transformers(0,5,0)
  liftEq = liftEqWith sizeOfBArray indexBArray
  {-# INLINE liftEq #-}
#else
  eq1 = liftEqWith sizeOfBArray indexBArray (==)
  {-# INLINE eq1 #-}
#endif


instance Ord1 BArray where
#if MIN_VERSION_transformers(0,5,0)
  liftCompare = liftCompareWith sizeOfBArray indexBArray
  {-# INLINE liftCompare #-}
#else
  compare1 = liftCompareWith sizeOfBArray indexBArray compare
  {-# INLINE compare1 #-}
#endif


instance Semigroup (BArray e) where
  (<>) = appendWith newRawBMArray copyBArray freezeBMArray sizeOfBArray
  {-# INLINE (<>) #-}
  sconcat xs = concatWith newRawBMArray copyBArray freezeBMArray sizeOfBArray (NE.toList xs)
  {-# INLINE sconcat #-}
  stimes n = cycleWith newRawBMArray copyBArray freezeBMArray sizeOfBArray (fromIntegral n)
  {-# INLINE stimes #-}

instance Monoid (BArray e) where
  mempty = runST $ newRawBMArray 0 >>= freezeBMArray
  {-# NOINLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}
  mconcat = concatWith newRawBMArray copyBArray freezeBMArray sizeOfBArray
  {-# INLINE mconcat #-}

-- | Compare pointers for two immutable arrays and see if they refer to the exact same one.
--
-- @since 0.3.0
isSameBArray :: BArray a -> BArray a -> Bool
isSameBArray a1 a2 = runST (isSameBMArray <$> thawBArray a1 <*> thawBArray a2)
{-# INLINE isSameBArray #-}

-- | /O(1)/ - Get the number of elements in an immutable array
--
-- Documentation for utilized primop: `sizeofArray#`.
--
-- @since 0.3.0
sizeOfBArray :: forall e. BArray e -> Size
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
     forall e.
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
     forall e.
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



-- | /O(sz)/ - Copy a subsection of an immutable array into a subsection of a mutable
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
     forall e m s. MonadPrim s m
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
  -- > dstSize <- getSizeOfBMArray dstMutArray
  -- > dstStartIx < unSize dstSize
  -> Size
  -- ^ /sz/ - Number of elements to copy over
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- > srcStartIx + unSize sz < unSize (sizeOfBArray srcArray)
  --
  -- > dstSize <- getSizeOfBMArray dstMutArray
  -- > dstStartIx + unSize sz < unSize dstSize
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
-- BArray [1,10,3,4,5]
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
     forall e m s. MonadPrim s m
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
-- | /O(sz)/ - Create a new mutable array with size @sz@ and copy that number of elements
-- from source immutable @srcArray@ starting at an offset @startIx@ into the newly created
-- @dstMutArray@. This function can help avoid an issue with referential transparency that
-- is inherent to `thawBArray`.
--
-- [Unsafe] When any of the preconditions for @startIx@ or @sz@ is violated this function
-- can result in a copy of some data that doesn't belong to @srcArray@ or more likely a
-- failure with a segfault. Failure with out of memory is also a possibility when the @sz is
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
-- BArray [2,10,4]
-- >>> a
-- BArray [1,2,3,4,5]
--
-- @since 0.3.0
thawCopyBArray ::
     forall e m s. MonadPrim s m
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
toListBArray :: forall e. BArray e -> [e]
toListBArray ba = build (\ c n -> foldrWithFB sizeOfBArray indexBArray c n ba)
{-# INLINE toListBArray #-}



-- | /O(min(length list, sz))/ - Same as `fromListBArray`, except that it will allocate an
-- array exactly of @n@ size, as such it will not convert any portion of the list that
-- doesn't fit into the newly created array.
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
-- BArray [1,2,3]
-- >>> fromListBArrayN 3 [1 :: Int ..]
-- BArray [1,2,3]
--
-- @since 0.1.0
fromListBArrayN ::
     forall e. HasCallStack
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
fromListBArray :: forall e. [e] -> BArray e
fromListBArray xs = fromListBArrayN (coerce (length xs)) xs
{-# INLINE fromListBArray #-}



-- | /O(1)/ - cast a boxed immutable `A.Array` that is wired with GHC to `BArray` from primal.
--
-- >>> import Data.Array.IArray as IA
-- >>> let arr = IA.listArray (10, 15) [30 .. 35] :: IA.Array Int Integer
-- >>> arr
-- array (10,15) [(10,30),(11,31),(12,32),(13,33),(14,34),(15,35)]
-- >>> fromBaseBArray arr
-- BArray [30,31,32,33,34,35]
--
-- @since 0.3.0
fromBaseBArray :: A.Array ix e -> BArray e
fromBaseBArray (A.Array _ _ _ a#) = BArray a#

-- | /O(1)/ - cast a boxed `BArray` from primal into `A.Array`, which is wired with
-- GHC. Resulting array range starts at 0, like any sane array would.
--
-- >>> let arr = fromListBArray [1, 2, 3 :: Integer]
-- >>> arr
-- BArray [1,2,3]
-- >>> toBaseBArray arr
-- array (0,2) [(0,1),(1,2),(2,3)]
--
-- @since 0.3.0
toBaseBArray :: BArray e -> A.Array Int e
toBaseBArray a@(BArray a#) =
  let Size n = sizeOfBArray a
  in A.Array 0 (max 0 (n - 1)) n a#


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
isSameBMArray :: forall a s. BMArray a s -> BMArray a s -> Bool
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
-- >>> getSizeOfBMArray ma
-- Size {unSize = 1024}
--
-- @since 0.3.0
getSizeOfBMArray ::
     forall e m s. MonadPrim s m
  => BMArray e s
  -> m Size
getSizeOfBMArray (BMArray ma#) = --pure $! Size (I# (sizeofMutableArray# ma#))
  prim $ \s ->
    case getSizeofMutableArray# ma# s of
      (# s', n# #) -> (# s', coerce (I# n#) #)
{-# INLINE getSizeOfBMArray #-}

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
     forall e m s. MonadPrim s m
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
-- BArray [Nothing,Nothing,Just 2,Nothing]
--
-- It is important to note that an element is evaluated prior to being written into a
-- cell, so it will not overwrite the value of an array's cell if it evaluates to an
-- exception:
--
-- >>> import Control.Prim.Exception
-- >>> writeBMArray ma 2 (impureThrow DivideByZero)
-- *** Exception: divide by zero
-- >>> freezeBMArray ma
-- BArray [Nothing,Nothing,Just 2,Nothing]
--
-- However, it is evaluated only to Weak Head Normal Form (WHNF), so it is still possible
-- to write something that eventually evaluates to bottom.
--
-- >>> writeBMArray ma 3 (Just (7 `div` 0 ))
-- >>> freezeBMArray ma
-- BArray [Nothing,Nothing,Just 2,Just *** Exception: divide by zero
-- >>> readBMArray ma 3
-- Just *** Exception: divide by zero
--
-- Either `deepseq` or `writeDeepBMArray` can be used to alleviate that.
--
-- @since 0.3.0
writeBMArray ::
     forall e m s. MonadPrim s m
  => BMArray e s -- ^ /dstMutArray/ - An array to have the element written to
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
writeBMArray ma i !x = writeLazyBMArray ma i x -- TODO: figure out why doctests fail sporadically
--writeBMArray ma i = eval >=> writeLazyBMArray ma i
{-# INLINE writeBMArray #-}

{-
src/Data/Prim/Array.hs:697: failure in expression `freezeBMArray ma'
expected: BArray [Nothing,Nothing,Just 2,Just *** Exception: divide by zero
 but got: BArray [Nothing,Nothing,Just 2,Just 5282521669542503534]
                                              ^
Examples: 180  Tried: 63  Errors: 0  Failures: 1doctests: user error (Language.Haskell.GhciWrapper.close: Interpreter exited with an error (ExitFailure (-6)))
primal> Test suite doctests failed
Test suite failure for package primal-0.3.0.0
    doctests:  exited with: ExitFailure 1
Logs printed to console


Examples: 180  Tried: 26  Errors: 0  Failures: 0doctests: user error (Language.Haskell.GhciWrapper.close: Interpreter exited with an error (ExitFailure (-11)))
primal> Test suite doctests failed
Test suite failure for package primal-0.3.0.0
    doctests:  exited with: ExitFailure 1

https://travis-ci.com/github/lehins/primal/jobs/407895714
[34/180] src/Data/Prim/Array.hs:699: failure in expression `readBMArray ma 3'
expected: Just *** Exception: divide by zero
 but got: Just 140663761379224
               ^
-}

-- | /O(1)/ - Same as `writeBMArray` but allows to write a thunk into an array instead of an
-- evaluated element. Careful with memory leaks and thunks that evaluate to exceptions.
--
-- Documentation for utilized primop: `writeArray#`.
--
-- [Unsafe] Same reasons as `writeBMArray`
--
-- @since 0.3.0
writeLazyBMArray ::
     forall e m s. MonadPrim s m
  => BMArray e s
  -> Int
  -> e
  -> m ()
writeLazyBMArray (BMArray ma#) (I# i#) a = prim_ (writeArray# ma# i# a)
{-# INLINE writeLazyBMArray #-}


-- | /O(1)/ - Same as `writeBMArray`, except it ensures that the value being written is
-- fully evaluated, i.e. to Normal Form (NF).
--
-- [Unsafe] Same reasons as `writeBMArray`
--
-- @since 0.3.0
writeDeepBMArray ::
     forall e m s. (MonadPrim s m, NFData e)
  => BMArray e s
  -> Int
  -> e
  -> m ()
writeDeepBMArray ma i !x =
  case rnf x of
    () -> writeLazyBMArray ma i x
{-# INLINE writeDeepBMArray #-}



-- | Create a mutable boxed array where each element is set to the supplied initial value
-- @elt@, which is evaluated before array allocation happens. See `newLazyBMArray` for
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
     forall e m s. MonadPrim s m
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
newLazyBMArray ::
     forall e m s. MonadPrim s m
  => Size
  -> e
  -> m (BMArray e s)
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
-- exception when evaluated
--
-- [Unsafe] Same reasons as `newBMArray`
--
-- ==== __Examples__
--
-- >>> import Data.Prim
-- >>> let xs = "Hello Haskell"
-- >>> ma <- newRawBMArray (Size (length xs)) :: IO (BMArray Char RW)
-- >>> mapM_ (\(i, x) -> writeBMArray ma i x) (zip [0..] xs)
-- >>> freezeBMArray ma
-- BArray "Hello Haskell"
--
-- @since 0.3.0
newRawBMArray ::
     forall e m s. (HasCallStack, MonadPrim s m)
  => Size
  -> m (BMArray e s)
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
makeBMArray ::
     forall e m s. MonadPrim s m
  => Size
  -> (Int -> m e)
  -> m (BMArray e s)
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
freezeBMArray ::
     forall e m s. MonadPrim s m
  => BMArray e s
  -> m (BArray e)
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
     forall e m s. MonadPrim s m
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
-- failure with a segfault. Failure with out of memory is also a possibility when the @sz is
-- too large.
--
-- @since 0.3.0
cloneBMArray ::
     forall e m s. MonadPrim s m
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



-- | /O(1)/ - Reduce the size of a mutable boxed array.
--
-- Documentation for utilized primop: `shrinkMutableArray#`.
--
-- [Unsafe] - Violation of preconditions for @sz@ leads to undefined behavior
--
-- 0.3.0
shrinkBMArray ::
     forall e m s. MonadPrim s m
  => BMArray e s -- ^ /mutArray/ - Mutable unboxed array to be shrunk
  -> Size
  -- ^ /sz/ - New size for the array in number of elements
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- > curSize <- getSizeOfBMArray mutArray
  -- > sz <= curSize
  -> m ()
shrinkBMArray (BMArray ma#) (Size (I# sz#)) =
  prim_ (shrinkMutableArray# ma# sz#)
{-# INLINE shrinkBMArray #-}


-- | /O(1)/ - Either grow or shrink the size of a mutable unboxed array. Shrinking happens
-- in-place without new array creation and data copy, while growing the array is
-- implemented with creating new array and copy of the data over from the source array
-- @srcMutArray@. This has a consequence that produced array @dstMutArray@ might refer to
-- the same @srcMutArray@ or to a totally new array, which can be checked with
-- `isSameBMArray`.
--
-- Documentation on the utilized primop: `resizeMutableArray#`.
--
-- [Unsafe] - Same reasons as in `newRawBMArray`.
--
-- 0.3.0
resizeBMArray ::
     forall e m s. MonadPrim s m
  => BMArray e s -- ^ /srcMutArray/ - Mutable boxed array to be shrunk
  -> Size
  -- ^ /sz/ - New size for the array in number of elements
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- Should be below some upper limit that is dictated by the operating system and the total
  -- amount of available memory
  -> e
  -- ^ /elt/ - Element to write into extra space at the end when growing the array.
  -> m (BMArray e s) -- ^ /dstMutArray/ - produces a resized version of /srcMutArray/.
resizeBMArray (BMArray ma#) (Size (I# sz#)) e =
  prim $ \s ->
    case resizeMutableArray# ma# sz# e s of
      (# s', ma'# #) -> (# s', BMArray ma'# #)
{-# INLINE resizeBMArray #-}

-- | /O(1)/ - Same as `resizeBMArray`, except when growing the array empty space at the
-- end is filled with bottom.
--
-- [Partial] - When size @sz@ is larger then the size of @srcMutArray@ then @dstMutArray@
-- will have cells at the end initialized with thunks that throw `UndefinedElement`
-- exception.
--
-- [Unsafe] - Same reasons as in `newBMArray`.
--
-- 0.3.0
resizeRawBMArray ::
     forall e m s. MonadPrim s m
  => BMArray e s -- ^ /srcMutArray/ - Mutable boxed array to be shrunk
  -> Size
  -- ^ /sz/ - New size for the array in number of elements
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- Should be below some upper limit that is dictated by the operating system and the total
  -- amount of available memory
  -> m (BMArray e s) -- ^ /dstMutArray/ - produces a resized version of /srcMutArray/.
resizeRawBMArray ma sz = resizeBMArray ma sz (uninitialized "Data.Prim.Aray" "resizeRawBMArray")
{-# INLINE resizeRawBMArray #-}


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
     forall e m s. MonadPrim s m
  => BMArray e s -- ^ /srcMutArray/ - Source mutable array
  -> Int
  -- ^ /srcStartIx/ - Offset into the source mutable array where copy should start from
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= srcStartIx
  --
  -- > srcSize <- getSizeOfBMArray srcMutArray
  -- > srcStartIx < unSize srcSize
  -> BMArray e s -- ^ /dstMutArray/ - Destination mutable array
  -> Int
  -- ^ /dstStartIx/ - Offset into the destination mutable array where copy should start to
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= dstStartIx
  --
  -- > dstSize <- getSizeOfBMArray dstMutArray
  -- > dstStartIx < unSize dstSize
  -> Size
  -- ^ /sz/ - Number of elements to copy over
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- > srcSize <- getSizeOfBMArray srcMutArray
  -- > srcStartIx + unSize sz < unSize srcSize
  --
  -- > dstSize <- getSizeOfBMArray dstMutArray
  -- > dstStartIx + unSize sz < unSize dstSize
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

-- | Small boxed immutable array
data SBArray e = SBArray (SmallArray# e)


-- | @since 0.3.0
instance Functor SBArray where
  fmap f a =
    runST $
    makeSBMArray
      (sizeOfSBArray a)
      (pure . f . indexSBArray a) >>= freezeSBMArray
  {-# INLINE fmap #-}
  (<$) x a = runST $ newLazySBMArray (sizeOfSBArray a) x >>= freezeSBMArray
  {-# INLINE (<$) #-}

-- | @since 0.3.0
instance Foldable SBArray where
  null = (== 0) . sizeOfSBArray
  {-# INLINE null #-}
  length = coerce . sizeOfSBArray
  {-# INLINE length #-}
  foldr = foldrWithFB sizeOfSBArray indexSBArray
  {-# INLINE foldr #-}

instance Show1 SBArray where
#if MIN_VERSION_transformers(0,5,0)
  liftShowsPrec _ = liftShowsPrecArray "SBArray"
#else
  showsPrec1 = liftShowsPrecArray "SBArray" showList
#endif

instance Show e => Show (SBArray e) where
  showsPrec = showsPrec1

instance IsList (SBArray e) where
  type Item (SBArray e) = e
  fromList = fromListSBArray
  {-# INLINE fromList #-}
  fromListN n = fromListSBArrayN (coerce n)
  {-# INLINE fromListN #-}
  toList = toListSBArray
  {-# INLINE toList #-}

instance e ~ Char => IsString (SBArray e) where
  fromString = fromListSBArray
  {-# INLINE fromString #-}

instance NFData e => NFData (SBArray e) where
  rnf = foldrWithFB sizeOfSBArray indexSBArray deepseq ()
  {-# INLINE rnf #-}


instance Eq e => Eq (SBArray e) where
  (==) = eqWith isSameSBArray sizeOfSBArray indexSBArray
  {-# INLINE (==) #-}

instance Ord e => Ord (SBArray e) where
  compare = compareWith isSameSBArray sizeOfSBArray indexSBArray
  {-# INLINE compare #-}

instance Eq1 SBArray where
#if MIN_VERSION_transformers(0,5,0)
  liftEq = liftEqWith sizeOfSBArray indexSBArray
  {-# INLINE liftEq #-}
#else
  eq1 = liftEqWith sizeOfSBArray indexSBArray (==)
  {-# INLINE eq1 #-}
#endif

instance Ord1 SBArray where
#if MIN_VERSION_transformers(0,5,0)
  liftCompare = liftCompareWith sizeOfSBArray indexSBArray
  {-# INLINE liftCompare #-}
#else
  compare1 = liftCompareWith sizeOfSBArray indexSBArray compare
  {-# INLINE compare1 #-}
#endif


instance Semigroup (SBArray e) where
  (<>) = appendWith newRawSBMArray copySBArray freezeSBMArray sizeOfSBArray
  {-# INLINE (<>) #-}
  sconcat xs = concatWith newRawSBMArray copySBArray freezeSBMArray sizeOfSBArray (NE.toList xs)
  {-# INLINE sconcat #-}
  stimes n = cycleWith newRawSBMArray copySBArray freezeSBMArray sizeOfSBArray (fromIntegral n)
  {-# INLINE stimes #-}

instance Monoid (SBArray e) where
  mempty = runST $ newRawSBMArray 0 >>= freezeSBMArray
  {-# NOINLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}
  mconcat = concatWith newRawSBMArray copySBArray freezeSBMArray sizeOfSBArray
  {-# INLINE mconcat #-}


-- | Compare pointers for two immutable arrays and see if they refer to the exact same one.
--
-- @since 0.3.0
isSameSBArray :: SBArray a -> SBArray a -> Bool
isSameSBArray a1 a2 = runST (isSameSBMArray <$> thawSBArray a1 <*> thawSBArray a2)
{-# INLINE isSameSBArray #-}

-- | /O(1)/ - Get the number of elements in an immutable array
--
-- Documentation for utilized primop: `sizeofSmallArray#`.
--
-- @since 0.3.0
sizeOfSBArray :: forall e. SBArray e -> Size
sizeOfSBArray (SBArray a#) = Size (I# (sizeofSmallArray# a#))
{-# INLINE sizeOfSBArray #-}


-- | /O(1)/ - Index an element in the immutable small boxed array.
--
-- Documentation for utilized primop: `indexSmallArray#`.
--
-- [Unsafe] Bounds are not checked. When a precondition for @ix@ argument is violated the
-- result is either unpredictable output or failure with a segfault.
--
-- ==== __Examples__
--
-- >>> import Data.Prim.Array
-- >>> let a = fromListSBArray [[0 .. i] | i <- [0 .. 10 :: Int]]
-- >>> indexSBArray a 1
-- [0,1]
-- >>> indexSBArray a 5
-- [0,1,2,3,4,5]
--
-- @since 0.3.0
indexSBArray ::
     forall e.
     SBArray e
  -- ^ /array/ - Array where to lookup an element from
  -> Int
  -- ^ /ix/ - Position of the element within the @array@
  --
  -- /__Precoditions:__/
  --
  -- > 0 <= ix
  --
  -- > ix < unSize (sizeOfSBArray array)
  -> e
indexSBArray (SBArray a#) (I# i#) =
  case indexSmallArray# a# i# of
    (# x #) -> x
{-# INLINE indexSBArray #-}



-- | /O(sz)/ - Make an exact copy of a subsection of a pure immutable array.
--
-- [Unsafe] When any of the preconditions for @startIx@ or @sz@ is violated this function
-- can result in a copy of some data that doesn't belong to @srcArray@ or more likely a
-- failure with a segfault. Failure with out of memory is also a possibility when the @sz is
-- too large.
--
-- Documentation for utilized primop: `cloneSmallArray#`.
--
-- ====__Examples__
--
-- >>> let a = fromListSBArray ['a'..'z']
-- >>> a
-- SBArray "abcdefghijklmnopqrstuvwxyz"
-- >>> cloneSBArray a 23 3
-- SBArray "xyz"
--
-- @since 0.3.0
cloneSBArray ::
     forall e.
     SBArray e
  -- ^ /srcArray/ - Immutable source array
  -> Int
  -- ^ /startIx/ - Location within @srcArray@ where the copy of elements should start from
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= startIx
  --
  -- > startIx < unSize (sizeOfSBArray srcArray)
  -> Size
  -- ^ /sz/ - Size of the returned immutable array. Also this is the number of elements that
  -- will be copied over into the destionation array starting at the beginning.
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- > startIx + unSize sz < unSize (sizeOfSBArray srcArray)
  --
  -- Should be less then the actual available memory
  -> SBArray e
cloneSBArray (SBArray a#) (I# i#) (Size (I# n#)) = SBArray (cloneSmallArray# a# i# n#)
{-# INLINE cloneSBArray #-}



-- | /O(1)/ - Reduce the size of a mutable small boxed array.
--
-- Documentation for utilized primop: `shrinkSmallMutableArray#`.
--
-- [Unsafe] - Violation of preconditions for @sz@ leads to undefined behavior
--
-- 0.3.0
shrinkSBMArray ::
     forall e m s. MonadPrim s m
  => SBMArray e s -- ^ /mutArray/ - Mutable unboxed array to be shrunk
  -> Size
  -- ^ /sz/ - New size for the array in number of elements
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- > curSize <- getSizeOfSBMArray mutArray
  -- > sz <= curSize
  -> m ()
shrinkSBMArray (SBMArray ma#) (Size (I# sz#)) =
  prim_ (shrinkSmallMutableArray# ma# sz#)
{-# INLINE shrinkSBMArray #-}


-- | /O(1)/ - Either grow or shrink the size of a mutable unboxed array. Shrinking happens
-- in-place without new array creation and data copy, while growing the array is
-- implemented with creating new array and copy of the data over from the source array
-- @srcMutArray@. This has a consequence that produced array @dstMutArray@ might refer to
-- the same @srcMutArray@ or to a totally new array, which can be checked with
-- `isSameSBMArray`.
--
-- Documentation on the utilized primop: `resizeSmallMutableArray#`.
--
-- [Unsafe] - Same reasons as in `newRawSBMArray`.
--
-- 0.3.0
resizeSBMArray ::
     forall e m s. MonadPrim s m
  => SBMArray e s -- ^ /srcMutArray/ - Mutable boxed array to be shrunk
  -> Size
  -- ^ /sz/ - New size for the array in number of elements
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- Should be below some upper limit that is dictated by the operating system and the total
  -- amount of available memory
  -> e
  -- ^ /elt/ - Element to write into extra space at the end when growing the array.
  -> m (SBMArray e s) -- ^ /dstMutArray/ - produces a resized version of /srcMutArray/.
resizeSBMArray (SBMArray ma#) (Size (I# sz#)) e =
  prim $ \s ->
    case resizeSmallMutableArray# ma# sz# e s of
      (# s', ma'# #) -> (# s', SBMArray ma'# #)
{-# INLINE resizeSBMArray #-}

-- | /O(1)/ - Same as `resizeSBMArray`, except when growing the array empty space at the
-- end is filled with bottom.
--
-- [Partial] - When size @sz@ is larger then the size of @srcMutArray@ then @dstMutArray@
-- will have cells at the end initialized with thunks that throw `UndefinedElement`
-- exception.
--
-- [Unsafe] - Same reasons as in `newSBMArray`.
--
-- 0.3.0
resizeRawSBMArray ::
     forall e m s. MonadPrim s m
  => SBMArray e s -- ^ /srcMutArray/ - Mutable boxed array to be shrunk
  -> Size
  -- ^ /sz/ - New size for the array in number of elements
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- Should be below some upper limit that is dictated by the operating system and the total
  -- amount of available memory
  -> m (SBMArray e s) -- ^ /dstMutArray/ - produces a resized version of /srcMutArray/.
resizeRawSBMArray ma sz = resizeSBMArray ma sz (uninitialized "Data.Prim.Aray" "resizeRawSBMArray")
{-# INLINE resizeRawSBMArray #-}


-- | /O(sz)/ - Copy a subsection of an immutable array into a subsection of a mutable
-- array. Source and destination arrays must not be the same array in different states.
--
-- Documentation for utilized primop: `copySmallArray#`.
--
-- [Unsafe] When any of the preconditions for @srcStartIx@, @dstStartIx@ or @sz@ is violated
-- this function can result in a copy of some data that doesn't belong to @srcArray@ or more
-- likely a failure with a segfault.
--
-- @since 0.3.0
copySBArray ::
     forall e m s. MonadPrim s m
  => SBArray e
  -- ^ /srcArray/ - Source immutable array
  --
  -- /__Precondition:__/
  --
  -- > srcMutArray <- thawSBArray srcArray
  -- > srcMutArray /= dstMutArray
  -> Int
  -- ^ /srcStartIx/ - Offset into the source immutable array where copy should start from
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= srcStartIx
  --
  -- > srcStartIx < unSize (sizeOfSBArray srcArray)
  -> SBMArray e s -- ^ /dstMutArray/ - Destination mutable array
  -> Int
  -- ^ /dstStartIx/ - Offset into the destination mutable array where the copy should start
  -- at
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= dstStartIx
  --
  -- > dstSize <- getSizeOfSBMArray dstMutArray
  -- > dstStartIx < unSize dstSize
  -> Size
  -- ^ /sz/ - Number of elements to copy over
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- > srcStartIx + unSize sz < unSize (sizeOfSBArray srcArray)
  --
  -- > dstSize <- getSizeOfSBMArray dstMutArray
  -- > dstStartIx + unSize sz < unSize dstSize
  -> m ()
copySBArray (SBArray src#) (I# srcOff#) (SBMArray dst#) (I# dstOff#) (Size (I# n#)) =
  prim_ (copySmallArray# src# srcOff# dst# dstOff# n#)
{-# INLINE copySBArray #-}


-- | /O(1)/ - Convert a pure immutable boxed array into a mutable boxed array. Use
-- `freezeSBMArray` in order to go in the opposite direction.
--
-- Documentation for utilized primop: `unsafeThawSmallArray#`.
--
-- [Unsafe] This function makes it possible to break referential transparency, because any
-- subsequent destructive operation to the mutable boxed array will also be reflected in
-- the source immutable array as well. See `thawCopySBArray` that avoids this problem with
-- a fresh allocation and data copy.
--
-- ====__Examples__
--
-- >>> ma <- thawSBArray $ fromListSBArray [1 .. 5 :: Integer]
-- >>> writeSBMArray ma 1 10
-- >>> freezeSBMArray ma
-- SBArray [1,10,3,4,5]
--
-- Be careful not to retain a reference to the pure immutable source array after the
-- thawed version gets mutated.
--
-- >>> let a = fromListSBArray [1 .. 5 :: Integer]
-- >>> ma' <- thawSBArray a
-- >>> writeSBMArray ma' 0 100000
-- >>> a
-- SBArray [100000,2,3,4,5]
--
-- @since 0.3.0
thawSBArray ::
     forall e m s. MonadPrim s m
  => SBArray e
  -- ^ /array/ - Source immutable array that will be thawed
  -> m (SBMArray e s)
thawSBArray (SBArray a#) = prim $ \s ->
  case unsafeThawSmallArray# a# s of
    (# s', ma# #) -> (# s', SBMArray ma# #)
{-# INLINE thawSBArray #-}


-- | /O(sz)/ - Create a new mutable array with size @sz@ and copy that number of elements
-- from source immutable @srcArray@ starting at an offset @startIx@ into the newly created
-- @dstMutArray@. This function can help avoid an issue with referential transparency that
-- is inherent to `thawSBArray`.
--
-- [Unsafe] When any of the preconditions for @startIx@ or @sz@ is violated this function
-- can result in a copy of some data that doesn't belong to @srcArray@ or more likely a
-- failure with a segfault. Failure with out of memory is also a possibility when the @sz is
-- too large.
--
-- Documentation for utilized primop: `thawSmallArray#`.
--
-- ====__Examples__
--
-- >>> let a = fromListSBArray [1 .. 5 :: Int]
-- >>> ma <- thawCopySBArray a 1 3
-- >>> writeSBMArray ma 1 10
-- >>> freezeSBMArray ma
-- SBArray [2,10,4]
-- >>> a
-- SBArray [1,2,3,4,5]
--
-- @since 0.3.0
thawCopySBArray ::
     forall e m s. MonadPrim s m
  => SBArray e
  -- ^ /srcArray/ - Immutable source array
  -> Int
  -- ^ /startIx/ - Location within @srcArray@ where the copy of elements should start from
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= startIx
  --
  -- > startIx < unSize (sizeOfSBArray srcArray)
  -> Size
  -- ^ /sz/ - Size of the returned mutable array. Also this is the number of elements that
  -- will be copied over into the destionation array starting at the beginning.
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- > startIx + unSize sz < unSize (sizeOfSBArray srcArray)
  --
  -- Should be less then the actual available memory
  -> m (SBMArray e s)
  -- ^ /dstMutArray/ - Newly created destination mutable boxed array
thawCopySBArray (SBArray a#) (I# i#) (Size (I# n#)) = prim $ \s ->
  case thawSmallArray# a# i# n# s of
    (# s', ma# #) -> (# s', SBMArray ma# #)
{-# INLINE thawCopySBArray #-}



-- | Convert a pure boxed array into a list. It should work fine with GHC built-in list
-- fusion.
--
-- @since 0.1.0
toListSBArray :: forall e. SBArray e -> [e]
toListSBArray ba = build (\ c n -> foldrWithFB sizeOfSBArray indexSBArray c n ba)
{-# INLINE toListSBArray #-}



-- | /O(min(length list, sz))/ - Same as `fromListSBArray`, except that it will allocate
-- an array exactly of @n@ size, as such it will not convert any portion of the list that
-- doesn't fit into the newly created array.
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
-- >>> fromListSBArrayN 3 [1 :: Int, 2, 3]
-- SBArray [1,2,3]
-- >>> fromListSBArrayN 3 [1 :: Int ..]
-- SBArray [1,2,3]
--
-- @since 0.1.0
fromListSBArrayN ::
     forall e. HasCallStack
  => Size -- ^ /sz/ - Expected number of elements in the @list@
  -> [e] -- ^ /list/ - A list to bew loaded into the array
  -> SBArray e
fromListSBArrayN sz xs =
  runST $ fromListMutWith newRawSBMArray writeSBMArray sz xs >>= freezeSBMArray
{-# INLINE fromListSBArrayN #-}


-- | /O(length list)/ - Convert a list into an immutable boxed array. It is more efficient to use
-- `fromListSBArrayN` when the number of elements is known ahead of time. The reason for this
-- is that it is necessary to iterate the whole list twice: once to count how many elements
-- there is in order to create large enough array that can fit them; and the second time to
-- load the actual elements. Naturally, infinite lists will grind the program to a halt.
--
-- ====__Example__
--
-- >>> fromListSBArray "Hello Haskell"
-- SBArray "Hello Haskell"
--
-- @since 0.3.0
fromListSBArray :: forall e. [e] -> SBArray e
fromListSBArray xs = fromListSBArrayN (coerce (length xs)) xs
{-# INLINE fromListSBArray #-}


-- Mutable Small Boxed Array --
-------------------------------

-- | Small boxed mutable array
data SBMArray e s = SBMArray (SmallMutableArray# s e)

-- | Check if both of the arrays refer to the exact same one. None of the elements are
-- evaluated.
instance Eq (SBMArray e s) where
  (==) = isSameSBMArray
  {-# INLINE (==) #-}


-- | Compare pointers for two mutable arrays and see if they refer to the exact same one.
--
-- Documentation for utilized primop: `sameSmallMutableArray#`.
--
-- @since 0.3.0
isSameSBMArray :: forall a s. SBMArray a s -> SBMArray a s -> Bool
isSameSBMArray (SBMArray ma1#) (SBMArray ma2#) =
  isTrue# (sameSmallMutableArray# ma1# ma2#)
{-# INLINE isSameSBMArray #-}


-- | /O(1)/ - Get the size of a mutable boxed array
--
-- Documentation for utilized primop: `getSizeofSmallMutableArray#` for ghc-8.10 and newer
-- and fallback to `sizeofMutableArray#` for older versions.
--
-- ====__Example__
--
-- >>> ma <- newSBMArray 1024 "Element of each cell"
-- >>> getSizeOfSBMArray ma
-- Size {unSize = 1024}
--
-- @since 0.3.0
getSizeOfSBMArray :: forall e m s. MonadPrim s m => SBMArray e s -> m Size
getSizeOfSBMArray (SBMArray ma#) =
  prim $ \s ->
    case getSizeofSmallMutableArray# ma# s of
      (# s', i# #) -> (# s', coerce (I# i#) #)
{-# INLINE getSizeOfSBMArray #-}

-- | /O(1)/ - Read an element from a mutable small boxed array at the supplied index.
--
-- Documentation for utilized primop: `readSmallArray#`.
--
-- [Unsafe] Violation of @ix@ preconditions can result in undefined behavior or a failure
-- with a segfault
--
-- ==== __Example__
--
-- >>> ma <- makeSBMArray 10 (pure . ("Element ix: " ++) . show)
-- >>> readSBMArray ma 5
-- "Element ix: 5"
--
-- @since 0.1.0
readSBMArray ::
     forall e m s. MonadPrim s m
  => SBMArray e s -- ^ /srcMutArray/ - Array to read an element from
  -> Int
  -- ^ /ix/ - Index that refers to an element we need within the the @srcMutArray@
  --
  -- /__Precoditions:__/
  --
  -- > 0 <= ix
  --
  -- > ix < unSize (sizeOfMSBArray srcMutArray)
  -> m e
readSBMArray (SBMArray ma#) (I# i#) = prim (readSmallArray# ma# i#)
{-# INLINE readSBMArray #-}



-- | /O(1)/ - Write an element @elt@ into the mutable small boxed array @dstMutArray@ at
-- the supplied index @ix@. The actual element will be evaluated to WHNF prior to
-- mutation.
--
-- [Unsafe] Violation of @ix@ preconditions can result in heap corruption or a failure
-- with a segfault
--
-- ==== __Examples__
--
-- >>> ma <- newSBMArray 4 (Nothing :: Maybe Integer)
-- >>> writeSBMArray ma 2 (Just 2)
-- >>> freezeSBMArray ma
-- SBArray [Nothing,Nothing,Just 2,Nothing]
--
-- It is important to note that an element is evaluated prior to being written into a
-- cell, so it will not overwrite the value of an array's cell if it evaluates to an
-- exception:
--
-- >>> import Control.Prim.Exception
-- >>> writeSBMArray ma 2 (impureThrow DivideByZero)
-- *** Exception: divide by zero
-- >>> freezeSBMArray ma
-- SBArray [Nothing,Nothing,Just 2,Nothing]
--
-- However, it is evaluated only to Weak Head Normal Form (WHNF), so it is still possible
-- to write something that eventually evaluates to bottom.
--
-- >>> writeSBMArray ma 3 (Just (7 `div` 0 ))
-- >>> freezeSBMArray ma
-- SBArray [Nothing,Nothing,Just 2,Just *** Exception: divide by zero
--
-- Either `deepseq` or `writeDeepSBMArray` can be used to alleviate that.
--
-- @since 0.3.0
writeSBMArray ::
     forall e m s. MonadPrim s m
  => SBMArray e s -- ^ /dstMutArray/ - An array to have the element written to
  -> Int
  -- ^ /ix/ - Index within the the @dstMutArray@ that a refernce to the supplied element
  -- @elt@ will be written to.
  --
  -- /__Precoditions:__/
  --
  -- > 0 <= ix
  --
  -- > ix < unSize (sizeOfMSBArray srcArray)
  -> e
  -- ^ /elt/ - Element to be written into @dstMutArray@
  -> m ()
writeSBMArray ma i !x = writeLazySBMArray ma i x
{-# INLINE writeSBMArray #-}


-- | /O(1)/ - Same as `writeSBMArray` but allows to write a thunk into an array instead of an
-- evaluated element. Careful with memory leaks and thunks that evaluate to exceptions.
--
-- Documentation for utilized primop: `writeSmallArray#`.
--
-- [Unsafe] Same reasons as `writeSBMArray`
--
-- @since 0.3.0
writeLazySBMArray ::
     forall e m s. MonadPrim s m
  => SBMArray e s
  -> Int
  -> e
  -> m ()
writeLazySBMArray (SBMArray ma#) (I# i#) a = prim_ (writeSmallArray# ma# i# a)
{-# INLINE writeLazySBMArray #-}


-- | /O(1)/ - Same as `writeSBMArray`, except it ensures that the value being written is
-- fully evaluated, i.e. to Normal Form (NF).
--
-- [Unsafe] Same reasons as `writeSBMArray`
--
-- @since 0.3.0
writeDeepSBMArray ::
     forall e m s. (MonadPrim s m, NFData e)
  => SBMArray e s
  -> Int
  -> e
  -> m ()
writeDeepSBMArray ma i !x =
  case rnf x of
    () -> writeLazySBMArray ma i x
{-# INLINE writeDeepSBMArray #-}



-- | Create a mutable boxed array where each element is set to the supplied initial value
-- @elt@, which is evaluated before array allocation happens. See `newLazySBMArray` for
-- an ability to initialize with a thunk.
--
-- [Unsafe size] Violation of precondition for the @sz@ argument can result in the current
-- thread being killed with `HeapOverflow` asynchronous exception or death of the whole
-- process with some unchecked exception from RTS.
--
-- ====__Examples__
--
-- >>> newSBMArray 10 'A' >>= freezeSBMArray
-- SBArray "AAAAAAAAAA"
--
-- @since 0.3.0
newSBMArray ::
     forall e m s. MonadPrim s m
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
  -> m (SBMArray e s)
newSBMArray sz x = x `seq` newLazySBMArray sz x
{-# INLINE newSBMArray #-}

-- | Same as `newSBMArray`, except initial element is allowed to be a thunk.
--
-- Documentation for utilized primop: `newSmallArray#`.
--
-- [Unsafe] Same reasons as `newSBMArray`
--
-- @since 0.3.0
newLazySBMArray ::
     forall e m s. MonadPrim s m
  => Size
  -> e
  -> m (SBMArray e s)
newLazySBMArray (Size (I# n#)) a =
  prim $ \s ->
    case newSmallArray# n# a s of
      (# s', ma# #) -> (# s', SBMArray ma# #)
{-# INLINE newLazySBMArray #-}




-- | Create new mutable array, where each element is initilized to a thunk that throws an
-- error when evaluated. This is useful when there is a plan to later iterate over the whole
-- array and write values into each cell in some index aware fashion. Consider `makeSBMArray`
-- as an alternative.
--
-- [Partial] All array cells are initialized with thunks that throw `UndefinedElement`
-- exception.
--
-- [Unsafe] Same reasons as `newSBMArray`
--
-- ==== __Examples__
--
-- >>> import Data.Prim
-- >>> let xs = "Hello Haskell"
-- >>> ma <- newRawSBMArray (Size (length xs)) :: IO (SBMArray Char RW)
-- >>> mapM_ (\(i, x) -> writeSBMArray ma i x) (zip [0..] xs)
-- >>> freezeSBMArray ma
-- SBArray "Hello Haskell"
--
-- @since 0.3.0
newRawSBMArray ::
     forall e m s. (HasCallStack, MonadPrim s m)
  => Size
  -> m (SBMArray e s)
newRawSBMArray sz = newLazySBMArray sz (uninitialized "Data.Prim.Aray" "newRawSBMArray")
{-# INLINE newRawSBMArray #-}



-- | Create new mutable boxed array of the supplied size and fill it with a monadic action
-- that is applied to indices of each array cell.
--
-- [Unsafe] Same reasons as `newSBMArray`
--
-- ====__Examples__
--
-- >>> ma <- makeSBMArray 5 $ \i -> (toEnum (i + 97) :: Char) <$ putStrLn ("Handling index: " ++ show i)
-- Handling index: 0
-- Handling index: 1
-- Handling index: 2
-- Handling index: 3
-- Handling index: 4
-- >>> freezeSBMArray ma
-- SBArray "abcde"
--
-- @since 0.3.0
makeSBMArray ::
     forall e m s. MonadPrim s m
  => Size
  -> (Int -> m e)
  -> m (SBMArray e s)
makeSBMArray = makeMutWith newRawSBMArray writeSBMArray
{-# INLINE makeSBMArray #-}


-- | /O(1)/ - Convert a mutable boxed array into an immutable one. Use `thawSBArray` in order
-- to go in the opposite direction.
--
-- Documentation for utilized primop: `unsafeFreezeSmallArray#`.
--
-- [Unsafe] This function makes it possible to break referential transparency, because any
-- subsequent destructive operation to the source mutable boxed array will also be reflected
-- in the resulting immutable array. See `freezeCopySBMArray` that avoids this problem with
-- fresh allocation.
--
-- @since 0.3.0
freezeSBMArray ::
     forall e m s. MonadPrim s m
  => SBMArray e s
  -> m (SBArray e)
freezeSBMArray (SBMArray ma#) = prim $ \s ->
  case unsafeFreezeSmallArray# ma# s of
    (# s', a# #) -> (# s', SBArray a# #)
{-# INLINE freezeSBMArray #-}



-- | /O(sz)/ - Similar to `freezeSBMArray`, except it creates a new array with the copy of a
-- subsection of a mutable array before converting it into an immutable.
--
-- Documentation for utilized primop: `freezeSmallArray#`.
--
-- [Unsafe] When any of the preconditions for @startIx@ or @sz@ is violated this function
-- can result in a copy of some data that doesn't belong to @srcArray@ or more likely a
-- failure with a segfault or out of memory exception.
--
-- @since 0.3.0
freezeCopySBMArray ::
     forall e m s. MonadPrim s m
  => SBMArray e s
  -- ^ /srcArray/ - Source mutable array
  -> Int
  -- ^ /startIx/ - Location within @srcArray@ where the copy of elements should start from
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= startIx
  --
  -- > startIx < unSize (sizeOfSBArray srcArray)
  -> Size
  -- ^ /sz/ - Size of the returned immutable array. Also this is the number of elements that
  -- will be copied over into the destionation array starting at the beginning.
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- > startIx + unSize sz < unSize (sizeOfSBArray srcArray)
  --
  -- Should be less then actual available memory
  -> m (SBArray e)
freezeCopySBMArray (SBMArray ma#) (I# i#) (Size (I# n#)) = prim $ \s ->
  case freezeSmallArray# ma# i# n# s of
    (# s', a# #) -> (# s', SBArray a# #)
{-# INLINE freezeCopySBMArray #-}

-- | /O(sz)/ - Allocate a new small boxed mutable array of size @sz@ and copy that number
-- of the elements over from the @srcArray@ starting at index @ix@. Similar to
-- `cloneSBArray`, except that it works on mutable arrays.
--
-- Documentation for utilized primop: `cloneSmallMutableArray#`.
--
-- [Unsafe] When any of the preconditions for @startIx@ or @sz@ is violated this function
-- can result in a copy of some data that doesn't belong to @srcArray@ or more likely a
-- failure with a segfault. Failure with out of memory is also a possibility when the @sz is
-- too large.
--
-- @since 0.3.0
cloneSBMArray ::
     forall e m s. MonadPrim s m
  => SBMArray e s
  -- ^ /srcArray/ - Source mutable array
  -> Int
  -- ^ /startIx/ - Location within @srcArray@ where the copy of elements should start from
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= startIx
  --
  -- > startIx < unSize (sizeOfSBArray srcArray)
  -> Size
  -- ^ /sz/ - Size of the returned mutable array. Also this is the number of elements that
  -- will be copied over into the destionation array starting at the beginning.
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- > startIx + unSize sz < unSize (sizeOfSBArray srcArray)
  --
  -- Should be less then actual available memory
  -> m (SBMArray e s)
cloneSBMArray (SBMArray ma#) (I# i#) (Size (I# n#)) =
  prim $ \s ->
    case cloneSmallMutableArray# ma# i# n# s of
      (# s', ma'# #) -> (# s', SBMArray ma'# #)
{-# INLINE cloneSBMArray #-}


-- | /O(sz)/ - Copy a subsection of a mutable array into a subsection of another or the same
-- mutable array. Therefore, unlike `copySBArray`, memory ia allowed to overlap between source
-- and destination.
--
-- Documentation for utilized primop: `copySmallMutableArray#`.
--
-- [Unsafe] When any of the preconditions for @srcStartIx@, @dstStartIx@ or @sz@ is violated
-- this function can result in a copy of some data that doesn't belong to @srcArray@ or more
-- likely a failure with a segfault.
--
-- @since 0.3.0
moveSBMArray ::
     forall e m s. MonadPrim s m
  => SBMArray e s -- ^ /srcMutArray/ - Source mutable array
  -> Int
  -- ^ /srcStartIx/ - Offset into the source mutable array where copy should start from
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= srcStartIx
  --
  -- > srcSize <- getSizeOfSBMArray srcMutArray
  -- > srcStartIx < unSize srcSize
  -> SBMArray e s -- ^ /dstMutArray/ - Destination mutable array
  -> Int
  -- ^ /dstStartIx/ - Offset into the destination mutable array where copy should start to
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= dstStartIx
  --
  -- > dstSize <- getSizeOfSBMArray dstMutArray
  -- > dstStartIx < unSize dstSize
  -> Size
  -- ^ /sz/ - Number of elements to copy over
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- > srcSize <- getSizeOfSBMArray srcMutArray
  -- > srcStartIx + unSize sz < unSize srcSize
  --
  -- > dstSize <- getSizeOfSBMArray dstMutArray
  -- > dstStartIx + unSize sz < unSize dstSize
  --
  -> m ()
moveSBMArray (SBMArray src#) (I# srcOff#) (SBMArray dst#) (I# dstOff#) (Size (I# n#)) =
  prim_ (copySmallMutableArray# src# srcOff# dst# dstOff# n#)
{-# INLINE moveSBMArray #-}



-------------------
-- Unboxed Array --
-- ============= --


-- Immutable Unboxed Array --
-----------------------------

data UArray e = UArray ByteArray#
type role UArray nominal

instance (Prim e, Show e) => Show (UArray e) where
  showsPrec n arr
    | n > 1 = ('(' :) . inner . (')' :)
    | otherwise = inner
    where
      inner = ("UArray " ++) . shows (toList arr)

instance Prim e => IsList (UArray e) where
  type Item (UArray e) = e
  fromList = fromListUArray
  {-# INLINE fromList #-}
  fromListN n = fromListUArrayN (coerce n)
  {-# INLINE fromListN #-}
  toList = toListUArray
  {-# INLINE toList #-}

instance e ~ Char => IsString (UArray e) where
  fromString = fromListUArray
  {-# INLINE fromString #-}

-- | /O(1)/ - `UArray` is always in NF
instance NFData (UArray e) where
  rnf (UArray _) = ()
  {-# INLINE rnf #-}

instance (Prim e, Eq e) => Eq (UArray e) where
  (==) = eqWith isSameUArray sizeOfUArray indexUArray
  {-# INLINE (==) #-}

instance (Prim e, Ord e) => Ord (UArray e) where
  compare = compareWith isSameUArray sizeOfUArray indexUArray
  {-# INLINE compare #-}


instance Prim e => Semigroup (UArray e) where
  (<>) = appendWith newRawUMArray copyUArray freezeUMArray sizeOfUArray
  {-# INLINE (<>) #-}
  sconcat xs = concatWith newRawUMArray copyUArray freezeUMArray sizeOfUArray (NE.toList xs)
  {-# INLINE sconcat #-}
  stimes n = cycleWith newRawUMArray copyUArray freezeUMArray sizeOfUArray (fromIntegral n)
  {-# INLINE stimes #-}

instance Prim e => Monoid (UArray e) where
  mempty = runST $ newRawUMArray 0 >>= freezeUMArray
  {-# NOINLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}
  mconcat = concatWith newRawUMArray copyUArray freezeUMArray sizeOfUArray
  {-# INLINE mconcat #-}


-- | /O(1)/ - Compare pointers for two immutable arrays and see if they refer to the exact same one.
--
-- Documentation for utilized primop: `isSameByteArray#`.
--
-- @since 0.3.0
isSameUArray :: forall a b. UArray a -> UArray b -> Bool
isSameUArray (UArray ma1#) (UArray ma2#) = isTrue# (isSameByteArray# ma1# ma2#)
{-# INLINE isSameUArray #-}


-- | /O(1)/ - Check if memory for immutable unboxed array was allocated as pinned.
--
-- Documentation for utilized primop: `isByteArrayPinned#`.
--
-- @since 0.3.0
isPinnedUArray :: forall e. UArray e -> Bool
isPinnedUArray (UArray b#) = isTrue# (isByteArrayPinned# b#)
{-# INLINE isPinnedUArray #-}



-- | /O(1)/ - Get the size of an immutable array in number of elements.
--
-- Documentation for utilized primop: `sizeofByteArray#`.
--
-- @since 0.3.0
sizeOfUArray ::
     forall e. Prim e
  => UArray e
  -> Size
sizeOfUArray (UArray a#) =
  coerce (fromByteCount (coerce (I# (sizeofByteArray# a#))) :: Count e)
{-# INLINE sizeOfUArray #-}


-- | /O(1)/ - Index an element of a pure unboxed array.
--
-- Documentation for utilized primop: `indexByteArray#`.
--
-- [Unsafe] Bounds are not checked. When a precondition for @ix@ argument is violated the
-- result is either unpredictable output or failure with a segfault.
--
-- ==== __Examples__
--
-- >>> let a = fromListUArray ([Left pi, Right 123] :: [Either Double Int])
-- >>> indexUArray a 0
-- Left 3.141592653589793
-- >>> indexUArray a 1
-- Right 123
--
-- @since 0.3.0
indexUArray ::
     forall e. Prim e
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


-- | /O(1)/ - Convert a pure immutable unboxed array into a mutable unboxed array. Use
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
-- UArray [1,10,3,4,5]
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
thawUArray :: forall e m s. MonadPrim s m => UArray e -> m (UMArray e s)
thawUArray (UArray a#) =
  prim $ \s ->
    case unsafeThawByteArray# a# s of
      (# s', ma# #) -> (# s', UMArray ma# #)
{-# INLINE thawUArray #-}



-- | /O(n)/ - Convert a pure boxed array into a list. It should work fine with GHC built-in list
-- fusion.
--
-- @since 0.1.0
toListUArray ::
     forall e. Prim e
  => UArray e
  -> [e]
toListUArray ba = build (\ c n -> foldrWithFB sizeOfUArray indexUArray c n ba)
{-# INLINE toListUArray #-}

-- | /O(min(length list, sz))/ - Same as `fromListUArray`, except it will allocate an array exactly of @n@ size, as
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
-- >>> fromListUArrayN 3 [1 :: Int, 2, 3]
-- UArray [1,2,3]
-- >>> fromListUArrayN 3 [1 :: Int ..]
-- UArray [1,2,3]
--
-- @since 0.1.0
fromListUArrayN ::
     forall e. Prim e
  => Size -- ^ /sz/ - Expected number of elements in the @list@
  -> [e] -- ^ /list/ - A list to bew loaded into the array
  -> UArray e
fromListUArrayN sz xs =
  runST $ fromListMutWith newRawUMArray writeUMArray sz xs >>= freezeUMArray
{-# INLINE fromListUArrayN #-}


-- | /O(length list)/ - Convert a list into an immutable boxed array. It is more efficient to use
-- `fromListUArrayN` when the number of elements is known ahead of time. The reason for this
-- is that it is necessary to iterate the whole list twice: once to count how many elements
-- there is in order to create large enough array that can fit them; and the second time to
-- load the actual elements. Naturally, infinite lists will grind the program to a halt.
--
-- ====__Example__
--
-- >>> fromListUArray "Hello Haskell"
-- UArray "Hello Haskell"
--
-- @since 0.3.0
fromListUArray ::
     forall e. Prim e
  => [e]
  -> UArray e
fromListUArray xs = fromListUArrayN (coerce (length xs)) xs
{-# INLINE fromListUArray #-}

-- | /O(1)/ - cast an unboxed `A.UArray` that is wired with GHC to `UArray` from primal.
--
-- >>> import Data.Array.IArray as IA
-- >>> import Data.Array.Unboxed as UA
-- >>> let uarr = IA.listArray (10, 15) [30 .. 35] :: UA.UArray Int Word
-- >>> uarr
-- array (10,15) [(10,30),(11,31),(12,32),(13,33),(14,34),(15,35)]
-- >>> fromBaseUArray uarr
-- UArray [30,31,32,33,34,35]
--
-- @since 0.3.0
fromBaseUArray :: (Prim e, A.IArray A.UArray e) => A.UArray ix e -> UArray e
fromBaseUArray (A.UArray _ _ _ ba#) = UArray ba#

-- | /O(1)/ - cast an unboxed `UArray` from primal into `A.UArray`, which is wired with
-- GHC. Resulting array range starts at 0, like any sane array would.
--
-- >>> let uarr = fromListUArray [1, 2, 3 :: Int]
-- >>> uarr
-- UArray [1,2,3]
-- >>> toBaseUArray uarr
-- array (0,2) [(0,1),(1,2),(2,3)]
--
-- @since 0.3.0
toBaseUArray :: (Prim e, A.IArray A.UArray e) => UArray e -> A.UArray Int e
toBaseUArray a@(UArray ba#) =
  let Size n = sizeOfUArray a
  in A.UArray 0 (max 0 (n - 1)) n ba#

-- Mutable Unboxed Array --
---------------------------

data UMArray e s = UMArray (MutableByteArray# s)
type role UMArray nominal nominal

-- | Check if both of the arrays refer to the exact same one through poiner equality. None
-- of the elements are evaluated.
instance Eq (UMArray e s) where
  (==) = isSameUMArray
  {-# INLINE (==) #-}

-- | /O(1)/ - `UMArray` is always in NF
instance NFData (UMArray e s) where
  rnf (UMArray _) = ()
  {-# INLINE rnf #-}

-- | /O(1)/ - Compare pointers for two mutable arrays and see if they refer to the exact same one.
--
-- Documentation for utilized primop: `sameMutableByteArray#`.
--
-- @since 0.3.0
isSameUMArray :: forall a b s. UMArray a s -> UMArray b s -> Bool
isSameUMArray (UMArray ma1#) (UMArray ma2#) = isTrue# (sameMutableByteArray# ma1# ma2#)
{-# INLINE isSameUMArray #-}


-- | /O(1)/ - Check if memory for mutable unboxed array was allocated as pinned.
--
-- Documentation for utilized primop: `isMutableByteArrayPinned#`.
--
-- @since 0.3.0
isPinnedUMArray :: forall e s. UMArray e s -> Bool
isPinnedUMArray (UMArray mb#) = isTrue# (isMutableByteArrayPinned# mb#)
{-# INLINE isPinnedUMArray #-}

-- | /O(1)/ - Get the size of a mutable unboxed array
--
-- Documentation for utilized primop: `getSizeofMutableByteArray#`.
--
-- ====__Example__
--
-- >>> ma <- thawUArray $ fromListUArray ['a' .. 'z']
-- >>> getSizeOfUMArray ma
-- Size {unSize = 26}
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
     forall e m s. (Prim e, MonadPrim s m)
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


-- | /O(1)/ - Write an element into an unboxed mutable array at a supplied index.
--
-- Documentation for utilized primop: `writeMutableByteArray#`.
--
-- [Unsafe] Violation of @ix@ preconditions can result in heap corruption or a failure
-- with a segfault
--
-- ==== __Examples__
--
-- >>> import Data.Prim
-- >>> ma <- newRawUMArray 4 :: IO (UMArray (Maybe Int) RW)
-- >>> mapM_ (\i -> writeUMArray ma i Nothing) [0, 1, 3]
-- >>> writeUMArray ma 2 (Just 2)
-- >>> freezeUMArray ma
-- UArray [Nothing,Nothing,Just 2,Nothing]
--
-- @since 0.3.0
writeUMArray ::
     forall e m s. (Prim e, MonadPrim s m)
  => UMArray e s
  -> Int
  -> e
  -> m ()
writeUMArray (UMArray ma#) (I# i#) a = prim_ (writeMutableByteArray# ma# i# a)
{-# INLINE writeUMArray #-}

-- prop> newUMArray sz a === makeUMArray sz (const (pure a))
-- | /O(sz)/ - Allocate new mutable unboxed array. Similar to `newRawUMArray`, except all
-- elements are initialized to the supplied initial value. This is equivalent to
-- @makeUMArray sz (const (pure a))@ but often will be more efficient.
--
-- [Unsafe] When any of preconditions for @sz@ argument is violated the outcome is
-- unpredictable. One possible outcome is termination with `HeapOverflow` async
-- exception.
--
-- ==== __Examples__
--
-- >>> import Data.Prim
-- >>> let xs = "Hello"
-- >>> ma <- newUMArray (Size (length xs) + 8) '!' :: IO (UMArray Char RW)
-- >>> mapM_ (\(i, x) -> writeUMArray ma i x) (zip [0..] xs)
-- >>> freezeUMArray ma
-- UArray "Hello!!!!!!!!"
--
-- @since 0.3.0
newUMArray ::
     forall e m s. (Prim e, MonadPrim s m)
  => Size
  -- ^ /sz/ - Size of the array in number of elements.
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- Susceptible to integer overflow:
  --
  -- > 0 <= toByteCount (Count (unSize n) :: Count e)
  --
  -- Should be below some upper limit that is dictated by the operating system and the total
  -- amount of available memory
  -> e
  -> m (UMArray e s)
newUMArray n e = newRawUMArray n >>= \ma -> ma <$ setUMArray ma 0 n e
{-# INLINE newUMArray #-}


-- | Same `newUMArray`, but allocate memory as pinned. See `newRawPinnedUMArray` for more info.
--
-- [Unsafe] - Same reasons as `newUMArray`.
--
-- @since 0.3.0
newPinnedUMArray ::
     forall e m s. (Prim e, MonadPrim s m)
  => Size
  -> e
  -> m (UMArray e s)
newPinnedUMArray n e = newRawPinnedUMArray n >>= \ma -> ma <$ setUMArray ma 0 n e
{-# INLINE newPinnedUMArray #-}


-- | Same `newUMArray`, but allocate memory as pinned and aligned. See
-- `newRawAlignedPinnedUMArray` for more info.
--
-- [Unsafe] - Same reasons as `newUMArray`.
--
-- @since 0.3.0
newAlignedPinnedUMArray ::
     forall e m s. (Prim e, MonadPrim s m)
  => Size
  -> e
  -> m (UMArray e s)
newAlignedPinnedUMArray n e = newRawAlignedPinnedUMArray n >>= \ma -> ma <$ setUMArray ma 0 n e
{-# INLINE newAlignedPinnedUMArray #-}



-- | Create new mutable unboxed array of the supplied size and fill it with a monadic action
-- that is applied to indices of each array cell.
--
-- [Unsafe] Same reasons as `newUMArray`
--
-- ====__Examples__
--
-- >>> ma <- makeUMArray 5 $ \i -> (toEnum (i + 97) :: Char) <$ putStrLn ("Handling index: " ++ show i)
-- Handling index: 0
-- Handling index: 1
-- Handling index: 2
-- Handling index: 3
-- Handling index: 4
-- >>> freezeUMArray ma
-- UArray "abcde"
--
-- @since 0.3.0
makeUMArray ::
     forall e m s. (Prim e, MonadPrim s m)
  => Size
  -> (Int -> m e)
  -> m (UMArray e s)
makeUMArray = makeMutWith newRawUMArray writeUMArray
{-# INLINE makeUMArray #-}


-- | Same as `makeUMArray`, but allocate memory as pinned.
--
-- [Unsafe] Same reasons as `newUMArray`
--
-- @since 0.3.0
makePinnedUMArray ::
     forall e m s. (Prim e, MonadPrim s m)
  => Size
  -> (Int -> m e)
  -> m (UMArray e s)
makePinnedUMArray = makeMutWith newRawPinnedUMArray writeUMArray
{-# INLINE makePinnedUMArray #-}

-- | Same as `makeUMArray`, but allocate memory as pinned and aligned.
--
-- [Unsafe] Same reasons as `newUMArray`
--
-- @since 0.3.0
makeAlignedPinnedUMArray ::
     forall e m s. (Prim e, MonadPrim s m)
  => Size
  -> (Int -> m e)
  -> m (UMArray e s)
makeAlignedPinnedUMArray = makeMutWith newRawAlignedPinnedUMArray writeUMArray
{-# INLINE makeAlignedPinnedUMArray #-}


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
-- >>> import Data.Prim
-- >>> let xs = "Hello Haskell"
-- >>> ma <- newRawUMArray (Size (length xs)) :: IO (UMArray Char RW)
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
  -- Susceptible to integer overflow:
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
newRawPinnedUMArray ::
     forall e m s. (Prim e, MonadPrim s m)
  => Size
  -> m (UMArray e s)
newRawPinnedUMArray n =
  prim $ \s ->
    case newPinnedByteArray# (unCountBytes# (coerce n :: Count e)) s of
      (# s', ma# #) -> (# s', UMArray ma# #)
{-# INLINE newRawPinnedUMArray #-}

-- | /O(1)/ - Same as `newRawPinnedUMArray` except allocate new mutable unboxed array as
-- pinned and aligned according to the `Prim` instance for the type of element @__e__@
--
-- Documentation for utilized primop: `newAlignedPinnedByteArray#`.
--
-- [Unsafe] Same reasons as in `newRawUMArray`.
--
-- @since 0.3.0
newRawAlignedPinnedUMArray ::
     forall e m s. (Prim e, MonadPrim s m)
  => Size
  -> m (UMArray e s)
newRawAlignedPinnedUMArray n =
  prim $ \s ->
    let c# = unCountBytes# (coerce n :: Count e)
        a# = alignment# (proxy# :: Proxy# e)
     in case newAlignedPinnedByteArray# c# a# s of
          (# s', ma# #) -> (# s', UMArray ma# #)
{-# INLINE newRawAlignedPinnedUMArray #-}


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
     forall e m s. (Prim e, MonadPrim s m)
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

-- | /O(1)/ - Either grow or shrink the size of a mutable unboxed array. Shrinking happens
-- without new allocation and data copy, while growing the array is implemented with
-- allocation of new unpinned array and copy of the data over from the source array
-- @srcMutArray@. This has a consequence that produced array @dstMutArray@ might refer to
-- the same @srcMutArray@ or to a totally new array, which can be checked with
-- `isSameUMArray`.
--
-- Documentation on the utilized primop: `resizeMutableByteArray#`.
--
-- [Unsafe] - Same reasons as in `newRawUMArray`. When size @sz@ is larger then the
-- size of @srcMutArray@ then @dstMutArray@ will contain uninitialized memory at its end,
-- hence a potential problem for referential transparency.
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
  -- Susceptible to integer overflow:
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
freezeUMArray ::
     forall e m s. MonadPrim s m
  => UMArray e s
  -> m (UArray e)
freezeUMArray (UMArray ma#) = prim $ \s ->
  case unsafeFreezeByteArray# ma# s of
    (# s', a# #) -> (# s', UArray a# #)
{-# INLINE freezeUMArray #-}

-------------
-- Helpers --
-- ======= --

-- | Default "raw" element for boxed arrays.
uninitialized ::
     HasCallStack
  => String -- ^ Module name
  -> String -- ^ Function name
  -> a
uninitialized mname fname =
  impureThrow $
  UndefinedElement $ mname ++ "." ++ fname ++ "\n" ++ prettyCallStack callStack
{-# NOINLINE uninitialized #-}

-- | Convert a list to a mutable array
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

-- | Check for equality of two arrays
--
-- @since 0.3.0
eqWith ::
     Eq e
  => (a e -> a e -> Bool) -- ^ Pointer equality
  -> (a e -> Size) -- ^ Get the size of array
  -> (a e -> Int -> e) -- ^ Index an element of an array
  -> a e -- ^ First array
  -> a e -- ^ Second array
  -> Bool
eqWith isSame sizeOf index a1 a2 = isSame a1 a2 || (sz1 == sizeOf a2 && loop 0)
  where
    sz1@(Size n) = sizeOf a1
    loop i
      | i < n = index a1 i == index a2 i && loop (i + 1)
      | otherwise = True
{-# INLINE eqWith #-}



-- | Check for equality of two arrays
--
-- @since 0.3.0
liftEqWith ::
     (forall e. a e -> Size) -- ^ Get the size of array
  -> (forall e. a e -> Int -> e) -- ^ Index an element of an array
  -> (b -> c -> Bool)
  -> a b -- ^ First array
  -> a c -- ^ Second array
  -> Bool
liftEqWith sizeOf index eq a1 a2 = sz1 == sizeOf a2 && loop 0
  where
    sz1@(Size n) = sizeOf a1
    loop i
      | i < n = (index a1 i `eq` index a2 i) && loop (i + 1)
      | otherwise = True
{-# INLINE liftEqWith #-}

liftShowsPrecArray :: Foldable f => String -> ([e] -> ShowS) -> Int -> f e -> ShowS
liftShowsPrecArray tyName listShows n arr
  | n > 1 = ('(' :) . inner . (')' :)
  | otherwise = inner
  where
    inner = (tyName ++) . (' ' :) . listShows (F.toList arr)


-- | Compare two arrays using supplied functions
--
-- @since 0.3.0
compareWith ::
     Ord e
  => (a e -> a e -> Bool) -- ^ Pointer equality
  -> (a e -> Size) -- ^ Get the size of array
  -> (a e -> Int -> e) -- ^ Index an element of an array
  -> a e -- ^ First array
  -> a e -- ^ Second array
  -> Ordering
compareWith isSame sizeOf index a1 a2
  | isSame a1 a2 = EQ
  | otherwise = loop 0
  where
    Size n = min (sizeOf a1) (sizeOf a2)
    loop i
      | i < n = compare (index a1 i) (index a2 i) <> loop (i + 1)
      | otherwise = compare (sizeOf a1) (sizeOf a2)
{-# INLINE compareWith #-}


-- | Compare two arrays using supplied functions
--
-- @since 0.3.0
liftCompareWith ::
     (forall e. a e -> Size) -- ^ Get the size of array
  -> (forall e. a e -> Int -> e) -- ^ Index an element of an array
  -> (b -> c -> Ordering)
  -> a b -- ^ First array
  -> a c -- ^ Second array
  -> Ordering
liftCompareWith sizeOf index comp a1 a2 = loop 0
  where
    Size n = min (sizeOf a1) (sizeOf a2)
    loop i
      | i < n = comp (index a1 i) (index a2 i) <> loop (i + 1)
      | otherwise = compare (sizeOf a1) (sizeOf a2)
{-# INLINE liftCompareWith #-}

-- | Append two arrays together using supplied functions
--
-- @since 0.3.0
appendWith ::
     (forall s. Size -> ST s (ma e s))
  -> (forall s. a e -> Int -> ma e s -> Int -> Size -> ST s ())
  -> (forall s. ma e s -> ST s (a e))
  -> (a e -> Size)
  -> a e
  -> a e
  -> a e
appendWith newRaw copy freeze sizeOf a1 a2 =
  runST $ do
    let n1 = sizeOf a1
        n2 = sizeOf a2
    ma <- newRaw (n1 + n2)
    copy a1 0 ma 0 n1
    copy a2 0 ma (coerce n1) n2
    freeze ma
{-# INLINE appendWith #-}


-- | Concat many arrays together using supplied functions
--
-- @since 0.3.0
concatWith ::
     (forall s. Size -> ST s (ma e s))
  -> (forall s. a e -> Int -> ma e s -> Int -> Size -> ST s ())
  -> (forall s. ma e s -> ST s (a e))
  -> (a e -> Size)
  -> [a e]
  -> a e
concatWith newRaw copy freeze sizeOf xs =
  runST $ do
    let as = [(sizeOf a, a) | a <- xs]
        !n = getSum $ foldMap (Sum . fst) as
    ma <- newRaw n
    let load i (sz, a) = (i + coerce sz) <$ copy a 0 ma i sz
    foldM_ load 0 as
    freeze ma
{-# INLINE concatWith #-}


-- | Repeat an array N times and concat them together using supplied functions
--
-- @since 0.3.0
cycleWith ::
     Monoid (a e)
  => (forall s. Size -> ST s (ma e s))
  -> (forall s. a e -> Int -> ma e s -> Int -> Size -> ST s ())
  -> (forall s. ma e s -> ST s (a e))
  -> (a e -> Size)
  -> Int
  -> a e
  -> a e
cycleWith newRaw copy freeze sizeOf k a
  | k <= 0 = mempty
  | otherwise =
    runST $ do
      let sz@(Size n) = sizeOf a
      ma <- newRaw (Size k * sz)
      let load i = when (i < k) $ copy a 0 ma (i * n) sz >> load (i + 1)
      load 0
      freeze ma
{-# INLINE cycleWith #-}
