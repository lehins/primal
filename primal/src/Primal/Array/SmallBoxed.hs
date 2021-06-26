{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
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
-- Module      : Primal.Array.SmallBoxed
-- Copyright   : (c) Alexey Kuleshevich 2020-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Array.SmallBoxed
  ( Size(..)
    -- * Small Boxed Array
    -- ** Immutable
  , SBArray(..)
  , isSameSBArray
  , sizeOfSBArray
  , indexSBArray
  , copySBArray
  , cloneSliceSBArray
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
  , cloneSliceSBMArray
  , shrinkSBMArray
  , resizeSBMArray
  , resizeRawSBMArray
  , freezeSBMArray
  , freezeCopySBMArray
  , casSBMArray
  -- * Re-export
  , MonadPrim
  ) where

import Data.Functor.Classes
import qualified Data.List.NonEmpty as NE (toList)
import Primal.Array.Internal
import Primal.Eval
import Primal.Exception
import Primal.Foreign
import Primal.Unbox


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
-- >>> import Primal.Array.SmallBoxed
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
-- >>> cloneSliceSBArray a 23 3
-- SBArray "xyz"
--
-- @since 0.3.0
cloneSliceSBArray ::
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
cloneSliceSBArray (SBArray a#) (I# i#) (Size (I# n#)) = SBArray (cloneSmallArray# a# i# n#)
{-# INLINE cloneSliceSBArray #-}



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
resizeRawSBMArray ma sz = resizeSBMArray ma sz (uninitialized "Primal.Array.SmallBoxed" "resizeRawSBMArray")
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

-- | /O(n)/ - evaluate all elements to NF
instance NFData e => MutNFData (SBMArray e) where
  rnfMutST ma = do
    Size k <- getSizeOfSBMArray ma
    let loop i =
          when (i < k) $ do
            rnf <$> readSBMArray ma i
            loop (i + 1)
    loop 0
  {-# INLINE rnfMutST #-}


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
-- >>> import Primal.Mutable.Freeze (freezeCloneMut)
-- >>> ma <- newSBMArray 4 (Nothing :: Maybe Integer)
-- >>> writeSBMArray ma 2 (Just 2)
-- >>> freezeCloneMut ma
-- SBArray [Nothing,Nothing,Just 2,Nothing]
--
-- It is important to note that an element is evaluated prior to being written into a
-- cell, so it will not overwrite the value of an array's cell if it evaluates to an
-- exception:
--
-- >>> import Primal.Exception
-- >>> writeSBMArray ma 2 (impureThrow DivideByZero)
-- *** Exception: divide by zero
-- >>> freezeCloneMut ma
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
newRawSBMArray sz = newLazySBMArray sz (uninitialized "Primal.Array.SmallBoxed" "newRawSBMArray")
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
-- `cloneSliceSBArray`, except that it works on mutable arrays.
--
-- Documentation for utilized primop: `cloneSmallMutableArray#`.
--
-- [Unsafe] When any of the preconditions for @startIx@ or @sz@ is violated this function
-- can result in a copy of some data that doesn't belong to @srcArray@ or more likely a
-- failure with a segfault. Failure with out of memory is also a possibility when the @sz is
-- too large.
--
-- @since 0.3.0
cloneSliceSBMArray ::
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
cloneSliceSBMArray (SBMArray ma#) (I# i#) (Size (I# n#)) =
  prim $ \s ->
    case cloneSmallMutableArray# ma# i# n# s of
      (# s', ma'# #) -> (# s', SBMArray ma'# #)
{-# INLINE cloneSliceSBMArray #-}


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
-- >>> import Primal.Mutable.Freeze (freezeCloneMut)
-- >>> ma <- makeSBMArray 5 (pure . (*10))
-- >>> freezeCloneMut ma
-- SBArray [0,10,20,30,40]
--
-- A possible mistake is to try and pass the expected value, instead of an actual element:
--
-- >>> casSBMArray ma 2 20 1000
-- (False,20)
-- >>> freezeCloneMut ma
-- SBArray [0,10,20,30,40]
--
-- But this will get us nowhere, since what we really need is the actual reference to the
-- value currently in the array cell, therefore we must read the exact value from the cell.
--
-- >>> expected <- readSBMArray ma 2
-- >>> r@(_, currentValue) <- casSBMArray ma 2 expected 1000
-- >>> freezeCloneMut ma
-- SBArray [0,10,1000,30,40]
-- >>> r
-- (True,1000)
-- >>> casSBMArray ma 2 currentValue 2000
-- (True,2000)
-- >>> freezeSBMArray ma
-- SBArray [0,10,2000,30,40]
--
-- In a concurrent setting current the contents of the cell can potentially be modified by
-- some other thread. When that happens we can retry the failed atomic update immendiately
-- by using the returned value.
--
-- @since 1.0.0
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
