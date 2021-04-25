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
-- Module      : Primal.Array.Boxed
-- Copyright   : (c) Alexey Kuleshevich 2020-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Array.Boxed
  ( Size(..)
    -- * Boxed Array
    -- $boxedArray
    -- ** Immutable
  , BArray(..)
  , isSameBArray
  , sizeOfBArray
  , indexBArray
  , copyBArray
  , cloneSliceBArray
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
  , cloneSliceBMArray
  , shrinkBMArray
  , resizeBMArray
  , resizeRawBMArray
  , freezeBMArray
  , freezeCopyBMArray
  , casBMArray
  -- * Re-export
  , MonadPrim
  ) where

import Data.Functor.Classes
import qualified Data.List.NonEmpty as NE (toList)
import qualified GHC.Arr as A
import Primal.Array.Internal
import Primal.Eval
import Primal.Exception
import Primal.Foreign
import Primal.Unbox


-----------------
-- Boxed Array --
-- =========== --


-- Immutable Boxed Array --
---------------------------

-- $boxedArray A boxed array is essentially a contiguous chunk of memory that holds
-- pointers to actual elements that are being stored somewhere else on the heap. Therefore
-- it is more efficient to use `Primal.Array.UArray` if the element being stored has a `Prim` instance
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
-- >>> import Primal.Array.Boxed
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
-- >>> cloneSliceBArray a 23 3
-- BArray "xyz"
--
-- @since 1.0.0
cloneSliceBArray ::
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
cloneSliceBArray (BArray a#) (I# i#) (Size (I# n#)) = BArray (cloneArray# a# i# n#)
{-# INLINE cloneSliceBArray #-}


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
-- > thawCopyBArray a i n === thawBArray $ cloneSliceBArray a i n
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
-- >>> let a = fromListBArray [1 .. 5 :: Integer]
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

-- | /O(n)/ - evaluate all elements to NF
instance NFData e => MutNFData (BMArray e) where
  rnfMutST ma = do
    Size k <- getSizeOfBMArray ma
    let loop i =
          when (i < k) $ do
            rnf <$> readBMArray ma i
            loop (i + 1)
    loop 0
  {-# INLINE rnfMutST #-}


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
-- >>> import Primal.Exception
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
newRawBMArray sz = newLazyBMArray sz (uninitialized "Primal.Array.Boxed" "newRawBMArray")
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
-- prop> cloneSliceBMArray ma i n === freezeCopyBMArray ma i n >>= thawBArray
-- prop> cloneSliceBMArray ma i n === newBMArray n undefined >>= \mb -> mb <$ moveBMArray ma i mb 0 n
-- | /O(sz)/ - Allocate a new mutable array of size @sz@ and copy that number of the
-- elements over from the @srcArray@ starting at index @ix@. Similar to `cloneSliceBArray`,
-- except it works on mutable arrays.
--
-- Documentation for utilized primop: `cloneMutableArray#`.
--
-- [Unsafe] When any of the preconditions for @startIx@ or @sz@ is violated this function
-- can result in a copy of some data that doesn't belong to @srcArray@ or more likely a
-- failure with a segfault. Failure with out of memory is also a possibility when the @sz is
-- too large.
--
-- @since 1.0.0
cloneSliceBMArray ::
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
cloneSliceBMArray (BMArray ma#) (I# i#) (Size (I# n#)) =
  prim $ \s ->
    case cloneMutableArray# ma# i# n# s of
      (# s', ma'# #) -> (# s', BMArray ma'# #)
{-# INLINE cloneSliceBMArray #-}



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
resizeRawBMArray ma sz = resizeBMArray ma sz (uninitialized "Primal.Array.Boxed" "resizeRawBMArray")
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
-- BArray [0,10,20,30,40]
--
-- A possible mistake is to try and pass the expected value, instead of an actual element:
--
-- >>> casBMArray ma 2 20 1000
-- (False,20)
-- >>> freezeBMArray ma
-- BArray [0,10,20,30,40]
--
-- But this will get us nowhere, since what we really need is the actual reference to the
-- value currently in the array cell
--
-- >>> expected <- readBMArray ma 2
-- >>> r@(_, currentValue) <- casBMArray ma 2 expected 1000
-- >>> r
-- (True,1000)
-- >>> freezeBMArray ma
-- BArray [0,10,1000,30,40]
--
-- In a concurrent setting current value can potentially be modified by some other
-- thread, therefore returned value can be immediately used as the expected one to the
-- next call, if we need to retry the atomic swap:
--
-- >>> casBMArray ma 2 currentValue 2000
-- (True,2000)
-- >>> freezeBMArray ma
-- BArray [0,10,2000,30,40]
--
-- @since 1.0.0
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
