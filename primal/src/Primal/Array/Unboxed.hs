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
-- Module      : Primal.Array.Unboxed
-- Copyright   : (c) Alexey Kuleshevich 2020-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Array.Unboxed
  ( Size(..)
    -- * Unboxed Array
    -- ** Immutable
  , UArray(..)
  , isSameUArray
  , isPinnedUArray
  , sizeOfUArray
  , indexUArray
  , cloneSliceUArray
  , copyUArray
  , thawUArray
  , thawCopyUArray
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
  , cloneSliceUMArray
  , setUMArray
  , shrinkUMArray
  , resizeUMArray
  , freezeUMArray
  , freezeCopyUMArray
  , casUMArray
  -- * Re-export
  , MonadPrim
  , Prim
  ) where

import Control.DeepSeq
import qualified Data.Array.Base as A
import qualified Data.List.NonEmpty as NE (toList)
import Primal.Array.Internal
import Primal.Exception
import Primal.Foreign
import Primal.Prim
import Primal.Prim.Atomic
import Primal.Prim.Class

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
-- >>> import Primal.Array.Unboxed
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
-- >>> let a = fromListUArray ['a'..'z']
-- >>> a
-- UArray "abcdefghijklmnopqrstuvwxyz"
-- >>> cloneSliceUArray a 23 3
-- UArray "xyz"
--
-- @since 1.0.0
cloneSliceUArray ::
     forall e. Prim e
  => UArray e
  -- ^ /srcArray/ - Immutable source array
  -> Int
  -- ^ /startIx/ - Location within @srcArray@ where the copy of elements should start from
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= startIx
  --
  -- > startIx < unSize (sizeOfUArray srcArray)
  -> Size
  -- ^ /sz/ - Size of the returned immutable array. Also this is the number of elements that
  -- will be copied over into the destionation array starting at the beginning.
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- > startIx + unSize sz < unSize (sizeOfUArray srcArray)
  --
  -- Should be less then the actual available memory
  -> UArray e
cloneSliceUArray arr off sz = runST $ thawCopyUArray arr off sz >>= freezeUMArray
{-# INLINE cloneSliceUArray #-}

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
  withPrimOffsets (undefined :: e) srcOff dstOff n $ \ srcOff# dstOff# n# ->
    prim_ (copyByteArray# src# srcOff# dst# dstOff# n#)
{-# INLINE copyUArray #-}


-- | /O(1)/ - Convert a pure immutable unboxed array into a mutable unboxed array. Use
-- `freezeUMArray` in order to go in the opposite direction.
--
-- Documentation for utilized primop: `unsafeThawByteArray#`.
--
-- [Unsafe] This function makes it possible to break referential transparency, because any
-- subsequent destructive operation to the mutable unboxed array will also be reflected in
-- the source immutable array as well. See `thawCopyUMArray` that avoids this problem with
-- fresh allocation.
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


-- | /O(sz)/ - Create a new mutable array with size @sz@ and copy that number of elements
-- from source immutable @srcArray@ starting at an offset @startIx@ into the newly created
-- @dstMutArray@. This function can help avoid an issue with referential transparency that
-- is inherent to `thawUArray`.
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
-- >>> let a = fromListUArray [1 .. 5 :: Int]
-- >>> ma <- thawCopyUArray a 1 3
-- >>> writeUMArray ma 1 10
-- >>> freezeUMArray ma
-- UArray [2,10,4]
-- >>> a
-- UArray [1,2,3,4,5]
--
-- @since 1.0.0
thawCopyUArray ::
     forall e m s. (Prim e, MonadPrim s m)
  => UArray e
  -- ^ /srcArray/ - Immutable source array
  -> Int
  -- ^ /startIx/ - Location within @srcArray@ where the copy of elements should start from
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= startIx
  --
  -- > startIx < unSize (sizeOfUArray srcArray)
  -> Size
  -- ^ /sz/ - Size of the returned mutable array. Also this is the number of elements that
  -- will be copied over into the destionation array starting at the beginning.
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- > startIx + unSize sz < unSize (sizeOfUArray srcArray)
  --
  -- Should be less then the actual available memory
  -> m (UMArray e s)
  -- ^ /dstMutArray/ - Newly created destination mutable boxed array
thawCopyUArray arr off sz = newRawUMArray sz >>= \marr -> marr <$ copyUArray arr off marr 0 sz
{-# INLINE thawCopyUArray #-}



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
-- >>> import Primal.Prim
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
-- >>> import Primal.Array.Unboxed
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
-- >>> import Primal.Prim
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
  withPrimOffsets (undefined :: e) srcOff dstOff n $ \ srcOff# dstOff# n# ->
    prim_ (copyMutableByteArray# src# srcOff# dst# dstOff# n#)
{-# INLINE moveUMArray #-}

withPrimOffsets :: forall e a. Prim e => e -> Int -> Int -> Size -> (Int# -> Int# -> Int# -> a) -> a
withPrimOffsets _ srcOff dstOff n f =
  let srcOff# = unOffBytes# (coerce srcOff :: Off e)
      dstOff# = unOffBytes# (coerce dstOff :: Off e)
      n# = unCountBytes# (coerce n :: Count e)
   in f srcOff# dstOff# n#
{-# INLINE withPrimOffsets #-}

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
-- in the resulting immutable array. See `freezeCopyUMArray` that avoids this problem with
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


-- | /O(sz)/ - Similar to `freezeUMArray`, except it creates a new array with the copy of a
-- subsection of a mutable array before converting it into an immutable.
--
-- Documentation for utilized primop: `freezeArray#`.
--
-- [Unsafe] When any of the preconditions for @startIx@ or @sz@ is violated this function
-- can result in a copy of some data that doesn't belong to @srcArray@ or more likely a
-- failure with a segfault or out of memory exception.
--
-- @since 0.3.0
freezeCopyUMArray ::
     forall e m s. (Prim e, MonadPrim s m)
  => UMArray e s
  -- ^ /srcArray/ - Source mutable array
  -> Int
  -- ^ /startIx/ - Location within @srcArray@ where the copy of elements should start from
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= startIx
  --
  -- > startIx < unSize (sizeOfUArray srcArray)
  -> Size
  -- ^ /sz/ - Size of the returned immutable array. Also this is the number of elements that
  -- will be copied over into the destionation array starting at the beginning.
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- > startIx + unSize sz < unSize (sizeOfUArray srcArray)
  --
  -- Should be less then actual available memory
  -> m (UArray e)
freezeCopyUMArray marr off sz = cloneSliceUMArray marr off sz >>= freezeUMArray
{-# INLINE freezeCopyUMArray #-}

-- | /O(sz)/ - Allocate a new mutable array of size @sz@ and copy that number of the
-- elements over from the @srcArray@ starting at index @ix@. Similar to `cloneSliceUArray`,
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
cloneSliceUMArray ::
     forall e m s. (Prim e, MonadPrim s m)
  => UMArray e s
  -- ^ /srcArray/ - Source mutable array
  -> Int
  -- ^ /startIx/ - Location within @srcArray@ where the copy of elements should start from
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= startIx
  --
  -- > startIx < unSize (sizeOfUArray srcArray)
  -> Size
  -- ^ /sz/ - Size of the returned mutable array. Also this is the number of elements that
  -- will be copied over into the destionation array starting at the beginning.
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- > startIx + unSize sz < unSize (sizeOfUArray srcArray)
  --
  -- Should be less then actual available memory
  -> m (UMArray e s)
cloneSliceUMArray marr off sz = freezeUMArray marr >>= \arr -> thawCopyUArray arr off sz
{-# INLINE cloneSliceUMArray #-}



-- | Compare-and-swap operation. Returns a boolean value, which indicates `True` for
-- success and `False` otherwise for the update, as well as the current value at the
-- supplied index. In case of success current value returned will be the newly supplied
-- one, otherwise it will still be the old one. Note that there is no `Eq` constraint on
-- the element, that is because compare operation is done on the memory contents itself
-- according to the `Atomic` class for the datatype
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
-- @since 1.0.0
casUMArray ::
     (Atomic e, MonadPrim s m)
  => UMArray e s -- ^ Mutable array to mutate
  -> Int -- ^ Index at which the cell should be set to the new value
  -> e -- ^ Reference to the expected boxed value
  -> e -- ^ New value to update the cell with
  -> m (Bool, e)
casUMArray (UMArray mba#) (I# i#) expected new =
  prim $ \s ->
    case casBoolMutableByteArray# mba# i# expected new s of
      (# s', True #) -> (# s', (True, new) #)
      (# s', False #) ->
        case readMutableByteArray# mba# i# s' of
          (# s'', old #) -> (# s'', (False, old) #)
{-# INLINE casUMArray #-}
