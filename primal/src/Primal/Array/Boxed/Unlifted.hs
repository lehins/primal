{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Primal.Array.Boxed.Unlifted
-- Copyright   : (c) Alexey Kuleshevich 2020-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Array.Boxed.Unlifted
  ( Size(..)
    -- * Unlifted Array
    -- ** Immutable
  , UBArray(..)
  , sizeOfUBArray
  , indexUBArray
  , cloneSliceUBArray
  , copyUBArray
  , thawUBArray
  , thawCopyUBArray
  , toListUBArray
  , fromListUBArray
  , fromListUBArrayN
  -- ** Mutable
  , UBMArray(..)
  , isSameUBMArray
  , getSizeOfUBMArray
  , readUBMArray
  , writeUBMArray
  , newUBMArray
  , newRawUBMArray
  , makeUBMArray

  , moveUBMArray
  , cloneSliceUBMArray
  , setUBMArray -- TODO implement using recursive copy region logic.
  , shrinkUBMArray
  , resizeUBMArray
  , freezeUBMArray
  , freezeCopyUBMArray
  -- *** Mutable content
  , readMutUBMArray
  , writeMutUBMArray
  , makeMutUBMArray
  -- * Re-export
  , Primal
  , Unlift
  , MutUnlift
  ) where

import qualified Data.List.NonEmpty as NE (toList)
import Primal.Array.Internal
import Primal.Eval
import Primal.Foreign
import Primal.Monad
import Primal.Unlift
import Data.Semigroup

--------------------------
-- Unlifted Boxed Array --
-- ==================== --


-- Immutable Unlifted Boxed Array --
------------------------------------

data UBArray a = UBArray ArrayArray#
type role UBArray nominal

instance (Unlift e, Show e) => Show (UBArray e) where
  showsPrec n arr
    | n > 1 = ('(' :) . inner . (')' :)
    | otherwise = inner
    where
      inner = ("UBArray " ++) . shows (toList arr)

instance Unlift e => IsList (UBArray e) where
  type Item (UBArray e) = e
  fromList = fromListUBArray
  {-# INLINE fromList #-}
  fromListN n = fromListUBArrayN (coerce n)
  {-# INLINE fromListN #-}
  toList = toListUBArray
  {-# INLINE toList #-}

instance (Unlift e, NFData e) => NFData (UBArray e) where
  rnf = foldrWithFB sizeOfUBArray indexUBArray deepseq ()
  {-# INLINE rnf #-}


instance (Unlift e, Eq e) => Eq (UBArray e) where
  (==) = eqWith isSameUBArray sizeOfUBArray indexUBArray
  {-# INLINE (==) #-}

instance (Unlift e, Ord e) => Ord (UBArray e) where
  compare = compareWith isSameUBArray sizeOfUBArray indexUBArray
  {-# INLINE compare #-}


instance Unlift e => Semigroup (UBArray e) where
  (<>) = appendWith newRawUBMArray copyUBArray freezeUBMArray sizeOfUBArray
  {-# INLINE (<>) #-}
  sconcat xs = concatWith newRawUBMArray copyUBArray freezeUBMArray sizeOfUBArray (NE.toList xs)
  {-# INLINE sconcat #-}
  stimes n = cycleWith newRawUBMArray copyUBArray freezeUBMArray sizeOfUBArray (fromIntegral n)
  {-# INLINE stimes #-}

instance Unlift e => Monoid (UBArray e) where
  mempty = runST $ newRawUBMArray 0 >>= freezeUBMArray
  {-# NOINLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}
  mconcat = concatWith newRawUBMArray copyUBArray freezeUBMArray sizeOfUBArray
  {-# INLINE mconcat #-}


-- | /O(1)/ - Get the size of an immutable array in number of elements.
--
-- Documentation for utilized primop: `sizeofByteArray#`.
--
-- @since 1.0.0
sizeOfUBArray ::
     forall e. Unlift e
  => UBArray e
  -> Size
sizeOfUBArray (UBArray a#) = Size (I# (sizeofArrayArray# a#))
{-# INLINE sizeOfUBArray #-}


-- | Compare pointers for two immutable arrays and see if they refer to the exact same one.
--
-- @since 0.3.0
isSameUBArray :: UBArray a -> UBArray a -> Bool
isSameUBArray a1 a2 = runST (isSameUBMArray <$> thawUBArray a1 <*> thawUBArray a2)
{-# INLINE isSameUBArray #-}

-- | /O(1)/ - Index an element of a pure unboxed array.
--
-- Documentation for utilized primop: `indexByteArray#`.
--
-- [Unsafe] Bounds are not checked. When a precondition for @ix@ argument is violated the
-- result is either unpredictable output or failure with a segfault.
--
-- ==== __Examples__
--
-- >>> import Primal.Array.Unlifted
-- >>> let a = fromListUBArray ([Left pi, Right 123] :: [Either Double Int])
-- >>> indexUBArray a 0
-- Left 3.141592653589793
-- >>> indexUBArray a 1
-- Right 123
--
-- @since 1.0.0
indexUBArray ::
     forall e. Unlift e
  => UBArray e
  -- ^ /array/ - Array where to lookup an element from
  -> Int
  -- ^ /ix/ - Position of the element within the @array@
  --
  -- /__Precoditions:__/
  --
  -- > 0 <= ix
  --
  -- > ix < unSize (sizeOfUBArray array)
  -> e
indexUBArray (UBArray aa#) (I# i#) = indexArrayArray# aa# i#
{-# INLINE indexUBArray #-}


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
-- >>> let a = fromListUBArray ['a'..'z']
-- >>> a
-- UBArray "abcdefghijklmnopqrstuvwxyz"
-- >>> cloneSliceUBArray a 23 3
-- UBArray "xyz"
--
-- @since 1.0.0
cloneSliceUBArray ::
     forall e. Unlift e
  => UBArray e
  -- ^ /srcArray/ - Immutable source array
  -> Int
  -- ^ /startIx/ - Location within @srcArray@ where the copy of elements should start from
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= startIx
  --
  -- > startIx < unSize (sizeOfUBArray srcArray)
  -> Size
  -- ^ /sz/ - Size of the returned immutable array. Also this is the number of elements that
  -- will be copied over into the destionation array starting at the beginning.
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- > startIx + unSize sz < unSize (sizeOfUBArray srcArray)
  --
  -- Should be less then the actual available memory
  -> UBArray e
cloneSliceUBArray arr off sz = runST $ thawCopyUBArray arr off sz >>= freezeUBMArray
{-# INLINE cloneSliceUBArray #-}

-- | /O(sz)/ - Copy a subsection of an immutable array into a subsection of another mutable
-- array. Source and destination arrays must not be the same array in different states.
--
-- Documentation for utilized primop: `copyByteArray#`.
--
-- [Unsafe] When any of the preconditions for @srcStartIx@, @dstStartIx@ or @sz@ is violated
-- this function can result in a copy of some data that doesn't belong to @srcArray@ or
-- failure with a segfault.
--
-- @since 1.0.0
copyUBArray ::
     forall e m s. (Unlift e, Primal s m)
  => UBArray e
  -- ^ /srcArray/ - Source immutable array
  --
  -- /__Precondition:__/
  --
  -- > srcMutArray <- thawUBArray srcArray
  -- > srcMutArray /= dstMutArray
  -> Int
  -- ^ /srcStartIx/ - Offset into the source immutable array where copy should start from
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= srcStartIx
  --
  -- > srcStartIx < unSize (sizeOfUBArray srcArray)
  -> UBMArray e s -- ^ /dstMutArray/ - Destination mutable array
  -> Int
  -- ^ /dstStartIx/ - Offset into the destination mutable array where the copy should start
  -- at
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= dstStartIx
  --
  -- > dstSize <- getSizeOfMUBArray dstMutArray
  -- > dstStartIx < unSize dstSize
  -> Size
  -- ^ /sz/ - Number of elements to copy over
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- > srcStartIx + unSize sz < unSize (sizeOfUBArray srcArray)
  --
  -- > dstSize <- getSizeOfMUBArray dstMutArray
  -- > dstStartIx + unSize sz < unSize dstSize
  -> m ()
copyUBArray (UBArray src#) (I# srcOff#) (UBMArray dst#) (I# dstOff#) (Size (I# n#)) =
  primal_ (copyArrayArray# src# srcOff# dst# dstOff# n#)
{-# INLINE copyUBArray #-}


-- | /O(1)/ - Convert a pure immutable unboxed array into a mutable unboxed array. Use
-- `freezeUBMArray` in order to go in the opposite direction.
--
-- [Unsafe] This function makes it possible to break referential transparency, because any
-- subsequent destructive operation to the mutable unboxed array will also be reflected in
-- the source immutable array as well. See `thawCopyUBMArray` that avoids this problem with
-- fresh allocation.
--
-- ====__Examples__
--
-- >>> ma <- thawUBArray $ fromListUBArray [1 .. 5 :: Int]
-- >>> writeUBMArray ma 1 10
-- >>> freezeUBMArray ma
-- UBArray [1,10,3,4,5]
--
-- Be careful not to retain a reference to the pure immutable source array after the
-- thawed version gets mutated.
--
-- >>> let a = fromListUBArray [1 .. 5 :: Int]
-- >>> ma' <- thawUBArray a
-- >>> writeUBMArray ma' 0 100000
-- >>> a
-- UBArray [100000,2,3,4,5]
--
-- @since 1.0.0
thawUBArray :: forall e m s. Primal s m => UBArray e -> m (UBMArray e s)
thawUBArray (UBArray a#) =
  primal $ \s ->
    case unsafeThawArray# (unsafeCoerce# a#) s of
      (# s', ma# #) -> (# s', UBMArray (unsafeCoerce# ma#) #)
{-# INLINE thawUBArray #-}


-- | /O(sz)/ - Create a new mutable array with size @sz@ and copy that number of elements
-- from source immutable @srcArray@ starting at an offset @startIx@ into the newly created
-- @dstMutArray@. This function can help avoid an issue with referential transparency that
-- is inherent to `thawUBArray`.
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
-- >>> let a = fromListUBArray [1 .. 5 :: Int]
-- >>> ma <- thawCopyUBArray a 1 3
-- >>> writeUBMArray ma 1 10
-- >>> freezeUBMArray ma
-- UBArray [2,10,4]
-- >>> a
-- UBArray [1,2,3,4,5]
--
-- @since 1.0.0
thawCopyUBArray ::
     forall e m s. (Unlift e, Primal s m)
  => UBArray e
  -- ^ /srcArray/ - Immutable source array
  -> Int
  -- ^ /startIx/ - Location within @srcArray@ where the copy of elements should start from
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= startIx
  --
  -- > startIx < unSize (sizeOfUBArray srcArray)
  -> Size
  -- ^ /sz/ - Size of the returned mutable array. Also this is the number of elements that
  -- will be copied over into the destionation array starting at the beginning.
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- > startIx + unSize sz < unSize (sizeOfUBArray srcArray)
  --
  -- Should be less then the actual available memory
  -> m (UBMArray e s)
  -- ^ /dstMutArray/ - Newly created destination mutable boxed array
thawCopyUBArray arr off sz = newRawUBMArray sz >>= \marr -> marr <$ copyUBArray arr off marr 0 sz
{-# INLINE thawCopyUBArray #-}



-- | /O(n)/ - Convert a pure boxed array into a list. It should work fine with GHC built-in list
-- fusion.
--
-- @since 0.1.0
toListUBArray ::
     forall e. Unlift e
  => UBArray e
  -> [e]
toListUBArray ba = build (\ c n -> foldrWithFB sizeOfUBArray indexUBArray c n ba)
{-# INLINE toListUBArray #-}

-- | /O(min(length list, sz))/ - Same as `fromListUBArray`, except it will allocate an array exactly of @n@ size, as
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
-- >>> fromListUBArrayN 3 [1 :: Int, 2, 3]
-- UBArray [1,2,3]
-- >>> fromListUBArrayN 3 [1 :: Int ..]
-- UBArray [1,2,3]
--
-- @since 0.1.0
fromListUBArrayN ::
     forall e. Unlift e
  => Size -- ^ /sz/ - Expected number of elements in the @list@
  -> [e] -- ^ /list/ - A list to bew loaded into the array
  -> UBArray e
fromListUBArrayN sz xs =
  runST $ fromListMutWith newRawUBMArray writeUBMArray sz xs >>= freezeUBMArray
{-# INLINE fromListUBArrayN #-}


-- | /O(length list)/ - Convert a list into an immutable boxed array. It is more efficient to use
-- `fromListUBArrayN` when the number of elements is known ahead of time. The reason for this
-- is that it is necessary to iterate the whole list twice: once to count how many elements
-- there is in order to create large enough array that can fit them; and the second time to
-- load the actual elements. Naturally, infinite lists will grind the program to a halt.
--
-- ====__Example__
--
-- >>> fromListUBArray "Hello Haskell"
-- UBArray "Hello Haskell"
--
-- @since 1.0.0
fromListUBArray ::
     forall e. Unlift e
  => [e]
  -> UBArray e
fromListUBArray xs = fromListUBArrayN (coerce (length xs)) xs
{-# INLINE fromListUBArray #-}

-- Mutable Unlifted Boxed Array --
----------------------------------

data UBMArray (ma :: k) s = UBMArray (MutableArrayArray# s)
type role UBMArray nominal nominal

-- | /O(n)/ - evaluate all elements to NF
instance (Unlift e, NFData e) => MutNFData (UBMArray e) where
  rnfMutST ma = do
    Size k <- getSizeOfUBMArray ma
    let loop i =
          when (i < k) $ do
            rnf <$> readUBMArray ma i
            loop (i + 1)
    loop 0
  {-# INLINE rnfMutST #-}

-- | /O(n)/ - evaluate all elements to NF
instance (MutUnlift e, MutNFData e) => MutNFData (UBMArray e) where
  rnfMutST ma = do
    Size k <- getSizeOfUBMArray ma
    let loop i =
          when (i < k) $ do
            rnfMut =<< readMutUBMArray ma i
            loop (i + 1)
    loop 0
  {-# INLINE rnfMutST #-}

-- | /O(1)/ - Compare pointers for two mutable arrays and see if they refer to the exact same one.
--
-- Documentation for utilized primop: `sameMutableArrayArray#`.
--
-- @since 1.0.0
isSameUBMArray :: forall a b s. UBMArray a s -> UBMArray b s -> Bool
isSameUBMArray (UBMArray ma1#) (UBMArray ma2#) = isTrue# (sameMutableArrayArray# ma1# ma2#)
{-# INLINE isSameUBMArray #-}

-- | /O(1)/ - Get the size of a mutable unboxed array
--
-- Documentation for utilized primop: `getSizeofMutableByteArray#`.
--
-- ====__Example__
--
-- >>> ma <- thawUBArray $ fromListUBArray ['a' .. 'z']
-- >>> getSizeOfUBMArray ma
-- Size {unSize = 26}
--
-- @since 1.0.0
getSizeOfUBMArray ::
     forall e m s. Primal s m
  => UBMArray e s
  -> m Size
getSizeOfUBMArray (UBMArray ma#) =
  primal $ \s -> case getSizeofMutableArrayArray# ma# s of
                   (# s', n# #) -> (# s', Size (I# n#) #)
{-# INLINE getSizeOfUBMArray #-}



-- | /O(1)/ - Read an element from a mutable array at the supplied index.
--
-- [Unsafe] Violation of @ix@ preconditions can result in value that doesn't belong to
-- @srcMutArray@ or a failure with a segfault
--
-- ==== __Examples__
--
-- >>> ma <- thawUBArray $ fromListUBArray "Hi!"
-- >>> readUBMArray ma 2
-- '!'
--
-- @since 1.0.0
readUBMArray ::
     forall e m s. (Unlift e, Primal s m)
  => UBMArray e s -- ^ /srcMutArray/ - Array to read an element from
  -> Int
  -- ^ /ix/ - Index for the element we need within the the @srcMutArray@
  --
  -- /__Precoditions:__/
  --
  -- > 0 <= ix
  --
  -- > srcSize <- getSizeOfMUBArray srcMutArray
  -- > ix < unSize srcSize
  -> m e
readUBMArray (UBMArray maa#) (I# i#) = primal (readMutableArrayArray# maa# i#)
{-# INLINE readUBMArray #-}


-- | /O(1)/ - Write an element into a mutable array at a supplied index.
--
-- Documentation for utilized primop: `writeMutableByteArray#`.
--
-- [Unsafe] Violation of @ix@ preconditions can result in heap corruption or a failure
-- with a segfault
--
-- ==== __Examples__
--
-- >>> ma <- newRawUBMArray 4 :: IO (UBMArray (Maybe Int) RW)
-- >>> mapM_ (\i -> writeUBMArray ma i Nothing) [0, 1, 3]
-- >>> writeUBMArray ma 2 (Just 2)
-- >>> freezeUBMArray ma
-- UBArray [Nothing,Nothing,Just 2,Nothing]
--
-- @since 1.0.0
writeUBMArray ::
     forall e m s. (Unlift e, Primal s m)
  => UBMArray e s
  -> Int
  -> e
  -> m ()
writeUBMArray (UBMArray ma#) (I# i#) a = primal_ (writeMutableArrayArray# ma# i# a)
{-# INLINE writeUBMArray #-}





-- | /O(1)/ - Read a mutable element from a mutable array at the supplied index.
--
-- [Unsafe] Violation of @ix@ preconditions can result in value that doesn't belong to
-- @srcMutArray@ or a failure with a segfault
--
-- ==== __Examples__
--
-- >>> ma <- thawUBArray $ fromListUBArray "Hi!"
-- >>> readUBMArray ma 2
-- '!'
--
-- @since 1.0.0
readMutUBMArray ::
     forall me m s. (MutUnlift me, Primal s m)
  => UBMArray me s -- ^ /srcMutArray/ - Array to read an element from
  -> Int
  -- ^ /ix/ - Index for the element we need within the the @srcMutArray@
  --
  -- /__Precoditions:__/
  --
  -- > 0 <= ix
  --
  -- > srcSize <- getSizeOfMUBArray srcMutArray
  -- > ix < unSize srcSize
  -> m (me s)
readMutUBMArray (UBMArray maa#) (I# i#) = primal (readMutMutableArrayArray# maa# i#)
{-# INLINE readMutUBMArray #-}


-- | /O(1)/ - Write a mutable  element into a mutable array at a supplied index.
--
-- [Unsafe] Violation of @ix@ preconditions can result in heap corruption or a failure
-- with a segfault
--
-- ==== __Examples__
--
-- >>> ma <- newRawUBMArray 4 :: IO (UBMArray (Maybe Int) RW)
-- >>> mapM_ (\i -> writeUBMArray ma i Nothing) [0, 1, 3]
-- >>> writeUBMArray ma 2 (Just 2)
-- >>> freezeUBMArray ma
-- UBArray [Nothing,Nothing,Just 2,Nothing]
--
-- @since 1.0.0
writeMutUBMArray ::
     forall me m s. (Primal s m, MutUnlift me)
  => UBMArray me s
  -> Int
  -> me s
  -> m ()
writeMutUBMArray (UBMArray maa#) (I# i#) a = primal_ (writeMutMutableArrayArray# maa# i# a)
{-# INLINE writeMutUBMArray #-}



-- prop> newUBMArray sz a === makeUBMArray sz (const (pure a))
-- | /O(sz)/ - Allocate new mutable unboxed array. Similar to `newRawUBMArray`, except all
-- elements are initialized to the supplied initial value. This is equivalent to
-- @makeUBMArray sz (const (pure a))@ but often will be more efficient.
--
-- [Unsafe] When any of preconditions for @sz@ argument is violated the outcome is
-- unpredictable. One possible outcome is termination with `HeapOverflow` async
-- exception.
--
-- ==== __Examples__
--
-- >>> import Primal.Array.Unlifted
-- >>> let xs = "Hello"
-- >>> ma <- newUBMArray (Size (length xs) + 8) '!' :: IO (UBMArray Char RW)
-- >>> mapM_ (\(i, x) -> writeUBMArray ma i x) (zip [0..] xs)
-- >>> freezeUBMArray ma
-- UBArray "Hello!!!!!!!!"
--
-- @since 1.0.0
newUBMArray ::
     forall e m s. (Unlift e, Primal s m)
  => Size
  -- ^ /sz/ - Size of the array in number of elements.
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- Should be below some upper limit that is dictated by the operating system and the total
  -- amount of available memory
  -> e
  -> m (UBMArray e s)
newUBMArray sz e = makeUBMArray sz (pure . const e)
{-# INLINE newUBMArray #-}



-- | Create new mutable unboxed array of the supplied size and fill it with a monadic action
-- that is applied to indices of each array cell.
--
-- [Unsafe] Same reasons as `newUBMArray`
--
-- ====__Examples__
--
-- >>> ma <- makeUBMArray 5 $ \i -> (toEnum (i + 97) :: Char) <$ putStrLn ("Handling index: " ++ show i)
-- Handling index: 0
-- Handling index: 1
-- Handling index: 2
-- Handling index: 3
-- Handling index: 4
-- >>> freezeUBMArray ma
-- UBArray "abcde"
--
-- @since 1.0.0
makeUBMArray ::
     forall e m s. (Unlift e, Primal s m)
  => Size
  -> (Int -> m e)
  -> m (UBMArray e s)
makeUBMArray = makeMutWith newRawUBMArray writeUBMArray
{-# INLINE makeUBMArray #-}


-- | Just like `makeUBMArray` but elements are themselves mutable
makeMutUBMArray ::
     forall e m s. (MutUnlift e, Primal s m)
  => Size
  -> (Int -> m (e s))
  -> m (UBMArray e s)
makeMutUBMArray = makeMutWith newRawUBMArray writeMutUBMArray
{-# INLINE makeMutUBMArray #-}


-- | /O(1)/ - Allocate new mutable array. None of the elements are initialized so
-- expect it to contain some random garbage.
--
-- Documentation for utilized primop: `newArrayArray#`.
--
-- [Unsafe] When any of preconditions for @sz@ argument is violated the outcome is
-- unpredictable. One possible outcome is termination with `HeapOverflow` async
-- exception.
--
-- ==== __Examples__
--
-- >>> let xs = "Hello Haskell"
-- >>> ma <- newRawUBMArray (Size (length xs)) :: IO (UBMArray Char RW)
-- >>> mapM_ (\(i, x) -> writeUBMArray ma i x) (zip [0..] xs)
-- >>> freezeUBMArray ma
-- UBArray "Hello Haskell"
--
-- @since 1.0.0
newRawUBMArray ::
     forall e m s. Primal s m
  => Size
  -- ^ /sz/ - Size of the array in number of elements.
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- Should be below some upper limit that is dictated by the operating system and the total
  -- amount of available memory
  -> m (UBMArray e s)
newRawUBMArray (Size (I# n#)) =
  primal $ \s ->
    case newArrayArray# n# s of
      (# s', ma# #) -> (# s', UBMArray ma# #)
{-# INLINE newRawUBMArray #-}

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
-- @since 1.0.0
moveUBMArray ::
     forall e m s. (Unlift e, Primal s m)
  => UBMArray e s -- ^ /srcMutArray/ - Source mutable array
  -> Int
  -- ^ /srcStartIx/ - Offset into the source mutable array where copy should start from
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= srcStartIx
  --
  -- > srcSize <- getSizeOfMUBArray srcMutArray
  -- > srcStartIx < unSize srcSize
  -> UBMArray e s -- ^ /dstMutArray/ - Destination mutable array
  -> Int
  -- ^ /dstStartIx/ - Offset into the destination mutable array where copy should start to
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= dstStartIx
  --
  -- > dstSize <- getSizeOfMUBArray dstMutArray
  -- > dstStartIx < unSize dstSize
  -> Size
  -- ^ /sz/ - Number of elements to copy over
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- > srcSize <- getSizeOfMUBArray srcMutArray
  -- > srcStartIx + unSize sz < unSize srcSize
  --
  -- > dstSize <- getSizeOfMUBArray dstMutArray
  -- > dstStartIx + unSize sz < unSize dstSize
  -> m ()
moveUBMArray (UBMArray src#) (I# srcOff#) (UBMArray dst#) (I# dstOff#) (Size (I# n#)) =
  primal_ (copyMutableArrayArray# src# srcOff# dst# dstOff# n#)
{-# INLINE moveUBMArray #-}


-- | /O(n)/ - Write the same element into the @dstMutArray@ mutable array @n@ times starting
-- at @dstStartIx@ offset.
--
-- [Unsafe]
--
-- @since 0.3.0
setUBMArray ::
     forall e m s. (Unlift e, Primal s m)
  => UBMArray e s -- ^ /dstMutArray/ - Mutable array
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
setUBMArray ma o (Size n) e = forM_ [o .. n - 1] $ \i -> writeUBMArray ma i e
{-# INLINE setUBMArray #-}


-- | /O(1)/ - Reduce the size of a mutable array.
--
-- Documentation for utilized primop: `shrinkMutableByteArray#`.
--
-- [Unsafe] - Violation of preconditions for @sz@ leads to undefined behavior
--
-- 0.3.0
shrinkUBMArray ::
     forall e m s. (Primal s m, Unlift e)
  => UBMArray e s -- ^ /mutArray/ - Mutable array to be shrunk
  -> Size
  -- ^ /sz/ - New size for the array in number of elements
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- > curSize <- getSizeOfUBMArray mutArray
  -- > sz <= curSize
  -> m ()
shrinkUBMArray (UBMArray mb#) (Size (I# n#)) = primal_ (shrinkMutableArrayArray# mb# n#)
{-# INLINE shrinkUBMArray #-}

-- | /O(1)/ - Either grow or shrink the size of a mutable unboxed array. Shrinking happens
-- without new allocation and data copy, while growing the array is implemented with
-- allocation of new unpinned array and copy of the data over from the source array
-- @srcMutArray@. This has a consequence that produced array @dstMutArray@ might refer to
-- the same @srcMutArray@ or to a totally new array, which can be checked with
-- `isSameUBMArray`.
--
-- Documentation on the utilized primop: `resizeMutableByteArray#`.
--
-- [Unsafe] - Same reasons as in `newRawUBMArray`. When size @sz@ is larger then the
-- size of @srcMutArray@ then @dstMutArray@ will contain uninitialized memory at its end,
-- hence a potential problem for referential transparency.
--
-- 0.3.0
resizeUBMArray ::
     forall e m s. (Primal s m, Unlift e)
  => UBMArray e s -- ^ /srcMutArray/ - Mutable unboxed array to be shrunk
  -> Size
  -- ^ /sz/ - New size for the array in number of elements
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- Should be below some upper limit that is dictated by the operating system and the total
  -- amount of available memory
  -> m (UBMArray e s) -- ^ /dstMutArray/ - produces a resized version of /srcMutArray/.
resizeUBMArray (UBMArray mb#) (Size (I# n#)) =
  primal $ \s ->
    case resizeMutableArrayArray# mb# n# s of
      (# s', mb'# #) -> (# s', UBMArray mb'# #)
{-# INLINE resizeUBMArray #-}



-- | /O(1)/ - Convert a mutable unboxed array into an immutable one. Use `thawUBArray` in order
-- to go in the opposite direction.
--
-- Documentation on the utilized primop: `unsafeFreezeByteArray#`.
--
-- [Unsafe] This function makes it possible to break referential transparency, because any
-- subsequent destructive operation to the source mutable boxed array will also be reflected
-- in the resulting immutable array. See `freezeCopyUBMArray` that avoids this problem with
-- fresh allocation.
--
-- @since 1.0.0
freezeUBMArray ::
     forall e m s. Primal s m
  => UBMArray e s
  -> m (UBArray e)
freezeUBMArray (UBMArray ma#) = primal $ \s ->
  case unsafeFreezeArrayArray# ma# s of
    (# s', a# #) -> (# s', UBArray a# #)
{-# INLINE freezeUBMArray #-}


-- | /O(sz)/ - Similar to `freezeUBMArray`, except it creates a new array with the copy of a
-- subsection of a mutable array before converting it into an immutable.
--
-- Documentation for utilized primop: `freezeArray#`.
--
-- [Unsafe] When any of the preconditions for @startIx@ or @sz@ is violated this function
-- can result in a copy of some data that doesn't belong to @srcArray@ or more likely a
-- failure with a segfault or out of memory exception.
--
-- @since 1.0.0
freezeCopyUBMArray ::
     forall e m s. (Unlift e, Primal s m)
  => UBMArray e s
  -- ^ /srcArray/ - Source mutable array
  -> Int
  -- ^ /startIx/ - Location within @srcArray@ where the copy of elements should start from
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= startIx
  --
  -- > startIx < unSize (sizeOfUBArray srcArray)
  -> Size
  -- ^ /sz/ - Size of the returned immutable array. Also this is the number of elements that
  -- will be copied over into the destionation array starting at the beginning.
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- > startIx + unSize sz < unSize (sizeOfUBArray srcArray)
  --
  -- Should be less then actual available memory
  -> m (UBArray e)
freezeCopyUBMArray marr off sz = cloneSliceUBMArray marr off sz >>= freezeUBMArray
{-# INLINE freezeCopyUBMArray #-}

-- | /O(sz)/ - Allocate a new mutable array of size @sz@ and copy that number of the
-- elements over from the @srcArray@ starting at index @ix@. Similar to `cloneSliceUBArray`,
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
cloneSliceUBMArray ::
     forall e m s. (Unlift e, Primal s m)
  => UBMArray e s
  -- ^ /srcArray/ - Source mutable array
  -> Int
  -- ^ /startIx/ - Location within @srcArray@ where the copy of elements should start from
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= startIx
  --
  -- > startIx < unSize (sizeOfUBArray srcArray)
  -> Size
  -- ^ /sz/ - Size of the returned mutable array. Also this is the number of elements that
  -- will be copied over into the destionation array starting at the beginning.
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= sz
  --
  -- > startIx + unSize sz < unSize (sizeOfUBArray srcArray)
  --
  -- Should be less then actual available memory
  -> m (UBMArray e s)
cloneSliceUBMArray marr off sz = freezeUBMArray marr >>= \arr -> thawCopyUBArray arr off sz
{-# INLINE cloneSliceUBMArray #-}
