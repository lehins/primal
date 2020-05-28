{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE Unsafe #-}
-- |
-- Module      : Data.Prim.MArray.Internal
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.MArray.Internal
  ( Elt
  , module Data.Prim.MArray.Internal
  ) where

import Control.Monad.ST
import Control.Prim.Monad
import Data.Prim.Memory
import Data.Prim.Memory.Addr
import Data.Prim.Memory.ByteArray
import Data.Prim.Memory.Bytes
import Data.Prim.MRef
import Foreign.Prim


class MRef mut => MArray mut where
  type Array mut = (frozen :: *) | frozen -> mut

  -- | Access the size of an immutable array
  --
  -- @since 0.1.0
  sizeOfArray :: Array mut -> Size

  -- | Read an element from an immutable array
  --
  -- ====__Examples__
  --
  -- >>> :set -XOverloadedStrings
  -- >>> import Data.Prim.Memory.Addr
  -- >>> a = "Haskell arrays" :: Addr Char
  -- >>> a
  -- "Haskell arrays"
  -- >>> indexArray a 3
  -- 'k'
  --
  -- @since 0.1.0
  indexArray ::
       Array mut -- ^ Array to be indexed
    -> Int
    -- ^ Offset into the array
    --
    -- [Unsafe /offset/] /Unchecked precondition:/ @offset >= 0 && offset < `sizeOfArray` mut@
    -> Elt mut

  -- | Get the size of a mutable array. Unlike `sizeOfArray` it is a monadic operation
  -- because some mutable arrays support in place resizing.
  --
  -- @since 0.1.0
  getSizeOfMArray :: MonadPrim s m => mut s -> m Size

  -- | Convert an immutable array into the matching mutable array.
  --
  -- ====__Examples__
  --
  -- In the example below it is safe to thaw the original immutable array and mutate it
  -- afterwards because we do not keep around the reference to it.
  --
  -- >>> :set -XOverloadedStrings
  -- >>> import Data.Prim.Memory.Addr
  -- >>> ma <- thawArray ("A whole bread" :: Addr Char)
  -- >>> writeMArray ma 4 'a'
  -- >>> writeMArray ma 11 'e'
  -- >>> freezeMArray ma
  -- "A whale breed"
  --
  -- @since 0.1.0
  thawArray :: MonadPrim s m =>
       Array mut -- ^ Immutable array to thaw
    -> m (mut s)
    -- ^ Thawed mutable array. Any mutation will also affect the source immutable array
    --
    -- [Unsafe /mutable array/] Allows to break referential transparency.

  -- | Convert a mutable array into the matching immutable array.
  --
  -- @since 0.1.0
  freezeMArray :: MonadPrim s m =>
       mut s
    -- ^ Mutable array to freeze. Any further mutation will also affect the immutable
    -- array
    --
    -- [Unsafe /mutable array/] Allows to break referential transparency.
    -> m (Array mut)

  newRawMArray :: MonadPrim s m => Size -> m (mut s)

  -- | Read an element from a mutable array
  --
  -- ====__Examples__
  --
  -- >>> import Data.Prim.Memory.Addr
  -- >>> callocMAddr (Count 5 :: Count Int)
  -- >>> ma <- callocMAddr (Count 5 :: Count Int)
  -- >>> readMArray ma 2
  -- 0
  -- >>> writeMArray ma 2 99
  -- >>> readMArray ma 2
  -- 99
  --
  -- @since 0.1.0
  readMArray :: MonadPrim s m =>
       mut s -- ^ Array to read element from
    -> Int
    -- ^ Offset into the array
    --
    -- [Unsafe /offset/] /Unchecked precondition:/ @offset >= 0 && offset < `getSizeOfMArray` mut@
    -> m (Elt mut)

  -- | Write an element into a mutable array
  --
  -- @since 0.1.0
  writeMArray :: MonadPrim s m =>
       mut s -- ^ Array to write an element into
    -> Int
    -- ^ Offset into the array
    --
    -- [Unsafe /offset/] /Unchecked precondition:/ @offset >= 0 && offset < `getSizeOfMArray` mut@
    -> Elt mut -- ^ Element to be written
    -> m ()

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
  copyArray ::
       MonadPrim s m
    => Array mut -- ^ Source immutable array
    -> Int -- ^ Offset into the source immutable array
    -> mut s -- ^ Destination mutable array
    -> Int -- ^ Offset into the destination mutable array
    -> Size -- ^ Number of elements to copy over
    -> m ()


  -- | Copy a subsection of a mutable array into a subsection of another or the same
  -- mutable array. Therefore, unlike `copyArray`, memory overlap is allowed.
  --
  -- [Unsafe offset] Each offset cannot be negative or larger than the size of a
  -- corresponding array, otherwise it can result in an unchecked exception
  --
  -- [Unsafe new size] Number of elements to be copied cannot be larger than the size of an
  -- each array minus their corersponding offsets.
  --
  -- @since 0.1.0
  moveMArray ::
       MonadPrim s m
    => mut s -- ^ Source mutable array
    -> Int -- ^ Offset into the source mutable array
    -> mut s -- ^ Destination mutable array
    -> Int -- ^ Offset into the destination mutable array
    -> Size -- ^ Number of elements to copy over
    -> m ()

  cloneArray :: Array mut -> Int -> Size -> Array mut
  cloneArray arr i n = runST $ thawCopyArray arr i n >>= freezeMArray
  {-# INLINE cloneArray #-}

  cloneMArray :: MonadPrim s m => mut s -> Int -> Size -> m (mut s)
  cloneMArray ma i n = newRawMArray n >>= \mad -> mad <$ moveMArray ma i mad 0 n
  {-# INLINE cloneMArray #-}

  newMArray :: MonadPrim s m => Size -> Elt mut -> m (mut s)
  newMArray n a = newRawMArray n >>= \ma -> ma <$ setMArray ma 0 n a
  {-# INLINE newMArray #-}

  thawCopyArray :: MonadPrim s m => Array mut -> Int -> Size -> m (mut s)
  thawCopyArray a i n = newRawMArray n >>= \ma -> ma <$ copyArray a i ma 0 n
  {-# INLINE thawCopyArray #-}

  freezeCopyMArray :: MonadPrim s m => mut s -> Int -> Size -> m (Array mut)
  freezeCopyMArray ma i n = newRawMArray n >>= \mad -> moveMArray ma i mad 0 n >> freezeMArray mad
  {-# INLINE freezeCopyMArray #-}

  setMArray :: MonadPrim s m => mut s -> Int -> Size -> Elt mut -> m ()
  setMArray ma i0 (Size n0) x =
    let n = n0 + i0
        go i = when (i < n) $ writeMArray ma i x >> go (i + 1)
    in go i0
  {-# INLINE setMArray #-}

  shrinkMArray :: MonadPrim s m => mut s -> Size -> m (mut s)
  shrinkMArray ma sz = cloneMArray ma 0 sz
  {-# INLINE shrinkMArray #-}

  resizeMArray :: MonadPrim s m => mut s -> Size -> m (mut s)
  resizeMArray ma sz = do
    a <- freezeMArray ma
    ma' <- newRawMArray sz
    ma' <$ copyArray a 0 ma' 0 sz
  {-# INLINE resizeMArray #-}


instance Typeable p => MArray (MBytes p) where
  type Array (MBytes p) = Bytes p

  sizeOfArray = coerce . byteCountBytes
  {-# INLINE sizeOfArray #-}

  indexArray a i = indexOffBytes a (coerce i)
  {-# INLINE indexArray #-}

  getSizeOfMArray = fmap coerce . getByteCountMBytes
  {-# INLINE getSizeOfMArray #-}

  thawArray = thawBytes
  {-# INLINE thawArray #-}

  freezeMArray = freezeMBytes
  {-# INLINE freezeMArray #-}

  newRawMArray n = allocByteCountMem (coerce n)
  {-# INLINE newRawMArray #-}

  writeMArray mb i = writeOffMBytes mb (coerce i)
  {-# INLINE writeMArray #-}

  readMArray mb i = readOffMBytes mb (coerce i)
  {-# INLINE readMArray #-}

  copyArray as os mbd od n =
    copyBytesToMBytes as (coerce os) mbd (coerce od) (coerce n :: Count Word8)
  {-# INLINE copyArray #-}

  moveMArray mbs os mbd od n =
    moveMBytesToMBytes mbs (coerce os) mbd (coerce od) (coerce n :: Count Word8)
  {-# INLINE moveMArray #-}

  setMArray mb i sz = setMBytes mb (coerce i) (coerce sz)
  {-# INLINE setMArray #-}

  shrinkMArray mb sz = mb <$ shrinkMBytes mb (coerce sz :: Count Word8)
  {-# INLINE shrinkMArray #-}

  resizeMArray mb sz = reallocMBytes mb (coerce sz :: Count Word8)
  {-# INLINE resizeMArray #-}


instance Prim e => MArray (MAddr e) where
  type Array (MAddr e) = Addr e

  sizeOfArray = coerce . countAddr
  {-# INLINE sizeOfArray #-}

  indexArray a i = indexOffAddr a (coerce i)
  {-# INLINE indexArray #-}

  getSizeOfMArray = fmap coerce . getCountMAddr
  {-# INLINE getSizeOfMArray #-}

  thawArray = thawAddr
  {-# INLINE thawArray #-}

  freezeMArray = freezeMAddr
  {-# INLINE freezeMArray #-}

  newRawMArray = allocMAddr . coerce
  {-# INLINE newRawMArray #-}

  writeMArray ma i = writeOffMAddr ma (coerce i)
  {-# INLINE writeMArray #-}

  readMArray ma i = readOffMAddr ma (coerce i)
  {-# INLINE readMArray #-}

  copyArray as os mad od n = copyAddrToMAddr as (coerce os) mad (coerce od) (coerce n)
  {-# INLINE copyArray #-}

  moveMArray mas os mad od n = moveMAddrToMAddr mas (coerce os) mad (coerce od) (coerce n)
  {-# INLINE moveMArray #-}

  setMArray ma i sz = setMAddr ma (coerce i) (coerce sz)
  {-# INLINE setMArray #-}

  shrinkMArray ma sz = ma <$ shrinkMAddr ma (coerce sz :: Count e)
  {-# INLINE shrinkMArray #-}

  resizeMArray ma sz = reallocMAddr ma (coerce sz :: Count e)
  {-# INLINE resizeMArray #-}



instance (Typeable p, Prim e) => MArray (MByteArray p e) where
  type Array (MByteArray p e) = ByteArray p e

  sizeOfArray = sizeByteArray
  {-# INLINE sizeOfArray #-}

  indexArray a i = indexOffMem a (coerce i)
  {-# INLINE indexArray #-}

  getSizeOfMArray = getSizeMByteArray
  {-# INLINE getSizeOfMArray #-}

  thawArray = thawByteArray
  {-# INLINE thawArray #-}

  freezeMArray = freezeMByteArray
  {-# INLINE freezeMArray #-}

  newRawMArray = allocMByteArray
  {-# INLINE newRawMArray #-}

  writeMArray = writeMByteArray
  {-# INLINE writeMArray #-}

  readMArray = readMByteArray
  {-# INLINE readMArray #-}

  copyArray = copyByteArrayToMByteArray
  {-# INLINE copyArray #-}

  moveMArray = moveMByteArrayToMByteArray
  {-# INLINE moveMArray #-}

  setMArray = setMByteArray
  {-# INLINE setMArray #-}

  shrinkMArray ma sz = ma <$ shrinkMByteArray ma sz
  {-# INLINE shrinkMArray #-}

  resizeMArray = reallocMByteArray
  {-# INLINE resizeMArray #-}

-- | Convert a list into an array strictly, i.e. each element is evaluated to WHNF prior
-- to it being written into the newly created array. In order to allocate the array ahead
-- of time, the spine of a list will be evaluated first, in order to get the total
-- number of elements. Infinite lists will cause the program to halt. On the other hand
-- if the length of a list is known ahead of time, `fromListArrayN` can be used instead as
-- optimization.
--
-- @since 0.1.0
fromListArray :: MArray mut => [Elt mut] -> Array mut
fromListArray xs = fromListArrayN (Size (length xs)) xs
{-# INLINE fromListArray #-}



-- | Same as `fromListArray`, except it will allocate an array exactly of @n@ size, as
-- such it will not convert any portion of the list that doesn't fit into the newly
-- created array.
--
-- [Unsafe size] if the length of supplied list is actually smaller then the expected
-- size, thunks with `UndefinedElement` will be left in the tail of the array.
--
-- ====__Examples__
--
-- >>> fromListArrayN 3 [1 :: Int, 2, 3]
-- Array [1,2,3]
-- >>> fromListArrayN 3 [1 :: Int ..]
-- Array [1,2,3]
-- >>> fromListArrayN 3 [1 :: Int, 2]
-- Array [1,2*** Exception: undefined array element: Data.Prim.Array.Boxed.uninitialized
--
-- @since 0.1.0
fromListArrayN ::
     forall mut. MArray mut
  => Size -- ^ Expected @n@ size of a list
  -> [Elt mut]
  -> Array mut
fromListArrayN sz@(Size n) ls =
  runST $ do
    ma :: mut s <- newRawMArray sz
    let go i =
          \case
            x:xs
              | i < n -> writeMArray ma i x >> go (i + 1) xs
            _ -> pure ()
    go 0 ls
    freezeMArray ma

-- | Convert a pure boxed array into a list. It should work fine with GHC built-in list
-- fusion.
--
-- @since 0.1.0
toListArray :: MArray mut => Array mut -> [Elt mut]
toListArray ba = build (\ c n -> foldrArray c n ba)
{-# INLINE toListArray #-}

-- | Strict right fold
foldrArray :: MArray mut => (Elt mut -> b -> b) -> b -> Array mut -> b
foldrArray c nil a = go 0
  where
    Size k = sizeOfArray a
    go i
      | i == k = nil
      | otherwise =
        let !v = indexArray a i
         in v `c` go (i + 1)
{-# INLINE[0] foldrArray #-}

makeArray :: MArray mut => Size -> (Int -> Elt mut) -> Array mut
makeArray sz f = runST $ makeArrayM sz (pure . f)
{-# INLINE makeArray #-}

makeArrayM ::
     (MArray mut, MonadPrim s m) => Size -> (Int -> m (Elt mut)) -> m (Array mut)
makeArrayM sz@(Size n) f =
  createArrayM_ sz $ \ma ->
    let go i
          | i < n = f i >>= writeMArray ma i >> go (i + 1)
          | otherwise = pure ()
     in go 0
{-# INLINE makeArrayM #-}

createArrayM ::
     (MArray mut, MonadPrim s m)
  => Size
  -> (mut s -> m b)
  -> m (b, Array mut)
createArrayM sz f =
  newRawMArray sz >>= \ma -> f ma >>= \b -> (,) b <$> freezeMArray ma
{-# INLINE createArrayM #-}

createArrayM_ ::
     (MArray mut, MonadPrim s m)
  => Size
  -> (mut s -> m b)
  -> m (Array mut)
createArrayM_ sz f =
  newRawMArray sz >>= \ma -> f ma >> freezeMArray ma
{-# INLINE createArrayM_ #-}


createArrayST :: MArray mut => Size -> (forall s. mut s -> ST s b) -> (b, Array mut)
createArrayST sz f = runST $ createArrayM sz f
{-# INLINE createArrayST #-}

createArrayST_ :: MArray mut => Size -> (forall s. mut s -> ST s b) -> Array mut
createArrayST_ sz f = runST $ createArrayM_ sz f
{-# INLINE createArrayST_ #-}


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
-- >>> ma <- makeMArray 5 $ \i -> modifyFetchRef ref (\cur -> cur ++ show i ++ ",")
-- >>> mapM_ (readMArray ma >=> putStrLn) [0 .. 4]
-- Numbers: 0,
-- Numbers: 0,1,
-- Numbers: 0,1,2,
-- Numbers: 0,1,2,3,
-- Numbers: 0,1,2,3,4,
--
-- @since 0.1.0
makeMArray :: (MArray mut, MonadPrim s m) => Size -> (Int -> m (Elt mut)) -> m (mut s)
makeMArray sz@(Size n) f = do
  ma <- newRawMArray sz
  let go i = when (i < n) $ f i >>= writeMArray ma i >> go (i + 1)
  ma <$ go 0
{-# INLINE makeMArray #-}

-- | Traverse an array with a monadic action.
--
-- @since 0.1.0
traverseArray ::
     (MArray mut, MArray mut', MonadPrim s m)
  => (Elt mut -> m (Elt mut'))
  -> Array mut
  -> m (Array mut')
traverseArray f a = makeArrayM (sizeOfArray a) (f . indexArray a)
{-# INLINE traverseArray #-}


