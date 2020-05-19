{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
-- |
-- Module      : Data.Prim.Array.Internal
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Array.Internal
  ( Mutable(..)
  , Size(..)
  , toListArray
  , fromListArray
  , fromListArrayN
  , foldrArray
  , makeArray
  , makeArrayM
  , createArrayM
  , createArrayM_
  , makeMArray
  , traverseArray
  ) where

import Control.Monad.ST
import Control.Prim.Monad
import Data.Prim.Memory
import Data.Prim.Memory.Addr
import Data.Prim.Memory.ByteArray
import Data.Prim.Memory.Bytes
import GHC.Exts

class Mutable mut where
  type Frozen mut = (r :: *) | r -> mut
  type Elt mut :: *

  sizeOfArray :: Frozen mut -> Size

  indexArray :: Frozen mut -> Int -> Elt mut

  getSizeOfMArray :: MonadPrim s m => mut s -> m Size

  thawArray :: MonadPrim s m => Frozen mut -> m (mut s)

  freezeMArray :: MonadPrim s m => mut s -> m (Frozen mut)

  newRawMArray :: MonadPrim s m => Size -> m (mut s)

  readMArray :: MonadPrim s m => mut s -> Int -> m (Elt mut)

  writeMArray :: MonadPrim s m => mut s -> Int -> Elt mut -> m ()

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
    => Frozen mut -- ^ Source immutable array
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

  cloneArray :: Frozen mut -> Int -> Size -> Frozen mut
  cloneArray arr i n = runST $ thawCopyArray arr i n >>= freezeMArray
  {-# INLINE cloneArray #-}

  cloneMArray :: MonadPrim s m => mut s -> Int -> Size -> m (mut s)
  cloneMArray ma i n = newRawMArray n >>= \mad -> mad <$ moveMArray ma i mad 0 n
  {-# INLINE cloneMArray #-}

  newMArray :: MonadPrim s m => Size -> Elt mut -> m (mut s)
  newMArray n a = newRawMArray n >>= \ma -> ma <$ setMArray ma 0 n a
  {-# INLINE newMArray #-}

  thawCopyArray :: MonadPrim s m => Frozen mut -> Int -> Size -> m (mut s)
  thawCopyArray a i n = newRawMArray n >>= \ma -> ma <$ copyArray a i ma 0 n
  {-# INLINE thawCopyArray #-}

  freezeCopyMArray :: MonadPrim s m => mut s -> Int -> Size -> m (Frozen mut)
  freezeCopyMArray ma i n = newRawMArray n >>= \mad -> moveMArray ma i mad 0 n >> freezeMArray mad
  {-# INLINE freezeCopyMArray #-}

  setMArray :: MonadPrim s m => mut s -> Int -> Size -> Elt mut -> m ()
  setMArray ma i0 (Size n0) x =
    let n = n0 + i0
        go i = when (i < n) $ writeMArray ma i x >> go (i + 1)
    in go i0
  {-# INLINE setMArray #-}


instance Typeable p => Mutable (MBytes p) where
  type Frozen (MBytes p) = Bytes p
  type Elt (MBytes p) = Word8

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

  newRawMArray n = allocMBytes (coerce n :: Count Word8)
  {-# INLINE newRawMArray #-}

  writeMArray ma i = writeOffMBytes ma (coerce i)
  {-# INLINE writeMArray #-}

  readMArray ma i = readOffMBytes ma (coerce i)
  {-# INLINE readMArray #-}

  copyArray as os mad od n =
    copyBytesToMBytes as (coerce os) mad (coerce od) (coerce n :: Count Word8)
  {-# INLINE copyArray #-}

  moveMArray mas os mad od n =
    moveMBytesToMBytes mas (coerce os) mad (coerce od) (coerce n :: Count Word8)
  {-# INLINE moveMArray #-}

  setMArray ma i sz = setMBytes ma (coerce i) (coerce sz)
  {-# INLINE setMArray #-}


instance Prim e => Mutable (MAddr e) where
  type Frozen (MAddr e) = Addr e
  type Elt (MAddr e) = e

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


instance (Typeable p, Prim e) => Mutable (MByteArray p e) where
  type Frozen (MByteArray p e) = ByteArray p e
  type Elt (MByteArray p e) = e

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



-- | Convert a list into an array strictly, i.e. each element is evaluated to WHNF prior
-- to it being written into the newly created array. In order to allocate the array ahead
-- of time, the spine of a list will be evaluated first, in order to get the total
-- number of elements. Infinite lists will cause the program to halt. On the other hand
-- if the length of a list is known ahead of time, `fromListArrayN` can be used instead as
-- optimization.
--
-- @since 0.1.0
fromListArray :: Mutable mut => [Elt mut] -> Frozen mut
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
     forall mut. Mutable mut
  => Size -- ^ Expected @n@ size of a list
  -> [Elt mut]
  -> Frozen mut
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
toListArray :: Mutable mut => Frozen mut -> [Elt mut]
toListArray ba = build (\ c n -> foldrArray c n ba)
{-# INLINE toListArray #-}

-- | Strict right fold
foldrArray :: Mutable mut => (Elt mut -> b -> b) -> b -> Frozen mut -> b
foldrArray c nil a = go 0
  where
    Size k = sizeOfArray a
    go i
      | i == k = nil
      | otherwise =
        let !v = indexArray a i
         in v `c` go (i + 1)
{-# INLINE[0] foldrArray #-}

makeArray :: Mutable mut => Size -> (Int -> Elt mut) -> Frozen mut
makeArray sz f = runST $ makeArrayM sz (pure . f)
{-# INLINE makeArray #-}

makeArrayM ::
     (Mutable mut, MonadPrim s m) => Size -> (Int -> m (Elt mut)) -> m (Frozen mut)
makeArrayM sz@(Size n) f =
  createArrayM_ sz $ \ma ->
    let go i
          | i < n = f i >>= writeMArray ma i >> go (i + 1)
          | otherwise = pure ()
     in go 0
{-# INLINE makeArrayM #-}

createArrayM ::
     (Mutable mut, MonadPrim s m)
  => Size
  -> (mut s -> m b)
  -> m (b, Frozen mut)
createArrayM sz f =
  newRawMArray sz >>= \ma -> f ma >>= \b -> (,) b <$> freezeMArray ma
{-# INLINE createArrayM #-}

createArrayM_ ::
     (Mutable mut, MonadPrim s m)
  => Size
  -> (mut s -> m b)
  -> m (Frozen mut)
createArrayM_ sz f =
  newRawMArray sz >>= \ma -> f ma >> freezeMArray ma
{-# INLINE createArrayM_ #-}


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
makeMArray :: (Mutable mut, MonadPrim s m) => Size -> (Int -> m (Elt mut)) -> m (mut s)
makeMArray sz@(Size n) f = do
  ma <- newRawMArray sz
  let go i = when (i < n) $ f i >>= writeMArray ma i >> go (i + 1)
  ma <$ go 0
{-# INLINE makeMArray #-}

-- | Traverse an array with a monadic action.
--
-- @since 0.1.0
traverseArray ::
     (Mutable mut, Mutable mut', MonadPrim s m)
  => (Elt mut -> m (Elt mut'))
  -> Frozen mut
  -> m (Frozen mut')
traverseArray f a = makeArrayM (sizeOfArray a) (f . indexArray a)
{-# INLINE traverseArray #-}
