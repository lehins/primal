{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  , Size(..)
  , module Data.Prim.MArray.Internal
  ) where

import Control.Monad.ST
import Control.Prim.Monad
import Data.Prim.Memory
import Data.Prim.Memory.Addr
import Data.Prim.Memory.PArray
import Data.Prim.Memory.Bytes
import Data.Prim.MRef
import Foreign.Prim
import Data.Prim.Array (Size(..))

-- TODO: make super Container
class Array a e where
  -- | Access the size of an immutable array
  --
  -- @since 0.1.0
  sizeOfArray :: a e -> Size

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
       a e -- ^ Array to be indexed
    -> Int
    -- ^ Offset into the array
    --
    -- [Unsafe /offset/] /Unchecked precondition:/ @offset >= 0 && offset < `sizeOfArray` mut@
    -> e

  cloneArray :: a e -> Int -> Size -> a e
  -- cloneArray arr i n = runST $ thawCopyArray arr i n >>= freezeMArray
  -- {-# INLINE cloneArray #-}

-- instance Typeable p => MArray (MBytes p) where
--   type Array (MBytes p) = Bytes p

--   sizeOfArray = coerce . byteCountBytes
--   {-# INLINE sizeOfArray #-}

--   indexArray a i = indexOffBytes a (coerce i)
--   {-# INLINE indexArray #-}

--   getSizeOfMArray = fmap coerce . getByteCountMBytes
--   {-# INLINE getSizeOfMArray #-}

--   thawArray = thawBytes
--   {-# INLINE thawArray #-}

--   freezeMArray = freezeMBytes
--   {-# INLINE freezeMArray #-}

--   newRawMArray n = allocMutMem (coerce n :: Count Word8)
--   {-# INLINE newRawMArray #-}

--   writeMArray mb i = writeOffMBytes mb (coerce i)
--   {-# INLINE writeMArray #-}

--   readMArray mb i = readOffMBytes mb (coerce i)
--   {-# INLINE readMArray #-}

--   copyArray as os mbd od n =
--     copyBytesToMBytes as (coerce os) mbd (coerce od) (coerce n :: Count Word8)
--   {-# INLINE copyArray #-}

--   moveMArray mbs os mbd od n =
--     moveMBytesToMBytes mbs (coerce os) mbd (coerce od) (coerce n :: Count Word8)
--   {-# INLINE moveMArray #-}

--   setMArray mb i sz = setMBytes mb (coerce i) (coerce sz)
--   {-# INLINE setMArray #-}

--   shrinkMArray mb sz = mb <$ shrinkMBytes mb (coerce sz :: Count Word8)
--   {-# INLINE shrinkMArray #-}

--   resizeMArray mb sz = reallocMBytes mb (coerce sz :: Count Word8)
--   {-# INLINE resizeMArray #-}


-- instance Prim e => MArray (MAddr e) where
--   type Array (MAddr e) = Addr e

--   sizeOfArray = coerce . countAddr
--   {-# INLINE sizeOfArray #-}

--   indexArray a i = indexOffAddr a (coerce i)
--   {-# INLINE indexArray #-}

--   getSizeOfMArray = fmap coerce . getCountMAddr
--   {-# INLINE getSizeOfMArray #-}

--   thawArray = thawAddr
--   {-# INLINE thawArray #-}

--   freezeMArray = freezeMAddr
--   {-# INLINE freezeMArray #-}

--   newRawMArray = allocMAddr . coerce
--   {-# INLINE newRawMArray #-}

--   writeMArray ma i = writeOffMAddr ma (coerce i)
--   {-# INLINE writeMArray #-}

--   readMArray ma i = readOffMAddr ma (coerce i)
--   {-# INLINE readMArray #-}

--   copyArray as os mad od n = copyAddrToMAddr as (coerce os) mad (coerce od) (coerce n)
--   {-# INLINE copyArray #-}

--   moveMArray mas os mad od n = moveMAddrToMAddr mas (coerce os) mad (coerce od) (coerce n)
--   {-# INLINE moveMArray #-}

--   setMArray ma i sz = setMAddr ma (coerce i) (coerce sz)
--   {-# INLINE setMArray #-}

--   shrinkMArray ma sz = ma <$ shrinkMAddr ma (coerce sz :: Count e)
--   {-# INLINE shrinkMArray #-}

--   resizeMArray ma sz = reallocMAddr ma (coerce sz :: Count e)
--   {-# INLINE resizeMArray #-}



-- instance (Typeable p, Prim e) => MArray (PMArray p e) where
--   type Array (PMArray p e) = PArray p e

--   sizeOfArray = sizePArray
--   {-# INLINE sizeOfArray #-}

--   indexArray a i = indexOffMem a (coerce i)
--   {-# INLINE indexArray #-}

--   getSizeOfMArray = getSizePMArray
--   {-# INLINE getSizeOfMArray #-}

--   thawArray = thawPArray
--   {-# INLINE thawArray #-}

--   freezeMArray = freezePMArray
--   {-# INLINE freezeMArray #-}

--   newRawMArray = allocPMArray
--   {-# INLINE newRawMArray #-}

--   writeMArray = writePMArray
--   {-# INLINE writeMArray #-}

--   readMArray = readPMArray
--   {-# INLINE readMArray #-}

--   copyArray = copyPArrayToPMArray
--   {-# INLINE copyArray #-}

--   moveMArray = movePMArrayToPMArray
--   {-# INLINE moveMArray #-}

--   setMArray = setPMArray
--   {-# INLINE setMArray #-}

--   shrinkMArray ma sz = ma <$ shrinkPMArray ma sz
--   {-# INLINE shrinkMArray #-}

--   resizeMArray = reallocPMArray
--   {-# INLINE resizeMArray #-}

-- -- | Convert a list into an array strictly, i.e. each element is evaluated to WHNF prior
-- -- to it being written into the newly created array. In order to allocate the array ahead
-- -- of time, the spine of a list will be evaluated first, in order to get the total
-- -- number of elements. Infinite lists will cause the program to halt. On the other hand
-- -- if the length of a list is known ahead of time, `fromListArrayN` can be used instead as
-- -- optimization.
-- --
-- -- @since 0.1.0
-- fromListArray :: MArray mut => [Elt mut] -> Array mut
-- fromListArray xs = fromListArrayN (Size (length xs)) xs
-- {-# INLINE fromListArray #-}



-- -- | Same as `fromListArray`, except it will allocate an array exactly of @n@ size, as
-- -- such it will not convert any portion of the list that doesn't fit into the newly
-- -- created array.
-- --
-- -- [Unsafe size] if the length of supplied list is actually smaller then the expected
-- -- size, thunks with `UndefinedElement` will be left in the tail of the array.
-- --
-- -- ====__Examples__
-- --
-- -- >>> fromListArrayN 3 [1 :: Int, 2, 3]
-- -- Array [1,2,3]
-- -- >>> fromListArrayN 3 [1 :: Int ..]
-- -- Array [1,2,3]
-- -- >>> fromListArrayN 3 [1 :: Int, 2]
-- -- Array [1,2*** Exception: undefined array element: Data.Prim.Array.Boxed.uninitialized
-- --
-- -- @since 0.1.0
-- fromListArrayN ::
--      forall mut. MArray mut
--   => Size -- ^ Expected @n@ size of a list
--   -> [Elt mut]
--   -> Array mut
-- fromListArrayN sz@(Size n) ls =
--   runST $ do
--     ma :: mut s <- newRawMArray sz
--     let go i =
--           \case
--             x:xs
--               | i < n -> writeMArray ma i x >> go (i + 1) xs
--             _ -> pure ()
--     go 0 ls
--     freezeMArray ma

-- -- | Convert a pure boxed array into a list. It should work fine with GHC built-in list
-- -- fusion.
-- --
-- -- @since 0.1.0
-- toListArray :: MArray mut => Array mut -> [Elt mut]
-- toListArray ba = build (\ c n -> foldrArray c n ba)
-- {-# INLINE toListArray #-}

-- -- | Strict right fold
-- foldrArray :: MArray mut => (Elt mut -> b -> b) -> b -> Array mut -> b
-- foldrArray c nil a = go 0
--   where
--     Size k = sizeOfArray a
--     go i
--       | i == k = nil
--       | otherwise =
--         let !v = indexArray a i
--          in v `c` go (i + 1)
-- {-# INLINE[0] foldrArray #-}

-- makeArray :: MArray mut => Size -> (Int -> Elt mut) -> Array mut
-- makeArray sz f = runST $ makeArrayM sz (pure . f)
-- {-# INLINE makeArray #-}

-- makeArrayM ::
--      (MArray mut, MonadPrim s m) => Size -> (Int -> m (Elt mut)) -> m (Array mut)
-- makeArrayM sz@(Size n) f =
--   createArrayM_ sz $ \ma ->
--     let go i
--           | i < n = f i >>= writeMArray ma i >> go (i + 1)
--           | otherwise = pure ()
--      in go 0
-- {-# INLINE makeArrayM #-}

-- createArrayM ::
--      (MArray mut, MonadPrim s m)
--   => Size
--   -> (mut s -> m b)
--   -> m (b, Array mut)
-- createArrayM sz f =
--   newRawMArray sz >>= \ma -> f ma >>= \b -> (,) b <$> freezeMArray ma
-- {-# INLINE createArrayM #-}

-- createArrayM_ ::
--      (MArray mut, MonadPrim s m)
--   => Size
--   -> (mut s -> m b)
--   -> m (Array mut)
-- createArrayM_ sz f =
--   newRawMArray sz >>= \ma -> f ma >> freezeMArray ma
-- {-# INLINE createArrayM_ #-}


-- createArrayST :: MArray mut => Size -> (forall s. mut s -> ST s b) -> (b, Array mut)
-- createArrayST sz f = runST $ createArrayM sz f
-- {-# INLINE createArrayST #-}

-- createArrayST_ :: MArray mut => Size -> (forall s. mut s -> ST s b) -> Array mut
-- createArrayST_ sz f = runST $ createArrayM_ sz f
-- {-# INLINE createArrayST_ #-}


-- -- | Create a new mutable array of a supplied size by applying a monadic action to indices
-- -- of each one of the new elements.
-- --
-- -- [Unsafe size] Negative or too large of an array size can kill the current thread with
-- -- `HeapOverflow` asynchronous exception.
-- --
-- -- ====__Examples__
-- --
-- -- >>> import Control.Monad ((>=>))
-- -- >>> import Data.Prim.Ref
-- -- >>> ref <- newRef "Numbers: "
-- -- >>> ma <- makeMArray 5 $ \i -> modifyFetchRef ref (\cur -> cur ++ show i ++ ",")
-- -- >>> mapM_ (readMArray ma >=> putStrLn) [0 .. 4]
-- -- Numbers: 0,
-- -- Numbers: 0,1,
-- -- Numbers: 0,1,2,
-- -- Numbers: 0,1,2,3,
-- -- Numbers: 0,1,2,3,4,
-- --
-- -- @since 0.1.0
-- makeMArray :: (MArray mut, MonadPrim s m) => Size -> (Int -> m (Elt mut)) -> m (mut s)
-- makeMArray sz@(Size n) f = do
--   ma <- newRawMArray sz
--   let go i = when (i < n) $ f i >>= writeMArray ma i >> go (i + 1)
--   ma <$ go 0
-- {-# INLINE makeMArray #-}



-- -- | Traverse an array with a monadic action.
-- --
-- -- @since 0.1.0
-- traverseArray ::
--      (MArray mut, MArray mut', MonadPrim s m)
--   => (Elt mut -> m (Elt mut'))
--   -> Array mut
--   -> m (Array mut')
-- traverseArray f a = makeArrayM (sizeOfArray a) (f . indexArray a)
-- {-# INLINE traverseArray #-}


