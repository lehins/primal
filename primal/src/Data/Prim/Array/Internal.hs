{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TypeFamilyDependencies #-}
#else
{-# LANGUAGE FunctionalDependencies #-}
#endif
-- |
-- Module      : Data.Prim.Array.Internal
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Array.Internal
  ( MArray(..)
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
import GHC.Exts

newtype Size = Size Int
  deriving (Show, Eq, Ord, Num, Real, Integral, Bounded, Enum)

#if __GLASGOW_HASKELL__ >= 800
class MArray mut a where
  type IArray mut = (r :: * -> *) | r -> mut
#else
class MArray mut a where
  data IArray mut :: * -> *
#endif

  sizeOfArray :: IArray mut a -> Size

  indexArray :: IArray mut a -> Int -> a

  getSizeOfMArray :: MonadPrim s m => mut a s -> m Size

  thawArray :: MonadPrim s m => IArray mut a -> m (mut a s)

  freezeMArray :: MonadPrim s m => mut a s -> m (IArray mut a)

  newRawMArray :: MonadPrim s m => Size -> m (mut a s)

  readMArray :: MonadPrim s m => mut a s -> Int -> m a

  writeMArray :: MonadPrim s m => mut a s -> Int -> a -> m ()

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
    => IArray mut a -- ^ Source immutable array
    -> Int -- ^ Offset into the source immutable array
    -> mut a s -- ^ Destination mutable array
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
    => mut a s -- ^ Source mutable array
    -> Int -- ^ Offset into the source mutable array
    -> mut a s -- ^ Destination mutable array
    -> Int -- ^ Offset into the destination mutable array
    -> Size -- ^ Number of elements to copy over
    -> m ()

  cloneArray :: IArray mut a -> Int -> Size -> IArray mut a
  cloneArray arr i n = runST $ thawCopyArray arr i n >>= freezeMArray
  {-# INLINE cloneArray #-}

  cloneMArray :: MonadPrim s m => mut a s -> Int -> Size -> m (mut a s)
  cloneMArray ma i n = newRawMArray n >>= \mad -> mad <$ moveMArray ma i mad 0 n
  {-# INLINE cloneMArray #-}

  newMArray :: MonadPrim s m => Size -> a -> m (mut a s)
  newMArray n a = newRawMArray n >>= \ma -> ma <$ setMArray ma 0 n a
  {-# INLINE newMArray #-}

  thawCopyArray :: MonadPrim s m => IArray mut a -> Int -> Size -> m (mut a s)
  thawCopyArray a i n = newRawMArray n >>= \ma -> ma <$ copyArray a i ma 0 n
  {-# INLINE thawCopyArray #-}

  freezeCopyMArray :: MonadPrim s m => mut a s -> Int -> Size -> m (IArray mut a)
  freezeCopyMArray ma i n = newRawMArray n >>= \mad -> moveMArray ma i mad 0 n >> freezeMArray mad
  {-# INLINE freezeCopyMArray #-}

  setMArray :: MonadPrim s m => mut a s -> Int -> Size -> a -> m ()
  setMArray ma i0 (Size n0) x =
    let n = n0 + i0
        go i = when (i < n) $ writeMArray ma i x >> go (i + 1)
    in go i0
  {-# INLINE setMArray #-}




-- | Convert a list into an array strictly, i.e. each element is evaluated to WHNF prior
-- to it being written into the newly created array. In order to allocate the array ahead
-- of time, the spine of a list will be evaluated first, in order to get the total
-- number of elements. Infinite lists will cause the program to halt. On the other hand
-- if the length of a list is known ahead of time, `fromListArrayN` can be used instead as
-- optimization.
--
-- @since 0.1.0
fromListArray :: MArray mut a => [a] -> IArray mut a
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
     forall mut a. MArray mut a
  => Size -- ^ Expected @n@ size of a list
  -> [a]
  -> IArray mut a
fromListArrayN sz@(Size n) ls =
  runST $ do
    ma :: mut a s <- newRawMArray sz
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
toListArray :: MArray mut a => IArray mut a -> [a]
toListArray ba = build (\ c n -> foldrArray c n ba)
{-# INLINE toListArray #-}

-- | Strict right fold
foldrArray :: MArray mut a => (a -> b -> b) -> b -> IArray mut a -> b
foldrArray c nil a = go 0
  where
    Size k = sizeOfArray a
    go i
      | i == k = nil
      | otherwise =
        let !v = indexArray a i
         in v `c` go (i + 1)
{-# INLINE[0] foldrArray #-}

makeArray :: MArray mut a => Size -> (Int -> a) -> IArray mut a
makeArray sz f = runST $ makeArrayM sz (pure . f)
{-# INLINE makeArray #-}

makeArrayM ::
     (MArray mut a, MonadPrim s m) => Size -> (Int -> m a) -> m (IArray mut a)
makeArrayM sz@(Size n) f =
  createArrayM_ sz $ \ma ->
    let go i
          | i < n = f i >>= writeMArray ma i >> go (i + 1)
          | otherwise = pure ()
     in go 0
{-# INLINE makeArrayM #-}

createArrayM ::
     (MArray mut a, MonadPrim s m)
  => Size
  -> (mut a s -> m b)
  -> m (b, IArray mut a)
createArrayM sz f =
  newRawMArray sz >>= \ma -> f ma >>= \b -> (,) b <$> freezeMArray ma
{-# INLINE createArrayM #-}

createArrayM_ ::
     (MArray mut a, MonadPrim s m)
  => Size
  -> (mut a s -> m b)
  -> m (IArray mut a)
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
makeMArray :: (MArray mut a, MonadPrim s m) => Size -> (Int -> m a) -> m (mut a s)
makeMArray sz@(Size n) f = do
  ma <- newRawMArray sz
  let go i = when (i < n) $ f i >>= writeMArray ma i >> go (i + 1)
  ma <$ go 0
{-# INLINE makeMArray #-}

-- | Traverse an array with a monadic action.
--
-- @since 0.1.0
traverseArray ::
     (MArray mut a, MArray mut b, MonadPrim s m)
  => (a -> m b)
  -> IArray mut a
  -> m (IArray mut b)
traverseArray f a = makeArrayM (sizeOfArray a) (f . indexArray a)
{-# INLINE traverseArray #-}
