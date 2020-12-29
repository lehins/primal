{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE Unsafe #-}
-- |
-- Module      : Primal.Container.Mutable.Array.Internal
-- Copyright   : (c) Alexey Kuleshevich 2020-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Container.Mutable.Array.Internal
  ( Size(..)
  , module Primal.Container.Mutable.Array.Internal
  ) where

import Control.Monad.ST
import Primal.Container.Mutable.Ref.Internal
import Primal.Data.Array
import Primal.Foreign
import Primal.Memory
import Primal.Memory.Addr
import Primal.Memory.PArray
import Primal.Monad

type family Frozen (mutable :: * -> * -> *) = (frozen :: * -> *) | frozen -> mutable


class MRef ma e => MArray ma e where

  -- | Access the size of an immutable array
  --
  -- @since 0.1.0
  sizeOfArray :: Frozen ma e -> Size

  -- | Read an element from an immutable array
  --
  -- ====__Examples__
  --
  -- >>> :set -XOverloadedStrings
  -- >>> import Primal.Memory.Addr
  -- >>> a = "Haskell arrays" :: Addr Char
  -- >>> a
  -- "Haskell arrays"
  -- >>> indexArray a 3
  -- 'k'
  --
  -- @since 0.1.0
  indexArray ::
       Frozen ma e -- ^ Array to be indexed
    -> Int
    -- ^ Offset into the array
    --
    -- [Unsafe /offset/] /Unchecked precondition:/ @offset >= 0 && offset < `sizeOfArray` mut@
    -> e

  -- | Get the size of a mutable array. Unlike `sizeOfArray` it is a monadic operation
  -- because mutable arrays support in place resizing.
  --
  -- @since 0.1.0
  getSizeOfMArray :: MonadPrim s m => ma e s -> m Size

  -- | Convert an immutable array into the matching mutable array.
  --
  -- ====__Examples__
  --
  -- In the example below it is safe to thaw the original immutable array and mutate it
  -- afterwards because we do not keep around the reference to it.
  --
  -- >>> :set -XOverloadedStrings
  -- >>> import Primal.Memory.Addr
  -- >>> ma <- thawArray ("A whole bread" :: Addr Char)
  -- >>> writeMArray ma 4 'a'
  -- >>> writeMArray ma 11 'e'
  -- >>> freezeMArray ma
  -- "A whale breed"
  --
  -- @since 0.1.0
  thawArray :: MonadPrim s m =>
       Frozen ma e -- ^ Immutable array to thaw
    -> m (ma e s)
    -- ^ Thawed mutable array. Any mutation will also affect the source immutable array
    --
    -- [Unsafe /mutable array/] Allows to break referential transparency.

  -- | Convert a mutable array into the matching immutable array.
  --
  -- @since 0.1.0
  freezeMArray :: MonadPrim s m =>
       ma e s
    -- ^ Mutable array to freeze. Any further mutation will also affect the immutable
    -- array
    --
    -- [Unsafe /mutable array/] Allows to break referential transparency.
    -> m (Frozen ma e)

  newRawMArray :: MonadPrim s m => Size -> m (ma e s)

  -- | Read an element from a mutable array
  --
  -- ====__Examples__
  --
  -- >>> import Primal.Memory.Addr
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
       ma e s -- ^ Array to read element from
    -> Int
    -- ^ Offset into the array
    --
    -- [Unsafe /offset/] /Unchecked precondition:/ @offset >= 0 && offset < `getSizeOfMArray` mut@
    -> m e

  -- | Write an element into a mutable array
  --
  -- @since 0.1.0
  writeMArray :: MonadPrim s m =>
       ma e s -- ^ Array to write an element into
    -> Int
    -- ^ Offset into the array
    --
    -- [Unsafe /offset/] /Unchecked precondition:/ @offset >= 0 && offset < `getSizeOfMArray` mut@
    -> e -- ^ Element to be written
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
    => Frozen ma e -- ^ Source immutable array
    -> Int -- ^ Offset into the source immutable array
    -> ma e s -- ^ Destination mutable array
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
    => ma e s -- ^ Source mutable array
    -> Int -- ^ Offset into the source mutable array
    -> ma e s -- ^ Destination mutable array
    -> Int -- ^ Offset into the destination mutable array
    -> Size -- ^ Number of elements to copy over
    -> m ()

  cloneArray :: Frozen ma e -> Int -> Size -> Frozen ma e
  cloneArray arr i n = runST $ thawCopyArray arr i n >>= freezeMArray
  {-# INLINE cloneArray #-}

  cloneMArray :: MonadPrim s m => ma e s -> Int -> Size -> m (ma e s)
  cloneMArray ma i n = newRawMArray n >>= \mad -> mad <$ moveMArray ma i mad 0 n
  {-# INLINE cloneMArray #-}

  newMArray :: MonadPrim s m => Size -> e -> m (ma e s)
  newMArray n a = newRawMArray n >>= \ma -> ma <$ setMArray ma 0 n a
  {-# INLINE newMArray #-}

  thawCopyArray :: MonadPrim s m => Frozen ma e -> Int -> Size -> m (ma e s)
  thawCopyArray a i n = newRawMArray n >>= \ma -> ma <$ copyArray a i ma 0 n
  {-# INLINE thawCopyArray #-}

  freezeCopyMArray :: MonadPrim s m => ma e s -> Int -> Size -> m (Frozen ma e)
  freezeCopyMArray ma i n = newRawMArray n >>= \mad -> moveMArray ma i mad 0 n >> freezeMArray mad
  {-# INLINE freezeCopyMArray #-}

  setMArray :: MonadPrim s m => ma e s -> Int -> Size -> e -> m ()
  setMArray ma i0 (Size n0) x =
    let n = n0 + i0
        go i = when (i < n) $ writeMArray ma i x >> go (i + 1)
    in go i0
  {-# INLINE setMArray #-}

  shrinkMArray :: MonadPrim s m => ma e s -> Size -> m (ma e s)
  shrinkMArray ma sz = cloneMArray ma 0 sz
  {-# INLINE shrinkMArray #-}

  resizeMArray :: MonadPrim s m => ma e s -> Size -> m (ma e s)
  resizeMArray ma sz = do
    a <- freezeMArray ma
    ma' <- newRawMArray sz
    ma' <$ copyArray a 0 ma' 0 sz
  {-# INLINE resizeMArray #-}


type instance Frozen MAddr = Addr

instance Prim e => MArray MAddr e where
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
  copyArray as os mad od n =
    copyAddrToMAddr as (coerce os) mad (coerce od) (coerce n)
  {-# INLINE copyArray #-}
  moveMArray mas os mad od n =
    moveMAddrToMAddr mas (coerce os) mad (coerce od) (coerce n)
  {-# INLINE moveMArray #-}
  setMArray ma i sz = setMAddr ma (coerce i) (coerce sz)
  {-# INLINE setMArray #-}
  shrinkMArray ma sz = ma <$ shrinkMAddr ma (coerce sz :: Count e)
  {-# INLINE shrinkMArray #-}
  resizeMArray ma sz = reallocMAddr ma (coerce sz :: Count e)
  {-# INLINE resizeMArray #-}


type instance Frozen (PMArray p) = PArray p

instance (Typeable p, Prim e) => MArray (PMArray p) e where
  sizeOfArray = sizePArray
  {-# INLINE sizeOfArray #-}
  indexArray a i = indexOffMem a (coerce i)
  {-# INLINE indexArray #-}
  getSizeOfMArray = getSizePMArray
  {-# INLINE getSizeOfMArray #-}
  thawArray = thawPArray
  {-# INLINE thawArray #-}
  freezeMArray = freezePMArray
  {-# INLINE freezeMArray #-}
  newRawMArray = allocPMArray
  {-# INLINE newRawMArray #-}
  writeMArray = writePMArray
  {-# INLINE writeMArray #-}
  readMArray = readPMArray
  {-# INLINE readMArray #-}
  copyArray = copyPArrayToPMArray
  {-# INLINE copyArray #-}
  moveMArray = movePMArrayToPMArray
  {-# INLINE moveMArray #-}
  setMArray = setPMArray
  {-# INLINE setMArray #-}
  shrinkMArray ma sz = ma <$ shrinkPMArray ma sz
  {-# INLINE shrinkMArray #-}
  resizeMArray = reallocPMArray
  {-# INLINE resizeMArray #-}


type instance Frozen BMArray = BArray

instance MArray BMArray e where
  indexArray = indexBArray
  {-# INLINE indexArray #-}
  sizeOfArray = sizeOfBArray
  {-# INLINE sizeOfArray #-}
  getSizeOfMArray = getSizeOfBMArray
  {-# INLINE getSizeOfMArray #-}
  thawArray = thawBArray
  {-# INLINE thawArray #-}
  thawCopyArray = thawCopyBArray
  {-# INLINE thawCopyArray #-}
  freezeMArray = freezeBMArray
  {-# INLINE freezeMArray #-}
  freezeCopyMArray = freezeCopyBMArray
  {-# INLINE freezeCopyMArray #-}
  newRawMArray = newRawBMArray
  {-# INLINE newRawMArray #-}
  readMArray = readBMArray
  {-# INLINE readMArray #-}
  writeMArray = writeBMArray
  {-# INLINE writeMArray #-}
  newMArray = newBMArray
  {-# INLINE newMArray #-}
  copyArray = copyBArray
  {-# INLINE copyArray #-}
  moveMArray = moveBMArray
  {-# INLINE moveMArray #-}
  cloneArray = cloneBArray
  {-# INLINE cloneArray #-}
  cloneMArray = cloneBMArray
  {-# INLINE cloneMArray #-}


type instance Frozen SBMArray = SBArray

instance MArray SBMArray e where
  indexArray = indexSBArray
  {-# INLINE indexArray #-}
  sizeOfArray = sizeOfSBArray
  {-# INLINE sizeOfArray #-}
  getSizeOfMArray = getSizeOfSBMArray
  {-# INLINE getSizeOfMArray #-}
  thawArray = thawSBArray
  {-# INLINE thawArray #-}
  thawCopyArray = thawCopySBArray
  {-# INLINE thawCopyArray #-}
  freezeMArray = freezeSBMArray
  {-# INLINE freezeMArray #-}
  freezeCopyMArray = freezeCopySBMArray
  {-# INLINE freezeCopyMArray #-}
  newRawMArray = newRawSBMArray
  {-# INLINE newRawMArray #-}
  readMArray = readSBMArray
  {-# INLINE readMArray #-}
  writeMArray = writeSBMArray
  {-# INLINE writeMArray #-}
  newMArray = newSBMArray
  {-# INLINE newMArray #-}
  copyArray = copySBArray
  {-# INLINE copyArray #-}
  moveMArray = moveSBMArray
  {-# INLINE moveMArray #-}
  cloneArray = cloneSBArray
  {-# INLINE cloneArray #-}
  cloneMArray = cloneSBMArray
  {-# INLINE cloneMArray #-}


type instance Frozen UMArray = UArray

instance Prim e => MArray UMArray e where
  indexArray = indexUArray
  {-# INLINE indexArray #-}
  sizeOfArray = sizeOfUArray
  {-# INLINE sizeOfArray #-}
  getSizeOfMArray = getSizeOfUMArray
  {-# INLINE getSizeOfMArray #-}
  thawArray = thawUArray
  {-# INLINE thawArray #-}
  freezeMArray = freezeUMArray
  {-# INLINE freezeMArray #-}
  newRawMArray = newRawUMArray
  {-# INLINE newRawMArray #-}
  readMArray = readUMArray
  {-# INLINE readMArray #-}
  writeMArray = writeUMArray
  {-# INLINE writeMArray #-}
  newMArray = newUMArray
  {-# INLINE newMArray #-}
  copyArray = copyUArray
  {-# INLINE copyArray #-}
  moveMArray = moveUMArray
  {-# INLINE moveMArray #-}

-- | Convert a list into an array strictly, i.e. each element is evaluated to WHNF prior
-- to it being written into the newly created array. In order to allocate the array ahead
-- of time, the spine of a list will be evaluated first, in order to get the total
-- number of elements. Infinite lists will cause the program to halt. On the other hand
-- if the length of a list is known ahead of time, `fromListArrayN` can be used instead as
-- optimization.
--
-- @since 0.1.0
fromListArray :: MArray ma e => [e] -> Frozen ma e
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
     forall ma e. MArray ma e
  => Size -- ^ Expected @n@ size of a list
  -> [e]
  -> Frozen ma e
fromListArrayN sz@(Size n) ls =
  runST $ do
    ma :: ma e s <- newRawMArray sz
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
toListArray :: MArray ma e => Frozen ma e -> [e]
toListArray ba = build (\ c n -> foldrArray c n ba)
{-# INLINE toListArray #-}

-- | Strict right fold
foldrArray :: MArray ma e => (e -> b -> b) -> b -> Frozen ma e -> b
foldrArray c nil a = go 0
  where
    Size k = sizeOfArray a
    go i
      | i == k = nil
      | otherwise =
        let !v = indexArray a i
         in v `c` go (i + 1)
{-# INLINE[0] foldrArray #-}

makeArray :: MArray ma e => Size -> (Int -> e) -> Frozen ma e
makeArray sz f = runST $ makeArrayM sz (pure . f)
{-# INLINE makeArray #-}

makeArrayM ::
     (MArray ma e, MonadPrim s m) => Size -> (Int -> m e) -> m (Frozen ma e)
makeArrayM sz@(Size n) f =
  createArrayM_ sz $ \ma ->
    let go i
          | i < n = f i >>= writeMArray ma i >> go (i + 1)
          | otherwise = pure ()
     in go 0
{-# INLINE makeArrayM #-}

createArrayM ::
     (MArray ma e, MonadPrim s m)
  => Size
  -> (ma e s -> m b)
  -> m (b, Frozen ma e)
createArrayM sz f =
  newRawMArray sz >>= \ma -> f ma >>= \b -> (,) b <$> freezeMArray ma
{-# INLINE createArrayM #-}

createArrayM_ ::
     (MArray ma e, MonadPrim s m)
  => Size
  -> (ma e s -> m b)
  -> m (Frozen ma e)
createArrayM_ sz f =
  newRawMArray sz >>= \ma -> f ma >> freezeMArray ma
{-# INLINE createArrayM_ #-}


createArrayST :: MArray ma e => Size -> (forall s. ma e s -> ST s b) -> (b, Frozen ma e)
createArrayST sz f = runST $ createArrayM sz f
{-# INLINE createArrayST #-}

createArrayST_ :: MArray ma e => Size -> (forall s. ma e s -> ST s b) -> Frozen ma e
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
makeMArray :: (MArray ma e, MonadPrim s m) => Size -> (Int -> m e) -> m (ma e s)
makeMArray sz@(Size n) f = do
  ma <- newRawMArray sz
  let go i = when (i < n) $ f i >>= writeMArray ma i >> go (i + 1)
  ma <$ go 0
{-# INLINE makeMArray #-}



-- | Traverse an array with a monadic action.
--
-- @since 0.1.0
traverseArray ::
     (MArray ma e, MArray ma' e', MonadPrim s m)
  => (e -> m e')
  -> Frozen ma e
  -> m (Frozen ma' e')
traverseArray f a = makeArrayM (sizeOfArray a) (f . indexArray a)
{-# INLINE traverseArray #-}


