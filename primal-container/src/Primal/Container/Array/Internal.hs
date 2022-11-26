{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
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
-- Module      : Primal.Container.Array.Internal
-- Copyright   : (c) Alexey Kuleshevich 2020-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module Primal.Container.Array.Internal (
  Size (..),
  module Primal.Mutable.Freeze,
  module Primal.Container.Array.Internal,
  module Primal.Container.Internal,
) where

import Control.Monad.ST
import Data.Kind
import Primal.Array
import Primal.Container.Internal
import Primal.Container.Ref.Internal
import Primal.Foreign
import Primal.Memory
import Primal.Memory.Addr
import Primal.Memory.FAddr
import Primal.Memory.PUArray
import Primal.Mutable.Freeze

type family Array (ma :: Type -> Type -> Type) e :: Type where
  Array ma e = Frozen (ma e)

class MutRef ma => MutArray ma where
  -- | Access the size of an immutable array
  --
  -- @since 0.1.0
  sizeOfArray :: Elt ma e => Array ma e -> Size

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
  indexArray
    :: Elt ma e
    => Array ma e
    -- ^ Array to be indexed
    -> Int
    -- ^ Offset into the array
    --
    -- [Unsafe /offset/] /Unchecked precondition:/ @offset >= 0 && offset < `sizeOfArray` mut@
    -> e

  -- | Get the size of a mutable array. Unlike `sizeOfArray` it is a monadic operation
  -- because mutable arrays support in place resizing.
  --
  -- @since 1.0.0
  getSizeOfMutArrayST :: Elt ma e => ma e s -> ST s Size

  newRawMutArrayST :: Elt ma e => Size -> ST s (ma e s)

  -- | Read an element from a mutable array
  --
  -- ====__Examples__
  --
  -- >>> import Primal.Memory.Addr
  -- >>> callocMAddr (Count 5 :: Count Int)
  -- >>> ma <- callocMAddr (Count 5 :: Count Int)
  -- >>> readMutArray ma 2
  -- 0
  -- >>> writeMutArray ma 2 99
  -- >>> readMutArray ma 2
  -- 99
  --
  -- @since 1.0.0
  readMutArrayST
    :: Elt ma e
    => ma e s
    -- ^ Array to read element from
    -> Int
    -- ^ Offset into the array
    --
    -- [Unsafe /offset/] /Unchecked precondition:/ @offset >= 0 && offset < `getSizeOfMutArrayST` mut@
    -> ST s e

  -- | Write an element into a mutable array
  --
  -- @since 1.0.0
  writeMutArrayST
    :: Elt ma e
    => ma e s
    -- ^ Array to write an element into
    -> Int
    -- ^ Offset into the array
    --
    -- [Unsafe /offset/] /Unchecked precondition:/ @offset >= 0 && offset < `getSizeOfMutArrayST` mut@
    -> e
    -- ^ Element to be written
    -> ST s ()

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
  -- @since 1.0.0
  copyArrayST
    :: Elt ma e
    => Array ma e
    -- ^ Source immutable array
    -> Int
    -- ^ Offset into the source immutable array
    -> ma e s
    -- ^ Destination mutable array
    -> Int
    -- ^ Offset into the destination mutable array
    -> Size
    -- ^ Number of elements to copy over
    -> ST s ()

  -- | Copy a subsection of a mutable array into a subsection of another or the same
  -- mutable array. Therefore, unlike `copyArray`, memory overlap is allowed.
  --
  -- [Unsafe offset] Each offset cannot be negative or larger than the size of a
  -- corresponding array, otherwise it can result in an unchecked exception
  --
  -- [Unsafe new size] Number of elements to be copied cannot be larger than the size of an
  -- each array minus their corersponding offsets.
  --
  -- @since 1.0.0
  moveMutArrayST
    :: Elt ma e
    => ma e s
    -- ^ Source mutable array
    -> Int
    -- ^ Offset into the source mutable array
    -> ma e s
    -- ^ Destination mutable array
    -> Int
    -- ^ Offset into the destination mutable array
    -> Size
    -- ^ Number of elements to copy over
    -> ST s ()

  cloneSliceArray :: Elt ma e => Array ma e -> Int -> Size -> Array ma e
  cloneSliceArray arr i n = runST $ thawCloneSliceArrayST arr i n >>= freezeMutArrayST
  {-# INLINE cloneSliceArray #-}

  cloneSliceMutArrayST :: Elt ma e => ma e s -> Int -> Size -> ST s (ma e s)
  cloneSliceMutArrayST ma i n =
    newRawMutArrayST n >>= \mad -> mad <$ moveMutArrayST ma i mad 0 n
  {-# INLINE cloneSliceMutArrayST #-}

  newMutArrayST :: Elt ma e => Size -> e -> ST s (ma e s)
  newMutArrayST n a = newRawMutArrayST n >>= \ma -> ma <$ setMutArrayST ma 0 n a
  {-# INLINE newMutArrayST #-}

  thawCloneSliceArrayST :: Elt ma e => Array ma e -> Int -> Size -> ST s (ma e s)
  thawCloneSliceArrayST a i n = newRawMutArrayST n >>= \ma -> ma <$ copyArrayST a i ma 0 n
  {-# INLINE thawCloneSliceArrayST #-}

  freezeCloneSliceMutArrayST :: Elt ma e => ma e s -> Int -> Size -> ST s (Array ma e)
  freezeCloneSliceMutArrayST ma i n =
    newRawMutArrayST n >>= \mad -> moveMutArrayST ma i mad 0 n >> freezeMutArrayST mad
  {-# INLINE freezeCloneSliceMutArrayST #-}

  setMutArrayST :: Elt ma e => ma e s -> Int -> Size -> e -> ST s ()
  setMutArrayST ma i0 (Size n0) x =
    let n = n0 + i0
        go i = when (i < n) $ writeMutArrayST ma i x >> go (i + 1)
     in go i0
  {-# INLINE setMutArrayST #-}

  shrinkMutArrayST :: Elt ma e => ma e s -> Size -> ST s (ma e s)
  shrinkMutArrayST ma sz = cloneSliceMutArrayST ma 0 sz
  {-# INLINE shrinkMutArrayST #-}

  resizeMutArrayST :: Elt ma e => ma e s -> Size -> ST s (ma e s)
  resizeMutArrayST ma sz = do
    a <- freezeMutArrayST ma
    ma' <- newRawMutArrayST sz
    ma' <$ copyArrayST a 0 ma' 0 sz
  {-# INLINE resizeMutArrayST #-}

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
  -- >>> writeMutArrayST ma 4 'a'
  -- >>> writeMutArrayST ma 11 'e'
  -- >>> freezeMutArrayST ma
  -- "A whale breed"
  --
  -- @since 1.0.0
  thawArrayST
    :: Elt ma e
    => Array ma e
    -- ^ Immutable array to thaw
    -> ST s (ma e s)
    -- ^ Thawed mutable array. Any mutation will also affect the source immutable array
    --
    -- [Unsafe /mutable array/] Allows to break referential transparency.
  default thawArrayST :: (Elt ma e, MutFreeze (ma e)) => Array ma e -> ST s (ma e s)
  thawArrayST = thawST
  {-# INLINE thawArrayST #-}

  -- | Convert a mutable array into the matching immutable array.
  --
  -- @since 1.0.0
  freezeMutArrayST
    :: Elt ma e
    => ma e s
    -- ^ Mutable array to freeze. Any further mutation will also affect the immutable
    -- array
    --
    -- [Unsafe /mutable array/] Allows to break referential transparency.
    -> ST s (Array ma e)
  default freezeMutArrayST :: (Elt ma e, MutFreeze (ma e)) => ma e s -> ST s (Array ma e)
  freezeMutArrayST = freezeMut
  {-# INLINE freezeMutArrayST #-}

instance MutArray MAddr where
  sizeOfArray = coerce . countAddr
  {-# INLINE sizeOfArray #-}
  indexArray a i = indexOffAddr a (coerce i)
  {-# INLINE indexArray #-}
  getSizeOfMutArrayST = fmap coerce . getCountMAddr
  {-# INLINE getSizeOfMutArrayST #-}
  newRawMutArrayST = allocMAddr . coerce
  {-# INLINE newRawMutArrayST #-}
  writeMutArrayST ma i = writeOffMAddr ma (coerce i)
  {-# INLINE writeMutArrayST #-}
  readMutArrayST ma i = readOffMAddr ma (coerce i)
  {-# INLINE readMutArrayST #-}
  copyArrayST as os mad od n =
    copyAddrToMAddr as (coerce os) mad (coerce od) (coerce n)
  {-# INLINE copyArrayST #-}
  moveMutArrayST mas os mad od n =
    moveMAddrToMAddr mas (coerce os) mad (coerce od) (coerce n)
  {-# INLINE moveMutArrayST #-}
  setMutArrayST ma i sz = setOffMAddr ma (coerce i) (coerce sz)
  {-# INLINE setMutArrayST #-}
  shrinkMutArrayST ma sz = ma <$ shrinkMAddr ma (coerce sz :: Count e)
  {-# INLINE shrinkMutArrayST #-}
  resizeMutArrayST ma sz = reallocMAddr ma (coerce sz :: Count e)
  {-# INLINE resizeMutArrayST #-}

instance MutArray FMAddr where
  sizeOfArray = coerce . countFAddr
  {-# INLINE sizeOfArray #-}
  indexArray a i = indexOffFAddr a (coerce i)
  {-# INLINE indexArray #-}
  getSizeOfMutArrayST = fmap coerce . getCountFMAddr
  {-# INLINE getSizeOfMutArrayST #-}
  newRawMutArrayST = allocFMAddr . coerce
  {-# INLINE newRawMutArrayST #-}
  writeMutArrayST ma i = writeOffFMAddr ma (coerce i)
  {-# INLINE writeMutArrayST #-}
  readMutArrayST ma i = readOffFMAddr ma (coerce i)
  {-# INLINE readMutArrayST #-}
  copyArrayST as os mad od n =
    copyFAddrToFMAddr as (coerce os) mad (coerce od) (coerce n)
  {-# INLINE copyArrayST #-}
  moveMutArrayST mas os mad od n =
    moveFMAddrToFMAddr mas (coerce os) mad (coerce od) (coerce n)
  {-# INLINE moveMutArrayST #-}
  setMutArrayST ma i sz = setOffFMAddr ma (coerce i) (coerce sz)
  {-# INLINE setMutArrayST #-}
  shrinkMutArrayST ma sz = reallocFMAddr ma (coerce sz :: Count e)
  {-# INLINE shrinkMutArrayST #-}
  resizeMutArrayST ma sz = reallocFMAddr ma (coerce sz :: Count e)
  {-# INLINE resizeMutArrayST #-}

instance Typeable p => MutArray (PUMArray p) where
  sizeOfArray = sizePUArray
  {-# INLINE sizeOfArray #-}
  indexArray a i = indexOffMem a (coerce i)
  {-# INLINE indexArray #-}
  getSizeOfMutArrayST = getSizeOfPUMArray
  {-# INLINE getSizeOfMutArrayST #-}
  newRawMutArrayST = allocPUMArray
  {-# INLINE newRawMutArrayST #-}
  writeMutArrayST = writePUMArray
  {-# INLINE writeMutArrayST #-}
  readMutArrayST = readPUMArray
  {-# INLINE readMutArrayST #-}
  copyArrayST = copyPUArrayToPUMArray
  {-# INLINE copyArrayST #-}
  moveMutArrayST = movePUMArrayToPUMArray
  {-# INLINE moveMutArrayST #-}
  setMutArrayST = setPUMArray
  {-# INLINE setMutArrayST #-}
  shrinkMutArrayST ma sz = ma <$ shrinkPUMArray ma sz
  {-# INLINE shrinkMutArrayST #-}
  resizeMutArrayST = reallocPUMArray
  {-# INLINE resizeMutArrayST #-}

instance MutArray BMArray where
  indexArray = indexBArray
  {-# INLINE indexArray #-}
  sizeOfArray = sizeOfBArray
  {-# INLINE sizeOfArray #-}
  getSizeOfMutArrayST = getSizeOfBMArray
  {-# INLINE getSizeOfMutArrayST #-}
  thawCloneSliceArrayST = thawCloneSliceBArray
  {-# INLINE thawCloneSliceArrayST #-}
  freezeCloneSliceMutArrayST = freezeCloneSliceBMArray
  {-# INLINE freezeCloneSliceMutArrayST #-}
  newRawMutArrayST = newRawBMArray
  {-# INLINE newRawMutArrayST #-}
  readMutArrayST = readBMArray
  {-# INLINE readMutArrayST #-}
  writeMutArrayST = writeBMArray
  {-# INLINE writeMutArrayST #-}
  newMutArrayST = newBMArray
  {-# INLINE newMutArrayST #-}
  copyArrayST = copyBArray
  {-# INLINE copyArrayST #-}
  moveMutArrayST = moveBMArray
  {-# INLINE moveMutArrayST #-}
  cloneSliceArray = cloneSliceBArray
  {-# INLINE cloneSliceArray #-}
  cloneSliceMutArrayST = cloneSliceBMArray
  {-# INLINE cloneSliceMutArrayST #-}

instance MutArray SBMArray where
  indexArray = indexSBArray
  {-# INLINE indexArray #-}
  sizeOfArray = sizeOfSBArray
  {-# INLINE sizeOfArray #-}
  getSizeOfMutArrayST = getSizeOfSBMArray
  {-# INLINE getSizeOfMutArrayST #-}
  thawCloneSliceArrayST = thawCloneSliceSBArray
  {-# INLINE thawCloneSliceArrayST #-}
  freezeCloneSliceMutArrayST = freezeCloneSliceSBMArray
  {-# INLINE freezeCloneSliceMutArrayST #-}
  newRawMutArrayST = newRawSBMArray
  {-# INLINE newRawMutArrayST #-}
  readMutArrayST = readSBMArray
  {-# INLINE readMutArrayST #-}
  writeMutArrayST = writeSBMArray
  {-# INLINE writeMutArrayST #-}
  newMutArrayST = newSBMArray
  {-# INLINE newMutArrayST #-}
  copyArrayST = copySBArray
  {-# INLINE copyArrayST #-}
  moveMutArrayST = moveSBMArray
  {-# INLINE moveMutArrayST #-}
  cloneSliceArray = cloneSliceSBArray
  {-# INLINE cloneSliceArray #-}
  cloneSliceMutArrayST = cloneSliceSBMArray
  {-# INLINE cloneSliceMutArrayST #-}

instance MutArray UMArray where
  indexArray = indexUArray
  {-# INLINE indexArray #-}
  sizeOfArray = sizeOfUArray
  {-# INLINE sizeOfArray #-}
  getSizeOfMutArrayST = getSizeOfUMArray
  {-# INLINE getSizeOfMutArrayST #-}
  readMutArrayST = readUMArray
  {-# INLINE readMutArrayST #-}
  writeMutArrayST = writeUMArray
  {-# INLINE writeMutArrayST #-}
  newMutArrayST = newUMArray
  {-# INLINE newMutArrayST #-}
  newRawMutArrayST = newRawUMArray
  {-# INLINE newRawMutArrayST #-}
  copyArrayST = copyUArray
  {-# INLINE copyArrayST #-}
  moveMutArrayST = moveUMArray
  {-# INLINE moveMutArrayST #-}

-- | Convert a list into an array strictly, i.e. each element is evaluated to WHNF prior
-- to it being written into the newly created array. In order to allocate the array ahead
-- of time, the spine of a list will be evaluated first, in order to get the total
-- number of elements. Infinite lists will cause the program to halt. On the other hand
-- if the length of a list is known ahead of time, `fromListArrayN` can be used instead as
-- optimization.
--
-- @since 0.1.0
fromListArray :: (MutArray ma, Elt ma e) => [e] -> Array ma e
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
fromListArrayN
  :: forall ma e
   . (MutArray ma, Elt ma e)
  => Size
  -- ^ Expected @n@ size of a list
  -> [e]
  -> Array ma e
fromListArrayN sz@(Size n) ls =
  runST $ do
    ma :: ma e s <- newRawMutArrayST sz
    let go i =
          \case
            x : xs
              | i < n -> writeMutArrayST ma i x >> go (i + 1) xs
            _ -> pure ()
    go 0 ls
    freezeMutArrayST ma

-- | Convert a pure boxed array into a list. It should work fine with GHC built-in list
-- fusion.
--
-- @since 0.1.0
toListArray
  :: forall ma e
   . (MutArray ma, Elt ma e)
  => Array ma e
  -> [e]
toListArray ba = build (\c n -> foldrArray c n ba)
{-# INLINE toListArray #-}

-- | Strict right fold
foldrArray
  :: forall ma e b
   . (MutArray ma, Elt ma e)
  => (e -> b -> b)
  -> b
  -> Array ma e
  -> b
foldrArray c nil a = go 0
  where
    Size k = sizeOfArray a
    go i
      | i == k = nil
      | otherwise =
          let !v = indexArray a i
           in v `c` go (i + 1)
{-# INLINE [0] foldrArray #-}

makeArray
  :: forall ma e
   . (MutArray ma, Elt ma e)
  => Size
  -> (Int -> e)
  -> Array ma e
makeArray sz f = runST $ makeArrayM sz (pure . f)
{-# INLINE makeArray #-}

makeArrayM
  :: forall ma e m s
   . (MutArray ma, Elt ma e, Primal s m)
  => Size
  -> (Int -> m e)
  -> m (Array ma e)
makeArrayM sz@(Size n) f =
  createArrayM_ sz $ \ma ->
    let go i = when (i < n) (f i >>= writeMutArray ma i >> go (i + 1))
     in go 0
{-# INLINE makeArrayM #-}

createArrayM
  :: forall ma e b m s
   . (MutArray ma, Elt ma e, Primal s m)
  => Size
  -> (ma e s -> m b)
  -> m (b, Array ma e)
createArrayM sz f =
  newRawMutArray sz >>= \ma -> f ma >>= \b -> (,) b <$> freezeMutArray ma
{-# INLINE createArrayM #-}

createArrayM_
  :: forall ma e b m s
   . (MutArray ma, Elt ma e, Primal s m)
  => Size
  -> (ma e s -> m b)
  -> m (Array ma e)
createArrayM_ sz f =
  newRawMutArray sz >>= \ma -> f ma >> freezeMutArray ma
{-# INLINE createArrayM_ #-}

createArrayST
  :: forall ma e b
   . (MutArray ma, Elt ma e)
  => Size
  -> (forall s. ma e s -> ST s b)
  -> (b, Array ma e)
createArrayST sz f = runST $ createArrayM sz f
{-# INLINE createArrayST #-}

createArrayST_
  :: forall ma e b
   . (MutArray ma, Elt ma e)
  => Size
  -> (forall s. ma e s -> ST s b)
  -> Array ma e
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
-- >>> ma <- makeMutArray 5 $ \i -> modifyFetchRef ref (\cur -> cur ++ show i ++ ",")
-- >>> mapM_ (readMutArray ma >=> putStrLn) [0 .. 4]
-- Numbers: 0,
-- Numbers: 0,1,
-- Numbers: 0,1,2,
-- Numbers: 0,1,2,3,
-- Numbers: 0,1,2,3,4,
--
-- @since 0.1.0
makeMutArray
  :: forall ma e m s
   . (MutArray ma, Elt ma e, Primal s m)
  => Size
  -> (Int -> m e)
  -> m (ma e s)
makeMutArray sz@(Size n) f = do
  ma <- newRawMutArray sz
  let go i = when (i < n) $ f i >>= writeMutArray ma i >> go (i + 1)
  ma <$ go 0
{-# INLINE makeMutArray #-}

-- -- | Traverse an array with a monadic action.
-- --
-- -- @since 0.1.0
-- traverseArray ::
--      (MutArray ma, Elt ma e, Elt ma e', Primal s m)
--   => (e -> m e')
--   -> Array ma e
--   -> m (Frozen (ma e'))
-- traverseArray f a = makeArrayM (sizeOfArray a) (f . indexArray a)
-- {-# INLINE traverseArray #-}

-- | Get the size of a mutable array. Unlike `sizeOfArray` it is a monadic operation
-- because mutable arrays support in place resizing.
--
-- @since 1.0.0
getSizeOfMutArray
  :: forall ma e m s
   . (MutArray ma, Elt ma e, Primal s m)
  => ma e s
  -> m Size
getSizeOfMutArray = liftST . getSizeOfMutArrayST
{-# INLINE getSizeOfMutArray #-}

newRawMutArray
  :: forall ma e m s
   . (MutArray ma, Elt ma e, Primal s m)
  => Size
  -> m (ma e s)
newRawMutArray = liftST . newRawMutArrayST
{-# INLINE newRawMutArray #-}

-- | Read an element from a mutable array
--
-- ====__Examples__
--
-- >>> import Primal.Memory.Addr
-- >>> callocMAddr (Count 5 :: Count Int)
-- >>> ma <- callocMAddr (Count 5 :: Count Int)
-- >>> readMutArray ma 2
-- 0
-- >>> writeMutArray ma 2 99
-- >>> readMutArray ma 2
-- 99
--
-- @since 0.1.0
readMutArray
  :: forall ma e m s
   . (MutArray ma, Elt ma e, Primal s m)
  => ma e s
  -- ^ Array to read element from
  -> Int
  -- ^ Offset into the array
  --
  -- [Unsafe /offset/] /Unchecked precondition:/ @offset >= 0 && offset < `getSizeOfMutArray` mut@
  -> m e
readMutArray ma = liftST . readMutArrayST ma
{-# INLINE readMutArray #-}

-- | Write an element into a mutable array
--
-- @since 0.1.0
writeMutArray
  :: forall ma e m s
   . (MutArray ma, Elt ma e, Primal s m)
  => ma e s
  -- ^ Array to write an element into
  -> Int
  -- ^ Offset into the array
  --
  -- [Unsafe /offset/] /Unchecked precondition:/ @offset >= 0 && offset < `getSizeOfMutArray` mut@
  -> e
  -- ^ Element to be written
  -> m ()
writeMutArray ma i = liftST . writeMutArrayST ma i
{-# INLINE writeMutArray #-}

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
copyArray
  :: forall ma e m s
   . (MutArray ma, Elt ma e, Primal s m)
  => Array ma e
  -- ^ Source immutable array
  -> Int
  -- ^ Offset into the source immutable array
  -> ma e s
  -- ^ Destination mutable array
  -> Int
  -- ^ Offset into the destination mutable array
  -> Size
  -- ^ Number of elements to copy over
  -> m ()
copyArray ma ia mb ib = liftST . copyArrayST ma ia mb ib
{-# INLINE copyArray #-}

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
moveMutArray
  :: forall ma e m s
   . (MutArray ma, Elt ma e, Primal s m)
  => ma e s
  -- ^ Source mutable array
  -> Int
  -- ^ Offset into the source mutable array
  -> ma e s
  -- ^ Destination mutable array
  -> Int
  -- ^ Offset into the destination mutable array
  -> Size
  -- ^ Number of elements to copy over
  -> m ()
moveMutArray ma ia mb ib = liftST . moveMutArrayST ma ia mb ib
{-# INLINE moveMutArray #-}

cloneSliceMutArray
  :: forall ma e m s
   . (MutArray ma, Elt ma e, Primal s m)
  => ma e s
  -> Int
  -> Size
  -> m (ma e s)
cloneSliceMutArray ma i = liftST . cloneSliceMutArrayST ma i
{-# INLINE cloneSliceMutArray #-}

newMutArray
  :: forall ma e m s
   . (MutArray ma, Elt ma e, Primal s m)
  => Size
  -> e
  -> m (ma e s)
newMutArray k = liftST . newMutArrayST k
{-# INLINE newMutArray #-}

thawCloneSliceArray
  :: forall ma e m s
   . (MutArray ma, Elt ma e, Primal s m)
  => Array ma e
  -> Int
  -> Size
  -> m (ma e s)
thawCloneSliceArray a i = liftST . thawCloneSliceArrayST a i
{-# INLINE thawCloneSliceArray #-}

freezeCloneSliceMutArray
  :: forall ma e m s
   . (MutArray ma, Elt ma e, Primal s m)
  => ma e s
  -> Int
  -> Size
  -> m (Array ma e)
freezeCloneSliceMutArray ma i = liftST . freezeCloneSliceMutArrayST ma i
{-# INLINE freezeCloneSliceMutArray #-}

setMutArray
  :: forall ma e m s
   . (MutArray ma, Elt ma e, Primal s m)
  => ma e s
  -> Int
  -> Size
  -> e
  -> m ()
setMutArray ma i k = liftST . setMutArrayST ma i k
{-# INLINE setMutArray #-}

shrinkMutArray
  :: forall ma e m s
   . (MutArray ma, Elt ma e, Primal s m)
  => ma e s
  -> Size
  -> m (ma e s)
shrinkMutArray ma = liftST . shrinkMutArrayST ma
{-# INLINE shrinkMutArray #-}

resizeMutArray
  :: forall ma e m s
   . (MutArray ma, Elt ma e, Primal s m)
  => ma e s
  -> Size
  -> m (ma e s)
resizeMutArray ma = liftST . resizeMutArrayST ma
{-# INLINE resizeMutArray #-}

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
-- >>> writeMutArray ma 4 'a'
-- >>> writeMutArray ma 11 'e'
-- >>> freezeMutArray ma
-- "A whale breed"
--
-- @since 0.1.0
thawArray
  :: forall ma e m s
   . (MutArray ma, Elt ma e, Primal s m)
  => Array ma e
  -- ^ Immutable array to thaw
  -> m (ma e s)
  -- ^ Thawed mutable array. Any mutation will also affect the source immutable array
  --
  -- [Unsafe /mutable array/] Allows to break referential transparency.
thawArray = liftST . thawArrayST
{-# INLINE thawArray #-}

-- | Convert a mutable array into the matching immutable array.
--
-- @since 0.1.0
freezeMutArray
  :: forall ma e m s
   . (MutArray ma, Elt ma e, Primal s m)
  => ma e s
  -- ^ Mutable array to freeze. Any further mutation will also affect the immutable
  -- array
  --
  -- [Unsafe /mutable array/] Allows to break referential transparency.
  -> m (Array ma e)
freezeMutArray = liftST . freezeMutArrayST
{-# INLINE freezeMutArray #-}
