{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Data.Prim.MArray.Boxed
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.MArray.Boxed
  ( BArray(..)
  , BMArray(..)
  , Size(..)
  -- * Immutable
  , makeBArray
  , makeBArrayM
  , sizeOfBArray
  , indexBArray
  -- * Mutable
  -- ** Create
  , newBMArray
  , newRawBMArray
  , newLazyBMArray
  , makeBMArray
  , createBArrayM
  , createBArrayM_
  , sizeOfBMArray
  -- ** Access
  , readBMArray
  , writeBMArray
  , writeLazyBMArray
  , writeDeepBMArray
  -- *** Atomic
  , casBMArray
  , atomicModifyFetchNewBMArray
  , atomicModifyFetchOldBMArray
  , atomicModifyBMArray
  , atomicModifyBMArray_
  , atomicModifyBMArray2
  -- *
  , thawBArray
  , thawCopyBArray
  , freezeBMArray
  , freezeCopyBMArray
  , copyBArray
  , moveBMArray
  , cloneBArray
  , cloneBMArray
  -- * List
  , fromListBArray
  , fromListBArrayN
  , toListBArray
  -- * Helpers
  , foldrBArray
  , traverseBArray
  ) where

import Control.DeepSeq
import Control.Monad.ST
import Control.Prim.Monad
import Data.Prim
import Data.Bits
import qualified Data.Prim.MArray.Internal as I
import Data.Prim.MRef.Atomic
import Data.Prim.MRef.Internal
import Foreign.Prim
import Data.Prim.Array


instance Functor BArray where
  fmap f a = runST $ traverseBArray (pure . f) a

instance MRef (BMArray e) where
  type Elt (BMArray e) = e
  newRawMRef = newRawBMArray 1
  {-# INLINE newRawMRef #-}
  readMRef mba = readBMArray mba 0
  {-# INLINE readMRef #-}
  writeMRef mba = writeBMArray mba 0
  {-# INLINE writeMRef #-}
  newMRef = newBMArray 1
  {-# INLINE newMRef #-}


instance AtomicMRef (BMArray e) where
  casMRef mba = casBMArray mba 0
  {-# INLINE casMRef #-}

instance Num e => AtomicCountMRef (BMArray e)
instance Bits e => AtomicBitsMRef (BMArray e)

instance I.MArray (BMArray e) where
  type Array (BMArray e) = BArray e
  indexArray = indexBArray
  {-# INLINE indexArray #-}
  sizeOfArray = sizeOfBArray
  {-# INLINE sizeOfArray #-}
  getSizeOfMArray = pure . sizeOfBMArray
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




-- | Create new mutable array, where each element is set to a thunk that throws an error
-- when evaluated. This is useful when there is a plan to iterate over the whole array
-- and write values into each cell monadically or in some index aware fashion.
--
-- [Unsafe size] Negative or too large of an array size can kill the current thread with `HeapOverflow`
-- asynchronous exception.
--
-- ==== __Examples__
--
-- >>> import Control.Prim.Monad
-- >>> ma <- newRawBMArray 10 :: IO (BMArray Int RW)
-- >>> sizeOfBMArray ma
-- Size 10
-- >>> readBMArray ma 1
-- *** Exception: undefined array element: Data.Prim.MAray.Boxed.newRawBMArray
--
-- @since 0.1.0
newRawBMArray :: MonadPrim s m => Size -> m (BMArray e s)
newRawBMArray sz = newLazyBMArray sz (uninitialized "Data.Prim.MAray.Boxed" "newRawBMArray")
{-# INLINE newRawBMArray #-}





-- | Same as `cloneBArray`, except it works on mutable arrays
--
-- [Unsafe offset] Offset cannot be negative or larger than the size of an array,
-- otherwise it can result in an unchecked exception
--
-- [Unsafe new size] Number of elements to be copied cannot be larger than the size of an
-- array minus the offset.
--
-- @since 0.1.0
cloneBMArray :: MonadPrim s m => BMArray e s -> Int -> Size -> m (BMArray e s)
cloneBMArray (BMArray ma#) (I# i#) (Size (I# n#)) =
  prim $ \s ->
    case cloneMutableArray# ma# i# n# s of
      (# s', ma'# #) -> (# s', BMArray ma'# #)
{-# INLINE cloneBMArray #-}


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
copyBArray ::
     MonadPrim s m
  => BArray e -- ^ Source immutable array
  -> Int -- ^ Offset into the source immutable array
  -> BMArray e s -- ^ Destination mutable array
  -> Int -- ^ Offset into the destination mutable array
  -> Size -- ^ Number of elements to copy over
  -> m ()
copyBArray (BArray src#) (I# srcOff#) (BMArray dst#) (I# dstOff#) (Size (I# n#)) =
  prim_ (copyArray# src# srcOff# dst# dstOff# n#)
{-# INLINE copyBArray #-}

-- | Copy a subsection of a mutable array into a subsection of another or the same
-- mutable array. Therefore, unlike `copyBArray`, memory overlap is allowed.
--
-- [Unsafe offset] Each offset cannot be negative or larger than the size of a
-- corresponding array, otherwise it can result in an unchecked exception
--
-- [Unsafe new size] Number of elements to be copied cannot be larger than the size of an
-- each array minus their corersponding offsets.
--
-- @since 0.1.0
moveBMArray ::
     MonadPrim s m
  => BMArray e s -- ^ Source mutable array
  -> Int -- ^ Offset into the source mutable array
  -> BMArray e s -- ^ Destination mutable array
  -> Int -- ^ Offset into the destination mutable array
  -> Size -- ^ Number of elements to copy over
  -> m ()
moveBMArray (BMArray src#) (I# srcOff#) (BMArray dst#) (I# dstOff#) (Size (I# n#)) =
  prim_ (copyMutableArray# src# srcOff# dst# dstOff# n#)
{-# INLINE moveBMArray #-}



atomicModifyBMArray# :: MonadPrim s m => BMArray e s -> Int -> (e -> (# e, b #)) -> m b
atomicModifyBMArray# ma@(BMArray ma#) i@(I# i#) f = do
  current0 <- readBMArray ma i
  prim $
    let go expected s =
          case f expected of
            (# new, artifact #) ->
              case casArray# ma# i# expected new s of
                (# s', 0#, _ #)     -> (# s', artifact #)
                (# s', _, actual #) -> go actual s'
     in go current0
{-# INLINE atomicModifyBMArray# #-}


atomicModifyFetchNewBMArray :: MonadPrim s m => BMArray e s -> Int -> (e -> e) -> m e
atomicModifyFetchNewBMArray ma i f =
  atomicModifyBMArray# ma i (\a -> let a' = f a in (# a', a' #))
{-# INLINE atomicModifyFetchNewBMArray #-}

-- atomicModifyFetchNewBMArray ma@(BMArray ma#) i@(I# i#) f = do
--   current0 <- readBMArray ma i
--   prim $ \s0 ->
--     let go expected s =
--           case casBArray# ma# i# expected (f expected) s of
--             (# s', 0#, actual #) -> go actual s'
--             (# s', _, current #) -> (# s', current #)
--     in go current0 s0
  -- let go e =
  --       casBMArray ma i e (f e) >>= \case
  --         (True, new) -> pure new
  --         (_, current) -> go current
  --  in readBMArray ma i >>= go

atomicModifyFetchOldBMArray :: MonadPrim s m => BMArray e s -> Int -> (e -> e) -> m e
atomicModifyFetchOldBMArray ma i f =
  atomicModifyBMArray# ma i (\a -> (# f a, a #))
{-# INLINE atomicModifyFetchOldBMArray #-}
  -- let go e =
  --       casBMArray ma i e (f e) >>= \case
  --         (True, _new) -> pure e
  --         (_, current) -> go current
  --  in readBMArray ma i >>= go



atomicModifyBMArray :: MonadPrim s m => BMArray e s -> Int -> (e -> (e, b)) -> m b
atomicModifyBMArray ma i f =
  atomicModifyBMArray# ma i (\a -> let (a', b) = f a in (# a', b #))
{-# INLINE atomicModifyBMArray #-}
  -- let go e =
  --       let (new, artifact) = f e
  --        in casBMArray ma i e new >>= \case
  --             (True, _new) -> pure artifact
  --             (_, current) -> go current
  --  in readBMArray ma i >>= go


atomicModifyBMArray_ :: MonadPrim s m => BMArray e s -> Int -> (e -> e) -> m ()
atomicModifyBMArray_ ma i f =
  atomicModifyBMArray# ma i (\a -> let a' = f a in (# a', () #))
{-# INLINE atomicModifyBMArray_ #-}


atomicModifyBMArray2 :: MonadPrim s m => BMArray e s -> Int -> (e -> (e, b)) -> m (e, e, b)
atomicModifyBMArray2 ma i f =
  atomicModifyBMArray# ma i (\a -> let (a', b) = f a in (# a', (a, a', b) #))
{-# INLINE atomicModifyBMArray2 #-}


-- | Convert a list into an array strictly, i.e. each element is evaluated to WHNF prior
-- to being written into the newly created array. In order to allocate the array ahead
-- of time, the spine of a list will be evaluated first, in order to get the total
-- number of elements. Infinite lists will cause the program to halt. On the other hand
-- if the length of a list is known ahead of time, `fromListBArrayN` can be used instead as
-- optimization.
--
-- @since 0.1.0
fromListBArray :: [e] -> BArray e
fromListBArray xs = fromListBArrayN (Size (length xs)) xs
{-# INLINE fromListBArray #-}

-- | Same as `fromListBArray`, except it will allocate an array exactly of @n@ size, as
-- such it will not convert any portion of the list that doesn't fit into the newly
-- created array.
--
-- [Unsafe size] if the length of supplied list is actually smaller then the expected
-- size, thunks with `UndefinedElement` will be left in the tail of the array.
--
-- ====__Examples__
--
-- >>> fromListBArrayN 3 [1 :: Int, 2, 3]
-- Array [1,2,3]
-- >>> fromListBArrayN 3 [1 :: Int ..]
-- Array [1,2,3]
-- >>> fromListBArrayN 3 [1 :: Int, 2]
-- Array [1,2*** Exception: undefined array element: Data.Prim.Array.Boxed.uninitialized
--
-- @since 0.1.0
fromListBArrayN ::
     Size -- ^ Expected @n@ size of a list
  -> [e]
  -> BArray e
fromListBArrayN = I.fromListArrayN
{-# INLINE fromListBArrayN #-}

-- | Convert a pure boxed array into a list. It should work fine with GHC built-in list
-- fusion.
--
-- @since 0.1.0
toListBArray :: BArray e -> [e]
toListBArray = I.toListArray
{-# INLINE toListBArray #-}

-- | Strict right fold
foldrBArray :: (e -> b -> b) -> b -> BArray e -> b
foldrBArray = I.foldrArray
{-# INLINE foldrBArray #-}

makeBArray :: Size -> (Int -> e) -> BArray e
makeBArray = I.makeArray
{-# INLINE makeBArray #-}

makeBArrayM :: MonadPrim s m => Size -> (Int -> m e) -> m (BArray e)
makeBArrayM = I.makeArrayM
{-# INLINE makeBArrayM #-}

createBArrayM :: MonadPrim s m => Size -> (BMArray e s -> m b) -> m (b, BArray e)
createBArrayM = I.createArrayM
{-# INLINE createBArrayM #-}

createBArrayM_ :: MonadPrim s m => Size -> (BMArray e s -> m b) -> m (BArray e)
createBArrayM_ = I.createArrayM_
{-# INLINE createBArrayM_ #-}


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
-- >>> ma <- makeBMArray 5 $ \i -> modifyFetchRef ref (\cur -> cur ++ show i ++ ",")
-- >>> mapM_ (readBMArray ma >=> putStrLn) [0 .. 4]
-- Numbers: 0,
-- Numbers: 0,1,
-- Numbers: 0,1,2,
-- Numbers: 0,1,2,3,
-- Numbers: 0,1,2,3,4,
--
-- @since 0.1.0
makeBMArray :: MonadPrim s m => Size -> (Int -> m e) -> m (BMArray e s)
makeBMArray = I.makeMArray
{-# INLINE makeBMArray #-}

-- | Traverse an array with a monadic action.
--
-- @since 0.1.0
traverseBArray :: MonadPrim s m => (e -> m b) -> BArray e -> m (BArray b)
traverseBArray = I.traverseArray
{-# INLINE traverseBArray #-}
