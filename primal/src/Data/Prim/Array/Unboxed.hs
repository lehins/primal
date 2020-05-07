{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Data.Prim.Array.Unboxed
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Array.Unboxed
  ( Array
  , UnboxedArray(..)
  , MArray
  , UnboxedMArray(..)
  , Size(..)
  -- * Immutable
  , makeArray
  , makeArrayM
  , sizeOfArray
  , indexArray
  -- * Mutable
  -- ** Create
  , newMArray
  , newRawMArray
  , makeMArray
  , createArrayM
  , createArrayM_
  -- ** Access
  , readMArray
  , writeMArray
  -- *** Atomic
  -- , casMArray
  -- , atomicModifyFetchMArray
  -- , atomicFetchModifyMArray
  -- , atomicModifyMArray
  -- , atomicModifyMArray_
  -- , atomicModifyMArray2
  -- *
  , thawArray
  , thawCopyArray
  , freezeMArray
  , freezeCopyMArray
  , copyArray
  , moveMArray
  , cloneArray
  , cloneMArray
  -- -- * List
  -- , fromListArray
  -- , fromListArrayN
  -- , toListArray
  -- -- * Helpers
  -- , foldrArray
  -- , traverseArray
  ) where

import Control.DeepSeq
import Control.Exception (ArrayException(UndefinedElement), throw)
import Control.Monad.ST
import Control.Prim.Monad
import Data.Prim
import Data.Prim.Class
import Data.Prim.Array.Internal (Size(..))
import qualified Data.Prim.Array.Internal as I
import Foreign.Prim

type Array a = UnboxedArray a

instance (Prim a, Show a) => Show (UnboxedArray a) where
  showsPrec n arr
    | n > 1 = ('(' :) . inner . (')' :)
    | otherwise = inner
    where
      inner = ("Array " ++) . shows (toList arr)

instance Prim a => IsList (UnboxedArray a) where
  type Item (Array a) = a
  fromList = I.fromListArray
  fromListN n = I.fromListArrayN (Size n)
  toList = I.toListArray

data UnboxedMArray a s = MArray (MutableByteArray# s)

type MArray a s = UnboxedMArray a s


-- | Check if both of the arrays refer to the exact same one. None of the elements are
-- evaluated.
instance Eq (UnboxedMArray a s) where
  MArray ma1# == MArray ma2# = isTrue# (sameMutableByteArray# ma1# ma2#)

#if __GLASGOW_HASKELL__ >= 800
data UnboxedArray a = Array ByteArray#

instance Prim a => I.MArray UnboxedMArray a where
  type IArray UnboxedMArray = UnboxedArray
#else
type UnboxedArray a = I.IArray UnboxedMArray a

instance Prim a => I.MArray UnboxedMArray a where
  data IArray UnboxedMArray a = Array ByteArray#
#endif
  sizeOfArray = sizeOfArray
  {-# INLINE sizeOfArray #-}
  indexArray = indexArray
  {-# INLINE indexArray #-}
  getSizeOfMArray = getSizeOfMArray
  {-# INLINE getSizeOfMArray #-}
  thawArray = thawArray
  {-# INLINE thawArray #-}
  freezeMArray = freezeMArray
  {-# INLINE freezeMArray #-}
  newRawMArray = newRawMArray
  {-# INLINE newRawMArray #-}
  readMArray = readMArray
  {-# INLINE readMArray #-}
  writeMArray = writeMArray
  {-# INLINE writeMArray #-}
  copyArray = copyArray
  {-# INLINE copyArray #-}
  moveMArray = moveMArray
  {-# INLINE moveMArray #-}
  setMArray = setMArray
  {-# INLINE setMArray #-}



sizeOfArray :: Array a -> Size
sizeOfArray (Array a#) = Size (I# (sizeofByteArray# a#))
{-# INLINE sizeOfArray #-}

-- | Index an element of a pure unboxed array.
--
-- [Unsafe index] Negative or larger than array size can fail with unchecked exception
--
-- ==== __Examples__
--
-- >>> import Data.Prim.Array.Unboxed
-- >>> let a = makeArray 1024 (\i -> [0 .. i])
-- >>> print $ indexArray a 1
-- [0,1]
-- >>> print $ indexArray a 5
-- [0,1,2,3,4,5]
--
-- @since 0.1.0
indexArray :: Prim a => Array a -> Int -> a
indexArray (Array a#) (I# i#) = indexByteArray# a# i#
{-# INLINE indexArray #-}



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
-- >>> ma <- newRawMArray 10 :: IO (MArray Int RW)
-- >>> sizeOfMArray ma
-- Size 10
-- >>> readMArray ma 1
-- *** Exception: undefined array element: Data.Prim.Array.Unboxed.uninitialized
--
-- @since 0.1.0

-- | Create a mutable unboxed array where each element is set to the supplied initial
-- value, which is evaluated immediately before that. See `newMArrayLazy` for an ability
-- to initialize with a thunk or `newRawArray` that will set each element to an
-- `UndefinedElement` exception.
--
-- [Unsafe size] Negative or too large of an array size can kill the current thread with
-- `HeapOverflow` asynchronous exception.
--
-- ====__Examples__
--
-- >>> newMArray 10 'A' >>= freezeMArray
-- Array "AAAAAAAAAA"
--
-- | Same as `newMArray`, except initial element is allowed to be a thunk. Prefer using
-- `newRawMArray`, instead of supplying an `error`, whenever initial element is not
-- known ahead of time. Or even better try using other creation functions that iterate
-- over an array and overwrite each element, such as `makeMArray`.
--
-- [Unsafe size] Negative or too large of an array size can kill the current thread with `HeapOverflow`
-- asynchronous exception.
--
-- @since 0.1.0
newRawMArray :: forall a m s . (Prim a, MonadPrim s m) => Size -> m (MArray a s)
newRawMArray n =
  let c = coerce n :: Count a
   in prim $ \s ->
        case newByteArray# (fromCount# c) s of
          (# s', ma# #) -> (# s', MArray ma# #)
{-# INLINE newRawMArray #-}

newMArray :: (Prim a, MonadPrim s m) => Size -> a -> m (MArray a s)
newMArray = I.newMArray
{-# INLINE newMArray #-}


-- | Get the size of a mutable unboxed array
--
-- >>> ma <- newMArray 1024 "Element of each cell"
-- >>> sizeOfMArray ma
-- Size 1024
--
-- @since 0.1.0
getSizeOfMArray ::
     forall a m s. (Prim a, MonadPrim s m)
  => MArray a s
  -> m Size
getSizeOfMArray (MArray ma#) =
  prim $ \s ->
    case getSizeofMutableByteArray# ma# s of
      (# s', n# #) -> (# s', coerce (countSize (I# n#) :: Count a) #)
{-# INLINE getSizeOfMArray #-}


-- | Read an element from mutable unboxed array at a supplied index.
--
-- [Unsafe index] Negative or larger than array size can fail with unchecked exception
--
-- ==== __Examples__
--
-- >>> ma <- makeMArray 10 (pure . ("Element ix: " ++) . show)
-- >>> readMArray ma 5
-- "Element ix: 5"
--
-- @since 0.1.0
readMArray :: (Prim a, MonadPrim s m) => MArray a s -> Int -> m a
readMArray (MArray ma#) (I# i#) = prim (readMutableByteArray# ma# i#)
{-# INLINE readMArray #-}

-- | Write an element into a mutable unboxed array at a supplied index strictly. An
-- element will be evaluated to WHNF.
--
-- [Unsafe index] Negative or larger than array size can fail with unchecked exception
--
-- ==== __Examples__
--
-- >>> ma <- newMArray 4 (Nothing :: Maybe Int)
-- >>> writeMArray ma 2 (Just 2)
-- >>> freezeMArray ma
-- Array [Nothing,Nothing,Just 2,Nothing]
--
-- Important to note that an element is evaluated prior to being written into a cell, so
-- it will not overwrite a value with if it evaluates to an exception:
--
-- >>> import Control.Exception
-- >>> writeMArray ma 2 (throw DivideByZero)
-- *** Exception: divide by zero
-- >>> freezeMArray ma
-- Array [Nothing,Nothing,Just 2,Nothing]
--
-- But it is evaluated to Normal Form, so it is still possible to write something that
-- eventually evaluates to bottom.
--
-- >>> writeMArray ma 3 (Just (7 `div` 0 ))
-- >>> freezeMArray ma
-- Array [Nothing,Nothing,Just 2,Just *** Exception: divide by zero
--
-- Either `deepseq` or `writeMArrayDeep` can be used to alleviate that.
--
-- @since 0.1.0
writeMArray :: (Prim a, MonadPrim s m) => MArray a s -> Int -> a -> m ()
writeMArray (MArray ma#) (I# i#) a = prim_ (writeMutableByteArray# ma# i# a)
{-# INLINE writeMArray #-}

-- | Convert a pure immutable unboxed array into a mutable unboxed array. See
-- `thawCopyArray` for a safer alternative.
--
-- [Unsafe array] There is no copying being done, therefore any modification done to the
-- mutable array will be reflected in the immutable as well.
--
-- ====__Examples__
--
-- The correct way to use it is with combining it with something like `cloneArray` or
-- `copyArray`, in order to preserve referential transperancy
--
-- >>> let a = fromListArray [1 .. 5 :: Int]
-- >>> ma <- thawArray $ cloneArray a 0 (sizeOfArray a)
-- >>> writeMArray ma 1 10
-- >>> freezeMArray ma
-- Array [1,10,3,4,5]
-- >>> print a
-- Array [1,2,3,4,5]
--
-- Be careful not mutate pure immutable arrays, that are still being referenced in some pure
-- setting.
--
-- >>> ma' <- thawArray a
-- >>> writeMArray ma' 0 100000
-- >>> print a
-- Array [100000,2,3,4,5]
--
-- @since 0.1.0
thawArray :: MonadPrim s m => Array a -> m (MArray a s)
thawArray (Array a#) = prim $ \s ->
  case thawByteArray# a# s of
    (# s', ma# #) -> (# s', MArray ma# #)
{-# INLINE thawArray #-}

thawCopyArray :: (Prim a, MonadPrim s m) => Array a -> Int -> Size -> m (MArray a s)
thawCopyArray = I.thawCopyArray
{-# INLINE thawCopyArray #-}

-- | Convert a mutable unboxed array into an immutable.
--
-- [Unsafe further mutation] After the mutable array is frozen, it is unsafe to mutate
-- it, because the changes will be reflected in the newly created immutable array as well.
--
-- See `freezeCopyMArray` for a safer alternative or use `cloneMArray` prior to
-- freezing if further mutation of an array is still needed.
--
-- @since 0.1.0
freezeMArray :: MonadPrim s m => MArray a s -> m (Array a)
freezeMArray (MArray ma#) = prim $ \s ->
  case unsafeFreezeByteArray# ma# s of
    (# s', a# #) -> (# s', Array a# #)
{-# INLINE freezeMArray #-}

freezeCopyMArray :: (Prim a, MonadPrim s m) => MArray a s -> Int -> Size -> m (Array a)
freezeCopyMArray = I.freezeCopyMArray
{-# INLINE freezeCopyMArray #-}


-- | Make an exact copy of a subsection of a pure immutable array.
---
-- [Unsafe offset] Offset cannot be negative or larger than the size of an array,
-- otherwise it can result in an unchecked exception
--
-- [Unsafe new size] Number of elements to be copied cannot be larger than the size of an
-- array minus the offset.
--
-- ====__Examples__
--
-- >>> let a = fromListArray ['a'..'z']
-- >>> a
-- Array "abcdefghijklmnopqrstuvwxyz"
-- >>> cloneArray a 23 3
-- Array "xyz"
--
-- @since 0.1.0
cloneArray :: Prim a => Array a -> Int -> Size -> Array a
cloneArray = I.cloneArray
{-# INLINE cloneArray #-}

-- | Same as `cloneArray`, except it works on mutable arrays
--
-- [Unsafe offset] Offset cannot be negative or larger than the size of an array,
-- otherwise it can result in an unchecked exception
--
-- [Unsafe new size] Number of elements to be copied cannot be larger than the size of an
-- array minus the offset.
--
-- @since 0.1.0
cloneMArray :: (Prim a, MonadPrim s m) => MArray a s -> Int -> Size -> m (MArray a s)
cloneMArray = I.cloneMArray
{-# INLINE cloneMArray #-}


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
     forall a m s. (Prim a, MonadPrim s m)
  => Array a -- ^ Source immutable array
  -> Int -- ^ Offset into the source immutable array
  -> MArray a s -- ^ Destination mutable array
  -> Int -- ^ Offset into the destination mutable array
  -> Size -- ^ Number of elements to copy over
  -> m ()
copyArray (Array src#) srcOff (MArray dst#) dstOff n =
  let srcOff# = fromOff# (coerce srcOff :: Off a)
      dstOff# = fromOff# (coerce dstOff :: Off a)
      n# = fromCount# (coerce n :: Count a)
  in prim_ (copyByteArray# src# srcOff# dst# dstOff# n#)
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
moveMArray ::
     forall a m s. (Prim a, MonadPrim s m)
  => MArray a s -- ^ Source mutable array
  -> Int -- ^ Offset into the source mutable array
  -> MArray a s -- ^ Destination mutable array
  -> Int -- ^ Offset into the destination mutable array
  -> Size -- ^ Number of elements to copy over
  -> m ()
moveMArray (MArray src#) srcOff (MArray dst#) dstOff n =
  let srcOff# = fromOff# (coerce srcOff :: Off a)
      dstOff# = fromOff# (coerce dstOff :: Off a)
      n# = fromCount# (coerce n :: Count a)
  in prim_ (copyMutableByteArray# src# srcOff# dst# dstOff# n#)
{-# INLINE moveMArray #-}

setMArray ::
     (Prim a, MonadPrim s m)
  => MArray a s -- ^ Mutable array
  -> Int -- ^ Offset into the mutable array
  -> Size -- ^ Number of elements to overwrite
  -> a -- ^ Element to overwrite the cells with
  -> m ()
setMArray (MArray ma#) (I# o#) (Size (I# n#)) a =
  prim_ (setMutableByteArray# ma# o# n# a)
{-# INLINE setMArray #-}


-- -- | Compare-and-swap operation that can be used as a concurrency primitive for
-- -- implementing atomic operations on the mutable array. Returns a boolean value, which
-- -- indicates `True` for success and `False` otherwise for the update, as well as the
-- -- current value at the supplied index. In case of success current value returned will
-- -- be the newly supplied one, otherwise it will still be the old one. Note that there is
-- -- no `Eq` constraint on the element, that is because compare operation is done on a
-- -- thunk reference reference, not the value itself, in other words the expected value
-- -- must be the exact same one.
-- --
-- -- [Unsafe index] Negative or larger than array size can fail with unchecked exception
-- --
-- -- ====__Examples__
-- --
-- -- >>> ma <- makeMArray 5 (pure . (*10))
-- -- >>> freezeMArray ma
-- -- Array [0,10,20,30,40]
-- --
-- -- A possible mistake is to try and pass the expected value, instead of an actual element:
-- --
-- -- >>> casMArray ma 2 20 1000
-- -- (False,20)
-- -- >>> freezeMArray ma
-- -- Array [0,10,20,30,40]
-- --
-- -- But this will get us nowhere, since what we really need is the actual reference to the
-- -- value currently in the array cell
-- --
-- -- >>> expected <- readMArray ma 2
-- -- >>> r@(_, currentValue) <- casMArray ma 2 expected 1000
-- -- >>> freezeMArray ma
-- -- Array [0,10,1000,30,40]
-- -- >>> r
-- -- (True,1000)
-- --
-- -- In a concurrent setting current value can potentially be modified by some other
-- -- thread, therefore returned value can be immedieately used as the expected one to the
-- -- next call, if we don want to retry the atomic modification:
-- --
-- -- >>> casMArray ma 2 currentValue 2000
-- -- (True,2000)
-- -- >>> freezeMArray ma
-- -- Array [0,10,2000,30,40]
-- --
-- -- @since 0.1.0
-- casMArray ::
--      MonadPrim s m
--   => MArray a s -- ^ Mutable array to mutate
--   -> Int -- ^ Index at which the cell should be set to the new value
--   -> a -- ^ Reference to the expected unboxed value
--   -> a -- ^ New value to update the cell with
--   -> m (Bool, a)
-- casMArray (MArray ma#) (I# i#) expected new =
--   prim $ \s ->
--     case casArray# ma# i# expected new s of
--       (# s', failed#, actual #) -> (# s', (isTrue# (failed# ==# 0#), actual) #)
-- {-# INLINE casMArray #-}


-- atomicModifyMArray# :: MonadPrim s m => MArray a s -> Int -> (a -> (# a, b #)) -> m b
-- atomicModifyMArray# ma@(MArray ma#) i@(I# i#) f = do
--   current0 <- readMArray ma i
--   prim $
--     let go expected s =
--           case f expected of
--             (# new, artifact #) ->
--               case casArray# ma# i# expected new s of
--                 (# s', 0#, _ #) -> (# s', artifact #)
--                 (# s', _, actual #) -> go actual s'
--      in go current0
-- {-# INLINE atomicModifyMArray# #-}


-- atomicModifyFetchMArray :: MonadPrim s m => MArray a s -> Int -> (a -> a) -> m a
-- atomicModifyFetchMArray ma i f =
--   atomicModifyMArray# ma i (\a -> let a' = f a in (# a', a' #))
-- {-# INLINE atomicModifyFetchMArray #-}

-- -- atomicModifyFetchMArray ma@(MArray ma#) i@(I# i#) f = do
-- --   current0 <- readMArray ma i
-- --   prim $ \s0 ->
-- --     let go expected s =
-- --           case casArray# ma# i# expected (f expected) s of
-- --             (# s', 0#, actual #) -> go actual s'
-- --             (# s', _, current #) -> (# s', current #)
-- --     in go current0 s0
--   -- let go e =
--   --       casMArray ma i e (f e) >>= \case
--   --         (True, new) -> pure new
--   --         (_, current) -> go current
--   --  in readMArray ma i >>= go

-- atomicFetchModifyMArray :: MonadPrim s m => MArray a s -> Int -> (a -> a) -> m a
-- atomicFetchModifyMArray ma i f =
--   atomicModifyMArray# ma i (\a -> (# f a, a #))
-- {-# INLINE atomicFetchModifyMArray #-}
--   -- let go e =
--   --       casMArray ma i e (f e) >>= \case
--   --         (True, _new) -> pure e
--   --         (_, current) -> go current
--   --  in readMArray ma i >>= go



-- atomicModifyMArray :: MonadPrim s m => MArray a s -> Int -> (a -> (a, b)) -> m b
-- atomicModifyMArray ma i f =
--   atomicModifyMArray# ma i (\a -> let (a', b) = f a in (# a', b #))
-- {-# INLINE atomicModifyMArray #-}
--   -- let go e =
--   --       let (new, artifact) = f e
--   --        in casMArray ma i e new >>= \case
--   --             (True, _new) -> pure artifact
--   --             (_, current) -> go current
--   --  in readMArray ma i >>= go


-- atomicModifyMArray_ :: MonadPrim s m => MArray a s -> Int -> (a -> a) -> m ()
-- atomicModifyMArray_ ma i f =
--   atomicModifyMArray# ma i (\a -> let a' = f a in (# a', () #))
-- {-# INLINE atomicModifyMArray_ #-}


-- atomicModifyMArray2 :: MonadPrim s m => MArray a s -> Int -> (a -> (a, b)) -> m (a, a, b)
-- atomicModifyMArray2 ma i f =
--   atomicModifyMArray# ma i (\a -> let (a', b) = f a in (# a', (a, a', b) #))
-- {-# INLINE atomicModifyMArray2 #-}

-- | Convert a list into an array strictly, i.e. each element is evaluated to WHNF prior
-- to being written into the newly created array. In order to allocate the array ahead
-- of time, the spine of a list will be evaluated first, in order to get the total
-- number of elements. Infinite lists will cause the program to halt. On the other hand
-- if the length of a list is known ahead of time, `fromListArrayN` can be used instead as
-- optimization.
--
-- @since 0.1.0
fromListArray :: Prim a => [a] -> Array a
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
     Prim a
  => Size -- ^ Expected @n@ size of a list
  -> [a]
  -> Array a
fromListArrayN = I.fromListArrayN
{-# INLINE fromListArrayN #-}

-- | Convert a pure boxed array into a list. It should work fine with GHC built-in list
-- fusion.
--
-- @since 0.1.0
toListArray :: Prim a => Array a -> [a]
toListArray = I.toListArray
{-# INLINE toListArray #-}

-- | Strict right fold
foldrArray :: Prim a => (a -> b -> b) -> b -> Array a -> b
foldrArray = I.foldrArray
{-# INLINE foldrArray #-}

makeArray :: Prim a => Size -> (Int -> a) -> Array a
makeArray = I.makeArray
{-# INLINE makeArray #-}

makeArrayM :: (Prim a, MonadPrim s m) => Size -> (Int -> m a) -> m (Array a)
makeArrayM = I.makeArrayM
{-# INLINE makeArrayM #-}

createArrayM :: (Prim a, MonadPrim s m) => Size -> (MArray a s -> m b) -> m (b, Array a)
createArrayM = I.createArrayM
{-# INLINE createArrayM #-}

createArrayM_ :: (Prim a, MonadPrim s m) => Size -> (MArray a s -> m b) -> m (Array a)
createArrayM_ = I.createArrayM_
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
makeMArray :: (Prim a, MonadPrim s m) => Size -> (Int -> m a) -> m (MArray a s)
makeMArray = I.makeMArray
{-# INLINE makeMArray #-}

-- | Traverse an array with a monadic action.
--
-- @since 0.1.0
traverseArray :: (Prim a, Prim b, MonadPrim s m) => (a -> m b) -> Array a -> m (Array b)
traverseArray = I.traverseArray
{-# INLINE traverseArray #-}
