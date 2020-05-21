{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
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
  , pattern Array
  , UnboxedArray
  , MArray
  , pattern MArray
  , UnboxedMArray
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
  -- , atomicModifyFetchNewMArray
  -- , atomicModifyFetchOldMArray
  -- , atomicModifyMArray
  -- , atomicModifyMArray_
  -- , atomicModifyMArray2
  -- *
  , shrinkMArray
  , resizeMArray
  , thawArray
  , thawCopyArray
  , freezeMArray
  , freezeCopyMArray
  , copyArray
  , moveMArray
  , cloneArray
  , cloneMArray
  -- * ByteArray
  , toByteArray
  , fromByteArray
  , toMByteArray
  , fromMByteArray
  -- * List
  , fromListArray
  , fromListArrayN
  , toListArray
  -- * Helpers
  , foldrArray
  , traverseArray
  ) where

import Control.DeepSeq
import Control.Exception (ArrayException(UndefinedElement), throw)
import Control.Monad.ST
import Control.Prim.Monad
import Data.Prim
import Data.Prim.Class
import Data.Prim.Memory.Bytes
import Data.Prim.Memory.ByteArray
import Data.Prim.Array.Internal (Size(..))
import qualified Data.Prim.Array.Internal as I
import Foreign.Prim

type Array e = UnboxedArray e

instance (Prim e, Show e) => Show (UnboxedArray e) where
  showsPrec n arr
    | n > 1 = ('(' :) . inner . (')' :)
    | otherwise = inner
    where
      inner = ("Array " ++) . shows (toList arr)

instance Prim e => IsList (UnboxedArray e) where
  type Item (Array e) = e
  fromList = I.fromListArray
  fromListN n = I.fromListArrayN (Size n)
  toList = I.toListArray

data UnboxedMArray e s = MArray (MutableByteArray# s)

type MArray e s = UnboxedMArray e s


-- | Check if both of the arrays refer to the exact same one. None of the elements are
-- evaluated.
instance Eq (UnboxedMArray e s) where
  MArray ma1# == MArray ma2# = isTrue# (sameMutableByteArray# ma1# ma2#)

data UnboxedArray e = Array ByteArray#

instance Prim e => I.Mutable (UnboxedMArray e) where
  type Frozen (UnboxedMArray e) = UnboxedArray e
  type Elt (UnboxedMArray e) = e
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
  shrinkMArray ma sz = ma <$ shrinkMArray ma sz
  {-# INLINE shrinkMArray #-}
  resizeMArray = resizeMArray
  {-# INLINE resizeMArray #-}


-- | /O(1)/ - Cast an unboxed array into a `ByteArray`
--
-- @since 0.1.0
toByteArray :: UnboxedArray e -> ByteArray 'Inc e
toByteArray (Array a#) = ByteArray (fromByteArray# a#)
{-# INLINE toByteArray #-}

-- | /O(1)/ - Cast a `ByteArray` into an unboxed array
--
-- @since 0.1.0
fromByteArray :: ByteArray p e -> UnboxedArray e
fromByteArray (ByteArray ba) = Array (toByteArray# ba)
{-# INLINE fromByteArray #-}


-- | /O(1)/ - Cast a mutable unboxed array into a `MByteArray`
--
-- @since 0.1.0
toMByteArray :: UnboxedMArray e s -> MByteArray 'Inc e s
toMByteArray (MArray a#) = MByteArray (fromMutableByteArray# a#)
{-# INLINE toMByteArray #-}

-- | /O(1)/ - Cast an `MByteArray` into a mutable unboxed array
--
-- @since 0.1.0
fromMByteArray :: MByteArray p e s -> UnboxedMArray e s
fromMByteArray (MByteArray ba) = MArray (toMutableByteArray# ba)
{-# INLINE fromMByteArray #-}



sizeOfArray :: Array e -> Size
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
indexArray :: Prim e => Array e -> Int -> e
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
newRawMArray :: forall e m s . (Prim e, MonadPrim s m) => Size -> m (MArray e s)
newRawMArray n =
  prim $ \s ->
    case newByteArray# (fromCount# (coerce n :: Count e)) s of
      (# s', ma# #) -> (# s', MArray ma# #)
{-# INLINE newRawMArray #-}

newMArray :: (Prim e, MonadPrim s m) => Size -> e -> m (MArray e s)
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
     forall e m s. (Prim e, MonadPrim s m)
  => MArray e s
  -> m Size
getSizeOfMArray (MArray ma#) =
  prim $ \s ->
    case getSizeofMutableByteArray# ma# s of
      (# s', n# #) -> (# s', coerce (fromByteCount (Count (I# n#)) :: Count e) #)
{-# INLINE getSizeOfMArray #-}


shrinkMArray ::
     forall e m s. (MonadPrim s m, Prim e)
  => MArray e s
  -> Size
  -> m ()
shrinkMArray (MArray mb#) sz =
  prim_ (shrinkMutableByteArray# mb# (fromCount# (coerce sz :: Count e)))
{-# INLINE shrinkMArray #-}

resizeMArray ::
     forall e m s. (MonadPrim s m, Prim e)
  => MArray e s
  -> Size
  -> m (MArray e s)
resizeMArray (MArray mb#) sz =
  prim $ \s ->
    case resizeMutableByteArray# mb# (fromCount# (coerce sz :: Count e)) s of
      (# s', mb'# #) -> (# s', MArray mb'# #)
{-# INLINE resizeMArray #-}


-- | Read an element from a mutable unboxed array at the supplied index.
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
readMArray :: (Prim e, MonadPrim s m) => MArray e s -> Int -> m e
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
writeMArray :: (Prim e, MonadPrim s m) => MArray e s -> Int -> e -> m ()
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
thawArray :: MonadPrim s m => Array e -> m (MArray e s)
thawArray (Array a#) = prim $ \s ->
  case unsafeThawByteArray# a# s of
    (# s', ma# #) -> (# s', MArray ma# #)
{-# INLINE thawArray #-}

thawCopyArray :: (Prim e, MonadPrim s m) => Array e -> Int -> Size -> m (MArray e s)
thawCopyArray = I.thawCopyArray
{-# INLINE thawCopyArray #-}

-- | Convert a mutable unboxed array into an immutable.
--
-- [Unsafe further mutation] After the mutable array is frozen, it is unsafe to mutate
-- it, because the changes will be reflected in the newly created immutable array as well.
--
-- See `freezeCopyMArray` for a safer alternative or use `cloneMArray` prior to
-- freezing if further mutation of an arrac
-- @since 0.1.0
freezeMArray :: MonadPrim s m => MArray e s -> m (Array e)
freezeMArray (MArray ma#) = prim $ \s ->
  case unsafeFreezeByteArray# ma# s of
    (# s', a# #) -> (# s', Array a# #)
{-# INLINE freezeMArray #-}

freezeCopyMArray :: (Prim e, MonadPrim s m) => MArray e s -> Int -> Size -> m (Array e)
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
cloneArray :: Prim e => Array e -> Int -> Size -> Array e
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
cloneMArray :: (Prim e, MonadPrim s m) => MArray e s -> Int -> Size -> m (MArray e s)
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
     forall e m s. (Prim e, MonadPrim s m)
  => Array e -- ^ Source immutable array
  -> Int -- ^ Offset into the source immutable array
  -> MArray e s -- ^ Destination mutable array
  -> Int -- ^ Offset into the destination mutable array
  -> Size -- ^ Number of elements to copy over
  -> m ()
copyArray (Array src#) srcOff (MArray dst#) dstOff n =
  let srcOff# = fromOff# (coerce srcOff :: Off e)
      dstOff# = fromOff# (coerce dstOff :: Off e)
      n# = fromCount# (coerce n :: Count e)
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
     forall e m s. (Prim e, MonadPrim s m)
  => MArray e s -- ^ Source mutable array
  -> Int -- ^ Offset into the source mutable array
  -> MArray e s -- ^ Destination mutable array
  -> Int -- ^ Offset into the destination mutable array
  -> Size -- ^ Number of elements to copy over
  -> m ()
moveMArray (MArray src#) srcOff (MArray dst#) dstOff n =
  let srcOff# = fromOff# (coerce srcOff :: Off e)
      dstOff# = fromOff# (coerce dstOff :: Off e)
      n# = fromCount# (coerce n :: Count e)
  in prim_ (copyMutableByteArray# src# srcOff# dst# dstOff# n#)
{-# INLINE moveMArray #-}

setMArray ::
     (Prim e, MonadPrim s m)
  => MArray e s -- ^ Mutable array
  -> Int -- ^ Offset into the mutable array
  -> Size -- ^ Number of elements to overwrite
  -> e -- ^ Element to overwrite the cells with
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


-- atomicModifyFetchNewMArray :: MonadPrim s m => MArray a s -> Int -> (a -> a) -> m a
-- atomicModifyFetchNewMArray ma i f =
--   atomicModifyMArray# ma i (\a -> let a' = f a in (# a', a' #))
-- {-# INLINE atomicModifyFetchNewMArray #-}

-- -- atomicModifyFetchNewMArray ma@(MArray ma#) i@(I# i#) f = do
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

-- atomicModifyFetchOldMArray :: MonadPrim s m => MArray a s -> Int -> (a -> a) -> m a
-- atomicModifyFetchOldMArray ma i f =
--   atomicModifyMArray# ma i (\a -> (# f a, a #))
-- {-# INLINE atomicModifyFetchOldMArray #-}
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
fromListArray :: Prim e => [e] -> Array e
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
     Prim e
  => Size -- ^ Expected @n@ size of a list
  -> [e]
  -> Array e
fromListArrayN = I.fromListArrayN
{-# INLINE fromListArrayN #-}

-- | Convert a pure boxed array into a list. It should work fine with GHC built-in list
-- fusion.
--
-- @since 0.1.0
toListArray :: Prim e => Array e -> [e]
toListArray = I.toListArray
{-# INLINE toListArray #-}

-- | Strict right fold
foldrArray :: Prim e => (e -> b -> b) -> b -> Array e -> b
foldrArray = I.foldrArray
{-# INLINE foldrArray #-}

makeArray :: Prim e => Size -> (Int -> e) -> Array e
makeArray = I.makeArray
{-# INLINE makeArray #-}

makeArrayM :: (Prim e, MonadPrim s m) => Size -> (Int -> m e) -> m (Array e)
makeArrayM = I.makeArrayM
{-# INLINE makeArrayM #-}

createArrayM :: (Prim e, MonadPrim s m) => Size -> (MArray e s -> m b) -> m (b, Array e)
createArrayM = I.createArrayM
{-# INLINE createArrayM #-}

createArrayM_ :: (Prim e, MonadPrim s m) => Size -> (MArray e s -> m b) -> m (Array e)
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
makeMArray :: (Prim e, MonadPrim s m) => Size -> (Int -> m e) -> m (MArray e s)
makeMArray = I.makeMArray
{-# INLINE makeMArray #-}

-- | Traverse an array with a monadic action.
--
-- @since 0.1.0
traverseArray :: (Prim e, Prim b, MonadPrim s m) => (e -> m b) -> Array e -> m (Array b)
traverseArray = I.traverseArray
{-# INLINE traverseArray #-}
