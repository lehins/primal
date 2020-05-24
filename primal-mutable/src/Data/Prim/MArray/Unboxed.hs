{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Data.Prim.MArray.Unboxed
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.MArray.Unboxed
  ( UArray(..)
  , MUArray(..)
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
  -- * Bytes (unsafe)
  , toBytes
  , fromBytes
  , toMBytes
  , fromMBytes
  -- * List
  , fromListArray
  , fromListArrayN
  , toListArray
  -- * Helpers
  , foldrArray
  , traverseArray
  ) where

import Control.Prim.Monad
import Data.Bits
import Data.Prim
import Data.Prim.Class
import qualified Data.Prim.MArray.Internal as I
import Data.Prim.Memory.ByteArray
import Data.Prim.Memory.Bytes
import Data.Prim.MRef.Atomic
import Data.Prim.MRef.Internal
import Foreign.Prim

instance (Prim e, Show e) => Show (UArray e) where
  showsPrec n arr
    | n > 1 = ('(' :) . inner . (')' :)
    | otherwise = inner
    where
      inner = ("Array " ++) . shows (toList arr)

instance Prim e => IsList (UArray e) where
  type Item (UArray e) = e
  fromList = I.fromListArray
  fromListN n = I.fromListArrayN (Size n)
  toList = I.toListArray

data MUArray e s = MUArray (MutableByteArray# s)


-- | Check if both of the arrays refer to the exact same one. None of the elements are
-- evaluated.
instance Eq (MUArray e s) where
  MUArray ma1# == MUArray ma2# = isTrue# (sameMutableByteArray# ma1# ma2#)

data UArray e = UArray ByteArray#


instance Prim e => MRef (MUArray e) where
  type Elt (MUArray e) = e
  newRawMRef = newRawMArray 1
  {-# INLINE newRawMRef #-}
  readMRef uma = readMArray uma 0
  {-# INLINE readMRef #-}
  writeMRef uma = writeMArray uma 0
  {-# INLINE writeMRef #-}


instance Atomic e => AtomicMRef (MUArray e) where
  atomicReadMRef mba = atomicReadMBytes (toMBytes mba) (0 :: Off e)
  {-# INLINE atomicReadMRef #-}
  atomicWriteMRef mba = atomicWriteMBytes (toMBytes mba) (0 :: Off e)
  {-# INLINE atomicWriteMRef #-}
  casMRef mba = casBoolFetchMBytes (toMBytes mba) (0 :: Off e)
  {-# INLINE casMRef #-}
  atomicModifyMRef mba = atomicModifyMBytes (toMBytes mba) (0 :: Off e)
  {-# INLINE atomicModifyMRef #-}


instance (Num e, AtomicCount e) => AtomicCountMRef (MUArray e) where
  atomicAddFetchOldMRef mba = atomicAddFetchOldMBytes (toMBytes mba) (0 :: Off e)
  {-# INLINE atomicAddFetchOldMRef #-}
  atomicAddFetchNewMRef mba = atomicAddFetchNewMBytes (toMBytes mba) (0 :: Off e)
  {-# INLINE atomicAddFetchNewMRef #-}
  atomicSubFetchOldMRef mba = atomicSubFetchOldMBytes (toMBytes mba) (0 :: Off e)
  {-# INLINE atomicSubFetchOldMRef #-}
  atomicSubFetchNewMRef mba = atomicSubFetchNewMBytes (toMBytes mba) (0 :: Off e)
  {-# INLINE atomicSubFetchNewMRef #-}


instance (Bits e, AtomicBits e) => AtomicBitsMRef (MUArray e) where
  atomicAndFetchOldMRef mba = atomicAndFetchOldMBytes (toMBytes mba) (0 :: Off e)
  {-# INLINE atomicAndFetchOldMRef #-}
  atomicAndFetchNewMRef mba = atomicAndFetchNewMBytes (toMBytes mba) (0 :: Off e)
  {-# INLINE atomicAndFetchNewMRef #-}
  atomicNandFetchOldMRef mba = atomicNandFetchOldMBytes (toMBytes mba) (0 :: Off e)
  {-# INLINE atomicNandFetchOldMRef #-}
  atomicNandFetchNewMRef mba = atomicNandFetchNewMBytes (toMBytes mba) (0 :: Off e)
  {-# INLINE atomicNandFetchNewMRef #-}
  atomicOrFetchOldMRef mba = atomicOrFetchOldMBytes (toMBytes mba) (0 :: Off e)
  {-# INLINE atomicOrFetchOldMRef #-}
  atomicOrFetchNewMRef mba = atomicOrFetchNewMBytes (toMBytes mba) (0 :: Off e)
  {-# INLINE atomicOrFetchNewMRef #-}
  atomicXorFetchOldMRef mba = atomicXorFetchOldMBytes (toMBytes mba) (0 :: Off e)
  {-# INLINE atomicXorFetchOldMRef #-}
  atomicXorFetchNewMRef mba = atomicXorFetchNewMBytes (toMBytes mba) (0 :: Off e)
  {-# INLINE atomicXorFetchNewMRef #-}
  atomicNotFetchOldMRef mba = atomicNotFetchOldMBytes (toMBytes mba) (0 :: Off e)
  {-# INLINE atomicNotFetchOldMRef #-}
  atomicNotFetchNewMRef mba = atomicNotFetchNewMBytes (toMBytes mba) (0 :: Off e)
  {-# INLINE atomicNotFetchNewMRef #-}


instance Prim e => I.MArray (MUArray e) where
  type Array (MUArray e) = UArray e
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
toByteArray :: UArray e -> ByteArray 'Inc e
toByteArray (UArray a#) = ByteArray (fromByteArray# a#)
{-# INLINE toByteArray #-}

-- | /O(1)/ - Cast a `ByteArray` into an unboxed array
--
-- @since 0.1.0
fromByteArray :: ByteArray p e -> UArray e
fromByteArray (ByteArray ba) = UArray (toByteArray# ba)
{-# INLINE fromByteArray #-}


-- | /O(1)/ - Cast a mutable unboxed array into a `MByteArray`
--
-- @since 0.1.0
toMByteArray :: MUArray e s -> MByteArray 'Inc e s
toMByteArray (MUArray mba#) = MByteArray (fromMutableByteArray# mba#)
{-# INLINE toMByteArray #-}

-- | /O(1)/ - Cast an `MByteArray` into a mutable unboxed array
--
-- @since 0.1.0
fromMByteArray :: MByteArray p e s -> MUArray e s
fromMByteArray (MByteArray mb) = MUArray (toMutableByteArray# mb)
{-# INLINE fromMByteArray #-}


-- | /O(1)/ - Cast an unboxed array into a `Bytes`
--
-- @since 0.1.0
toBytes :: UArray e -> Bytes 'Inc
toBytes (UArray ba#) = fromByteArray# ba#
{-# INLINE toBytes #-}

-- | /O(1)/ - Cast a `Bytes` into an unboxed array
--
-- @since 0.1.0
fromBytes :: Bytes p -> UArray e
fromBytes b = UArray (toByteArray# b)
{-# INLINE fromBytes #-}

-- | /O(1)/ - Cast a mutable unboxed array into a `MByteArray`
--
-- @since 0.1.0
toMBytes :: MUArray e s -> MBytes 'Inc s
toMBytes (MUArray a#) = fromMutableByteArray# a#
{-# INLINE toMBytes #-}

-- | /O(1)/ - Cast an `MByteArray` into a mutable unboxed array
--
-- @since 0.1.0
fromMBytes :: MBytes p s -> MUArray e s
fromMBytes mb = MUArray (toMutableByteArray# mb)
{-# INLINE fromMBytes #-}



sizeOfArray :: UArray e -> Size
sizeOfArray (UArray a#) = Size (I# (sizeofByteArray# a#))
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
indexArray :: Prim e => UArray e -> Int -> e
indexArray (UArray a#) (I# i#) = indexByteArray# a# i#
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
-- UArray "AAAAAAAAAA"
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
newRawMArray :: forall e m s . (Prim e, MonadPrim s m) => Size -> m (MUArray e s)
newRawMArray n =
  prim $ \s ->
    case newByteArray# (fromCount# (coerce n :: Count e)) s of
      (# s', ma# #) -> (# s', MUArray ma# #)
{-# INLINE newRawMArray #-}

newMArray :: (Prim e, MonadPrim s m) => Size -> e -> m (MUArray e s)
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
  => MUArray e s
  -> m Size
getSizeOfMArray (MUArray ma#) =
  prim $ \s ->
    case getSizeofMutableByteArray# ma# s of
      (# s', n# #) -> (# s', coerce (fromByteCount (Count (I# n#)) :: Count e) #)
{-# INLINE getSizeOfMArray #-}


shrinkMArray ::
     forall e m s. (MonadPrim s m, Prim e)
  => MUArray e s
  -> Size
  -> m ()
shrinkMArray (MUArray mb#) sz =
  prim_ (shrinkMutableByteArray# mb# (fromCount# (coerce sz :: Count e)))
{-# INLINE shrinkMArray #-}

resizeMArray ::
     forall e m s. (MonadPrim s m, Prim e)
  => MUArray e s
  -> Size
  -> m (MUArray e s)
resizeMArray (MUArray mb#) sz =
  prim $ \s ->
    case resizeMutableByteArray# mb# (fromCount# (coerce sz :: Count e)) s of
      (# s', mb'# #) -> (# s', MUArray mb'# #)
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
readMArray :: (Prim e, MonadPrim s m) => MUArray e s -> Int -> m e
readMArray (MUArray ma#) (I# i#) = prim (readMutableByteArray# ma# i#)
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
-- UArray [Nothing,Nothing,Just 2,Nothing]
--
-- Important to note that an element is evaluated prior to being written into a cell, so
-- it will not overwrite a value with if it evaluates to an exception:
--
-- >>> import Control.Exception
-- >>> writeMArray ma 2 (throw DivideByZero)
-- *** Exception: divide by zero
-- >>> freezeMArray ma
-- UArray [Nothing,Nothing,Just 2,Nothing]
--
-- But it is evaluated to Normal Form, so it is still possible to write something that
-- eventually evaluates to bottom.
--
-- >>> writeMArray ma 3 (Just (7 `div` 0 ))
-- >>> freezeMArray ma
-- UArray [Nothing,Nothing,Just 2,Just *** Exception: divide by zero
--
-- Either `deepseq` or `writeMArrayDeep` can be used to alleviate that.
--
-- @since 0.1.0
writeMArray :: (Prim e, MonadPrim s m) => MUArray e s -> Int -> e -> m ()
writeMArray (MUArray ma#) (I# i#) a = prim_ (writeMutableByteArray# ma# i# a)
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
-- UArray [1,10,3,4,5]
-- >>> print a
-- UArray [1,2,3,4,5]
--
-- Be careful not mutate pure immutable arrays, that are still being referenced in some pure
-- setting.
--
-- >>> ma' <- thawArray a
-- >>> writeMArray ma' 0 100000
-- >>> print a
-- UArray [100000,2,3,4,5]
--
-- @since 0.1.0
thawArray :: MonadPrim s m => UArray e -> m (MUArray e s)
thawArray (UArray a#) = prim $ \s ->
  case unsafeThawByteArray# a# s of
    (# s', ma# #) -> (# s', MUArray ma# #)
{-# INLINE thawArray #-}

thawCopyArray :: (Prim e, MonadPrim s m) => UArray e -> Int -> Size -> m (MUArray e s)
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
freezeMArray :: MonadPrim s m => MUArray e s -> m (UArray e)
freezeMArray (MUArray ma#) = prim $ \s ->
  case unsafeFreezeByteArray# ma# s of
    (# s', a# #) -> (# s', UArray a# #)
{-# INLINE freezeMArray #-}

freezeCopyMArray :: (Prim e, MonadPrim s m) => MUArray e s -> Int -> Size -> m (UArray e)
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
-- UArray "abcdefghijklmnopqrstuvwxyz"
-- >>> cloneArray a 23 3
-- UArray "xyz"
--
-- @since 0.1.0
cloneArray :: Prim e => UArray e -> Int -> Size -> UArray e
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
cloneMArray :: (Prim e, MonadPrim s m) => MUArray e s -> Int -> Size -> m (MUArray e s)
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
  => UArray e -- ^ Source immutable array
  -> Int -- ^ Offset into the source immutable array
  -> MUArray e s -- ^ Destination mutable array
  -> Int -- ^ Offset into the destination mutable array
  -> Size -- ^ Number of elements to copy over
  -> m ()
copyArray (UArray src#) srcOff (MUArray dst#) dstOff n =
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
  => MUArray e s -- ^ Source mutable array
  -> Int -- ^ Offset into the source mutable array
  -> MUArray e s -- ^ Destination mutable array
  -> Int -- ^ Offset into the destination mutable array
  -> Size -- ^ Number of elements to copy over
  -> m ()
moveMArray (MUArray src#) srcOff (MUArray dst#) dstOff n =
  let srcOff# = fromOff# (coerce srcOff :: Off e)
      dstOff# = fromOff# (coerce dstOff :: Off e)
      n# = fromCount# (coerce n :: Count e)
  in prim_ (copyMutableByteArray# src# srcOff# dst# dstOff# n#)
{-# INLINE moveMArray #-}

setMArray ::
     (Prim e, MonadPrim s m)
  => MUArray e s -- ^ Mutable array
  -> Int -- ^ Offset into the mutable array
  -> Size -- ^ Number of elements to overwrite
  -> e -- ^ Element to overwrite the cells with
  -> m ()
setMArray (MUArray ma#) (I# o#) (Size (I# n#)) a =
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
-- -- UArray [0,10,20,30,40]
-- --
-- -- A possible mistake is to try and pass the expected value, instead of an actual element:
-- --
-- -- >>> casMArray ma 2 20 1000
-- -- (False,20)
-- -- >>> freezeMArray ma
-- -- UArray [0,10,20,30,40]
-- --
-- -- But this will get us nowhere, since what we really need is the actual reference to the
-- -- value currently in the array cell
-- --
-- -- >>> expected <- readMArray ma 2
-- -- >>> r@(_, currentValue) <- casMArray ma 2 expected 1000
-- -- >>> freezeMArray ma
-- -- UArray [0,10,1000,30,40]
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
-- -- UArray [0,10,2000,30,40]
-- --
-- -- @since 0.1.0
-- casMArray ::
--      MonadPrim s m
--   => MArray a s -- ^ Mutable array to mutate
--   -> Int -- ^ Index at which the cell should be set to the new value
--   -> a -- ^ Reference to the expected unboxed value
--   -> a -- ^ New value to update the cell with
--   -> m (Bool, a)
-- casMArray (MUArray ma#) (I# i#) expected new =
--   prim $ \s ->
--     case casArray# ma# i# expected new s of
--       (# s', failed#, actual #) -> (# s', (isTrue# (failed# ==# 0#), actual) #)
-- {-# INLINE casMArray #-}


-- atomicModifyMArray# :: MonadPrim s m => MArray a s -> Int -> (a -> (# a, b #)) -> m b
-- atomicModifyMArray# ma@(MUArray ma#) i@(I# i#) f = do
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

-- -- atomicModifyFetchNewMArray ma@(MUArray ma#) i@(I# i#) f = do
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
fromListArray :: Prim e => [e] -> UArray e
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
-- UArray [1,2,3]
-- >>> fromListArrayN 3 [1 :: Int ..]
-- UArray [1,2,3]
-- >>> fromListArrayN 3 [1 :: Int, 2]
-- UArray [1,2*** Exception: undefined array element: Data.Prim.Array.Boxed.uninitialized
--
-- @since 0.1.0
fromListArrayN ::
     Prim e
  => Size -- ^ Expected @n@ size of a list
  -> [e]
  -> UArray e
fromListArrayN = I.fromListArrayN
{-# INLINE fromListArrayN #-}

-- | Convert a pure boxed array into a list. It should work fine with GHC built-in list
-- fusion.
--
-- @since 0.1.0
toListArray :: Prim e => UArray e -> [e]
toListArray = I.toListArray
{-# INLINE toListArray #-}

-- | Strict right fold
foldrArray :: Prim e => (e -> b -> b) -> b -> UArray e -> b
foldrArray = I.foldrArray
{-# INLINE foldrArray #-}

makeArray :: Prim e => Size -> (Int -> e) -> UArray e
makeArray = I.makeArray
{-# INLINE makeArray #-}

makeArrayM :: (Prim e, MonadPrim s m) => Size -> (Int -> m e) -> m (UArray e)
makeArrayM = I.makeArrayM
{-# INLINE makeArrayM #-}

createArrayM :: (Prim e, MonadPrim s m) => Size -> (MUArray e s -> m b) -> m (b, UArray e)
createArrayM = I.createArrayM
{-# INLINE createArrayM #-}

createArrayM_ :: (Prim e, MonadPrim s m) => Size -> (MUArray e s -> m b) -> m (UArray e)
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
makeMArray :: (Prim e, MonadPrim s m) => Size -> (Int -> m e) -> m (MUArray e s)
makeMArray = I.makeMArray
{-# INLINE makeMArray #-}

-- | Traverse an array with a monadic action.
--
-- @since 0.1.0
traverseArray :: (Prim e, Prim b, MonadPrim s m) => (e -> m b) -> UArray e -> m (UArray b)
traverseArray = I.traverseArray
{-# INLINE traverseArray #-}
