{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
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
  , makeUArray
  , makeUArrayM
  , sizeOfUArray
  , indexUArray
  -- * Mutable
  -- ** Create
  , newMUArray
  , newRawMUArray
  , makeMUArray
  , createUArrayM
  , createUArrayM_
  -- ** Access
  , readMUArray
  , writeMUArray
  -- *** Atomic
  -- , casMUArray
  -- , atomicModifyFetchNewMUArray
  -- , atomicModifyFetchOldMUArray
  -- , atomicModifyMUArray
  -- , atomicModifyMUArray_
  -- , atomicModifyMUArray2
  -- *
  , shrinkMUArray
  , resizeMUArray
  , thawUArray
  , thawCopyUArray
  , freezeMUArray
  , freezeCopyMUArray
  , copyUArray
  , moveMUArray
  , cloneUArray
  , cloneMUArray
  -- * ByteUArray
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
  , fromListUArray
  , fromListUArrayN
  , toListUArray
  -- * Helpers
  , foldrUArray
  , traverseUArray
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


-- | Check if both of the arrays refer to the exact same one through poiner equality. None
-- of the elements are evaluated.
instance Eq (MUArray e s) where
  MUArray ma1# == MUArray ma2# = isTrue# (sameMutableByteArray# ma1# ma2#)



data UArray e = UArray ByteArray#

type role UArray representational

instance Prim e => MRef (MUArray e) where
  type Elt (MUArray e) = e
  newRawMRef = newRawMUArray 1
  {-# INLINE newRawMRef #-}
  readMRef uma = readMUArray uma 0
  {-# INLINE readMRef #-}
  writeMRef uma = writeMUArray uma 0
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
  sizeOfArray = sizeOfUArray
  {-# INLINE sizeOfArray #-}
  indexArray = indexUArray
  {-# INLINE indexArray #-}
  getSizeOfMArray = getSizeOfMUArray
  {-# INLINE getSizeOfMArray #-}
  thawArray = thawUArray
  {-# INLINE thawArray #-}
  freezeMArray = freezeMUArray
  {-# INLINE freezeMArray #-}
  newRawMArray = newRawMUArray
  {-# INLINE newRawMArray #-}
  readMArray = readMUArray
  {-# INLINE readMArray #-}
  writeMArray = writeMUArray
  {-# INLINE writeMArray #-}
  copyArray = copyUArray
  {-# INLINE copyArray #-}
  moveMArray = moveMUArray
  {-# INLINE moveMArray #-}
  setMArray = setMUArray
  {-# INLINE setMArray #-}
  shrinkMArray ma sz = ma <$ shrinkMUArray ma sz
  {-# INLINE shrinkMArray #-}
  resizeMArray = resizeMUArray
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



sizeOfUArray :: UArray e -> Size
sizeOfUArray (UArray a#) = Size (I# (sizeofByteArray# a#))
{-# INLINE sizeOfUArray #-}

-- | Index an element of a pure unboxed array.
--
-- [Unsafe index] Negative or larger than array size can fail with unchecked exception
--
-- ==== __Examples__
--
-- >>> import Data.Prim.Array.Unboxed
-- >>> let a = makeUArray 1024 (\i -> [0 .. i])
-- >>> print $ indexUArray a 1
-- [0,1]
-- >>> print $ indexUArray a 5
-- [0,1,2,3,4,5]
--
-- @since 0.1.0
indexUArray :: Prim e => UArray e -> Int -> e
indexUArray (UArray a#) (I# i#) = indexByteArray# a# i#
{-# INLINE indexUArray #-}



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
-- >>> ma <- newRawMUArray 10 :: IO (MArray Int RW)
-- >>> sizeOfMUArray ma
-- Size 10
-- >>> readMUArray ma 1
-- *** Exception: undefined array element: Data.Prim.Array.Unboxed.uninitialized
--
-- @since 0.1.0

-- | Create a mutable unboxed array where each element is set to the supplied initial
-- value, which is evaluated immediately before that. See `newMUArrayLazy` for an ability
-- to initialize with a thunk or `newRawUArray` that will set each element to an
-- `UndefinedElement` exception.
--
-- [Unsafe size] Negative or too large of an array size can kill the current thread with
-- `HeapOverflow` asynchronous exception.
--
-- ====__Examples__
--
-- >>> newMUArray 10 'A' >>= freezeMUArray
-- UArray "AAAAAAAAAA"
--
-- | Same as `newMUArray`, except initial element is allowed to be a thunk. Prefer using
-- `newRawMUArray`, instead of supplying an `error`, whenever initial element is not
-- known ahead of time. Or even better try using other creation functions that iterate
-- over an array and overwrite each element, such as `makeMUArray`.
--
-- [Unsafe size] Negative or too large of an array size can kill the current thread with `HeapOverflow`
-- asynchronous exception.
--
-- @since 0.1.0
newRawMUArray :: forall e m s . (Prim e, MonadPrim s m) => Size -> m (MUArray e s)
newRawMUArray n =
  prim $ \s ->
    case newByteArray# (fromCount# (coerce n :: Count e)) s of
      (# s', ma# #) -> (# s', MUArray ma# #)
{-# INLINE newRawMUArray #-}

newMUArray :: (Prim e, MonadPrim s m) => Size -> e -> m (MUArray e s)
newMUArray = I.newMArray
{-# INLINE newMUArray #-}


-- | Get the size of a mutable unboxed array
--
-- >>> ma <- newMUArray 1024 "Element of each cell"
-- >>> sizeOfMUArray ma
-- Size 1024
--
-- @since 0.1.0
getSizeOfMUArray ::
     forall e m s. (Prim e, MonadPrim s m)
  => MUArray e s
  -> m Size
getSizeOfMUArray (MUArray ma#) =
  prim $ \s ->
    case getSizeofMutableByteArray# ma# s of
      (# s', n# #) -> (# s', coerce (fromByteCount (Count (I# n#)) :: Count e) #)
{-# INLINE getSizeOfMUArray #-}


shrinkMUArray ::
     forall e m s. (MonadPrim s m, Prim e)
  => MUArray e s
  -> Size
  -> m ()
shrinkMUArray (MUArray mb#) sz =
  prim_ (shrinkMutableByteArray# mb# (fromCount# (coerce sz :: Count e)))
{-# INLINE shrinkMUArray #-}

resizeMUArray ::
     forall e m s. (MonadPrim s m, Prim e)
  => MUArray e s
  -> Size
  -> m (MUArray e s)
resizeMUArray (MUArray mb#) sz =
  prim $ \s ->
    case resizeMutableByteArray# mb# (fromCount# (coerce sz :: Count e)) s of
      (# s', mb'# #) -> (# s', MUArray mb'# #)
{-# INLINE resizeMUArray #-}


-- | Read an element from a mutable unboxed array at the supplied index.
--
-- [Unsafe index] Negative or larger than array size can fail with unchecked exception
--
-- ==== __Examples__
--
-- >>> ma <- makeMUArray 10 (pure . ("Element ix: " ++) . show)
-- >>> readMUArray ma 5
-- "Element ix: 5"
--
-- @since 0.1.0
readMUArray :: (Prim e, MonadPrim s m) => MUArray e s -> Int -> m e
readMUArray (MUArray ma#) (I# i#) = prim (readMutableByteArray# ma# i#)
{-# INLINE readMUArray #-}

-- | Write an element into a mutable unboxed array at a supplied index strictly. An
-- element will be evaluated to WHNF.
--
-- [Unsafe index] Negative or larger than array size can fail with unchecked exception
--
-- ==== __Examples__
--
-- >>> ma <- newMUArray 4 (Nothing :: Maybe Int)
-- >>> writeMUArray ma 2 (Just 2)
-- >>> freezeMUArray ma
-- UArray [Nothing,Nothing,Just 2,Nothing]
--
-- Important to note that an element is evaluated prior to being written into a cell, so
-- it will not overwrite a value with if it evaluates to an exception:
--
-- >>> import Control.Exception
-- >>> writeMUArray ma 2 (throw DivideByZero)
-- *** Exception: divide by zero
-- >>> freezeMUArray ma
-- UArray [Nothing,Nothing,Just 2,Nothing]
--
-- But it is evaluated to Normal Form, so it is still possible to write something that
-- eventually evaluates to bottom.
--
-- >>> writeMUArray ma 3 (Just (7 `div` 0 ))
-- >>> freezeMUArray ma
-- UArray [Nothing,Nothing,Just 2,Just *** Exception: divide by zero
--
-- Either `deepseq` or `writeMUArrayDeep` can be used to alleviate that.
--
-- @since 0.1.0
writeMUArray :: (Prim e, MonadPrim s m) => MUArray e s -> Int -> e -> m ()
writeMUArray (MUArray ma#) (I# i#) a = prim_ (writeMutableByteArray# ma# i# a)
{-# INLINE writeMUArray #-}

-- | Convert a pure immutable unboxed array into a mutable unboxed array. See
-- `thawCopyUArray` for a safer alternative.
--
-- [Unsafe array] There is no copying being done, therefore any modification done to the
-- mutable array will be reflected in the immutable as well.
--
-- ====__Examples__
--
-- The correct way to use it is with combining it with something like `cloneUArray` or
-- `copyUArray`, in order to preserve referential transperancy
--
-- >>> let a = fromListUArray [1 .. 5 :: Int]
-- >>> ma <- thawUArray $ cloneUArray a 0 (sizeOfUArray a)
-- >>> writeMUArray ma 1 10
-- >>> freezeMUArray ma
-- UArray [1,10,3,4,5]
-- >>> print a
-- UArray [1,2,3,4,5]
--
-- Be careful not mutate pure immutable arrays, that are still being referenced in some pure
-- setting.
--
-- >>> ma' <- thawUArray a
-- >>> writeMUArray ma' 0 100000
-- >>> print a
-- UArray [100000,2,3,4,5]
--
-- @since 0.1.0
thawUArray :: MonadPrim s m => UArray e -> m (MUArray e s)
thawUArray (UArray a#) = prim $ \s ->
  case unsafeThawByteArray# a# s of
    (# s', ma# #) -> (# s', MUArray ma# #)
{-# INLINE thawUArray #-}

thawCopyUArray :: (Prim e, MonadPrim s m) => UArray e -> Int -> Size -> m (MUArray e s)
thawCopyUArray = I.thawCopyArray
{-# INLINE thawCopyUArray #-}

-- | Convert a mutable unboxed array into an immutable.
--
-- [Unsafe further mutation] After the mutable array is frozen, it is unsafe to mutate
-- it, because the changes will be reflected in the newly created immutable array as well.
--
-- See `freezeCopyMUArray` for a safer alternative or use `cloneMUArray` prior to
-- freezing if further mutation of an arrac
-- @since 0.1.0
freezeMUArray :: MonadPrim s m => MUArray e s -> m (UArray e)
freezeMUArray (MUArray ma#) = prim $ \s ->
  case unsafeFreezeByteArray# ma# s of
    (# s', a# #) -> (# s', UArray a# #)
{-# INLINE freezeMUArray #-}

freezeCopyMUArray :: (Prim e, MonadPrim s m) => MUArray e s -> Int -> Size -> m (UArray e)
freezeCopyMUArray = I.freezeCopyMArray
{-# INLINE freezeCopyMUArray #-}


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
-- >>> let a = fromListUArray ['a'..'z']
-- >>> a
-- UArray "abcdefghijklmnopqrstuvwxyz"
-- >>> cloneUArray a 23 3
-- UArray "xyz"
--
-- @since 0.1.0
cloneUArray :: Prim e => UArray e -> Int -> Size -> UArray e
cloneUArray = I.cloneArray
{-# INLINE cloneUArray #-}

-- | Same as `cloneUArray`, except it works on mutable arrays
--
-- [Unsafe offset] Offset cannot be negative or larger than the size of an array,
-- otherwise it can result in an unchecked exception
--
-- [Unsafe new size] Number of elements to be copied cannot be larger than the size of an
-- array minus the offset.
--
-- @since 0.1.0
cloneMUArray :: (Prim e, MonadPrim s m) => MUArray e s -> Int -> Size -> m (MUArray e s)
cloneMUArray = I.cloneMArray
{-# INLINE cloneMUArray #-}


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
copyUArray ::
     forall e m s. (Prim e, MonadPrim s m)
  => UArray e -- ^ Source immutable array
  -> Int -- ^ Offset into the source immutable array
  -> MUArray e s -- ^ Destination mutable array
  -> Int -- ^ Offset into the destination mutable array
  -> Size -- ^ Number of elements to copy over
  -> m ()
copyUArray (UArray src#) srcOff (MUArray dst#) dstOff n =
  let srcOff# = fromOff# (coerce srcOff :: Off e)
      dstOff# = fromOff# (coerce dstOff :: Off e)
      n# = fromCount# (coerce n :: Count e)
  in prim_ (copyByteArray# src# srcOff# dst# dstOff# n#)
{-# INLINE copyUArray #-}

-- | Copy a subsection of a mutable array into a subsection of another or the same
-- mutable array. Therefore, unlike `copyUArray`, memory overlap is allowed.
--
-- [Unsafe offset] Each offset cannot be negative or larger than the size of a
-- corresponding array, otherwise it can result in an unchecked exception
--
-- [Unsafe new size] Number of elements to be copied cannot be larger than the size of an
-- each array minus their corersponding offsets.
--
-- @since 0.1.0
moveMUArray ::
     forall e m s. (Prim e, MonadPrim s m)
  => MUArray e s -- ^ Source mutable array
  -> Int -- ^ Offset into the source mutable array
  -> MUArray e s -- ^ Destination mutable array
  -> Int -- ^ Offset into the destination mutable array
  -> Size -- ^ Number of elements to copy over
  -> m ()
moveMUArray (MUArray src#) srcOff (MUArray dst#) dstOff n =
  let srcOff# = fromOff# (coerce srcOff :: Off e)
      dstOff# = fromOff# (coerce dstOff :: Off e)
      n# = fromCount# (coerce n :: Count e)
  in prim_ (copyMutableByteArray# src# srcOff# dst# dstOff# n#)
{-# INLINE moveMUArray #-}

setMUArray ::
     (Prim e, MonadPrim s m)
  => MUArray e s -- ^ Mutable array
  -> Int -- ^ Offset into the mutable array
  -> Size -- ^ Number of elements to overwrite
  -> e -- ^ Element to overwrite the cells with
  -> m ()
setMUArray (MUArray ma#) (I# o#) (Size (I# n#)) a =
  prim_ (setMutableByteArray# ma# o# n# a)
{-# INLINE setMUArray #-}


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
-- -- >>> ma <- makeMUArray 5 (pure . (*10))
-- -- >>> freezeMUArray ma
-- -- UArray [0,10,20,30,40]
-- --
-- -- A possible mistake is to try and pass the expected value, instead of an actual element:
-- --
-- -- >>> casMUArray ma 2 20 1000
-- -- (False,20)
-- -- >>> freezeMUArray ma
-- -- UArray [0,10,20,30,40]
-- --
-- -- But this will get us nowhere, since what we really need is the actual reference to the
-- -- value currently in the array cell
-- --
-- -- >>> expected <- readMUArray ma 2
-- -- >>> r@(_, currentValue) <- casMUArray ma 2 expected 1000
-- -- >>> freezeMUArray ma
-- -- UArray [0,10,1000,30,40]
-- -- >>> r
-- -- (True,1000)
-- --
-- -- In a concurrent setting current value can potentially be modified by some other
-- -- thread, therefore returned value can be immedieately used as the expected one to the
-- -- next call, if we don want to retry the atomic modification:
-- --
-- -- >>> casMUArray ma 2 currentValue 2000
-- -- (True,2000)
-- -- >>> freezeMUArray ma
-- -- UArray [0,10,2000,30,40]
-- --
-- -- @since 0.1.0
-- casMUArray ::
--      MonadPrim s m
--   => MArray a s -- ^ Mutable array to mutate
--   -> Int -- ^ Index at which the cell should be set to the new value
--   -> a -- ^ Reference to the expected unboxed value
--   -> a -- ^ New value to update the cell with
--   -> m (Bool, a)
-- casMUArray (MUArray ma#) (I# i#) expected new =
--   prim $ \s ->
--     case casUArray# ma# i# expected new s of
--       (# s', failed#, actual #) -> (# s', (isTrue# (failed# ==# 0#), actual) #)
-- {-# INLINE casMUArray #-}


-- atomicModifyMUArray# :: MonadPrim s m => MArray a s -> Int -> (a -> (# a, b #)) -> m b
-- atomicModifyMUArray# ma@(MUArray ma#) i@(I# i#) f = do
--   current0 <- readMUArray ma i
--   prim $
--     let go expected s =
--           case f expected of
--             (# new, artifact #) ->
--               case casUArray# ma# i# expected new s of
--                 (# s', 0#, _ #) -> (# s', artifact #)
--                 (# s', _, actual #) -> go actual s'
--      in go current0
-- {-# INLINE atomicModifyMUArray# #-}


-- atomicModifyFetchNewMUArray :: MonadPrim s m => MArray a s -> Int -> (a -> a) -> m a
-- atomicModifyFetchNewMUArray ma i f =
--   atomicModifyMUArray# ma i (\a -> let a' = f a in (# a', a' #))
-- {-# INLINE atomicModifyFetchNewMUArray #-}

-- -- atomicModifyFetchNewMUArray ma@(MUArray ma#) i@(I# i#) f = do
-- --   current0 <- readMUArray ma i
-- --   prim $ \s0 ->
-- --     let go expected s =
-- --           case casUArray# ma# i# expected (f expected) s of
-- --             (# s', 0#, actual #) -> go actual s'
-- --             (# s', _, current #) -> (# s', current #)
-- --     in go current0 s0
--   -- let go e =
--   --       casMUArray ma i e (f e) >>= \case
--   --         (True, new) -> pure new
--   --         (_, current) -> go current
--   --  in readMUArray ma i >>= go

-- atomicModifyFetchOldMUArray :: MonadPrim s m => MArray a s -> Int -> (a -> a) -> m a
-- atomicModifyFetchOldMUArray ma i f =
--   atomicModifyMUArray# ma i (\a -> (# f a, a #))
-- {-# INLINE atomicModifyFetchOldMUArray #-}
--   -- let go e =
--   --       casMUArray ma i e (f e) >>= \case
--   --         (True, _new) -> pure e
--   --         (_, current) -> go current
--   --  in readMUArray ma i >>= go



-- atomicModifyMUArray :: MonadPrim s m => MArray a s -> Int -> (a -> (a, b)) -> m b
-- atomicModifyMUArray ma i f =
--   atomicModifyMUArray# ma i (\a -> let (a', b) = f a in (# a', b #))
-- {-# INLINE atomicModifyMUArray #-}
--   -- let go e =
--   --       let (new, artifact) = f e
--   --        in casMUArray ma i e new >>= \case
--   --             (True, _new) -> pure artifact
--   --             (_, current) -> go current
--   --  in readMUArray ma i >>= go


-- atomicModifyMUArray_ :: MonadPrim s m => MArray a s -> Int -> (a -> a) -> m ()
-- atomicModifyMUArray_ ma i f =
--   atomicModifyMUArray# ma i (\a -> let a' = f a in (# a', () #))
-- {-# INLINE atomicModifyMUArray_ #-}


-- atomicModifyMUArray2 :: MonadPrim s m => MArray a s -> Int -> (a -> (a, b)) -> m (a, a, b)
-- atomicModifyMUArray2 ma i f =
--   atomicModifyMUArray# ma i (\a -> let (a', b) = f a in (# a', (a, a', b) #))
-- {-# INLINE atomicModifyMUArray2 #-}

-- | Convert a list into an array strictly, i.e. each element is evaluated to WHNF prior
-- to being written into the newly created array. In order to allocate the array ahead
-- of time, the spine of a list will be evaluated first, in order to get the total
-- number of elements. Infinite lists will cause the program to halt. On the other hand
-- if the length of a list is known ahead of time, `fromListUArrayN` can be used instead as
-- optimization.
--
-- @since 0.1.0
fromListUArray :: Prim e => [e] -> UArray e
fromListUArray xs = fromListUArrayN (Size (length xs)) xs
{-# INLINE fromListUArray #-}

-- | Same as `fromListUArray`, except it will allocate an array exactly of @n@ size, as
-- such it will not convert any portion of the list that doesn't fit into the newly
-- created array.
--
-- [Unsafe size] if the length of supplied list is actually smaller then the expected
-- size, thunks with `UndefinedElement` will be left in the tail of the array.
--
-- ====__Examples__
--
-- >>> fromListUArrayN 3 [1 :: Int, 2, 3]
-- UArray [1,2,3]
-- >>> fromListUArrayN 3 [1 :: Int ..]
-- UArray [1,2,3]
-- >>> fromListUArrayN 3 [1 :: Int, 2]
-- UArray [1,2*** Exception: undefined array element: Data.Prim.Array.Boxed.uninitialized
--
-- @since 0.1.0
fromListUArrayN ::
     Prim e
  => Size -- ^ Expected @n@ size of a list
  -> [e]
  -> UArray e
fromListUArrayN = I.fromListArrayN
{-# INLINE fromListUArrayN #-}

-- | Convert a pure boxed array into a list. It should work fine with GHC built-in list
-- fusion.
--
-- @since 0.1.0
toListUArray :: Prim e => UArray e -> [e]
toListUArray = I.toListArray
{-# INLINE toListUArray #-}

-- | Strict right fold
foldrUArray :: Prim e => (e -> b -> b) -> b -> UArray e -> b
foldrUArray = I.foldrArray
{-# INLINE foldrUArray #-}

makeUArray :: Prim e => Size -> (Int -> e) -> UArray e
makeUArray = I.makeArray
{-# INLINE makeUArray #-}

makeUArrayM :: (Prim e, MonadPrim s m) => Size -> (Int -> m e) -> m (UArray e)
makeUArrayM = I.makeArrayM
{-# INLINE makeUArrayM #-}

createUArrayM :: (Prim e, MonadPrim s m) => Size -> (MUArray e s -> m b) -> m (b, UArray e)
createUArrayM = I.createArrayM
{-# INLINE createUArrayM #-}

createUArrayM_ :: (Prim e, MonadPrim s m) => Size -> (MUArray e s -> m b) -> m (UArray e)
createUArrayM_ = I.createArrayM_
{-# INLINE createUArrayM_ #-}


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
-- >>> ma <- makeMUArray 5 $ \i -> modifyFetchRef ref (\cur -> cur ++ show i ++ ",")
-- >>> mapM_ (readMUArray ma >=> putStrLn) [0 .. 4]
-- Numbers: 0,
-- Numbers: 0,1,
-- Numbers: 0,1,2,
-- Numbers: 0,1,2,3,
-- Numbers: 0,1,2,3,4,
--
-- @since 0.1.0
makeMUArray :: (Prim e, MonadPrim s m) => Size -> (Int -> m e) -> m (MUArray e s)
makeMUArray = I.makeMArray
{-# INLINE makeMUArray #-}

-- | Traverse an array with a monadic action.
--
-- @since 0.1.0
traverseUArray :: (Prim e, Prim b, MonadPrim s m) => (e -> m b) -> UArray e -> m (UArray b)
traverseUArray = I.traverseArray
{-# INLINE traverseUArray #-}
