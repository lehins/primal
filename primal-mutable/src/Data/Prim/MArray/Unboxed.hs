{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
  , UMArray(..)
  , Size(..)
  -- * Immutable
  , makeUArray
  , makeUArrayM
  , sizeOfUArray
  , indexUArray
  -- * Mutable
  -- ** Create
  , newUMArray
  , newRawUMArray
  , makeUMArray
  , createUArrayM
  , createUArrayM_
  -- ** Access
  , readUMArray
  , writeUMArray
  -- *** Atomic
  -- , casUMArray
  -- , atomicModifyFetchNewUMArray
  -- , atomicModifyFetchOldUMArray
  -- , atomicModifyUMArray
  -- , atomicModifyUMArray_
  -- , atomicModifyUMArray2
  -- *
  , shrinkUMArray
  , resizeUMArray
  , thawUArray
  , thawCopyUArray
  , freezeUMArray
  , freezeCopyUMArray
  , copyUArray
  , moveUMArray
  , cloneUArray
  , cloneUMArray
  -- * PArray
  , toPArray
  , fromPArray
  , toPMArray
  , fromPMArray
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
import Data.Prim.Memory.PArray
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

data UMArray e s = UMArray (MutableByteArray# s)
type role UMArray nominal nominal


-- | Check if both of the arrays refer to the exact same one through poiner equality. None
-- of the elements are evaluated.
instance Eq (UMArray e s) where
  UMArray ma1# == UMArray ma2# = isTrue# (sameMutableByteArray# ma1# ma2#)



data UArray e = UArray ByteArray#

type role UArray nominal

instance Prim e => MRef (UMArray e) where
  type Elt (UMArray e) = e
  newRawMRef = newRawUMArray 1
  {-# INLINE newRawMRef #-}
  readMRef uma = readUMArray uma 0
  {-# INLINE readMRef #-}
  writeMRef uma = writeUMArray uma 0
  {-# INLINE writeMRef #-}


instance Atomic e => AtomicMRef (UMArray e) where
  atomicReadMRef mba = atomicReadMBytes (toMBytes mba) (0 :: Off e)
  {-# INLINE atomicReadMRef #-}
  atomicWriteMRef mba = atomicWriteMBytes (toMBytes mba) (0 :: Off e)
  {-# INLINE atomicWriteMRef #-}
  casMRef mba = casBoolFetchMBytes (toMBytes mba) (0 :: Off e)
  {-# INLINE casMRef #-}
  atomicModifyMRef mba = atomicModifyMBytes (toMBytes mba) (0 :: Off e)
  {-# INLINE atomicModifyMRef #-}


instance (Num e, AtomicCount e) => AtomicCountMRef (UMArray e) where
  atomicAddFetchOldMRef mba = atomicAddFetchOldMBytes (toMBytes mba) (0 :: Off e)
  {-# INLINE atomicAddFetchOldMRef #-}
  atomicAddFetchNewMRef mba = atomicAddFetchNewMBytes (toMBytes mba) (0 :: Off e)
  {-# INLINE atomicAddFetchNewMRef #-}
  atomicSubFetchOldMRef mba = atomicSubFetchOldMBytes (toMBytes mba) (0 :: Off e)
  {-# INLINE atomicSubFetchOldMRef #-}
  atomicSubFetchNewMRef mba = atomicSubFetchNewMBytes (toMBytes mba) (0 :: Off e)
  {-# INLINE atomicSubFetchNewMRef #-}


instance (Bits e, AtomicBits e) => AtomicBitsMRef (UMArray e) where
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


instance Prim e => I.MArray (UMArray e) where
  type Array (UMArray e) = UArray e
  sizeOfArray = sizeOfUArray
  {-# INLINE sizeOfArray #-}
  indexArray = indexUArray
  {-# INLINE indexArray #-}
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
  copyArray = copyUArray
  {-# INLINE copyArray #-}
  moveMArray = moveUMArray
  {-# INLINE moveMArray #-}
  setMArray = setUMArray
  {-# INLINE setMArray #-}
  shrinkMArray ma sz = ma <$ shrinkUMArray ma sz
  {-# INLINE shrinkMArray #-}
  resizeMArray = resizeUMArray
  {-# INLINE resizeMArray #-}


-- | /O(1)/ - Cast an unboxed array into a `PArray`
--
-- @since 0.1.0
toPArray :: UArray e -> PArray 'Inc e
toPArray (UArray a#) = PArray (fromByteArray# a#)
{-# INLINE toPArray #-}

-- | /O(1)/ - Cast a `PArray` into an unboxed array
--
-- @since 0.1.0
fromPArray :: PArray p e -> UArray e
fromPArray (PArray ba) = UArray (toByteArray# ba)
{-# INLINE fromPArray #-}


-- | /O(1)/ - Cast a mutable unboxed array into a `PMArray`
--
-- @since 0.1.0
toPMArray :: UMArray e s -> PMArray 'Inc e s
toPMArray (UMArray mba#) = PMArray (fromMutableByteArray# mba#)
{-# INLINE toPMArray #-}

-- | /O(1)/ - Cast an `PMArray` into a mutable unboxed array
--
-- @since 0.1.0
fromPMArray :: PMArray p e s -> UMArray e s
fromPMArray (PMArray mb) = UMArray (toMutableByteArray# mb)
{-# INLINE fromPMArray #-}


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

-- | /O(1)/ - Cast a mutable unboxed array into a `PMArray`
--
-- @since 0.1.0
toMBytes :: UMArray e s -> MBytes 'Inc s
toMBytes (UMArray a#) = fromMutableByteArray# a#
{-# INLINE toMBytes #-}

-- | /O(1)/ - Cast an `PMArray` into a mutable unboxed array
--
-- @since 0.1.0
fromMBytes :: MBytes p s -> UMArray e s
fromMBytes mb = UMArray (toMutableByteArray# mb)
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
-- >>> ma <- newRawUMArray 10 :: IO (MArray Int RW)
-- >>> sizeOfUMArray ma
-- Size 10
-- >>> readUMArray ma 1
-- *** Exception: undefined array element: Data.Prim.Array.Unboxed.uninitialized
--
-- @since 0.1.0

-- | Create a mutable unboxed array where each element is set to the supplied initial
-- value, which is evaluated immediately before that. See `newUMArrayLazy` for an ability
-- to initialize with a thunk or `newRawUArray` that will set each element to an
-- `UndefinedElement` exception.
--
-- [Unsafe size] Negative or too large of an array size can kill the current thread with
-- `HeapOverflow` asynchronous exception.
--
-- ====__Examples__
--
-- >>> newUMArray 10 'A' >>= freezeUMArray
-- UArray "AAAAAAAAAA"
--
-- | Same as `newUMArray`, except initial element is allowed to be a thunk. Prefer using
-- `newRawUMArray`, instead of supplying an `error`, whenever initial element is not
-- known ahead of time. Or even better try using other creation functions that iterate
-- over an array and overwrite each element, such as `makeUMArray`.
--
-- [Unsafe size] Negative or too large of an array size can kill the current thread with `HeapOverflow`
-- asynchronous exception.
--
-- @since 0.1.0
newRawUMArray :: forall e m s . (Prim e, MonadPrim s m) => Size -> m (UMArray e s)
newRawUMArray n =
  prim $ \s ->
    case newByteArray# (unCountBytes# (coerce n :: Count e)) s of
      (# s', ma# #) -> (# s', UMArray ma# #)
{-# INLINE newRawUMArray #-}

newUMArray :: (Prim e, MonadPrim s m) => Size -> e -> m (UMArray e s)
newUMArray = I.newMArray
{-# INLINE newUMArray #-}


-- | Get the size of a mutable unboxed array
--
-- >>> ma <- newUMArray 1024 "Element of each cell"
-- >>> sizeOfUMArray ma
-- Size 1024
--
-- @since 0.1.0
getSizeOfUMArray ::
     forall e m s. (Prim e, MonadPrim s m)
  => UMArray e s
  -> m Size
getSizeOfUMArray (UMArray ma#) =
  prim $ \s ->
    case getSizeofMutableByteArray# ma# s of
      (# s', n# #) -> (# s', coerce (fromByteCount (Count (I# n#)) :: Count e) #)
{-# INLINE getSizeOfUMArray #-}


shrinkUMArray ::
     forall e m s. (MonadPrim s m, Prim e)
  => UMArray e s
  -> Size
  -> m ()
shrinkUMArray (UMArray mb#) sz =
  prim_ (shrinkMutableByteArray# mb# (unCountBytes# (coerce sz :: Count e)))
{-# INLINE shrinkUMArray #-}

resizeUMArray ::
     forall e m s. (MonadPrim s m, Prim e)
  => UMArray e s
  -> Size
  -> m (UMArray e s)
resizeUMArray (UMArray mb#) sz =
  prim $ \s ->
    case resizeMutableByteArray# mb# (unCountBytes# (coerce sz :: Count e)) s of
      (# s', mb'# #) -> (# s', UMArray mb'# #)
{-# INLINE resizeUMArray #-}


-- | Read an element from a mutable unboxed array at the supplied index.
--
-- [Unsafe index] Negative or larger than array size can fail with unchecked exception
--
-- ==== __Examples__
--
-- >>> ma <- makeUMArray 10 (pure . ("Element ix: " ++) . show)
-- >>> readUMArray ma 5
-- "Element ix: 5"
--
-- @since 0.1.0
readUMArray :: (Prim e, MonadPrim s m) => UMArray e s -> Int -> m e
readUMArray (UMArray ma#) (I# i#) = prim (readMutableByteArray# ma# i#)
{-# INLINE readUMArray #-}

-- | Write an element into a mutable unboxed array at a supplied index strictly. An
-- element will be evaluated to WHNF.
--
-- [Unsafe index] Negative or larger than array size can fail with unchecked exception
--
-- ==== __Examples__
--
-- >>> ma <- newUMArray 4 (Nothing :: Maybe Int)
-- >>> writeUMArray ma 2 (Just 2)
-- >>> freezeUMArray ma
-- UArray [Nothing,Nothing,Just 2,Nothing]
--
-- Important to note that an element is evaluated prior to being written into a cell, so
-- it will not overwrite a value with if it evaluates to an exception:
--
-- >>> import Control.Exception
-- >>> writeUMArray ma 2 (throw DivideByZero)
-- *** Exception: divide by zero
-- >>> freezeUMArray ma
-- UArray [Nothing,Nothing,Just 2,Nothing]
--
-- But it is evaluated to Normal Form, so it is still possible to write something that
-- eventually evaluates to bottom.
--
-- >>> writeUMArray ma 3 (Just (7 `div` 0 ))
-- >>> freezeUMArray ma
-- UArray [Nothing,Nothing,Just 2,Just *** Exception: divide by zero
--
-- Either `deepseq` or `writeUMArrayDeep` can be used to alleviate that.
--
-- @since 0.1.0
writeUMArray :: (Prim e, MonadPrim s m) => UMArray e s -> Int -> e -> m ()
writeUMArray (UMArray ma#) (I# i#) a = prim_ (writeMutableByteArray# ma# i# a)
{-# INLINE writeUMArray #-}

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
-- >>> writeUMArray ma 1 10
-- >>> freezeUMArray ma
-- UArray [1,10,3,4,5]
-- >>> print a
-- UArray [1,2,3,4,5]
--
-- Be careful not mutate pure immutable arrays, that are still being referenced in some pure
-- setting.
--
-- >>> ma' <- thawUArray a
-- >>> writeUMArray ma' 0 100000
-- >>> print a
-- UArray [100000,2,3,4,5]
--
-- @since 0.1.0
thawUArray :: MonadPrim s m => UArray e -> m (UMArray e s)
thawUArray (UArray a#) = prim $ \s ->
  case unsafeThawByteArray# a# s of
    (# s', ma# #) -> (# s', UMArray ma# #)
{-# INLINE thawUArray #-}

thawCopyUArray :: (Prim e, MonadPrim s m) => UArray e -> Int -> Size -> m (UMArray e s)
thawCopyUArray = I.thawCopyArray
{-# INLINE thawCopyUArray #-}

-- | Convert a mutable unboxed array into an immutable.
--
-- [Unsafe further mutation] After the mutable array is frozen, it is unsafe to mutate
-- it, because the changes will be reflected in the newly created immutable array as well.
--
-- See `freezeCopyUMArray` for a safer alternative or use `cloneUMArray` prior to
-- freezing if further mutation of an arrac
-- @since 0.1.0
freezeUMArray :: MonadPrim s m => UMArray e s -> m (UArray e)
freezeUMArray (UMArray ma#) = prim $ \s ->
  case unsafeFreezeByteArray# ma# s of
    (# s', a# #) -> (# s', UArray a# #)
{-# INLINE freezeUMArray #-}

freezeCopyUMArray :: (Prim e, MonadPrim s m) => UMArray e s -> Int -> Size -> m (UArray e)
freezeCopyUMArray = I.freezeCopyMArray
{-# INLINE freezeCopyUMArray #-}


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
cloneUMArray :: (Prim e, MonadPrim s m) => UMArray e s -> Int -> Size -> m (UMArray e s)
cloneUMArray = I.cloneMArray
{-# INLINE cloneUMArray #-}


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
  -> UMArray e s -- ^ Destination mutable array
  -> Int -- ^ Offset into the destination mutable array
  -> Size -- ^ Number of elements to copy over
  -> m ()
copyUArray (UArray src#) srcOff (UMArray dst#) dstOff n =
  let srcOff# = unOffBytes# (coerce srcOff :: Off e)
      dstOff# = unOffBytes# (coerce dstOff :: Off e)
      n# = unCountBytes# (coerce n :: Count e)
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
moveUMArray ::
     forall e m s. (Prim e, MonadPrim s m)
  => UMArray e s -- ^ Source mutable array
  -> Int -- ^ Offset into the source mutable array
  -> UMArray e s -- ^ Destination mutable array
  -> Int -- ^ Offset into the destination mutable array
  -> Size -- ^ Number of elements to copy over
  -> m ()
moveUMArray (UMArray src#) srcOff (UMArray dst#) dstOff n =
  let srcOff# = unOffBytes# (coerce srcOff :: Off e)
      dstOff# = unOffBytes# (coerce dstOff :: Off e)
      n# = unCountBytes# (coerce n :: Count e)
  in prim_ (copyMutableByteArray# src# srcOff# dst# dstOff# n#)
{-# INLINE moveUMArray #-}

setUMArray ::
     (Prim e, MonadPrim s m)
  => UMArray e s -- ^ Mutable array
  -> Int -- ^ Offset into the mutable array
  -> Size -- ^ Number of elements to overwrite
  -> e -- ^ Element to overwrite the cells with
  -> m ()
setUMArray (UMArray ma#) (I# o#) (Size (I# n#)) a =
  prim_ (setMutableByteArray# ma# o# n# a)
{-# INLINE setUMArray #-}


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
-- -- >>> ma <- makeUMArray 5 (pure . (*10))
-- -- >>> freezeUMArray ma
-- -- UArray [0,10,20,30,40]
-- --
-- -- A possible mistake is to try and pass the expected value, instead of an actual element:
-- --
-- -- >>> casUMArray ma 2 20 1000
-- -- (False,20)
-- -- >>> freezeUMArray ma
-- -- UArray [0,10,20,30,40]
-- --
-- -- But this will get us nowhere, since what we really need is the actual reference to the
-- -- value currently in the array cell
-- --
-- -- >>> expected <- readUMArray ma 2
-- -- >>> r@(_, currentValue) <- casUMArray ma 2 expected 1000
-- -- >>> freezeUMArray ma
-- -- UArray [0,10,1000,30,40]
-- -- >>> r
-- -- (True,1000)
-- --
-- -- In a concurrent setting current value can potentially be modified by some other
-- -- thread, therefore returned value can be immedieately used as the expected one to the
-- -- next call, if we don want to retry the atomic modification:
-- --
-- -- >>> casUMArray ma 2 currentValue 2000
-- -- (True,2000)
-- -- >>> freezeUMArray ma
-- -- UArray [0,10,2000,30,40]
-- --
-- -- @since 0.1.0
-- casUMArray ::
--      MonadPrim s m
--   => MArray a s -- ^ Mutable array to mutate
--   -> Int -- ^ Index at which the cell should be set to the new value
--   -> a -- ^ Reference to the expected unboxed value
--   -> a -- ^ New value to update the cell with
--   -> m (Bool, a)
-- casUMArray (UMArray ma#) (I# i#) expected new =
--   prim $ \s ->
--     case casUArray# ma# i# expected new s of
--       (# s', failed#, actual #) -> (# s', (isTrue# (failed# ==# 0#), actual) #)
-- {-# INLINE casUMArray #-}


-- atomicModifyUMArray# :: MonadPrim s m => MArray a s -> Int -> (a -> (# a, b #)) -> m b
-- atomicModifyUMArray# ma@(UMArray ma#) i@(I# i#) f = do
--   current0 <- readUMArray ma i
--   prim $
--     let go expected s =
--           case f expected of
--             (# new, artifact #) ->
--               case casUArray# ma# i# expected new s of
--                 (# s', 0#, _ #) -> (# s', artifact #)
--                 (# s', _, actual #) -> go actual s'
--      in go current0
-- {-# INLINE atomicModifyUMArray# #-}


-- atomicModifyFetchNewUMArray :: MonadPrim s m => MArray a s -> Int -> (a -> a) -> m a
-- atomicModifyFetchNewUMArray ma i f =
--   atomicModifyUMArray# ma i (\a -> let a' = f a in (# a', a' #))
-- {-# INLINE atomicModifyFetchNewUMArray #-}

-- -- atomicModifyFetchNewUMArray ma@(UMArray ma#) i@(I# i#) f = do
-- --   current0 <- readUMArray ma i
-- --   prim $ \s0 ->
-- --     let go expected s =
-- --           case casUArray# ma# i# expected (f expected) s of
-- --             (# s', 0#, actual #) -> go actual s'
-- --             (# s', _, current #) -> (# s', current #)
-- --     in go current0 s0
--   -- let go e =
--   --       casUMArray ma i e (f e) >>= \case
--   --         (True, new) -> pure new
--   --         (_, current) -> go current
--   --  in readUMArray ma i >>= go

-- atomicModifyFetchOldUMArray :: MonadPrim s m => MArray a s -> Int -> (a -> a) -> m a
-- atomicModifyFetchOldUMArray ma i f =
--   atomicModifyUMArray# ma i (\a -> (# f a, a #))
-- {-# INLINE atomicModifyFetchOldUMArray #-}
--   -- let go e =
--   --       casUMArray ma i e (f e) >>= \case
--   --         (True, _new) -> pure e
--   --         (_, current) -> go current
--   --  in readUMArray ma i >>= go



-- atomicModifyUMArray :: MonadPrim s m => MArray a s -> Int -> (a -> (a, b)) -> m b
-- atomicModifyUMArray ma i f =
--   atomicModifyUMArray# ma i (\a -> let (a', b) = f a in (# a', b #))
-- {-# INLINE atomicModifyUMArray #-}
--   -- let go e =
--   --       let (new, artifact) = f e
--   --        in casUMArray ma i e new >>= \case
--   --             (True, _new) -> pure artifact
--   --             (_, current) -> go current
--   --  in readUMArray ma i >>= go


-- atomicModifyUMArray_ :: MonadPrim s m => MArray a s -> Int -> (a -> a) -> m ()
-- atomicModifyUMArray_ ma i f =
--   atomicModifyUMArray# ma i (\a -> let a' = f a in (# a', () #))
-- {-# INLINE atomicModifyUMArray_ #-}


-- atomicModifyUMArray2 :: MonadPrim s m => MArray a s -> Int -> (a -> (a, b)) -> m (a, a, b)
-- atomicModifyUMArray2 ma i f =
--   atomicModifyUMArray# ma i (\a -> let (a', b) = f a in (# a', (a, a', b) #))
-- {-# INLINE atomicModifyUMArray2 #-}

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

createUArrayM :: (Prim e, MonadPrim s m) => Size -> (UMArray e s -> m b) -> m (b, UArray e)
createUArrayM = I.createArrayM
{-# INLINE createUArrayM #-}

createUArrayM_ :: (Prim e, MonadPrim s m) => Size -> (UMArray e s -> m b) -> m (UArray e)
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
-- >>> ma <- makeUMArray 5 $ \i -> modifyFetchRef ref (\cur -> cur ++ show i ++ ",")
-- >>> mapM_ (readUMArray ma >=> putStrLn) [0 .. 4]
-- Numbers: 0,
-- Numbers: 0,1,
-- Numbers: 0,1,2,
-- Numbers: 0,1,2,3,
-- Numbers: 0,1,2,3,4,
--
-- @since 0.1.0
makeUMArray :: (Prim e, MonadPrim s m) => Size -> (Int -> m e) -> m (UMArray e s)
makeUMArray = I.makeMArray
{-# INLINE makeUMArray #-}

-- | Traverse an array with a monadic action.
--
-- @since 0.1.0
traverseUArray :: (Prim e, Prim b, MonadPrim s m) => (e -> m b) -> UArray e -> m (UArray b)
traverseUArray = I.traverseArray
{-# INLINE traverseUArray #-}
