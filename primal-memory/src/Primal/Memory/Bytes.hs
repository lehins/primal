{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Primal.Memory.Bytes
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Memory.Bytes
  ( module Primal.Element.Unbox
    -- * Immutable
  , Bytes
  , toByteArray#
  , fromByteArray#
  , cloneBytes
  , emptyBytes
  , eqBytes
  , singletonBytes
  , isEmptyBytes
  , createBytes
  , createBytes_
  , createBytesST
  , createBytesST_
  -- * Pinness
  , Pinned(..)
  , isPinnedBytes
  , isPinnedMBytes
  , toPinnedBytes
  , toPinnedMBytes
  , toIncBytes
  , toIncMBytes
  , relaxPinnedBytes
  , relaxPinnedMBytes
  , ensurePinnedBytes
  , ensurePinnedMBytes
  -- * Mutable
  , MBytes
  , toMutableByteArray#
  , fromMutableByteArray#
  , isSameBytes
  , isSamePinnedBytes
  , isSameMBytes
  , indexOffBytes
  , indexByteOffBytes
  , byteCountBytes
  , countBytes
  , countRemBytes
  , compareBytes
  , compareByteOffBytes
  , compareByteOffMBytes
  -- * Mutable
  -- ** To/From immutable
  , thawBytes
  , thawCloneSliceBytes
  , freezeMBytes
  , freezeCloneSliceMBytes
  -- ** Construction
  , allocMBytes
  , singletonMBytes
  , allocPinnedMBytes
  , allocAlignedPinnedMBytes
  , allocUnpinnedMBytes
  , allocZeroMBytes
  , allocZeroPinnedMBytes
  , allocZeroAlignedPinnedMBytes
  , shrinkMBytes
  , resizeMBytes
  , reallocMBytes
  , coerceStateMBytes
  -- ** Modifying data
  , cloneMBytes
  , withCloneMBytes
  , withCloneMBytes_
  , withCloneMBytesST
  , withCloneMBytesST_
  , loadListMBytes
  , loadListMBytes_
  , copyBytesToMBytes
  , moveMBytesToMBytes
  -- ** Moving data
  -- * Size
  , getByteCountMBytes
  , getCountMBytes
  , getCountRemOfMBytes
  -- * Access
  , readOffMBytes
  , readByteOffMBytes
  , writeOffMBytes
  , writeByteOffMBytes
  , setMBytes
  , zeroMBytes
  -- ** Ptr
  -- *** Access
  , withPtrBytes
  , withNoHaltPtrBytes
  , withPtrMBytes
  , withNoHaltPtrMBytes
  -- *** Conversion
  , toPtrBytes
  , toPtrMBytes
  , toForeignPtrBytes
  , toMForeignPtrMBytes
  -- *** Copying
  , copyPtrToMBytes
  , movePtrToMBytes
  , copyBytesToPtr
  , copyMBytesToPtr
  , moveMBytesToPtr
  , copyByteOffPtrToMBytes
  , moveByteOffPtrToMBytes
  , copyByteOffBytesToPtr
  , copyByteOffMBytesToPtr
  , moveByteOffMBytesToPtr
  , compareByteOffBytesToPtr
  , compareByteOffPtrToBytes
  -- * Conversion
  , toUArrayBytes
  , fromUArrayBytes
  , toUMArrayMBytes
  , fromUMArrayMBytes
  , fromListBytes
  , fromListBytesN
  , fromListZeroBytesN_
  , appendBytes
  , concatBytes
  , toListBytes
  , toListSlackBytes
  -- * Weak pointers
  , mkWeakMBytes
  , mkWeakNoFinalizerMBytes
  -- * Atomic
  , casMBytes
  , casBoolMBytes
  , casBoolFetchMBytes
  , atomicReadMBytes
  , atomicWriteMBytes
  , atomicModifyMBytes
  , atomicModifyMBytes_
  , atomicBoolModifyFetchOldMBytes
  , atomicModifyFetchOldMBytes
  , atomicModifyFetchNewMBytes
  -- ** Numberic
  , atomicAddFetchOldMBytes
  , atomicAddFetchNewMBytes
  , atomicSubFetchOldMBytes
  , atomicSubFetchNewMBytes
  -- ** Binary
  , atomicAndFetchOldMBytes
  , atomicAndFetchNewMBytes
  , atomicNandFetchOldMBytes
  , atomicNandFetchNewMBytes
  , atomicOrFetchOldMBytes
  , atomicOrFetchNewMBytes
  , atomicXorFetchOldMBytes
  , atomicXorFetchNewMBytes
  , atomicNotFetchOldMBytes
  , atomicNotFetchNewMBytes
  -- * Prefetch
  , prefetchBytes0
  , prefetchMBytes0
  , prefetchBytes1
  , prefetchMBytes1
  , prefetchBytes2
  , prefetchMBytes2
  , prefetchBytes3
  , prefetchMBytes3
  -- * Helpers
  ) where

import Data.Maybe (fromMaybe)
import Primal.Foreign
import Primal.Memory.Internal
import Primal.Memory.Weak
import Primal.Memory.ForeignPtr
import Primal.Monad
import Primal.Element.Unbox
import Primal.Element.Unbox.Atomic

eqBytes :: Bytes p1 -> Bytes p2 -> Bool
eqBytes b1 b2 = isSameBytes b1 b2 || eqByteMem b1 b2
{-# INLINE eqBytes #-}

---- Pure

compareBytes :: Unbox e => Bytes p1 -> Off e -> Bytes p2 -> Off e -> Count e -> Ordering
compareBytes (Bytes b1#) off1 (Bytes b2#) off2 c =
  toOrdering# (compareByteArrays# b1# (unOffBytes# off1) b2# (unOffBytes# off2) (unCountBytes# c))
{-# INLINE compareBytes #-}

-- compareMBytes ::
--      (Unbox e, Primal s m)
--   => MBytes p1 s
--   -> Off e
--   -> MBytes p2 s
--   -> Off e
--   -> Count e
--   -> m Ordering
-- compareMBytes (Bytes b1#) off1 (Bytes b2#) off2 c =
--   toOrdering# (compareByteArrays# b1# (unOffBytes# off1) b2# (unOffBytes# off2) (unCountBytes# c))
-- {-# INLINE compareBytes #-}


-- | This function allows the change of state token. Use with care, because it can allow
-- mutation to escape the `ST` monad.
coerceStateMBytes :: MBytes p s' -> MBytes p s
coerceStateMBytes = unsafeCoerce#


emptyBytes :: Bytes p
emptyBytes = castPinnednessBytes $ runST $ allocPinnedMBytes (0 :: Count Word8) >>= freezeMBytes
{-# NOINLINE emptyBytes #-}

isEmptyBytes :: Bytes p -> Bool
isEmptyBytes b = byteCountBytes b == 0
{-# INLINE isEmptyBytes #-}

singletonBytes :: forall e p. (Unbox e, Typeable p) => e -> Bytes p
singletonBytes a = runST $ singletonMBytes a >>= freezeMBytes
{-# INLINE singletonBytes #-}

---- Mutable

singletonMBytes :: forall e p m s. (Unbox e, Typeable p, Primal s m) => e -> m (MBytes p s)
singletonMBytes a = do
  mb <- allocMBytes (1 :: Count e)
  mb <$ writeOffMBytes mb 0 a
{-# INLINE singletonMBytes #-}

thawCloneSliceBytes ::
     (Typeable pd, Unbox e, Primal s m)
  => Bytes ps
  -> Off e
  -> Count e
  -> m (MBytes pd s)
thawCloneSliceBytes b o c = allocMBytes c >>= \mb -> mb <$ copyBytesToMBytes b o mb 0 c
{-# INLINE thawCloneSliceBytes #-}



freezeCloneSliceMBytes ::
     (Unbox e, Typeable pd, Primal s m)
  => MBytes ps s
  -> Off e
  -> Count e
  -> m (Bytes pd)
freezeCloneSliceMBytes mbSrc o c = do
  createBytes_ c $ \mbDst -> moveMBytesToMBytes mbSrc o mbDst 0 c
{-# INLINE freezeCloneSliceMBytes #-}

cloneBytes :: Typeable p => Bytes p -> Bytes p
cloneBytes b = runST $ thawBytes b >>= cloneMBytes >>= freezeMBytes
{-# INLINE cloneBytes #-}

cloneMBytes :: (Primal s m, Typeable p) => MBytes p s -> m (MBytes p s)
cloneMBytes mb = do
  n <- getCountMBytes mb
  mb' <- allocMBytes (n :: Count Word8)
  mb' <$ moveMBytesToMBytes mb 0 mb' 0 n
{-# INLINE cloneMBytes #-}


copyBytesToMBytes ::
     (Primal s m, Unbox e) => Bytes ps -> Off e -> MBytes pd s -> Off e -> Count e -> m ()
copyBytesToMBytes (Bytes src#) srcOff (MBytes dst#) dstOff c =
  primal_ $
  copyByteArray# src# (unOffBytes# srcOff) dst# (unOffBytes# dstOff) (unCountBytes# c)
{-# INLINE copyBytesToMBytes #-}


moveMBytesToMBytes ::
     (Primal s m, Unbox e) => MBytes ps s-> Off e -> MBytes pd s -> Off e -> Count e -> m ()
moveMBytesToMBytes (MBytes src#) srcOff (MBytes dst#) dstOff c =
  primal_ (copyMutableByteArray# src# (unOffBytes# srcOff) dst# (unOffBytes# dstOff) (unCountBytes# c))
{-# INLINE moveMBytesToMBytes #-}

-- | Allocated memory is not cleared, so make sure to fill it in properly, otherwise you
-- might find some garbage there.
createBytes ::
     forall p e b s m. (Unbox e, Typeable p, Primal s m)
  => Count e
  -> (MBytes p s -> m b)
  -> m (b, Bytes p)
createBytes n f = do
  mb <- allocMBytes n
  !res <- f mb
  (,) res <$> freezeMBytes mb
{-# INLINE createBytes #-}

createBytes_ ::
     forall p e b s m. (Unbox e, Typeable p, Primal s m)
  => Count e
  -> (MBytes p s -> m b)
  -> m (Bytes p)
createBytes_ n f = allocMBytes n >>= \mb -> f mb >> freezeMBytes mb
{-# INLINE createBytes_ #-}

createBytesST ::
     forall p e b. (Unbox e, Typeable p)
  => Count e
  -> (forall s . MBytes p s -> ST s b)
  -> (b, Bytes p)
createBytesST n f = runST $ createBytes n f
{-# INLINE createBytesST #-}

createBytesST_ ::
     forall p e b. (Unbox e, Typeable p)
  => Count e
  -> (forall s. MBytes p s -> ST s b)
  -> Bytes p
createBytesST_ n f =  runST $ createBytes_ n f
{-# INLINE createBytesST_ #-}

allocZeroMBytes :: (Primal s m, Unbox e, Typeable p) => Count e -> m (MBytes p s)
allocZeroMBytes n = allocMBytes n >>= \mb -> mb <$ setMBytes mb 0 (toByteCount n) 0
{-# INLINE allocZeroMBytes #-}



-- | Fill the mutable array with zeros efficiently.
zeroMBytes :: Primal s m => MBytes p s -> m ()
zeroMBytes mba@(MBytes mba#) = do
  Count (I# n#) <- getByteCountMBytes mba
  primal_ (setByteArray# mba# 0# n# 0#)
{-# INLINE zeroMBytes #-}


withCloneMBytes ::
     (Primal s m, Typeable p)
  => Bytes p
  -> (MBytes p s -> m a)
  -> m (a, Bytes p)
withCloneMBytes b f = do
  mb <- cloneMBytes =<< thawBytes b
  !res <- f mb
  b' <- freezeMBytes mb
  pure (res, b')
{-# INLINE withCloneMBytes #-}

withCloneMBytes_ ::
  (Primal s m, Typeable p)
  => Bytes p
  -> (MBytes p s -> m a)
  -> m (Bytes p)
withCloneMBytes_ b f = thawBytes b >>= cloneMBytes >>= \mb -> f mb >> freezeMBytes mb
{-# INLINE withCloneMBytes_ #-}

withCloneMBytesST ::
  Typeable p => Bytes p -> (forall s. MBytes p s -> ST s a) -> (a, Bytes p)
withCloneMBytesST b f = runST $ withCloneMBytes b f
{-# INLINE withCloneMBytesST #-}

withCloneMBytesST_ ::
  Typeable p => Bytes p -> (forall s. MBytes p s -> ST s a) -> Bytes p
withCloneMBytesST_ b f = runST $ withCloneMBytes_ b f
{-# INLINE withCloneMBytesST_ #-}




-- | Get the count of elements of type @a@ that can fit into bytes as well as the slack
-- number of bytes that would be leftover in case when total number of bytes available is
-- not exactly divisable by the size of the element that will be stored in the memory
-- chunk.
countRemBytes :: forall e p. Unbox e => Bytes p -> (Count e, Count Word8)
countRemBytes = fromByteCountRem . byteCountBytes
{-# INLINE countRemBytes #-}



-- | Get the number of elements of type @a@ that can fit into bytes as well as the slack
-- number of bytes that would be leftover in case when total number of bytes available is
-- not exactly divisable by the size of the element that will be stored in the memory
-- chunk.
getCountRemOfMBytes ::
     forall e p s m. (Primal s m, Unbox e)
  => MBytes p s
  -> m (Count e, Count Word8)
getCountRemOfMBytes b = fromByteCountRem <$> getByteCountMBytes b
{-# INLINE getCountRemOfMBytes #-}

-- | It is only guaranteed to convert the whole memory to a list whenever the size of
-- allocated memory is exactly divisible by the size of the element, otherwise there will
-- be some slack left unaccounted for.
toListBytes :: Unbox e => Bytes p -> [e]
toListBytes = toListMem
{-# INLINE toListBytes #-}

toListSlackBytes :: Unbox e => Bytes p -> ([e], [Word8])
toListSlackBytes = toListSlackMem
{-# INLINE toListSlackBytes #-}

-- | Same as `loadListMutMem`
loadListMBytes :: (Unbox e, Typeable p, Primal s m) => [e] -> MBytes p s -> m ([e], Count e)
loadListMBytes = loadListMutMem
{-# INLINE loadListMBytes #-}

-- | Same as `loadListMutMem_`
loadListMBytes_ :: (Unbox e, Typeable p, Primal s m) => [e] -> MBytes p s -> m ()
loadListMBytes_ = loadListMutMem_
{-# INLINE loadListMBytes_ #-}

-- | Same as `fromListZeroMemN_`
--
-- @since 0.3.0
fromListZeroBytesN_ :: (Unbox e, Typeable p) => Count e -> [e] -> Bytes p
fromListZeroBytesN_ = fromListZeroMemN_
{-# INLINE fromListZeroBytesN_ #-}

-- | Exactly like `fromListMemN`, but restricted to `Bytes`.
fromListBytesN ::
     (Unbox e, Typeable p)
  => Count e
  -> [e]
  -> (Either [e] (Count e), Bytes p)
fromListBytesN = fromListMemN
{-# INLINE fromListBytesN #-}

fromListBytes ::
     forall e p. (Unbox e, Typeable p)
  => [e]
  -> Bytes p
fromListBytes = fromListMem
{-# INLINE fromListBytes #-}

-- | Allocate new memory region and append second bytes region after the first one
appendBytes ::
     Typeable p
  => Bytes p1 -- ^ First memory region
  -> Bytes p2 -- ^ Second memory region
  -> Bytes p
appendBytes = appendMem
{-# INLINE appendBytes #-}


concatBytes :: Typeable p => [Bytes p'] -> Bytes p
concatBytes = concatMem
{-# INLINE concatBytes #-}


ensurePinnedBytes :: Bytes p -> Bytes 'Pin
ensurePinnedBytes b = fromMaybe (convertMem b) (toPinnedBytes b)
{-# INLINE ensurePinnedBytes #-}

ensurePinnedMBytes :: Primal s m => MBytes p s -> m (MBytes 'Pin s)
ensurePinnedMBytes mb =
  case toPinnedMBytes mb of
    Just pmb -> pure pmb
    Nothing  -> do
      n8 :: Count Word8 <- getCountMBytes mb
      pmb <- allocPinnedMBytes n8
      pmb <$ moveMBytesToMBytes mb 0 pmb 0 n8
{-# INLINE ensurePinnedMBytes #-}



-- | Create a `Weak` pointer associated with the supplied `MBytes`.
--
-- Similar to `Data.IORef.mkWeakRef` from @base@, but works in any `UnliftPrimal` with
-- `RealWorld` state token and accepts another value to be stored in a weak ptr.
--
-- @since 1.0.0
mkWeakMBytes ::
     forall a b p m. UnliftPrimal RW m
  => MBytes p RW -- ^ Reference that will act as a key for the newly created weak pointer
  -> a -- ^ Value that can be later dereferenced with `deRefWeak`.
  -> m b -- ^ An action that will get executed whenever `MBytes` gets garbage collected by
         -- the runtime.
  -> m (Weak a)
mkWeakMBytes (MBytes mba#) val finalizer =
  runInPrimalState finalizer $ \f# s ->
    case mkWeak# mba# val f# s of
      (# s', weak# #) -> (# s', Weak weak# #)
{-# INLINE mkWeakMBytes #-}

-- | Create a `Weak` pointer associated with the supplied `MBytes`.
--
-- Similar to `mkWeakMBytes`, except it does not require a finalizer. This is useful for FFI
-- finalizers. One can be added later with `addCFinalizer` or `addCFinalizerEnv`
--
-- @since 1.0.0
mkWeakNoFinalizerMBytes ::
     forall a p m. UnliftPrimal RW m
  => MBytes p RW -- ^ Reference that will act as a key for the newly created weak pointer
  -> a -- ^ Value that can be later dereferenced with `deRefWeak`
  -> m (Weak a)
mkWeakNoFinalizerMBytes (MBytes mba#) val =
  primal $ \s ->
    case mkWeakNoFinalizer# mba# val s of
      (# s', weak# #) -> (# s', Weak weak# #)
{-# INLINE mkWeakNoFinalizerMBytes #-}


-- | Perform atomic modification of an element in the `MBytes` at the supplied
-- index. Returns the actual value.  Offset is in number of elements,
-- rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
casMBytes ::
     (Primal s m, Atomic e)
  => MBytes p s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__a__@, rather than bytes.
  -> e -- ^ Expected old value
  -> e -- ^ New value
  -> m e
casMBytes (MBytes mba#) (Off (I# i#)) expected new =
  primal $ casMutableByteArray# mba# i# expected new
{-# INLINE casMBytes #-}


-- | Perform atomic modification of an element in the `MBytes` at the supplied
-- index. Returns `True` if swap was successfull and false otherwise.  Offset is in number
-- of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
casBoolMBytes ::
     (Primal s m, Atomic e)
  => MBytes p s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__a__@, rather than bytes.
  -> e -- ^ Expected old value
  -> e -- ^ New value
  -> m Bool
casBoolMBytes (MBytes mba#) (Off (I# i#)) expected new =
  primal $ casBoolMutableByteArray# mba# i# expected new
{-# INLINE casBoolMBytes #-}

-- | Just like `casBoolMBytes`, but also returns the actual value, which will match the
-- supplied expected value if the returned flag is `True`
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
casBoolFetchMBytes ::
     (Primal s m, Atomic e)
  => MBytes p s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__a__@, rather than bytes.
  -> e -- ^ Expected old value
  -> e -- ^ New value
  -> m (Bool, e)
casBoolFetchMBytes mb off expected new = do
  isCasSucc <- casBoolMBytes mb off expected new
  actual <-
    if isCasSucc
      then pure new
      else readOffMBytes mb off
  pure (isCasSucc, actual)
{-# INLINE casBoolFetchMBytes #-}


-- | Perform atomic read of `MBytes` at the supplied index. Offset is in number of
-- elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicReadMBytes ::
     (Primal s m, Atomic e)
  => MBytes p s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__a__@, rather than bytes.
  -> m e
atomicReadMBytes (MBytes mba#) (Off (I# i#)) =
  primal $ atomicReadMutableByteArray# mba# i#
{-# INLINE atomicReadMBytes #-}


-- | Perform a write into `MBytes` at the supplied index atomically. Offset is in number
-- of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicWriteMBytes ::
     (Primal s m, Atomic e)
  => MBytes p s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__a__@, rather than bytes.
  -> e
  -> m ()
atomicWriteMBytes (MBytes mba#) (Off (I# i#)) e =
  primal_ $ atomicWriteMutableByteArray# mba# i# e
{-# INLINE atomicWriteMBytes #-}


-- | Perform atomic modification of an element in the `MBytes` at the supplied
-- index. Returns the artifact of computation @__b__@.  Offset is in number of elements,
-- rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicModifyMBytes ::
     (Primal s m, Atomic e)
  => MBytes p s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__a__@, rather than bytes.
  -> (e -> (e, b)) -- ^ Function that is applied to the old value and returns new value
                   -- and some artifact of computation @__b__@
  -> m b
atomicModifyMBytes (MBytes mba#) (Off (I# i#)) f =
  primal $
  atomicModifyMutableByteArray# mba# i# $ \a ->
    case f a of
      (a', b) -> (# a', b #)
{-# INLINE atomicModifyMBytes #-}

-- | Perform atomic modification of an element in the `MBytes` at the supplied
-- index.  Offset is in number of elements, rather than bytes. Implies a full memory
-- barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicModifyMBytes_ ::
     (Primal s m, Atomic e)
  => MBytes p s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__a__@, rather than bytes.
  -> (e -> e) -- ^ Function that is applied to the old value and returns new value.
  -> m ()
atomicModifyMBytes_ (MBytes mba#) (Off (I# i#)) f =
  primal_ $ atomicModifyMutableByteArray_# mba# i# f
{-# INLINE atomicModifyMBytes_ #-}


-- | Perform atomic modification of an element in the `MBytes` at the supplied
-- index. Returns the previous value.  Offset is in number of elements, rather than
-- bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicModifyFetchOldMBytes ::
     (Primal s m, Atomic e)
  => MBytes p s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__a__@, rather than bytes.
  -> (e -> e) -- ^ Function that is applied to the old value and returns the new value
  -> m e
atomicModifyFetchOldMBytes (MBytes mba#) (Off (I# i#)) f =
  primal $ atomicModifyFetchOldMutableByteArray# mba# i# f
{-# INLINE atomicModifyFetchOldMBytes #-}


-- | Perform atomic modification of an element in the `MBytes` at the supplied
-- index. Returns the previous value.  Offset is in number of elements, rather than
-- bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicBoolModifyFetchOldMBytes ::
     (Primal s m, Atomic e)
  => MBytes p s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__a__@, rather than bytes.
  -> (e -> e) -- ^ Function that is applied to the old value and returns the new value
  -> m e
atomicBoolModifyFetchOldMBytes (MBytes mba#) (Off (I# i#)) f =
  primal $ atomicBoolModifyFetchOldMutableByteArray# mba# i# f
{-# INLINE atomicBoolModifyFetchOldMBytes #-}


-- | Perform atomic modification of an element in the `MBytes` at the supplied
-- index.  Offset is in number of elements, rather than bytes. Implies a full memory
-- barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicModifyFetchNewMBytes ::
     (Primal s m, Atomic e)
  => MBytes p s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__a__@, rather than bytes.
  -> (e -> e) -- ^ Function that is applied to the old value and returns the new value
  -> m e
atomicModifyFetchNewMBytes (MBytes mba#) (Off (I# i#)) f =
  primal $ atomicModifyFetchNewMutableByteArray# mba# i# f
{-# INLINE atomicModifyFetchNewMBytes #-}






-- | Add a numeric value to an element of a `MBytes`, corresponds to @(`+`)@ done
-- atomically. Returns the previous value.  Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicAddFetchOldMBytes ::
     (Primal s m, AtomicCount e)
  => MBytes p s
  -> Off e
  -> e
  -> m e
atomicAddFetchOldMBytes (MBytes mba#) (Off (I# i#)) a =
  primal (atomicAddFetchOldMutableByteArray# mba# i# a)
{-# INLINE atomicAddFetchOldMBytes #-}

-- | Add a numeric value to an element of a `MBytes`, corresponds to @(`+`)@ done
-- atomically. Returns the new value.  Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicAddFetchNewMBytes ::
     (Primal s m, AtomicCount e)
  => MBytes p s
  -> Off e
  -> e
  -> m e
atomicAddFetchNewMBytes (MBytes mba#) (Off (I# i#)) a =
  primal (atomicAddFetchNewMutableByteArray# mba# i# a)
{-# INLINE atomicAddFetchNewMBytes #-}



-- | Subtract a numeric value from an element of a `MBytes`, corresponds to
-- @(`-`)@ done atomically. Returns the previous value.  Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicSubFetchOldMBytes ::
     (Primal s m, AtomicCount e)
  => MBytes p s
  -> Off e
  -> e
  -> m e
atomicSubFetchOldMBytes (MBytes mba#) (Off (I# i#)) a =
  primal (atomicSubFetchOldMutableByteArray# mba# i# a)
{-# INLINE atomicSubFetchOldMBytes #-}

-- | Subtract a numeric value from an element of a `MBytes`, corresponds to
-- @(`-`)@ done atomically. Returns the new value. Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicSubFetchNewMBytes ::
     (Primal s m, AtomicCount e)
  => MBytes p s
  -> Off e
  -> e
  -> m e
atomicSubFetchNewMBytes (MBytes mba#) (Off (I# i#)) a =
  primal (atomicSubFetchNewMutableByteArray# mba# i# a)
{-# INLINE atomicSubFetchNewMBytes #-}



-- | Binary conjunction (AND) of an element of a `MBytes` with the supplied value,
-- corresponds to @(`Data.Bits..&.`)@ done atomically. Returns the previous value. Offset
-- is in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicAndFetchOldMBytes ::
     (Primal s m, AtomicBits e)
  => MBytes p s
  -> Off e
  -> e
  -> m e
atomicAndFetchOldMBytes (MBytes mba#) (Off (I# i#)) a =
  primal (atomicAndFetchOldMutableByteArray# mba# i# a)
{-# INLINE atomicAndFetchOldMBytes #-}

-- | Binary conjunction (AND) of an element of a `MBytes` with the supplied value,
-- corresponds to @(`Data.Bits..&.`)@ done atomically. Returns the new value. Offset is
-- in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicAndFetchNewMBytes ::
     (Primal s m, AtomicBits e)
  => MBytes p s
  -> Off e
  -> e
  -> m e
atomicAndFetchNewMBytes (MBytes mba#) (Off (I# i#)) a =
  primal (atomicAndFetchNewMutableByteArray# mba# i# a)
{-# INLINE atomicAndFetchNewMBytes #-}



-- | Negation of binary conjunction (NAND) of an element of a `MBytes` with the
-- supplied value, corresponds to @\\x y -> `Data.Bits.complement` (x `Data.Bits..&.` y)@
-- done atomically. Returns the previous value. Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicNandFetchOldMBytes ::
     (Primal s m, AtomicBits e)
  => MBytes p s
  -> Off e
  -> e
  -> m e
atomicNandFetchOldMBytes (MBytes mba#) (Off (I# i#)) a =
  primal (atomicNandFetchOldMutableByteArray# mba# i# a)
{-# INLINE atomicNandFetchOldMBytes #-}

-- | Negation of binary conjunction (NAND)  of an element of a `MBytes` with the supplied
-- value, corresponds to @\\x y -> `Data.Bits.complement` (x `Data.Bits..&.` y)@ done
-- atomically. Returns the new value. Offset is in number of elements, rather than
-- bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicNandFetchNewMBytes ::
     (Primal s m, AtomicBits e)
  => MBytes p s
  -> Off e
  -> e
  -> m e
atomicNandFetchNewMBytes (MBytes mba#) (Off (I# i#)) a =
  primal (atomicNandFetchNewMutableByteArray# mba# i# a)
{-# INLINE atomicNandFetchNewMBytes #-}




-- | Binary disjunction (OR) of an element of a `MBytes` with the supplied value,
-- corresponds to @(`Data.Bits..|.`)@ done atomically. Returns the previous value. Offset
-- is in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicOrFetchOldMBytes ::
     (Primal s m, AtomicBits e)
  => MBytes p s
  -> Off e
  -> e
  -> m e
atomicOrFetchOldMBytes (MBytes mba#) (Off (I# i#)) a =
  primal (atomicOrFetchOldMutableByteArray# mba# i# a)
{-# INLINE atomicOrFetchOldMBytes #-}

-- | Binary disjunction (OR) of an element of a `MBytes` with the supplied value,
-- corresponds to @(`Data.Bits..|.`)@ done atomically. Returns the new value. Offset is
-- in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicOrFetchNewMBytes ::
     (Primal s m, AtomicBits e)
  => MBytes p s
  -> Off e
  -> e
  -> m e
atomicOrFetchNewMBytes (MBytes mba#) (Off (I# i#)) a =
  primal (atomicOrFetchNewMutableByteArray# mba# i# a)
{-# INLINE atomicOrFetchNewMBytes #-}



-- | Binary exclusive disjunction (XOR) of an element of a `MBytes` with the supplied value,
-- corresponds to @`Data.Bits.xor`@ done atomically. Returns the previous value. Offset
-- is in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicXorFetchOldMBytes ::
     (Primal s m, AtomicBits e)
  => MBytes p s
  -> Off e
  -> e
  -> m e
atomicXorFetchOldMBytes (MBytes mba#) (Off (I# i#)) a =
  primal (atomicXorFetchOldMutableByteArray# mba# i# a)
{-# INLINE atomicXorFetchOldMBytes #-}

-- | Binary exclusive disjunction (XOR) of an element of a `MBytes` with the supplied value,
-- corresponds to @`Data.Bits.xor`@ done atomically. Returns the new value. Offset is
-- in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicXorFetchNewMBytes ::
     (Primal s m, AtomicBits e)
  => MBytes p s
  -> Off e
  -> e
  -> m e
atomicXorFetchNewMBytes (MBytes mba#) (Off (I# i#)) a =
  primal (atomicXorFetchNewMutableByteArray# mba# i# a)
{-# INLINE atomicXorFetchNewMBytes #-}





-- | Binary negation (NOT) of an element of a `MBytes`, corresponds to
-- @(`Data.Bits.complement`)@ done atomically. Returns the previous value. Offset is in
-- number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicNotFetchOldMBytes ::
     (Primal s m, AtomicBits e)
  => MBytes p s
  -> Off e
  -> m e
atomicNotFetchOldMBytes (MBytes mba#) (Off (I# i#)) =
  primal (atomicNotFetchOldMutableByteArray# mba# i#)
{-# INLINE atomicNotFetchOldMBytes #-}

-- | Binary negation (NOT) of an element of a `MBytes`, corresponds to
-- @(`Data.Bits.complement`)@ done atomically. Returns the new value. Offset is in number
-- of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicNotFetchNewMBytes ::
     (Primal s m, AtomicBits e)
  => MBytes p s
  -> Off e
  -> m e
atomicNotFetchNewMBytes (MBytes mba#) (Off (I# i#)) =
  primal (atomicNotFetchNewMutableByteArray# mba# i#)
{-# INLINE atomicNotFetchNewMBytes #-}




prefetchBytes0 :: (Primal s m, Unbox e) => Bytes p -> Off e -> m ()
prefetchBytes0 (Bytes b#) off = primal_ (prefetchByteArray0# b# (unOffBytes# off))
{-# INLINE prefetchBytes0 #-}

prefetchMBytes0 :: (Primal s m, Unbox e) => MBytes p s -> Off e -> m ()
prefetchMBytes0 (MBytes mb#) off = primal_ (prefetchMutableByteArray0# mb# (unOffBytes# off))
{-# INLINE prefetchMBytes0 #-}

prefetchBytes1 :: (Primal s m, Unbox e) => Bytes p -> Off e -> m ()
prefetchBytes1 (Bytes b#) off = primal_ (prefetchByteArray1# b# (unOffBytes# off))
{-# INLINE prefetchBytes1 #-}

prefetchMBytes1 :: (Primal s m, Unbox e) => MBytes p s -> Off e -> m ()
prefetchMBytes1 (MBytes mb#) off = primal_ (prefetchMutableByteArray1# mb# (unOffBytes# off))
{-# INLINE prefetchMBytes1 #-}

prefetchBytes2 :: (Primal s m, Unbox e) => Bytes p -> Off e -> m ()
prefetchBytes2 (Bytes b#) off = primal_ (prefetchByteArray2# b# (unOffBytes# off))
{-# INLINE prefetchBytes2 #-}

prefetchMBytes2 :: (Primal s m, Unbox e) => MBytes p s -> Off e -> m ()
prefetchMBytes2 (MBytes mb#) off = primal_ (prefetchMutableByteArray2# mb# (unOffBytes# off))
{-# INLINE prefetchMBytes2 #-}

prefetchBytes3 :: (Primal s m, Unbox e) => Bytes p -> Off e -> m ()
prefetchBytes3 (Bytes b#) off = primal_ (prefetchByteArray3# b# (unOffBytes# off))
{-# INLINE prefetchBytes3 #-}

prefetchMBytes3 :: (Primal s m, Unbox e) => MBytes p s -> Off e -> m ()
prefetchMBytes3 (MBytes mb#) off = primal_ (prefetchMutableByteArray3# mb# (unOffBytes# off))
{-# INLINE prefetchMBytes3 #-}

