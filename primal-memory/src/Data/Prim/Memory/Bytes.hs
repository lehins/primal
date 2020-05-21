{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Data.Prim.Memory.Bytes
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Memory.Bytes
  ( -- * Mutable
    Bytes
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
  -- * Mutable
  -- ** To/From immutable
  , thawBytes
  , freezeMBytes
  -- ** Construction
  , allocMBytes
  , singletonMBytes
  , allocPinnedMBytes
  , allocAlignedMBytes
  , allocUnpinnedMBytes
  , callocMBytes
  , callocAlignedMBytes
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
  , withPtrBytes
  , withNoHaltPtrBytes
  , withPtrMBytes
  , withNoHaltPtrMBytes
  , toPtrBytes
  , toPtrMBytes
  , toForeignPtrBytes
  , toForeignPtrMBytes
  -- * Conversion
  , fromListBytes
  , fromListBytesN
  , fromListBytesN_
  , appendBytes
  , concatBytes
  , toListBytes
  , toListSlackBytes
  -- * Atomic
  , casMBytes
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
  , module Data.Prim
  -- * Helpers
  ) where

import Control.Monad.ST
import Control.Prim.Monad
import Data.Maybe (fromMaybe)
import Data.Prim
import Data.Prim.Atomic
import Data.Prim.Memory.Internal
import Data.Prim.Memory.Bytes.Internal
import Foreign.Prim

-- | Wrap `ByteArray#` into `Bytes`
toByteArray# :: Bytes p -> ByteArray#
toByteArray# (Bytes b#) = b#

-- | Unwrap `Bytes` to get the underlying `ByteArray#`.
fromByteArray# :: ByteArray# -> Bytes 'Inc
fromByteArray# = Bytes

-- | Wrap `MutableByteArray#` into `MBytes`
toMutableByteArray# :: MBytes p s -> MutableByteArray# s
toMutableByteArray# (MBytes mb#) = mb#

-- | Unwrap `MBytes` to get the underlying `MutableByteArray#`.
fromMutableByteArray# :: MutableByteArray# s -> MBytes 'Inc s
fromMutableByteArray# = MBytes




-- | Check if two mutable bytes pointers refer to the same memory
isSameMBytes :: MBytes p1 s -> MBytes p2 s -> Bool
isSameMBytes (MBytes mb1#) (MBytes mb2#) = isTrue# (sameMutableByteArray# mb1# mb2#)
{-# INLINE isSameMBytes #-}

eqBytes :: Bytes p1 -> Bytes p2 -> Bool
eqBytes b1 b2 = isSameBytes b1 b2 || eqMem b1 b2
{-# INLINE eqBytes #-}

---- Pure

-- -- This works exactly the same as `compareBytes` except it is implemented with FFI
-- -- call instead of a primop. It will probably prove to be useless and will be removed in
-- -- the future.
-- memcmpBytes :: Prim e => Bytes p1 -> Off e -> Bytes p2 -> Off e -> Count e -> Ordering
-- memcmpBytes (Bytes ba1#) off1 (Bytes ba2#) off2 c =
--   toOrdering# (memcmpByteArray# ba1# (fromOff# off1) ba2# (fromOff# off2) (fromCount# c))
-- {-# INLINE memcmpBytes #-}

compareBytes :: Prim e => Bytes p1 -> Off e -> Bytes p2 -> Off e -> Count e -> Ordering
compareBytes (Bytes b1#) off1 (Bytes b2#) off2 c =
  toOrdering# (compareByteArrays# b1# (fromOff# off1) b2# (fromOff# off2) (fromCount# c))
{-# INLINE compareBytes #-}


-- | This function allows the change of state token. Use with care, because it can allow
-- mutation to escape the `ST` monad.
coerceStateMBytes :: MBytes p s' -> MBytes p s
coerceStateMBytes = unsafeCoerce#


emptyBytes :: Bytes p
emptyBytes = castPinnesBytes $ runST $ allocPinnedMBytes (0 :: Count Word8) >>= freezeMBytes
{-# INLINE emptyBytes #-}

isEmptyBytes :: Bytes p -> Bool
isEmptyBytes b = byteCountBytes b == 0
{-# INLINE isEmptyBytes #-}

singletonBytes :: forall e p. (Prim e, Typeable p) => e -> Bytes p
singletonBytes a = runST $ singletonMBytes a >>= freezeMBytes
{-# INLINE singletonBytes #-}

---- Mutable

singletonMBytes :: forall e p m s. (Prim e, Typeable p, MonadPrim s m) => e -> m (MBytes p s)
singletonMBytes a = do
  mb <- allocMBytes (1 :: Count e)
  mb <$ writeOffMBytes mb 0 a
{-# INLINE singletonMBytes #-}

cloneBytes :: Typeable p => Bytes p -> Bytes p
cloneBytes b = runST $ thawBytes b >>= cloneMBytes >>= freezeMBytes
{-# INLINE cloneBytes #-}

cloneMBytes :: (MonadPrim s m, Typeable p) => MBytes p s -> m (MBytes p s)
cloneMBytes mb = do
  n <- getCountMBytes mb
  mb' <- allocMBytes (n :: Count Word8)
  mb' <$ moveMBytesToMBytes mb 0 mb' 0 n
{-# INLINE cloneMBytes #-}


copyBytesToMBytes ::
     (MonadPrim s m, Prim e) => Bytes ps -> Off e -> MBytes pd s -> Off e -> Count e -> m ()
copyBytesToMBytes (Bytes src#) srcOff (MBytes dst#) dstOff c =
  prim_ $
  copyByteArray# src# (fromOff# srcOff) dst# (fromOff# dstOff) (fromCount# c)
{-# INLINE copyBytesToMBytes #-}


moveMBytesToMBytes ::
     (MonadPrim s m, Prim e) => MBytes ps s-> Off e -> MBytes pd s -> Off e -> Count e -> m ()
moveMBytesToMBytes (MBytes src#) srcOff (MBytes dst#) dstOff c =
  prim_ (copyMutableByteArray# src# (fromOff# srcOff) dst# (fromOff# dstOff) (fromCount# c))
{-# INLINE moveMBytesToMBytes #-}

-- | Allocated memory is not cleared, so make sure to fill it in properly, otherwise you
-- might find some garbage there.
createBytes ::
     forall p e b s m. (Prim e, Typeable p, MonadPrim s m)
  => Count e
  -> (MBytes p s -> m b)
  -> m (b, Bytes p)
createBytes n f = do
  mb <- allocMBytes n
  !res <- f mb
  (,) res <$> freezeMBytes mb
{-# INLINE createBytes #-}

createBytes_ ::
     forall p e b s m. (Prim e, Typeable p, MonadPrim s m)
  => Count e
  -> (MBytes p s -> m b)
  -> m (Bytes p)
createBytes_ n f = allocMBytes n >>= \mb -> f mb >> freezeMBytes mb
{-# INLINE createBytes_ #-}

createBytesST ::
     forall p e b. (Prim e, Typeable p)
  => Count e
  -> (forall s . MBytes p s -> ST s b)
  -> (b, Bytes p)
createBytesST n f = runST $ createBytes n f
{-# INLINE createBytesST #-}

createBytesST_ ::
     forall p e b. (Prim e, Typeable p)
  => Count e
  -> (forall s. MBytes p s -> ST s b)
  -> Bytes p
createBytesST_ n f =  runST $ createBytes_ n f
{-# INLINE createBytesST_ #-}

callocMBytes :: (MonadPrim s m, Prim e, Typeable p) => Count e -> m (MBytes p s)
callocMBytes n = allocMBytes n >>= \mb -> mb <$ setMBytes mb 0 (toByteCount n) 0
{-# INLINE callocMBytes #-}



-- | Fill the mutable array with zeros efficiently.
zeroMBytes :: MonadPrim s m => MBytes p s -> m ()
zeroMBytes mba@(MBytes mba#) = do
  Count (I# n#) <- getByteCountMBytes mba
  prim_ (setByteArray# mba# 0# n# 0#)
{-# INLINE zeroMBytes #-}


withCloneMBytes ::
     (MonadPrim s m, Typeable p)
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
  (MonadPrim s m, Typeable p)
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
countRemBytes :: forall e p. Prim e => Bytes p -> (Count e, Count Word8)
countRemBytes = fromByteCountRem . byteCountBytes
{-# INLINE countRemBytes #-}



-- | Get the number of elements of type @a@ that can fit into bytes as well as the slack
-- number of bytes that would be leftover in case when total number of bytes available is
-- not exactly divisable by the size of the element that will be stored in the memory
-- chunk.
getCountRemOfMBytes ::
     forall e p s m. (MonadPrim s m, Prim e)
  => MBytes p s
  -> m (Count e, Count Word8)
getCountRemOfMBytes b = fromByteCountRem <$> getByteCountMBytes b
{-# INLINE getCountRemOfMBytes #-}

-- | It is only guaranteed to convert the whole memory to a list whenever the size of
-- allocated memory is exactly divisible by the size of the element, otherwise there will
-- be some slack left unaccounted for.
toListBytes :: Prim e => Bytes p -> [e]
toListBytes = toListMem
{-# INLINE toListBytes #-}

toListSlackBytes :: Prim e => Bytes p -> ([e], [Word8])
toListSlackBytes = toListSlackMem
{-# INLINE toListSlackBytes #-}

-- | Returns `EQ` if the full list did fit into the supplied memory chunk exactly.
-- Otherwise it will return either `LT` if the list was smaller than allocated memory or
-- `GT` if the list was bigger than the available memory and did not fit into `MBytes`.
loadListMBytes :: (MonadPrim s m, Prim e) => [e] -> MBytes p s -> m Ordering
loadListMBytes ys mb = do
  (c, slack) <- getCountRemOfMBytes mb
  loadListMemN (countAsProxy ys c) slack ys mb
{-# INLINE loadListMBytes #-}

loadListMBytes_ :: (MonadPrim s m, Prim e) => [e] -> MBytes p s -> m ()
loadListMBytes_ ys mb = do
  c <- getCountMBytes mb
  loadListMemN_ (countAsProxy ys c) ys mb
{-# INLINE loadListMBytes_ #-}

fromListBytesN_ :: (Prim e, Typeable p) => Count e -> [e] -> Bytes p
fromListBytesN_ = fromListMemN_
{-# INLINE fromListBytesN_ #-}

-- | If the list is bigger than the supplied @`Count` a@ then `GT` ordering will be
-- returned, along with the `Bytes` fully filled with the prefix of the list. On the other
-- hand if the list is smaller than the supplied `Count`, `LT` with partially filled
-- `Bytes` will returned. In the latter case expect some garbage at the end of the
-- allocated memory, since no attempt is made to zero it out. Exact match obviously
-- results in an `EQ`.
fromListBytesN ::
     (Prim e, Typeable p)
  => Count e
  -> [e]
  -> (Ordering, Bytes p)
fromListBytesN = fromListMemN
{-# INLINE fromListBytesN #-}

fromListBytes ::
     forall e p. (Prim e, Typeable p)
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

relaxPinnedBytes :: Bytes p -> Bytes 'Inc
relaxPinnedBytes = castPinnesBytes

relaxPinnedMBytes :: MBytes p e -> MBytes 'Inc e
relaxPinnedMBytes = castPinnesMBytes



ensurePinnedBytes :: Bytes p -> Bytes 'Pin
ensurePinnedBytes b = fromMaybe (convertMem b) (toPinnedBytes b)
{-# INLINE ensurePinnedBytes #-}

ensurePinnedMBytes :: MonadPrim s m => MBytes p s -> m (MBytes 'Pin s)
ensurePinnedMBytes mb =
  case toPinnedMBytes mb of
    Just pmb -> pure pmb
    Nothing  -> do
      n8 :: Count Word8 <- getCountMBytes mb
      pmb <- allocPinnedMBytes n8
      pmb <$ moveMBytesToMBytes mb 0 pmb 0 n8
{-# INLINE ensurePinnedMBytes #-}

toPinnedBytes :: Bytes p -> Maybe (Bytes 'Pin)
toPinnedBytes (Bytes b#)
  | isTrue# (isByteArrayPinned# b#) = Just (Bytes b#)
  | otherwise = Nothing
{-# INLINE toPinnedBytes #-}

toPinnedMBytes :: MBytes p s -> Maybe (MBytes 'Pin s)
toPinnedMBytes (MBytes mb#)
  | isTrue# (isMutableByteArrayPinned# mb#) = Just (MBytes mb#)
  | otherwise = Nothing
{-# INLINE toPinnedMBytes #-}



-- | Perform atomic modification of an element in the `MBytes` at the supplied
-- index. Returns the artifact of computation @__b__@.  Offset is in number of elements,
-- rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
casMBytes ::
     (MonadPrim s m, Atomic e)
  => MBytes p s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__a__@, rather than bytes.
  -> e -- ^ Expected old value
  -> e -- ^ New value
  -> m e
casMBytes (MBytes mba#) (Off (I# i#)) old new = prim $ casMutableByteArray# mba# i# old new
{-# INLINE casMBytes #-}


-- | Perform atomic read of `MBytes` at the supplied index. Offset is in number of
-- elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicReadMBytes ::
     (MonadPrim s m, Atomic e)
  => MBytes p s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__a__@, rather than bytes.
  -> m e
atomicReadMBytes (MBytes mba#) (Off (I# i#)) =
  prim $ atomicReadMutableByteArray# mba# i#
{-# INLINE atomicReadMBytes #-}


-- | Perform a write into `MBytes` at the supplied index atomically. Offset is in number
-- of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicWriteMBytes ::
     (MonadPrim s m, Atomic e)
  => MBytes p s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__a__@, rather than bytes.
  -> e
  -> m ()
atomicWriteMBytes (MBytes mba#) (Off (I# i#)) e =
  prim_ $ atomicWriteMutableByteArray# mba# i# e
{-# INLINE atomicWriteMBytes #-}


-- | Perform atomic modification of an element in the `MBytes` at the supplied
-- index. Returns the artifact of computation @__b__@.  Offset is in number of elements,
-- rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicModifyMBytes ::
     (MonadPrim s m, Atomic e)
  => MBytes p s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__a__@, rather than bytes.
  -> (e -> (e, b)) -- ^ Function that is applied to the old value and returns new value
                   -- and some artifact of computation @__b__@
  -> m b
atomicModifyMBytes (MBytes mba#) (Off (I# i#)) f =
  prim $
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
     (MonadPrim s m, Atomic e)
  => MBytes p s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__a__@, rather than bytes.
  -> (e -> e) -- ^ Function that is applied to the old value and returns new value.
  -> m ()
atomicModifyMBytes_ (MBytes mba#) (Off (I# i#)) f =
  prim_ $ atomicModifyMutableByteArray_# mba# i# f
{-# INLINE atomicModifyMBytes_ #-}


-- | Perform atomic modification of an element in the `MBytes` at the supplied
-- index. Returns the previous value.  Offset is in number of elements, rather than
-- bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicModifyFetchOldMBytes ::
     (MonadPrim s m, Atomic e)
  => MBytes p s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__a__@, rather than bytes.
  -> (e -> e) -- ^ Function that is applied to the old value and returns the new value
  -> m e
atomicModifyFetchOldMBytes (MBytes mba#) (Off (I# i#)) f =
  prim $ atomicModifyFetchOldMutableByteArray# mba# i# f
{-# INLINE atomicModifyFetchOldMBytes #-}


-- | Perform atomic modification of an element in the `MBytes` at the supplied
-- index. Returns the previous value.  Offset is in number of elements, rather than
-- bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicBoolModifyFetchOldMBytes ::
     (MonadPrim s m, Atomic e)
  => MBytes p s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__a__@, rather than bytes.
  -> (e -> e) -- ^ Function that is applied to the old value and returns the new value
  -> m e
atomicBoolModifyFetchOldMBytes (MBytes mba#) (Off (I# i#)) f =
  prim $ atomicBoolModifyFetchOldMutableByteArray# mba# i# f
{-# INLINE atomicBoolModifyFetchOldMBytes #-}


-- | Perform atomic modification of an element in the `MBytes` at the supplied
-- index.  Offset is in number of elements, rather than bytes. Implies a full memory
-- barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicModifyFetchNewMBytes ::
     (MonadPrim s m, Atomic e)
  => MBytes p s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__a__@, rather than bytes.
  -> (e -> e) -- ^ Function that is applied to the old value and returns the new value
  -> m e
atomicModifyFetchNewMBytes (MBytes mba#) (Off (I# i#)) f =
  prim $ atomicModifyFetchNewMutableByteArray# mba# i# f
{-# INLINE atomicModifyFetchNewMBytes #-}






-- | Add a numeric value to an element of a `MBytes`, corresponds to @(`+`)@ done
-- atomically. Returns the previous value.  Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicAddFetchOldMBytes ::
     (MonadPrim s m, AtomicCount e)
  => MBytes p s
  -> Off e
  -> e
  -> m e
atomicAddFetchOldMBytes (MBytes mba#) (Off (I# i#)) a =
  prim (atomicAddFetchOldMutableByteArray# mba# i# a)
{-# INLINE atomicAddFetchOldMBytes #-}

-- | Add a numeric value to an element of a `MBytes`, corresponds to @(`+`)@ done
-- atomically. Returns the new value.  Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicAddFetchNewMBytes ::
     (MonadPrim s m, AtomicCount e)
  => MBytes p s
  -> Off e
  -> e
  -> m e
atomicAddFetchNewMBytes (MBytes mba#) (Off (I# i#)) a =
  prim (atomicAddFetchNewMutableByteArray# mba# i# a)
{-# INLINE atomicAddFetchNewMBytes #-}



-- | Subtract a numeric value from an element of a `MBytes`, corresponds to
-- @(`-`)@ done atomically. Returns the previous value.  Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicSubFetchOldMBytes ::
     (MonadPrim s m, AtomicCount e)
  => MBytes p s
  -> Off e
  -> e
  -> m e
atomicSubFetchOldMBytes (MBytes mba#) (Off (I# i#)) a =
  prim (atomicSubFetchOldMutableByteArray# mba# i# a)
{-# INLINE atomicSubFetchOldMBytes #-}

-- | Subtract a numeric value from an element of a `MBytes`, corresponds to
-- @(`-`)@ done atomically. Returns the new value. Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicSubFetchNewMBytes ::
     (MonadPrim s m, AtomicCount e)
  => MBytes p s
  -> Off e
  -> e
  -> m e
atomicSubFetchNewMBytes (MBytes mba#) (Off (I# i#)) a =
  prim (atomicSubFetchNewMutableByteArray# mba# i# a)
{-# INLINE atomicSubFetchNewMBytes #-}



-- | Binary conjunction (AND) of an element of a `MBytes` with the supplied value,
-- corresponds to @(`Data.Bits..&.`)@ done atomically. Returns the previous value. Offset
-- is in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicAndFetchOldMBytes ::
     (MonadPrim s m, AtomicBits e)
  => MBytes p s
  -> Off e
  -> e
  -> m e
atomicAndFetchOldMBytes (MBytes mba#) (Off (I# i#)) a =
  prim (atomicAndFetchOldMutableByteArray# mba# i# a)
{-# INLINE atomicAndFetchOldMBytes #-}

-- | Binary conjunction (AND) of an element of a `MBytes` with the supplied value,
-- corresponds to @(`Data.Bits..&.`)@ done atomically. Returns the new value. Offset is
-- in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicAndFetchNewMBytes ::
     (MonadPrim s m, AtomicBits e)
  => MBytes p s
  -> Off e
  -> e
  -> m e
atomicAndFetchNewMBytes (MBytes mba#) (Off (I# i#)) a =
  prim (atomicAndFetchNewMutableByteArray# mba# i# a)
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
     (MonadPrim s m, AtomicBits e)
  => MBytes p s
  -> Off e
  -> e
  -> m e
atomicNandFetchOldMBytes (MBytes mba#) (Off (I# i#)) a =
  prim (atomicNandFetchOldMutableByteArray# mba# i# a)
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
     (MonadPrim s m, AtomicBits e)
  => MBytes p s
  -> Off e
  -> e
  -> m e
atomicNandFetchNewMBytes (MBytes mba#) (Off (I# i#)) a =
  prim (atomicNandFetchNewMutableByteArray# mba# i# a)
{-# INLINE atomicNandFetchNewMBytes #-}




-- | Binary disjunction (OR) of an element of a `MBytes` with the supplied value,
-- corresponds to @(`Data.Bits..|.`)@ done atomically. Returns the previous value. Offset
-- is in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicOrFetchOldMBytes ::
     (MonadPrim s m, AtomicBits e)
  => MBytes p s
  -> Off e
  -> e
  -> m e
atomicOrFetchOldMBytes (MBytes mba#) (Off (I# i#)) a =
  prim (atomicOrFetchOldMutableByteArray# mba# i# a)
{-# INLINE atomicOrFetchOldMBytes #-}

-- | Binary disjunction (OR) of an element of a `MBytes` with the supplied value,
-- corresponds to @(`Data.Bits..|.`)@ done atomically. Returns the new value. Offset is
-- in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicOrFetchNewMBytes ::
     (MonadPrim s m, AtomicBits e)
  => MBytes p s
  -> Off e
  -> e
  -> m e
atomicOrFetchNewMBytes (MBytes mba#) (Off (I# i#)) a =
  prim (atomicOrFetchNewMutableByteArray# mba# i# a)
{-# INLINE atomicOrFetchNewMBytes #-}



-- | Binary exclusive disjunction (XOR) of an element of a `MBytes` with the supplied value,
-- corresponds to @`Data.Bits.xor`@ done atomically. Returns the previous value. Offset
-- is in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicXorFetchOldMBytes ::
     (MonadPrim s m, AtomicBits e)
  => MBytes p s
  -> Off e
  -> e
  -> m e
atomicXorFetchOldMBytes (MBytes mba#) (Off (I# i#)) a =
  prim (atomicXorFetchOldMutableByteArray# mba# i# a)
{-# INLINE atomicXorFetchOldMBytes #-}

-- | Binary exclusive disjunction (XOR) of an element of a `MBytes` with the supplied value,
-- corresponds to @`Data.Bits.xor`@ done atomically. Returns the new value. Offset is
-- in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicXorFetchNewMBytes ::
     (MonadPrim s m, AtomicBits e)
  => MBytes p s
  -> Off e
  -> e
  -> m e
atomicXorFetchNewMBytes (MBytes mba#) (Off (I# i#)) a =
  prim (atomicXorFetchNewMutableByteArray# mba# i# a)
{-# INLINE atomicXorFetchNewMBytes #-}





-- | Binary negation (NOT) of an element of a `MBytes`, corresponds to
-- @(`Data.Bits.complement`)@ done atomically. Returns the previous value. Offset is in
-- number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicNotFetchOldMBytes ::
     (MonadPrim s m, AtomicBits e)
  => MBytes p s
  -> Off e
  -> m e
atomicNotFetchOldMBytes (MBytes mba#) (Off (I# i#)) =
  prim (atomicNotFetchOldMutableByteArray# mba# i#)
{-# INLINE atomicNotFetchOldMBytes #-}

-- | Binary negation (NOT) of an element of a `MBytes`, corresponds to
-- @(`Data.Bits.complement`)@ done atomically. Returns the new value. Offset is in number
-- of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicNotFetchNewMBytes ::
     (MonadPrim s m, AtomicBits e)
  => MBytes p s
  -> Off e
  -> m e
atomicNotFetchNewMBytes (MBytes mba#) (Off (I# i#)) =
  prim (atomicNotFetchNewMutableByteArray# mba# i#)
{-# INLINE atomicNotFetchNewMBytes #-}




prefetchBytes0 :: (MonadPrim s m, Prim e) => Bytes p -> Off e -> m ()
prefetchBytes0 (Bytes b#) off = prim_ (prefetchByteArray0# b# (fromOff# off))
{-# INLINE prefetchBytes0 #-}

prefetchMBytes0 :: (MonadPrim s m, Prim e) => MBytes p s -> Off e -> m ()
prefetchMBytes0 (MBytes mb#) off = prim_ (prefetchMutableByteArray0# mb# (fromOff# off))
{-# INLINE prefetchMBytes0 #-}

prefetchBytes1 :: (MonadPrim s m, Prim e) => Bytes p -> Off e -> m ()
prefetchBytes1 (Bytes b#) off = prim_ (prefetchByteArray1# b# (fromOff# off))
{-# INLINE prefetchBytes1 #-}

prefetchMBytes1 :: (MonadPrim s m, Prim e) => MBytes p s -> Off e -> m ()
prefetchMBytes1 (MBytes mb#) off = prim_ (prefetchMutableByteArray1# mb# (fromOff# off))
{-# INLINE prefetchMBytes1 #-}

prefetchBytes2 :: (MonadPrim s m, Prim e) => Bytes p -> Off e -> m ()
prefetchBytes2 (Bytes b#) off = prim_ (prefetchByteArray2# b# (fromOff# off))
{-# INLINE prefetchBytes2 #-}

prefetchMBytes2 :: (MonadPrim s m, Prim e) => MBytes p s -> Off e -> m ()
prefetchMBytes2 (MBytes mb#) off = prim_ (prefetchMutableByteArray2# mb# (fromOff# off))
{-# INLINE prefetchMBytes2 #-}

prefetchBytes3 :: (MonadPrim s m, Prim e) => Bytes p -> Off e -> m ()
prefetchBytes3 (Bytes b#) off = prim_ (prefetchByteArray3# b# (fromOff# off))
{-# INLINE prefetchBytes3 #-}

prefetchMBytes3 :: (MonadPrim s m, Prim e) => MBytes p s -> Off e -> m ()
prefetchMBytes3 (MBytes mb#) off = prim_ (prefetchMutableByteArray3# mb# (fromOff# off))
{-# INLINE prefetchMBytes3 #-}

