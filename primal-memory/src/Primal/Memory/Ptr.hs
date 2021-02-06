{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Primal.Memory.Ptr
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Memory.Ptr
  ( module GHC.Ptr
  , plusOffPtr
  , plusByteOffPtr
  , minusOffPtr
  , minusOffRemPtr
  , minusByteOffPtr
  , readPtr
  , readOffPtr
  , readByteOffPtr
  , writePtr
  , writeOffPtr
  , writeByteOffPtr
  , setOffPtr
  , copyPtrToPtr
  , copyByteOffPtrToPtr
  , movePtrToPtr
  , moveByteOffPtrToPtr
  , comparePtrToPtr
  , compareByteOffPtrToPtr
  , freeHaskellFunPtr
  , module X
  , WordPtr(..)
  , ptrToWordPtr
  , wordPtrToPtr
  , IntPtr(..)
  , ptrToIntPtr
  , intPtrToPtr
  -- * Atomic
  , casOffPtr
  , atomicModifyOffPtr
  , atomicModifyOffPtr_
  , atomicModifyFetchOldOffPtr
  , atomicModifyFetchNewOffPtr
  -- ** Numeric
  , atomicAddFetchOldOffPtr
  , atomicAddFetchNewOffPtr
  , atomicSubFetchOldOffPtr
  , atomicSubFetchNewOffPtr
  -- ** Binary
  , atomicAndFetchOldOffPtr
  , atomicAndFetchNewOffPtr
  , atomicNandFetchOldOffPtr
  , atomicNandFetchNewOffPtr
  , atomicOrFetchOldOffPtr
  , atomicOrFetchNewOffPtr
  , atomicXorFetchOldOffPtr
  , atomicXorFetchNewOffPtr
  , atomicNotFetchOldOffPtr
  , atomicNotFetchNewOffPtr
  -- * Prefetch
  , prefetchPtr0
  , prefetchPtr1
  , prefetchPtr2
  , prefetchPtr3
  , prefetchOffPtr0
  , prefetchOffPtr1
  , prefetchOffPtr2
  , prefetchOffPtr3
  -- * Copy/Move
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
  , module Primal.Unbox
  ) where


import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr as X hiding (IntPtr, WordPtr, freeHaskellFunPtr, intPtrToPtr,
                         ptrToIntPtr, ptrToWordPtr, wordPtrToPtr)
import qualified Foreign.Ptr as GHC (freeHaskellFunPtr)
import GHC.Ptr
import Primal.Foreign
import Primal.Memory.Bytes.Internal (Bytes(..), MBytes(..))
import Primal.Monad
import Primal.Monad.Unsafe
import Primal.Unbox
import Primal.Unbox.Atomic
import Primal.Unbox.Class



copyPtrToMBytes ::
     (MonadPrim s m, Unbox e) => Ptr e -> Off e -> MBytes p s -> Off e -> Count e -> m ()
copyPtrToMBytes src srcOff dst dstOff =
  copyByteOffPtrToMBytes src (toByteOff srcOff) dst (toByteOff dstOff)
{-# INLINE copyPtrToMBytes #-}



copyByteOffPtrToMBytes ::
     (MonadPrim s m, Unbox e) => Ptr e -> Off Word8 -> MBytes p s -> Off Word8 -> Count e -> m ()
copyByteOffPtrToMBytes (Ptr srcAddr#) (Off (I# srcOff#)) (MBytes dst#) (Off (I# dstOff#)) c =
  prim_ $ copyAddrToByteArray# (srcAddr# `plusAddr#` srcOff#) dst# dstOff# (unCountBytes# c)
{-# INLINE copyByteOffPtrToMBytes #-}


copyBytesToPtr :: (MonadPrim s m, Unbox e) => Bytes p -> Off e -> Ptr e -> Off e -> Count e -> m ()
copyBytesToPtr src srcOff dst dstOff =
  copyByteOffBytesToPtr src (toByteOff srcOff) dst (toByteOff dstOff)
{-# INLINE copyBytesToPtr #-}


copyByteOffBytesToPtr ::
     (MonadPrim s m, Unbox e)
  => Bytes p
  -> Off Word8
  -> Ptr e
  -> Off Word8
  -> Count e
  -> m ()
copyByteOffBytesToPtr (Bytes src#) (Off (I# srcOff#)) (Ptr dstAddr#) (Off (I# dstOff#)) c =
  prim_ $
  copyByteArrayToAddr#
    src#
    srcOff#
    (dstAddr# `plusAddr#` dstOff#)
    (unCountBytes# c)
{-# INLINE copyByteOffBytesToPtr #-}


copyMBytesToPtr :: (MonadPrim s m, Unbox e) => MBytes p s -> Off e -> Ptr e -> Off e -> Count e -> m ()
copyMBytesToPtr src srcOff dst dstOff =
  copyByteOffMBytesToPtr src (toByteOff srcOff) dst (toByteOff dstOff)
{-# INLINE copyMBytesToPtr #-}


copyByteOffMBytesToPtr ::
     (MonadPrim s m, Unbox e)
  => MBytes p s
  -> Off Word8
  -> Ptr e
  -> Off Word8
  -> Count e
  -> m ()
copyByteOffMBytesToPtr (MBytes src#) (Off (I# srcOff#)) (Ptr dstAddr#) (Off (I# dstOff#)) c =
  prim_ $
  copyMutableByteArrayToAddr#
    src#
    srcOff#
    (dstAddr# `plusAddr#` dstOff#)
    (unCountBytes# c)
{-# INLINE copyByteOffMBytesToPtr #-}


movePtrToMBytes :: (MonadPrim s m, Unbox e) => Ptr e -> Off e -> MBytes p s -> Off e -> Count e -> m ()
movePtrToMBytes src srcOff dst dstOff =
  moveByteOffPtrToMBytes src (toByteOff srcOff) dst (toByteOff dstOff)
{-# INLINE movePtrToMBytes #-}

moveByteOffPtrToMBytes ::
     (MonadPrim s m, Unbox e)
  => Ptr e
  -> Off Word8
  -> MBytes p s
  -> Off Word8
  -> Count e
  -> m ()
moveByteOffPtrToMBytes (Ptr srcAddr#) (Off (I# srcOff#)) (MBytes dst#) (Off (I# dstOff#)) c =
  unsafeIOToPrim $
  memmoveMutableByteArrayFromAddr# srcAddr# srcOff# dst# dstOff# (unCountBytes# c)
{-# INLINE moveByteOffPtrToMBytes #-}

moveMBytesToPtr :: (MonadPrim s m, Unbox e) => MBytes p s -> Off e -> Ptr e -> Off e -> Count e -> m ()
moveMBytesToPtr src srcOff dst dstOff =
  moveByteOffMBytesToPtr src (toByteOff srcOff) dst (toByteOff dstOff)
{-# INLINE moveMBytesToPtr #-}


moveByteOffMBytesToPtr ::
  (MonadPrim s m, Unbox e) => MBytes p s -> Off Word8 -> Ptr e -> Off Word8 -> Count e -> m ()
moveByteOffMBytesToPtr (MBytes src#) (Off (I# srcOff#)) (Ptr dstAddr#) (Off (I# dstOff#)) c =
  unsafeIOToPrim $
  memmoveMutableByteArrayToAddr# src# srcOff# dstAddr# dstOff# (unCountBytes# c)
{-# INLINE moveByteOffMBytesToPtr #-}


compareByteOffBytesToPtr ::
     Unbox e => Bytes p -> Off Word8 -> Ptr e -> Off Word8 -> Count e -> Ordering
compareByteOffBytesToPtr (Bytes b#) (Off (I# off1#)) (Ptr addr#) (Off (I# off2#)) c =
  toOrdering# (memcmpByteArrayAddr# b# off1# addr# off2# (unCountBytes# c))
{-# INLINE compareByteOffBytesToPtr #-}

compareByteOffPtrToBytes ::
     Unbox e => Ptr e -> Off Word8 -> Bytes p -> Off Word8 -> Count e -> Ordering
compareByteOffPtrToBytes (Ptr addr#) (Off (I# off1#)) (Bytes b#) (Off (I# off2#)) c =
  toOrdering# (memcmpAddrByteArray# addr# off1# b# off2# (unCountBytes# c))
{-# INLINE compareByteOffPtrToBytes #-}




setOffPtr ::
     (MonadPrim s m, Unbox e)
  => Ptr e -- ^ Chunk of memory to fill
  -> Off e -- ^ Offset in number of elements
  -> Count e -- ^ Number of cells to fill
  -> e -- ^ A value to fill the cells with
  -> m ()
setOffPtr (Ptr addr#) (Off (I# o#)) (Count (I# n#)) a = prim_ (setOffAddr# addr# o# n# a)
{-# INLINE setOffPtr #-}


readOffPtr :: (MonadPrim s m, Unbox e) => Ptr e -> Off e -> m e
readOffPtr (Ptr addr#) (Off (I# i#)) = prim (readOffAddr# addr# i#)
{-# INLINE readOffPtr #-}


readByteOffPtr :: (MonadPrim s m, Unbox e) => Ptr e -> Off Word8 -> m e
readByteOffPtr ptr (Off i) =
  case ptr `plusPtr` i of
    Ptr addr# -> prim (readOffAddr# addr# 0#)
{-# INLINE readByteOffPtr #-}

writeOffPtr :: (MonadPrim s m, Unbox e) => Ptr e -> Off e -> e -> m ()
writeOffPtr (Ptr addr#) (Off (I# i#)) a = prim_ (writeOffAddr# addr# i# a)
{-# INLINE writeOffPtr #-}

writeByteOffPtr :: (MonadPrim s m, Unbox e) => Ptr e -> Off Word8 -> e -> m ()
writeByteOffPtr ptr (Off i) a =
  case ptr `plusPtr` i of
    Ptr addr# -> prim_ (writeOffAddr# addr# 0# a)
{-# INLINE writeByteOffPtr #-}

readPtr :: (MonadPrim s m, Unbox e) => Ptr e -> m e
readPtr (Ptr addr#) = prim (readOffAddr# addr# 0#)
{-# INLINE readPtr #-}

writePtr :: (MonadPrim s m, Unbox e) => Ptr e -> e -> m ()
writePtr (Ptr addr#) a = prim_ (writeOffAddr# addr# 0# a)
{-# INLINE writePtr #-}

plusByteOffPtr :: Ptr e -> Off Word8 -> Ptr e
plusByteOffPtr (Ptr addr#) (Off (I# off#)) = Ptr (addr# `plusAddr#` off#)
{-# INLINE plusByteOffPtr #-}


plusOffPtr :: Unbox e => Ptr e -> Off e -> Ptr e
plusOffPtr (Ptr addr#) off = Ptr (addr# `plusAddr#` unOffBytes# off)
{-# INLINE plusOffPtr #-}

-- | Find the offset in bytes that is between the two pointers by subtracting one address
-- from another.
--
-- @since 0.1.0
minusByteOffPtr :: Ptr e -> Ptr e -> Off Word8
minusByteOffPtr (Ptr xaddr#) (Ptr yaddr#) = Off (I# (xaddr# `minusAddr#` yaddr#))
{-# INLINE minusByteOffPtr #-}

-- | Find the offset in number of elements that is between the two pointers by subtracting
-- one address from another and dividing the result by the size of an element.
--
-- @since 0.1.0
minusOffPtr :: Unbox e => Ptr e -> Ptr e -> Off e
minusOffPtr (Ptr xaddr#) (Ptr yaddr#) =
  fromByteOff (Off (I# (xaddr# `minusAddr#` yaddr#)))
{-# INLINE minusOffPtr #-}

-- | Same as `minusOffPtr`, but will also return the remainder in bytes that is left over.
--
-- @since 0.1.0
minusOffRemPtr :: Unbox e => Ptr e -> Ptr e -> (Off e, Off Word8)
minusOffRemPtr (Ptr xaddr#) (Ptr yaddr#) =
  fromByteOffRem (Off (I# (xaddr# `minusAddr#` yaddr#)))
{-# INLINE minusOffRemPtr #-}

copyPtrToPtr :: (MonadPrim s m, Unbox e) => Ptr e -> Off e -> Ptr e -> Off e -> Count e -> m ()
copyPtrToPtr srcPtr srcOff dstPtr dstOff c =
  unsafeIOToPrim $
  copyBytes
    (dstPtr `plusOffPtr` dstOff)
    (srcPtr `plusOffPtr` srcOff)
    (unCountBytes c)
{-# INLINE copyPtrToPtr #-}

copyByteOffPtrToPtr ::
     (MonadPrim s m, Unbox e)
  => Ptr e
  -> Off Word8
  -> Ptr e
  -> Off Word8
  -> Count e
  -> m ()
copyByteOffPtrToPtr srcPtr (Off srcOff) dstPtr (Off dstOff) c =
  unsafeIOToPrim $
  copyBytes
    (dstPtr `plusPtr` dstOff)
    (srcPtr `plusPtr` srcOff)
    (unCountBytes c)
{-# INLINE copyByteOffPtrToPtr #-}

movePtrToPtr :: (MonadPrim s m, Unbox e) => Ptr e -> Off e -> Ptr e -> Off e -> Count e -> m ()
movePtrToPtr src srcOff dst dstOff =
  moveByteOffPtrToPtr src (toByteOff srcOff) dst (toByteOff dstOff)
{-# INLINE movePtrToPtr #-}

moveByteOffPtrToPtr ::
     (MonadPrim s m, Unbox e)
  => Ptr e
  -> Off Word8
  -> Ptr e
  -> Off Word8
  -> Count e
  -> m ()
moveByteOffPtrToPtr (Ptr srcAddr#) (Off (I# srcOff#)) (Ptr dstAddr#) (Off (I# dstOff#)) c =
  unsafeIOToPrim $ memmoveAddr# srcAddr# srcOff# dstAddr# dstOff# (unCountBytes# c)
{-# INLINE moveByteOffPtrToPtr #-}

-- | Compare memory between two pointers. Offsets and count is in number of elements,
-- instead of byte count. Use `compareByteOffPtrToPtr` when offset in bytes is required.
comparePtrToPtr :: Unbox e => Ptr e -> Off e -> Ptr e -> Off e -> Count e -> Ordering
comparePtrToPtr (Ptr addr1#) off1 (Ptr addr2#) off2 c =
  toOrdering# (memcmpAddr# addr1# (unOffBytes# off1) addr2# (unOffBytes# off2) (unCountBytes# c))
{-# INLINE comparePtrToPtr #-}

-- | Same as `comparePtrToPtr`, except offset is in bytes instead of number of elements.
compareByteOffPtrToPtr ::
     Unbox e => Ptr e -> Off Word8 -> Ptr e -> Off Word8 -> Count e -> Ordering
compareByteOffPtrToPtr (Ptr addr1#) (Off (I# off1#)) (Ptr addr2#) (Off (I# off2#)) c =
  toOrdering# (memcmpAddr# addr1# off1# addr2# off2# (unCountBytes# c))
{-# INLINE compareByteOffPtrToPtr #-}




-- | Perform atomic modification of an element in the `Ptr` at the supplied
-- index. Returns the artifact of computation @__b__@.  Offset is in number of elements,
-- rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
casOffPtr ::
     (MonadPrim s m, Atomic e)
  => Ptr e -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__a__@, rather than bytes.
  -> e -- ^ Expected old value
  -> e -- ^ New value
  -> m e
casOffPtr (Ptr addr#) (Off (I# i#)) old new = prim $ casOffAddr# addr# i# old new
{-# INLINE casOffPtr #-}

-- | Perform atomic modification of an element in the `Ptr` at the supplied
-- index. Returns the artifact of computation @__b__@.  Offset is in number of elements,
-- rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicModifyOffPtr ::
     (MonadPrim s m, Atomic e)
  => Ptr e -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__a__@, rather than bytes.
  -> (e -> (e, b)) -- ^ Function that is applied to the old value and returns new value
                   -- and some artifact of computation @__b__@
  -> m b
atomicModifyOffPtr (Ptr addr#) (Off (I# i#)) f =
  prim $
  atomicModifyOffAddr# addr# i# $ \a ->
    case f a of
      (a', b) -> (# a', b #)
{-# INLINE atomicModifyOffPtr #-}

-- | Perform atomic modification of an element in the `Ptr` at the supplied
-- index.  Offset is in number of elements, rather than bytes. Implies a full memory
-- barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicModifyOffPtr_ ::
     (MonadPrim s m, Atomic e)
  => Ptr e -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__a__@, rather than bytes.
  -> (e -> e) -- ^ Function that is applied to the old value and returns new value.
  -> m ()
atomicModifyOffPtr_ (Ptr addr#) (Off (I# i#)) f =
  prim_ $ atomicModifyOffAddr_# addr# i# f
{-# INLINE atomicModifyOffPtr_ #-}


-- | Perform atomic modification of an element in the `Ptr` at the supplied
-- index. Returns the previous value.  Offset is in number of elements, rather than
-- bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicModifyFetchOldOffPtr ::
     (MonadPrim s m, Atomic e)
  => Ptr e -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__a__@, rather than bytes.
  -> (e -> e) -- ^ Function that is applied to the old value and returns the new value
  -> m e
atomicModifyFetchOldOffPtr (Ptr addr#) (Off (I# i#)) f =
  prim $ atomicModifyFetchOldOffAddr# addr# i# f
{-# INLINE atomicModifyFetchOldOffPtr #-}


-- | Perform atomic modification of an element in the `Ptr` at the supplied
-- index.  Offset is in number of elements, rather than bytes. Implies a full memory
-- barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicModifyFetchNewOffPtr ::
     (MonadPrim s m, Atomic e)
  => Ptr e -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__a__@, rather than bytes.
  -> (e -> e) -- ^ Function that is applied to the old value and returns the new value
  -> m e
atomicModifyFetchNewOffPtr (Ptr addr#) (Off (I# i#)) f =
  prim $ atomicModifyFetchNewOffAddr# addr# i# f
{-# INLINE atomicModifyFetchNewOffPtr #-}



-- | Add a numeric value to an element of a `Ptr`, corresponds to @(`+`)@ done
-- atomically. Returns the previous value.  Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicAddFetchOldOffPtr ::
     (MonadPrim s m, AtomicCount e)
  => Ptr e
  -> Off e
  -> e
  -> m e
atomicAddFetchOldOffPtr (Ptr addr#) (Off (I# i#)) a =
  prim (atomicAddFetchOldOffAddr# addr# i# a)
{-# INLINE atomicAddFetchOldOffPtr #-}

-- | Add a numeric value to an element of a `Ptr`, corresponds to @(`+`)@ done
-- atomically. Returns the new value.  Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicAddFetchNewOffPtr ::
     (MonadPrim s m, AtomicCount e)
  => Ptr e
  -> Off e
  -> e
  -> m e
atomicAddFetchNewOffPtr (Ptr addr#) (Off (I# i#)) a =
  prim (atomicAddFetchNewOffAddr# addr# i# a)
{-# INLINE atomicAddFetchNewOffPtr #-}



-- | Subtract a numeric value from an element of a `Ptr`, corresponds to
-- @(`-`)@ done atomically. Returns the previous value.  Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicSubFetchOldOffPtr ::
     (MonadPrim s m, AtomicCount e)
  => Ptr e
  -> Off e
  -> e
  -> m e
atomicSubFetchOldOffPtr (Ptr addr#) (Off (I# i#)) a =
  prim (atomicSubFetchOldOffAddr# addr# i# a)
{-# INLINE atomicSubFetchOldOffPtr #-}

-- | Subtract a numeric value from an element of a `Ptr`, corresponds to
-- @(`-`)@ done atomically. Returns the new value. Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicSubFetchNewOffPtr ::
     (MonadPrim s m, AtomicCount e)
  => Ptr e
  -> Off e
  -> e
  -> m e
atomicSubFetchNewOffPtr (Ptr addr#) (Off (I# i#)) a =
  prim (atomicSubFetchNewOffAddr# addr# i# a)
{-# INLINE atomicSubFetchNewOffPtr #-}



-- | Binary conjunction (AND) of an element of a `Ptr` with the supplied value,
-- corresponds to @(`Data.Bits..&.`)@ done atomically. Returns the previous value. Offset
-- is in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicAndFetchOldOffPtr ::
     (MonadPrim s m, AtomicBits e)
  => Ptr e
  -> Off e
  -> e
  -> m e
atomicAndFetchOldOffPtr (Ptr addr#) (Off (I# i#)) a =
  prim (atomicAndFetchOldOffAddr# addr# i# a)
{-# INLINE atomicAndFetchOldOffPtr #-}

-- | Binary conjunction (AND) of an element of a `Ptr` with the supplied value,
-- corresponds to @(`Data.Bits..&.`)@ done atomically. Returns the new value. Offset is
-- in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicAndFetchNewOffPtr ::
     (MonadPrim s m, AtomicBits e)
  => Ptr e
  -> Off e
  -> e
  -> m e
atomicAndFetchNewOffPtr (Ptr addr#) (Off (I# i#)) a =
  prim (atomicAndFetchNewOffAddr# addr# i# a)
{-# INLINE atomicAndFetchNewOffPtr #-}



-- | Negation of binary conjunction (NAND) of an element of a `Ptr` with the
-- supplied value, corresponds to @\\x y -> `Data.Bits.complement` (x `Data.Bits..&.` y)@
-- done atomically. Returns the previous value. Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicNandFetchOldOffPtr ::
     (MonadPrim s m, AtomicBits e)
  => Ptr e
  -> Off e
  -> e
  -> m e
atomicNandFetchOldOffPtr (Ptr addr#) (Off (I# i#)) a =
  prim (atomicNandFetchOldOffAddr# addr# i# a)
{-# INLINE atomicNandFetchOldOffPtr #-}

-- | Negation of binary conjunction (NAND)  of an element of a `Ptr` with the supplied
-- value, corresponds to @\\x y -> `Data.Bits.complement` (x `Data.Bits..&.` y)@ done
-- atomically. Returns the new value. Offset is in number of elements, rather than
-- bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicNandFetchNewOffPtr ::
     (MonadPrim s m, AtomicBits e)
  => Ptr e
  -> Off e
  -> e
  -> m e
atomicNandFetchNewOffPtr (Ptr addr#) (Off (I# i#)) a =
  prim (atomicNandFetchNewOffAddr# addr# i# a)
{-# INLINE atomicNandFetchNewOffPtr #-}




-- | Binary disjunction (OR) of an element of a `Ptr` with the supplied value,
-- corresponds to @(`Data.Bits..|.`)@ done atomically. Returns the previous value. Offset
-- is in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicOrFetchOldOffPtr ::
     (MonadPrim s m, AtomicBits e)
  => Ptr e
  -> Off e
  -> e
  -> m e
atomicOrFetchOldOffPtr (Ptr addr#) (Off (I# i#)) a =
  prim (atomicOrFetchOldOffAddr# addr# i# a)
{-# INLINE atomicOrFetchOldOffPtr #-}

-- | Binary disjunction (OR) of an element of a `Ptr` with the supplied value,
-- corresponds to @(`Data.Bits..|.`)@ done atomically. Returns the new value. Offset is
-- in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicOrFetchNewOffPtr ::
     (MonadPrim s m, AtomicBits e)
  => Ptr e
  -> Off e
  -> e
  -> m e
atomicOrFetchNewOffPtr (Ptr addr#) (Off (I# i#)) a =
  prim (atomicOrFetchNewOffAddr# addr# i# a)
{-# INLINE atomicOrFetchNewOffPtr #-}



-- | Binary exclusive disjunction (XOR) of an element of a `Ptr` with the supplied value,
-- corresponds to @`Data.Bits.xor`@ done atomically. Returns the previous value. Offset
-- is in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicXorFetchOldOffPtr ::
     (MonadPrim s m, AtomicBits e)
  => Ptr e
  -> Off e
  -> e
  -> m e
atomicXorFetchOldOffPtr (Ptr addr#) (Off (I# i#)) a =
  prim (atomicXorFetchOldOffAddr# addr# i# a)
{-# INLINE atomicXorFetchOldOffPtr #-}

-- | Binary exclusive disjunction (XOR) of an element of a `Ptr` with the supplied value,
-- corresponds to @`Data.Bits.xor`@ done atomically. Returns the new value. Offset is
-- in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicXorFetchNewOffPtr ::
     (MonadPrim s m, AtomicBits e)
  => Ptr e
  -> Off e
  -> e
  -> m e
atomicXorFetchNewOffPtr (Ptr addr#) (Off (I# i#)) a =
  prim (atomicXorFetchNewOffAddr# addr# i# a)
{-# INLINE atomicXorFetchNewOffPtr #-}





-- | Binary negation (NOT) of an element of a `Ptr`, corresponds to
-- @(`Data.Bits.complement`)@ done atomically. Returns the previous value. Offset is in
-- number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicNotFetchOldOffPtr ::
     (MonadPrim s m, AtomicBits e)
  => Ptr e
  -> Off e
  -> m e
atomicNotFetchOldOffPtr (Ptr addr#) (Off (I# i#)) =
  prim (atomicNotFetchOldOffAddr# addr# i#)
{-# INLINE atomicNotFetchOldOffPtr #-}

-- | Binary negation (NOT) of an element of a `Ptr`, corresponds to
-- @(`Data.Bits.complement`)@ done atomically. Returns the new value. Offset is in number
-- of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicNotFetchNewOffPtr ::
     (MonadPrim s m, AtomicBits e)
  => Ptr e
  -> Off e
  -> m e
atomicNotFetchNewOffPtr (Ptr addr#) (Off (I# i#)) =
  prim (atomicNotFetchNewOffAddr# addr# i#)
{-# INLINE atomicNotFetchNewOffPtr #-}





prefetchPtr0 :: MonadPrim s m => Ptr e -> m ()
prefetchPtr0 (Ptr b#) = prim_ (prefetchAddr0# b# 0#)
{-# INLINE prefetchPtr0 #-}

prefetchPtr1 :: MonadPrim s m => Ptr a -> m ()
prefetchPtr1 (Ptr b#) = prim_ (prefetchAddr1# b# 0#)
{-# INLINE prefetchPtr1 #-}

prefetchPtr2 :: MonadPrim s m => Ptr e -> m ()
prefetchPtr2 (Ptr b#) = prim_ (prefetchAddr2# b# 0#)
{-# INLINE prefetchPtr2 #-}

prefetchPtr3 :: MonadPrim s m => Ptr e -> m ()
prefetchPtr3 (Ptr b#) = prim_ (prefetchAddr3# b# 0#)
{-# INLINE prefetchPtr3 #-}

prefetchOffPtr0 :: (MonadPrim s m, Unbox e) => Ptr e -> Off e -> m ()
prefetchOffPtr0 (Ptr b#) off = prim_ (prefetchAddr0# b# (unOffBytes# off))
{-# INLINE prefetchOffPtr0 #-}

prefetchOffPtr1 :: (MonadPrim s m, Unbox e) => Ptr e -> Off e -> m ()
prefetchOffPtr1 (Ptr b#) off = prim_ (prefetchAddr1# b# (unOffBytes# off))
{-# INLINE prefetchOffPtr1 #-}

prefetchOffPtr2 :: (MonadPrim s m, Unbox e) => Ptr e -> Off e -> m ()
prefetchOffPtr2 (Ptr b#) off = prim_ (prefetchAddr2# b# (unOffBytes# off))
{-# INLINE prefetchOffPtr2 #-}

prefetchOffPtr3 :: (MonadPrim s m, Unbox e) => Ptr e -> Off e -> m ()
prefetchOffPtr3 (Ptr b#) off = prim_ (prefetchAddr3# b# (unOffBytes# off))
{-# INLINE prefetchOffPtr3 #-}

-- | Same as `GHC.freeHaskellFunPtr`
--
-- @since 0.1.0
freeHaskellFunPtr :: MonadPrim s m => FunPtr a -> m ()
freeHaskellFunPtr = unsafeIOToPrim . GHC.freeHaskellFunPtr
