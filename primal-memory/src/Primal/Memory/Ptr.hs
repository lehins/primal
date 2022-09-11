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
  , mallocPtr
  , callocPtr
  , reallocPtr
  , freePtr
  , freeFinalizerPtr
  , guardedFreePtr
  , guardedFreeFinalizerEnvPtr
  , isSamePtr
  , plusOffPtr
  , plusByteOffPtr
  , minusOffPtr
  , minusOffRemPtr
  , minusByteOffPtr
  , minusByteCountPtr
  , plusCountPtr
  , plusByteCountPtr
  , indexPtr
  , indexOffPtr
  , indexByteOffPtr
  , readPtr
  , readOffPtr
  , readByteOffPtr
  , writePtr
  , writeOffPtr
  , writeByteOffPtr
  , setOffPtr
  , setByteOffPtr
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
  -- * Bytes
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
  -- ** FFI calls
  , mallocIO
  , callocIO
  , reallocIO
  , freeIO
  , guardedFreeIO
  ) where


import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr as X hiding (IntPtr, WordPtr, freeHaskellFunPtr, intPtrToPtr,
                         ptrToIntPtr, ptrToWordPtr, wordPtrToPtr)
import qualified Foreign.Ptr as GHC (freeHaskellFunPtr)
import qualified Foreign.ForeignPtr as GHC (FinalizerPtr, FinalizerEnvPtr)
import GHC.Ptr
import Primal.Foreign
import Primal.Monad
import Primal.Monad.Unsafe
import Primal.Element.Unbox
import Primal.Element.Unbox.Atomic

foreign import ccall unsafe "stdlib.h malloc"
  mallocIO :: CSize -> IO (Ptr a)
foreign import ccall unsafe "stdlib.h calloc"
  callocIO :: CSize -> CSize -> IO (Ptr a)
foreign import ccall unsafe "stdlib.h realloc"
  reallocIO :: Ptr a -> CSize -> IO (Ptr b)

foreign import ccall unsafe "stdlib.h free"
  freeIO :: Ptr a -> IO ()
foreign import ccall unsafe "stdlib.h &free"
  freeFinalizerPtr :: GHC.FinalizerPtr a

foreign import ccall unsafe "primal_memory.c guarded_free"
  guardedFreeIO :: Ptr CBool -> Ptr a -> IO ()
foreign import ccall unsafe "primal_memory.c &guarded_free"
  guardedFreeFinalizerEnvPtr :: GHC.FinalizerEnvPtr CBool a


mallocPtr :: forall e s m. (Unbox e, Primal s m) => Count e -> m (Ptr e)
mallocPtr = unsafeIOToPrimal . mallocIO . fromIntegral . toByteCount

callocPtr :: forall e s m. (Unbox e, Primal s m) => Count e -> m (Ptr e)
callocPtr c = unsafeIOToPrimal $ callocIO (fromIntegral c) (fromIntegral (byteCountProxy c))

reallocPtr :: forall e s m. (Unbox e, Primal s m) => Ptr e -> Count e -> m (Ptr e)
reallocPtr ptr c =
  unsafeIOToPrimal $ reallocIO ptr (fromIntegral (toByteCount c))

freePtr ::
     forall e s m. (Unbox e, Primal s m)
  => Ptr e
  -- ^ Pointer to free. It must have been allocated with either `mallocPtr`,
  -- `callocPtr` or `reallocPtr` and must not have been freed before.
  -> m ()
freePtr = unsafeIOToPrimal . freeIO

guardedFreePtr ::
     forall e s m. (Unbox e, Primal s m)
  => Ptr CBool
  -- ^ If this pointer contains @false@ or is null, then @free@ call will procede as
  -- normal, otherwise it is skipped.
  -> Ptr e
  -- ^ Pointer to free. It must have been allocated with either `mallocPtr`,
  -- `callocPtr` or `reallocPtr` and must not have been freed before.
  -> m ()
guardedFreePtr envPtr = unsafeIOToPrimal . guardedFreeIO envPtr


isSamePtr :: Ptr e -> Ptr e -> Bool
isSamePtr (Ptr a1#) (Ptr a2#) = isTrue# (a1# `eqAddr#` a2#)
{-# INLINE isSamePtr #-}


setOffPtr ::
     (Primal s m, Unbox e)
  => Ptr e -- ^ Chunk of memory to fill
  -> Off e -- ^ Offset in number of elements
  -> Count e -- ^ Number of cells to fill
  -> e -- ^ A value to fill the cells with
  -> m ()
setOffPtr (Ptr addr#) (Off (I# o#)) (Count (I# n#)) a = primal_ (setOffAddr# addr# o# n# a)
{-# INLINE setOffPtr #-}

setByteOffPtr ::
     (Primal s m, Unbox e)
  => Ptr e -- ^ Chunk of memory to fill
  -> Off Word8 -- ^ Offset in number of Bytes
  -> Count e -- ^ Number of cells to fill
  -> e -- ^ A value to fill the cells with
  -> m ()
setByteOffPtr (Ptr addr#) (Off (I# o#)) (Count (I# n#)) a = primal_ (setByteOffAddr# addr# o# n# a)
{-# INLINE setByteOffPtr #-}


readOffPtr :: (Primal s m, Unbox e) => Ptr e -> Off e -> m e
readOffPtr (Ptr addr#) (Off (I# i#)) = primal (readOffAddr# addr# i#)
{-# INLINE readOffPtr #-}


readByteOffPtr :: (Primal s m, Unbox e) => Ptr e -> Off Word8 -> m e
readByteOffPtr (Ptr addr#) (Off (I# i#)) = primal (readByteOffAddr# addr# i#)
{-# INLINE readByteOffPtr #-}

writeOffPtr :: (Primal s m, Unbox e) => Ptr e -> Off e -> e -> m ()
writeOffPtr (Ptr addr#) (Off (I# i#)) a = primal_ (writeOffAddr# addr# i# a)
{-# INLINE writeOffPtr #-}

writeByteOffPtr :: (Primal s m, Unbox e) => Ptr e -> Off Word8 -> e -> m ()
writeByteOffPtr (Ptr addr#) (Off (I# i#)) a = primal_ (writeByteOffAddr# addr# i# a)
{-# INLINE writeByteOffPtr #-}

indexPtr :: Unbox e => Ptr e -> e
indexPtr (Ptr addr#) = indexOffAddr# addr# 0#
{-# INLINE indexPtr #-}

indexOffPtr :: Unbox e => Ptr e -> Off e -> e
indexOffPtr (Ptr addr#) (Off (I# off#)) = indexOffAddr# addr# off#
{-# INLINE indexOffPtr #-}

indexByteOffPtr :: Unbox e => Ptr e -> Off Word8 -> e
indexByteOffPtr (Ptr addr#) (Off (I# off#)) = indexByteOffAddr# addr# off#
{-# INLINE indexByteOffPtr #-}

readPtr :: (Primal s m, Unbox e) => Ptr e -> m e
readPtr (Ptr addr#) = primal (readOffAddr# addr# 0#)
{-# INLINE readPtr #-}

writePtr :: (Primal s m, Unbox e) => Ptr e -> e -> m ()
writePtr (Ptr addr#) a = primal_ (writeOffAddr# addr# 0# a)
{-# INLINE writePtr #-}

plusByteOffPtr :: Ptr e -> Off Word8 -> Ptr e
plusByteOffPtr (Ptr addr#) (Off (I# off#)) = Ptr (addr# `plusAddr#` off#)
{-# INLINE plusByteOffPtr #-}

plusOffPtr :: Unbox e => Ptr e -> Off e -> Ptr e
plusOffPtr (Ptr addr#) off = Ptr (addr# `plusAddr#` unOffBytes# off)
{-# INLINE plusOffPtr #-}


plusByteCountPtr :: Ptr e -> Count Word8 -> Ptr e
plusByteCountPtr (Ptr addr#) (Count (I# off#)) = Ptr (addr# `plusAddr#` off#)
{-# INLINE plusByteCountPtr #-}

plusCountPtr :: Unbox e => Ptr e -> Count e -> Ptr e
plusCountPtr (Ptr addr#) off = Ptr (addr# `plusAddr#` unCountBytes# off)
{-# INLINE plusCountPtr #-}


-- | Find the number of bytes that is between the two pointers by subtracting one address
-- from another.
--
-- @since 1.0.0
minusByteCountPtr :: Ptr e1 -> Ptr e2 -> Count Word8
minusByteCountPtr (Ptr addr1#) (Ptr addr2#) = Count (I# (addr1# `minusAddr#` addr2#))
{-# INLINE minusByteCountPtr #-}

-- | Find the offset in bytes that is between the two pointers by subtracting one address
-- from another.
--
-- @since 0.1.0
minusByteOffPtr :: Ptr e1 -> Ptr e2 -> Off Word8
minusByteOffPtr (Ptr addr1#) (Ptr addr2#) = Off (I# (addr1# `minusAddr#` addr2#))
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

copyPtrToPtr :: (Primal s m, Unbox e) => Ptr e -> Off e -> Ptr e -> Off e -> Count e -> m ()
copyPtrToPtr srcPtr srcOff dstPtr dstOff c =
  unsafeIOToPrimal $
  copyBytes
    (dstPtr `plusOffPtr` dstOff)
    (srcPtr `plusOffPtr` srcOff)
    (unCountBytes c)
{-# INLINE copyPtrToPtr #-}

copyByteOffPtrToPtr ::
     (Primal s m, Unbox e)
  => Ptr e
  -> Off Word8
  -> Ptr e
  -> Off Word8
  -> Count e
  -> m ()
copyByteOffPtrToPtr srcPtr (Off srcOff) dstPtr (Off dstOff) c =
  unsafeIOToPrimal $
  copyBytes
    (dstPtr `plusPtr` dstOff)
    (srcPtr `plusPtr` srcOff)
    (unCountBytes c)
{-# INLINE copyByteOffPtrToPtr #-}

movePtrToPtr :: (Primal s m, Unbox e) => Ptr e -> Off e -> Ptr e -> Off e -> Count e -> m ()
movePtrToPtr src srcOff dst dstOff =
  moveByteOffPtrToPtr src (toByteOff srcOff) dst (toByteOff dstOff)
{-# INLINE movePtrToPtr #-}

moveByteOffPtrToPtr ::
     (Primal s m, Unbox e)
  => Ptr e
  -> Off Word8
  -> Ptr e
  -> Off Word8
  -> Count e
  -> m ()
moveByteOffPtrToPtr (Ptr srcAddr#) (Off (I# srcOff#)) (Ptr dstAddr#) (Off (I# dstOff#)) c =
  unsafeIOToPrimal $ memmoveAddr# srcAddr# srcOff# dstAddr# dstOff# (unCountBytes# c)
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
     (Primal s m, Atomic e)
  => Ptr e -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__a__@, rather than bytes.
  -> e -- ^ Expected old value
  -> e -- ^ New value
  -> m e
casOffPtr (Ptr addr#) (Off (I# i#)) old new = primal $ casOffAddr# addr# i# old new
{-# INLINE casOffPtr #-}

-- | Perform atomic modification of an element in the `Ptr` at the supplied
-- index. Returns the artifact of computation @__b__@.  Offset is in number of elements,
-- rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicModifyOffPtr ::
     (Primal s m, Atomic e)
  => Ptr e -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__a__@, rather than bytes.
  -> (e -> (e, b)) -- ^ Function that is applied to the old value and returns new value
                   -- and some artifact of computation @__b__@
  -> m b
atomicModifyOffPtr (Ptr addr#) (Off (I# i#)) f =
  primal $
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
     (Primal s m, Atomic e)
  => Ptr e -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__a__@, rather than bytes.
  -> (e -> e) -- ^ Function that is applied to the old value and returns new value.
  -> m ()
atomicModifyOffPtr_ (Ptr addr#) (Off (I# i#)) f =
  primal_ $ atomicModifyOffAddr_# addr# i# f
{-# INLINE atomicModifyOffPtr_ #-}


-- | Perform atomic modification of an element in the `Ptr` at the supplied
-- index. Returns the previous value.  Offset is in number of elements, rather than
-- bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicModifyFetchOldOffPtr ::
     (Primal s m, Atomic e)
  => Ptr e -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__a__@, rather than bytes.
  -> (e -> e) -- ^ Function that is applied to the old value and returns the new value
  -> m e
atomicModifyFetchOldOffPtr (Ptr addr#) (Off (I# i#)) f =
  primal $ atomicModifyFetchOldOffAddr# addr# i# f
{-# INLINE atomicModifyFetchOldOffPtr #-}


-- | Perform atomic modification of an element in the `Ptr` at the supplied
-- index.  Offset is in number of elements, rather than bytes. Implies a full memory
-- barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicModifyFetchNewOffPtr ::
     (Primal s m, Atomic e)
  => Ptr e -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__a__@, rather than bytes.
  -> (e -> e) -- ^ Function that is applied to the old value and returns the new value
  -> m e
atomicModifyFetchNewOffPtr (Ptr addr#) (Off (I# i#)) f =
  primal $ atomicModifyFetchNewOffAddr# addr# i# f
{-# INLINE atomicModifyFetchNewOffPtr #-}



-- | Add a numeric value to an element of a `Ptr`, corresponds to @(`+`)@ done
-- atomically. Returns the previous value.  Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicAddFetchOldOffPtr ::
     (Primal s m, AtomicCount e)
  => Ptr e
  -> Off e
  -> e
  -> m e
atomicAddFetchOldOffPtr (Ptr addr#) (Off (I# i#)) a =
  primal (atomicAddFetchOldOffAddr# addr# i# a)
{-# INLINE atomicAddFetchOldOffPtr #-}

-- | Add a numeric value to an element of a `Ptr`, corresponds to @(`+`)@ done
-- atomically. Returns the new value.  Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicAddFetchNewOffPtr ::
     (Primal s m, AtomicCount e)
  => Ptr e
  -> Off e
  -> e
  -> m e
atomicAddFetchNewOffPtr (Ptr addr#) (Off (I# i#)) a =
  primal (atomicAddFetchNewOffAddr# addr# i# a)
{-# INLINE atomicAddFetchNewOffPtr #-}



-- | Subtract a numeric value from an element of a `Ptr`, corresponds to
-- @(`-`)@ done atomically. Returns the previous value.  Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicSubFetchOldOffPtr ::
     (Primal s m, AtomicCount e)
  => Ptr e
  -> Off e
  -> e
  -> m e
atomicSubFetchOldOffPtr (Ptr addr#) (Off (I# i#)) a =
  primal (atomicSubFetchOldOffAddr# addr# i# a)
{-# INLINE atomicSubFetchOldOffPtr #-}

-- | Subtract a numeric value from an element of a `Ptr`, corresponds to
-- @(`-`)@ done atomically. Returns the new value. Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicSubFetchNewOffPtr ::
     (Primal s m, AtomicCount e)
  => Ptr e
  -> Off e
  -> e
  -> m e
atomicSubFetchNewOffPtr (Ptr addr#) (Off (I# i#)) a =
  primal (atomicSubFetchNewOffAddr# addr# i# a)
{-# INLINE atomicSubFetchNewOffPtr #-}



-- | Binary conjunction (AND) of an element of a `Ptr` with the supplied value,
-- corresponds to @(`Data.Bits..&.`)@ done atomically. Returns the previous value. Offset
-- is in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicAndFetchOldOffPtr ::
     (Primal s m, AtomicBits e)
  => Ptr e
  -> Off e
  -> e
  -> m e
atomicAndFetchOldOffPtr (Ptr addr#) (Off (I# i#)) a =
  primal (atomicAndFetchOldOffAddr# addr# i# a)
{-# INLINE atomicAndFetchOldOffPtr #-}

-- | Binary conjunction (AND) of an element of a `Ptr` with the supplied value,
-- corresponds to @(`Data.Bits..&.`)@ done atomically. Returns the new value. Offset is
-- in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicAndFetchNewOffPtr ::
     (Primal s m, AtomicBits e)
  => Ptr e
  -> Off e
  -> e
  -> m e
atomicAndFetchNewOffPtr (Ptr addr#) (Off (I# i#)) a =
  primal (atomicAndFetchNewOffAddr# addr# i# a)
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
     (Primal s m, AtomicBits e)
  => Ptr e
  -> Off e
  -> e
  -> m e
atomicNandFetchOldOffPtr (Ptr addr#) (Off (I# i#)) a =
  primal (atomicNandFetchOldOffAddr# addr# i# a)
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
     (Primal s m, AtomicBits e)
  => Ptr e
  -> Off e
  -> e
  -> m e
atomicNandFetchNewOffPtr (Ptr addr#) (Off (I# i#)) a =
  primal (atomicNandFetchNewOffAddr# addr# i# a)
{-# INLINE atomicNandFetchNewOffPtr #-}




-- | Binary disjunction (OR) of an element of a `Ptr` with the supplied value,
-- corresponds to @(`Data.Bits..|.`)@ done atomically. Returns the previous value. Offset
-- is in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicOrFetchOldOffPtr ::
     (Primal s m, AtomicBits e)
  => Ptr e
  -> Off e
  -> e
  -> m e
atomicOrFetchOldOffPtr (Ptr addr#) (Off (I# i#)) a =
  primal (atomicOrFetchOldOffAddr# addr# i# a)
{-# INLINE atomicOrFetchOldOffPtr #-}

-- | Binary disjunction (OR) of an element of a `Ptr` with the supplied value,
-- corresponds to @(`Data.Bits..|.`)@ done atomically. Returns the new value. Offset is
-- in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicOrFetchNewOffPtr ::
     (Primal s m, AtomicBits e)
  => Ptr e
  -> Off e
  -> e
  -> m e
atomicOrFetchNewOffPtr (Ptr addr#) (Off (I# i#)) a =
  primal (atomicOrFetchNewOffAddr# addr# i# a)
{-# INLINE atomicOrFetchNewOffPtr #-}



-- | Binary exclusive disjunction (XOR) of an element of a `Ptr` with the supplied value,
-- corresponds to @`Data.Bits.xor`@ done atomically. Returns the previous value. Offset
-- is in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicXorFetchOldOffPtr ::
     (Primal s m, AtomicBits e)
  => Ptr e
  -> Off e
  -> e
  -> m e
atomicXorFetchOldOffPtr (Ptr addr#) (Off (I# i#)) a =
  primal (atomicXorFetchOldOffAddr# addr# i# a)
{-# INLINE atomicXorFetchOldOffPtr #-}

-- | Binary exclusive disjunction (XOR) of an element of a `Ptr` with the supplied value,
-- corresponds to @`Data.Bits.xor`@ done atomically. Returns the new value. Offset is
-- in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicXorFetchNewOffPtr ::
     (Primal s m, AtomicBits e)
  => Ptr e
  -> Off e
  -> e
  -> m e
atomicXorFetchNewOffPtr (Ptr addr#) (Off (I# i#)) a =
  primal (atomicXorFetchNewOffAddr# addr# i# a)
{-# INLINE atomicXorFetchNewOffPtr #-}





-- | Binary negation (NOT) of an element of a `Ptr`, corresponds to
-- @(`Data.Bits.complement`)@ done atomically. Returns the previous value. Offset is in
-- number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicNotFetchOldOffPtr ::
     (Primal s m, AtomicBits e)
  => Ptr e
  -> Off e
  -> m e
atomicNotFetchOldOffPtr (Ptr addr#) (Off (I# i#)) =
  primal (atomicNotFetchOldOffAddr# addr# i#)
{-# INLINE atomicNotFetchOldOffPtr #-}

-- | Binary negation (NOT) of an element of a `Ptr`, corresponds to
-- @(`Data.Bits.complement`)@ done atomically. Returns the new value. Offset is in number
-- of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicNotFetchNewOffPtr ::
     (Primal s m, AtomicBits e)
  => Ptr e
  -> Off e
  -> m e
atomicNotFetchNewOffPtr (Ptr addr#) (Off (I# i#)) =
  primal (atomicNotFetchNewOffAddr# addr# i#)
{-# INLINE atomicNotFetchNewOffPtr #-}





prefetchPtr0 :: Primal s m => Ptr e -> m ()
prefetchPtr0 (Ptr b#) = primal_ (prefetchAddr0# b# 0#)
{-# INLINE prefetchPtr0 #-}

prefetchPtr1 :: Primal s m => Ptr a -> m ()
prefetchPtr1 (Ptr b#) = primal_ (prefetchAddr1# b# 0#)
{-# INLINE prefetchPtr1 #-}

prefetchPtr2 :: Primal s m => Ptr e -> m ()
prefetchPtr2 (Ptr b#) = primal_ (prefetchAddr2# b# 0#)
{-# INLINE prefetchPtr2 #-}

prefetchPtr3 :: Primal s m => Ptr e -> m ()
prefetchPtr3 (Ptr b#) = primal_ (prefetchAddr3# b# 0#)
{-# INLINE prefetchPtr3 #-}

prefetchOffPtr0 :: (Primal s m, Unbox e) => Ptr e -> Off e -> m ()
prefetchOffPtr0 (Ptr b#) off = primal_ (prefetchAddr0# b# (unOffBytes# off))
{-# INLINE prefetchOffPtr0 #-}

prefetchOffPtr1 :: (Primal s m, Unbox e) => Ptr e -> Off e -> m ()
prefetchOffPtr1 (Ptr b#) off = primal_ (prefetchAddr1# b# (unOffBytes# off))
{-# INLINE prefetchOffPtr1 #-}

prefetchOffPtr2 :: (Primal s m, Unbox e) => Ptr e -> Off e -> m ()
prefetchOffPtr2 (Ptr b#) off = primal_ (prefetchAddr2# b# (unOffBytes# off))
{-# INLINE prefetchOffPtr2 #-}

prefetchOffPtr3 :: (Primal s m, Unbox e) => Ptr e -> Off e -> m ()
prefetchOffPtr3 (Ptr b#) off = primal_ (prefetchAddr3# b# (unOffBytes# off))
{-# INLINE prefetchOffPtr3 #-}

-- | Same as `GHC.freeHaskellFunPtr`
--
-- @since 0.1.0
freeHaskellFunPtr :: Primal s m => FunPtr a -> m ()
freeHaskellFunPtr = unsafeIOToPrimal . GHC.freeHaskellFunPtr
