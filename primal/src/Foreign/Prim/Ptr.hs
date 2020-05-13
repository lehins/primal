{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Foreign.Prim.Ptr
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Foreign.Prim.Ptr
  ( module GHC.Ptr
  , plusOffPtr
  , plusByteOffPtr
  , minusByteCountPtr
  , minusCountPtr
  , minusCountRemPtr
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
  , atomicFetchModifyOffPtr
  , atomicModifyFetchOffPtr
  -- ** Numeric
  , atomicFetchAddOffPtr
  , atomicAddFetchOffPtr
  , atomicFetchSubOffPtr
  , atomicSubFetchOffPtr
  -- ** Binary
  , atomicFetchAndOffPtr
  , atomicAndFetchOffPtr
  , atomicFetchNandOffPtr
  , atomicNandFetchOffPtr
  , atomicFetchOrOffPtr
  , atomicOrFetchOffPtr
  , atomicFetchXorOffPtr
  , atomicXorFetchOffPtr
  , atomicFetchNotOffPtr
  , atomicNotFetchOffPtr
  -- * Prefetch
  , prefetchPtr0
  , prefetchPtr1
  , prefetchPtr2
  , prefetchPtr3
  , prefetchOffPtr0
  , prefetchOffPtr1
  , prefetchOffPtr2
  , prefetchOffPtr3
  ) where

import Control.Prim.Monad
import Control.Prim.Monad.Unsafe
import Data.Prim
import Data.Prim.Atomic
import Data.Prim.Class
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Prim
import qualified Foreign.Ptr as GHC (freeHaskellFunPtr)
import Foreign.Ptr as X hiding (IntPtr, WordPtr, freeHaskellFunPtr, intPtrToPtr,
                         ptrToIntPtr, ptrToWordPtr, wordPtrToPtr)
import GHC.Ptr

setOffPtr ::
     (MonadPrim s m, Prim e)
  => Ptr e -- ^ Chunk of memory to fill
  -> Off e -- ^ Offset in number of elements
  -> Count e -- ^ Number of cells to fill
  -> e -- ^ A value to fill the cells with
  -> m ()
setOffPtr (Ptr addr#) (Off (I# o#)) (Count (I# n#)) a = prim_ (setOffAddr# addr# o# n# a)
{-# INLINE setOffPtr #-}


readOffPtr :: (MonadPrim s m, Prim e) => Ptr e -> Off e -> m e
readOffPtr (Ptr addr#) (Off (I# i#)) = prim (readOffAddr# addr# i#)
{-# INLINE readOffPtr #-}


readByteOffPtr :: (MonadPrim s m, Prim e) => Ptr e -> Off Word8 -> m e
readByteOffPtr ptr (Off i) =
  case ptr `plusPtr` i of
    Ptr addr# -> prim (readOffAddr# addr# 0#)
{-# INLINE readByteOffPtr #-}

writeOffPtr :: (MonadPrim s m, Prim e) => Ptr e -> Off e -> e -> m ()
writeOffPtr (Ptr addr#) (Off (I# i#)) a = prim_ (writeOffAddr# addr# i# a)
{-# INLINE writeOffPtr #-}

writeByteOffPtr :: (MonadPrim s m, Prim e) => Ptr e -> Off Word8 -> e -> m ()
writeByteOffPtr ptr (Off i) a =
  case ptr `plusPtr` i of
    Ptr addr# -> prim_ (writeOffAddr# addr# 0# a)
{-# INLINE writeByteOffPtr #-}

readPtr :: (MonadPrim s m, Prim e) => Ptr e -> m e
readPtr (Ptr addr#) = prim (readOffAddr# addr# 0#)
{-# INLINE readPtr #-}

writePtr :: (MonadPrim s m, Prim e) => Ptr e -> e -> m ()
writePtr (Ptr addr#) a = prim_ (writeOffAddr# addr# 0# a)
{-# INLINE writePtr #-}

-- | Count how many bytes is between the two pointers by subtracting the addresses.
minusByteCountPtr :: Prim e => Ptr e -> Ptr e -> Count Word8
minusByteCountPtr (Ptr xaddr#) (Ptr yaddr#) = Count (I# (xaddr# `minusAddr#` yaddr#))
{-# INLINE minusByteCountPtr #-}

plusByteOffPtr :: Prim e => Ptr e -> Off Word8 -> Ptr e
plusByteOffPtr (Ptr addr#) (Off (I# off#)) = Ptr (addr# `plusAddr#` off#)
{-# INLINE plusByteOffPtr #-}


plusOffPtr :: Prim e => Ptr e -> Off e -> Ptr e
plusOffPtr (Ptr addr#) off = Ptr (addr# `plusAddr#` fromOff# off)
{-# INLINE plusOffPtr #-}

-- | Count how many elements of type @a@ canb fit between the two addresses
minusCountPtr :: Prim e => Ptr e -> Ptr e -> Count e
minusCountPtr (Ptr xaddr#) (Ptr yaddr#) =
  fromByteCount (Count (I# (xaddr# `minusAddr#` yaddr#)))
{-# INLINE minusCountPtr #-}

-- | Same as `minusCountPtr`, but will also return the slack that is left over
minusCountRemPtr :: Prim e => Ptr e -> Ptr e -> (Count e, Int)
minusCountRemPtr (Ptr xaddr#) (Ptr yaddr#) =
  fromByteCountRem (Count (I# (xaddr# `minusAddr#` yaddr#)))
{-# INLINE minusCountRemPtr #-}

copyPtrToPtr :: (MonadPrim s m, Prim e) => Ptr e -> Off e -> Ptr e -> Off e -> Count e -> m ()
copyPtrToPtr srcPtr srcOff dstPtr dstOff c =
  unsafeIOToPrim $
  copyBytes
    (dstPtr `plusOffPtr` dstOff)
    (srcPtr `plusOffPtr` srcOff)
    (fromCount c)
{-# INLINE copyPtrToPtr #-}

copyByteOffPtrToPtr ::
     (MonadPrim s m, Prim e)
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
    (fromCount c)
{-# INLINE copyByteOffPtrToPtr #-}

movePtrToPtr :: (MonadPrim s m, Prim e) => Ptr e -> Off e -> Ptr e -> Off e -> Count e -> m ()
movePtrToPtr src srcOff dst dstOff =
  moveByteOffPtrToPtr src (toByteOff srcOff) dst (toByteOff dstOff)
{-# INLINE movePtrToPtr #-}

moveByteOffPtrToPtr ::
     (MonadPrim s m, Prim e)
  => Ptr e
  -> Off Word8
  -> Ptr e
  -> Off Word8
  -> Count e
  -> m ()
moveByteOffPtrToPtr (Ptr srcAddr#) (Off (I# srcOff#)) (Ptr dstAddr#) (Off (I# dstOff#)) c =
  unsafeIOToPrim $ memmoveAddr# srcAddr# srcOff# dstAddr# dstOff# (fromCount# c)
{-# INLINE moveByteOffPtrToPtr #-}

-- | Compare memory between two pointers. Offsets and count is in number of elements,
-- instead of byte count. Use `compareByteOffPtrToPtr` when offset in bytes is required.
comparePtrToPtr :: Prim e => Ptr e -> Off e -> Ptr e -> Off e -> Count e -> Ordering
comparePtrToPtr (Ptr addr1#) off1 (Ptr addr2#) off2 c =
  toOrdering# (memcmpAddr# addr1# (fromOff# off1) addr2# (fromOff# off2) (fromCount# c))
{-# INLINE comparePtrToPtr #-}

-- | Same as `comparePtrToPtr`, except offset is in bytes instead of number of elements.
compareByteOffPtrToPtr ::
     Prim e => Ptr e -> Off Word8 -> Ptr e -> Off Word8 -> Count e -> Ordering
compareByteOffPtrToPtr (Ptr addr1#) (Off (I# off1#)) (Ptr addr2#) (Off (I# off2#)) c =
  toOrdering# (memcmpAddr# addr1# off1# addr2# off2# (fromCount# c))
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
atomicFetchModifyOffPtr ::
     (MonadPrim s m, Atomic e)
  => Ptr e -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__a__@, rather than bytes.
  -> (e -> e) -- ^ Function that is applied to the old value and returns the new value
  -> m e
atomicFetchModifyOffPtr (Ptr addr#) (Off (I# i#)) f =
  prim $ atomicFetchModifyOffAddr# addr# i# f
{-# INLINE atomicFetchModifyOffPtr #-}


-- | Perform atomic modification of an element in the `Ptr` at the supplied
-- index.  Offset is in number of elements, rather than bytes. Implies a full memory
-- barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicModifyFetchOffPtr ::
     (MonadPrim s m, Atomic e)
  => Ptr e -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__a__@, rather than bytes.
  -> (e -> e) -- ^ Function that is applied to the old value and returns the new value
  -> m e
atomicModifyFetchOffPtr (Ptr addr#) (Off (I# i#)) f =
  prim $ atomicModifyFetchOffAddr# addr# i# f
{-# INLINE atomicModifyFetchOffPtr #-}



-- | Add a numeric value to an element of a `Ptr`, corresponds to @(`+`)@ done
-- atomically. Returns the previous value.  Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicFetchAddOffPtr ::
     (MonadPrim s m, AtomicCount e)
  => Ptr e
  -> Off e
  -> e
  -> m e
atomicFetchAddOffPtr (Ptr addr#) (Off (I# i#)) a =
  prim (atomicFetchAddOffAddr# addr# i# a)
{-# INLINE atomicFetchAddOffPtr #-}

-- | Add a numeric value to an element of a `Ptr`, corresponds to @(`+`)@ done
-- atomically. Returns the new value.  Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicAddFetchOffPtr ::
     (MonadPrim s m, AtomicCount e)
  => Ptr e
  -> Off e
  -> e
  -> m e
atomicAddFetchOffPtr (Ptr addr#) (Off (I# i#)) a =
  prim (atomicAddFetchOffAddr# addr# i# a)
{-# INLINE atomicAddFetchOffPtr #-}



-- | Subtract a numeric value from an element of a `Ptr`, corresponds to
-- @(`-`)@ done atomically. Returns the previous value.  Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicFetchSubOffPtr ::
     (MonadPrim s m, AtomicCount e)
  => Ptr e
  -> Off e
  -> e
  -> m e
atomicFetchSubOffPtr (Ptr addr#) (Off (I# i#)) a =
  prim (atomicFetchSubOffAddr# addr# i# a)
{-# INLINE atomicFetchSubOffPtr #-}

-- | Subtract a numeric value from an element of a `Ptr`, corresponds to
-- @(`-`)@ done atomically. Returns the new value. Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicSubFetchOffPtr ::
     (MonadPrim s m, AtomicCount e)
  => Ptr e
  -> Off e
  -> e
  -> m e
atomicSubFetchOffPtr (Ptr addr#) (Off (I# i#)) a =
  prim (atomicSubFetchOffAddr# addr# i# a)
{-# INLINE atomicSubFetchOffPtr #-}



-- | Binary conjunction (AND) of an element of a `Ptr` with the supplied value,
-- corresponds to @(`Data.Bits..&.`)@ done atomically. Returns the previous value. Offset
-- is in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicFetchAndOffPtr ::
     (MonadPrim s m, AtomicBits e)
  => Ptr e
  -> Off e
  -> e
  -> m e
atomicFetchAndOffPtr (Ptr addr#) (Off (I# i#)) a =
  prim (atomicFetchAndOffAddr# addr# i# a)
{-# INLINE atomicFetchAndOffPtr #-}

-- | Binary conjunction (AND) of an element of a `Ptr` with the supplied value,
-- corresponds to @(`Data.Bits..&.`)@ done atomically. Returns the new value. Offset is
-- in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicAndFetchOffPtr ::
     (MonadPrim s m, AtomicBits e)
  => Ptr e
  -> Off e
  -> e
  -> m e
atomicAndFetchOffPtr (Ptr addr#) (Off (I# i#)) a =
  prim (atomicAndFetchOffAddr# addr# i# a)
{-# INLINE atomicAndFetchOffPtr #-}



-- | Negation of binary conjunction (NAND) of an element of a `Ptr` with the
-- supplied value, corresponds to @\\x y -> `Data.Bits.complement` (x `Data.Bits..&.` y)@
-- done atomically. Returns the previous value. Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicFetchNandOffPtr ::
     (MonadPrim s m, AtomicBits e)
  => Ptr e
  -> Off e
  -> e
  -> m e
atomicFetchNandOffPtr (Ptr addr#) (Off (I# i#)) a =
  prim (atomicFetchNandOffAddr# addr# i# a)
{-# INLINE atomicFetchNandOffPtr #-}

-- | Negation of binary conjunction (NAND)  of an element of a `Ptr` with the supplied
-- value, corresponds to @\\x y -> `Data.Bits.complement` (x `Data.Bits..&.` y)@ done
-- atomically. Returns the new value. Offset is in number of elements, rather than
-- bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicNandFetchOffPtr ::
     (MonadPrim s m, AtomicBits e)
  => Ptr e
  -> Off e
  -> e
  -> m e
atomicNandFetchOffPtr (Ptr addr#) (Off (I# i#)) a =
  prim (atomicNandFetchOffAddr# addr# i# a)
{-# INLINE atomicNandFetchOffPtr #-}




-- | Binary disjunction (OR) of an element of a `Ptr` with the supplied value,
-- corresponds to @(`Data.Bits..|.`)@ done atomically. Returns the previous value. Offset
-- is in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicFetchOrOffPtr ::
     (MonadPrim s m, AtomicBits e)
  => Ptr e
  -> Off e
  -> e
  -> m e
atomicFetchOrOffPtr (Ptr addr#) (Off (I# i#)) a =
  prim (atomicFetchOrOffAddr# addr# i# a)
{-# INLINE atomicFetchOrOffPtr #-}

-- | Binary disjunction (OR) of an element of a `Ptr` with the supplied value,
-- corresponds to @(`Data.Bits..|.`)@ done atomically. Returns the new value. Offset is
-- in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicOrFetchOffPtr ::
     (MonadPrim s m, AtomicBits e)
  => Ptr e
  -> Off e
  -> e
  -> m e
atomicOrFetchOffPtr (Ptr addr#) (Off (I# i#)) a =
  prim (atomicOrFetchOffAddr# addr# i# a)
{-# INLINE atomicOrFetchOffPtr #-}



-- | Binary exclusive disjunction (XOR) of an element of a `Ptr` with the supplied value,
-- corresponds to @`Data.Bits.xor`@ done atomically. Returns the previous value. Offset
-- is in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicFetchXorOffPtr ::
     (MonadPrim s m, AtomicBits e)
  => Ptr e
  -> Off e
  -> e
  -> m e
atomicFetchXorOffPtr (Ptr addr#) (Off (I# i#)) a =
  prim (atomicFetchXorOffAddr# addr# i# a)
{-# INLINE atomicFetchXorOffPtr #-}

-- | Binary exclusive disjunction (XOR) of an element of a `Ptr` with the supplied value,
-- corresponds to @`Data.Bits.xor`@ done atomically. Returns the new value. Offset is
-- in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicXorFetchOffPtr ::
     (MonadPrim s m, AtomicBits e)
  => Ptr e
  -> Off e
  -> e
  -> m e
atomicXorFetchOffPtr (Ptr addr#) (Off (I# i#)) a =
  prim (atomicXorFetchOffAddr# addr# i# a)
{-# INLINE atomicXorFetchOffPtr #-}





-- | Binary negation (NOT) of an element of a `Ptr`, corresponds to
-- @(`Data.Bits.complement`)@ done atomically. Returns the previous value. Offset is in
-- number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicFetchNotOffPtr ::
     (MonadPrim s m, AtomicBits e)
  => Ptr e
  -> Off e
  -> m e
atomicFetchNotOffPtr (Ptr addr#) (Off (I# i#)) =
  prim (atomicFetchNotOffAddr# addr# i#)
{-# INLINE atomicFetchNotOffPtr #-}

-- | Binary negation (NOT) of an element of a `Ptr`, corresponds to
-- @(`Data.Bits.complement`)@ done atomically. Returns the new value. Offset is in number
-- of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicNotFetchOffPtr ::
     (MonadPrim s m, AtomicBits e)
  => Ptr e
  -> Off e
  -> m e
atomicNotFetchOffPtr (Ptr addr#) (Off (I# i#)) =
  prim (atomicNotFetchOffAddr# addr# i#)
{-# INLINE atomicNotFetchOffPtr #-}





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

prefetchOffPtr0 :: (MonadPrim s m, Prim e) => Ptr e -> Off e -> m ()
prefetchOffPtr0 (Ptr b#) off = prim_ (prefetchAddr0# b# (fromOff# off))
{-# INLINE prefetchOffPtr0 #-}

prefetchOffPtr1 :: (MonadPrim s m, Prim e) => Ptr e -> Off e -> m ()
prefetchOffPtr1 (Ptr b#) off = prim_ (prefetchAddr1# b# (fromOff# off))
{-# INLINE prefetchOffPtr1 #-}

prefetchOffPtr2 :: (MonadPrim s m, Prim e) => Ptr e -> Off e -> m ()
prefetchOffPtr2 (Ptr b#) off = prim_ (prefetchAddr2# b# (fromOff# off))
{-# INLINE prefetchOffPtr2 #-}

prefetchOffPtr3 :: (MonadPrim s m, Prim e) => Ptr e -> Off e -> m ()
prefetchOffPtr3 (Ptr b#) off = prim_ (prefetchAddr3# b# (fromOff# off))
{-# INLINE prefetchOffPtr3 #-}

-- | Same as `GHC.freeHaskellFunPtr`
--
-- @since 0.1.0
freeHaskellFunPtr :: MonadPrim s m => FunPtr a -> m ()
freeHaskellFunPtr = unsafeIOToPrim . GHC.freeHaskellFunPtr
