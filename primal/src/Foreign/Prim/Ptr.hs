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
  , readPtr
  , readOffPtr
  , readByteOffPtr
  , writePtr
  , writeOffPtr
  , writeByteOffPtr
  , setOffPtr
  , copyPtrToPtr
  , movePtrToPtr
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
     (MonadPrim s m, Prim a)
  => Ptr a -- ^ Chunk of memory to fill
  -> Off a -- ^ Offset in number of elements
  -> Count a -- ^ Number of cells to fill
  -> a -- ^ A value to fill the cells with
  -> m ()
setOffPtr (Ptr addr#) (Off (I# o#)) (Count (I# n#)) a = prim_ (setOffAddr# addr# o# n# a)
{-# INLINE setOffPtr #-}


readOffPtr :: (MonadPrim s m, Prim a) => Ptr a -> Off a -> m a
readOffPtr (Ptr addr#) (Off (I# i#)) = prim (readOffAddr# addr# i#)
{-# INLINE readOffPtr #-}


readByteOffPtr :: (MonadPrim s m, Prim a) => Ptr a -> Off Word8 -> m a
readByteOffPtr ptr (Off i) =
  case ptr `plusPtr` i of
    Ptr addr# -> prim (readOffAddr# addr# 0#)
{-# INLINE readByteOffPtr #-}

writeOffPtr :: (MonadPrim s m, Prim a) => Ptr a -> Off a -> a -> m ()
writeOffPtr (Ptr addr#) (Off (I# i#)) a = prim_ (writeOffAddr# addr# i# a)
{-# INLINE writeOffPtr #-}

writeByteOffPtr :: (MonadPrim s m, Prim a) => Ptr a -> Off Word8 -> a -> m ()
writeByteOffPtr ptr (Off i) a =
  case ptr `plusPtr` i of
    Ptr addr# -> prim_ (writeOffAddr# addr# 0# a)
{-# INLINE writeByteOffPtr #-}

readPtr :: (MonadPrim s m, Prim a) => Ptr a -> m a
readPtr (Ptr addr#) = prim (readOffAddr# addr# 0#)
{-# INLINE readPtr #-}

writePtr :: (MonadPrim s m, Prim a) => Ptr a -> a -> m ()
writePtr (Ptr addr#) a = prim_ (writeOffAddr# addr# 0# a)
{-# INLINE writePtr #-}

plusOffPtr :: Prim a => Ptr a -> Off a -> Ptr a
plusOffPtr (Ptr addr#) off = Ptr (addr# `plusAddr#` fromOff# off)
{-# INLINE plusOffPtr #-}

copyPtrToPtr :: (MonadPrim s m, Prim a) => Ptr a -> Off a -> Ptr a -> Off a -> Count a -> m ()
copyPtrToPtr srcPtr srcOff dstPtr dstOff c =
  unsafeIOToPrim $
  copyBytes
    (dstPtr `plusOffPtr` dstOff)
    (srcPtr `plusOffPtr` srcOff)
    (fromCount c)
{-# INLINE copyPtrToPtr #-}

movePtrToPtr :: (MonadPrim s m, Prim a) => Ptr a -> Off a -> Ptr a -> Off a -> Count a -> m ()
movePtrToPtr (Ptr srcAddr#) srcOff (Ptr dstAddr#) dstOff c =
  unsafeIOToPrim $
  memmoveAddr#
    srcAddr#
    (fromOff# srcOff)
    dstAddr#
    (fromOff# dstOff)
    (fromCount# c)
{-# INLINE movePtrToPtr #-}




-- | Perform atomic modification of an element in the `Ptr` at the supplied
-- index. Returns the artifact of computation @__b__@.  Offset is in number of elements,
-- rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
casOffPtr ::
     (MonadPrim s m, Atomic a)
  => Ptr a -- ^ Array to be mutated
  -> Off a -- ^ Index is in elements of @__a__@, rather than bytes.
  -> a -- ^ Expected old value
  -> a -- ^ New value
  -> m a
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
     (MonadPrim s m, Atomic a)
  => Ptr a -- ^ Array to be mutated
  -> Off a -- ^ Index is in elements of @__a__@, rather than bytes.
  -> (a -> (a, b)) -- ^ Function that is applied to the old value and returns new value
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
     (MonadPrim s m, Atomic a)
  => Ptr a -- ^ Array to be mutated
  -> Off a -- ^ Index is in elements of @__a__@, rather than bytes.
  -> (a -> a) -- ^ Function that is applied to the old value and returns new value.
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
     (MonadPrim s m, Atomic a)
  => Ptr a -- ^ Array to be mutated
  -> Off a -- ^ Index is in elements of @__a__@, rather than bytes.
  -> (a -> a) -- ^ Function that is applied to the old value and returns the new value
  -> m a
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
     (MonadPrim s m, Atomic a)
  => Ptr a -- ^ Array to be mutated
  -> Off a -- ^ Index is in elements of @__a__@, rather than bytes.
  -> (a -> a) -- ^ Function that is applied to the old value and returns the new value
  -> m a
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
     (MonadPrim s m, AtomicCount a)
  => Ptr a
  -> Off a
  -> a
  -> m a
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
     (MonadPrim s m, AtomicCount a)
  => Ptr a
  -> Off a
  -> a
  -> m a
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
     (MonadPrim s m, AtomicCount a)
  => Ptr a
  -> Off a
  -> a
  -> m a
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
     (MonadPrim s m, AtomicCount a)
  => Ptr a
  -> Off a
  -> a
  -> m a
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
     (MonadPrim s m, AtomicBits a)
  => Ptr a
  -> Off a
  -> a
  -> m a
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
     (MonadPrim s m, AtomicBits a)
  => Ptr a
  -> Off a
  -> a
  -> m a
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
     (MonadPrim s m, AtomicBits a)
  => Ptr a
  -> Off a
  -> a
  -> m a
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
     (MonadPrim s m, AtomicBits a)
  => Ptr a
  -> Off a
  -> a
  -> m a
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
     (MonadPrim s m, AtomicBits a)
  => Ptr a
  -> Off a
  -> a
  -> m a
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
     (MonadPrim s m, AtomicBits a)
  => Ptr a
  -> Off a
  -> a
  -> m a
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
     (MonadPrim s m, AtomicBits a)
  => Ptr a
  -> Off a
  -> a
  -> m a
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
     (MonadPrim s m, AtomicBits a)
  => Ptr a
  -> Off a
  -> a
  -> m a
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
     (MonadPrim s m, AtomicBits a)
  => Ptr a
  -> Off a
  -> m a
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
     (MonadPrim s m, AtomicBits a)
  => Ptr a
  -> Off a
  -> m a
atomicNotFetchOffPtr (Ptr addr#) (Off (I# i#)) =
  prim (atomicNotFetchOffAddr# addr# i#)
{-# INLINE atomicNotFetchOffPtr #-}





prefetchPtr0 :: MonadPrim s m => Ptr a -> m ()
prefetchPtr0 (Ptr b#) = prim_ (prefetchAddr0# b# 0#)
{-# INLINE prefetchPtr0 #-}

prefetchPtr1 :: MonadPrim s m => Ptr a -> m ()
prefetchPtr1 (Ptr b#) = prim_ (prefetchAddr1# b# 0#)
{-# INLINE prefetchPtr1 #-}

prefetchPtr2 :: MonadPrim s m => Ptr a -> m ()
prefetchPtr2 (Ptr b#) = prim_ (prefetchAddr2# b# 0#)
{-# INLINE prefetchPtr2 #-}

prefetchPtr3 :: MonadPrim s m => Ptr a -> m ()
prefetchPtr3 (Ptr b#) = prim_ (prefetchAddr3# b# 0#)
{-# INLINE prefetchPtr3 #-}

prefetchOffPtr0 :: (MonadPrim s m, Prim a) => Ptr a -> Off a -> m ()
prefetchOffPtr0 (Ptr b#) off = prim_ (prefetchAddr0# b# (fromOff# off))
{-# INLINE prefetchOffPtr0 #-}

prefetchOffPtr1 :: (MonadPrim s m, Prim a) => Ptr a -> Off a -> m ()
prefetchOffPtr1 (Ptr b#) off = prim_ (prefetchAddr1# b# (fromOff# off))
{-# INLINE prefetchOffPtr1 #-}

prefetchOffPtr2 :: (MonadPrim s m, Prim a) => Ptr a -> Off a -> m ()
prefetchOffPtr2 (Ptr b#) off = prim_ (prefetchAddr2# b# (fromOff# off))
{-# INLINE prefetchOffPtr2 #-}

prefetchOffPtr3 :: (MonadPrim s m, Prim a) => Ptr a -> Off a -> m ()
prefetchOffPtr3 (Ptr b#) off = prim_ (prefetchAddr3# b# (fromOff# off))
{-# INLINE prefetchOffPtr3 #-}

-- | Same as `GHC.freeHaskellFunPtr`
--
-- @since 0.1.0
freeHaskellFunPtr :: MonadPrim s m => FunPtr a -> m ()
freeHaskellFunPtr = unsafeIOToPrim . GHC.freeHaskellFunPtr
