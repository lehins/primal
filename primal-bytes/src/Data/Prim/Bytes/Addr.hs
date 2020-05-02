{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Data.Prim.Bytes
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Bytes.Addr
  ( -- * Immutable Addr
    Addr(..)
  , castAddr
  , toAddr
  , curOffAddr
  , countOfAddr
  , plusOffAddr
  , indexAddr
  , indexOffAddr
  , indexByteOffAddr
  , readAddr
  , readOffAddr
  , readByteOffAddr
  , thawAddr
  , freezeMAddr
  , withPtrAddr
  , withAddrAddr#
  , withNoHaltPtrAddr

   -- * Mutable MAddr
  , MAddr(..)
  , castMAddr
  , allocMAddr
  , callocMAddr
  , curOffMAddr
  , getCountOfMAddr
  , plusOffMAddr
  , readMAddr
  , readOffMAddr
  , readByteOffMAddr
  , writeMAddr
  , writeOffMAddr
  , writeByteOffMAddr

  , withPtrMAddr
  , withAddrMAddr#
  , withNoHaltPtrMAddr
  -- * Atomic
  , casOffMAddr
  , atomicModifyOffMAddr
  , atomicModifyOffMAddr_
  , atomicFetchModifyOffMAddr
  , atomicModifyFetchOffMAddr
  -- ** Numeric
  , atomicFetchAddOffMAddr
  , atomicAddFetchOffMAddr
  , atomicFetchSubOffMAddr
  , atomicSubFetchOffMAddr
  -- ** Binary
  , atomicFetchAndOffMAddr
  , atomicAndFetchOffMAddr
  , atomicFetchNandOffMAddr
  , atomicNandFetchOffMAddr
  , atomicFetchOrOffMAddr
  , atomicOrFetchOffMAddr
  , atomicFetchXorOffMAddr
  , atomicXorFetchOffMAddr
  , atomicFetchNotOffMAddr
  , atomicNotFetchOffMAddr
  -- * Prefetch
  -- ** Directly
  , prefetchAddr0
  , prefetchMAddr0
  , prefetchAddr1
  , prefetchMAddr1
  , prefetchAddr2
  , prefetchMAddr2
  , prefetchAddr3
  , prefetchMAddr3
  -- ** With offset
  , prefetchOffAddr0
  , prefetchOffMAddr0
  , prefetchOffAddr1
  , prefetchOffMAddr1
  , prefetchOffAddr2
  , prefetchOffMAddr2
  , prefetchOffAddr3
  , prefetchOffMAddr3

  ) where

import Control.DeepSeq
import Control.Prim.Monad
import Control.Prim.Monad.Unsafe
import Data.Prim
import Data.Prim.Atomic
import Data.Prim.Bytes
import Data.Prim.Class
import Foreign.Prim
import Foreign.Ptr


data Addr a = Addr
  { addrAddr# :: Addr#
  , addrBytes :: {-# UNPACK #-}!(Bytes 'Pin)
  }

data MAddr a s = MAddr
  { mAddrAddr#  :: Addr#
  , mAddrMBytes :: {-# UNPACK #-}!(MBytes 'Pin s)
  }

instance Eq (Addr a) where
  a1 == a2 =
    isSameAddr a1 a2 || (c1' == c2' && memcmpAddr a1' 0 a2' 0 c1' == EQ)
    where
      a1' = castAddr a1 :: Addr Word8
      a2' = castAddr a2 :: Addr Word8
      c1' = countOfAddr a1'
      c2' = countOfAddr a2'

castAddr :: Addr a -> Addr b
castAddr = coerce

castMAddr :: MAddr a s -> MAddr b s
castMAddr = coerce

memcmpAddr :: Prim a => Addr a -> Off a -> Addr a -> Off a -> Count a -> Ordering
memcmpAddr a1 o1 a2 o2 c = unsafeInlineIO $
  withAddrAddr# a1 $ \ addr1# ->
    withAddrAddr# a2 $ \ addr2# ->
      pure $ toOrdering# (memcmpAddr# addr1# (fromOff# o1) addr2# (fromOff# o2) (fromCount# c))

isSameAddr :: Addr a -> Addr a -> Bool
isSameAddr (Addr a1# _) (Addr a2# _) = isTrue# (a1# `eqAddr#` a2#)

instance NFData (Addr a) where
  rnf (Addr _ _) = ()

instance NFData (MAddr a s) where
  rnf (MAddr _ _) = ()

toAddr :: Bytes 'Pin -> Addr a
toAddr b@(Bytes b#) = Addr (byteArrayContents# b#) b

toMAddr :: MBytes 'Pin s -> MAddr a s
toMAddr mb =
  case toPtrMBytes mb of
    Ptr addr# -> MAddr addr# mb

allocMAddr :: (MonadPrim s m, Prim a) => Count a -> m (MAddr a s)
allocMAddr c = toMAddr <$> allocAlignedMBytes c

callocMAddr :: (MonadPrim s m, Prim a) => Count a -> m (MAddr a s)
callocMAddr c = toMAddr <$> callocAlignedMBytes c

plusOffAddr :: Prim a => Addr a -> Off a -> Addr a
plusOffAddr (Addr addr# b) off = Addr (addr# `plusAddr#` fromOff# off) b

plusOffMAddr :: Prim a => MAddr a s -> Off a -> MAddr a s
plusOffMAddr (MAddr addr# mb) off = MAddr (addr# `plusAddr#` fromOff# off) mb

curOffAddr :: Prim a => Addr a -> Off a
curOffAddr (Addr addr# (Bytes b#)) =
  let count = countSize (I# (addr# `minusAddr#` byteArrayContents# b#))
  in offAsProxy count (Off (unCount count))

countOfAddr ::
     forall a. Prim a
  => Addr a
  -> Count a
countOfAddr addr@(Addr _ b) = countOfBytes b - coerce (curOffAddr addr)

getCountOfMAddr :: (MonadPrim s m, Prim a) => MAddr a s -> m (Count a)
getCountOfMAddr maddr@(MAddr _ mb) =
  subtract (coerce (curOffMAddr maddr)) <$> getCountOfMBytes mb


indexAddr :: Prim a => Addr a -> a
indexAddr addr = indexOffAddr addr 0

indexOffAddr :: Prim a => Addr a -> Off a -> a
indexOffAddr addr off = unsafeInlineIO $ readOffAddr addr off

indexByteOffAddr :: Prim a => Addr a -> Off Word8 -> a
indexByteOffAddr addr off = unsafeInlineIO $ readByteOffAddr addr off

withPtrAddr :: MonadPrim s m => Addr a -> (Ptr a -> m b) -> m b
withPtrAddr addr f = withAddrAddr# addr $ \addr# -> f (Ptr addr#)
{-# INLINE withPtrAddr #-}

withAddrAddr# :: MonadPrim s m => Addr a -> (Addr# -> m b) -> m b
withAddrAddr# (Addr addr# b) f = do
  a <- f addr#
  a <$ touch b
{-# INLINE withAddrAddr# #-}

withNoHaltPtrAddr :: MonadUnliftPrim s m => Addr a -> (Ptr a -> m b) -> m b
withNoHaltPtrAddr (Addr addr# b) f = withUnliftPrim b $ f (Ptr addr#)
{-# INLINE withNoHaltPtrAddr #-}

curOffMAddr :: Prim a => MAddr a s -> Off a
curOffMAddr (MAddr addr# mb) =
  let count = countSize (Ptr addr# `minusPtr` toPtrMBytes mb)
  in offAsProxy count (Off (unCount count))

withPtrMAddr :: MonadPrim s m => MAddr a s -> (Ptr a -> m b) -> m b
withPtrMAddr maddr f = withAddrMAddr# maddr $ \addr# -> f (Ptr addr#)
{-# INLINE withPtrMAddr #-}

withAddrMAddr# :: MonadPrim s m => MAddr a s -> (Addr# -> m b) -> m b
withAddrMAddr# (MAddr addr# mb) f = do
  a <- f addr#
  a <$ touch mb
{-# INLINE withAddrMAddr# #-}

withNoHaltPtrMAddr :: MonadUnliftPrim s m => MAddr a s -> (Ptr a -> m b) -> m b
withNoHaltPtrMAddr (MAddr addr# mb) f = withUnliftPrim mb $ f (Ptr addr#)
{-# INLINE withNoHaltPtrMAddr #-}

thawAddr :: MonadPrim s m => Addr a -> m (MAddr a s)
thawAddr (Addr addr# b) = MAddr addr# <$> thawBytes b

freezeMAddr :: MonadPrim s m => MAddr a s -> m (Addr a)
freezeMAddr (MAddr addr# mb) = Addr addr# <$> freezeMBytes mb


readAddr :: (MonadPrim s m, Prim a) => Addr a -> m a
readAddr addr = readOffAddr addr 0

readOffAddr :: (MonadPrim s m, Prim a) => Addr a -> Off a -> m a
readOffAddr (Addr addr# b) (Off (I# off#)) = do
  a <- prim (seq# (indexOffAddr# addr# off#))
  a <$ touch b

readByteOffAddr :: (MonadPrim s m, Prim a) => Addr a -> Off Word8 -> m a
readByteOffAddr (Addr addr# b) (Off (I# off#)) = do
  a <- prim (seq# (indexOffAddr# (addr# `plusAddr#` off#) 0#))
  a <$ touch b


readMAddr :: (MonadPrim s m, Prim a) => MAddr a s -> m a
readMAddr maddr = readOffMAddr maddr 0

readOffMAddr :: (MonadPrim s m, Prim a) => MAddr a s -> Off a -> m a
readOffMAddr (MAddr addr# mb) (Off (I# off#)) = do
  a <- prim (readOffAddr# addr# off#)
  a <$ touch mb

readByteOffMAddr :: (MonadPrim s m, Prim a) => MAddr a s -> Off Word8 -> m a
readByteOffMAddr (MAddr addr# mb) (Off (I# off#)) = do
  a <- prim (readOffAddr# (addr# `plusAddr#` off#) 0#)
  a <$ touch mb

writeMAddr :: (MonadPrim s m, Prim a) => MAddr a s -> a -> m ()
writeMAddr maddr = writeOffMAddr maddr 0

writeOffMAddr :: (MonadPrim s m, Prim a) => MAddr a s -> Off a -> a -> m ()
writeOffMAddr (MAddr addr# mb) (Off (I# off#)) a = do
  prim_ (writeOffAddr# addr# off# a)
  touch mb


writeByteOffMAddr :: (MonadPrim s m, Prim a) => MAddr a s -> Off Word8 -> a -> m ()
writeByteOffMAddr (MAddr addr# mb) (Off (I# off#)) a = do
  prim_ (writeOffAddr# (addr# `plusAddr#` off#) 0# a)
  touch mb




-- | Perform atomic modification of an element in the `MAddr` at the supplied
-- index. Returns the artifact of computation @__b__@.  Offset is in number of elements,
-- rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
casOffMAddr ::
     (MonadPrim s m, Atomic a)
  => MAddr a s -- ^ Array to be mutated
  -> Off a -- ^ Index is in elements of @__a__@, rather than bytes.
  -> a -- ^ Expected old value
  -> a -- ^ New value
  -> m a
casOffMAddr maddr (Off (I# i#)) old new = withAddrMAddr# maddr $ \ addr# -> prim $ casOffAddr# addr# i# old new
{-# INLINE casOffMAddr #-}

-- | Perform atomic modification of an element in the `MAddr` at the supplied
-- index. Returns the artifact of computation @__b__@.  Offset is in number of elements,
-- rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicModifyOffMAddr ::
     (MonadPrim s m, Atomic a)
  => MAddr a s -- ^ Array to be mutated
  -> Off a -- ^ Index is in elements of @__a__@, rather than bytes.
  -> (a -> (a, b)) -- ^ Function that is applied to the old value and returns new value
                   -- and some artifact of computation @__b__@
  -> m b
atomicModifyOffMAddr maddr (Off (I# i#)) f =
  withAddrMAddr# maddr $ \ addr# -> prim $
  atomicModifyOffAddr# addr# i# $ \a ->
    case f a of
      (a', b) -> (# a', b #)
{-# INLINE atomicModifyOffMAddr #-}

-- | Perform atomic modification of an element in the `MAddr` at the supplied
-- index.  Offset is in number of elements, rather than bytes. Implies a full memory
-- barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicModifyOffMAddr_ ::
     (MonadPrim s m, Atomic a)
  => MAddr a s -- ^ Array to be mutated
  -> Off a -- ^ Index is in elements of @__a__@, rather than bytes.
  -> (a -> a) -- ^ Function that is applied to the old value and returns new value.
  -> m ()
atomicModifyOffMAddr_ maddr (Off (I# i#)) f =
  withAddrMAddr# maddr $ \ addr# -> prim_ $ atomicModifyOffAddr_# addr# i# f
{-# INLINE atomicModifyOffMAddr_ #-}


-- | Perform atomic modification of an element in the `MAddr` at the supplied
-- index. Returns the previous value.  Offset is in number of elements, rather than
-- bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicFetchModifyOffMAddr ::
     (MonadPrim s m, Atomic a)
  => MAddr a s -- ^ Array to be mutated
  -> Off a -- ^ Index is in elements of @__a__@, rather than bytes.
  -> (a -> a) -- ^ Function that is applied to the old value and returns the new value
  -> m a
atomicFetchModifyOffMAddr maddr (Off (I# i#)) f =
  withAddrMAddr# maddr $ \ addr# -> prim $ atomicFetchModifyOffAddr# addr# i# f
{-# INLINE atomicFetchModifyOffMAddr #-}


-- | Perform atomic modification of an element in the `MAddr` at the supplied
-- index.  Offset is in number of elements, rather than bytes. Implies a full memory
-- barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicModifyFetchOffMAddr ::
     (MonadPrim s m, Atomic a)
  => MAddr a s -- ^ Array to be mutated
  -> Off a -- ^ Index is in elements of @__a__@, rather than bytes.
  -> (a -> a) -- ^ Function that is applied to the old value and returns the new value
  -> m a
atomicModifyFetchOffMAddr maddr (Off (I# i#)) f =
  withAddrMAddr# maddr $ \ addr# -> prim $ atomicModifyFetchOffAddr# addr# i# f
{-# INLINE atomicModifyFetchOffMAddr #-}



-- | Add a numeric value to an element of a `MAddr`, corresponds to @(`+`)@ done
-- atomically. Returns the previous value.  Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicFetchAddOffMAddr ::
     (MonadPrim s m, AtomicCount a)
  => MAddr a s
  -> Off a
  -> a
  -> m a
atomicFetchAddOffMAddr maddr (Off (I# i#)) a =
  withAddrMAddr# maddr $ \ addr# -> prim $ atomicFetchAddOffAddr# addr# i# a
{-# INLINE atomicFetchAddOffMAddr #-}

-- | Add a numeric value to an element of a `MAddr`, corresponds to @(`+`)@ done
-- atomically. Returns the new value.  Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicAddFetchOffMAddr ::
     (MonadPrim s m, AtomicCount a)
  => MAddr a s
  -> Off a
  -> a
  -> m a
atomicAddFetchOffMAddr maddr (Off (I# i#)) a =
  withAddrMAddr# maddr $ \ addr# -> prim $ atomicAddFetchOffAddr# addr# i# a
{-# INLINE atomicAddFetchOffMAddr #-}



-- | Subtract a numeric value from an element of a `MAddr`, corresponds to
-- @(`-`)@ done atomically. Returns the previous value.  Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicFetchSubOffMAddr ::
     (MonadPrim s m, AtomicCount a)
  => MAddr a s
  -> Off a
  -> a
  -> m a
atomicFetchSubOffMAddr maddr (Off (I# i#)) a =
  withAddrMAddr# maddr $ \ addr# -> prim $ atomicFetchSubOffAddr# addr# i# a
{-# INLINE atomicFetchSubOffMAddr #-}

-- | Subtract a numeric value from an element of a `MAddr`, corresponds to
-- @(`-`)@ done atomically. Returns the new value. Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicSubFetchOffMAddr ::
     (MonadPrim s m, AtomicCount a)
  => MAddr a s
  -> Off a
  -> a
  -> m a
atomicSubFetchOffMAddr maddr (Off (I# i#)) a =
  withAddrMAddr# maddr $ \ addr# -> prim $ atomicSubFetchOffAddr# addr# i# a
{-# INLINE atomicSubFetchOffMAddr #-}



-- | Binary conjunction (AND) of an element of a `MAddr` with the supplied value,
-- corresponds to @(`Data.Bits..&.`)@ done atomically. Returns the previous value. Offset
-- is in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicFetchAndOffMAddr ::
     (MonadPrim s m, AtomicBits a)
  => MAddr a s
  -> Off a
  -> a
  -> m a
atomicFetchAndOffMAddr maddr (Off (I# i#)) a =
  withAddrMAddr# maddr $ \ addr# -> prim $ atomicFetchAndOffAddr# addr# i# a
{-# INLINE atomicFetchAndOffMAddr #-}

-- | Binary conjunction (AND) of an element of a `MAddr` with the supplied value,
-- corresponds to @(`Data.Bits..&.`)@ done atomically. Returns the new value. Offset is
-- in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicAndFetchOffMAddr ::
     (MonadPrim s m, AtomicBits a)
  => MAddr a s
  -> Off a
  -> a
  -> m a
atomicAndFetchOffMAddr maddr (Off (I# i#)) a =
  withAddrMAddr# maddr $ \ addr# -> prim $ atomicAndFetchOffAddr# addr# i# a
{-# INLINE atomicAndFetchOffMAddr #-}



-- | Negation of binary conjunction (NAND) of an element of a `MAddr` with the
-- supplied value, corresponds to @\\x y -> `Data.Bits.complement` (x `Data.Bits..&.` y)@
-- done atomically. Returns the previous value. Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicFetchNandOffMAddr ::
     (MonadPrim s m, AtomicBits a)
  => MAddr a s
  -> Off a
  -> a
  -> m a
atomicFetchNandOffMAddr maddr (Off (I# i#)) a =
  withAddrMAddr# maddr $ \ addr# -> prim $ atomicFetchNandOffAddr# addr# i# a
{-# INLINE atomicFetchNandOffMAddr #-}

-- | Negation of binary conjunction (NAND)  of an element of a `MAddr` with the supplied
-- value, corresponds to @\\x y -> `Data.Bits.complement` (x `Data.Bits..&.` y)@ done
-- atomically. Returns the new value. Offset is in number of elements, rather than
-- bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicNandFetchOffMAddr ::
     (MonadPrim s m, AtomicBits a)
  => MAddr a s
  -> Off a
  -> a
  -> m a
atomicNandFetchOffMAddr maddr (Off (I# i#)) a =
  withAddrMAddr# maddr $ \ addr# -> prim $ atomicNandFetchOffAddr# addr# i# a
{-# INLINE atomicNandFetchOffMAddr #-}




-- | Binary disjunction (OR) of an element of a `MAddr` with the supplied value,
-- corresponds to @(`Data.Bits..|.`)@ done atomically. Returns the previous value. Offset
-- is in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicFetchOrOffMAddr ::
     (MonadPrim s m, AtomicBits a)
  => MAddr a s
  -> Off a
  -> a
  -> m a
atomicFetchOrOffMAddr maddr (Off (I# i#)) a =
  withAddrMAddr# maddr $ \ addr# -> prim $ atomicFetchOrOffAddr# addr# i# a
{-# INLINE atomicFetchOrOffMAddr #-}

-- | Binary disjunction (OR) of an element of a `MAddr` with the supplied value,
-- corresponds to @(`Data.Bits..|.`)@ done atomically. Returns the new value. Offset is
-- in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicOrFetchOffMAddr ::
     (MonadPrim s m, AtomicBits a)
  => MAddr a s
  -> Off a
  -> a
  -> m a
atomicOrFetchOffMAddr maddr (Off (I# i#)) a =
  withAddrMAddr# maddr $ \ addr# -> prim $ atomicOrFetchOffAddr# addr# i# a
{-# INLINE atomicOrFetchOffMAddr #-}



-- | Binary exclusive disjunction (XOR) of an element of a `MAddr` with the supplied value,
-- corresponds to @`Data.Bits.xor`@ done atomically. Returns the previous value. Offset
-- is in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicFetchXorOffMAddr ::
     (MonadPrim s m, AtomicBits a)
  => MAddr a s
  -> Off a
  -> a
  -> m a
atomicFetchXorOffMAddr maddr (Off (I# i#)) a =
  withAddrMAddr# maddr $ \ addr# -> prim $ atomicFetchXorOffAddr# addr# i# a
{-# INLINE atomicFetchXorOffMAddr #-}

-- | Binary exclusive disjunction (XOR) of an element of a `MAddr` with the supplied value,
-- corresponds to @`Data.Bits.xor`@ done atomically. Returns the new value. Offset is
-- in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicXorFetchOffMAddr ::
     (MonadPrim s m, AtomicBits a)
  => MAddr a s
  -> Off a
  -> a
  -> m a
atomicXorFetchOffMAddr maddr (Off (I# i#)) a =
  withAddrMAddr# maddr $ \ addr# -> prim $ atomicXorFetchOffAddr# addr# i# a
{-# INLINE atomicXorFetchOffMAddr #-}





-- | Binary negation (NOT) of an element of a `MAddr`, corresponds to
-- @(`Data.Bits.complement`)@ done atomically. Returns the previous value. Offset is in
-- number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicFetchNotOffMAddr ::
     (MonadPrim s m, AtomicBits a)
  => MAddr a s
  -> Off a
  -> m a
atomicFetchNotOffMAddr maddr (Off (I# i#)) =
  withAddrMAddr# maddr $ \ addr# -> prim $ atomicFetchNotOffAddr# addr# i#
{-# INLINE atomicFetchNotOffMAddr #-}

-- | Binary negation (NOT) of an element of a `MAddr`, corresponds to
-- @(`Data.Bits.complement`)@ done atomically. Returns the new value. Offset is in number
-- of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicNotFetchOffMAddr ::
     (MonadPrim s m, AtomicBits a)
  => MAddr a s
  -> Off a
  -> m a
atomicNotFetchOffMAddr maddr (Off (I# i#)) =
  withAddrMAddr# maddr $ \ addr# -> prim $ atomicNotFetchOffAddr# addr# i#
{-# INLINE atomicNotFetchOffMAddr #-}




prefetchAddr0 :: MonadPrim s m => Addr a -> m ()
prefetchAddr0 (Addr addr# _) = prim_ (prefetchAddr0# addr# 0#)
{-# INLINE prefetchAddr0 #-}

prefetchMAddr0 :: MonadPrim s m => MAddr a s -> m ()
prefetchMAddr0 (MAddr maddr# _) = prim_ (prefetchAddr0# maddr# 0#)
{-# INLINE prefetchMAddr0 #-}

prefetchAddr1 :: MonadPrim s m => Addr a -> m ()
prefetchAddr1 (Addr addr# _) = prim_ (prefetchAddr1# addr# 0#)
{-# INLINE prefetchAddr1 #-}

prefetchMAddr1 :: MonadPrim s m => MAddr a s -> m ()
prefetchMAddr1 (MAddr maddr# _) = prim_ (prefetchAddr1# maddr# 0#)
{-# INLINE prefetchMAddr1 #-}

prefetchAddr2 :: MonadPrim s m => Addr a -> m ()
prefetchAddr2 (Addr addr# _) = prim_ (prefetchAddr2# addr# 0#)
{-# INLINE prefetchAddr2 #-}

prefetchMAddr2 :: MonadPrim s m => MAddr a s -> m ()
prefetchMAddr2 (MAddr maddr# _) = prim_ (prefetchAddr2# maddr# 0#)
{-# INLINE prefetchMAddr2 #-}

prefetchAddr3 :: MonadPrim s m => Addr a -> m ()
prefetchAddr3 (Addr addr# _) = prim_ (prefetchAddr3# addr# 0#)
{-# INLINE prefetchAddr3 #-}

prefetchMAddr3 :: MonadPrim s m => MAddr a s -> m ()
prefetchMAddr3 (MAddr maddr# _) = prim_ (prefetchAddr3# maddr# 0#)
{-# INLINE prefetchMAddr3 #-}


prefetchOffAddr0 :: (MonadPrim s m, Prim a) => Addr a -> Off a -> m ()
prefetchOffAddr0 (Addr addr# _) off = prim_ (prefetchAddr0# addr# (fromOff# off))
{-# INLINE prefetchOffAddr0 #-}

prefetchOffMAddr0 :: (MonadPrim s m, Prim a) => MAddr a s -> Off a -> m ()
prefetchOffMAddr0 (MAddr maddr# _) off = prim_ (prefetchAddr0# maddr# (fromOff# off))
{-# INLINE prefetchOffMAddr0 #-}

prefetchOffAddr1 :: (MonadPrim s m, Prim a) => Addr a -> Off a -> m ()
prefetchOffAddr1 (Addr addr# _) off = prim_ (prefetchAddr1# addr# (fromOff# off))
{-# INLINE prefetchOffAddr1 #-}

prefetchOffMAddr1 :: (MonadPrim s m, Prim a) => MAddr a s -> Off a -> m ()
prefetchOffMAddr1 (MAddr maddr# _) off = prim_ (prefetchAddr1# maddr# (fromOff# off))
{-# INLINE prefetchOffMAddr1 #-}

prefetchOffAddr2 :: (MonadPrim s m, Prim a) => Addr a -> Off a -> m ()
prefetchOffAddr2 (Addr addr# _) off = prim_ (prefetchAddr2# addr# (fromOff# off))
{-# INLINE prefetchOffAddr2 #-}

prefetchOffMAddr2 :: (MonadPrim s m, Prim a) => MAddr a s -> Off a -> m ()
prefetchOffMAddr2 (MAddr maddr# _) off = prim_ (prefetchAddr2# maddr# (fromOff# off))
{-# INLINE prefetchOffMAddr2 #-}

prefetchOffAddr3 :: (MonadPrim s m, Prim a) => Addr a -> Off a -> m ()
prefetchOffAddr3 (Addr addr# _) off = prim_ (prefetchAddr3# addr# (fromOff# off))
{-# INLINE prefetchOffAddr3 #-}

prefetchOffMAddr3 :: (MonadPrim s m, Prim a) => MAddr a s -> Off a -> m ()
prefetchOffMAddr3 (MAddr maddr# _) off = prim_ (prefetchAddr3# maddr# (fromOff# off))
{-# INLINE prefetchOffMAddr3 #-}

