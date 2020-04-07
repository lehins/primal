{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Data.Prim.Class
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Class
  ( Prim(..)
  , mutableByteArrayContents#
  , setMutableByteArrayLoop#
  ) where

import GHC.Exts
import GHC.Int
import GHC.Word
import Control.Monad.Prim.Unsafe
import Data.Prim.Foreign

#include "MachDeps.h"

-- | A type class describing how a data type can be written to and read from memory.
class Coercible a (PrimBase a) => Prim a where
  type PrimBase a :: *

  -- | Size of a value in bytes.
  sizeOf# :: Proxy# a -> Int
  default sizeOf# :: Prim (PrimBase a) => Proxy# a -> Int
  sizeOf# _ = sizeOf# (proxy# :: Proxy# (PrimBase a))
  {-# INLINE sizeOf# #-}

  -- | Memory alignment of a value in bytes.
  alignment# :: Proxy# a -> Int
  default alignment# :: Prim (PrimBase a) => Proxy# a -> Int
  alignment# _ = alignment# (proxy# :: Proxy# (PrimBase a))
  {-# INLINE alignment# #-}

  -- | Read a value from the primitive `ByteArray#`. The offset is in elements of type
  -- @a@ rather than in bytes.
  --
  -- The following equality holds:
  --
  -- > indexByteArray# ba# i# == indexOffAddr# (byteArrayContents# ba#) i#
  --
  indexByteArray# :: ByteArray# -> Int# -> a
  default indexByteArray# :: Prim (PrimBase a) => ByteArray# -> Int# -> a
  indexByteArray# ba# i# = coerce (indexByteArray# ba# i# :: PrimBase a)
  {-# INLINE indexByteArray# #-}

  -- | Index an element from memory specified by an address and an offset. The offset is
  -- in elements of type @a@ rather than in bytes.
  indexOffAddr# :: Addr# -> Int# -> a
  default indexOffAddr# :: Prim (PrimBase a) => Addr# -> Int# -> a
  indexOffAddr# addr# i# = coerce (indexOffAddr# addr# i# :: PrimBase a)
  {-# INLINE indexOffAddr# #-}

  -- | Read a value from the the primitive `MutablByteArray#`. The offset is in elements
  -- of type @a@ rather than in bytes.
  readMutableByteArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
  default readMutableByteArray# :: Prim (PrimBase a) =>
                                   MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
  readMutableByteArray# mba# i# s# = case readMutableByteArray# mba# i# s# of
                                       (# s'#, pa :: PrimBase a #) -> (# s'#, coerce pa #)
  {-# INLINE readMutableByteArray# #-}

  -- | Read a value from a memory position given by an address and an offset.
  -- The offset is in elements of type @a@ rather than in bytes.
  readOffAddr# :: Addr# -> Int# -> State# s -> (# State# s, a #)
  default readOffAddr# :: Prim (PrimBase a) =>
                          Addr# -> Int# -> State# s -> (# State# s, a #)
  readOffAddr# addr# i# s# = case readOffAddr# addr# i# s# of
                               (# s'#, pa :: PrimBase a #) -> (# s'#, coerce pa #)
  {-# INLINE readOffAddr# #-}


  -- | Write a value to the mutable array. The offset is in elements of type @a@ rather
  -- than in bytes.
  writeMutableByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> State# s
  default writeMutableByteArray# :: Prim (PrimBase a) =>
                                    MutableByteArray# s -> Int# -> a -> State# s -> State# s
  writeMutableByteArray# mba# i# a = writeMutableByteArray# mba# i# (coerce a :: PrimBase a)
  {-# INLINE writeMutableByteArray# #-}

  -- | Write a value to a memory position given by an address and an offset.
  -- The offset is in elements of type @a@ rather than in bytes.
  writeOffAddr# :: Addr# -> Int# -> a -> State# s -> State# s
  default writeOffAddr# :: Prim (PrimBase a) =>
                           Addr# -> Int# -> a -> State# s -> State# s
  writeOffAddr# mba# i# a = writeOffAddr# mba# i# (coerce a :: PrimBase a)
  {-# INLINE writeOffAddr# #-}

  -- | Fill a slice of the mutable array with a value. The offset and length of the chunk
  -- are in elements of type @a@ rather than in bytes.
  setMutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> a -> State# s -> State# s
  default setMutableByteArray# :: Prim (PrimBase a) =>
                                  MutableByteArray# s -> Int# -> Int# -> a -> State# s -> State# s
  setMutableByteArray# mba# i# n# a = setMutableByteArray# mba# i# n# (coerce a :: PrimBase a)
  {-# INLINE setMutableByteArray# #-}

  -- | Fill a memory block given by an address, an offset and a length.
  -- The offset and length are in elements of type @a@ rather than in bytes.
  setOffAddr# :: Addr# -> Int# -> Int# -> a -> State# s -> State# s
  default setOffAddr# :: Prim (PrimBase a) =>
                         Addr# -> Int# -> Int# -> a -> State# s -> State# s
  setOffAddr# mba# i# n# a = setOffAddr# mba# i# n# (coerce a :: PrimBase a)
  {-# INLINE setOffAddr# #-}

instance Prim Int where
  type PrimBase Int = Int
  sizeOf# _ = SIZEOF_HSINT
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_HSINT
  {-# INLINE alignment# #-}
  indexByteArray# ba# i# = I# (indexIntArray# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# ba# i# = I# (indexIntOffAddr# ba# i#)
  {-# INLINE indexOffAddr# #-}
  readMutableByteArray# mba# i# s# = case readIntArray# mba# i# s# of
                                       (# s'#, a# #) -> (# s'#, I# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s# = case readIntOffAddr# mba# i# s# of
                              (# s'#, a# #) -> (# s'#, I# a# #)
  {-# INLINE readOffAddr# #-}
  writeMutableByteArray# mba# i# (I# a#) = writeIntArray# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (I# a#) = writeIntOffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
#if WORD_SIZE_IN_BITS >= 64
  setMutableByteArray# mba# o# n# (I# a#) = setMutableByteArray# mba# o# n# (I64# a#)
  setOffAddr# addr# o# n# (I# a#) = setOffAddr# addr# o# n# (I64# a#)
#else
  setMutableByteArray# mba# o# n# (I# a#) = setMutableByteArray# mba# o# n# (I32# a#)
  setOffAddr# addr# o# n# (I# a#) = setOffAddr# addr# o# n# (I32# a#)
#endif
  {-# INLINE setMutableByteArray# #-}
  {-# INLINE setOffAddr# #-}

instance Prim Int8 where
  type PrimBase Int8 = Int8
  sizeOf# _ = SIZEOF_INT8
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_INT8
  {-# INLINE alignment# #-}
  indexByteArray# ba# i# = I8# (indexInt8Array# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# ba# i# = I8# (indexInt8OffAddr# ba# i#)
  {-# INLINE indexOffAddr# #-}
  readMutableByteArray# mba# i# s# = case readInt8Array# mba# i# s# of
                                       (# s'#, a# #) -> (# s'#, I8# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s# = case readInt8OffAddr# mba# i# s# of
                              (# s'#, a# #) -> (# s'#, I8# a# #)
  {-# INLINE readOffAddr# #-}
  writeMutableByteArray# mba# i# (I8# a#) = writeInt8Array# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (I8# a#) = writeInt8OffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
  setMutableByteArray# mba# i# n# (I8# a#) = setByteArray# mba# i# n# a#
  {-# INLINE setMutableByteArray# #-}
  setOffAddr# addr# o# n# a = unsafePrimBase_ (memsetInt8Addr# addr# o# n# a)
  {-# INLINE setOffAddr# #-}

instance Prim Int16 where
  type PrimBase Int16 = Int16
  sizeOf# _ = SIZEOF_INT16
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_INT16
  {-# INLINE alignment# #-}
  indexByteArray# ba# i# = I16# (indexInt16Array# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# ba# i# = I16# (indexInt16OffAddr# ba# i#)
  {-# INLINE indexOffAddr# #-}
  readMutableByteArray# mba# i# s# = case readInt16Array# mba# i# s# of
                                       (# s'#, a# #) -> (# s'#, I16# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s# = case readInt16OffAddr# mba# i# s# of
                              (# s'#, a# #) -> (# s'#, I16# a# #)
  {-# INLINE readOffAddr# #-}
  writeMutableByteArray# mba# i# (I16# a#) = writeInt16Array# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (I16# a#) = writeInt16OffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
  setMutableByteArray# mba# o# n# a = unsafePrimBase_ (memsetInt16MutableByteArray# mba# o# n# a)
  {-# INLINE setMutableByteArray# #-}
  setOffAddr# addr# o# n# a = unsafePrimBase_ (memsetInt16Addr# addr# o# n# a)
  {-# INLINE setOffAddr# #-}

instance Prim Int32 where
  type PrimBase Int32 = Int32
  sizeOf# _ = SIZEOF_INT32
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_INT32
  {-# INLINE alignment# #-}
  indexByteArray# ba# i# = I32# (indexInt32Array# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# ba# i# = I32# (indexInt32OffAddr# ba# i#)
  {-# INLINE indexOffAddr# #-}
  readMutableByteArray# mba# i# s# = case readInt32Array# mba# i# s# of
                                       (# s'#, a# #) -> (# s'#, I32# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s# = case readInt32OffAddr# mba# i# s# of
                              (# s'#, a# #) -> (# s'#, I32# a# #)
  {-# INLINE readOffAddr# #-}
  writeMutableByteArray# mba# i# (I32# a#) = writeInt32Array# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (I32# a#) = writeInt32OffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
  setMutableByteArray# mba# o# n# a = unsafePrimBase_ (memsetInt32MutableByteArray# mba# o# n# a)
  {-# INLINE setMutableByteArray# #-}
  setOffAddr# addr# o# n# a = unsafePrimBase_ (memsetInt32Addr# addr# o# n# a)
  {-# INLINE setOffAddr# #-}

instance Prim Int64 where
  type PrimBase Int64 = Int64
  sizeOf# _ = SIZEOF_INT64
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_INT64
  {-# INLINE alignment# #-}
  indexByteArray# ba# i# = I64# (indexInt64Array# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# ba# i# = I64# (indexInt64OffAddr# ba# i#)
  {-# INLINE indexOffAddr# #-}
  readMutableByteArray# mba# i# s# = case readInt64Array# mba# i# s# of
                                       (# s'#, a# #) -> (# s'#, I64# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s# = case readInt64OffAddr# mba# i# s# of
                              (# s'#, a# #) -> (# s'#, I64# a# #)
  {-# INLINE readOffAddr# #-}
  writeMutableByteArray# mba# i# (I64# a#) = writeInt64Array# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (I64# a#) = writeInt64OffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
  setMutableByteArray# mba# o# n# a = unsafePrimBase_ (memsetInt64MutableByteArray# mba# o# n# a)
  {-# INLINE setMutableByteArray# #-}
  setOffAddr# addr# o# n# a = unsafePrimBase_ (memsetInt64Addr# addr# o# n# a)
  {-# INLINE setOffAddr# #-}


instance Prim Word where
  type PrimBase Word = Word
  sizeOf# _ = SIZEOF_HSWORD
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_HSWORD
  {-# INLINE alignment# #-}
  indexByteArray# ba# i# = W# (indexWordArray# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# ba# i# = W# (indexWordOffAddr# ba# i#)
  {-# INLINE indexOffAddr# #-}
  readMutableByteArray# mba# i# s# = case readWordArray# mba# i# s# of
                                       (# s'#, a# #) -> (# s'#, W# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s# = case readWordOffAddr# mba# i# s# of
                              (# s'#, a# #) -> (# s'#, W# a# #)
  {-# INLINE readOffAddr# #-}
  writeMutableByteArray# mba# i# (W# a#) = writeWordArray# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (W# a#) = writeWordOffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
#if WORD_SIZE_IN_BITS >= 64
  setMutableByteArray# mba# o# n# (W# a#) = setMutableByteArray# mba# o# n# (W64# a#)
  setOffAddr# addr# o# n# (W# a#) = setOffAddr# addr# o# n# (W64# a#)
#else
  setMutableByteArray# mba# o# n# (W# a#) = setMutableByteArray# mba# o# n# (W32# a#)
  setOffAddr# addr# o# n# (W# a#) = setOffAddr# addr# o# n# (W32# a#)
#endif
  {-# INLINE setMutableByteArray# #-}
  {-# INLINE setOffAddr# #-}

instance Prim Word8 where
  type PrimBase Word8 = Word8
  sizeOf# _ = SIZEOF_WORD8
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_WORD8
  {-# INLINE alignment# #-}
  indexByteArray# ba# i# = W8# (indexWord8Array# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# ba# i# = W8# (indexWord8OffAddr# ba# i#)
  {-# INLINE indexOffAddr# #-}
  readMutableByteArray# mba# i# s# = case readWord8Array# mba# i# s# of
                                       (# s'#, a# #) -> (# s'#, W8# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s# = case readWord8OffAddr# mba# i# s# of
                              (# s'#, a# #) -> (# s'#, W8# a# #)
  {-# INLINE readOffAddr# #-}
  writeMutableByteArray# mba# i# (W8# a#) = writeWord8Array# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (W8# a#) = writeWord8OffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
  setMutableByteArray# mba# i# n# (W8# a#) = setByteArray# mba# i# n# (word2Int# a#)
  {-# INLINE setMutableByteArray# #-}
  setOffAddr# addr# o# n# a = unsafePrimBase_ (memsetWord8Addr# addr# o# n# a)
  {-# INLINE setOffAddr# #-}

instance Prim Word16 where
  type PrimBase Word16 = Word16
  sizeOf# _ = SIZEOF_WORD16
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_WORD16
  {-# INLINE alignment# #-}
  indexByteArray# ba# i# = W16# (indexWord16Array# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# ba# i# = W16# (indexWord16OffAddr# ba# i#)
  {-# INLINE indexOffAddr# #-}
  readMutableByteArray# mba# i# s# = case readWord16Array# mba# i# s# of
                                       (# s'#, a# #) -> (# s'#, W16# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s# = case readWord16OffAddr# mba# i# s# of
                              (# s'#, a# #) -> (# s'#, W16# a# #)
  {-# INLINE readOffAddr# #-}
  writeMutableByteArray# mba# i# (W16# a#) = writeWord16Array# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (W16# a#) = writeWord16OffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
  setMutableByteArray# mba# o# n# a = unsafePrimBase_ (memsetWord16MutableByteArray# mba# o# n# a)
  {-# INLINE setMutableByteArray# #-}
  setOffAddr# addr# o# n# a = unsafePrimBase_ (memsetWord16Addr# addr# o# n# a)
  {-# INLINE setOffAddr# #-}

instance Prim Word32 where
  type PrimBase Word32 = Word32
  sizeOf# _ = SIZEOF_WORD32
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_WORD32
  {-# INLINE alignment# #-}
  indexByteArray# ba# i# = W32# (indexWord32Array# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# ba# i# = W32# (indexWord32OffAddr# ba# i#)
  {-# INLINE indexOffAddr# #-}
  readMutableByteArray# mba# i# s# = case readWord32Array# mba# i# s# of
                                       (# s'#, a# #) -> (# s'#, W32# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s# = case readWord32OffAddr# mba# i# s# of
                              (# s'#, a# #) -> (# s'#, W32# a# #)
  {-# INLINE readOffAddr# #-}
  writeMutableByteArray# mba# i# (W32# a#) = writeWord32Array# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (W32# a#) = writeWord32OffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
  setMutableByteArray# mba# o# n# a = unsafePrimBase_ (memsetWord32MutableByteArray# mba# o# n# a)
  {-# INLINE setMutableByteArray# #-}
  setOffAddr# addr# o# n# a = unsafePrimBase_ (memsetWord32Addr# addr# o# n# a)
  {-# INLINE setOffAddr# #-}

instance Prim Word64 where
  type PrimBase Word64 = Word64
  sizeOf# _ = SIZEOF_WORD64
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_WORD64
  {-# INLINE alignment# #-}
  indexByteArray# ba# i# = W64# (indexWord64Array# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# ba# i# = W64# (indexWord64OffAddr# ba# i#)
  {-# INLINE indexOffAddr# #-}
  readMutableByteArray# mba# i# s# = case readWord64Array# mba# i# s# of
                                       (# s'#, a# #) -> (# s'#, W64# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s# = case readWord64OffAddr# mba# i# s# of
                              (# s'#, a# #) -> (# s'#, W64# a# #)
  {-# INLINE readOffAddr# #-}
  writeMutableByteArray# mba# i# (W64# a#) = writeWord64Array# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (W64# a#) = writeWord64OffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
  setMutableByteArray# mba# o# n# a = unsafePrimBase_ (memsetWord64MutableByteArray# mba# o# n# a)
  {-# INLINE setMutableByteArray# #-}
  setOffAddr# addr# o# n# a = unsafePrimBase_ (memsetWord64Addr# addr# o# n# a)
  {-# INLINE setOffAddr# #-}

mutableByteArrayContents# :: MutableByteArray# s -> Addr#
mutableByteArrayContents# mba# = byteArrayContents# (unsafeCoerce# mba#)
{-# INLINE mutableByteArrayContents# #-}

-- | A loop that uses `writeMutableByteArray#` to set the values in the region. It is a
-- suboptimal way to fill the memory with a single value that is why it is only provided
-- here for convenience
setMutableByteArrayLoop# ::
     Prim a => MutableByteArray# s -> Int# -> Int# -> a -> State# s -> State# s
setMutableByteArrayLoop# mba# o# n# a = go o#
  where
    k# = o# +# n#
    go i# s#
      | isTrue# (i# <# k#) = go (i# +# 1#) (writeMutableByteArray# mba# i# a s#)
      | otherwise = s#
{-# INLINE setMutableByteArrayLoop# #-}

