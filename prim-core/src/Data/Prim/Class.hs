{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
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
  , thawByteArray#
  , mutableByteArrayContents#
  , setMutableByteArrayLoop#
  , impossibleError
  ) where

import Control.Monad.Prim.Unsafe
import Data.Prim.Foreign
import Foreign.C.Types
import Foreign.Ptr
import GHC.Exts
import GHC.Int
import GHC.TypeNats
import GHC.Word

#include "MachDeps.h"
#include "HsBaseConfig.h"

-- | A type class describing how a data type can be written to and read from memory.
class Coercible a (PrimBase a) => Prim a where
  type PrimBase a :: *

  type SizeOf a :: Nat
  type SizeOf a = SizeOf (PrimBase a)
  type Alignment a :: Nat
  type Alignment a = Alignment (PrimBase a)

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
  type SizeOf Int = SIZEOF_HSINT
  type Alignment Int = ALIGNMENT_HSINT
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
  type SizeOf Int8 = SIZEOF_INT8
  type Alignment Int8 = ALIGNMENT_INT8
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
  type SizeOf Int16 = SIZEOF_INT16
  type Alignment Int16 = ALIGNMENT_INT16
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
  type SizeOf Int32 = SIZEOF_INT32
  type Alignment Int32 = ALIGNMENT_INT32
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
  type SizeOf Int64 = SIZEOF_INT64
  type Alignment Int64 = ALIGNMENT_INT64
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
  type SizeOf Word = SIZEOF_HSWORD
  type Alignment Word = ALIGNMENT_HSWORD
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
  type SizeOf Word8 = SIZEOF_WORD8
  type Alignment Word8 = ALIGNMENT_WORD8
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
  type SizeOf Word16 = SIZEOF_WORD16
  type Alignment Word16 = ALIGNMENT_WORD16
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
  type SizeOf Word32 = SIZEOF_WORD32
  type Alignment Word32 = ALIGNMENT_WORD32
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
  type SizeOf Word64 = SIZEOF_WORD64
  type Alignment Word64 = ALIGNMENT_WORD64
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


instance Prim Float where
  type PrimBase Float = Float
  type SizeOf Float = SIZEOF_FLOAT
  type Alignment Float = ALIGNMENT_FLOAT
  sizeOf# _ = SIZEOF_FLOAT
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_FLOAT
  {-# INLINE alignment# #-}
  indexByteArray# ba# i# = F# (indexFloatArray# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# ba# i# = F# (indexFloatOffAddr# ba# i#)
  {-# INLINE indexOffAddr# #-}
  readMutableByteArray# mba# i# s# = case readFloatArray# mba# i# s# of
                                       (# s'#, a# #) -> (# s'#, F# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s# = case readFloatOffAddr# mba# i# s# of
                              (# s'#, a# #) -> (# s'#, F# a# #)
  {-# INLINE readOffAddr# #-}
  writeMutableByteArray# mba# i# (F# a#) = writeFloatArray# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (F# a#) = writeFloatOffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
  setMutableByteArray# mba# o# n# a = unsafePrimBase_ (memsetFloatMutableByteArray# mba# o# n# a)
  {-# INLINE setMutableByteArray# #-}
  setOffAddr# addr# o# n# a = unsafePrimBase_ (memsetFloatAddr# addr# o# n# a)
  {-# INLINE setOffAddr# #-}

instance Prim Double where
  type PrimBase Double = Double
  type SizeOf Double = SIZEOF_DOUBLE
  type Alignment Double = ALIGNMENT_DOUBLE
  sizeOf# _ = SIZEOF_DOUBLE
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_DOUBLE
  {-# INLINE alignment# #-}
  indexByteArray# ba# i# = D# (indexDoubleArray# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# ba# i# = D# (indexDoubleOffAddr# ba# i#)
  {-# INLINE indexOffAddr# #-}
  readMutableByteArray# mba# i# s# = case readDoubleArray# mba# i# s# of
                                       (# s'#, a# #) -> (# s'#, D# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s# = case readDoubleOffAddr# mba# i# s# of
                              (# s'#, a# #) -> (# s'#, D# a# #)
  {-# INLINE readOffAddr# #-}
  writeMutableByteArray# mba# i# (D# a#) = writeDoubleArray# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (D# a#) = writeDoubleOffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
  setMutableByteArray# mba# o# n# a = unsafePrimBase_ (memsetDoubleMutableByteArray# mba# o# n# a)
  {-# INLINE setMutableByteArray# #-}
  setOffAddr# addr# o# n# a = unsafePrimBase_ (memsetDoubleAddr# addr# o# n# a)
  {-# INLINE setOffAddr# #-}

bool2int# :: Bool -> Int#
bool2int# b = if b then 1# else 0#

instance Prim Bool where
  type PrimBase Bool = Bool
  type SizeOf Bool = SIZEOF_INT8
  type Alignment Bool = ALIGNMENT_INT8
  sizeOf# _ = SIZEOF_INT8
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_INT8
  {-# INLINE alignment# #-}
  indexByteArray# ba# i# = isTrue# (indexInt8Array# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# ba# i# = isTrue# (indexInt8OffAddr# ba# i#)
  {-# INLINE indexOffAddr# #-}
  readMutableByteArray# mba# i# s# = case readInt8Array# mba# i# s# of
                                       (# s'#, a# #) -> (# s'#, isTrue# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s# = case readInt8OffAddr# mba# i# s# of
                              (# s'#, a# #) -> (# s'#, isTrue# a# #)
  {-# INLINE readOffAddr# #-}
  writeMutableByteArray# mba# i# b = writeInt8Array# mba# i# (bool2int# b)
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# b = writeInt8OffAddr# mba# i# (bool2int# b)
  {-# INLINE writeOffAddr# #-}
  setMutableByteArray# mba# o# n# b = setByteArray# mba# o# n# (bool2int# b)
  {-# INLINE setMutableByteArray# #-}
  setOffAddr# addr# o# n# b = setOffAddr# addr# o# n# (I8# (bool2int# b))
  {-# INLINE setOffAddr# #-}

instance Prim Char where
  type PrimBase Char = Char
  type SizeOf Char = SIZEOF_INT32
  type Alignment Char = ALIGNMENT_INT32
  sizeOf# _ = SIZEOF_INT32
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_INT32
  {-# INLINE alignment# #-}
  indexByteArray# ba# i# = C# (chr# (indexInt32Array# ba# i#))
  {-# INLINE indexByteArray# #-}
  indexOffAddr# ba# i# = C# (chr# (indexInt32OffAddr# ba# i#))
  {-# INLINE indexOffAddr# #-}
  readMutableByteArray# mba# i# s# = case readInt32Array# mba# i# s# of
                                       (# s'#, a# #) -> (# s'#, C# (chr# a#) #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s# = case readInt32OffAddr# mba# i# s# of
                              (# s'#, a# #) -> (# s'#, C# (chr# a#) #)
  {-# INLINE readOffAddr# #-}
  writeMutableByteArray# mba# i# (C# a#) = writeInt32Array# mba# i# (ord# a#)
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (C# a#) = writeInt32OffAddr# mba# i# (ord# a#)
  {-# INLINE writeOffAddr# #-}
  setMutableByteArray# mba# o# n# (C# a#) = setMutableByteArray# mba# o# n# (I32# (ord# a#))
  {-# INLINE setMutableByteArray# #-}
  setOffAddr# addr# o# n# (C# a#) = setOffAddr# addr# o# n# (I32# (ord# a#))
  {-# INLINE setOffAddr# #-}

instance Prim (Ptr a) where
  type PrimBase (Ptr a) = Ptr a
  type SizeOf (Ptr a) = SIZEOF_HSINT
  type Alignment (Ptr a) = ALIGNMENT_HSINT
  sizeOf# _ = SIZEOF_HSINT
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_HSINT
  {-# INLINE alignment# #-}
  indexByteArray# ba# i# = Ptr (int2Addr# (indexIntArray# ba# i#))
  {-# INLINE indexByteArray# #-}
  indexOffAddr# ba# i# = Ptr (int2Addr# (indexIntOffAddr# ba# i#))
  {-# INLINE indexOffAddr# #-}
  readMutableByteArray# mba# i# s# = case readIntArray# mba# i# s# of
                                       (# s'#, a# #) -> (# s'#, Ptr (int2Addr# a#) #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s# = case readIntOffAddr# mba# i# s# of
                              (# s'#, a# #) -> (# s'#, Ptr (int2Addr# a#) #)
  {-# INLINE readOffAddr# #-}
  writeMutableByteArray# mba# i# (Ptr a#) = writeIntArray# mba# i# (addr2Int# a#)
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (Ptr a#) = writeIntOffAddr# mba# i# (addr2Int# a#)
  {-# INLINE writeOffAddr# #-}
#if WORD_SIZE_IN_BITS >= 64
  setMutableByteArray# mba# o# n# (Ptr a#) = setMutableByteArray# mba# o# n# (I64# (addr2Int# a#))
  setOffAddr# addr# o# n# (Ptr a#) = setOffAddr# addr# o# n# (I64# (addr2Int# a#))
#else
  setMutableByteArray# mba# o# n# (Ptr a#) = setMutableByteArray# mba# o# n# (I32# (addr2Int# a#))
  setOffAddr# addr# o# n# (Ptr a#) = setOffAddr# addr# o# n# (I32# (addr2Int# a#))
#endif
  {-# INLINE setMutableByteArray# #-}
  {-# INLINE setOffAddr# #-}

instance Prim (FunPtr a) where
  type PrimBase (FunPtr a) = FunPtr a
  type SizeOf (FunPtr a) = SIZEOF_HSINT
  type Alignment (FunPtr a) = ALIGNMENT_HSINT
  sizeOf# _ = SIZEOF_HSINT
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_HSINT
  {-# INLINE alignment# #-}
  indexByteArray# ba# i# = FunPtr (int2Addr# (indexIntArray# ba# i#))
  {-# INLINE indexByteArray# #-}
  indexOffAddr# ba# i# = FunPtr (int2Addr# (indexIntOffAddr# ba# i#))
  {-# INLINE indexOffAddr# #-}
  readMutableByteArray# mba# i# s# = case readIntArray# mba# i# s# of
                                       (# s'#, a# #) -> (# s'#, FunPtr (int2Addr# a#) #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s# = case readIntOffAddr# mba# i# s# of
                              (# s'#, a# #) -> (# s'#, FunPtr (int2Addr# a#) #)
  {-# INLINE readOffAddr# #-}
  writeMutableByteArray# mba# i# (FunPtr a#) = writeIntArray# mba# i# (addr2Int# a#)
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (FunPtr a#) = writeIntOffAddr# mba# i# (addr2Int# a#)
  {-# INLINE writeOffAddr# #-}
#if WORD_SIZE_IN_BITS >= 64
  setMutableByteArray# mba# o# n# (FunPtr a#) = setMutableByteArray# mba# o# n# (I64# (addr2Int# a#))
  setOffAddr# addr# o# n# (FunPtr a#) = setOffAddr# addr# o# n# (I64# (addr2Int# a#))
#else
  setMutableByteArray# mba# o# n# (FunPtr a#) = setMutableByteArray# mba# o# n# (I32# (addr2Int# a#))
  setOffAddr# addr# o# n# (FunPtr a#) = setOffAddr# addr# o# n# (I32# (addr2Int# a#))
#endif
  {-# INLINE setMutableByteArray# #-}
  {-# INLINE setOffAddr# #-}

instance Prim IntPtr where
  type PrimBase IntPtr = Int

instance Prim WordPtr where
  type PrimBase WordPtr = Word

instance Prim CBool where
  type PrimBase CBool = HTYPE_BOOL

instance Prim CChar where
  type PrimBase CChar = HTYPE_CHAR

instance Prim CSChar where
  type PrimBase CSChar = HTYPE_SIGNED_CHAR

instance Prim CUChar where
  type PrimBase CUChar = HTYPE_UNSIGNED_CHAR

instance Prim CShort where
  type PrimBase CShort = HTYPE_SHORT

instance Prim CUShort where
  type PrimBase CUShort = HTYPE_UNSIGNED_SHORT

instance Prim CInt where
  type PrimBase CInt = HTYPE_INT

instance Prim CUInt where
  type PrimBase CUInt = HTYPE_UNSIGNED_INT

instance Prim CLong where
  type PrimBase CLong = HTYPE_LONG

instance Prim CULong where
  type PrimBase CULong = HTYPE_UNSIGNED_LONG

instance Prim CPtrdiff where
  type PrimBase CPtrdiff = HTYPE_PTRDIFF_T

instance Prim CSize where
  type PrimBase CSize = HTYPE_SIZE_T

instance Prim CWchar where
  type PrimBase CWchar = HTYPE_WCHAR_T

instance Prim CSigAtomic where
  type PrimBase CSigAtomic = HTYPE_SIG_ATOMIC_T

instance Prim CIntPtr where
  type PrimBase CIntPtr = HTYPE_INTPTR_T

instance Prim CUIntPtr where
  type PrimBase CUIntPtr = HTYPE_UINTPTR_T

instance Prim CIntMax where
  type PrimBase CIntMax = HTYPE_INTMAX_T

instance Prim CUIntMax where
  type PrimBase CUIntMax = HTYPE_UINTMAX_T




thawByteArray# :: ByteArray# -> State# s -> (# State# s, MutableByteArray# s #)
thawByteArray# ba# s# = (# s#, unsafeCoerce# ba# #)
{-# INLINE thawByteArray# #-}

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


impossibleError :: String -> String -> a
impossibleError fname msg = errorWithoutStackTrace $ "Impossible <" ++ fname ++ ">:" ++ msg
{-# NOINLINE impossibleError #-}
