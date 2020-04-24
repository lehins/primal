{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
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
  , errorImpossible
  , bool2Int#
  , int2Bool#
  ) where

import Control.Prim.Monad.Unsafe
import Foreign.Prim
import Foreign.C.Types
import Foreign.Ptr
import GHC.Stable
import GHC.Exts
import GHC.Int
import GHC.TypeNats as Nats
import GHC.Word
import Data.Kind

#include "MachDeps.h"
#include "HsBaseConfig.h"

-- | A type class describing how a data type can be written to and read from memory.
class Prim a where
  type PrimBase a :: Type

  type SizeOf a :: Nat
  type SizeOf a = SizeOf (PrimBase a)
  type Alignment a :: Nat
  type Alignment a = Alignment (PrimBase a)

  toPrim :: a -> PrimBase a
  default toPrim :: (Coercible a (PrimBase a), Prim (PrimBase a)) => a -> PrimBase a
  toPrim = coerce

  fromPrim :: PrimBase a -> a
  default fromPrim :: (Coercible a (PrimBase a), Prim (PrimBase a)) => PrimBase a -> a
  fromPrim = coerce

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
  indexByteArray# ba# i# = fromPrim (indexByteArray# ba# i# :: PrimBase a)
  {-# INLINE indexByteArray# #-}

  -- | Index an element from memory specified by an address and an offset. The offset is
  -- in elements of type @a@ rather than in bytes.
  indexOffAddr# :: Addr# -> Int# -> a
  default indexOffAddr# :: Prim (PrimBase a) => Addr# -> Int# -> a
  indexOffAddr# addr# i# = fromPrim (indexOffAddr# addr# i# :: PrimBase a)
  {-# INLINE indexOffAddr# #-}

  -- | Read a value from the the primitive `MutablByteArray#`. The offset is in elements
  -- of type @a@ rather than in bytes.
  readMutableByteArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
  default readMutableByteArray# :: Prim (PrimBase a) =>
                                   MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
  readMutableByteArray# mba# i# s = case readMutableByteArray# mba# i# s of
                                      (# s', pa :: PrimBase a #) -> (# s', fromPrim pa #)
  {-# INLINE readMutableByteArray# #-}

  -- | Read a value from a memory position given by an address and an offset.
  -- The offset is in elements of type @a@ rather than in bytes.
  readOffAddr# :: Addr# -> Int# -> State# s -> (# State# s, a #)
  default readOffAddr# :: Prim (PrimBase a) =>
                          Addr# -> Int# -> State# s -> (# State# s, a #)
  readOffAddr# addr# i# s = case readOffAddr# addr# i# s of
                              (# s', pa :: PrimBase a #) -> (# s', fromPrim pa #)
  {-# INLINE readOffAddr# #-}


  -- | Write a value to the mutable array. The offset is in elements of type @a@ rather
  -- than in bytes.
  writeMutableByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> State# s
  default writeMutableByteArray# :: Prim (PrimBase a) =>
                                    MutableByteArray# s -> Int# -> a -> State# s -> State# s
  writeMutableByteArray# mba# i# a = writeMutableByteArray# mba# i# (toPrim a :: PrimBase a)
  {-# INLINE writeMutableByteArray# #-}

  -- | Write a value to a memory position given by an address and an offset.
  -- The offset is in elements of type @a@ rather than in bytes.
  writeOffAddr# :: Addr# -> Int# -> a -> State# s -> State# s
  default writeOffAddr# :: Prim (PrimBase a) =>
                           Addr# -> Int# -> a -> State# s -> State# s
  writeOffAddr# mba# i# a = writeOffAddr# mba# i# (toPrim a)
  {-# INLINE writeOffAddr# #-}

  -- | Fill a slice of the mutable array with a value. The offset and length of the chunk
  -- are in elements of type @a@ rather than in bytes.
  setMutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> a -> State# s -> State# s
  default setMutableByteArray# :: Prim (PrimBase a) =>
                                  MutableByteArray# s -> Int# -> Int# -> a -> State# s -> State# s
  setMutableByteArray# mba# i# n# a = setMutableByteArray# mba# i# n# (toPrim a)
  {-# INLINE setMutableByteArray# #-}

  -- | Fill a memory block given by an address, an offset and a length.
  -- The offset and length are in elements of type @a@ rather than in bytes.
  setOffAddr# :: Addr# -> Int# -> Int# -> a -> State# s -> State# s
  default setOffAddr# :: Prim (PrimBase a) =>
                         Addr# -> Int# -> Int# -> a -> State# s -> State# s
  setOffAddr# mba# i# n# a = setOffAddr# mba# i# n# (toPrim a)
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
  indexOffAddr# addr# i# = I# (indexIntOffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readMutableByteArray# mba# i# s = case readIntArray# mba# i# s of
                                      (# s', a# #) -> (# s', I# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readIntOffAddr# mba# i# s of
                             (# s', a# #) -> (# s', I# a# #)
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
  indexOffAddr# addr# i# = I8# (indexInt8OffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readMutableByteArray# mba# i# s = case readInt8Array# mba# i# s of
                                      (# s', a# #) -> (# s', I8# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readInt8OffAddr# mba# i# s of
                             (# s', a# #) -> (# s', I8# a# #)
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
  indexOffAddr# addr# i# = I16# (indexInt16OffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readMutableByteArray# mba# i# s = case readInt16Array# mba# i# s of
                                      (# s', a# #) -> (# s', I16# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readInt16OffAddr# mba# i# s of
                             (# s', a# #) -> (# s', I16# a# #)
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
  indexOffAddr# addr# i# = I32# (indexInt32OffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readMutableByteArray# mba# i# s = case readInt32Array# mba# i# s of
                                      (# s', a# #) -> (# s', I32# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readInt32OffAddr# mba# i# s of
                             (# s', a# #) -> (# s', I32# a# #)
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
  indexOffAddr# addr# i# = I64# (indexInt64OffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readMutableByteArray# mba# i# s = case readInt64Array# mba# i# s of
                                      (# s', a# #) -> (# s', I64# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readInt64OffAddr# mba# i# s of
                             (# s', a# #) -> (# s', I64# a# #)
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
  indexOffAddr# addr# i# = W# (indexWordOffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readMutableByteArray# mba# i# s = case readWordArray# mba# i# s of
                                      (# s', a# #) -> (# s', W# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readWordOffAddr# mba# i# s of
                             (# s', a# #) -> (# s', W# a# #)
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
  indexOffAddr# addr# i# = W8# (indexWord8OffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readMutableByteArray# mba# i# s = case readWord8Array# mba# i# s of
                                      (# s', a# #) -> (# s', W8# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readWord8OffAddr# mba# i# s of
                             (# s', a# #) -> (# s', W8# a# #)
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
  indexOffAddr# addr# i# = W16# (indexWord16OffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readMutableByteArray# mba# i# s = case readWord16Array# mba# i# s of
                                      (# s', a# #) -> (# s', W16# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readWord16OffAddr# mba# i# s of
                             (# s', a# #) -> (# s', W16# a# #)
  {-# INLINE readOffAddr# #-}
  writeMutableByteArray# mba# i# (W16# a#) = writeWord16Array# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (W16# a#) = writeWord16OffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
  setMutableByteArray# mba# o# n# a =
    unsafePrimBase_ (memsetWord16MutableByteArray# mba# o# n# a)
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
  indexOffAddr# addr# i# = W32# (indexWord32OffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readMutableByteArray# mba# i# s = case readWord32Array# mba# i# s of
                                      (# s', a# #) -> (# s', W32# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readWord32OffAddr# mba# i# s of
                             (# s', a# #) -> (# s', W32# a# #)
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
  indexOffAddr# addr# i# = W64# (indexWord64OffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readMutableByteArray# mba# i# s = case readWord64Array# mba# i# s of
                                      (# s', a# #) -> (# s', W64# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readWord64OffAddr# mba# i# s of
                             (# s', a# #) -> (# s', W64# a# #)
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
  indexOffAddr# addr# i# = F# (indexFloatOffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readMutableByteArray# mba# i# s = case readFloatArray# mba# i# s of
                                      (# s', a# #) -> (# s', F# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readFloatOffAddr# mba# i# s of
                             (# s', a# #) -> (# s', F# a# #)
  {-# INLINE readOffAddr# #-}
  writeMutableByteArray# mba# i# (F# a#) = writeFloatArray# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (F# a#) = writeFloatOffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
  setMutableByteArray# mba# o# n# (F# f#) =
    unsafePrimBase_ (memsetWord32MutableByteArray# mba# o# n# (W32# (floatToWord32# f#)))
  {-# INLINE setMutableByteArray# #-}
  setOffAddr# addr# o# n# (F# f#) =
    unsafePrimBase_ (memsetWord32Addr# addr# o# n# (W32# (floatToWord32# f#)))

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
  indexOffAddr# addr# i# = D# (indexDoubleOffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readMutableByteArray# mba# i# s = case readDoubleArray# mba# i# s of
                                      (# s', a# #) -> (# s', D# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readDoubleOffAddr# mba# i# s of
                             (# s', a# #) -> (# s', D# a# #)
  {-# INLINE readOffAddr# #-}
  writeMutableByteArray# mba# i# (D# a#) = writeDoubleArray# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (D# a#) = writeDoubleOffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
  setMutableByteArray# mba# o# n# (D# d#) =
    unsafePrimBase_ (memsetWord64MutableByteArray# mba# o# n# (W64# (doubleToWord64# d#)))
  {-# INLINE setMutableByteArray# #-}
  setOffAddr# addr# o# n# (D# d#) =
    unsafePrimBase_ (memsetWord64Addr# addr# o# n# (W64# (doubleToWord64# d#)))
  {-# INLINE setOffAddr# #-}

bool2Int# :: Bool -> Int#
bool2Int# b = if b then 1# else 0#
{-# INLINE bool2Int# #-}

int2Bool# :: Int# -> Bool
int2Bool# i# = isTrue# (i# /=# 0#) -- tagToEnum# (i# /=# 0#) -- (andI# i# 1#)
{-# INLINE int2Bool# #-}

instance Prim Bool where
  type PrimBase Bool = Int8
  fromPrim (I8# i#) = int2Bool# i#
  {-# INLINE fromPrim #-}
  toPrim b = I8# (bool2Int# b)
  {-# INLINE toPrim #-}
  -- sizeOf# _ = SIZEOF_INT8
  -- {-# INLINE sizeOf# #-}
  -- alignment# _ = ALIGNMENT_INT8
  -- {-# INLINE alignment# #-}
  -- indexByteArray# ba# i# = int2Bool# (indexInt8Array# ba# i#)
  -- {-# INLINE indexByteArray# #-}
  -- indexOffAddr# addr# i# = int2Bool# (indexInt8OffAddr# addr# i#)
  -- {-# INLINE indexOffAddr# #-}
  -- readMutableByteArray# mba# i# s = case readInt8Array# mba# i# s of
  --                                      (# s', a# #) -> (# s', int2Bool# a# #)
  -- {-# INLINE readMutableByteArray# #-}
  -- readOffAddr# mba# i# s = case readInt8OffAddr# mba# i# s of
  --                             (# s', a# #) -> (# s', int2Bool# a# #)
  -- {-# INLINE readOffAddr# #-}
  -- writeMutableByteArray# mba# i# b = writeInt8Array# mba# i# (bool2Int# b)
  -- {-# INLINE writeMutableByteArray# #-}
  -- writeOffAddr# mba# i# b = writeInt8OffAddr# mba# i# (bool2Int# b)
  -- {-# INLINE writeOffAddr# #-}
  -- setMutableByteArray# mba# o# n# b = setByteArray# mba# o# n# (bool2Int# b)
  -- {-# INLINE setMutableByteArray# #-}
  -- setOffAddr# addr# o# n# b = setOffAddr# addr# o# n# (I8# (bool2Int# b))
  -- {-# INLINE setOffAddr# #-}

instance Prim Char where
  type PrimBase Char = Char
  type SizeOf Char = SIZEOF_HSCHAR
  type Alignment Char = ALIGNMENT_HSCHAR
  sizeOf# _ = SIZEOF_HSCHAR
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_HSCHAR
  {-# INLINE alignment# #-}
  indexByteArray# ba# i# = C# (indexWideCharArray# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# = C# (indexWideCharOffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readMutableByteArray# mba# i# s = case readWideCharArray# mba# i# s of
                                      (# s', a# #) -> (# s', C# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readWideCharOffAddr# mba# i# s of
                             (# s', a# #) -> (# s', C# a# #)
  {-# INLINE readOffAddr# #-}
  writeMutableByteArray# mba# i# (C# a#) = writeWideCharArray# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (C# a#) = writeWideCharOffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
  setMutableByteArray# mba# o# n# (C# a#) = setMutableByteArray# mba# o# n# (I32# (ord# a#))
  {-# INLINE setMutableByteArray# #-}
  setOffAddr# addr# o# n# (C# a#) = setOffAddr# addr# o# n# (I32# (ord# a#))
  {-# INLINE setOffAddr# #-}

instance Prim (Ptr a) where
  type PrimBase (Ptr a) = Ptr a
  type SizeOf (Ptr a) = SIZEOF_HSPTR
  type Alignment (Ptr a) = ALIGNMENT_HSPTR
  sizeOf# _ = SIZEOF_HSINT
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_HSINT
  {-# INLINE alignment# #-}
  indexByteArray# ba# i# = Ptr (indexAddrArray# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# = Ptr (indexAddrOffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readMutableByteArray# mba# i# s = case readAddrArray# mba# i# s of
                                      (# s', a# #) -> (# s', Ptr a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readAddrOffAddr# mba# i# s of
                             (# s', a# #) -> (# s', Ptr a# #)
  {-# INLINE readOffAddr# #-}
  writeMutableByteArray# mba# i# (Ptr a#) = writeAddrArray# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (Ptr a#) = writeAddrOffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
#if SIZEOF_HSFUNPTR == SIZEOF_INT64
  setMutableByteArray# mba# o# n# (Ptr a#) = setMutableByteArray# mba# o# n# (I64# (addr2Int# a#))
  setOffAddr# addr# o# n# (Ptr a#) = setOffAddr# addr# o# n# (I64# (addr2Int# a#))
#elif SIZEOF_HSFUNPTR == SIZEOF_INT32
  setMutableByteArray# mba# o# n# (Ptr a#) = setMutableByteArray# mba# o# n# (I32# (addr2Int# a#))
  setOffAddr# addr# o# n# (Ptr a#) = setOffAddr# addr# o# n# (I32# (addr2Int# a#))
#else
#error Ptr is of unsupported size SIZEOF_HSPTR
#endif
  {-# INLINE setMutableByteArray# #-}
  {-# INLINE setOffAddr# #-}

instance Prim (FunPtr a) where
  type PrimBase (FunPtr a) = Ptr a
  toPrim (FunPtr addr#) = Ptr addr#
  fromPrim (Ptr addr#) = FunPtr addr#


instance Prim (StablePtr a) where
  type PrimBase (StablePtr a) = StablePtr a
  type SizeOf (StablePtr a) = SIZEOF_HSSTABLEPTR
  type Alignment (StablePtr a) = ALIGNMENT_HSSTABLEPTR
  sizeOf# _ = SIZEOF_HSINT
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_HSINT
  {-# INLINE alignment# #-}
  indexByteArray# ba# i# = StablePtr (indexStablePtrArray# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# = StablePtr (indexStablePtrOffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readMutableByteArray# mba# i# s = case readStablePtrArray# mba# i# s of
                                      (# s', a# #) -> (# s', StablePtr a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readStablePtrOffAddr# mba# i# s of
                             (# s', a# #) -> (# s', StablePtr a# #)
  {-# INLINE readOffAddr# #-}
  writeMutableByteArray# mba# i# (StablePtr a#) = writeStablePtrArray# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (StablePtr a#) = writeStablePtrOffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
#if SIZEOF_HSSTABLEPTR == SIZEOF_INT64
  setMutableByteArray# mba# o# n# (StablePtr a#) = setMutableByteArray# mba# o# n# (I64# (unsafeCoerce# a#))
  setOffAddr# addr# o# n# (StablePtr a#) = setOffAddr# addr# o# n# (I64# (unsafeCoerce# a#))
#elif SIZEOF_HSSTABLEPTR == SIZEOF_INT32
  setMutableByteArray# mba# o# n# (StablePtr a#) = setMutableByteArray# mba# o# n# (I32# (unsafeCoerce# a#))
  setOffAddr# addr# o# n# (StablePtr a#) = setOffAddr# addr# o# n# (I32# (unsafeCoerce# a#))
#else
#error StablePtr is of unsupported size SIZEOF_HSSTABLEPTR
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

instance Prim CLLong where
  type PrimBase CLLong = HTYPE_LONG_LONG

instance Prim CULLong where
  type PrimBase CULLong = HTYPE_UNSIGNED_LONG_LONG

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

instance Prim CFloat where
  type PrimBase CFloat = HTYPE_FLOAT

instance Prim CDouble where
  type PrimBase CDouble = HTYPE_DOUBLE




instance (Eq a, Prim a) => Prim (a, a) where
  type PrimBase (a, a) = (a, a)
  type SizeOf (a, a) = 2 Nats.* SizeOf a
  type Alignment (a, a) = 2 Nats.* Alignment a
  sizeOf# _ = 2 * sizeOf# (proxy# :: Proxy# a)
  {-# INLINE sizeOf# #-}
  alignment# _ = 2 * alignment# (proxy# :: Proxy# a)
  {-# INLINE alignment# #-}
  indexByteArray# ba# i# =
    let i2# = 2# *# i#
    in (indexByteArray# ba# i2#, indexByteArray# ba# (i2# +# 1#))
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# =
    let i2# = 2# *# i#
    in (indexOffAddr# addr# i2#, indexOffAddr# addr# (i2# +# 1#))
  {-# INLINE indexOffAddr# #-}
  readMutableByteArray# mba# i# s =
    let i2# = 2# *# i#
    in case readMutableByteArray# mba# i2# s of
         (# s', a0 #) ->
           case readMutableByteArray# mba# (i2# +# 1#) s' of
             (# s'', a1 #) -> (# s'', (a0, a1) #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# addr# i# s =
    let i2# = 2# *# i#
    in case readOffAddr# addr# i2# s of
         (# s', a0 #) ->
           case readOffAddr# addr# (i2# +# 1#) s' of
             (# s'', a1 #) -> (# s'', (a0, a1) #)
  {-# INLINE readOffAddr# #-}
  writeMutableByteArray# mba# i# (a0, a1) s =
    let i2# = 2# *# i#
    in writeMutableByteArray# mba# (i2# +# 1#) a1 (writeMutableByteArray# mba# i2# a0 s)
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# addr# i# (a0, a1) s =
    let i2# = 2# *# i#
    in writeOffAddr# addr# (i2# +# 1#) a1
       (writeOffAddr# addr# i2# a0 s)
  {-# INLINE writeOffAddr# #-}
  setMutableByteArray# mba# o# n# a@(a0, a1) s
    | a0 == a1 = setMutableByteArray# mba# (o# *# 2#) (n# *# 2#) a0 s
    | otherwise = setMutableByteArrayLoop# mba# o# n# a s
  {-# INLINE setMutableByteArray# #-}
  setOffAddr# addr# o# n# a@(a0, a1) s
    | a0 == a1 = setOffAddr# addr# (o# *# 2#) (n# *# 2#) a0 s
    | otherwise = setOffAddr# addr# o# n# a s
  {-# INLINE setOffAddr# #-}


instance (Eq a, Prim a) => Prim (a, a, a) where
  type PrimBase (a, a, a) = (a, a, a)
  type SizeOf (a, a, a) = 3 Nats.* SizeOf a
  type Alignment (a, a, a) = 3 Nats.* Alignment a
  sizeOf# _ = 3 * sizeOf# (proxy# :: Proxy# a)
  {-# INLINE sizeOf# #-}
  alignment# _ = 3 * alignment# (proxy# :: Proxy# a)
  {-# INLINE alignment# #-}
  indexByteArray# ba# i# =
    let i3# = 3# *# i#
    in ( indexByteArray# ba# i3#
       , indexByteArray# ba# (i3# +# 1#)
       , indexByteArray# ba# (i3# +# 2#)
       )
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# =
    let i3# = 3# *# i#
    in ( indexOffAddr# addr# i3#
       , indexOffAddr# addr# (i3# +# 1#)
       , indexOffAddr# addr# (i3# +# 2#)
       )
  {-# INLINE indexOffAddr# #-}
  readMutableByteArray# mba# i# s =
    let i3# = 3# *# i#
    in case readMutableByteArray# mba# i3#         s  of { (# s0, a0 #) ->
       case readMutableByteArray# mba# (i3# +# 1#) s0 of { (# s1, a1 #) ->
       case readMutableByteArray# mba# (i3# +# 2#) s1 of { (# s2, a2 #) ->
         (# s2, (a0, a1, a2) #)
       }}}
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# addr# i# s =
    let i3# = 3# *# i#
    in case readOffAddr# addr# i3#         s  of { (# s0, a0 #) ->
       case readOffAddr# addr# (i3# +# 1#) s0 of { (# s1, a1 #) ->
       case readOffAddr# addr# (i3# +# 2#) s1 of { (# s2, a2 #) ->
         (# s2, (a0, a1, a2) #)
       }}}
  {-# INLINE readOffAddr# #-}
  writeMutableByteArray# mba# i# (a0, a1, a2) s =
    let i3# = 3# *# i#
    in writeMutableByteArray# mba# (i3# +# 2#) a2
       (writeMutableByteArray# mba# (i3# +# 1#) a1
        (writeMutableByteArray# mba# i3# a0 s))
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# addr# i# (a0, a1, a2) s =
    let i3# = 3# *# i#
    in writeOffAddr# addr# (i3# +# 2#) a2
       (writeOffAddr# addr# (i3# +# 1#) a1
        (writeOffAddr# addr# i3# a0 s))
  {-# INLINE writeOffAddr# #-}
  setMutableByteArray# mba# o# n# a@(a0, a1, a2) s
    | a0 == a1 && a1 == a2 = setMutableByteArray# mba# (o# *# 3#) (n# *# 3#) a0 s
    | otherwise = setMutableByteArrayLoop# mba# o# n# a s
  {-# INLINE setMutableByteArray# #-}
  setOffAddr# addr# o# n# a@(a0, a1, a2) s
    | a0 == a1 && a1 == a2 = setOffAddr# addr# (o# *# 3#) (n# *# 3#) a0 s
    | otherwise = setOffAddr# addr# o# n# a s
  {-# INLINE setOffAddr# #-}




thawByteArray# :: ByteArray# -> State# s -> (# State# s, MutableByteArray# s #)
thawByteArray# ba# s = (# s, unsafeCoerce# ba# #)
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
    go i# s
      | isTrue# (i# <# k#) = go (i# +# 1#) (writeMutableByteArray# mba# i# a s)
      | otherwise = s
{-# INLINE setMutableByteArrayLoop# #-}


errorImpossible :: String -> String -> a
errorImpossible fname msg = errorWithoutStackTrace $ "Impossible <" ++ fname ++ ">:" ++ msg
{-# NOINLINE errorImpossible #-}
