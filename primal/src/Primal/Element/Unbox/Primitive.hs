{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Primal.Element.Unbox.Primitive
-- Copyright   : (c) Alexey Kuleshevich 2020-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Element.Unbox.Primitive () where


#include "MachDeps.h"

import Primal.Element.Unbox.Class
import Primal.Element.Unbox.Tuples ()
import Primal.Foreign hiding (Any)
import GHC.Stable

instance Unbox Int where
  type UnboxIso Int = Int
  type SizeOf Int = SIZEOF_HSINT
  type Alignment Int = ALIGNMENT_HSINT
  sizeOf# _ = SIZEOF_HSINT#
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_HSINT#
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i# = I# (indexWord8ArrayAsInt# ba# i#)
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# = I# (indexIntArray# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# = I# (indexIntOffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readByteOffMutableByteArray# mba# i# s = case readWord8ArrayAsInt# mba# i# s of
                                             (# s', a# #) -> (# s', I# a# #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readMutableByteArray# mba# i# s = case readIntArray# mba# i# s of
                                      (# s', a# #) -> (# s', I# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readIntOffAddr# mba# i# s of
                             (# s', a# #) -> (# s', I# a# #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i# (I# a#) = writeWord8ArrayAsInt# mba# i# a#
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# (I# a#) = writeIntArray# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (I# a#) = writeIntOffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
#if WORD_SIZE_IN_BITS >= 64
  setByteOffMutableByteArray# mba# o# n# (I# a#) = setByteOffMutableByteArray# mba# o# n# (I64# a#)
  setAddr# addr# n# (I# a#) = setAddr# addr# n# (I64# a#)
#else
  setByteOffMutableByteArray# mba# o# n# (I# a#) = setByteOffMutableByteArray# mba# o# n# (I32# a#)
  setAddr# addr# n# (I# a#) = setAddr# addr# n# (I32# a#)
#endif
  {-# INLINE setByteOffMutableByteArray# #-}
  {-# INLINE setAddr# #-}

instance Unbox Int8 where
  type UnboxIso Int8 = Int8
  type SizeOf Int8 = SIZEOF_INT8
  type Alignment Int8 = ALIGNMENT_INT8
  sizeOf# _ = SIZEOF_INT8#
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_INT8#
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i# = I8# (indexInt8Array# ba# i#)
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# = I8# (indexInt8Array# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# = I8# (indexInt8OffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  indexByteOffAddr# addr# i# = I8# (indexInt8OffAddr# addr# i#)
  {-# INLINE indexByteOffAddr# #-}
  readByteOffMutableByteArray# mba# i# s = case readInt8Array# mba# i# s of
                                             (# s', a# #) -> (# s', I8# a# #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readMutableByteArray# mba# i# s = case readInt8Array# mba# i# s of
                                      (# s', a# #) -> (# s', I8# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readInt8OffAddr# mba# i# s of
                             (# s', a# #) -> (# s', I8# a# #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i# (I8# a#) = writeInt8Array# mba# i# a#
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# (I8# a#) = writeInt8Array# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (I8# a#) = writeInt8OffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
  setByteOffMutableByteArray# mba# i# n# (I8# a#) = setByteArray# mba# i# n# a#
  {-# INLINE setByteOffMutableByteArray# #-}
  setAddr# addr# n# (I8# a#) = setInt8Addr# addr# n# a#
  {-# INLINE setAddr# #-}

instance Unbox Int16 where
  type UnboxIso Int16 = Int16
  type SizeOf Int16 = SIZEOF_INT16
  type Alignment Int16 = ALIGNMENT_INT16
  sizeOf# _ = SIZEOF_INT16#
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_INT16#
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i# = I16# (indexWord8ArrayAsInt16# ba# i#)
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# = I16# (indexInt16Array# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# = I16# (indexInt16OffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readByteOffMutableByteArray# mba# i# s = case readWord8ArrayAsInt16# mba# i# s of
                                             (# s', a# #) -> (# s', I16# a# #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readMutableByteArray# mba# i# s = case readInt16Array# mba# i# s of
                                      (# s', a# #) -> (# s', I16# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readInt16OffAddr# mba# i# s of
                             (# s', a# #) -> (# s', I16# a# #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i# (I16# a#) = writeWord8ArrayAsInt16# mba# i# a#
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# (I16# a#) = writeInt16Array# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (I16# a#) = writeInt16OffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
  setByteOffMutableByteArray# mba# o# n# (I16# a#) = setWord8ArrayAsInt16# mba# o# n# a#
  {-# INLINE setByteOffMutableByteArray# #-}
  setAddr# addr# n# (I16# a#) = setInt16Addr# addr# n# a#
  {-# INLINE setAddr# #-}

instance Unbox Int32 where
  type UnboxIso Int32 = Int32
  type SizeOf Int32 = SIZEOF_INT32
  type Alignment Int32 = ALIGNMENT_INT32
  sizeOf# _ = SIZEOF_INT32#
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_INT32#
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i# = I32# (indexWord8ArrayAsInt32# ba# i#)
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# = I32# (indexInt32Array# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# = I32# (indexInt32OffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readByteOffMutableByteArray# mba# i# s = case readWord8ArrayAsInt32# mba# i# s of
                                             (# s', a# #) -> (# s', I32# a# #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readMutableByteArray# mba# i# s = case readInt32Array# mba# i# s of
                                      (# s', a# #) -> (# s', I32# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readInt32OffAddr# mba# i# s of
                             (# s', a# #) -> (# s', I32# a# #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i# (I32# a#) = writeWord8ArrayAsInt32# mba# i# a#
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# (I32# a#) = writeInt32Array# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (I32# a#) = writeInt32OffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
  setByteOffMutableByteArray# mba# o# n# (I32# a#) = setWord8ArrayAsInt32# mba# o# n# a#
  {-# INLINE setByteOffMutableByteArray# #-}
  setAddr# addr# n# (I32# a#) = setInt32Addr# addr# n# a#
  {-# INLINE setAddr# #-}

instance Unbox Int64 where
  type UnboxIso Int64 = Int64
  type SizeOf Int64 = SIZEOF_INT64
  type Alignment Int64 = ALIGNMENT_INT64
  sizeOf# _ = SIZEOF_INT64#
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_INT64#
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i# = I64# (indexWord8ArrayAsInt64# ba# i#)
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# = I64# (indexInt64Array# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# = I64# (indexInt64OffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readByteOffMutableByteArray# mba# i# s = case readWord8ArrayAsInt64# mba# i# s of
                                             (# s', a# #) -> (# s', I64# a# #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readMutableByteArray# mba# i# s = case readInt64Array# mba# i# s of
                                      (# s', a# #) -> (# s', I64# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readInt64OffAddr# mba# i# s of
                             (# s', a# #) -> (# s', I64# a# #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i# (I64# a#) = writeWord8ArrayAsInt64# mba# i# a#
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# (I64# a#) = writeInt64Array# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (I64# a#) = writeInt64OffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
  setByteOffMutableByteArray# mba# o# n# (I64# a#) = setWord8ArrayAsInt64# mba# o# n# a#
  {-# INLINE setByteOffMutableByteArray# #-}
  setAddr# addr# n# (I64# a#) = setInt64Addr# addr# n# a#
  {-# INLINE setAddr# #-}


instance Unbox Word where
  type UnboxIso Word = Word
  type SizeOf Word = SIZEOF_HSWORD
  type Alignment Word = ALIGNMENT_HSWORD
  sizeOf# _ = SIZEOF_HSWORD#
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_HSWORD#
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i# = W# (indexWord8ArrayAsWord# ba# i#)
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# = W# (indexWordArray# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# = W# (indexWordOffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readByteOffMutableByteArray# mba# i# s = case readWord8ArrayAsWord# mba# i# s of
                                             (# s', a# #) -> (# s', W# a# #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readMutableByteArray# mba# i# s = case readWordArray# mba# i# s of
                                      (# s', a# #) -> (# s', W# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readWordOffAddr# mba# i# s of
                             (# s', a# #) -> (# s', W# a# #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i# (W# a#) = writeWord8ArrayAsWord# mba# i# a#
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# (W# a#) = writeWordArray# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (W# a#) = writeWordOffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
#if WORD_SIZE_IN_BITS >= 64
  setByteOffMutableByteArray# mba# o# n# (W# a#) = setByteOffMutableByteArray# mba# o# n# (W64# a#)
  setAddr# addr# n# (W# a#) = setAddr# addr# n# (W64# a#)
#else
  setByteOffMutableByteArray# mba# o# n# (W# a#) = setByteOffMutableByteArray# mba# o# n# (W32# a#)
  setAddr# addr# n# (W# a#) = setAddr# addr# n# (W32# a#)
#endif
  {-# INLINE setByteOffMutableByteArray# #-}
  {-# INLINE setAddr# #-}

instance Unbox Word8 where
  type UnboxIso Word8 = Word8
  type SizeOf Word8 = SIZEOF_WORD8
  type Alignment Word8 = ALIGNMENT_WORD8
  sizeOf# _ = SIZEOF_WORD8#
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_WORD8#
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i# = W8# (indexWord8Array# ba# i#)
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# = W8# (indexWord8Array# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# = W8# (indexWord8OffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  indexByteOffAddr# addr# i# = W8# (indexWord8OffAddr# addr# i#)
  {-# INLINE indexByteOffAddr# #-}
  readByteOffMutableByteArray# mba# i# s = case readWord8Array# mba# i# s of
                                             (# s', a# #) -> (# s', W8# a# #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readMutableByteArray# mba# i# s = case readWord8Array# mba# i# s of
                                      (# s', a# #) -> (# s', W8# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readWord8OffAddr# mba# i# s of
                             (# s', a# #) -> (# s', W8# a# #)
  {-# INLINE readOffAddr# #-}
  readByteOffAddr# mba# i# s = case readWord8OffAddr# mba# i# s of
                                 (# s', a# #) -> (# s', W8# a# #)
  {-# INLINE readByteOffAddr# #-}
  writeByteOffMutableByteArray# mba# i# (W8# a#) = writeWord8Array# mba# i# a#
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# (W8# a#) = writeWord8Array# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (W8# a#) = writeWord8OffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
  writeByteOffAddr# mba# i# (W8# a#) = writeWord8OffAddr# mba# i# a#
  {-# INLINE writeByteOffAddr# #-}
  setByteOffMutableByteArray# mba# i# n# (W8# a#) = setByteArray# mba# i# n# (word2Int# a#)
  {-# INLINE setByteOffMutableByteArray# #-}
  setAddr# addr# n# (W8# a#) = setWord8Addr# addr# n# a#
  {-# INLINE setAddr# #-}

instance Unbox Word16 where
  type UnboxIso Word16 = Word16
  type SizeOf Word16 = SIZEOF_WORD16
  type Alignment Word16 = ALIGNMENT_WORD16
  sizeOf# _ = SIZEOF_WORD16#
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_WORD16#
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i# = W16# (indexWord8ArrayAsWord16# ba# i#)
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# = W16# (indexWord16Array# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# = W16# (indexWord16OffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readByteOffMutableByteArray# mba# i# s = case readWord8ArrayAsWord16# mba# i# s of
                                             (# s', a# #) -> (# s', W16# a# #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readMutableByteArray# mba# i# s = case readWord16Array# mba# i# s of
                                      (# s', a# #) -> (# s', W16# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readWord16OffAddr# mba# i# s of
                             (# s', a# #) -> (# s', W16# a# #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i# (W16# a#) = writeWord8ArrayAsWord16# mba# i# a#
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# (W16# a#) = writeWord16Array# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (W16# a#) = writeWord16OffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
  setByteOffMutableByteArray# mba# o# n# (W16# a#) = setWord8ArrayAsWord16# mba# o# n# a#
  {-# INLINE setByteOffMutableByteArray# #-}
  setAddr# addr# n# (W16# a#) = setWord16Addr# addr# n# a#
  {-# INLINE setAddr# #-}

instance Unbox Word32 where
  type UnboxIso Word32 = Word32
  type SizeOf Word32 = SIZEOF_WORD32
  type Alignment Word32 = ALIGNMENT_WORD32
  sizeOf# _ = SIZEOF_WORD32#
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_WORD32#
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i# = W32# (indexWord8ArrayAsWord32# ba# i#)
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# = W32# (indexWord32Array# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# = W32# (indexWord32OffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readByteOffMutableByteArray# mba# i# s = case readWord8ArrayAsWord32# mba# i# s of
                                             (# s', a# #) -> (# s', W32# a# #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readMutableByteArray# mba# i# s = case readWord32Array# mba# i# s of
                                      (# s', a# #) -> (# s', W32# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readWord32OffAddr# mba# i# s of
                             (# s', a# #) -> (# s', W32# a# #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i# (W32# a#) = writeWord8ArrayAsWord32# mba# i# a#
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# (W32# a#) = writeWord32Array# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (W32# a#) = writeWord32OffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
  setByteOffMutableByteArray# mba# o# n# (W32# a#) = setWord8ArrayAsWord32# mba# o# n# a#
  {-# INLINE setByteOffMutableByteArray# #-}
  setAddr# addr# n# (W32# a#) = setWord32Addr# addr# n# a#
  {-# INLINE setAddr# #-}

instance Unbox Word64 where
  type UnboxIso Word64 = Word64
  type SizeOf Word64 = SIZEOF_WORD64
  type Alignment Word64 = ALIGNMENT_WORD64
  sizeOf# _ = SIZEOF_WORD64#
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_WORD64#
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i# = W64# (indexWord8ArrayAsWord64# ba# i#)
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# = W64# (indexWord64Array# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# = W64# (indexWord64OffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readByteOffMutableByteArray# mba# i# s = case readWord8ArrayAsWord64# mba# i# s of
                                             (# s', a# #) -> (# s', W64# a# #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readMutableByteArray# mba# i# s = case readWord64Array# mba# i# s of
                                      (# s', a# #) -> (# s', W64# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readWord64OffAddr# mba# i# s of
                             (# s', a# #) -> (# s', W64# a# #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i# (W64# a#) = writeWord8ArrayAsWord64# mba# i# a#
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# (W64# a#) = writeWord64Array# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (W64# a#) = writeWord64OffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
  setByteOffMutableByteArray# mba# o# n# (W64# a#) = setWord8ArrayAsWord64# mba# o# n# a#
  {-# INLINE setByteOffMutableByteArray# #-}
  setAddr# addr# n# (W64# a#) = setWord64Addr# addr# n# a#
  {-# INLINE setAddr# #-}


instance Unbox Float where
  type UnboxIso Float = Float
  type SizeOf Float = SIZEOF_FLOAT
  type Alignment Float = ALIGNMENT_FLOAT
  sizeOf# _ = SIZEOF_FLOAT#
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_FLOAT#
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i# = F# (indexWord8ArrayAsFloat# ba# i#)
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# = F# (indexFloatArray# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# = F# (indexFloatOffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readByteOffMutableByteArray# mba# i# s = case readWord8ArrayAsFloat# mba# i# s of
                                             (# s', a# #) -> (# s', F# a# #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readMutableByteArray# mba# i# s = case readFloatArray# mba# i# s of
                                      (# s', a# #) -> (# s', F# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readFloatOffAddr# mba# i# s of
                             (# s', a# #) -> (# s', F# a# #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i# (F# a#) = writeWord8ArrayAsFloat# mba# i# a#
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# (F# a#) = writeFloatArray# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (F# a#) = writeFloatOffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
  setByteOffMutableByteArray# mba# o# n# (F# f#) =
    setWord8ArrayAsWord32# mba# o# n# (floatToWord32# f#)
  {-# INLINE setByteOffMutableByteArray# #-}
  setAddr# addr# n# (F# f#) = setWord32Addr# addr# n# (floatToWord32# f#)
  {-# INLINE setAddr# #-}

instance Unbox Double where
  type UnboxIso Double = Double
  type SizeOf Double = SIZEOF_DOUBLE
  type Alignment Double = ALIGNMENT_DOUBLE
  sizeOf# _ = SIZEOF_DOUBLE#
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_DOUBLE#
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i# = D# (indexWord8ArrayAsDouble# ba# i#)
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# = D# (indexDoubleArray# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# = D# (indexDoubleOffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readByteOffMutableByteArray# mba# i# s = case readWord8ArrayAsDouble# mba# i# s of
                                             (# s', a# #) -> (# s', D# a# #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readMutableByteArray# mba# i# s = case readDoubleArray# mba# i# s of
                                      (# s', a# #) -> (# s', D# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readDoubleOffAddr# mba# i# s of
                             (# s', a# #) -> (# s', D# a# #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i# (D# a#) = writeWord8ArrayAsDouble# mba# i# a#
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# (D# a#) = writeDoubleArray# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (D# a#) = writeDoubleOffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
  setByteOffMutableByteArray# mba# o# n# (D# d#) =
    setWord8ArrayAsWord64# mba# o# n# (doubleToWord64# d#)
  {-# INLINE setByteOffMutableByteArray# #-}
  setAddr# addr# n# (D# d#) = setWord64Addr# addr# n# (doubleToWord64# d#)
  {-# INLINE setAddr# #-}

instance Unbox Char where
  type UnboxIso Char = Char
  type SizeOf Char = SIZEOF_HSCHAR
  type Alignment Char = ALIGNMENT_HSCHAR
  sizeOf# _ = SIZEOF_HSCHAR#
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_HSCHAR#
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i# = C# (indexWord8ArrayAsWideChar# ba# i#)
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# = C# (indexWideCharArray# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# = C# (indexWideCharOffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readByteOffMutableByteArray# mba# i# s = case readWord8ArrayAsWideChar# mba# i# s of
                                             (# s', a# #) -> (# s', C# a# #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readMutableByteArray# mba# i# s = case readWideCharArray# mba# i# s of
                                      (# s', a# #) -> (# s', C# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readWideCharOffAddr# mba# i# s of
                             (# s', a# #) -> (# s', C# a# #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i# (C# a#) = writeWord8ArrayAsWideChar# mba# i# a#
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# (C# a#) = writeWideCharArray# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (C# a#) = writeWideCharOffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
  setByteOffMutableByteArray# mba# o# n# (C# a#) =
    setByteOffMutableByteArray# mba# o# n# (I32# (ord# a#))
  {-# INLINE setByteOffMutableByteArray# #-}
  setAddr# addr# n# (C# a#) = setAddr# addr# n# (I32# (ord# a#))
  {-# INLINE setAddr# #-}

instance Unbox (Ptr a) where
  type UnboxIso (Ptr a) = Ptr a
  type SizeOf (Ptr a) = SIZEOF_HSPTR
  type Alignment (Ptr a) = ALIGNMENT_HSPTR
  sizeOf# _ = SIZEOF_HSPTR#
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_HSPTR#
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i# = Ptr (indexWord8ArrayAsAddr# ba# i#)
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# = Ptr (indexAddrArray# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# = Ptr (indexAddrOffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readByteOffMutableByteArray# mba# i# s = case readWord8ArrayAsAddr# mba# i# s of
                                             (# s', a# #) -> (# s', Ptr a# #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readMutableByteArray# mba# i# s = case readAddrArray# mba# i# s of
                                      (# s', a# #) -> (# s', Ptr a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readAddrOffAddr# mba# i# s of
                             (# s', a# #) -> (# s', Ptr a# #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i# (Ptr a#) = writeWord8ArrayAsAddr# mba# i# a#
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# (Ptr a#) = writeAddrArray# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (Ptr a#) = writeAddrOffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
#if SIZEOF_HSPTR == SIZEOF_INT64
  setByteOffMutableByteArray# mba# o# n# (Ptr a#) =
    setByteOffMutableByteArray# mba# o# n# (I64# (addr2Int# a#))
  setAddr# addr# n# (Ptr a#) = setAddr# addr# n# (I64# (addr2Int# a#))
#elif SIZEOF_HSPTR == SIZEOF_INT32
  setByteOffMutableByteArray# mba# o# n# (Ptr a#) =
    setByteOffMutableByteArray# mba# o# n# (I32# (addr2Int# a#))
  setAddr# addr# n# (Ptr a#) = setAddr# addr# n# (I32# (addr2Int# a#))
#else
#error Ptr is of unsupported size SIZEOF_HSPTR
#endif
  {-# INLINE setByteOffMutableByteArray# #-}
  {-# INLINE setAddr# #-}


instance Unbox (StablePtr a) where
  type UnboxIso (StablePtr a) = StablePtr a
  type SizeOf (StablePtr a) = SIZEOF_HSSTABLEPTR
  type Alignment (StablePtr a) = ALIGNMENT_HSSTABLEPTR
  sizeOf# _ = SIZEOF_HSSTABLEPTR#
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_HSSTABLEPTR#
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i# = StablePtr (indexWord8ArrayAsStablePtr# ba# i#)
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# = StablePtr (indexStablePtrArray# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# = StablePtr (indexStablePtrOffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readByteOffMutableByteArray# mba# i# s = case readWord8ArrayAsStablePtr# mba# i# s of
                                             (# s', a# #) -> (# s', StablePtr a# #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readMutableByteArray# mba# i# s = case readStablePtrArray# mba# i# s of
                                      (# s', a# #) -> (# s', StablePtr a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readStablePtrOffAddr# mba# i# s of
                             (# s', a# #) -> (# s', StablePtr a# #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i# (StablePtr a#) = writeWord8ArrayAsStablePtr# mba# i# a#
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# (StablePtr a#) = writeStablePtrArray# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (StablePtr a#) = writeStablePtrOffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
#if SIZEOF_HSSTABLEPTR == SIZEOF_INT64
  setByteOffMutableByteArray# mba# o# n# (StablePtr a#) =
    setByteOffMutableByteArray# mba# o# n# (I64# (unsafeCoerce# a#))
  setAddr# addr# n# (StablePtr a#) = setAddr# addr# n# (I64# (unsafeCoerce# a#))
#elif SIZEOF_HSSTABLEPTR == SIZEOF_INT32
  setByteOffMutableByteArray# mba# o# n# (StablePtr a#) =
    setByteOffMutableByteArray# mba# o# n# (I32# (unsafeCoerce# a#))
  setAddr# addr# n# (StablePtr a#) = setAddr# addr# n# (I32# (unsafeCoerce# a#))
#else
#error StablePtr is of unsupported size SIZEOF_HSSTABLEPTR
#endif
  {-# INLINE setByteOffMutableByteArray# #-}
  {-# INLINE setAddr# #-}

