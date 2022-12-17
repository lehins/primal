{-# LANGUAGE MagicHash #-}
{-# OPTIONS_HADDOCK print-explicit-runtime-reps #-}

-- |
-- Module      : Primal.Ops.ByteArray
-- Copyright   : (c) Alexey Kuleshevich 2022-2023
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
--
-- A @`ByteArray#`@ and @`MutableByteArray#`@ is a region of raw memory in the
-- garbage-collected heap, which is not scanned for pointers.  There are three sets of
-- operations for accessing byte array contents: index for reading from immutable byte
-- arrays, and read\/write for mutable byte arrays.  Each set contains operations for a
-- range of useful primitive data types.  Each operation takes an offset measured in terms
-- of the size of the primitive type being read or written.
module Primal.Ops.ByteArray (
  -- * Byte Arrays

  -- ** Immutable Byte Array
  ByteArray#,
  byteArrayContents#,
  sizeofByteArray#,
  compareByteArrays#,

  -- *** Indexing
  indexCharArray#,
  indexWideCharArray#,
  indexIntArray#,
  indexWordArray#,
  indexAddrArray#,
  indexFloatArray#,
  indexDoubleArray#,
  indexStablePtrArray#,
  indexInt8Array#,
  indexInt16Array#,
  indexInt32Array#,
  indexInt64Array#,
  indexWord8Array#,
  indexWord16Array#,
  indexWord32Array#,
  indexWord64Array#,
  indexWord8ArrayAsChar#,
  indexWord8ArrayAsWideChar#,
  indexWord8ArrayAsInt#,
  indexWord8ArrayAsWord#,
  indexWord8ArrayAsAddr#,
  indexWord8ArrayAsFloat#,
  indexWord8ArrayAsDouble#,
  indexWord8ArrayAsStablePtr#,
  indexWord8ArrayAsInt16#,
  indexWord8ArrayAsInt32#,
  indexWord8ArrayAsInt64#,
  indexWord8ArrayAsWord16#,
  indexWord8ArrayAsWord32#,
  indexWord8ArrayAsWord64#,

  -- ** Mutable Byte Array
  MutableByteArray#,
  -- mutableByteArrayContents#,
  getSizeofMutableByteArray#,
  newByteArray#,
  newPinnedByteArray#,
  newAlignedPinnedByteArray#,
  isMutableByteArrayPinned#,
  isByteArrayPinned#,
  shrinkMutableByteArray#,
  resizeMutableByteArray#,
  unsafeFreezeByteArray#,

  -- *** Reading
  readCharArray#,
  readWideCharArray#,
  readIntArray#,
  readWordArray#,
  readAddrArray#,
  readFloatArray#,
  readDoubleArray#,
  readStablePtrArray#,
  readInt8Array#,
  readInt16Array#,
  readInt32Array#,
  readInt64Array#,
  readWord8Array#,
  readWord16Array#,
  readWord32Array#,
  readWord64Array#,
  readWord8ArrayAsChar#,
  readWord8ArrayAsWideChar#,
  readWord8ArrayAsInt#,
  readWord8ArrayAsWord#,
  readWord8ArrayAsAddr#,
  readWord8ArrayAsFloat#,
  readWord8ArrayAsDouble#,
  readWord8ArrayAsStablePtr#,
  readWord8ArrayAsInt16#,
  readWord8ArrayAsInt32#,
  readWord8ArrayAsInt64#,
  readWord8ArrayAsWord16#,
  readWord8ArrayAsWord32#,
  readWord8ArrayAsWord64#,

  -- *** Writing
  writeCharArray#,
  writeWideCharArray#,
  writeIntArray#,
  writeWordArray#,
  writeAddrArray#,
  writeFloatArray#,
  writeDoubleArray#,
  writeStablePtrArray#,
  writeInt8Array#,
  writeInt16Array#,
  writeInt32Array#,
  writeInt64Array#,
  writeWord8Array#,
  writeWord16Array#,
  writeWord32Array#,
  writeWord64Array#,
  writeWord8ArrayAsChar#,
  writeWord8ArrayAsWideChar#,
  writeWord8ArrayAsInt#,
  writeWord8ArrayAsWord#,
  writeWord8ArrayAsAddr#,
  writeWord8ArrayAsFloat#,
  writeWord8ArrayAsDouble#,
  writeWord8ArrayAsStablePtr#,
  writeWord8ArrayAsInt16#,
  writeWord8ArrayAsInt32#,
  writeWord8ArrayAsInt64#,
  writeWord8ArrayAsWord16#,
  writeWord8ArrayAsWord32#,
  writeWord8ArrayAsWord64#,

  -- *** Bulk writing
  setByteArray#,
  copyByteArray#,
  copyMutableByteArray#,
  copyByteArrayToAddr#,
  copyMutableByteArrayToAddr#,
  copyAddrToByteArray#,

  -- *** Atomic
  -- atomicReadIntArray#,
  -- atomicWriteIntArray#,
  -- casIntArray#,
  -- casInt8Array#,
  -- casInt16Array#,
  -- casInt32Array#,
  -- casInt64Array#,
  -- fetchAddIntArray#,
  -- fetchSubIntArray#,
  -- fetchAndIntArray#,
  -- fetchNandIntArray#,
  -- fetchOrIntArray#,
  -- fetchXorIntArray#,
) where

import GHC.Exts
