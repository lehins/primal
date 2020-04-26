{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
-- |
-- Module      : Foreign.Prim.C
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Foreign.Prim.C
  ( isSameByteArray#
  , isSameMutableByteArray#
  , toOrdering#
  , memcmpAddr#
  , memcmpByteArray#

  , memsetWord8MutableByteArray#
  , memsetWord8Addr#
  , memsetInt8MutableByteArray#
  , memsetInt8Addr#
  , memsetWord16MutableByteArray#
  , memsetWord16Addr#
  , memsetInt16MutableByteArray#
  , memsetInt16Addr#
  , memsetWord32MutableByteArray#
  , memsetWord32Addr#
  , memsetInt32MutableByteArray#
  , memsetInt32Addr#
  , memsetWord64MutableByteArray#
  , memsetWord64Addr#
  , memsetInt64MutableByteArray#
  , memsetInt64Addr#

  , memmoveAddr#
  , memmoveMutableByteArray#
  , memmoveMutableByteArrayToAddr#
  , memmoveMutableByteArrayFromAddr#

  , compareByteArrays#

  , getSizeofMutableByteArray#
  , isByteArrayPinned#
  , isMutableByteArrayPinned#

  , indexWord8ArrayAsChar#
  , readWord8ArrayAsChar#
  , writeWord8ArrayAsChar#
  , indexWord8ArrayAsWideChar#
  , readWord8ArrayAsWideChar#
  , writeWord8ArrayAsWideChar#
  , indexWord8ArrayAsAddr#
  , readWord8ArrayAsAddr#
  , writeWord8ArrayAsAddr#
  , indexWord8ArrayAsStablePtr#
  , readWord8ArrayAsStablePtr#
  , writeWord8ArrayAsStablePtr#
  , indexWord8ArrayAsFloat#
  , readWord8ArrayAsFloat#
  , writeWord8ArrayAsFloat#
  , indexWord8ArrayAsDouble#
  , readWord8ArrayAsDouble#
  , writeWord8ArrayAsDouble#
  , indexWord8ArrayAsInt16#
  , readWord8ArrayAsInt16#
  , writeWord8ArrayAsInt16#
  , indexWord8ArrayAsInt32#
  , readWord8ArrayAsInt32#
  , writeWord8ArrayAsInt32#
  , indexWord8ArrayAsInt64#
  , readWord8ArrayAsInt64#
  , writeWord8ArrayAsInt64#
  , indexWord8ArrayAsInt#
  , readWord8ArrayAsInt#
  , writeWord8ArrayAsInt#
  , indexWord8ArrayAsWord16#
  , readWord8ArrayAsWord16#
  , writeWord8ArrayAsWord16#
  , indexWord8ArrayAsWord32#
  , readWord8ArrayAsWord32#
  , writeWord8ArrayAsWord32#
  , indexWord8ArrayAsWord64#
  , readWord8ArrayAsWord64#
  , writeWord8ArrayAsWord64#
  , indexWord8ArrayAsWord#
  , readWord8ArrayAsWord#
  , writeWord8ArrayAsWord#

  , CBool(..)
  , module Foreign.C.Types
  , module GHC.Exts
  ) where

import GHC.Exts
import GHC.Int
import GHC.Word
import Foreign.C.Types
#if __GLASGOW_HASKELL__ < 806
import Foreign.Storable
import Foreign.Prim.StablePtr
import Control.Prim.Monad.Unsafe
#endif

#include "MachDeps.h"
#include "HsBaseConfig.h"
#include "prim_core.h"


-- | Because GC is guaranteed not to move unpinned memory during the unsafe FFI call we
-- can compare memory pointers on the C side. Because the addresses cannot change
-- underneath us we can safely guarantee pointer equality for the same pinned or unpinned
-- arrays

foreign import ccall unsafe "prim_core.c prim_core_ptreq"
  isSameByteArray# :: ByteArray# -> ByteArray# -> Int#

foreign import ccall unsafe "prim_core.c prim_core_ptreq"
  isSameMutableByteArray# :: MutableByteArray# s -> MutableByteArray# s -> Int#

-- | Convert memcmp result into an ordering
toOrdering# :: Int# -> Ordering
toOrdering# =
  \case
    0# -> EQ
    n# ->
      if isTrue# (n# <# 0#)
        then LT
        else GT

foreign import ccall unsafe "prim_core.c prim_core_memcmp"
  memcmpAddr# :: Addr# -> Int# -> Addr# -> Int# -> Int# -> Int#
foreign import ccall unsafe "prim_core.c prim_core_memcmp"
  memcmpByteArray# :: ByteArray# -> Int# -> ByteArray# -> Int# -> Int# -> Int#

foreign import ccall unsafe "prim_core.c prim_core_memset8"
  memsetInt8MutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> Int8 -> IO ()

foreign import ccall unsafe "prim_core.c prim_core_memset8"
  memsetInt8Addr# :: Addr# -> Int# -> Int# -> Int8 -> IO ()

foreign import ccall unsafe "prim_core.c prim_core_memset8"
  memsetWord8MutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> Word8 -> IO ()

foreign import ccall unsafe "prim_core.c prim_core_memset8"
  memsetWord8Addr# :: Addr# -> Int# -> Int# -> Word8 -> IO ()

foreign import ccall unsafe "prim_core.c prim_core_memset16"
  memsetInt16MutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> Int16 -> IO ()

foreign import ccall unsafe "prim_core.c prim_core_memset16"
  memsetInt16Addr# :: Addr# -> Int# -> Int# -> Int16 -> IO ()

foreign import ccall unsafe "prim_core.c prim_core_memset16"
  memsetWord16MutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> Word16 -> IO ()

foreign import ccall unsafe "prim_core.c prim_core_memset16"
  memsetWord16Addr# :: Addr# -> Int# -> Int# -> Word16 -> IO ()


foreign import ccall unsafe "prim_core.c prim_core_memset32"
  memsetInt32MutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> Int32 -> IO ()

foreign import ccall unsafe "prim_core.c prim_core_memset32"
  memsetInt32Addr# :: Addr# -> Int# -> Int# -> Int32 -> IO ()

foreign import ccall unsafe "prim_core.c prim_core_memset32"
  memsetWord32MutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> Word32 -> IO ()

foreign import ccall unsafe "prim_core.c prim_core_memset32"
  memsetWord32Addr# :: Addr# -> Int# -> Int# -> Word32 -> IO ()


foreign import ccall unsafe "prim_core.c prim_core_memset64"
  memsetInt64MutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> Int64 -> IO ()

foreign import ccall unsafe "prim_core.c prim_core_memset64"
  memsetInt64Addr# :: Addr# -> Int# -> Int# -> Int64 -> IO ()

foreign import ccall unsafe "prim_core.c prim_core_memset64"
  memsetWord64MutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> Word64 -> IO ()

foreign import ccall unsafe "prim_core.c prim_core_memset64"
  memsetWord64Addr# :: Addr# -> Int# -> Int# -> Word64 -> IO ()

foreign import ccall unsafe "prim_core.c prim_core_memmove"
  memmoveAddr# :: Addr# -- ^ Source ptr
               -> Int# -- ^ Offset in bytes into source array
               -> Addr# -- ^ Destination ptr
               -> Int# -- ^ Offset in bytes into destination
               -> Int# -- ^ Number of bytes to copy
               -> IO ()
foreign import ccall unsafe "prim_core.c prim_core_memmove"
  memmoveMutableByteArray# :: MutableByteArray# s -- ^ Source array
                           -> Int# -- ^ Offset in bytes into source array
                           -> MutableByteArray# s -- ^ Destination
                           -> Int# -- ^ Offset in bytes into destination
                           -> Int# -- ^ Number of bytes to copy
                           -> IO ()
foreign import ccall unsafe "prim_core.c prim_core_memmove"
  memmoveMutableByteArrayToAddr# :: MutableByteArray# s -- ^ Source array
                                 -> Int# -- ^ Offset in bytes into source array
                                 -> Addr# -- ^ Destination ptr
                                 -> Int# -- ^ Offset in bytes into destination
                                 -> Int# -- ^ Number of bytes to copy
                                 -> IO ()
foreign import ccall unsafe "prim_core.c prim_core_memmove"
  memmoveMutableByteArrayFromAddr# :: Addr# -- ^ Source Ptr
                                   -> Int# -- ^ Offset in bytes into source array
                                   -> MutableByteArray# s -- ^ Destination
                                   -> Int# -- ^ Offset in bytes into destination
                                   -> Int# -- ^ Number of bytes to copy
                                   -> IO ()


-- ghc-8.6 (i.e. 806 version) introduced these new functions, for versions before we
-- use their re-implementations in C:
#if __GLASGOW_HASKELL__ < 806
indexWord8ArrayAsChar# :: ByteArray# -> Int# -> Char#
indexWord8ArrayAsChar# = indexCharArray#

readWord8ArrayAsChar# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Char# #)
readWord8ArrayAsChar# = readCharArray#

writeWord8ArrayAsChar# :: MutableByteArray# d -> Int# -> Char# -> State# d -> State# d
writeWord8ArrayAsChar# = writeCharArray#

foreign import ccall unsafe "prim_core.c prim_core_memread32"
  indexWord8ArrayAsWideChar# :: ByteArray# -> Int# -> Char#

foreign import ccall unsafe "prim_core.c prim_core_memread32"
  readWord8ArrayAsWideCharIO# :: MutableByteArray# d -> Int# -> IO Char

readWord8ArrayAsWideChar# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Char# #)
readWord8ArrayAsWideChar# mb# i# s =
  case unsafePrimBase (readWord8ArrayAsWideCharIO# mb# i#) s of
    (# s', C# c# #) -> (# s', c# #)

foreign import ccall unsafe "prim_core.c prim_core_memwrite32"
  writeWord8ArrayAsWideCharIO# :: MutableByteArray# d -> Int# -> Char# -> IO ()

writeWord8ArrayAsWideChar# :: MutableByteArray# d -> Int# -> Char# -> State# d -> State# d
writeWord8ArrayAsWideChar# mb# i# c# = unsafePrimBase_ (writeWord8ArrayAsWideCharIO# mb# i# c#)

-- Addr#

#if SIZEOF_HSPTR == SIZEOF_INT64
foreign import ccall unsafe "prim_core.c prim_core_memread64"
  indexWord8ArrayAsAddr# :: ByteArray# -> Int# -> Addr#

foreign import ccall unsafe "prim_core.c prim_core_memread64"
  readWord8ArrayAsPtrIO# :: MutableByteArray# d -> Int# -> IO (Ptr a)

foreign import ccall unsafe "prim_core.c prim_core_memwrite64"
  writeWord8ArrayAsAddrIO# :: MutableByteArray# d -> Int# -> Addr# -> IO ()


#elif SIZEOF_HSPTR == SIZEOF_INT32
foreign import ccall unsafe "prim_core.c prim_core_memread32"
  indexWord8ArrayAsAddr# :: ByteArray# -> Int# -> Addr#

foreign import ccall unsafe "prim_core.c prim_core_memread32"
  readWord8ArrayAsPtrIO# :: MutableByteArray# d -> Int# -> IO (Ptr a)

foreign import ccall unsafe "prim_core.c prim_core_memwrite32
  writeWord8ArrayAsAddrIO# :: MutableByteArray# d -> Int# -> Addr# -> IO ()
#else
#error Ptr is of unsupported size SIZEOF_HSPTR
#endif

readWord8ArrayAsAddr# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Addr# #)
readWord8ArrayAsAddr# mb# i# s =
  case unsafePrimBase (readWord8ArrayAsPtrIO# mb# i#) s of
    (# s', Ptr addr# #) -> (# s', addr# #)

writeWord8ArrayAsAddr# :: MutableByteArray# d -> Int# -> Addr# -> State# d -> State# d
writeWord8ArrayAsAddr# mb# i# addr# = unsafePrimBase_ (writeWord8ArrayAsAddrIO# mb# i# addr#)

-- StablePtr#

#if SIZEOF_HSSTABLEPTR == SIZEOF_INT64
foreign import ccall unsafe "prim_core.c prim_core_memread64"
  indexWord8ArrayAsStablePtr# :: ByteArray# -> Int# -> StablePtr# a

foreign import ccall unsafe "prim_core.c prim_core_memread64"
  readWord8ArrayAsStablePtrIO# :: MutableByteArray# d -> Int# -> IO (StablePtr a)

foreign import ccall unsafe "prim_core.c prim_core_memwrite64"
  writeWord8ArrayAsStablePtrIO# :: MutableByteArray# d -> Int# -> StablePtr# a -> IO ()
#elif SIZEOF_HSSTABLEPTR == SIZEOF_INT32
foreign import ccall unsafe "prim_core.c prim_core_memread32"
  indexWord8ArrayAsStablePtr# :: ByteArray# -> Int# -> StablePtr# a

foreign import ccall unsafe "prim_core.c prim_core_memread32"
  readWord8ArrayAsStablePtrIO# :: MutableByteArray# d -> Int# -> IO (StablePtr a)

foreign import ccall unsafe "prim_core.c prim_core_memwrite32
  writeWord8ArrayAsStablePtrIO# :: MutableByteArray# d -> Int# -> StablePtr# a -> IO ()
#else
#error StablePtr is of unsupported size SIZEOF_HSPTR
#endif

readWord8ArrayAsStablePtr# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, StablePtr# a #)
readWord8ArrayAsStablePtr# mb# i# s =
  case unsafePrimBase (readWord8ArrayAsStablePtrIO# mb# i#) s of
    (# s', StablePtr addr# #) -> (# s', addr# #)

writeWord8ArrayAsStablePtr# :: MutableByteArray# d -> Int# -> StablePtr# a -> State# d -> State# d
writeWord8ArrayAsStablePtr# mb# i# addr# = unsafePrimBase_ (writeWord8ArrayAsStablePtrIO# mb# i# addr#)


-- Float#

foreign import ccall unsafe "prim_core.c prim_core_memread32"
  indexWord8ArrayAsFloat# :: ByteArray# -> Int# -> Float#

foreign import ccall unsafe "prim_core.c prim_core_memread32"
  readWord8ArrayAsFloatIO# :: MutableByteArray# d -> Int# -> IO Float

readWord8ArrayAsFloat# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Float# #)
readWord8ArrayAsFloat# mb# i# s =
  case unsafePrimBase (readWord8ArrayAsFloatIO# mb# i#) s of
    (# s', F# a# #) -> (# s', a# #)

foreign import ccall unsafe "prim_core.c prim_core_memwrite32"
  writeWord8ArrayAsFloatIO# :: MutableByteArray# d -> Int# -> Float# -> IO ()

writeWord8ArrayAsFloat# :: MutableByteArray# d -> Int# -> Float# -> State# d -> State# d
writeWord8ArrayAsFloat# mb# i# a# = unsafePrimBase_ (writeWord8ArrayAsFloatIO# mb# i# a#)

-- Double#

foreign import ccall unsafe "prim_core.c prim_core_memread64"
  indexWord8ArrayAsDouble# :: ByteArray# -> Int# -> Double#

foreign import ccall unsafe "prim_core.c prim_core_memread64"
  readWord8ArrayAsDoubleIO# :: MutableByteArray# d -> Int# -> IO Double

readWord8ArrayAsDouble# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Double# #)
readWord8ArrayAsDouble# mb# i# s =
  case unsafePrimBase (readWord8ArrayAsDoubleIO# mb# i#) s of
    (# s', D# a# #) -> (# s', a# #)

foreign import ccall unsafe "prim_core.c prim_core_memwrite64"
  writeWord8ArrayAsDoubleIO# :: MutableByteArray# d -> Int# -> Double# -> IO ()

writeWord8ArrayAsDouble# :: MutableByteArray# d -> Int# -> Double# -> State# d -> State# d
writeWord8ArrayAsDouble# mb# i# a# = unsafePrimBase_ (writeWord8ArrayAsDoubleIO# mb# i# a#)

-- Int16#

foreign import ccall unsafe "prim_core.c prim_core_memread16"
  indexWord8ArrayAsInt16# :: ByteArray# -> Int# -> Int#

foreign import ccall unsafe "prim_core.c prim_core_memread16"
  readWord8ArrayAsInt16IO# :: MutableByteArray# d -> Int# -> IO Int16

readWord8ArrayAsInt16# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Int# #)
readWord8ArrayAsInt16# mb# i# s =
  case unsafePrimBase (readWord8ArrayAsInt16IO# mb# i#) s of
    (# s', I16# a# #) -> (# s', a# #)

foreign import ccall unsafe "prim_core.c prim_core_memwrite16"
  writeWord8ArrayAsInt16IO# :: MutableByteArray# d -> Int# -> Int# -> IO ()

writeWord8ArrayAsInt16# :: MutableByteArray# d -> Int# -> Int# -> State# d -> State# d
writeWord8ArrayAsInt16# mb# i# a# = unsafePrimBase_ (writeWord8ArrayAsInt16IO# mb# i# a#)


-- Int32#

foreign import ccall unsafe "prim_core.c prim_core_memread32"
  indexWord8ArrayAsInt32# :: ByteArray# -> Int# -> Int#

foreign import ccall unsafe "prim_core.c prim_core_memread32"
  readWord8ArrayAsInt32IO# :: MutableByteArray# d -> Int# -> IO Int32

readWord8ArrayAsInt32# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Int# #)
readWord8ArrayAsInt32# mb# i# s =
  case unsafePrimBase (readWord8ArrayAsInt32IO# mb# i#) s of
    (# s', I32# a# #) -> (# s', a# #)

foreign import ccall unsafe "prim_core.c prim_core_memwrite32"
  writeWord8ArrayAsInt32IO# :: MutableByteArray# d -> Int# -> Int# -> IO ()

writeWord8ArrayAsInt32# :: MutableByteArray# d -> Int# -> Int# -> State# d -> State# d
writeWord8ArrayAsInt32# mb# i# a# = unsafePrimBase_ (writeWord8ArrayAsInt32IO# mb# i# a#)


-- Int64#

foreign import ccall unsafe "prim_core.c prim_core_memread64"
  readWord8ArrayAsInt64IO# :: MutableByteArray# d -> Int# -> IO Int64

#if WORD_SIZE_IN_BITS >= 64
foreign import ccall unsafe "prim_core.c prim_core_memread64"
  indexWord8ArrayAsInt64# :: ByteArray# -> Int# -> Int#
foreign import ccall unsafe "prim_core.c prim_core_memwrite64"
  writeWord8ArrayAsInt64IO# :: MutableByteArray# d -> Int# -> Int# -> IO ()

readWord8ArrayAsInt64# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Int# #)
writeWord8ArrayAsInt64# :: MutableByteArray# d -> Int# -> Int# -> State# d -> State# d

#else
foreign import ccall unsafe "prim_core.c prim_core_memread64"
  indexWord8ArrayAsInt64# :: ByteArray# -> Int# -> Int64#
foreign import ccall unsafe "prim_core.c prim_core_memwrite64"
  writeWord8ArrayAsInt64IO# :: MutableByteArray# d -> Int# -> Int64# -> IO ()

readWord8ArrayAsInt64# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Int64# #)
writeWord8ArrayAsInt64# :: MutableByteArray# d -> Int# -> Int64# -> State# d -> State# d
#endif

readWord8ArrayAsInt64# mb# i# s =
  case unsafePrimBase (readWord8ArrayAsInt64IO# mb# i#) s of
    (# s', I64# a# #) -> (# s', a# #)
writeWord8ArrayAsInt64# mb# i# a# = unsafePrimBase_ (writeWord8ArrayAsInt64IO# mb# i# a#)

-- Int#

#if WORD_SIZE_IN_BITS >= 64
foreign import ccall unsafe "prim_core.c prim_core_memread64"
  indexWord8ArrayAsInt# :: ByteArray# -> Int# -> Int#

foreign import ccall unsafe "prim_core.c prim_core_memread64"
  readWord8ArrayAsIntIO# :: MutableByteArray# d -> Int# -> IO Int

foreign import ccall unsafe "prim_core.c prim_core_memwrite64"
  writeWord8ArrayAsIntIO# :: MutableByteArray# d -> Int# -> Int# -> IO ()
#else
foreign import ccall unsafe "prim_core.c prim_core_memread32"
  indexWord8ArrayAsInt# :: ByteArray# -> Int# -> Int#

foreign import ccall unsafe "prim_core.c prim_core_memread32"
  readWord8ArrayAsIntIO# :: MutableByteArray# d -> Int# -> IO Int64

foreign import ccall unsafe "prim_core.c prim_core_memwrite32"
  writeWord8ArrayAsIntIO# :: MutableByteArray# d -> Int# -> Int# -> IO ()
#endif

readWord8ArrayAsInt# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Int# #)
readWord8ArrayAsInt# mb# i# s =
  case unsafePrimBase (readWord8ArrayAsIntIO# mb# i#) s of
    (# s', I# a# #) -> (# s', a# #)

writeWord8ArrayAsInt# :: MutableByteArray# d -> Int# -> Int# -> State# d -> State# d
writeWord8ArrayAsInt# mb# i# a# = unsafePrimBase_ (writeWord8ArrayAsIntIO# mb# i# a#)

-- Word16#

foreign import ccall unsafe "prim_core.c prim_core_memread16"
  indexWord8ArrayAsWord16# :: ByteArray# -> Int# -> Word#

foreign import ccall unsafe "prim_core.c prim_core_memread16"
  readWord8ArrayAsWord16IO# :: MutableByteArray# d -> Int# -> IO Word16

readWord8ArrayAsWord16# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Word# #)
readWord8ArrayAsWord16# mb# i# s =
  case unsafePrimBase (readWord8ArrayAsWord16IO# mb# i#) s of
    (# s', W16# a# #) -> (# s', a# #)

foreign import ccall unsafe "prim_core.c prim_core_memwrite16"
  writeWord8ArrayAsWord16IO# :: MutableByteArray# d -> Int# -> Word# -> IO ()

writeWord8ArrayAsWord16# :: MutableByteArray# d -> Int# -> Word# -> State# d -> State# d
writeWord8ArrayAsWord16# mb# i# a# = unsafePrimBase_ (writeWord8ArrayAsWord16IO# mb# i# a#)


-- Word32#

foreign import ccall unsafe "prim_core.c prim_core_memread32"
  indexWord8ArrayAsWord32# :: ByteArray# -> Int# -> Word#

foreign import ccall unsafe "prim_core.c prim_core_memread32"
  readWord8ArrayAsWord32IO# :: MutableByteArray# d -> Int# -> IO Word32

readWord8ArrayAsWord32# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Word# #)
readWord8ArrayAsWord32# mb# i# s =
  case unsafePrimBase (readWord8ArrayAsWord32IO# mb# i#) s of
    (# s', W32# a# #) -> (# s', a# #)

foreign import ccall unsafe "prim_core.c prim_core_memwrite32"
  writeWord8ArrayAsWord32IO# :: MutableByteArray# d -> Int# -> Word# -> IO ()

writeWord8ArrayAsWord32# :: MutableByteArray# d -> Int# -> Word# -> State# d -> State# d
writeWord8ArrayAsWord32# mb# i# a# = unsafePrimBase_ (writeWord8ArrayAsWord32IO# mb# i# a#)


-- Word64#

foreign import ccall unsafe "prim_core.c prim_core_memread64"
  readWord8ArrayAsWord64IO# :: MutableByteArray# d -> Int# -> IO Word64

#if WORD_SIZE_IN_BITS >= 64
foreign import ccall unsafe "prim_core.c prim_core_memread64"
  indexWord8ArrayAsWord64# :: ByteArray# -> Int# -> Word#
foreign import ccall unsafe "prim_core.c prim_core_memwrite64"
  writeWord8ArrayAsWord64IO# :: MutableByteArray# d -> Int# -> Word# -> IO ()

readWord8ArrayAsWord64# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Word# #)
writeWord8ArrayAsWord64# :: MutableByteArray# d -> Int# -> Word# -> State# d -> State# d

#else
foreign import ccall unsafe "prim_core.c prim_core_memread64"
  indexWord8ArrayAsWord64# :: ByteArray# -> Int# -> Word64#
foreign import ccall unsafe "prim_core.c prim_core_memwrite64"
  writeWord8ArrayAsWord64IO# :: MutableByteArray# d -> Int# -> Word64# -> IO ()

readWord8ArrayAsWord64# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Word64# #)
writeWord8ArrayAsWord64# :: MutableByteArray# d -> Int# -> Word64# -> State# d -> State# d
#endif

readWord8ArrayAsWord64# mb# i# s =
  case unsafePrimBase (readWord8ArrayAsWord64IO# mb# i#) s of
    (# s', W64# a# #) -> (# s', a# #)
writeWord8ArrayAsWord64# mb# i# a# = unsafePrimBase_ (writeWord8ArrayAsWord64IO# mb# i# a#)

-- Word#

#if WORD_SIZE_IN_BITS >= 64
foreign import ccall unsafe "prim_core.c prim_core_memread64"
  indexWord8ArrayAsWord# :: ByteArray# -> Int# -> Word#

foreign import ccall unsafe "prim_core.c prim_core_memread64"
  readWord8ArrayAsWordIO# :: MutableByteArray# d -> Int# -> IO Word

foreign import ccall unsafe "prim_core.c prim_core_memwrite64"
  writeWord8ArrayAsWordIO# :: MutableByteArray# d -> Int# -> Word# -> IO ()
#else
foreign import ccall unsafe "prim_core.c prim_core_memread32"
  indexWord8ArrayAsWord# :: ByteArray# -> Int# -> Word#

foreign import ccall unsafe "prim_core.c prim_core_memread32"
  readWord8ArrayAsWordIO# :: MutableByteArray# d -> Int# -> IO Word64

foreign import ccall unsafe "prim_core.c prim_core_memwrite32"
  writeWord8ArrayAsWordIO# :: MutableByteArray# d -> Int# -> Word# -> IO ()
#endif

readWord8ArrayAsWord# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Word# #)
readWord8ArrayAsWord# mb# i# s =
  case unsafePrimBase (readWord8ArrayAsWordIO# mb# i#) s of
    (# s', W# a# #) -> (# s', a# #)

writeWord8ArrayAsWord# :: MutableByteArray# d -> Int# -> Word# -> State# d -> State# d
writeWord8ArrayAsWord# mb# i# a# = unsafePrimBase_ (writeWord8ArrayAsWordIO# mb# i# a#)

-- ghc-8.2 (i.e. 802 version) introduced these two functions, for versions before those
-- use their re-implementations in C:
#if __GLASGOW_HASKELL__ < 802
foreign import ccall unsafe "prim_core.c prim_core_is_byte_array_pinned"
  isByteArrayPinned# :: ByteArray# -> Int#
foreign import ccall unsafe "prim_core.c prim_core_is_byte_array_pinned"
  isMutableByteArrayPinned# :: MutableByteArray# s -> Int#

newtype {-# CTYPE "bool" #-} CBool = CBool HTYPE_BOOL
  deriving (Eq,Ord,Num,Enum,Storable,Real,Read,Show)
#endif /* __GLASGOW_HASKELL__ < 802 */


#if __GLASGOW_HASKELL__ < 800
-- | Compatibility function for the old compiler versions
getSizeofMutableByteArray# :: MutableByteArray# d -> State# d -> (# State# d, Int# #)
getSizeofMutableByteArray# mba# s = (# s, sizeofMutableByteArray# mba# #)
{-# INLINE getSizeofMutableByteArray# #-}

compareByteArrays# :: ByteArray# -> Int# -> ByteArray# -> Int# -> Int# -> Int#
compareByteArrays# = memcmpByteArray#
#endif /* __GLASGOW_HASKELL__ < 800 */


#endif /* __GLASGOW_HASKELL__ < 806 */
