{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
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
  ( -- * Backwards compatibility
    --
    -- Functions introduced in older versions that have backwards compatible
    -- implementations here. Section names are ghc versions in which exported
    -- functions and types were introduced.
    module Foreign.Prim.C.LtGHC806
  -- * Forward compatibility
  -- * Extra functionality
  -- ** Atomic
  , module Foreign.Prim.C.Atomic
  -- ** Comparison
  , isSameByteArray#
  , isSameMutableByteArray#
  , toOrdering#
  , fromOrdering#
  , memcmpAddr#
  , memcmpAddrByteArray#
  , memcmpByteArray#
  , memcmpByteArrayAddr#
  -- ** Setting memory
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

  -- ** Moving memory
  , memmoveAddr#
  , memmoveMutableByteArray#
  , memmoveMutableByteArrayToAddr#
  , memmoveMutableByteArrayFromAddr#
  ) where

import GHC.Exts
import GHC.Int
import GHC.Word
import Foreign.Prim.C.Atomic
import Foreign.Prim.C.LtGHC806

-- | Because GC is guaranteed not to move unpinned memory during the unsafe FFI call we
-- can compare memory pointers on the C side. Because the addresses cannot change
-- underneath us we can safely guarantee pointer equality for the same pinned or unpinned
-- arrays

foreign import ccall unsafe "primal.c primal_ptreq"
  isSameByteArray# :: ByteArray# -> ByteArray# -> Int#

foreign import ccall unsafe "primal.c primal_ptreq"
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

fromOrdering# :: Ordering -> Int#
fromOrdering# =
  \case
    EQ -> 0#
    LT -> -1#
    GT -> 1#

foreign import ccall unsafe "primal.c primal_memcmp"
  memcmpAddr# :: Addr# -> Int# -> Addr# -> Int# -> Int# -> Int#
foreign import ccall unsafe "primal.c primal_memcmp"
  memcmpAddrByteArray# :: Addr# -> Int# -> ByteArray# -> Int# -> Int# -> Int#
foreign import ccall unsafe "primal.c primal_memcmp"
  memcmpByteArray# :: ByteArray# -> Int# -> ByteArray# -> Int# -> Int# -> Int#
foreign import ccall unsafe "primal.c primal_memcmp"
  memcmpByteArrayAddr# :: ByteArray# -> Int# -> Addr# -> Int# -> Int# -> Int#

foreign import ccall unsafe "primal.c primal_memset8"
  memsetInt8MutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> Int8 -> IO ()

foreign import ccall unsafe "primal.c primal_memset8"
  memsetInt8Addr# :: Addr# -> Int# -> Int# -> Int8 -> IO ()

foreign import ccall unsafe "primal.c primal_memset8"
  memsetWord8MutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> Word8 -> IO ()

foreign import ccall unsafe "primal.c primal_memset8"
  memsetWord8Addr# :: Addr# -> Int# -> Int# -> Word8 -> IO ()

foreign import ccall unsafe "primal.c primal_memset16"
  memsetInt16MutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> Int16 -> IO ()

foreign import ccall unsafe "primal.c primal_memset16"
  memsetInt16Addr# :: Addr# -> Int# -> Int# -> Int16 -> IO ()

foreign import ccall unsafe "primal.c primal_memset16"
  memsetWord16MutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> Word16 -> IO ()

foreign import ccall unsafe "primal.c primal_memset16"
  memsetWord16Addr# :: Addr# -> Int# -> Int# -> Word16 -> IO ()


foreign import ccall unsafe "primal.c primal_memset32"
  memsetInt32MutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> Int32 -> IO ()

foreign import ccall unsafe "primal.c primal_memset32"
  memsetInt32Addr# :: Addr# -> Int# -> Int# -> Int32 -> IO ()

foreign import ccall unsafe "primal.c primal_memset32"
  memsetWord32MutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> Word32 -> IO ()

foreign import ccall unsafe "primal.c primal_memset32"
  memsetWord32Addr# :: Addr# -> Int# -> Int# -> Word32 -> IO ()


foreign import ccall unsafe "primal.c primal_memset64"
  memsetInt64MutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> Int64 -> IO ()

foreign import ccall unsafe "primal.c primal_memset64"
  memsetInt64Addr# :: Addr# -> Int# -> Int# -> Int64 -> IO ()

foreign import ccall unsafe "primal.c primal_memset64"
  memsetWord64MutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> Word64 -> IO ()

foreign import ccall unsafe "primal.c primal_memset64"
  memsetWord64Addr# :: Addr# -> Int# -> Int# -> Word64 -> IO ()

foreign import ccall unsafe "primal.c primal_memmove"
  memmoveAddr# :: Addr# -- ^ Source ptr
               -> Int# -- ^ Offset in bytes into source array
               -> Addr# -- ^ Destination ptr
               -> Int# -- ^ Offset in bytes into destination
               -> Int# -- ^ Number of bytes to copy
               -> IO ()
foreign import ccall unsafe "primal.c primal_memmove"
  memmoveMutableByteArray# :: MutableByteArray# s -- ^ Source array
                           -> Int# -- ^ Offset in bytes into source array
                           -> MutableByteArray# s -- ^ Destination
                           -> Int# -- ^ Offset in bytes into destination
                           -> Int# -- ^ Number of bytes to copy
                           -> IO ()
foreign import ccall unsafe "primal.c primal_memmove"
  memmoveMutableByteArrayToAddr# :: MutableByteArray# s -- ^ Source array
                                 -> Int# -- ^ Offset in bytes into source array
                                 -> Addr# -- ^ Destination ptr
                                 -> Int# -- ^ Offset in bytes into destination
                                 -> Int# -- ^ Number of bytes to copy
                                 -> IO ()
foreign import ccall unsafe "primal.c primal_memmove"
  memmoveMutableByteArrayFromAddr# :: Addr# -- ^ Source Ptr
                                   -> Int# -- ^ Offset in bytes into source array
                                   -> MutableByteArray# s -- ^ Destination
                                   -> Int# -- ^ Offset in bytes into destination
                                   -> Int# -- ^ Number of bytes to copy
                                   -> IO ()

