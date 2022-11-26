{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- |
-- Module      : Primal.Foreign.C
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module Primal.Foreign.C
  ( -- * Backwards compatibility

  --
  -- Functions introduced in older versions that have backwards compatible
  -- implementations here. Section names are ghc versions in which exported
  -- functions and types were introduced.
      module Primal.Foreign.C.LtGHC806

    -- * Forward compatibility

    -- * Extra functionality

    -- ** Atomic
  , module Primal.Foreign.C.Atomic

    -- ** Comparison
  , isSameByteArray#
  , isSameMutableByteArray#
  , toOrdering#
  , fromOrdering#
  , toOrdering
  , fromOrdering
  , memcmpAddr#
  , memcmpAddrByteArray#
  , memcmpByteArray#
  , memcmpByteArrayAddr#
  , memcmpMutableByteArray#
  , memcmpMutableByteArrayAddr#

    -- ** Setting memory
  , setWord8ArrayAsInt16#
  , setWord8ArrayAsInt32#
  , setWord8ArrayAsInt64#
  , setWord8ArrayAsWord16#
  , setWord8ArrayAsWord32#
  , setWord8ArrayAsWord64#
  , setInt8Addr#
  , setInt16Addr#
  , setInt32Addr#
  , setInt64Addr#
  , setWord8Addr#
  , setWord16Addr#
  , setWord32Addr#
  , setWord64Addr#
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
import Primal.Foreign.C.Atomic
import Primal.Foreign.C.LtGHC806
import Primal.Monad.Unsafe

-- | Because GC is guaranteed not to move unpinned memory during the unsafe FFI call we
-- can compare memory pointers on the C side. Because the addresses cannot change
-- underneath us we can safely guarantee pointer equality for the same pinned or unpinned
-- arrays
foreign import ccall unsafe "primal.c primal_ptreq"
  isSameByteArray# :: ByteArray# -> ByteArray# -> Int#

foreign import ccall unsafe "primal.c primal_ptreq"
  isSameMutableByteArray# :: MutableByteArray# s -> MutableByteArray# s -> Int#

toOrdering :: Int -> Ordering
toOrdering (I# i#) = toOrdering# i#
{-# INLINE toOrdering #-}

fromOrdering :: Ordering -> Int
fromOrdering o = I# (fromOrdering# o)
{-# INLINE fromOrdering #-}

-- | Convert memcmp result into an ordering
toOrdering# :: Int# -> Ordering
toOrdering# =
  \case
    0# -> EQ
    n# ->
      if isTrue# (n# <# 0#)
        then LT
        else GT
{-# INLINE toOrdering# #-}

fromOrdering# :: Ordering -> Int#
fromOrdering# =
  \case
    EQ -> 0#
    LT -> -1#
    GT -> 1#
{-# INLINE fromOrdering# #-}

foreign import ccall unsafe "primal.c primal_memcmp"
  memcmpAddr# :: Addr# -> Int# -> Addr# -> Int# -> Int# -> Int#
foreign import ccall unsafe "primal.c primal_memcmp"
  memcmpAddrByteArray# :: Addr# -> Int# -> ByteArray# -> Int# -> Int# -> Int#
foreign import ccall unsafe "primal.c primal_memcmp"
  memcmpByteArray# :: ByteArray# -> Int# -> ByteArray# -> Int# -> Int# -> Int#
foreign import ccall unsafe "primal.c primal_memcmp"
  memcmpByteArrayAddr# :: ByteArray# -> Int# -> Addr# -> Int# -> Int# -> Int#
foreign import ccall unsafe "primal.c primal_memcmp"
  memcmpMutableByteArray# :: MutableByteArray# s -> Int# -> MutableByteArray# s -> Int# -> Int# -> IO Int
foreign import ccall unsafe "primal.c primal_memcmp"
  memcmpMutableByteArrayAddr# :: MutableByteArray# s -> Int# -> Addr# -> Int# -> Int# -> IO Int

foreign import ccall unsafe "primal.c primal_memset8"
  memsetInt8MutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> Int# -> IO ()

foreign import ccall unsafe "primal.c primal_memset8"
  memsetInt8Addr# :: Addr# -> Int# -> Int# -> Int# -> IO ()

foreign import ccall unsafe "primal.c primal_memset8"
  memsetWord8MutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> Word# -> IO ()

foreign import ccall unsafe "primal.c primal_memset8"
  memsetWord8Addr# :: Addr# -> Int# -> Int# -> Word# -> IO ()

setInt8Addr# :: Addr# -> Int# -> Int# -> State# s -> State# s
setInt8Addr# addr# n# a# = unsafePrimalState_ (memsetInt8Addr# addr# 0# n# a#)
{-# INLINE setInt8Addr# #-}

setWord8Addr# :: Addr# -> Int# -> Word# -> State# s -> State# s
setWord8Addr# addr# n# a# = unsafePrimalState_ (memsetWord8Addr# addr# 0# n# a#)
{-# INLINE setWord8Addr# #-}

foreign import ccall unsafe "primal.c primal_memset16"
  memsetInt16MutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> Int# -> IO ()

foreign import ccall unsafe "primal.c primal_memset16"
  memsetInt16Addr# :: Addr# -> Int# -> Int# -> Int# -> IO ()

foreign import ccall unsafe "primal.c primal_memset16"
  memsetWord16MutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> Word# -> IO ()

foreign import ccall unsafe "primal.c primal_memset16"
  memsetWord16Addr# :: Addr# -> Int# -> Int# -> Word# -> IO ()

setWord8ArrayAsInt16# :: MutableByteArray# s -> Int# -> Int# -> Int# -> State# s -> State# s
setWord8ArrayAsInt16# mba# o# n# a# = unsafePrimalState_ (memsetInt16MutableByteArray# mba# o# n# a#)
{-# INLINE setWord8ArrayAsInt16# #-}

setWord8ArrayAsWord16# :: MutableByteArray# s -> Int# -> Int# -> Word# -> State# s -> State# s
setWord8ArrayAsWord16# mba# o# n# a# = unsafePrimalState_ (memsetWord16MutableByteArray# mba# o# n# a#)
{-# INLINE setWord8ArrayAsWord16# #-}

setInt16Addr# :: Addr# -> Int# -> Int# -> State# s -> State# s
setInt16Addr# addr# n# a# = unsafePrimalState_ (memsetInt16Addr# addr# 0# n# a#)
{-# INLINE setInt16Addr# #-}

setWord16Addr# :: Addr# -> Int# -> Word# -> State# s -> State# s
setWord16Addr# addr# n# a# = unsafePrimalState_ (memsetWord16Addr# addr# 0# n# a#)
{-# INLINE setWord16Addr# #-}

foreign import ccall unsafe "primal.c primal_memset32"
  memsetInt32MutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> Int# -> IO ()

foreign import ccall unsafe "primal.c primal_memset32"
  memsetInt32Addr# :: Addr# -> Int# -> Int# -> Int# -> IO ()

foreign import ccall unsafe "primal.c primal_memset32"
  memsetWord32MutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> Word# -> IO ()

foreign import ccall unsafe "primal.c primal_memset32"
  memsetWord32Addr# :: Addr# -> Int# -> Int# -> Word# -> IO ()

setWord8ArrayAsInt32# :: MutableByteArray# s -> Int# -> Int# -> Int# -> State# s -> State# s
setWord8ArrayAsInt32# mba# o# n# a# = unsafePrimalState_ (memsetInt32MutableByteArray# mba# o# n# a#)
{-# INLINE setWord8ArrayAsInt32# #-}

setWord8ArrayAsWord32# :: MutableByteArray# s -> Int# -> Int# -> Word# -> State# s -> State# s
setWord8ArrayAsWord32# mba# o# n# a# = unsafePrimalState_ (memsetWord32MutableByteArray# mba# o# n# a#)
{-# INLINE setWord8ArrayAsWord32# #-}

setInt32Addr# :: Addr# -> Int# -> Int# -> State# s -> State# s
setInt32Addr# addr# n# a# = unsafePrimalState_ (memsetInt32Addr# addr# 0# n# a#)
{-# INLINE setInt32Addr# #-}

setWord32Addr# :: Addr# -> Int# -> Word# -> State# s -> State# s
setWord32Addr# addr# n# a# = unsafePrimalState_ (memsetWord32Addr# addr# 0# n# a#)
{-# INLINE setWord32Addr# #-}

foreign import ccall unsafe "primal.c primal_memset64"
  memsetInt64MutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> Int# -> IO ()

foreign import ccall unsafe "primal.c primal_memset64"
  memsetInt64Addr# :: Addr# -> Int# -> Int# -> Int# -> IO ()

foreign import ccall unsafe "primal.c primal_memset64"
  memsetWord64MutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> Word# -> IO ()

foreign import ccall unsafe "primal.c primal_memset64"
  memsetWord64Addr# :: Addr# -> Int# -> Int# -> Word# -> IO ()

setWord8ArrayAsInt64# :: MutableByteArray# s -> Int# -> Int# -> Int# -> State# s -> State# s
setWord8ArrayAsInt64# mba# o# n# a# = unsafePrimalState_ (memsetInt64MutableByteArray# mba# o# n# a#)
{-# INLINE setWord8ArrayAsInt64# #-}

setWord8ArrayAsWord64# :: MutableByteArray# s -> Int# -> Int# -> Word# -> State# s -> State# s
setWord8ArrayAsWord64# mba# o# n# a# = unsafePrimalState_ (memsetWord64MutableByteArray# mba# o# n# a#)
{-# INLINE setWord8ArrayAsWord64# #-}

setInt64Addr# :: Addr# -> Int# -> Int# -> State# s -> State# s
setInt64Addr# addr# n# a# = unsafePrimalState_ (memsetInt64Addr# addr# 0# n# a#)
{-# INLINE setInt64Addr# #-}

setWord64Addr# :: Addr# -> Int# -> Word# -> State# s -> State# s
setWord64Addr# addr# n# a# = unsafePrimalState_ (memsetWord64Addr# addr# 0# n# a#)
{-# INLINE setWord64Addr# #-}

foreign import ccall unsafe "primal.c primal_memmove"
  memmoveAddr#
    :: Addr#
    -- ^ Source ptr
    -> Int#
    -- ^ Offset in bytes into source array
    -> Addr#
    -- ^ Destination ptr
    -> Int#
    -- ^ Offset in bytes into destination
    -> Int#
    -- ^ Number of bytes to copy
    -> IO ()
foreign import ccall unsafe "primal.c primal_memmove"
  memmoveMutableByteArray#
    :: MutableByteArray# s
    -- ^ Source array
    -> Int#
    -- ^ Offset in bytes into source array
    -> MutableByteArray# s
    -- ^ Destination
    -> Int#
    -- ^ Offset in bytes into destination
    -> Int#
    -- ^ Number of bytes to copy
    -> IO ()
foreign import ccall unsafe "primal.c primal_memmove"
  memmoveMutableByteArrayToAddr#
    :: MutableByteArray# s
    -- ^ Source array
    -> Int#
    -- ^ Offset in bytes into source array
    -> Addr#
    -- ^ Destination ptr
    -> Int#
    -- ^ Offset in bytes into destination
    -> Int#
    -- ^ Number of bytes to copy
    -> IO ()
foreign import ccall unsafe "primal.c primal_memmove"
  memmoveMutableByteArrayFromAddr#
    :: Addr#
    -- ^ Source Ptr
    -> Int#
    -- ^ Offset in bytes into source array
    -> MutableByteArray# s
    -- ^ Destination
    -> Int#
    -- ^ Offset in bytes into destination
    -> Int#
    -- ^ Number of bytes to copy
    -> IO ()
