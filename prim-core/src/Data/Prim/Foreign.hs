{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
-- |
-- Module      : Data.Prim.Foreign
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Foreign
  ( memsetWord8MutableByteArray#
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
  , getSizeofMutableByteArray#
  , isByteArrayPinned#
  , isMutableByteArrayPinned#
  ) where

import GHC.Exts
import GHC.Int
import GHC.Word

foreign import ccall unsafe "prim.c prim_memset8"
  memsetInt8MutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> Int8 -> IO ()

foreign import ccall unsafe "prim.c prim_memset8"
  memsetInt8Addr# :: Addr# -> Int# -> Int# -> Int8 -> IO ()

foreign import ccall unsafe "prim.c prim_memset8"
  memsetWord8MutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> Word8 -> IO ()

foreign import ccall unsafe "prim.c prim_memset8"
  memsetWord8Addr# :: Addr# -> Int# -> Int# -> Word8 -> IO ()


foreign import ccall unsafe "prim.c prim_memset16"
  memsetInt16MutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> Int16 -> IO ()

foreign import ccall unsafe "prim.c prim_memset16"
  memsetInt16Addr# :: Addr# -> Int# -> Int# -> Int16 -> IO ()

foreign import ccall unsafe "prim.c prim_memset16"
  memsetWord16MutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> Word16 -> IO ()

foreign import ccall unsafe "prim.c prim_memset16"
  memsetWord16Addr# :: Addr# -> Int# -> Int# -> Word16 -> IO ()


foreign import ccall unsafe "prim.c prim_memset32"
  memsetInt32MutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> Int32 -> IO ()

foreign import ccall unsafe "prim.c prim_memset32"
  memsetInt32Addr# :: Addr# -> Int# -> Int# -> Int32 -> IO ()

foreign import ccall unsafe "prim.c prim_memset32"
  memsetWord32MutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> Word32 -> IO ()

foreign import ccall unsafe "prim.c prim_memset32"
  memsetWord32Addr# :: Addr# -> Int# -> Int# -> Word32 -> IO ()


foreign import ccall unsafe "prim.c prim_memset64"
  memsetInt64MutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> Int64 -> IO ()

foreign import ccall unsafe "prim.c prim_memset64"
  memsetInt64Addr# :: Addr# -> Int# -> Int# -> Int64 -> IO ()

foreign import ccall unsafe "prim.c prim_memset64"
  memsetWord64MutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> Word64 -> IO ()

foreign import ccall unsafe "prim.c prim_memset64"
  memsetWord64Addr# :: Addr# -> Int# -> Int# -> Word64 -> IO ()

#if __GLASGOW_HASKELL__ < 804
-- | Compatibility function for the old compiler versions
getSizeofMutableByteArray# mba# s# = (# s#, sizeofMutableByteArray# mba# #)
{-# INLINE getSizeofMutableByteArray# #-}
#endif

-- ghc-8.2 (i.e. 802 version) introduced these two functions, for versions before those
-- use their reimplementations in C:
#if __GLASGOW_HASKELL__ < 802
foreign import ccall unsafe "pvar.c pvar_is_byte_array_pinned"
  isByteArrayPinned# :: ByteArray# -> Int#
foreign import ccall unsafe "pvar.c pvar_is_byte_array_pinned"
  isMutableByteArrayPinned# :: MutableByteArray# s -> Int#
#endif
