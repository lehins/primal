{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
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
  ( isSameByteArray#
  , isSameMutableByteArray#
  --, memsetWord8MutableByteArray#
  , memsetWord8Addr#
  --, memsetInt8MutableByteArray#
  , memsetInt8Addr#
  , setWord16MutableByteArray#
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

  , getSizeofMutableByteArray#
  , isByteArrayPinned#
  , isMutableByteArrayPinned#
  , module Data.Prim.Foreign.Cmm
  ) where

import Control.Monad.Prim
import GHC.IO
import GHC.Exts
import GHC.Int
import GHC.Word
import Data.Prim.Foreign.Cmm

-- | Because GC is guaranteed not to move unpinned memory during the unsafe FFI call we
-- can compare memory pointers on the C side. Because the addresses cannot change
-- underneath us we can safely guarantee pointer equality for the same pinned or unpinned
-- arrays

foreign import ccall unsafe "prim_core.c prim_core_ptreq"
  isSameByteArray# :: ByteArray# -> ByteArray# -> Int#

foreign import ccall unsafe "prim_core.c prim_core_ptreq"
  isSameMutableByteArray# :: MutableByteArray# s -> MutableByteArray# s -> Int#


-- foreign import ccall unsafe "prim_core.c prim_core_memset8"
--   memsetInt8MutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> Int8 -> IO ()

foreign import ccall unsafe "prim_core.c prim_core_memset8"
  memsetInt8Addr# :: Addr# -> Int# -> Int# -> Int8 -> IO ()

-- foreign import ccall unsafe "prim_core.c prim_core_memset8"
--   memsetWord8MutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> Word8 -> IO ()

foreign import ccall unsafe "prim_core.c prim_core_memset8"
  memsetWord8Addr# :: Addr# -> Int# -> Int# -> Word8 -> IO ()

setWord16MutableByteArray# :: MutableByteArray# RealWorld -> Int# -> Int# -> Word16 -> IO ()
setWord16MutableByteArray# mba# i# o# w16@(W16# w16#) =
  let w8# = and# w16# 0xff##
  in if isTrue# (uncheckedShiftRL# w16# 8# `eqWord#` w8#)
     then prim_ (setByteArray# mba# (i# *# 2#) (o# *# 2#) (word2Int# w8#))
     else memsetWord16MutableByteArray# mba# i# o# w16

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


#if 0 && __GLASGOW_HASKELL__ < 804
-- | Compatibility function for the old compiler versions
getSizeofMutableByteArray# mba# s# = (# s#, sizeofMutableByteArray# mba# #)
{-# INLINE getSizeofMutableByteArray# #-}
#endif

-- ghc-8.2 (i.e. 802 version) introduced these two functions, for versions before those
-- use their reimplementations in C:
#if __GLASGOW_HASKELL__ < 802
foreign import ccall unsafe "prim_core.c prim_core_is_byte_array_pinned"
  isByteArrayPinned# :: ByteArray# -> Int#
foreign import ccall unsafe "prim_core.c prim_core_is_byte_array_pinned"
  isMutableByteArrayPinned# :: MutableByteArray# s -> Int#
#endif
