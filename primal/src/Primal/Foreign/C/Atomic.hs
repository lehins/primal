{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- |
-- Module      : Primal.Foreign.C.Atomic
-- Copyright   : (c) Alexey Kuleshevich 2020-2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module Primal.Foreign.C.Atomic
  ( module Primal.Foreign.C.Atomic
  ) where

import GHC.Exts
import GHC.Int
import GHC.Word
import Primal.Foreign.C.LtGHC802
import Primal.Monad.Unsafe

#include "MachDeps.h"

-- | Helper function for converting casBool IO actions
ioCBoolToBoolBase :: IO CBool -> State# s -> (# State# s, Bool #)
ioCBoolToBoolBase m s =
  case unsafePrimalState m s of
    (# s', CBool (W8# w#) #) -> (# s', isTrue# (word2Int# w#) #)
{-# INLINE ioCBoolToBoolBase #-}

-- | [Memory barrier](https://en.wikipedia.org/wiki/Memory_barrier). This will
-- ensure that the cache is fully updated before continuing.
syncSynchronize# :: State# s -> State# s
syncSynchronize# = unsafePrimalState_ syncSynchronize
{-# INLINE syncSynchronize# #-}

withMemBarrier# :: (State# s -> (# State# s, a #)) -> State# s -> (# State# s, a #)
withMemBarrier# f s = f (syncSynchronize# s)
{-# INLINE withMemBarrier# #-}

withMemBarrier_# :: (State# s -> State# s) -> State# s -> State# s
withMemBarrier_# f s = f (syncSynchronize# s)
{-# INLINE withMemBarrier_# #-}

foreign import ccall unsafe "primal_atomic.c primal_sync_synchronize"
  syncSynchronize :: IO ()

foreign import ccall unsafe "primal_atomic.c primal_sync8_lock_test_set"
  syncLockTestSetInt8ArrayIO :: MutableByteArray# s -> Int# -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_lock_test_set"
  syncLockTestSetInt8AddrIO :: Addr# -> Int# -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_lock_release"
  syncLockReleaseInt8ArrayIO :: MutableByteArray# s -> Int# -> IO ()
foreign import ccall unsafe "primal_atomic.c primal_sync8_lock_release"
  syncLockReleaseInt8AddrIO :: Addr# -> Int# -> IO ()

foreign import ccall unsafe "primal_atomic.c primal_sync8_cas_bool"
  syncCasInt8BoolAddrIO :: Addr# -> Int# -> Int8 -> Int8 -> IO CBool
foreign import ccall unsafe "primal_atomic.c primal_sync8_cas_bool"
  syncCasInt8BoolArrayIO :: MutableByteArray# s -> Int# -> Int8 -> Int8 -> IO CBool
foreign import ccall unsafe "primal_atomic.c primal_sync8_cas_bool"
  syncCasWord8BoolAddrIO :: Addr# -> Int# -> Word8 -> Word8 -> IO CBool
foreign import ccall unsafe "primal_atomic.c primal_sync8_cas_bool"
  syncCasWord8BoolArrayIO :: MutableByteArray# s -> Int# -> Word8 -> Word8 -> IO CBool

foreign import ccall unsafe "primal_atomic.c primal_sync8_cas"
  syncCasInt8AddrIO :: Addr# -> Int# -> Int8 -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_cas"
  syncCasInt8ArrayIO :: MutableByteArray# s -> Int# -> Int8 -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_cas"
  syncCasWord8AddrIO :: Addr# -> Int# -> Word8 -> Word8 -> IO Word8
foreign import ccall unsafe "primal_atomic.c primal_sync8_cas"
  syncCasWord8ArrayIO :: MutableByteArray# s -> Int# -> Word8 -> Word8 -> IO Word8

foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_add"
  syncAddFetchOldInt8AddrIO :: Addr# -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_add"
  syncAddFetchOldInt8ArrayIO :: MutableByteArray# s -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_add"
  syncAddFetchOldWord8AddrIO :: Addr# -> Int# -> Word8 -> IO Word8
foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_add"
  syncAddFetchOldWord8ArrayIO :: MutableByteArray# s -> Int# -> Word8 -> IO Word8

foreign import ccall unsafe "primal_atomic.c primal_sync8_add_fetch"
  syncAddFetchNewInt8AddrIO :: Addr# -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_add_fetch"
  syncAddFetchNewInt8ArrayIO :: MutableByteArray# s -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_add_fetch"
  syncAddFetchNewWord8AddrIO :: Addr# -> Int# -> Word8 -> IO Word8
foreign import ccall unsafe "primal_atomic.c primal_sync8_add_fetch"
  syncAddFetchNewWord8ArrayIO :: MutableByteArray# s -> Int# -> Word8 -> IO Word8

foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_sub"
  syncSubFetchOldInt8AddrIO :: Addr# -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_sub"
  syncSubFetchOldInt8ArrayIO :: MutableByteArray# s -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_sub"
  syncSubFetchOldWord8AddrIO :: Addr# -> Int# -> Word8 -> IO Word8
foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_sub"
  syncSubFetchOldWord8ArrayIO :: MutableByteArray# s -> Int# -> Word8 -> IO Word8

foreign import ccall unsafe "primal_atomic.c primal_sync8_sub_fetch"
  syncSubFetchNewInt8AddrIO :: Addr# -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_sub_fetch"
  syncSubFetchNewInt8ArrayIO :: MutableByteArray# s -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_sub_fetch"
  syncSubFetchNewWord8AddrIO :: Addr# -> Int# -> Word8 -> IO Word8
foreign import ccall unsafe "primal_atomic.c primal_sync8_sub_fetch"
  syncSubFetchNewWord8ArrayIO :: MutableByteArray# s -> Int# -> Word8 -> IO Word8

foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_and"
  syncAndFetchOldInt8AddrIO :: Addr# -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_and"
  syncAndFetchOldInt8ArrayIO :: MutableByteArray# s -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_and"
  syncAndFetchOldWord8AddrIO :: Addr# -> Int# -> Word8 -> IO Word8
foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_and"
  syncAndFetchOldWord8ArrayIO :: MutableByteArray# s -> Int# -> Word8 -> IO Word8

foreign import ccall unsafe "primal_atomic.c primal_sync8_and_fetch"
  syncAndFetchNewInt8AddrIO :: Addr# -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_and_fetch"
  syncAndFetchNewInt8ArrayIO :: MutableByteArray# s -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_and_fetch"
  syncAndFetchNewWord8AddrIO :: Addr# -> Int# -> Word8 -> IO Word8
foreign import ccall unsafe "primal_atomic.c primal_sync8_and_fetch"
  syncAndFetchNewWord8ArrayIO :: MutableByteArray# s -> Int# -> Word8 -> IO Word8

foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_nand"
  syncNandFetchOldInt8AddrIO :: Addr# -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_nand"
  syncNandFetchOldInt8ArrayIO :: MutableByteArray# s -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_nand"
  syncNandFetchOldWord8AddrIO :: Addr# -> Int# -> Word8 -> IO Word8
foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_nand"
  syncNandFetchOldWord8ArrayIO :: MutableByteArray# s -> Int# -> Word8 -> IO Word8

foreign import ccall unsafe "primal_atomic.c primal_sync8_nand_fetch"
  syncNandFetchNewInt8AddrIO :: Addr# -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_nand_fetch"
  syncNandFetchNewInt8ArrayIO :: MutableByteArray# s -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_nand_fetch"
  syncNandFetchNewWord8AddrIO :: Addr# -> Int# -> Word8 -> IO Word8
foreign import ccall unsafe "primal_atomic.c primal_sync8_nand_fetch"
  syncNandFetchNewWord8ArrayIO :: MutableByteArray# s -> Int# -> Word8 -> IO Word8

foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_or"
  syncOrFetchOldInt8AddrIO :: Addr# -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_or"
  syncOrFetchOldInt8ArrayIO :: MutableByteArray# s -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_or"
  syncOrFetchOldWord8AddrIO :: Addr# -> Int# -> Word8 -> IO Word8
foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_or"
  syncOrFetchOldWord8ArrayIO :: MutableByteArray# s -> Int# -> Word8 -> IO Word8

foreign import ccall unsafe "primal_atomic.c primal_sync8_or_fetch"
  syncOrFetchNewInt8AddrIO :: Addr# -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_or_fetch"
  syncOrFetchNewInt8ArrayIO :: MutableByteArray# s -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_or_fetch"
  syncOrFetchNewWord8AddrIO :: Addr# -> Int# -> Word8 -> IO Word8
foreign import ccall unsafe "primal_atomic.c primal_sync8_or_fetch"
  syncOrFetchNewWord8ArrayIO :: MutableByteArray# s -> Int# -> Word8 -> IO Word8

foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_xor"
  syncXorFetchOldInt8AddrIO :: Addr# -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_xor"
  syncXorFetchOldInt8ArrayIO :: MutableByteArray# s -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_xor"
  syncXorFetchOldWord8AddrIO :: Addr# -> Int# -> Word8 -> IO Word8
foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_xor"
  syncXorFetchOldWord8ArrayIO :: MutableByteArray# s -> Int# -> Word8 -> IO Word8

foreign import ccall unsafe "primal_atomic.c primal_sync8_xor_fetch"
  syncXorFetchNewInt8AddrIO :: Addr# -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_xor_fetch"
  syncXorFetchNewInt8ArrayIO :: MutableByteArray# s -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_xor_fetch"
  syncXorFetchNewWord8AddrIO :: Addr# -> Int# -> Word8 -> IO Word8
foreign import ccall unsafe "primal_atomic.c primal_sync8_xor_fetch"
  syncXorFetchNewWord8ArrayIO :: MutableByteArray# s -> Int# -> Word8 -> IO Word8

foreign import ccall unsafe "primal_atomic.c primal_sync16_cas_bool"
  syncCasInt16BoolAddrIO :: Addr# -> Int# -> Int16 -> Int16 -> IO CBool
foreign import ccall unsafe "primal_atomic.c primal_sync16_cas_bool"
  syncCasInt16BoolArrayIO :: MutableByteArray# s -> Int# -> Int16 -> Int16 -> IO CBool
foreign import ccall unsafe "primal_atomic.c primal_sync16_cas_bool"
  syncCasWord16BoolAddrIO :: Addr# -> Int# -> Word16 -> Word16 -> IO CBool
foreign import ccall unsafe "primal_atomic.c primal_sync16_cas_bool"
  syncCasWord16BoolArrayIO :: MutableByteArray# s -> Int# -> Word16 -> Word16 -> IO CBool

foreign import ccall unsafe "primal_atomic.c primal_sync16_cas"
  syncCasInt16AddrIO :: Addr# -> Int# -> Int16 -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_cas"
  syncCasInt16ArrayIO :: MutableByteArray# s -> Int# -> Int16 -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_cas"
  syncCasWord16AddrIO :: Addr# -> Int# -> Word16 -> Word16 -> IO Word16
foreign import ccall unsafe "primal_atomic.c primal_sync16_cas"
  syncCasWord16ArrayIO :: MutableByteArray# s -> Int# -> Word16 -> Word16 -> IO Word16

foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_add"
  syncAddFetchOldInt16AddrIO :: Addr# -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_add"
  syncAddFetchOldInt16ArrayIO :: MutableByteArray# s -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_add"
  syncAddFetchOldWord16AddrIO :: Addr# -> Int# -> Word16 -> IO Word16
foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_add"
  syncAddFetchOldWord16ArrayIO :: MutableByteArray# s -> Int# -> Word16 -> IO Word16

foreign import ccall unsafe "primal_atomic.c primal_sync16_add_fetch"
  syncAddFetchNewInt16AddrIO :: Addr# -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_add_fetch"
  syncAddFetchNewInt16ArrayIO :: MutableByteArray# s -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_add_fetch"
  syncAddFetchNewWord16AddrIO :: Addr# -> Int# -> Word16 -> IO Word16
foreign import ccall unsafe "primal_atomic.c primal_sync16_add_fetch"
  syncAddFetchNewWord16ArrayIO :: MutableByteArray# s -> Int# -> Word16 -> IO Word16

foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_sub"
  syncSubFetchOldInt16AddrIO :: Addr# -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_sub"
  syncSubFetchOldInt16ArrayIO :: MutableByteArray# s -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_sub"
  syncSubFetchOldWord16AddrIO :: Addr# -> Int# -> Word16 -> IO Word16
foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_sub"
  syncSubFetchOldWord16ArrayIO :: MutableByteArray# s -> Int# -> Word16 -> IO Word16

foreign import ccall unsafe "primal_atomic.c primal_sync16_sub_fetch"
  syncSubFetchNewInt16AddrIO :: Addr# -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_sub_fetch"
  syncSubFetchNewInt16ArrayIO :: MutableByteArray# s -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_sub_fetch"
  syncSubFetchNewWord16AddrIO :: Addr# -> Int# -> Word16 -> IO Word16
foreign import ccall unsafe "primal_atomic.c primal_sync16_sub_fetch"
  syncSubFetchNewWord16ArrayIO :: MutableByteArray# s -> Int# -> Word16 -> IO Word16

foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_and"
  syncAndFetchOldInt16AddrIO :: Addr# -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_and"
  syncAndFetchOldInt16ArrayIO :: MutableByteArray# s -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_and"
  syncAndFetchOldWord16AddrIO :: Addr# -> Int# -> Word16 -> IO Word16
foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_and"
  syncAndFetchOldWord16ArrayIO :: MutableByteArray# s -> Int# -> Word16 -> IO Word16

foreign import ccall unsafe "primal_atomic.c primal_sync16_and_fetch"
  syncAndFetchNewInt16AddrIO :: Addr# -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_and_fetch"
  syncAndFetchNewInt16ArrayIO :: MutableByteArray# s -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_and_fetch"
  syncAndFetchNewWord16AddrIO :: Addr# -> Int# -> Word16 -> IO Word16
foreign import ccall unsafe "primal_atomic.c primal_sync16_and_fetch"
  syncAndFetchNewWord16ArrayIO :: MutableByteArray# s -> Int# -> Word16 -> IO Word16

foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_nand"
  syncNandFetchOldInt16AddrIO :: Addr# -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_nand"
  syncNandFetchOldInt16ArrayIO :: MutableByteArray# s -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_nand"
  syncNandFetchOldWord16AddrIO :: Addr# -> Int# -> Word16 -> IO Word16
foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_nand"
  syncNandFetchOldWord16ArrayIO :: MutableByteArray# s -> Int# -> Word16 -> IO Word16

foreign import ccall unsafe "primal_atomic.c primal_sync16_nand_fetch"
  syncNandFetchNewInt16AddrIO :: Addr# -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_nand_fetch"
  syncNandFetchNewInt16ArrayIO :: MutableByteArray# s -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_nand_fetch"
  syncNandFetchNewWord16AddrIO :: Addr# -> Int# -> Word16 -> IO Word16
foreign import ccall unsafe "primal_atomic.c primal_sync16_nand_fetch"
  syncNandFetchNewWord16ArrayIO :: MutableByteArray# s -> Int# -> Word16 -> IO Word16

foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_or"
  syncOrFetchOldInt16AddrIO :: Addr# -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_or"
  syncOrFetchOldInt16ArrayIO :: MutableByteArray# s -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_or"
  syncOrFetchOldWord16AddrIO :: Addr# -> Int# -> Word16 -> IO Word16
foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_or"
  syncOrFetchOldWord16ArrayIO :: MutableByteArray# s -> Int# -> Word16 -> IO Word16

foreign import ccall unsafe "primal_atomic.c primal_sync16_or_fetch"
  syncOrFetchNewInt16AddrIO :: Addr# -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_or_fetch"
  syncOrFetchNewInt16ArrayIO :: MutableByteArray# s -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_or_fetch"
  syncOrFetchNewWord16AddrIO :: Addr# -> Int# -> Word16 -> IO Word16
foreign import ccall unsafe "primal_atomic.c primal_sync16_or_fetch"
  syncOrFetchNewWord16ArrayIO :: MutableByteArray# s -> Int# -> Word16 -> IO Word16

foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_xor"
  syncXorFetchOldInt16AddrIO :: Addr# -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_xor"
  syncXorFetchOldInt16ArrayIO :: MutableByteArray# s -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_xor"
  syncXorFetchOldWord16AddrIO :: Addr# -> Int# -> Word16 -> IO Word16
foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_xor"
  syncXorFetchOldWord16ArrayIO :: MutableByteArray# s -> Int# -> Word16 -> IO Word16

foreign import ccall unsafe "primal_atomic.c primal_sync16_xor_fetch"
  syncXorFetchNewInt16AddrIO :: Addr# -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_xor_fetch"
  syncXorFetchNewInt16ArrayIO :: MutableByteArray# s -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_xor_fetch"
  syncXorFetchNewWord16AddrIO :: Addr# -> Int# -> Word16 -> IO Word16
foreign import ccall unsafe "primal_atomic.c primal_sync16_xor_fetch"
  syncXorFetchNewWord16ArrayIO :: MutableByteArray# s -> Int# -> Word16 -> IO Word16

foreign import ccall unsafe "primal_atomic.c primal_sync32_cas_bool"
  syncCasInt32BoolAddrIO :: Addr# -> Int# -> Int32 -> Int32 -> IO CBool
foreign import ccall unsafe "primal_atomic.c primal_sync32_cas_bool"
  syncCasInt32BoolArrayIO :: MutableByteArray# s -> Int# -> Int32 -> Int32 -> IO CBool
foreign import ccall unsafe "primal_atomic.c primal_sync32_cas_bool"
  syncCasWord32BoolAddrIO :: Addr# -> Int# -> Word32 -> Word32 -> IO CBool
foreign import ccall unsafe "primal_atomic.c primal_sync32_cas_bool"
  syncCasWord32BoolArrayIO :: MutableByteArray# s -> Int# -> Word32 -> Word32 -> IO CBool

foreign import ccall unsafe "primal_atomic.c primal_sync32_cas"
  syncCasInt32AddrIO :: Addr# -> Int# -> Int32 -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_cas"
  syncCasInt32ArrayIO :: MutableByteArray# s -> Int# -> Int32 -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_cas"
  syncCasWord32AddrIO :: Addr# -> Int# -> Word32 -> Word32 -> IO Word32
foreign import ccall unsafe "primal_atomic.c primal_sync32_cas"
  syncCasWord32ArrayIO :: MutableByteArray# s -> Int# -> Word32 -> Word32 -> IO Word32

foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_add"
  syncAddFetchOldInt32AddrIO :: Addr# -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_add"
  syncAddFetchOldInt32ArrayIO :: MutableByteArray# s -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_add"
  syncAddFetchOldWord32AddrIO :: Addr# -> Int# -> Word32 -> IO Word32
foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_add"
  syncAddFetchOldWord32ArrayIO :: MutableByteArray# s -> Int# -> Word32 -> IO Word32

foreign import ccall unsafe "primal_atomic.c primal_sync32_add_fetch"
  syncAddFetchNewInt32AddrIO :: Addr# -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_add_fetch"
  syncAddFetchNewInt32ArrayIO :: MutableByteArray# s -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_add_fetch"
  syncAddFetchNewWord32AddrIO :: Addr# -> Int# -> Word32 -> IO Word32
foreign import ccall unsafe "primal_atomic.c primal_sync32_add_fetch"
  syncAddFetchNewWord32ArrayIO :: MutableByteArray# s -> Int# -> Word32 -> IO Word32

foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_sub"
  syncSubFetchOldInt32AddrIO :: Addr# -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_sub"
  syncSubFetchOldInt32ArrayIO :: MutableByteArray# s -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_sub"
  syncSubFetchOldWord32AddrIO :: Addr# -> Int# -> Word32 -> IO Word32
foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_sub"
  syncSubFetchOldWord32ArrayIO :: MutableByteArray# s -> Int# -> Word32 -> IO Word32

foreign import ccall unsafe "primal_atomic.c primal_sync32_sub_fetch"
  syncSubFetchNewInt32AddrIO :: Addr# -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_sub_fetch"
  syncSubFetchNewInt32ArrayIO :: MutableByteArray# s -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_sub_fetch"
  syncSubFetchNewWord32AddrIO :: Addr# -> Int# -> Word32 -> IO Word32
foreign import ccall unsafe "primal_atomic.c primal_sync32_sub_fetch"
  syncSubFetchNewWord32ArrayIO :: MutableByteArray# s -> Int# -> Word32 -> IO Word32

foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_and"
  syncAndFetchOldInt32AddrIO :: Addr# -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_and"
  syncAndFetchOldInt32ArrayIO :: MutableByteArray# s -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_and"
  syncAndFetchOldWord32AddrIO :: Addr# -> Int# -> Word32 -> IO Word32
foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_and"
  syncAndFetchOldWord32ArrayIO :: MutableByteArray# s -> Int# -> Word32 -> IO Word32

foreign import ccall unsafe "primal_atomic.c primal_sync32_and_fetch"
  syncAndFetchNewInt32AddrIO :: Addr# -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_and_fetch"
  syncAndFetchNewInt32ArrayIO :: MutableByteArray# s -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_and_fetch"
  syncAndFetchNewWord32AddrIO :: Addr# -> Int# -> Word32 -> IO Word32
foreign import ccall unsafe "primal_atomic.c primal_sync32_and_fetch"
  syncAndFetchNewWord32ArrayIO :: MutableByteArray# s -> Int# -> Word32 -> IO Word32

foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_nand"
  syncNandFetchOldInt32AddrIO :: Addr# -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_nand"
  syncNandFetchOldInt32ArrayIO :: MutableByteArray# s -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_nand"
  syncNandFetchOldWord32AddrIO :: Addr# -> Int# -> Word32 -> IO Word32
foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_nand"
  syncNandFetchOldWord32ArrayIO :: MutableByteArray# s -> Int# -> Word32 -> IO Word32

foreign import ccall unsafe "primal_atomic.c primal_sync32_nand_fetch"
  syncNandFetchNewInt32AddrIO :: Addr# -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_nand_fetch"
  syncNandFetchNewInt32ArrayIO :: MutableByteArray# s -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_nand_fetch"
  syncNandFetchNewWord32AddrIO :: Addr# -> Int# -> Word32 -> IO Word32
foreign import ccall unsafe "primal_atomic.c primal_sync32_nand_fetch"
  syncNandFetchNewWord32ArrayIO :: MutableByteArray# s -> Int# -> Word32 -> IO Word32

foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_or"
  syncOrFetchOldInt32AddrIO :: Addr# -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_or"
  syncOrFetchOldInt32ArrayIO :: MutableByteArray# s -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_or"
  syncOrFetchOldWord32AddrIO :: Addr# -> Int# -> Word32 -> IO Word32
foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_or"
  syncOrFetchOldWord32ArrayIO :: MutableByteArray# s -> Int# -> Word32 -> IO Word32

foreign import ccall unsafe "primal_atomic.c primal_sync32_or_fetch"
  syncOrFetchNewInt32AddrIO :: Addr# -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_or_fetch"
  syncOrFetchNewInt32ArrayIO :: MutableByteArray# s -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_or_fetch"
  syncOrFetchNewWord32AddrIO :: Addr# -> Int# -> Word32 -> IO Word32
foreign import ccall unsafe "primal_atomic.c primal_sync32_or_fetch"
  syncOrFetchNewWord32ArrayIO :: MutableByteArray# s -> Int# -> Word32 -> IO Word32

foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_xor"
  syncXorFetchOldInt32AddrIO :: Addr# -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_xor"
  syncXorFetchOldInt32ArrayIO :: MutableByteArray# s -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_xor"
  syncXorFetchOldWord32AddrIO :: Addr# -> Int# -> Word32 -> IO Word32
foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_xor"
  syncXorFetchOldWord32ArrayIO :: MutableByteArray# s -> Int# -> Word32 -> IO Word32

foreign import ccall unsafe "primal_atomic.c primal_sync32_xor_fetch"
  syncXorFetchNewInt32AddrIO :: Addr# -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_xor_fetch"
  syncXorFetchNewInt32ArrayIO :: MutableByteArray# s -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_xor_fetch"
  syncXorFetchNewWord32AddrIO :: Addr# -> Int# -> Word32 -> IO Word32
foreign import ccall unsafe "primal_atomic.c primal_sync32_xor_fetch"
  syncXorFetchNewWord32ArrayIO :: MutableByteArray# s -> Int# -> Word32 -> IO Word32

foreign import ccall unsafe "primal_atomic.c primal_sync_cas_bool"
  syncCasIntBoolAddrIO :: Addr# -> Int# -> Int -> Int -> IO CBool
foreign import ccall unsafe "primal_atomic.c primal_sync_cas_bool"
  syncCasIntBoolArrayIO :: MutableByteArray# s -> Int# -> Int -> Int -> IO CBool
foreign import ccall unsafe "primal_atomic.c primal_sync_cas_bool"
  syncCasWordBoolAddrIO :: Addr# -> Int# -> Word -> Word -> IO CBool
foreign import ccall unsafe "primal_atomic.c primal_sync_cas_bool"
  syncCasWordBoolArrayIO :: MutableByteArray# s -> Int# -> Word -> Word -> IO CBool

foreign import ccall unsafe "primal_atomic.c primal_sync_cas"
  syncCasIntAddrIO :: Addr# -> Int# -> Int -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_cas"
  syncCasIntArrayIO :: MutableByteArray# s -> Int# -> Int -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_cas"
  syncCasWordAddrIO :: Addr# -> Int# -> Word -> Word -> IO Word
foreign import ccall unsafe "primal_atomic.c primal_sync_cas"
  syncCasWordArrayIO :: MutableByteArray# s -> Int# -> Word -> Word -> IO Word

foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_add"
  syncAddFetchOldIntAddrIO :: Addr# -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_add"
  syncAddFetchOldIntArrayIO :: MutableByteArray# s -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_add"
  syncAddFetchOldWordAddrIO :: Addr# -> Int# -> Word -> IO Word
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_add"
  syncAddFetchOldWordArrayIO :: MutableByteArray# s -> Int# -> Word -> IO Word

foreign import ccall unsafe "primal_atomic.c primal_sync_add_fetch"
  syncAddFetchNewIntAddrIO :: Addr# -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_add_fetch"
  syncAddFetchNewIntArrayIO :: MutableByteArray# s -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_add_fetch"
  syncAddFetchNewWordAddrIO :: Addr# -> Int# -> Word -> IO Word
foreign import ccall unsafe "primal_atomic.c primal_sync_add_fetch"
  syncAddFetchNewWordArrayIO :: MutableByteArray# s -> Int# -> Word -> IO Word

foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_sub"
  syncSubFetchOldIntAddrIO :: Addr# -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_sub"
  syncSubFetchOldIntArrayIO :: MutableByteArray# s -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_sub"
  syncSubFetchOldWordAddrIO :: Addr# -> Int# -> Word -> IO Word
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_sub"
  syncSubFetchOldWordArrayIO :: MutableByteArray# s -> Int# -> Word -> IO Word

foreign import ccall unsafe "primal_atomic.c primal_sync_sub_fetch"
  syncSubFetchNewIntAddrIO :: Addr# -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_sub_fetch"
  syncSubFetchNewIntArrayIO :: MutableByteArray# s -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_sub_fetch"
  syncSubFetchNewWordAddrIO :: Addr# -> Int# -> Word -> IO Word
foreign import ccall unsafe "primal_atomic.c primal_sync_sub_fetch"
  syncSubFetchNewWordArrayIO :: MutableByteArray# s -> Int# -> Word -> IO Word

foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_and"
  syncAndFetchOldIntAddrIO :: Addr# -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_and"
  syncAndFetchOldIntArrayIO :: MutableByteArray# s -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_and"
  syncAndFetchOldWordAddrIO :: Addr# -> Int# -> Word -> IO Word
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_and"
  syncAndFetchOldWordArrayIO :: MutableByteArray# s -> Int# -> Word -> IO Word

foreign import ccall unsafe "primal_atomic.c primal_sync_and_fetch"
  syncAndFetchNewIntAddrIO :: Addr# -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_and_fetch"
  syncAndFetchNewIntArrayIO :: MutableByteArray# s -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_and_fetch"
  syncAndFetchNewWordAddrIO :: Addr# -> Int# -> Word -> IO Word
foreign import ccall unsafe "primal_atomic.c primal_sync_and_fetch"
  syncAndFetchNewWordArrayIO :: MutableByteArray# s -> Int# -> Word -> IO Word

foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_nand"
  syncNandFetchOldIntAddrIO :: Addr# -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_nand"
  syncNandFetchOldIntArrayIO :: MutableByteArray# s -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_nand"
  syncNandFetchOldWordAddrIO :: Addr# -> Int# -> Word -> IO Word
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_nand"
  syncNandFetchOldWordArrayIO :: MutableByteArray# s -> Int# -> Word -> IO Word

foreign import ccall unsafe "primal_atomic.c primal_sync_nand_fetch"
  syncNandFetchNewIntAddrIO :: Addr# -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_nand_fetch"
  syncNandFetchNewIntArrayIO :: MutableByteArray# s -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_nand_fetch"
  syncNandFetchNewWordAddrIO :: Addr# -> Int# -> Word -> IO Word
foreign import ccall unsafe "primal_atomic.c primal_sync_nand_fetch"
  syncNandFetchNewWordArrayIO :: MutableByteArray# s -> Int# -> Word -> IO Word

foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_or"
  syncOrFetchOldIntAddrIO :: Addr# -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_or"
  syncOrFetchOldIntArrayIO :: MutableByteArray# s -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_or"
  syncOrFetchOldWordAddrIO :: Addr# -> Int# -> Word -> IO Word
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_or"
  syncOrFetchOldWordArrayIO :: MutableByteArray# s -> Int# -> Word -> IO Word

foreign import ccall unsafe "primal_atomic.c primal_sync_or_fetch"
  syncOrFetchNewIntAddrIO :: Addr# -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_or_fetch"
  syncOrFetchNewIntArrayIO :: MutableByteArray# s -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_or_fetch"
  syncOrFetchNewWordAddrIO :: Addr# -> Int# -> Word -> IO Word
foreign import ccall unsafe "primal_atomic.c primal_sync_or_fetch"
  syncOrFetchNewWordArrayIO :: MutableByteArray# s -> Int# -> Word -> IO Word

foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_xor"
  syncXorFetchOldIntAddrIO :: Addr# -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_xor"
  syncXorFetchOldIntArrayIO :: MutableByteArray# s -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_xor"
  syncXorFetchOldWordAddrIO :: Addr# -> Int# -> Word -> IO Word
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_xor"
  syncXorFetchOldWordArrayIO :: MutableByteArray# s -> Int# -> Word -> IO Word

foreign import ccall unsafe "primal_atomic.c primal_sync_xor_fetch"
  syncXorFetchNewIntAddrIO :: Addr# -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_xor_fetch"
  syncXorFetchNewIntArrayIO :: MutableByteArray# s -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_xor_fetch"
  syncXorFetchNewWordAddrIO :: Addr# -> Int# -> Word -> IO Word
foreign import ccall unsafe "primal_atomic.c primal_sync_xor_fetch"
  syncXorFetchNewWordArrayIO :: MutableByteArray# s -> Int# -> Word -> IO Word

#if WORD_SIZE_IN_BITS >= 64

foreign import ccall unsafe "primal_atomic.c primal_sync_cas_bool"
  syncCasInt64BoolAddrIO :: Addr# -> Int# -> Int64 -> Int64 -> IO CBool
foreign import ccall unsafe "primal_atomic.c primal_sync_cas_bool"
  syncCasInt64BoolArrayIO :: MutableByteArray# s -> Int# -> Int64 -> Int64 -> IO CBool
foreign import ccall unsafe "primal_atomic.c primal_sync_cas_bool"
  syncCasWord64BoolAddrIO :: Addr# -> Int# -> Word64 -> Word64 -> IO CBool
foreign import ccall unsafe "primal_atomic.c primal_sync_cas_bool"
  syncCasWord64BoolArrayIO :: MutableByteArray# s -> Int# -> Word64 -> Word64 -> IO CBool

foreign import ccall unsafe "primal_atomic.c primal_sync_cas"
  syncCasInt64AddrIO :: Addr# -> Int# -> Int64 -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_cas"
  syncCasInt64ArrayIO :: MutableByteArray# s -> Int# -> Int64 -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_cas"
  syncCasWord64AddrIO :: Addr# -> Int# -> Word64 -> Word64 -> IO Word64
foreign import ccall unsafe "primal_atomic.c primal_sync_cas"
  syncCasWord64ArrayIO :: MutableByteArray# s -> Int# -> Word64 -> Word64 -> IO Word64

foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_add"
  syncAddFetchOldInt64AddrIO :: Addr# -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_add"
  syncAddFetchOldInt64ArrayIO :: MutableByteArray# s -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_add"
  syncAddFetchOldWord64AddrIO :: Addr# -> Int# -> Word64 -> IO Word64
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_add"
  syncAddFetchOldWord64ArrayIO :: MutableByteArray# s -> Int# -> Word64 -> IO Word64

foreign import ccall unsafe "primal_atomic.c primal_sync_add_fetch"
  syncAddFetchNewInt64AddrIO :: Addr# -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_add_fetch"
  syncAddFetchNewInt64ArrayIO :: MutableByteArray# s -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_add_fetch"
  syncAddFetchNewWord64AddrIO :: Addr# -> Int# -> Word64 -> IO Word64
foreign import ccall unsafe "primal_atomic.c primal_sync_add_fetch"
  syncAddFetchNewWord64ArrayIO :: MutableByteArray# s -> Int# -> Word64 -> IO Word64


foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_sub"
  syncSubFetchOldInt64AddrIO :: Addr# -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_sub"
  syncSubFetchOldInt64ArrayIO :: MutableByteArray# s -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_sub"
  syncSubFetchOldWord64AddrIO :: Addr# -> Int# -> Word64 -> IO Word64
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_sub"
  syncSubFetchOldWord64ArrayIO :: MutableByteArray# s -> Int# -> Word64 -> IO Word64

foreign import ccall unsafe "primal_atomic.c primal_sync_sub_fetch"
  syncSubFetchNewInt64AddrIO :: Addr# -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_sub_fetch"
  syncSubFetchNewInt64ArrayIO :: MutableByteArray# s -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_sub_fetch"
  syncSubFetchNewWord64AddrIO :: Addr# -> Int# -> Word64 -> IO Word64
foreign import ccall unsafe "primal_atomic.c primal_sync_sub_fetch"
  syncSubFetchNewWord64ArrayIO :: MutableByteArray# s -> Int# -> Word64 -> IO Word64


foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_and"
  syncAndFetchOldInt64AddrIO :: Addr# -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_and"
  syncAndFetchOldInt64ArrayIO :: MutableByteArray# s -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_and"
  syncAndFetchOldWord64AddrIO :: Addr# -> Int# -> Word64 -> IO Word64
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_and"
  syncAndFetchOldWord64ArrayIO :: MutableByteArray# s -> Int# -> Word64 -> IO Word64

foreign import ccall unsafe "primal_atomic.c primal_sync_and_fetch"
  syncAndFetchNewInt64AddrIO :: Addr# -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_and_fetch"
  syncAndFetchNewInt64ArrayIO :: MutableByteArray# s -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_and_fetch"
  syncAndFetchNewWord64AddrIO :: Addr# -> Int# -> Word64 -> IO Word64
foreign import ccall unsafe "primal_atomic.c primal_sync_and_fetch"
  syncAndFetchNewWord64ArrayIO :: MutableByteArray# s -> Int# -> Word64 -> IO Word64


foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_nand"
  syncNandFetchOldInt64AddrIO :: Addr# -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_nand"
  syncNandFetchOldInt64ArrayIO :: MutableByteArray# s -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_nand"
  syncNandFetchOldWord64AddrIO :: Addr# -> Int# -> Word64 -> IO Word64
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_nand"
  syncNandFetchOldWord64ArrayIO :: MutableByteArray# s -> Int# -> Word64 -> IO Word64

foreign import ccall unsafe "primal_atomic.c primal_sync_nand_fetch"
  syncNandFetchNewInt64AddrIO :: Addr# -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_nand_fetch"
  syncNandFetchNewInt64ArrayIO :: MutableByteArray# s -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_nand_fetch"
  syncNandFetchNewWord64AddrIO :: Addr# -> Int# -> Word64 -> IO Word64
foreign import ccall unsafe "primal_atomic.c primal_sync_nand_fetch"
  syncNandFetchNewWord64ArrayIO :: MutableByteArray# s -> Int# -> Word64 -> IO Word64


foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_or"
  syncOrFetchOldInt64AddrIO :: Addr# -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_or"
  syncOrFetchOldInt64ArrayIO :: MutableByteArray# s -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_or"
  syncOrFetchOldWord64AddrIO :: Addr# -> Int# -> Word64 -> IO Word64
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_or"
  syncOrFetchOldWord64ArrayIO :: MutableByteArray# s -> Int# -> Word64 -> IO Word64

foreign import ccall unsafe "primal_atomic.c primal_sync_or_fetch"
  syncOrFetchNewInt64AddrIO :: Addr# -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_or_fetch"
  syncOrFetchNewInt64ArrayIO :: MutableByteArray# s -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_or_fetch"
  syncOrFetchNewWord64AddrIO :: Addr# -> Int# -> Word64 -> IO Word64
foreign import ccall unsafe "primal_atomic.c primal_sync_or_fetch"
  syncOrFetchNewWord64ArrayIO :: MutableByteArray# s -> Int# -> Word64 -> IO Word64


foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_xor"
  syncXorFetchOldInt64AddrIO :: Addr# -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_xor"
  syncXorFetchOldInt64ArrayIO :: MutableByteArray# s -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_xor"
  syncXorFetchOldWord64AddrIO :: Addr# -> Int# -> Word64 -> IO Word64
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_xor"
  syncXorFetchOldWord64ArrayIO :: MutableByteArray# s -> Int# -> Word64 -> IO Word64

foreign import ccall unsafe "primal_atomic.c primal_sync_xor_fetch"
  syncXorFetchNewInt64AddrIO :: Addr# -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_xor_fetch"
  syncXorFetchNewInt64ArrayIO :: MutableByteArray# s -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_xor_fetch"
  syncXorFetchNewWord64AddrIO :: Addr# -> Int# -> Word64 -> IO Word64
foreign import ccall unsafe "primal_atomic.c primal_sync_xor_fetch"
  syncXorFetchNewWord64ArrayIO :: MutableByteArray# s -> Int# -> Word64 -> IO Word64

#endif
