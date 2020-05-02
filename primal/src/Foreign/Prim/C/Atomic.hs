{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_HADDOCK hide, not-home #-}
-- |
-- Module      : Foreign.Prim.C.Atomic
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Foreign.Prim.C.Atomic
  ( module Foreign.Prim.C.Atomic
  ) where

import Control.Prim.Monad.Unsafe
import GHC.Exts
import Data.Int
import Data.Word

#include "MachDeps.h"


-- | [Memory barrier](https://en.wikipedia.org/wiki/Memory_barrier). This will
-- ensure that the cache is fully updated before continuing.
syncSynchronize# :: State# s -> State# s
syncSynchronize# = unsafePrimBase_ syncSynchronize
{-# INLINE syncSynchronize# #-}

withMemBarrier# :: (State# s -> (# State# s, a #)) -> State# s -> (# State# s, a #)
withMemBarrier# f s = f (syncSynchronize# s)
{-# INLINE withMemBarrier# #-}

withMemBarrier_# :: (State# s -> State# s) -> State# s -> State# s
withMemBarrier_# f s = f (syncSynchronize# s)
{-# INLINE withMemBarrier_# #-}

foreign import ccall unsafe "primal_atomic.c primal_sync_synchronize"
  syncSynchronize :: IO ()


foreign import ccall unsafe "primal_atomic.c primal_sync_lock_test_set"
  syncLockTestSetIntArrayIO :: MutableByteArray# s -> Int# -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_lock_test_set"
  syncLockTestSetIntAddrIO :: Addr# -> Int# -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_lock_release"
  syncLockReleaseIntArrayIO :: MutableByteArray# s -> Int# -> IO ()
foreign import ccall unsafe "primal_atomic.c primal_sync_lock_release"
  syncLockReleaseIntAddrIO :: Addr# -> Int# -> IO ()


foreign import ccall unsafe "primal_atomic.c primal_sync8_cas"
  syncCasInt8AddrIO :: Addr# -> Int# -> Int8 -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_cas"
  syncCasInt8ArrayIO :: MutableByteArray# s -> Int# -> Int8 -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_cas"
  syncCasWord8AddrIO :: Addr# -> Int# -> Word8 -> Word8 -> IO Word8
foreign import ccall unsafe "primal_atomic.c primal_sync8_cas"
  syncCasWord8ArrayIO :: MutableByteArray# s -> Int# -> Word8 -> Word8 -> IO Word8

foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_add"
  syncFetchAddInt8AddrIO :: Addr# -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_add"
  syncFetchAddInt8ArrayIO :: MutableByteArray# s -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_add"
  syncFetchAddWord8AddrIO :: Addr# -> Int# -> Word8 -> IO Word8
foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_add"
  syncFetchAddWord8ArrayIO :: MutableByteArray# s -> Int# -> Word8 -> IO Word8

foreign import ccall unsafe "primal_atomic.c primal_sync8_add_fetch"
  syncAddFetchInt8AddrIO :: Addr# -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_add_fetch"
  syncAddFetchInt8ArrayIO :: MutableByteArray# s -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_add_fetch"
  syncAddFetchWord8AddrIO :: Addr# -> Int# -> Word8 -> IO Word8
foreign import ccall unsafe "primal_atomic.c primal_sync8_add_fetch"
  syncAddFetchWord8ArrayIO :: MutableByteArray# s -> Int# -> Word8 -> IO Word8


foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_sub"
  syncFetchSubInt8AddrIO :: Addr# -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_sub"
  syncFetchSubInt8ArrayIO :: MutableByteArray# s -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_sub"
  syncFetchSubWord8AddrIO :: Addr# -> Int# -> Word8 -> IO Word8
foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_sub"
  syncFetchSubWord8ArrayIO :: MutableByteArray# s -> Int# -> Word8 -> IO Word8

foreign import ccall unsafe "primal_atomic.c primal_sync8_sub_fetch"
  syncSubFetchInt8AddrIO :: Addr# -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_sub_fetch"
  syncSubFetchInt8ArrayIO :: MutableByteArray# s -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_sub_fetch"
  syncSubFetchWord8AddrIO :: Addr# -> Int# -> Word8 -> IO Word8
foreign import ccall unsafe "primal_atomic.c primal_sync8_sub_fetch"
  syncSubFetchWord8ArrayIO :: MutableByteArray# s -> Int# -> Word8 -> IO Word8


foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_and"
  syncFetchAndInt8AddrIO :: Addr# -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_and"
  syncFetchAndInt8ArrayIO :: MutableByteArray# s -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_and"
  syncFetchAndWord8AddrIO :: Addr# -> Int# -> Word8 -> IO Word8
foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_and"
  syncFetchAndWord8ArrayIO :: MutableByteArray# s -> Int# -> Word8 -> IO Word8

foreign import ccall unsafe "primal_atomic.c primal_sync8_and_fetch"
  syncAndFetchInt8AddrIO :: Addr# -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_and_fetch"
  syncAndFetchInt8ArrayIO :: MutableByteArray# s -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_and_fetch"
  syncAndFetchWord8AddrIO :: Addr# -> Int# -> Word8 -> IO Word8
foreign import ccall unsafe "primal_atomic.c primal_sync8_and_fetch"
  syncAndFetchWord8ArrayIO :: MutableByteArray# s -> Int# -> Word8 -> IO Word8


foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_nand"
  syncFetchNandInt8AddrIO :: Addr# -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_nand"
  syncFetchNandInt8ArrayIO :: MutableByteArray# s -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_nand"
  syncFetchNandWord8AddrIO :: Addr# -> Int# -> Word8 -> IO Word8
foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_nand"
  syncFetchNandWord8ArrayIO :: MutableByteArray# s -> Int# -> Word8 -> IO Word8

foreign import ccall unsafe "primal_atomic.c primal_sync8_nand_fetch"
  syncNandFetchInt8AddrIO :: Addr# -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_nand_fetch"
  syncNandFetchInt8ArrayIO :: MutableByteArray# s -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_nand_fetch"
  syncNandFetchWord8AddrIO :: Addr# -> Int# -> Word8 -> IO Word8
foreign import ccall unsafe "primal_atomic.c primal_sync8_nand_fetch"
  syncNandFetchWord8ArrayIO :: MutableByteArray# s -> Int# -> Word8 -> IO Word8


foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_or"
  syncFetchOrInt8AddrIO :: Addr# -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_or"
  syncFetchOrInt8ArrayIO :: MutableByteArray# s -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_or"
  syncFetchOrWord8AddrIO :: Addr# -> Int# -> Word8 -> IO Word8
foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_or"
  syncFetchOrWord8ArrayIO :: MutableByteArray# s -> Int# -> Word8 -> IO Word8

foreign import ccall unsafe "primal_atomic.c primal_sync8_or_fetch"
  syncOrFetchInt8AddrIO :: Addr# -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_or_fetch"
  syncOrFetchInt8ArrayIO :: MutableByteArray# s -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_or_fetch"
  syncOrFetchWord8AddrIO :: Addr# -> Int# -> Word8 -> IO Word8
foreign import ccall unsafe "primal_atomic.c primal_sync8_or_fetch"
  syncOrFetchWord8ArrayIO :: MutableByteArray# s -> Int# -> Word8 -> IO Word8


foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_xor"
  syncFetchXorInt8AddrIO :: Addr# -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_xor"
  syncFetchXorInt8ArrayIO :: MutableByteArray# s -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_xor"
  syncFetchXorWord8AddrIO :: Addr# -> Int# -> Word8 -> IO Word8
foreign import ccall unsafe "primal_atomic.c primal_sync8_fetch_xor"
  syncFetchXorWord8ArrayIO :: MutableByteArray# s -> Int# -> Word8 -> IO Word8

foreign import ccall unsafe "primal_atomic.c primal_sync8_xor_fetch"
  syncXorFetchInt8AddrIO :: Addr# -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_xor_fetch"
  syncXorFetchInt8ArrayIO :: MutableByteArray# s -> Int# -> Int8 -> IO Int8
foreign import ccall unsafe "primal_atomic.c primal_sync8_xor_fetch"
  syncXorFetchWord8AddrIO :: Addr# -> Int# -> Word8 -> IO Word8
foreign import ccall unsafe "primal_atomic.c primal_sync8_xor_fetch"
  syncXorFetchWord8ArrayIO :: MutableByteArray# s -> Int# -> Word8 -> IO Word8









foreign import ccall unsafe "primal_atomic.c primal_sync16_cas"
  syncCasInt16AddrIO :: Addr# -> Int# -> Int16 -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_cas"
  syncCasInt16ArrayIO :: MutableByteArray# s -> Int# -> Int16 -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_cas"
  syncCasWord16AddrIO :: Addr# -> Int# -> Word16 -> Word16 -> IO Word16
foreign import ccall unsafe "primal_atomic.c primal_sync16_cas"
  syncCasWord16ArrayIO :: MutableByteArray# s -> Int# -> Word16 -> Word16 -> IO Word16

foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_add"
  syncFetchAddInt16AddrIO :: Addr# -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_add"
  syncFetchAddInt16ArrayIO :: MutableByteArray# s -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_add"
  syncFetchAddWord16AddrIO :: Addr# -> Int# -> Word16 -> IO Word16
foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_add"
  syncFetchAddWord16ArrayIO :: MutableByteArray# s -> Int# -> Word16 -> IO Word16

foreign import ccall unsafe "primal_atomic.c primal_sync16_add_fetch"
  syncAddFetchInt16AddrIO :: Addr# -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_add_fetch"
  syncAddFetchInt16ArrayIO :: MutableByteArray# s -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_add_fetch"
  syncAddFetchWord16AddrIO :: Addr# -> Int# -> Word16 -> IO Word16
foreign import ccall unsafe "primal_atomic.c primal_sync16_add_fetch"
  syncAddFetchWord16ArrayIO :: MutableByteArray# s -> Int# -> Word16 -> IO Word16


foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_sub"
  syncFetchSubInt16AddrIO :: Addr# -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_sub"
  syncFetchSubInt16ArrayIO :: MutableByteArray# s -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_sub"
  syncFetchSubWord16AddrIO :: Addr# -> Int# -> Word16 -> IO Word16
foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_sub"
  syncFetchSubWord16ArrayIO :: MutableByteArray# s -> Int# -> Word16 -> IO Word16

foreign import ccall unsafe "primal_atomic.c primal_sync16_sub_fetch"
  syncSubFetchInt16AddrIO :: Addr# -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_sub_fetch"
  syncSubFetchInt16ArrayIO :: MutableByteArray# s -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_sub_fetch"
  syncSubFetchWord16AddrIO :: Addr# -> Int# -> Word16 -> IO Word16
foreign import ccall unsafe "primal_atomic.c primal_sync16_sub_fetch"
  syncSubFetchWord16ArrayIO :: MutableByteArray# s -> Int# -> Word16 -> IO Word16


foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_and"
  syncFetchAndInt16AddrIO :: Addr# -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_and"
  syncFetchAndInt16ArrayIO :: MutableByteArray# s -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_and"
  syncFetchAndWord16AddrIO :: Addr# -> Int# -> Word16 -> IO Word16
foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_and"
  syncFetchAndWord16ArrayIO :: MutableByteArray# s -> Int# -> Word16 -> IO Word16

foreign import ccall unsafe "primal_atomic.c primal_sync16_and_fetch"
  syncAndFetchInt16AddrIO :: Addr# -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_and_fetch"
  syncAndFetchInt16ArrayIO :: MutableByteArray# s -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_and_fetch"
  syncAndFetchWord16AddrIO :: Addr# -> Int# -> Word16 -> IO Word16
foreign import ccall unsafe "primal_atomic.c primal_sync16_and_fetch"
  syncAndFetchWord16ArrayIO :: MutableByteArray# s -> Int# -> Word16 -> IO Word16


foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_nand"
  syncFetchNandInt16AddrIO :: Addr# -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_nand"
  syncFetchNandInt16ArrayIO :: MutableByteArray# s -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_nand"
  syncFetchNandWord16AddrIO :: Addr# -> Int# -> Word16 -> IO Word16
foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_nand"
  syncFetchNandWord16ArrayIO :: MutableByteArray# s -> Int# -> Word16 -> IO Word16

foreign import ccall unsafe "primal_atomic.c primal_sync16_nand_fetch"
  syncNandFetchInt16AddrIO :: Addr# -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_nand_fetch"
  syncNandFetchInt16ArrayIO :: MutableByteArray# s -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_nand_fetch"
  syncNandFetchWord16AddrIO :: Addr# -> Int# -> Word16 -> IO Word16
foreign import ccall unsafe "primal_atomic.c primal_sync16_nand_fetch"
  syncNandFetchWord16ArrayIO :: MutableByteArray# s -> Int# -> Word16 -> IO Word16


foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_or"
  syncFetchOrInt16AddrIO :: Addr# -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_or"
  syncFetchOrInt16ArrayIO :: MutableByteArray# s -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_or"
  syncFetchOrWord16AddrIO :: Addr# -> Int# -> Word16 -> IO Word16
foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_or"
  syncFetchOrWord16ArrayIO :: MutableByteArray# s -> Int# -> Word16 -> IO Word16

foreign import ccall unsafe "primal_atomic.c primal_sync16_or_fetch"
  syncOrFetchInt16AddrIO :: Addr# -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_or_fetch"
  syncOrFetchInt16ArrayIO :: MutableByteArray# s -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_or_fetch"
  syncOrFetchWord16AddrIO :: Addr# -> Int# -> Word16 -> IO Word16
foreign import ccall unsafe "primal_atomic.c primal_sync16_or_fetch"
  syncOrFetchWord16ArrayIO :: MutableByteArray# s -> Int# -> Word16 -> IO Word16


foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_xor"
  syncFetchXorInt16AddrIO :: Addr# -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_xor"
  syncFetchXorInt16ArrayIO :: MutableByteArray# s -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_xor"
  syncFetchXorWord16AddrIO :: Addr# -> Int# -> Word16 -> IO Word16
foreign import ccall unsafe "primal_atomic.c primal_sync16_fetch_xor"
  syncFetchXorWord16ArrayIO :: MutableByteArray# s -> Int# -> Word16 -> IO Word16

foreign import ccall unsafe "primal_atomic.c primal_sync16_xor_fetch"
  syncXorFetchInt16AddrIO :: Addr# -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_xor_fetch"
  syncXorFetchInt16ArrayIO :: MutableByteArray# s -> Int# -> Int16 -> IO Int16
foreign import ccall unsafe "primal_atomic.c primal_sync16_xor_fetch"
  syncXorFetchWord16AddrIO :: Addr# -> Int# -> Word16 -> IO Word16
foreign import ccall unsafe "primal_atomic.c primal_sync16_xor_fetch"
  syncXorFetchWord16ArrayIO :: MutableByteArray# s -> Int# -> Word16 -> IO Word16




foreign import ccall unsafe "primal_atomic.c primal_sync32_cas"
  syncCasInt32AddrIO :: Addr# -> Int# -> Int32 -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_cas"
  syncCasInt32ArrayIO :: MutableByteArray# s -> Int# -> Int32 -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_cas"
  syncCasWord32AddrIO :: Addr# -> Int# -> Word32 -> Word32 -> IO Word32
foreign import ccall unsafe "primal_atomic.c primal_sync32_cas"
  syncCasWord32ArrayIO :: MutableByteArray# s -> Int# -> Word32 -> Word32 -> IO Word32

foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_add"
  syncFetchAddInt32AddrIO :: Addr# -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_add"
  syncFetchAddInt32ArrayIO :: MutableByteArray# s -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_add"
  syncFetchAddWord32AddrIO :: Addr# -> Int# -> Word32 -> IO Word32
foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_add"
  syncFetchAddWord32ArrayIO :: MutableByteArray# s -> Int# -> Word32 -> IO Word32

foreign import ccall unsafe "primal_atomic.c primal_sync32_add_fetch"
  syncAddFetchInt32AddrIO :: Addr# -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_add_fetch"
  syncAddFetchInt32ArrayIO :: MutableByteArray# s -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_add_fetch"
  syncAddFetchWord32AddrIO :: Addr# -> Int# -> Word32 -> IO Word32
foreign import ccall unsafe "primal_atomic.c primal_sync32_add_fetch"
  syncAddFetchWord32ArrayIO :: MutableByteArray# s -> Int# -> Word32 -> IO Word32


foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_sub"
  syncFetchSubInt32AddrIO :: Addr# -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_sub"
  syncFetchSubInt32ArrayIO :: MutableByteArray# s -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_sub"
  syncFetchSubWord32AddrIO :: Addr# -> Int# -> Word32 -> IO Word32
foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_sub"
  syncFetchSubWord32ArrayIO :: MutableByteArray# s -> Int# -> Word32 -> IO Word32

foreign import ccall unsafe "primal_atomic.c primal_sync32_sub_fetch"
  syncSubFetchInt32AddrIO :: Addr# -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_sub_fetch"
  syncSubFetchInt32ArrayIO :: MutableByteArray# s -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_sub_fetch"
  syncSubFetchWord32AddrIO :: Addr# -> Int# -> Word32 -> IO Word32
foreign import ccall unsafe "primal_atomic.c primal_sync32_sub_fetch"
  syncSubFetchWord32ArrayIO :: MutableByteArray# s -> Int# -> Word32 -> IO Word32


foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_and"
  syncFetchAndInt32AddrIO :: Addr# -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_and"
  syncFetchAndInt32ArrayIO :: MutableByteArray# s -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_and"
  syncFetchAndWord32AddrIO :: Addr# -> Int# -> Word32 -> IO Word32
foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_and"
  syncFetchAndWord32ArrayIO :: MutableByteArray# s -> Int# -> Word32 -> IO Word32

foreign import ccall unsafe "primal_atomic.c primal_sync32_and_fetch"
  syncAndFetchInt32AddrIO :: Addr# -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_and_fetch"
  syncAndFetchInt32ArrayIO :: MutableByteArray# s -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_and_fetch"
  syncAndFetchWord32AddrIO :: Addr# -> Int# -> Word32 -> IO Word32
foreign import ccall unsafe "primal_atomic.c primal_sync32_and_fetch"
  syncAndFetchWord32ArrayIO :: MutableByteArray# s -> Int# -> Word32 -> IO Word32


foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_nand"
  syncFetchNandInt32AddrIO :: Addr# -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_nand"
  syncFetchNandInt32ArrayIO :: MutableByteArray# s -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_nand"
  syncFetchNandWord32AddrIO :: Addr# -> Int# -> Word32 -> IO Word32
foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_nand"
  syncFetchNandWord32ArrayIO :: MutableByteArray# s -> Int# -> Word32 -> IO Word32

foreign import ccall unsafe "primal_atomic.c primal_sync32_nand_fetch"
  syncNandFetchInt32AddrIO :: Addr# -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_nand_fetch"
  syncNandFetchInt32ArrayIO :: MutableByteArray# s -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_nand_fetch"
  syncNandFetchWord32AddrIO :: Addr# -> Int# -> Word32 -> IO Word32
foreign import ccall unsafe "primal_atomic.c primal_sync32_nand_fetch"
  syncNandFetchWord32ArrayIO :: MutableByteArray# s -> Int# -> Word32 -> IO Word32


foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_or"
  syncFetchOrInt32AddrIO :: Addr# -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_or"
  syncFetchOrInt32ArrayIO :: MutableByteArray# s -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_or"
  syncFetchOrWord32AddrIO :: Addr# -> Int# -> Word32 -> IO Word32
foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_or"
  syncFetchOrWord32ArrayIO :: MutableByteArray# s -> Int# -> Word32 -> IO Word32

foreign import ccall unsafe "primal_atomic.c primal_sync32_or_fetch"
  syncOrFetchInt32AddrIO :: Addr# -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_or_fetch"
  syncOrFetchInt32ArrayIO :: MutableByteArray# s -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_or_fetch"
  syncOrFetchWord32AddrIO :: Addr# -> Int# -> Word32 -> IO Word32
foreign import ccall unsafe "primal_atomic.c primal_sync32_or_fetch"
  syncOrFetchWord32ArrayIO :: MutableByteArray# s -> Int# -> Word32 -> IO Word32


foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_xor"
  syncFetchXorInt32AddrIO :: Addr# -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_xor"
  syncFetchXorInt32ArrayIO :: MutableByteArray# s -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_xor"
  syncFetchXorWord32AddrIO :: Addr# -> Int# -> Word32 -> IO Word32
foreign import ccall unsafe "primal_atomic.c primal_sync32_fetch_xor"
  syncFetchXorWord32ArrayIO :: MutableByteArray# s -> Int# -> Word32 -> IO Word32

foreign import ccall unsafe "primal_atomic.c primal_sync32_xor_fetch"
  syncXorFetchInt32AddrIO :: Addr# -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_xor_fetch"
  syncXorFetchInt32ArrayIO :: MutableByteArray# s -> Int# -> Int32 -> IO Int32
foreign import ccall unsafe "primal_atomic.c primal_sync32_xor_fetch"
  syncXorFetchWord32AddrIO :: Addr# -> Int# -> Word32 -> IO Word32
foreign import ccall unsafe "primal_atomic.c primal_sync32_xor_fetch"
  syncXorFetchWord32ArrayIO :: MutableByteArray# s -> Int# -> Word32 -> IO Word32





foreign import ccall unsafe "primal_atomic.c primal_sync_cas"
  syncCasIntAddrIO :: Addr# -> Int# -> Int -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_cas"
  syncCasIntArrayIO :: MutableByteArray# s -> Int# -> Int -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_cas"
  syncCasWordAddrIO :: Addr# -> Int# -> Word -> Word -> IO Word
foreign import ccall unsafe "primal_atomic.c primal_sync_cas"
  syncCasWordArrayIO :: MutableByteArray# s -> Int# -> Word -> Word -> IO Word

foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_add"
  syncFetchAddIntAddrIO :: Addr# -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_add"
  syncFetchAddIntArrayIO :: MutableByteArray# s -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_add"
  syncFetchAddWordAddrIO :: Addr# -> Int# -> Word -> IO Word
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_add"
  syncFetchAddWordArrayIO :: MutableByteArray# s -> Int# -> Word -> IO Word

foreign import ccall unsafe "primal_atomic.c primal_sync_add_fetch"
  syncAddFetchIntAddrIO :: Addr# -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_add_fetch"
  syncAddFetchIntArrayIO :: MutableByteArray# s -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_add_fetch"
  syncAddFetchWordAddrIO :: Addr# -> Int# -> Word -> IO Word
foreign import ccall unsafe "primal_atomic.c primal_sync_add_fetch"
  syncAddFetchWordArrayIO :: MutableByteArray# s -> Int# -> Word -> IO Word


foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_sub"
  syncFetchSubIntAddrIO :: Addr# -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_sub"
  syncFetchSubIntArrayIO :: MutableByteArray# s -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_sub"
  syncFetchSubWordAddrIO :: Addr# -> Int# -> Word -> IO Word
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_sub"
  syncFetchSubWordArrayIO :: MutableByteArray# s -> Int# -> Word -> IO Word

foreign import ccall unsafe "primal_atomic.c primal_sync_sub_fetch"
  syncSubFetchIntAddrIO :: Addr# -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_sub_fetch"
  syncSubFetchIntArrayIO :: MutableByteArray# s -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_sub_fetch"
  syncSubFetchWordAddrIO :: Addr# -> Int# -> Word -> IO Word
foreign import ccall unsafe "primal_atomic.c primal_sync_sub_fetch"
  syncSubFetchWordArrayIO :: MutableByteArray# s -> Int# -> Word -> IO Word


foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_and"
  syncFetchAndIntAddrIO :: Addr# -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_and"
  syncFetchAndIntArrayIO :: MutableByteArray# s -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_and"
  syncFetchAndWordAddrIO :: Addr# -> Int# -> Word -> IO Word
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_and"
  syncFetchAndWordArrayIO :: MutableByteArray# s -> Int# -> Word -> IO Word

foreign import ccall unsafe "primal_atomic.c primal_sync_and_fetch"
  syncAndFetchIntAddrIO :: Addr# -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_and_fetch"
  syncAndFetchIntArrayIO :: MutableByteArray# s -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_and_fetch"
  syncAndFetchWordAddrIO :: Addr# -> Int# -> Word -> IO Word
foreign import ccall unsafe "primal_atomic.c primal_sync_and_fetch"
  syncAndFetchWordArrayIO :: MutableByteArray# s -> Int# -> Word -> IO Word


foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_nand"
  syncFetchNandIntAddrIO :: Addr# -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_nand"
  syncFetchNandIntArrayIO :: MutableByteArray# s -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_nand"
  syncFetchNandWordAddrIO :: Addr# -> Int# -> Word -> IO Word
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_nand"
  syncFetchNandWordArrayIO :: MutableByteArray# s -> Int# -> Word -> IO Word

foreign import ccall unsafe "primal_atomic.c primal_sync_nand_fetch"
  syncNandFetchIntAddrIO :: Addr# -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_nand_fetch"
  syncNandFetchIntArrayIO :: MutableByteArray# s -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_nand_fetch"
  syncNandFetchWordAddrIO :: Addr# -> Int# -> Word -> IO Word
foreign import ccall unsafe "primal_atomic.c primal_sync_nand_fetch"
  syncNandFetchWordArrayIO :: MutableByteArray# s -> Int# -> Word -> IO Word


foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_or"
  syncFetchOrIntAddrIO :: Addr# -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_or"
  syncFetchOrIntArrayIO :: MutableByteArray# s -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_or"
  syncFetchOrWordAddrIO :: Addr# -> Int# -> Word -> IO Word
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_or"
  syncFetchOrWordArrayIO :: MutableByteArray# s -> Int# -> Word -> IO Word

foreign import ccall unsafe "primal_atomic.c primal_sync_or_fetch"
  syncOrFetchIntAddrIO :: Addr# -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_or_fetch"
  syncOrFetchIntArrayIO :: MutableByteArray# s -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_or_fetch"
  syncOrFetchWordAddrIO :: Addr# -> Int# -> Word -> IO Word
foreign import ccall unsafe "primal_atomic.c primal_sync_or_fetch"
  syncOrFetchWordArrayIO :: MutableByteArray# s -> Int# -> Word -> IO Word


foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_xor"
  syncFetchXorIntAddrIO :: Addr# -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_xor"
  syncFetchXorIntArrayIO :: MutableByteArray# s -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_xor"
  syncFetchXorWordAddrIO :: Addr# -> Int# -> Word -> IO Word
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_xor"
  syncFetchXorWordArrayIO :: MutableByteArray# s -> Int# -> Word -> IO Word

foreign import ccall unsafe "primal_atomic.c primal_sync_xor_fetch"
  syncXorFetchIntAddrIO :: Addr# -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_xor_fetch"
  syncXorFetchIntArrayIO :: MutableByteArray# s -> Int# -> Int -> IO Int
foreign import ccall unsafe "primal_atomic.c primal_sync_xor_fetch"
  syncXorFetchWordAddrIO :: Addr# -> Int# -> Word -> IO Word
foreign import ccall unsafe "primal_atomic.c primal_sync_xor_fetch"
  syncXorFetchWordArrayIO :: MutableByteArray# s -> Int# -> Word -> IO Word




#if WORD_SIZE_IN_BITS >= 64

foreign import ccall unsafe "primal_atomic.c primal_sync_cas"
  syncCasInt64AddrIO :: Addr# -> Int# -> Int64 -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_cas"
  syncCasInt64ArrayIO :: MutableByteArray# s -> Int# -> Int64 -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_cas"
  syncCasWord64AddrIO :: Addr# -> Int# -> Word64 -> Word64 -> IO Word64
foreign import ccall unsafe "primal_atomic.c primal_sync_cas"
  syncCasWord64ArrayIO :: MutableByteArray# s -> Int# -> Word64 -> Word64 -> IO Word64

foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_add"
  syncFetchAddInt64AddrIO :: Addr# -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_add"
  syncFetchAddInt64ArrayIO :: MutableByteArray# s -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_add"
  syncFetchAddWord64AddrIO :: Addr# -> Int# -> Word64 -> IO Word64
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_add"
  syncFetchAddWord64ArrayIO :: MutableByteArray# s -> Int# -> Word64 -> IO Word64

foreign import ccall unsafe "primal_atomic.c primal_sync_add_fetch"
  syncAddFetchInt64AddrIO :: Addr# -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_add_fetch"
  syncAddFetchInt64ArrayIO :: MutableByteArray# s -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_add_fetch"
  syncAddFetchWord64AddrIO :: Addr# -> Int# -> Word64 -> IO Word64
foreign import ccall unsafe "primal_atomic.c primal_sync_add_fetch"
  syncAddFetchWord64ArrayIO :: MutableByteArray# s -> Int# -> Word64 -> IO Word64


foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_sub"
  syncFetchSubInt64AddrIO :: Addr# -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_sub"
  syncFetchSubInt64ArrayIO :: MutableByteArray# s -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_sub"
  syncFetchSubWord64AddrIO :: Addr# -> Int# -> Word64 -> IO Word64
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_sub"
  syncFetchSubWord64ArrayIO :: MutableByteArray# s -> Int# -> Word64 -> IO Word64

foreign import ccall unsafe "primal_atomic.c primal_sync_sub_fetch"
  syncSubFetchInt64AddrIO :: Addr# -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_sub_fetch"
  syncSubFetchInt64ArrayIO :: MutableByteArray# s -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_sub_fetch"
  syncSubFetchWord64AddrIO :: Addr# -> Int# -> Word64 -> IO Word64
foreign import ccall unsafe "primal_atomic.c primal_sync_sub_fetch"
  syncSubFetchWord64ArrayIO :: MutableByteArray# s -> Int# -> Word64 -> IO Word64


foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_and"
  syncFetchAndInt64AddrIO :: Addr# -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_and"
  syncFetchAndInt64ArrayIO :: MutableByteArray# s -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_and"
  syncFetchAndWord64AddrIO :: Addr# -> Int# -> Word64 -> IO Word64
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_and"
  syncFetchAndWord64ArrayIO :: MutableByteArray# s -> Int# -> Word64 -> IO Word64

foreign import ccall unsafe "primal_atomic.c primal_sync_and_fetch"
  syncAndFetchInt64AddrIO :: Addr# -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_and_fetch"
  syncAndFetchInt64ArrayIO :: MutableByteArray# s -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_and_fetch"
  syncAndFetchWord64AddrIO :: Addr# -> Int# -> Word64 -> IO Word64
foreign import ccall unsafe "primal_atomic.c primal_sync_and_fetch"
  syncAndFetchWord64ArrayIO :: MutableByteArray# s -> Int# -> Word64 -> IO Word64


foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_nand"
  syncFetchNandInt64AddrIO :: Addr# -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_nand"
  syncFetchNandInt64ArrayIO :: MutableByteArray# s -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_nand"
  syncFetchNandWord64AddrIO :: Addr# -> Int# -> Word64 -> IO Word64
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_nand"
  syncFetchNandWord64ArrayIO :: MutableByteArray# s -> Int# -> Word64 -> IO Word64

foreign import ccall unsafe "primal_atomic.c primal_sync_nand_fetch"
  syncNandFetchInt64AddrIO :: Addr# -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_nand_fetch"
  syncNandFetchInt64ArrayIO :: MutableByteArray# s -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_nand_fetch"
  syncNandFetchWord64AddrIO :: Addr# -> Int# -> Word64 -> IO Word64
foreign import ccall unsafe "primal_atomic.c primal_sync_nand_fetch"
  syncNandFetchWord64ArrayIO :: MutableByteArray# s -> Int# -> Word64 -> IO Word64


foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_or"
  syncFetchOrInt64AddrIO :: Addr# -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_or"
  syncFetchOrInt64ArrayIO :: MutableByteArray# s -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_or"
  syncFetchOrWord64AddrIO :: Addr# -> Int# -> Word64 -> IO Word64
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_or"
  syncFetchOrWord64ArrayIO :: MutableByteArray# s -> Int# -> Word64 -> IO Word64

foreign import ccall unsafe "primal_atomic.c primal_sync_or_fetch"
  syncOrFetchInt64AddrIO :: Addr# -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_or_fetch"
  syncOrFetchInt64ArrayIO :: MutableByteArray# s -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_or_fetch"
  syncOrFetchWord64AddrIO :: Addr# -> Int# -> Word64 -> IO Word64
foreign import ccall unsafe "primal_atomic.c primal_sync_or_fetch"
  syncOrFetchWord64ArrayIO :: MutableByteArray# s -> Int# -> Word64 -> IO Word64


foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_xor"
  syncFetchXorInt64AddrIO :: Addr# -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_xor"
  syncFetchXorInt64ArrayIO :: MutableByteArray# s -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_xor"
  syncFetchXorWord64AddrIO :: Addr# -> Int# -> Word64 -> IO Word64
foreign import ccall unsafe "primal_atomic.c primal_sync_fetch_xor"
  syncFetchXorWord64ArrayIO :: MutableByteArray# s -> Int# -> Word64 -> IO Word64

foreign import ccall unsafe "primal_atomic.c primal_sync_xor_fetch"
  syncXorFetchInt64AddrIO :: Addr# -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_xor_fetch"
  syncXorFetchInt64ArrayIO :: MutableByteArray# s -> Int# -> Int64 -> IO Int64
foreign import ccall unsafe "primal_atomic.c primal_sync_xor_fetch"
  syncXorFetchWord64AddrIO :: Addr# -> Int# -> Word64 -> IO Word64
foreign import ccall unsafe "primal_atomic.c primal_sync_xor_fetch"
  syncXorFetchWord64ArrayIO :: MutableByteArray# s -> Int# -> Word64 -> IO Word64

#endif
