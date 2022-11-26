{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- |
-- Module      : Primal.Memory.Encoding.Base16
-- Copyright   : (c) Alexey Kuleshevich 2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module Primal.Memory.Encoding.Base16
  ( -- * Encode
    encodeBase16
  , encodeBase16Mem
  , encodeBase16MutMem

    -- * Decode
  , DecodeError (..)
  , decodeBase16
  , decodeBase16Mem
  , decodeBase16MutMem
  ) where

import Control.Monad.Trans.Except
import Primal.Eval
import Primal.Exception
import Primal.Foreign
import Primal.Memory.Addr
import Primal.Memory.Internal
import Primal.Monad
import Primal.Monad.Unsafe
import Primal.Mutable.Freeze

-- | Error produced when decoding fails.
--
-- @since 1.0.0
data DecodeError
  = -- | Error produced when length of the input is unepxected
    DecodeInvalidLength
  | -- | Error produced when unepxected value is encountered. Contains an offset
    -- into source memory where that value is located.
    DecodeInvalidValue !(Off Word8)
  deriving (Show, Eq)

instance Exception DecodeError

instance NFData DecodeError where
  rnf = (`seq` ())

-- | Encode region of immutable memory as hexadecimal. Returned memory type is
-- the same as the source. See `encodeBase16Mem` if the output memory type
-- should be different.
--
-- ====__Example__
--
-- >>> import Primal.Memory
-- >>> import Primal.Memory.Encoding
-- >>> :set -XDataKinds
-- >>> let hex = encodeBase16 (fromByteListMem ([222,173,190,239] :: [Word8]) :: Bytes 'Inc)
-- >>> toStringMem hex
-- "deadbeef"
--
-- @since 1.0.0
encodeBase16 :: forall a ma. (a ~ Frozen ma, MemFreeze ma) => a -> a
encodeBase16 = encodeBase16Mem
{-# INLINE encodeBase16 #-}

-- | Encode region of immutable memory as hexadecimal.
--
-- @since 1.0.0
encodeBase16Mem :: forall mr ma. (MemRead mr, MemFreeze ma) => mr -> Frozen ma
encodeBase16Mem mr = runST $ do
  let bc = byteCountMem mr
  m <- allocMutMemST (bc * 2)
  encodeBase16MutMem mr m
  freezeMutST m
{-# INLINE encodeBase16Mem #-}

-- | Encode region of immutable memory as hexadecimal and write the output into
-- the supplied mutable region.
--
-- [Unsafe] Size of mutable memory should be at least double the size of the
-- immutable reagion, otherwise segfault is to be expected.
--
-- @since 1.0.0
encodeBase16MutMem :: forall mr ma m s. (MemRead mr, MemAlloc ma, Primal s m) => mr -> ma s -> m ()
encodeBase16MutMem mr m = do
  let bc = byteCountMem mr
      f srcBA# o =
        accessMutMemST
          m
          (\dstMBA# mo -> unsafeIOToST $ encodeBase16_BA_MBA# srcBA# o dstMBA# mo bc)
          (\dstAddr# mo -> unsafeIOToST $ encodeBase16_BA_Addr# srcBA# o dstAddr# mo bc)
          0
      g srcAddr# o =
        accessMutMemST
          m
          (\dstMBA# mo -> unsafeIOToST $ encodeBase16_Addr_MBA# srcAddr# o dstMBA# mo bc)
          (\dstAddr# mo -> unsafeIOToST $ encodeBase16_Addr_Addr# srcAddr# o dstAddr# mo bc)
          0
  liftST $ accessMem mr f g 0
{-# INLINE encodeBase16MutMem #-}

-- | Decode a hexidecimal encoded region of memory. Returned memory type is
-- the same as the source. See `decodeBase16Mem` if the output memory type
-- should be different.
--
-- ====__Example__
--
-- >>> import Primal.Memory
-- >>> import Primal.Memory.Encoding
-- >>> :set -XDataKinds
-- >>> decodeBase16 (fromStringMem "deadbeef" :: Bytes 'Inc)
-- Right [0xde,0xad,0xbe,0xef]
-- >>> decodeBase16 (fromStringMem "0123T456" :: Bytes 'Inc)
-- Left (DecodeInvalidValue (Off {unOff = 4}))
--
-- @since 1.0.0
decodeBase16 :: forall a ma. (a ~ Frozen ma, MemFreeze ma) => a -> Either DecodeError a
decodeBase16 = decodeBase16Mem
{-# INLINE decodeBase16 #-}

-- | Decode a hexidecimal encoded region of memory. Same as `decodeBase16`,
-- except returned memory type can be any other `MemAlloc`.
--
-- @since 1.0.0
decodeBase16Mem
  :: forall mr ma
   . (MemRead mr, MemFreeze ma)
  => mr
  -> Either DecodeError (Frozen ma)
decodeBase16Mem mr = runST $ do
  let Count c = byteCountMem mr
      q = Count (c `quot` 2) :: Count Word8
  m <- allocMutMemST q
  runExceptT $ handleExceptT $ do
    decodeBase16MutMem mr m
    freezeMutMem m
{-# INLINE decodeBase16Mem #-}

-- | Decode a hexidecimal encoded region of immutable memory and write the
-- output into the supplied mutable region.
--
-- [Unsafe] Size of mutable memory should be at least double the size of the
-- immutable region, otherwise segfault or heap corruption is to be expected.
--
-- @since 1.0.0
decodeBase16MutMem :: forall mr ma m s. (MemRead mr, MemFreeze ma, Primal s m) => mr -> ma s -> m ()
decodeBase16MutMem mr m = do
  let Count c = byteCountMem mr
      r = c `rem` 2
  q <- getByteCountMutMem m
  when (r /= 0) $ raiseM DecodeInvalidLength
  let f srcBA# o =
        accessMutMemST
          m
          (\dstMBA# mo -> unsafeIOToST $ decodeBase16_BA_MBA# srcBA# o dstMBA# mo q)
          (\dstAddr# mo -> unsafeIOToST $ decodeBase16_BA_Addr# srcBA# o dstAddr# mo q)
          0
  let g srcAddr# o =
        accessMutMemST
          m
          (\dstMBA# mo -> unsafeIOToST $ decodeBase16_Addr_MBA# srcAddr# o dstMBA# mo q)
          (\dstAddr# mo -> unsafeIOToST $ decodeBase16_Addr_Addr# srcAddr# o dstAddr# mo q)
          0
  res <- liftST $ accessMem mr f g 0
  unless (res == -1) $ raiseM $ DecodeInvalidValue res
{-# INLINE decodeBase16MutMem #-}

foreign import ccall unsafe "primal_memory.c primal_encode_base16"
  encodeBase16_BA_MBA# :: ByteArray# -> Off Word8 -> MutableByteArray# s -> Off Word8 -> Count Word8 -> IO ()
foreign import ccall unsafe "primal_memory.c primal_encode_base16"
  encodeBase16_BA_Addr# :: ByteArray# -> Off Word8 -> Addr# -> Off Word8 -> Count Word8 -> IO ()
foreign import ccall unsafe "primal_memory.c primal_encode_base16"
  encodeBase16_Addr_MBA# :: Addr# -> Off Word8 -> MutableByteArray# s -> Off Word8 -> Count Word8 -> IO ()
foreign import ccall unsafe "primal_memory.c primal_encode_base16"
  encodeBase16_Addr_Addr# :: Addr# -> Off Word8 -> Addr# -> Off Word8 -> Count Word8 -> IO ()

foreign import ccall unsafe "primal_memory.c primal_decode_base16"
  decodeBase16_BA_MBA# :: ByteArray# -> Off Word8 -> MutableByteArray# s -> Off Word8 -> Count Word8 -> IO (Off Word8)
foreign import ccall unsafe "primal_memory.c primal_decode_base16"
  decodeBase16_BA_Addr# :: ByteArray# -> Off Word8 -> Addr# -> Off Word8 -> Count Word8 -> IO (Off Word8)
foreign import ccall unsafe "primal_memory.c primal_decode_base16"
  decodeBase16_Addr_MBA# :: Addr# -> Off Word8 -> MutableByteArray# s -> Off Word8 -> Count Word8 -> IO (Off Word8)
foreign import ccall unsafe "primal_memory.c primal_decode_base16"
  decodeBase16_Addr_Addr# :: Addr# -> Off Word8 -> Addr# -> Off Word8 -> Count Word8 -> IO (Off Word8)
