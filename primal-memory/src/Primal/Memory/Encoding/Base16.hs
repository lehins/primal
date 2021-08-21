{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Primal.Memory.Encoding.Base16
-- Copyright   : (c) Alexey Kuleshevich 2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Memory.Encoding.Base16 where

import Data.Bits
import Primal.Unbox
import Primal.Eval
import Primal.Exception
import Primal.Memory.Internal
import Primal.Monad
import Primal.Mutable.Freeze
import Primal.Memory.Ptr
import Primal.Memory.Addr
import GHC.Exts
import GHC.Word


encodeBase16MutMemST :: forall mr ma s. (MemRead mr, MemAlloc ma) => mr -> ST s (ma s)
encodeBase16MutMemST mr = do
  let bc@(Count c) = byteCountMem mr
      c2 = c - (c `mod` 2)
  m <- allocMutMemST (bc * 2)
  let !(Ptr base16#) = lookupTableBase16
      go !i !o
        | i < c2 = do
          writeByteOffMutMemST m o $
            lookupAscii16 base16# (indexByteOffMem mr (Off i))
          writeByteOffMutMemST m (o + 2) $
            lookupAscii16 base16# (indexByteOffMem mr (Off i + 1))
          go (i + 2) (o + 4)
        | otherwise = pure o
  o <- go 0 0
  when (c /= c2) $
    writeByteOffMutMemST m o $
    lookupAscii16 base16# $ indexByteOffMem mr (Off c2)
  pure m
{-# INLINE [1] encodeBase16MutMemST #-}

{-# RULES
"encodeBase16MAddr/encodeBase16MutMemST" encodeBase16MutMemST = encodeBase16MAddr
#-}


encodeBase16Mem :: forall mr ma. (MemRead mr, MemAlloc ma) => mr -> Frozen ma
encodeBase16Mem mr = runST $ encodeBase16MutMemST mr >>= freezeMutST
{-# INLINE encodeBase16Mem #-}

encodeBase16 :: forall a ma. (a ~ Frozen ma, MemAlloc ma) => a -> a
encodeBase16 = encodeBase16Mem
{-# INLINE encodeBase16 #-}

encodeBase16MAddr :: Addr e -> ST s (MAddr Word8 s)
encodeBase16MAddr mr' = do
  let !mr = castAddr mr'
      !bc@(Count c) = byteCountAddr mr
      !e = c `rem` 2
      !end = mr `plusByteOffAddr` Off (c - e)
  m <- allocMAddr (bc * 2)
  let !(Ptr base16#) = lookupTableBase16
      go !i !o
        | isSameAddr i end = pure o
        | otherwise = do
          writeMAddr o (lookupAscii16 base16# (indexAddr i))
          writeByteOffMAddr o 2 (lookupAscii16 base16# (indexByteOffAddr i 1))
          go (i `plusByteOffAddr` 2) (o `plusByteOffMAddr` 4)
  o <- go mr (castMAddr m)
  when (e /= 0) $ writeMAddr o (lookupAscii16 base16# (indexAddr end))
  pure (castMAddr m)
{-# INLINE encodeBase16MAddr #-}


encodeBase16Addr :: Addr e -> Addr Word8
encodeBase16Addr mr = runST $ encodeBase16MAddr mr >>= freezeMAddr
{-# INLINE encodeBase16Addr #-}


lookupAscii16 :: Addr# -> Word8 -> Word16
lookupAscii16 base16# (W8# w#) = W16# (indexWord16OffAddr# base16# (word2Int# w#))
{-# INLINE lookupAscii16 #-}

lookupAscii8 :: Addr# -> Word8 -> Word8
lookupAscii8 base16# (W8# w#) = W8# (indexWord8OffAddr# base16# (word2Int# w#))
{-# INLINE lookupAscii8 #-}


lookupTableBase16 :: Ptr Word16
lookupTableBase16 = Ptr "000102030405060708090a0b0c0d0e0f\
                        \101112131415161718191a1b1c1d1e1f\
                        \202122232425262728292a2b2c2d2e2f\
                        \303132333435363738393a3b3c3d3e3f\
                        \404142434445464748494a4b4c4d4e4f\
                        \505152535455565758595a5b5c5d5e5f\
                        \606162636465666768696a6b6c6d6e6f\
                        \707172737475767778797a7b7c7d7e7f\
                        \808182838485868788898a8b8c8d8e8f\
                        \909192939495969798999a9b9c9d9e9f\
                        \a0a1a2a3a4a5a6a7a8a9aaabacadaeaf\
                        \b0b1b2b3b4b5b6b7b8b9babbbcbdbebf\
                        \c0c1c2c3c4c5c6c7c8c9cacbcccdcecf\
                        \d0d1d2d3d4d5d6d7d8d9dadbdcdddedf\
                        \e0e1e2e3e4e5e6e7e8e9eaebecedeeef\
                        \f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"#
{-# NOINLINE lookupTableBase16 #-}

data DecodeError = DecodeInvalidLength | DecodeInvalidValue !(Off Word8)
  deriving (Show, Eq)
instance Exception DecodeError

instance NFData DecodeError where
  rnf = (`seq` ())


decodeBase16MutMemST :: forall mr ma s. (MemRead mr, MemAlloc ma) => mr -> ST s (ma s)
decodeBase16MutMemST mr = do
  let Count c = byteCountMem mr
      q = c `quot` 2
      r = c `rem` 2
  when (r /= 0) $ raise DecodeInvalidLength
  m <- allocMutMemST (Count q :: Count Word8)
  let !(Ptr hi#) = hiBase16Table
      !(Ptr lo#) = loBase16Table
      go !i !o =
        when (i < c) $ do
          let !w4hi = lookupAscii8 hi# (indexByteOffMem mr (Off i))
          let !w4lo = lookupAscii8 lo# (indexByteOffMem mr (Off (i + 1)))
              !w = w4hi .|. w4lo
              raiseInvalid off = raise $ DecodeInvalidValue (Off i + off)
              {-# NOINLINE raiseInvalid #-}
          -- when (w == 0xff) $ do
          when (w4hi == 0xff) $ raiseInvalid 0
          when (w4lo == 0xff) $ raiseInvalid 1
          writeByteOffMutMemST m o w
          go (i + 2) (o + 1)
  go 0 0
  pure m
{-# INLINE decodeBase16MutMemST #-}

decodeBase16Mem ::
     forall mr ma. (MemRead mr, MemAlloc ma)
  => mr
  -> Either DecodeError (Frozen ma)
decodeBase16Mem mr = tryST $ decodeBase16MutMemST mr >>= freezeMutST
{-# INLINE decodeBase16Mem #-}

decodeBase16 :: forall a ma. (a ~ Frozen ma, MemAlloc ma) => a -> Either DecodeError a
decodeBase16 = decodeBase16Mem
{-# INLINE decodeBase16 #-}



decodeBase16Bytes :: Bytes a -> Bytes 'Pin
decodeBase16Bytes mr = runST $ do
  let Count c = byteCountMem mr
      q = c `quot` 2
      r = c `rem` 2
  when (r /= 0) $ raise DecodeInvalidLength
  m <- allocMutMemST (Count q :: Count Word8)
  let fromAscii !w
        | 0x30 <= w && w <= 0x39 = pure $ w - 48
        | 0x61 <= w && w <= 0x66 = pure $ w - 87
        | otherwise = raise DecodeInvalidLength
      {-# INLINE fromAscii #-}
      go !i =
        when (i < c) $ do
          let !w8a = indexByteOffMem mr (Off i) :: Word8
              !w8b = indexByteOffMem mr (Off (i + 1)) :: Word8
          w4a <- fromAscii w8a
          w4b <- fromAscii w8b
          writeByteOffMutMemST m (Off (i `unsafeShiftR` 1)) ((w4a `shiftL` 4) .|. w4b)
          go (i + 2)
  go 0
  freezeMutST m
{-# INLINE decodeBase16Bytes #-}

decodeBase16Addr :: Addr Word16 -> Addr Word8
decodeBase16Addr mr' = runST $ do
  let mr = castAddr mr'
      Count c = byteCountAddr mr
      q = c `quot` 2
      r = c `rem` 2
      end = mr `plusByteOffAddr` Off c
  when (r /= 0) $ raise DecodeInvalidLength
  m <- allocMAddr (Count q :: Count Word8)
  let fromAscii !w
        | 0x30 <= w && w <= 0x39 = pure $ w - 48
        | 0x61 <= w && w <= 0x66 = pure $ w - 87
        | otherwise = raise DecodeInvalidLength
      {-# INLINE fromAscii #-}
      go !i !o =
        unless (isSameAddr i end) $ do
          w4a <- fromAscii (indexAddr i :: Word8)
          w4b <- fromAscii (indexByteOffAddr i 1 :: Word8)
          writeMAddr o ((w4a `shiftL` 4) .|. w4b)
          go (i `plusByteOffAddr` 2) (o `plusByteOffMAddr` 1)
  go mr m
  freezeMAddr m
{-# INLINE decodeBase16Addr #-}

decodeBase16Addr' :: Addr Word16 -> Either DecodeError (Addr Word8)
decodeBase16Addr' mr' = tryST $ do
  let mr = castAddr mr'
      Count c = byteCountAddr mr
      q = c `quot` 2
      r = c `rem` 2
      end = mr `plusByteOffAddr` Off c
  when (r /= 0) $ raise DecodeInvalidLength
  m <- allocMAddr (Count q :: Count Word8)
  let !(Ptr hi#) = hiBase16Table
      !(Ptr lo#) = loBase16Table
      go !i !o =
        unless (isSameAddr i end) $ do
          let !w4hi = lookupAscii8 hi# (indexAddr i)
          let !w4lo = lookupAscii8 lo# (indexByteOffAddr i 1)
              !w = w4hi .|. w4lo
              raiseInvalid off =
                raise $ DecodeInvalidValue (countToOff (minusByteCountAddr i mr) + off)
              {-# NOINLINE raiseInvalid #-}
          -- when (w == 0xff) $ do
          when (w4hi == 0xff) $ raiseInvalid 0
          when (w4lo == 0xff) $ raiseInvalid 1
          writeMAddr o w
          go (i `plusByteOffAddr` 2) (o `plusByteOffMAddr` 1)
  go mr m
  freezeMAddr m
{-# INLINE decodeBase16Addr' #-}

loBase16Table :: Ptr Word8
loBase16Table =
  Ptr "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
      \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
      \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
      \\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\xff\xff\xff\xff\xff\xff\
      \\xff\x0a\x0b\x0c\x0d\x0e\x0f\xff\xff\xff\xff\xff\xff\xff\xff\xff\
      \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
      \\xff\x0a\x0b\x0c\x0d\x0e\x0f\xff\xff\xff\xff\xff\xff\xff\xff\xff\
      \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
      \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
      \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
      \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
      \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
      \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
      \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
      \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
      \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#
{-# NOINLINE loBase16Table #-}

hiBase16Table :: Ptr Word8
hiBase16Table =
  Ptr "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
      \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
      \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
      \\x00\x10\x20\x30\x40\x50\x60\x70\x80\x90\xff\xff\xff\xff\xff\xff\
      \\xff\xa0\xb0\xc0\xd0\xe0\xf0\xff\xff\xff\xff\xff\xff\xff\xff\xff\
      \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
      \\xff\xa0\xb0\xc0\xd0\xe0\xf0\xff\xff\xff\xff\xff\xff\xff\xff\xff\
      \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
      \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
      \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
      \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
      \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
      \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
      \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
      \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
      \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#
{-# NOINLINE hiBase16Table #-}



-- Correct pointer arithmetics and coversion, not fast though:
-- decodeBase16Addr :: Addr Word16 -> Addr Word8
-- decodeBase16Addr mr' = runST $ do
--   let mr = castAddr mr'
--       Count c = byteCountAddr mr
--       q = c `quot` 2
--       r = c `rem` 2
--       end = mr `plusByteOffAddr` Off c
--   when (r /= 0) $ raise DecodeInvalidLength
--   m <- allocMAddr (Count q :: Count Word8)
--   let fromAscii !w
--         | 0x30 <= w && w <= 0x39 = pure $ w - 48
--         | 0x61 <= w && w <= 0x66 = pure $ w - 87
--         | otherwise = raise DecodeInvalidLength
--       {-# INLINE fromAscii #-}
--       go !i !o =
--         unless (isSameAddr i end) $ do
--           w4a <- fromAscii (indexAddr i :: Word8)
--           w4b <- fromAscii (indexByteOffAddr i 1 :: Word8)
--           writeMAddr o ((w4a `shiftL` 4) .|. w4b)
--           go (i `plusByteOffAddr` 2) (o `plusByteOffMAddr` 1)
--   go mr m
--   freezeMAddr m
-- {-# INLINE decodeBase16Addr #-}


-- lookupAscii :: Addr# -> Word8 -> Word8
-- lookupAscii base16# (W8# w#) = W8# (indexWord8OffAddr# base16# (word2Int# w#))
-- {-# INLINE lookupAscii #-}

-- base16 :: Ptr Word8
-- base16 = Ptr "0123456789abcdef"#
-- encodeBase16MutMemST :: forall mr ma s. (MemRead mr, MemAlloc ma) => mr -> ST s (ma s)
-- encodeBase16MutMemST mr = do
--   let bc@(Count c) = byteCountMem mr
--   m <- allocMutMemST (bc * 2)
--   let !(Ptr base16#) = lookupTableBase16
--       go !i !o =
--         when (i < c) $ do
--           let !b = indexByteOffMem mr (Off i) :: Word8
--           writeByteOffMutMemST m o $ lookupAscii16 base16# b
--           go (i + 1) (o + 2)
--   m <$ go 0 0
-- {-# INLINE encodeBase16MutMemST #-}

-- encodeBase16Bytes :: Bytes a -> Bytes 'Inc
-- encodeBase16Bytes mr =
--   unsafeDupablePerformIO $ do
--     let bc@(Count c) = byteCountBytes mr
--     m <- allocMBytes (bc * 2)
--     let !base16# = "0123456789abcdef"#
--         go !i !o
--           | i == c = pure ()
--           | otherwise = do
--             let !b = indexByteOffBytes mr (Off i) :: Word8
--             writeByteOffMBytes m o (lookupAscii base16# (b `unsafeShiftR` 4))
--             writeByteOffMBytes m (o + 1) (lookupAscii base16# (b .&. 0xf))
--             go (i + 1) (o + 2)
--     go 0 0
--     freezeMBytes m
-- {-# INLINE encodeBase16Bytes #-}
