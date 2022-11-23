{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where


import Control.Monad.Trans.Except
import Criterion.Main
import Data.Bits
import Data.ByteArray.Encoding
import Data.ByteString.Base16 as Base16
import Primal.Eval
import Primal.Exception
import Primal.Foreign
import Primal.Memory
import Primal.Memory.Addr
import Primal.Memory.ByteString
import Primal.Memory.Encoding
import Primal.Memory.Internal
import System.Random.Stateful

main :: IO ()
main = do
  let n = 10000000 :: Count Word8
  stdGen <- getStdGen
  bs <- runStateGenT_ stdGen (uniformByteStringM (unCount n))
  b <- eval (fromByteStringBytes bs :: Bytes 'Pin)
  a <- eval (fromBytesAddr b :: Addr Word8)
  defaultMain
    [ bgroup
        "encode"
        [ bench "encodeBase16 (Addr)" $ whnf encodeBase16 a
        , bench "encodeBase16 (Bytes)" $ whnf encodeBase16 b
        , bench "encodeBase16 (ByteString)" $ whnf encodeBase16 bs
        , bench "encodeBase16Addr" $ whnf encodeBase16Addr a
        , bench "encodeBase16Native (Bytes)" $ whnf encodeBase16Native b
        , bench "bytestring-base16" $ whnf Base16.encode bs
        , bench "memory" $
          whnf (convertToBase Base16 :: ByteString -> ByteString) bs
        ]
    , bgroup
        "decode"
        [ env (pure $ encodeBase16 a) $ \aHex ->
            bench "decodeBase16 (Addr)" $ nf decodeBase16 aHex
        , env (pure $ encodeBase16 b) $ \bHex ->
            bench "decodeBase16 (Bytes)" $ nf decodeBase16 bHex
        , env (pure $ encodeBase16 bs) $ \bsHex ->
            bench "decodeBase16 (ByteString)" $ nf decodeBase16 bsHex
        , env (pure $ castAddr $ encodeBase16 a) $ \aHex ->
            bench "decodeBase16Addr" $ nf decodeBase16Addr aHex
        , env (pure $ encodeBase16 b) $ \bHex ->
            bench "decodeBase16Native (Bytes)" $ nf decodeBase16Native bHex
        , env (pure $ Base16.encode bs) $ \bsHex ->
            bench "bytestring-base16" $ nf Base16.decode bsHex
        , env (pure $ Base16.encode bs) $ \bsHex ->
            bench "memory" $
            nf
              (convertFromBase Base16 :: ByteString -> Either String ByteString)
              bsHex
        ]
    ]


encodeBase16Native :: forall a ma. (a ~ Frozen ma, MemFreeze ma) => a -> a
encodeBase16Native mr = runST $ do
  let bc@(Count c) = byteCountMem mr
      c2 = c - (c `mod` 2)
  m <- allocMutMemST (bc * 2)
  let !(Ptr base16#) = toBase16Table
      go !i !o
        | i < c2 = do
          writeByteOffMutMemST m o $
            lookupWord16 base16# (indexByteOffMem mr (Off i))
          writeByteOffMutMemST m (o + 2) $
            lookupWord16 base16# (indexByteOffMem mr (Off i + 1))
          go (i + 2) (o + 4)
        | otherwise = pure o
  o <- go 0 0
  when (c /= c2) $
    writeByteOffMutMemST m o $
      lookupWord16 base16# $ indexByteOffMem mr (Off c2)
  freezeMutMem m
{-# INLINE encodeBase16Native #-}



decodeBase16Native :: forall a ma. (a ~ Frozen ma, MemFreeze ma) => a -> Either DecodeError a
decodeBase16Native mr = runST $ runExceptT $ handleExceptT $ do
  let Count c = byteCountMem mr
      q = c `quot` 2
      r = c `rem` 2
  when (r /= 0) $ raiseM DecodeInvalidLength
  m <- allocMutMem (Count q :: Count Word8)
  let !(Ptr hi#) = hiFromBase16Table
      !(Ptr lo#) = loFromBase16Table
      go !i !o =
        when (i < c) $ do
          let !w4hi = lookupWord8 hi# (indexByteOffMem mr (Off i))
          let !w4lo = lookupWord8 lo# (indexByteOffMem mr (Off (i + 1)))
              !w = w4hi .|. w4lo
              raiseInvalid off = raiseM $ DecodeInvalidValue (Off i + off)
          when (w4hi == 0xff) $ raiseInvalid 0
          when (w4lo == 0xff) $ raiseInvalid 1
          writeByteOffMutMem m o w
          go (i + 2) (o + 1)
  go 0 0
  freezeMutMem m
{-# INLINE decodeBase16Native #-}



encodeBase16Addr :: Addr e -> Addr Word8
encodeBase16Addr mr' = runST $ do
  let !mr = castAddr mr'
      !bc@(Count c) = byteCountAddr mr
      !e = c `rem` 2
      !end = mr `plusByteOffAddr` Off (c - e)
  m <- allocMAddr (bc * 2)
  let !(Ptr base16#) = toBase16Table
      go !i !o
        | isSameAddr i end = pure o
        | otherwise = do
          writeMAddr o (lookupWord16 base16# (indexAddr i))
          writeByteOffMAddr o 2 (lookupWord16 base16# (indexByteOffAddr i 1))
          go (i `plusByteOffAddr` 2) (o `plusByteOffMAddr` 4)
  o <- go mr (castMAddr m)
  when (e /= 0) $ writeMAddr o (lookupWord16 base16# (indexAddr end))
  freezeMAddr (castMAddr m)
{-# INLINE encodeBase16Addr #-}


decodeBase16Addr :: Addr Word16 -> Either DecodeError (Addr Word8)
decodeBase16Addr mr' = runST $ runExceptT $ handleExceptT $ do
  let mr = castAddr mr'
      Count c = byteCountAddr mr
      q = c `quot` 2
      r = c `rem` 2
      end = mr `plusByteOffAddr` Off c
  when (r /= 0) $ raiseM DecodeInvalidLength
  m <- allocMAddr (Count q :: Count Word8)
  let !(Ptr hi#) = hiFromBase16Table
      !(Ptr lo#) = loFromBase16Table
      go !i !o =
        unless (isSameAddr i end) $ do
          let !w4hi = lookupWord8 hi# (indexAddr i)
          let !w4lo = lookupWord8 lo# (indexByteOffAddr i 1)
              !w = w4hi .|. w4lo
              raiseInvalid off =
                raiseM $ DecodeInvalidValue (countToOff (minusByteCountAddr i mr) + off)
          when (w4hi == 0xff) $ raiseInvalid 0
          when (w4lo == 0xff) $ raiseInvalid 1
          writeMAddr o w
          go (i `plusByteOffAddr` 2) (o `plusByteOffMAddr` 1)
  go mr m
  freezeMAddr m
{-# INLINE decodeBase16Addr #-}


lookupWord16 :: Addr# -> Word8 -> Word16
lookupWord16 base16# (W8# w#) = W16# (indexWord16OffAddr# base16# (word2Int# w#))
{-# INLINE lookupWord16 #-}

lookupWord8 :: Addr# -> Word8 -> Word8
lookupWord8 base16# (W8# w#) = W8# (indexWord8OffAddr# base16# (word2Int# w#))
{-# INLINE lookupWord8 #-}

toBase16Table :: Ptr Word16
toBase16Table =
  Ptr
    "000102030405060708090a0b0c0d0e0f\
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
{-# NOINLINE toBase16Table #-}


loFromBase16Table :: Ptr Word8
loFromBase16Table =
  Ptr
    "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
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
{-# NOINLINE loFromBase16Table #-}

hiFromBase16Table :: Ptr Word8
hiFromBase16Table =
  Ptr
    "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
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
{-# NOINLINE hiFromBase16Table #-}
