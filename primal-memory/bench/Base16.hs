{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Criterion.Main
import Data.ByteString.Base16 as Base16 (encode)
import Base16Copy as Base16
import Primal.Eval
import Primal.Memory
import Primal.Memory.Addr
import Primal.Memory.ByteString
import Primal.Memory.Encoding.Base16
import System.Random.Stateful
import Data.ByteArray.Encoding

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
        [ bench "bytestring-base16" $ whnf Base16.encode bs
        , bench "memory" $ whnf (convertToBase Base16 :: ByteString -> ByteString) bs
        , bench "encodeBase16Addr" $ whnf encodeBase16Addr a
        , bench "encodeBase16 (Addr)" $ whnf encodeBase16 a
        , bench "encodeBase16 (Bytes)" $ whnf encodeBase16 b
        , bench "encodeBase16 (ByteString)" $ whnf encodeBase16 bs
        ]
    , bgroup
        "decode"
        [ env (pure $ Base16.encode bs) $ \bsHex ->
            bench "bytestring-base16" $ nf Base16.decode bsHex
        , env (pure $ Base16.encode bs) $ \bsHex ->
            bench "memory" $ nf (convertFromBase Base16 :: ByteString -> Either String ByteString) bsHex
        -- , env (pure $ encodeBase16Addr bs) $ \aHex ->
        --     bench "decodeBase16Addr" $ nf decodeBase16Addr aHex
        -- , env (pure $ castAddr $ encodeBase16 a) $ \aHex ->
        --     bench "decodeBase16Addr" $ nf decodeBase16Addr aHex
        -- , env (pure $ castAddr $ encodeBase16 a) $ \aHex ->
        --     bench "decodeBase16Addr'" $ nf decodeBase16Addr' aHex
        -- , env (pure $ encodeBase16 b) $ \bHex ->
        --     bench "decodeBase16Bytes" $ nf decodeBase16Bytes bHex
        , env (pure $ encodeBase16 a) $ \aHex ->
            bench "decodeBase16 (Addr)" $ nf decodeBase16 aHex
        -- , env (pure $ encodeBase16 b) $ \bHex ->
        --     bench "decodeBase16 (Bytes)" $ nf decodeBase16 bHex
        , env (pure $ encodeBase16 bs) $ \bsHex ->
            bench "decodeBase16 (ByteString)" $ nf decodeBase16_ByteString bsHex
        ]
    ]

-- decode_ByteString :: ByteString -> Either String ByteString
-- decode_ByteString = Base16.decode
-- {-# NOINLINE decode_ByteString #-}

decodeBase16_ByteString :: ByteString -> Either DecodeError ByteString
decodeBase16_ByteString = decodeBase16
{-# NOINLINE decodeBase16_ByteString #-}
