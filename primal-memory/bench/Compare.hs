{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Criterion.Main
import Primal.Memory
import Primal.Memory.Addr
import Primal.Memory.Bytes
import Primal.Memory.ByteString
import Primal.Memory.PArray
import qualified Data.Primitive.ByteArray as BA
import Data.Primitive.PrimArray

main :: IO ()
main = do
  let n = 1000000 :: Count a
      n64 = n :: Count Word64
  -- Ensure that arrays are equal by filling them with zeros
  bEq1 <- freezeMBytes =<< allocZeroAlignedPinnedMBytes n64
  bEq2 <- freezeMBytes =<< allocZeroAlignedPinnedMBytes n64
  mbaEq1 <- BA.newAlignedPinnedByteArray (unCountBytes (n :: Count Word64)) 8
  mbaEq2 <- BA.newAlignedPinnedByteArray (unCountBytes (n :: Count Word64)) 8
  BA.setByteArray mbaEq1 0 (unCount n64) (0 :: Word64)
  BA.setByteArray mbaEq2 0 (unCount n64) (0 :: Word64)
  baEq1 <- BA.unsafeFreezeByteArray mbaEq1
  baEq2 <- BA.unsafeFreezeByteArray mbaEq2
  let paEq1 = PArray bEq1 :: PArray 'Pin Word64
      paEq2 = PArray bEq2 :: PArray 'Pin Word64
      bsEq1 = convertMem bEq1 :: ByteString
      bsEq2 = convertMem bEq2 :: ByteString
      addrEq1 = fromBytesAddr bEq1 :: Addr Word64
      addrEq2 = fromBytesAddr bEq2 :: Addr Word64
      primEq1 = PrimArray (toByteArray# bEq1) :: PrimArray Word64
      primEq2 = PrimArray (toByteArray# bEq2) :: PrimArray Word64
      pa8Eq1 = PArray bEq1 :: PArray 'Pin Word8
      pa8Eq2 = PArray bEq2 :: PArray 'Pin Word8
      addr8Eq1 = fromBytesAddr bEq1 :: Addr Word8
      addr8Eq2 = fromBytesAddr bEq2 :: Addr Word8
      prim8Eq1 = PrimArray (toByteArray# bEq1) :: PrimArray Word8
      prim8Eq2 = PrimArray (toByteArray# bEq2) :: PrimArray Word8

      addr16Eq1 = fromBytesAddr bEq1 :: Addr Word16
      addr16Eq2 = fromBytesAddr bEq2 :: Addr Word16
      pa16Eq1 = PArray bEq1 :: PArray 'Pin Word16
      pa16Eq2 = PArray bEq2 :: PArray 'Pin Word16
      prim16Eq1 = PrimArray (toByteArray# bEq1) :: PrimArray Word16
      prim16Eq2 = PrimArray (toByteArray# bEq2) :: PrimArray Word16
      addr32Eq1 = fromBytesAddr bEq1 :: Addr Word32
      addr32Eq2 = fromBytesAddr bEq2 :: Addr Word32
      pa32Eq1 = PArray bEq1 :: PArray 'Pin Word32
      pa32Eq2 = PArray bEq2 :: PArray 'Pin Word32
      prim32Eq1 = PrimArray (toByteArray# bEq1) :: PrimArray Word32
      prim32Eq2 = PrimArray (toByteArray# bEq2) :: PrimArray Word32
  defaultMain
    [ bgroup
        "eq"
        [ bgroup
            "Word8"
            [ bench "Bytes" $ whnf (bEq1 ==) bEq2
            , bench "ByteArray" $ whnf (baEq1 ==) baEq2
            , bench "ByteString" $ whnf (bsEq1 ==) bsEq2
            , bench "Addr" $ whnf (addr8Eq1 ==) addr8Eq2
            , bench "PArray" $ whnf (pa8Eq1 ==) pa8Eq2
            , bench "PrimArray" $ whnf (prim8Eq1 ==) prim8Eq2
            ]
        , bgroup
            "Word64"
            [ bench "Addr" $ whnf (addrEq1 ==) addrEq2
            , bench "PArray" $ whnf (paEq1 ==) paEq2
            , bench "PrimArray" $ whnf (primEq1 ==) primEq2
            ]
        ]
    , bgroup
        "compare"
        [ bgroup
            "Word8"
            [ bench "Bytes" $ whnf (compare bEq1) bEq2
            , bench "ByteArray" $ whnf (compare baEq1) baEq2
            , bench "ByteString" $ whnf (compare bsEq1) bsEq2
            , bench "Addr" $ whnf (compare addr8Eq1) addr8Eq2
            , bench "PArray" $ whnf (compare pa8Eq1) pa8Eq2
            , bench "PrimArray" $ whnf (compare prim8Eq1) prim8Eq2
            ]
        , bgroup
            "Word16"
            [ bench "Addr" $ whnf (compare addr16Eq1) addr16Eq2
            , bench "PArray" $ whnf (compare pa16Eq1) pa16Eq2
            , bench "PrimArray" $ whnf (compare prim16Eq1) prim16Eq2
            ]
        , bgroup
            "Word32"
            [ bench "Addr" $ whnf (compare addr32Eq1) addr32Eq2
            , bench "PArray" $ whnf (compare pa32Eq1) pa32Eq2
            , bench "PrimArray" $ whnf (compare prim32Eq1) prim32Eq2
            ]
        , bgroup
            "Word64"
            [ bench "Addr" $ whnf (compare addrEq1) addrEq2
            , bench "PArray" $ whnf (compare paEq1) paEq2
            , bench "PrimArray" $ whnf (compare primEq1) primEq2
            ]
        ]
    ]
