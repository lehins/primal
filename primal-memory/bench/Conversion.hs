{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Criterion.Main
import qualified Data.Primitive.ByteArray as BA
import Data.Semigroup
import qualified Foreign.ForeignPtr as GHC
import Foreign.Storable
import Primal.Eval
import Primal.Foreign
import Primal.Memory.Bytes
import Primal.Memory.ForeignPtr
import Primal.Memory.Ptr


main :: IO ()
main = do
  let n = 1000000 :: Count a
      n64 = n :: Count Word64
      xs = [1 .. unCount n]
  mb1 <- allocAlignedPinnedMBytes n64
  b1 <- freezeMBytes mb1
  mb2 <- allocAlignedPinnedMBytes n64
  mb3 <- allocAlignedPinnedMBytes n64
  let MForeignPtr fp = toMForeignPtrMBytes mb3
  mba <- BA.newAlignedPinnedByteArray (unCountBytes (n :: Count Word64)) 8
  ba <- BA.unsafeFreezeByteArray mba
  -- Ensure that arrays are equal by filling them with zeros
  bEq1 <- freezeMBytes =<< allocZeroAlignedPinnedMBytes n64
  bEq2 <- freezeMBytes =<< allocZeroAlignedPinnedMBytes n64
  mbaEq1 <- BA.newAlignedPinnedByteArray (unCountBytes (n :: Count Word64)) 8
  mbaEq2 <- BA.newAlignedPinnedByteArray (unCountBytes (n :: Count Word64)) 8
  BA.setByteArray mbaEq1 0 (unCount n64) (0 :: Word64)
  BA.setByteArray mbaEq2 0 (unCount n64) (0 :: Word64)
  baEq1 <- BA.unsafeFreezeByteArray mbaEq1
  baEq2 <- BA.unsafeFreezeByteArray mbaEq2
  defaultMain
    [ bgroup
        "eq"
        [ bench "Bytes" $ whnf (bEq1 ==) bEq2
        , bench "ByteArray" $ whnf (baEq1 ==) baEq2
        ]
    , bgroup
        "with"
        [ bench "direct" $ nfIO (bytesAction n64 mb1)
        , bench "withPtrMBytes (INLINE)" $ nfIO (ptrAction_inline n64 mb3)
        , bench "withPtrMBytes (withNoHaltPtrMBytes)" $ nfIO (ptrAction n64 mb2)
        , bench "withPtrMBytes (NOINLINE)" $ nfIO (ptrAction_noinline n64 mb1)
        , bench "withForeignPtr (INLINE)" $ nfIO (foreignPtrAction n64 fp)
        , bench "withForeignPtr (Storable)" $ nfIO (foreignPtrStorable n64 fp)
        ]
    , bgroup
        "list"
        [ bgroup
            "mappend"
            [ bench "Bytes" $ whnf (mappend bEq1) bEq2
            , bench "ByteArray" $ whnf (mappend baEq1) baEq2
            ]
        , bgroup
            "mconcat"
            [ bench "Bytes" $ whnf mconcat [bEq1, bEq2, bEq1]
            , bench "ByteArray" $ whnf mconcat [baEq1, baEq2, baEq1]
            ]
        , env (pure (5 :: Int)) $ \sLen ->
            bgroup
              "stimes"
              [ bench "Bytes" $ whnf (stimes sLen) bEq1
              , bench "ByteArray" $ whnf (stimes sLen) baEq1
              ]
        , bgroup
            "toList"
            [ bench "Bytes" $ nf toList b1
            , bench "ByteArray" $ nf toList ba
            ]
        , bgroup
            "fromList"
            [ bench "Bytes" $ whnf (fromListBytes :: [Int] -> Bytes 'Inc) xs
            , bench "ByteArray" $ whnf BA.byteArrayFromList xs
            ]
        , bgroup
            "fromListN"
            [ bench "Bytes" $ whnf (fromListZeroBytesN_ n :: [Int] -> Bytes 'Inc) xs
            , bench "ByteArray" $ whnf (BA.byteArrayFromListN (unCount n)) xs
            ]
        ]
    ]


withPtrMBytes_noinline :: MBytes 'Pin s -> (Ptr a -> IO b) -> IO b
withPtrMBytes_noinline mb f = do
  res <- f $ toPtrMBytes mb
  res <$ touch mb
{-# NOINLINE withPtrMBytes_noinline #-}

ptrAction :: forall a . (Num a, Unbox a) => Count a -> MBytes 'Pin RealWorld -> IO ()
ptrAction (Count n) mb = go 0
  where
    go i
      | i < n = do
        withNoHaltPtrMBytes mb $ \ptr -> (writeOffPtr ptr (Off i) (123 :: a) :: IO ())
        go (i + 1)
      | otherwise = pure ()

ptrAction_inline :: forall a . (Num a, Unbox a) => Count a -> MBytes 'Pin RealWorld -> IO ()
ptrAction_inline (Count n) mb = go 0
  where
    go i
      | i < n = do
        withPtrMBytes mb $ \ptr -> writeOffPtr ptr (Off i) (123 :: a)
        go (i + 1)
      | otherwise = pure ()

ptrAction_noinline :: forall a . (Num a, Unbox a) => Count a -> MBytes 'Pin RealWorld -> IO ()
ptrAction_noinline (Count n) mb = go 0
  where
    go i
      | i < n = do
        withPtrMBytes_noinline mb $ \ptr -> writeOffPtr ptr (Off i) (123 :: a)
        go (i + 1)
      | otherwise = pure ()

bytesAction :: forall a . (Num a, Unbox a) => Count a -> MBytes 'Pin RealWorld -> IO ()
bytesAction (Count n) mb = go 0
  where
    go i
      | i < n = do
        writeOffMBytes mb (Off i) (123 :: a)
        go (i + 1)
      | otherwise = pure ()

foreignPtrAction :: forall a . (Num a, Unbox a) => Count a -> ForeignPtr a -> IO ()
foreignPtrAction (Count n) fp = go 0
  where
    go i
      | i < n = do
        withForeignPtr fp $ \ptr -> writeOffPtr ptr (Off i) (123 :: a)
        go (i + 1)
      | otherwise = pure ()


foreignPtrStorable :: forall a . (Num a, Storable a) => Count a -> ForeignPtr a -> IO ()
foreignPtrStorable (Count n) fp = go 0
  where
    go i
      | i < n = do
        GHC.withForeignPtr fp $ \ptr -> pokeElemOff ptr i (123 :: a)
        go (i + 1)
      | otherwise = pure ()
