{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import GHC.Exts
import Data.Proxy
import Data.Typeable
import Criterion.Main
import Data.Prim.Bytes
import Data.Prim.Ptr
import Control.Monad.Prim
import qualified Data.Primitive.Types as BA
import qualified Data.Primitive.ByteArray as BA
import qualified Control.Monad.Primitive as BA
import Foreign.Storable as S

main :: IO ()
main = do
  let n = 1000000 :: Count a
      n64 = n :: Count Word64
      xs = [1 .. unCount n]
  mb1 <- allocAlignedMBytes n64
  mb2 <- allocAlignedMBytes n64
  mb3 <- allocAlignedMBytes n64
  mba <- BA.newAlignedPinnedByteArray (fromCount (n :: Count Word64)) 8
  ba <- BA.unsafeFreezeByteArray mba
  -- Ensure that arrays are equal
  mbEq1 <- callocAlignedMBytes n64
  mbEq2 <- callocAlignedMBytes n64
  mbaEq1 <- BA.newAlignedPinnedByteArray (fromCount (n :: Count Word64)) 8
  mbaEq2 <- BA.newAlignedPinnedByteArray (fromCount (n :: Count Word64)) 8
  BA.setByteArray mbaEq1 0 (unCount n64) (0 :: Word64)
  BA.setByteArray mbaEq2 0 (unCount n64) (0 :: Word64)
  baEq1 <- BA.unsafeFreezeByteArray mbaEq1
  baEq2 <- BA.unsafeFreezeByteArray mbaEq2
  defaultMain
    [ bgroup
        "ptr"
        [ env (freezeMBytes mb1) $ \b ->
            bench "(==) - isSameBytes" $ whnf (isSameBytes b) b
        , env (freezeMBytes mb1) $ \b ->
            bench "isSameBytes" $ whnf (isSameBytes b) (relaxPinned b)
        , env (freezeMBytes mb1) $ \b ->
            bench "isSamePinnedBytes" $ whnf (isSamePinnedBytes b) b
        , bench "(==) - sameByteArray (unexported)" $ whnf (ba ==) ba
        , bench "isSameMBytes" $ whnf (isSameMBytes mb1) mb1
        , bench "sameMutableByteArray" $ whnf (BA.sameMutableByteArray mba) mba
        ]
    , bgroup
        "eq"
        [ env ((,) <$> freezeMBytes mbEq1 <*> freezeMBytes mbEq2) $ \ ~(b1, b2) ->
            bench "Bytes" $ whnf (b1 ==) b2
        , bench "ByteArray" $ whnf (baEq1 ==) baEq2
        ]
    , bgroup
        "with"
        [ bench "withPtrMBytes (INLINE)" $ nfIO (ptrAction_inline n64 mb3)
        , bench "withPtrMBytes (withPtrNoHaltMBytes)" $ nfIO (ptrAction n64 mb2)
        , bench "withPtrMBytes (NOINLINE)" $ nfIO (ptrAction_noinline n64 mb1)
        ]
    , bgroup
        "list"
        [ bgroup
            "toList"
            [ env (freezeMBytes mb1) (bench "Bytes" . nf toList)
            , bench "ByteArray" $ nf toList ba
            ]
        , bgroup
            "fromList"
            [ env
                (pure xs)
                (bench "Bytes" . whnf (fromListBytes :: [Int] -> Bytes 'Inc))
            , bench "ByteArray" $ whnf BA.byteArrayFromList xs
            ]
        , bgroup
            "fromListN"
            [ env
                (pure xs)
                (bench "Bytes" . whnf (fromListBytesN_ n :: [Int] -> Bytes 'Inc))
            , bench "ByteArray" $ whnf (BA.byteArrayFromListN (unCount n)) xs
            ]
        ]
    , bgroup
        "set"
        [ bgroup
            "0"
            [ setBytesBench mb1 mb2 mba 0 (n * 8 :: Count Word8)
            , setBytesBench mb1 mb2 mba 0 (n * 4 :: Count Word16)
            , setBytesBench mb1 mb2 mba 0 (n * 2 :: Count Word32)
            , setBytesBench mb1 mb2 mba 0 (n :: Count Word64)
            , setBytesBench mb1 mb2 mba 0 (n * 2 :: Count Float)
            , setBytesBench mb1 mb2 mba 0 (n :: Count Double)
            , bench "setMBytes/Bool" $ nfIO (setMBytes mb1 0 (n * 8 :: Count Bool) False)
            ]
        , bgroup
            "regular"
            [ setBytesBench mb1 mb2 mba 123 (n * 8 :: Count Word8)
            , setBytesBench mb1 mb2 mba 123 (n * 4 :: Count Word16)
            , setBytesBench mb1 mb2 mba 123 (n * 2 :: Count Word32)
            , setBytesBench mb1 mb2 mba 123 (n :: Count Word64)
            , setBytesBench mb1 mb2 mba 123 (n * 2 :: Count Float)
            , setBytesBench mb1 mb2 mba 123 (n :: Count Double)
            , bench "setMBytes/Bool" $ nfIO (setMBytes mb1 0 (n * 8 :: Count Bool) True)
            ]
        , bgroup
            "symmetric"
            [ setBytesBench mb1 mb2 mba maxBound (n * 8 :: Count Word8)
            , setBytesBench mb1 mb2 mba maxBound (n * 4 :: Count Word16)
            , setBytesBench mb1 mb2 mba maxBound (n * 2 :: Count Word32)
            , setBytesBench mb1 mb2 mba maxBound (n :: Count Word64)
            ]
        ]
    , bgroup
        "access"
        [ env (freezeMBytes mbEq1) $ \b ->
            bgroup
              "index"
              [ benchIndex (Proxy :: Proxy Word8) b ba
              , benchIndex (Proxy :: Proxy Word16) b ba
              , benchIndex (Proxy :: Proxy Word32) b ba
              , benchIndex (Proxy :: Proxy Word64) b ba
              , benchIndex (Proxy :: Proxy Char) b ba
              , bgroup
                  "Bool"
                  [bench "Bytes" $ whnf (indexBytes b) (Off 125 :: Off Bool)]
              ]
        , bgroup
            "read"
            [ benchRead (Proxy :: Proxy Word8) mb1 mba
            , benchRead (Proxy :: Proxy Word16) mb1 mba
            , benchRead (Proxy :: Proxy Word32) mb1 mba
            , benchRead (Proxy :: Proxy Word64) mb1 mba
            , benchRead (Proxy :: Proxy Char) mb1 mba
            , bgroup
                "Bool" -- TODO: try out FFI
                [bench "Bytes" $ whnfIO (readMBytes mb1 (Off 125 :: Off Bool))]
            ]
        , bgroup
            "peek"
            [ benchPeek (Proxy :: Proxy Word8) mb1 mba
            , benchPeek (Proxy :: Proxy Word16) mb1 mba
            , benchPeek (Proxy :: Proxy Word32) mb1 mba
            , benchPeek (Proxy :: Proxy Word64) mb1 mba
            , benchPeek (Proxy :: Proxy Char) mb1 mba
            , bgroup
                "Bool"
                [ bench "Bytes" $
                  whnfIO (withPtrMBytes mb1 (readPtr :: Ptr Bool -> IO Bool))
                ]
            ]
        ]
    ]

benchIndex ::
     forall a p. (Typeable a, Prim a, BA.Prim a)
  => Proxy a
  -> Bytes p
  -> BA.ByteArray
  -> Benchmark
benchIndex px b ba =
  bgroup
    (showsType px "")
    [ bench "Bytes" $ whnf (indexBytes b) (Off i :: Off a)
    , bench "ByteArray" $ whnf (BA.indexByteArray ba :: Int -> a) i
    ]
  where i = 100

benchRead ::
     forall a p. (Typeable a, Prim a, BA.Prim a)
  => Proxy a
  -> MBytes p RealWorld
  -> BA.MutableByteArray RealWorld
  -> Benchmark
benchRead px mb mba =
  bgroup
    (showsType px "")
    [ bench "Bytes" $ whnfIO (readMBytes mb (Off i :: Off a))
    , bench "ByteArray" $ whnfIO (BA.readByteArray mba i :: IO a)
    ]
  where i = 100

benchPeek ::
     forall a. (Typeable a, Prim a, BA.Prim a)
  => Proxy a
  -> MBytes 'Pin RealWorld
  -> BA.MutableByteArray RealWorld
  -> Benchmark
benchPeek px mb mba =
  bgroup
    (showsType px "")
    [ bench "Bytes" $ whnfIO $ withPtrMBytes mb (readPtr :: Ptr a -> IO a)
    , bench "ByteArray" $
      whnfIO $ do
        let ptr = BA.mutableByteArrayContents mba
        res <- S.peek ptr
        res <$ BA.touch mba
    ]

setBytesBench ::
     forall a . (Typeable a, BA.Prim a, Prim a)
  => MBytes 'Pin RealWorld
  -> MBytes 'Pin RealWorld
  -> BA.MutableByteArray RealWorld
  -> a
  -> Count a
  -> Benchmark
setBytesBench mb1 mb2 mba a c@(Count n) =
  bgroup (showsType (Proxy :: Proxy a) "")
    [ bench "setMBytes" $ nfIO (setMBytes mb1 0 c a)
    , bench "setOffPtr" $ nfIO (withPtrMBytes mb2 $ \ ptr -> setOffPtr ptr 0 c a :: IO ())
    , bench "setByteArray" $ nfIO (BA.setByteArray mba 0 n a)
    ]


withPtrMBytes_noinline :: MBytes 'Pin s -> (Ptr a -> IO b) -> IO b
withPtrMBytes_noinline mb f = do
  res <- f (getPtrMBytes mb)
  res <$ touch mb
{-# NOINLINE withPtrMBytes_noinline #-}

ptrAction :: forall a . (Num a, Prim a) => Count a -> MBytes 'Pin RealWorld -> IO ()
ptrAction (Count n) mb = go 0
  where
    go i
      | i < n = do
        withPtrNoHaltMBytes mb $ \ptr -> (writeOffPtr ptr (Off i) (123 :: a) :: IO ())
        go (i + 1)
      | otherwise = pure ()

ptrAction_inline :: forall a . (Num a, Prim a) => Count a -> MBytes 'Pin RealWorld -> IO ()
ptrAction_inline (Count n) mb = go 0
  where
    go i
      | i < n = do
        withPtrMBytes mb $ \ptr -> writeOffPtr ptr (Off i) (123 :: a)
        go (i + 1)
      | otherwise = pure ()

ptrAction_noinline :: forall a . (Num a, Prim a) => Count a -> MBytes 'Pin RealWorld -> IO ()
ptrAction_noinline (Count n) mb = go 0
  where
    go i
      | i < n = do
        withPtrMBytes_noinline mb $ \ptr -> writeOffPtr ptr (Off i) (123 :: a)
        go (i + 1)
      | otherwise = pure ()
