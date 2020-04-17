{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Proxy
import Data.Typeable
import Criterion.Main
import Data.Prim.Bytes
import Data.Prim.Ptr
import Control.Monad.Prim
import qualified Data.Primitive.Types as Primitive
import qualified Data.Primitive.ByteArray as BA

main :: IO ()
main = do
  let n = 1000000 :: Count a
      n64 = n :: Count Word64
  mb1 <- allocAlignedMBytes n64
  mb2 <- allocAlignedMBytes n64
  mb3 <- allocAlignedMBytes n64
  mba <- BA.newAlignedPinnedByteArray (fromCount (n :: Count Word64)) 8
  ba <- BA.unsafeFreezeByteArray mba
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
        "set"
        [ bgroup
            "0"
            [ setBytesBench mb1 mb2 mba 0 (n * 8 :: Count Word8)
            , setBytesBench mb1 mb2 mba 0 (n * 4 :: Count Word16)
            , setBytesBench mb1 mb2 mba 0 (n * 2 :: Count Word32)
            , setBytesBench mb1 mb2 mba 0 (n :: Count Word64)
            , setBytesBench mb1 mb2 mba 0 (n * 2 :: Count Float)
            , setBytesBench mb1 mb2 mba 0 (n :: Count Double)
            ]
        , bgroup
            "regular"
            [ setBytesBench mb1 mb2 mba 123 (n * 8 :: Count Word8)
            , setBytesBench mb1 mb2 mba 123 (n * 4 :: Count Word16)
            , setBytesBench mb1 mb2 mba 123 (n * 2 :: Count Word32)
            , setBytesBench mb1 mb2 mba 123 (n :: Count Word64)
            , setBytesBench mb1 mb2 mba 123 (n * 2 :: Count Float)
            , setBytesBench mb1 mb2 mba 123 (n :: Count Double)
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
        "with"
        [ bench "withPtrMBytes (NOINLINE)" $ nfIO (ptrAction_noinline n64 mb1)
        , bench "withPtrMBytes (withPrimBase)" $ nfIO (ptrAction_inline n64 mb2)
        , bench "withPtrMBytes (INLINE)" $ nfIO (ptrAction_inline n64 mb3)
        ]
    ]


setBytesBench ::
     forall a . (Num a, Typeable a, Primitive.Prim a, Prim a)
  => MBytes 'Pin RealWorld
  -> MBytes 'Pin RealWorld
  -> BA.MutableByteArray RealWorld
  -> a
  -> Count a
  -> Benchmark
setBytesBench mb mb2 mba a c@(Count n) =
  bgroup (showsType (Proxy :: Proxy a) "")
    [ bench "setMBytes" $ nfIO (setMBytes mb 0 c a)
    , bench "setOffPtr" $ nfIO (withPtrMBytes mb2 $ \ ptr -> setOffPtr ptr 0 c a :: IO ())
    , bench "setByteArray" $ nfIO (BA.setByteArray mba 0 n a)
    ]


withPtrMBytes_noinline :: MBytes 'Pin s -> (Ptr a -> IO b) -> IO b
withPtrMBytes_noinline mb f = do
  res <- f (getPtrMBytes mb)
  res <$ touch mb
{-# NOINLINE withPtrMBytes_noinline #-}

withPtrMBytes_inline :: MBytes 'Pin s -> (Ptr a -> IO b) -> IO b
withPtrMBytes_inline mb f = do
  res <- f (getPtrMBytes mb)
  res <$ touch mb
{-# INLINE withPtrMBytes_inline #-}

ptrAction :: forall a . (Num a, Prim a) => Count a -> MBytes 'Pin RealWorld -> IO ()
ptrAction (Count n) mb = go 0
  where
    go i
      | i < n = do
        withPtrMBytes mb $ \ptr -> (writeOffPtr ptr (Off i) (123 :: a) :: IO ())
        go (i + 1)
      | otherwise = pure ()

ptrAction_inline :: forall a . (Num a, Prim a) => Count a -> MBytes 'Pin RealWorld -> IO ()
ptrAction_inline (Count n) mb = go 0
  where
    go i
      | i < n = do
        withPtrMBytes_inline mb $ \ptr -> writeOffPtr ptr (Off i) (123 :: a)
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
