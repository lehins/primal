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
  mb1 <- allocPinnedAlignedMBytes n64
  mb2 <- allocPinnedAlignedMBytes n64
  mb3 <- allocPinnedAlignedMBytes n64
  mba <- BA.newAlignedPinnedByteArray (fromCount (n :: Count Word64)) 8
  defaultMain
    [ bgroup
        "set"
        [ setBytesBench mb1 mb2 mba (n * 8 :: Count Word8)
        , setBytesBench mb1 mb2 mba (n * 4 :: Count Word16)
        , setBytesBench mb1 mb2 mba (n * 2 :: Count Word32)
        , setBytesBench mb1 mb2 mba (n :: Count Word64)
        , setBytesBench mb1 mb2 mba (n * 2 :: Count Float)
        , setBytesBench mb1 mb2 mba (n :: Count Double)
        ]
    , bgroup
        "with"
        [ bench "withPtrMBytes (NOINLINE)" $ nfIO (ptrAction_noinline n64 mb1)
        , bench "withPtrMBytes (withPrimBase)" $ nfIO (ptrAction_inline n64 mb2)
        , bench "withPtrMBytes (INLINE)" $ nfIO (ptrAction_inline n64 mb3)
        ]
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

setBytesBench ::
     forall a . (Num a, Typeable a, Primitive.Prim a, Prim a)
  => MBytes 'Pin RealWorld
  -> MBytes 'Pin RealWorld
  -> BA.MutableByteArray RealWorld
  -> Count a
  -> Benchmark
setBytesBench mb mb2 mba c@(Count n) =
  bgroup (showsType (Proxy :: Proxy a) "")
    [ bench "setMBytes" $ nfIO (setMBytes mb 0 c a)
    , bench "setOffPtr" $ nfIO (withPtrMBytes mb2 $ \ ptr -> setOffPtr ptr 0 c a :: IO ())
    , bench "setByteArray" $ nfIO (BA.setByteArray mba 0 n a)
    ]
  where
    a = 0xffff :: a
