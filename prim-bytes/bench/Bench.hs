{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Proxy
import Data.Typeable
import Criterion.Main
import Data.Prim.Bytes
import Data.Prim.Ptr
import Control.Monad.Prim
import qualified Data.Primitive.Types as Primitive
import qualified Data.Primitive.ByteArray as BA
import Data.Int
import Data.Word

main :: IO ()
main = do
  let n = 1000000 :: Count a
      n64 = n :: Count Word64
  mb1 <- allocPinnedMBytes n64
  mb2 <- allocPinnedMBytes n64
  mb3 <- allocPinnedMBytes n64
  mba <- BA.newByteArray $ fromCount (n :: Count Word64)
  defaultMain
      -- bgroup "set"
    --   [ setBytesBench @Word8 mb mba (n * 8)
    --   , setBytesBench @Word16 mb mba (n * 4)
    --   , setBytesBench @Word32 mb mba (n * 2)
    --   , setBytesBench @Word64 mb mba n
    --   , setBytesBench @Word mb mba n
    --   , setBytesBench @Float mb mba (n * 2)
    --   , setBytesBench @Double mb mba n
    --   ]
    -- ,
    [ bgroup
        "with"
        [ bench "withPtrMBytes (NOINLINE)" $
          nfIO (ptrAction_noinline n64 mb1)
        , bench "withPtrMBytes (withPrimBase)" $
          nfIO (ptrAction_inline n64 mb2)
        , bench "withPtrMBytes (INLINE)" $
          nfIO (ptrAction_inline n64 mb3)
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
     forall a p. (Num a, Typeable a, Primitive.Prim a, Prim a)
  => MBytes p RealWorld
  -> BA.MutableByteArray RealWorld
  -> Count a
  -> Benchmark
setBytesBench mb mba c@(Count n) =
  bgroup (showsType (Proxy :: Proxy a) "")
    [ bench "setMBytes" $ nfIO (setMBytes mb 0 c a)
    , bench "setByteArray" $ nfIO (BA.setByteArray mba 0 n a)
    ]
  where
    a = 123 :: a
