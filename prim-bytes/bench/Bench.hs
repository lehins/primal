{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Proxy
import Data.Typeable
import Criterion.Main
import Data.Prim.Bytes
import Control.Monad.Prim
import qualified Data.Primitive.Types as Primitive
import qualified Data.Primitive.ByteArray as BA
import Data.Int
import Data.Word

main :: IO ()
main = do
  let a = 217 :: Int
      n = 1000000
      nBytes = n * sizeOf a
  mb <- allocPinnedMBytes nBytes
  mba <- BA.newByteArray nBytes
  defaultMain
    [ bgroup "set"
      [ setBytesBench @Word8 mb mba n
      , setBytesBench @Word16 mb mba n
      , setBytesBench @Word32 mb mba n
      , setBytesBench @Word64 mb mba n
      , setBytesBench @Word mb mba n
      , setBytesBench @Float mb mba n
      , setBytesBench @Double mb mba n
      ]
    ]

setBytesBench ::
     forall a p. (Num a, Typeable a, Primitive.Prim a, Prim a)
  => MBytes p RealWorld
  -> BA.MutableByteArray RealWorld
  -> Int
  -> Benchmark
setBytesBench mb mba n =
  bgroup (showsType (Proxy :: Proxy a) "")
    [ bench "setMBytes" $ nfIO (setMBytes mb 0 (Count n) a)
    , bench "setByteArray" $ nfIO (BA.setByteArray mba 0 n a)
    ]
  where
    a = 123 :: a
