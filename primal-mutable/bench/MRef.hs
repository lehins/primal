{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.DeepSeq
--import Control.Concurrent
import Criterion.Main
import Data.IORef
--import Data.Atomics
--import Data.Prim.Atomic
import Data.Prim.Memory
import Data.Prim.Memory.Addr
-- import Data.Prim.MArray.Boxed.Small
-- import Data.Prim.MRef
import Data.Prim.Ref
import qualified Data.Prim.PVar as PV
--import UnliftIO.Async
import qualified Data.Mutable as M
import qualified Data.Primitive.Types as P
import qualified Data.Vector.Unboxed as U
import qualified Foreign.Storable as S

-- | Bogus Normal Form
newtype BNF a = BNF a

instance NFData (BNF a) where
  rnf (BNF a) = a `seq` ()

main :: IO ()
main = do
  let !i0 = 16 :: Int
      !i1 = 17 :: Int
      !w0 = 18 :: Word
      !w1 = 19 :: Word
      envPVar ::
           (PV.Prim e, NFData e)
        => e
        -> (PV.PVar e RW -> Benchmark)
        -> Benchmark
      envPVar e g = e `deepseq` env (BNF <$> PV.newPVar e) $ \(BNF var) -> g var
      envMAddr ::
           (Prim e, NFData e) => e -> (MAddr e RW -> Benchmark) -> Benchmark
      envMAddr e g = e `deepseq` env (BNF <$> newMAddr e) $ \(BNF var) -> g var
      envRef :: NFData e => e -> (Ref e RW -> Benchmark) -> Benchmark
      envRef e g = e `deepseq` env (BNF <$> newRef e) $ \ref -> g (coerce ref)
      envIORef :: NFData e => e -> (IORef e -> Benchmark) -> Benchmark
      envIORef e g =
        e `deepseq` env (BNF <$> newIORef e) $ \ref -> g (coerce ref)
      envPRef ::
           (P.Prim e, NFData e) => e -> (M.PRef RW e -> Benchmark) -> Benchmark
      envPRef e g = e `deepseq` env (BNF <$> M.newRef e) $ \(BNF var) -> g var
      envSRef ::
           (S.Storable e, NFData e)
        => e
        -> (M.SRef RW e -> Benchmark)
        -> Benchmark
      envSRef e g = e `deepseq` env (BNF <$> M.newRef e) $ \(BNF var) -> g var
      envURef ::
           (U.Unbox e, NFData e) => e -> (M.URef RW e -> Benchmark) -> Benchmark
      envURef e g = e `deepseq` env (BNF <$> M.newRef e) $ \(BNF var) -> g var
  defaultMain
    [ bgroup
        "Int"
        [ bgroup
            "read"
            [ envRef i0 $ \ref -> bench "Ref" $ whnfIO $ readRef ref
            , envIORef i0 $ \ref -> bench "IORef" $ whnfIO $ readIORef ref
            , envPVar i0 $ \ref -> bench "PVar" $ whnfIO $ PV.readPVar ref
            , envMAddr i0 $ \ref -> bench "MAddr" $ whnfIO $ readMAddr ref
            , envPRef i0 $ \ref -> bench "M.PRef" $ whnfIO $ M.readRef ref
            , envSRef i0 $ \ref -> bench "M.SRef" $ whnfIO $ M.readRef ref
            , envURef i0 $ \ref -> bench "M.URef" $ whnfIO $ M.readRef ref
            ]
        , bgroup
            "write"
            [ envRef i0 $ \ref -> bench "Ref" $ whnfIO $ writeRef ref i1
            , envIORef i0 $ \ref -> bench "IORef" $ whnfIO $ writeIORef ref i1
            , envPVar i0 $ \ref -> bench "PVar" $ whnfIO $ PV.writePVar ref i1
            , envMAddr i0 $ \ref -> bench "MAddr" $ whnfIO $ writeMAddr ref i1
            , envPRef i0 $ \ref -> bench "M.PRef" $ whnfIO $ M.writeRef ref i1
            , envSRef i0 $ \ref -> bench "M.SRef" $ whnfIO $ M.writeRef ref i1
            , envURef i0 $ \ref -> bench "M.URef" $ whnfIO $ M.writeRef ref i1
            ]
        ]
    , bgroup
        "(Int, Word)"
        [ bgroup
            "read"
            [ envRef (i0, w0) $ \ref -> bench "Ref" $ nfIO $ readRef ref
            , envIORef (i0, w0) $ \ref -> bench "IORef" $ nfIO $ readIORef ref
            , envPVar (i0, w0) $ \ref -> bench "PVar" $ nfIO $ PV.readPVar ref
            , envMAddr (i0, w0) $ \ref -> bench "MAddr" $ nfIO $ readMAddr ref
            , envURef (i0, w0) $ \ref -> bench "M.URef" $ nfIO $ M.readRef ref
            ]
        , bgroup
            "write"
            [ envRef (i0, w0) $ \ref ->
                bench "Ref" $ whnfIO $ writeRef ref (i1, w1)
            , envIORef (i0, w0) $ \ref ->
                bench "IORef" $ whnfIO $ writeIORef ref (i1, w1)
            , envPVar (i0, w0) $ \ref ->
                bench "PVar" $ whnfIO $ PV.writePVar ref (i1, w1)
            , envMAddr (i0, w0) $ \ref ->
                bench "MAddr" $ whnfIO $ writeMAddr ref (i1, w1)
            , envURef (i0, w0) $ \ref ->
                bench "M.URef" $ whnfIO $ M.writeRef ref (i1, w1)
            ]
        ]
    , bgroup
        "Maybe Int"
        [ bgroup
            "read"
            [ envRef (Just i0) $ \ref -> bench "Ref" $ nfIO $ readRef ref
            , envIORef (Just i0) $ \ref -> bench "IORef" $ nfIO $ readIORef ref
            , envPVar (Just i0) $ \ref -> bench "PVar" $ nfIO $ PV.readPVar ref
            , envMAddr (Just i0) $ \ref -> bench "MAddr" $ nfIO $ readMAddr ref
            ]
        , bgroup
            "write"
            [ envRef (Just i0) $ \ref -> bench "Ref" $ whnfIO $ writeRef ref (Just i1)
            , envIORef (Just i0) $ \ref -> bench "IORef" $ whnfIO $ writeIORef ref (Just i1)
            , envPVar (Just i0) $ \ref -> bench "PVar" $ whnfIO $ PV.writePVar ref (Just i1)
            , envMAddr (Just i0) $ \ref -> bench "MAddr" $ whnfIO $ writeMAddr ref (Just i1)
            ]
        ]
    ]
