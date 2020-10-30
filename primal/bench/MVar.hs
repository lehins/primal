{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import qualified Control.Concurrent.MVar as Base
import Control.Prim.Concurrent.MVar
import Control.Prim.Eval
import Control.Prim.Monad
import Criterion.Main
import Data.Coerce
import qualified Data.IORef as Base
import Data.Prim.Ref
import qualified UnliftIO.MVar as Unlift
import Data.Atomics (atomicModifyIORefCAS, atomicModifyIORefCAS_)

main :: IO ()
main = do
  let !i0 = 16 :: Integer
      !i1 = 17 :: Integer
      envRef :: NFData e => e -> (Ref e RW -> Benchmark) -> Benchmark
      envRef e g = e `deepseq` env (BNF <$> newRef e) $ \ref -> g (coerce ref)
      envIORef :: NFData e => e -> (Base.IORef e -> Benchmark) -> Benchmark
      envIORef e g =
        e `deepseq` env (BNF <$> Base.newIORef e) $ \ref -> g (coerce ref)
      envMVar :: (NFData e) => e -> (MVar e RW -> Benchmark) -> Benchmark
      envMVar e g = e `deepseq` env (BNF <$> newMVar e) $ \(BNF var) -> g var
      envBaseMVar :: (NFData e) => e -> (Base.MVar e -> Benchmark) -> Benchmark
      envBaseMVar e g =
        e `deepseq` env (BNF <$> Base.newMVar e) $ \(BNF var) -> g var
  defaultMain
    [ bgroup
        "Int"
        [ bgroup
            "new"
            [ bench "newRef" $ whnfIO $ newRef i0
            , bench "newIORef (base)" $ whnfIO $ Base.newIORef i0
            , bench "newEmptyMVar" $ whnfIO newEmptyMVar
            , bench "newEmptyMVar (base)" $ whnfIO Base.newEmptyMVar
            , bench "newEmptyMVar (unliftio)" $ whnfIO Unlift.newEmptyMVar
            , bench "newMVar" $ whnfIO $ newMVar i0
            , bench "newMVar (base)" $ whnfIO $ Base.newMVar i0
            , bench "newMVar (unliftio)" $ whnfIO $ Unlift.newMVar i0
            ]
        , bgroup
            "read"
            [ envRef i0 $ \ref -> bench "readRef" $ whnfIO $ readRef ref
            , envIORef i0 $ \ref ->
                bench "readIORef (base)" $ whnfIO $ Base.readIORef ref
            , envMVar i0 $ \ref -> bench "readMVar" $ whnfIO $ readMVar ref
            , envBaseMVar i0 $ \ref ->
                bench "readMVar (base)" $ whnfIO $ Base.readMVar ref
            , envBaseMVar i0 $ \ref ->
                bench "readMVar (unliftio)" $ whnfIO $ Unlift.readMVar ref
            ]
        , bgroup
            "write"
            [ envRef i0 $ \ref -> bench "writeRef" $ whnfIO $ writeRef ref i1
            , envIORef i0 $ \ref ->
                bench "writeIORef" $ whnfIO $ Base.writeIORef ref i1
            , envMVar i0 $ \ref -> bench "writeMVar" $ whnfIO $ writeMVar ref i1
            ]
        , bgroup
            "modify"
            [ envRef i0 $ \ref ->
                bench "modifyRef_" $ whnfIO $ modifyRef_ ref (+ i1)
            , envIORef i0 $ \ref ->
                bench "modifyIORef' (base)" $
                whnfIO $ Base.modifyIORef' ref (+ i1)
            , envMVar i0 $ \ref ->
                bench "modifyMVar_" $ whnfIO $ modifyMVar_ ref (pure . (+ i1))
            , envBaseMVar i0 $ \ref ->
                bench "modifyMVar_ (base)" $
                whnfIO $ Base.modifyMVar_ ref (pure . (+ i1))
            , envBaseMVar i0 $ \ref ->
                bench "modifyMVar_ (unliftio)" $
                whnfIO $ Unlift.modifyMVar_ ref (pure . (+ i1))
            ]
        , bgroup
            "modifyMVarMasked"
            [ envMVar i0 $ \ref ->
                bench "modifyMVarMasked_" $
                whnfIO $ modifyMVarMasked_ ref (pure . (+ i1))
            , envBaseMVar i0 $ \ref ->
                bench "modifyMVarMasked_ (base)" $
                whnfIO $ Base.modifyMVarMasked_ ref (pure . (+ i1))
            , envBaseMVar i0 $ \ref ->
                bench "modifyMVarMasked_ (unliftio)" $
                whnfIO $ Unlift.modifyMVarMasked_ ref (pure . (+ i1))
            ]
        ]
    , bgroup
        "atomicWrite"
        [ envRef i0 $ \ref ->
            bench "atomicWriteRef" $ whnfIO $ atomicWriteRef ref i1
        , envIORef i0 $ \ref ->
            bench "atomicWriteIORef" $ whnfIO $ Base.atomicWriteIORef ref i1
        ]
    , bgroup
        "atomicModify"
        [ envRef i0 $ \ref ->
            bench "atomicModifyRef" $
            whnfIO $ atomicModifyRef ref $ \x -> (x + i1, x)
        , envIORef i0 $ \ref ->
            bench "atomicModifyIORefCAS" $
            whnfIO $ atomicModifyIORefCAS ref $ \x -> (x + i1, x)
        , envIORef i0 $ \ref ->
            bench "atomicModifyIORef'" $
            whnfIO $ Base.atomicModifyIORef' ref $ \x -> (x + i1, x)
        ]
    , bgroup
        "atomicModify_"
        [ envRef i0 $ \ref ->
            bench "atomicModifyRef_" $ whnfIO $ atomicModifyRef_ ref (+ i1)
        , envIORef i0 $ \ref ->
            bench "atomicModifyIORefCAS_" $
            whnfIO $ atomicModifyIORefCAS_ ref (+ i1)
        , envIORef i0 $ \ref ->
            bench "atomicModifyIORef'" $
            whnfIO $ Base.atomicModifyIORef' ref $ \x -> (x + i1, ())
        ]
    ]
