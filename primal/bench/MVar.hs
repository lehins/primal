{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import qualified Control.Concurrent.MVar as Base
import Primal.Concurrent.MVar
import Primal.Eval
import Primal.Monad
import Criterion.Main
import Data.Coerce
import qualified Data.IORef as Base
import Primal.Ref
import qualified UnliftIO.MVar as Unlift
import Data.Atomics (atomicModifyIORefCAS, atomicModifyIORefCAS_)

main :: IO ()
main = do
  let !i0 = 16 :: Integer
      !i1 = 17 :: Integer
      envBRef :: NFData e => e -> (BRef e RW -> Benchmark) -> Benchmark
      envBRef e g = e `deepseq` env (BNF <$> newBRef e) $ \ref -> g (coerce ref)
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
            [ bench "newBRef" $ whnfIO $ newBRef i0
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
            [ envBRef i0 $ \ref -> bench "readBRef" $ whnfIO $ readBRef ref
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
            [ envBRef i0 $ \ref -> bench "writeBRef" $ whnfIO $ writeBRef ref i1
            , envIORef i0 $ \ref ->
                bench "writeIORef" $ whnfIO $ Base.writeIORef ref i1
            , envMVar i0 $ \ref -> bench "writeMVar" $ whnfIO $ writeMVar ref i1
            ]
        , bgroup
            "modify"
            [ envBRef i0 $ \ref ->
                bench "modifyBRef_" $ whnfIO $ modifyBRef_ ref (+ i1)
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
        [ envBRef i0 $ \ref ->
            bench "atomicWriteBRef" $ whnfIO $ atomicWriteBRef ref i1
        , envIORef i0 $ \ref ->
            bench "atomicWriteIORef" $ whnfIO $ Base.atomicWriteIORef ref i1
        ]
    , bgroup
        "atomicModify"
        [ envBRef i0 $ \ref ->
            bench "atomicModifyBRef" $
            whnfIO $ atomicModifyBRef ref $ \x -> (x + i1, x)
        , envIORef i0 $ \ref ->
            bench "atomicModifyIORefCAS" $
            whnfIO $ atomicModifyIORefCAS ref $ \x -> (x + i1, x)
        , envIORef i0 $ \ref ->
            bench "atomicModifyIORef'" $
            whnfIO $ Base.atomicModifyIORef' ref $ \x -> (x + i1, x)
        ]
    , bgroup
        "atomicModify_"
        [ envBRef i0 $ \ref ->
            bench "atomicModifyBRef_" $ whnfIO $ atomicModifyBRef_ ref (+ i1)
        , envIORef i0 $ \ref ->
            bench "atomicModifyIORefCAS_" $
            whnfIO $ atomicModifyIORefCAS_ ref (+ i1)
        , envIORef i0 $ \ref ->
            bench "atomicModifyIORef'" $
            whnfIO $ Base.atomicModifyIORef' ref $ \x -> (x + i1, ())
        ]
    ]
