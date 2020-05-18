{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad
import Criterion.Main
import Data.Int
import Data.IORef
import Data.Atomics
--import Data.Prim.Atomic
import Data.Prim.Memory
import Data.Prim.Memory.Bytes
import Data.Prim.Ref
import Prelude as P
import UnliftIO.Async
import GHC.IORef

main :: IO ()
main = do
  let !k = 17 :: Int
      !c = 2 -- caps
      !n = 100000 :: Int
      !e0 = 16 :: Int
      !off0 = 0 :: Off Int
  defaultMain
    [ bgroup
        "Single"
        [ bgroup
            "Read"
            [ env (singletonMBytes e0 :: IO (MBytes 'Inc RW)) $ \mb ->
                bgroup
                  "MBytes"
                  [ bench "readMBytes" $ whnfIO (readOffMBytes mb off0)
                  , bench "atomicReadMBytes" $ whnfIO (atomicReadMBytes mb off0)
                  ]
            , env (newRef e0) $ \ref ->
                bgroup
                  "Ref"
                  [ bench "readRef" $ whnfIO (readRef ref)
                  , bench "atomicReadRef" $ whnfIO (atomicReadRef ref)
                  ]
            , env (newIORef e0) $ \ioRef ->
                bgroup
                  "IORef"
                  [ bench "readIORef" $ whnfIO (readIORef ioRef)
                  , bench "atomicReadIORef" $
                    whnfIO (fst <$> atomicModifyIORef'_ ioRef id)
                  ]
            ]
        , bgroup
            "Write"
            [ env (singletonMBytes e0 :: IO (MBytes 'Inc RW)) $ \mb ->
                bgroup
                  "MBytes"
                  [ bench "writeMBytes" $
                    whnfIO (writeOffMBytes mb off0 (1 :: Int))
                  , bench "atomicWriteMBytes" $
                    whnfIO (atomicWriteMBytes mb off0 (1 :: Int))
                  ]
            , env (newRef e0) $ \ref ->
                bgroup
                  "Ref"
                  [ bench "writeRef" $ whnfIO (writeRef ref 1)
                  , bench "atomicWriteRef" $ whnfIO (atomicWriteRef ref 1)
                  ]
            , env (newIORef e0) $ \ioRef ->
                bgroup
                  "IORef"
                  [ bench "writeIORef" $ whnfIO (writeIORef ioRef 1)
                  , bench "atomicWriteIORef" $ whnfIO (atomicWriteIORef ioRef 1)
                  ]
            ]
        , bgroup
            "AddFetchOld"
            [ env (singletonMBytes e0 :: IO (MBytes 'Inc RW)) $ \mb ->
                bgroup
                  "MBytes"
                  [ bench "modifyFetchOldMem" $
                    whnfIO $ modifyFetchOldMem mb off0 (+ k)
                  , bench "atomicModifyFetchOldMBytes" $
                    whnfIO $ atomicModifyFetchOldMBytes mb off0 (+ k)
                  , bench "atomicAddFetchOldMBytes" $
                    whnfIO $ atomicAddFetchOldMBytes mb off0 k
                  ]
            , env (newRef 0) $ \ref ->
                bgroup
                  "Ref"
                  [ bench "modifyFetchOldRef" $ whnfIO $ modifyFetchOldRef ref (+ k)
                  , bench "atomicModifyFetchOldRef" $
                    whnfIO $ atomicModifyFetchOldRef ref (+ k)
                  ]
            , env (newIORef 0) $ \ioRef ->
                bgroup
                  "IORef"
                  [ bench "modifyFetchOldIORef" $
                    whnfIO $ do
                      a <- readIORef ioRef
                      let a' = a + k
                      a' `seq` (a <$ writeIORef ioRef a')
                  , bench "atomicModifyIORef'" $
                    whnfIO $ atomicModifyIORef' ioRef (\x -> (x + k, x))
                  , bench "atomicModifyIORefCAS" $
                    whnfIO $ atomicModifyIORefCAS ioRef (\x -> (x + k, x))
                  ]
            ]
        , bgroup
            "AddMaybe"
            [ env (singletonMBytes (Just 0 :: Maybe Int) :: IO (MBytes 'Inc RW)) $ \mb ->
                let off0' = Off 0 :: Off (Maybe Int)
                 in bgroup
                      "MBytes"
                      [ bench "modifyFetchOldMem" $
                        nfIO $ modifyFetchOldMem mb off0' (fmap (+ k))
                      ]
            , env (newRef (Just 0)) $ \ref ->
                bgroup
                  "Ref"
                  [ bench "modifyFetchOldRef" $
                    nfIO $ modifyFetchOldRef ref (fmap (+ k))
                  , bench "atomicModifyFetchOldRef" $
                    nfIO $ atomicModifyFetchOldRef ref (fmap (+ k))
                  ]
            , env (newIORef (Just 0)) $ \ioRef ->
                bgroup
                  "IORef"
                  [ bench "modifyFetchOldIORef" $
                    nfIO $ do
                      a <- readIORef ioRef
                      let a' = (k +) <$> a
                      a' `seq` (a <$ writeIORef ioRef a')
                  , bench "atomicModifyIORef'" $
                    nfIO $ atomicModifyIORef' ioRef (\x -> ((k +) <$> x, x))
                  , bench "atomicModifyIORefCAS" $
                    nfIO $ atomicModifyIORefCAS ioRef (\x -> ((k +) <$> x, x))
                  ]
            ]
        ]
    , bgroup
        "Concurrent"
        [ bgroup
            "AddFetchOld"
            [ env (singletonMBytes e0 :: IO (MBytes 'Inc RW)) $ \mb ->
                bgroup
                  "MBytes"
                  [ bench "modifyFetchOldMem (single core)" $
                    nfIO $
                    forM_ [1 .. n] $ \k' -> modifyFetchOldMem mb off0 (+ k')
                  , bench "atomicModifyFetchOldMBytes" $
                    nfIO $
                    pooledForConcurrentlyN_ c [1 .. n] $ \k' ->
                      atomicModifyFetchOldMBytes mb off0 (+ k')
                  , bench "atomicAddFetchOldMBytes" $
                    nfIO $
                    pooledForConcurrentlyN_ c [1 .. n] $ \k' ->
                      atomicAddFetchOldMBytes mb off0 k'
                  ]
            , env (newRef 0) $ \ref ->
                bgroup
                  "Ref"
                  [ bench "modifyFetchOldRef  (single core)" $
                    nfIO $ forM_ [1 .. n] $ \k' -> modifyFetchOldRef ref (+ k')
                  , bench "atomicModifyFetchOldRef" $
                    nfIO $
                    pooledForConcurrentlyN_ c [1 .. n] $ \k' ->
                      atomicModifyFetchOldRef ref (+ k')
                  ]
            , env (newIORef 0) $ \ioRef ->
                bgroup
                  "IORef"
                  [ bench "modifyFetchOldIORef (single core)" $
                    nfIO $
                    forM_ [1 .. n] $ \k' -> do
                      a <- readIORef ioRef
                      let a' = a + k'
                      a' `seq` (a <$ writeIORef ioRef a')
                  , bench "atomicModifyIORef'" $
                    nfIO $
                    pooledForConcurrentlyN_ c [1 .. n] $ \k' ->
                      atomicModifyIORef' ioRef (\x -> (x + k', x))
                  , bench "atomicModifyIORefCAS" $
                    nfIO $
                    pooledForConcurrentlyN_ c [1 .. n] $ \k' ->
                      atomicModifyIORefCAS ioRef (\x -> (x + k', x))
                  ]
            ]
        ]
        -- , bgroup
        --     "Sequential"
        --     [ bgroup
        --         "MBytes"
        --         [ env (newMBytes 0) $ \avar ->
        --             bench "modifyMBytes_" $
        --             whnfIO $ forM_ [1 .. n] (\i -> modifyMBytes_ avar (+ i))
        --         ]
        --     , bgroup
        --         "Ref"
        --         [ env (newRef 0) $ \ref ->
        --             bench "modifyRef_" $
        --             whnfIO $ forM_ [1 .. n] (\i -> modifyRef_ ref (+ i))
        --         ]
        --     , bgroup
        --         "IORef"
        --         [ env (newIORef 0) $ \ref ->
        --             bench "modifyIORef'" $
        --             whnfIO $ forM_ [1 .. n] (\i -> modifyIORef' ref (+ i))
        --         ]
        --     ]
        -- , bgroup
        --     "Concurrent"
        --     [ bgroup
        --         "MBytes"
        --         [ env (newMBytes 0) $ \avar ->
        --             bench "atomicAddFetchOldMBytes" $
        --             whnfIO $
        --             pooledForConcurrentlyN_
        --               c
        --               [1 .. n]
        --               (atomicAddFetchOldMBytes avar)
        --         , env (newMBytes 0) $ \avar ->
        --             bench "atomicModifyMBytes_" $
        --             whnfIO $
        --             pooledForConcurrentlyN_
        --               c
        --               [1 .. n]
        --               (\i -> atomicModifyMBytes_ avar (+ i))
        --         ]
        --     , bgroup
        --         "MBytes (Int32) "
        --         [ env (newMBytes (0 :: Int32)) $ \avar ->
        --             bench "atomicAddFetchOldMBytes" $
        --             whnfIO $
        --             pooledForConcurrentlyN_
        --               c
        --               [1 .. fromIntegral n]
        --               (atomicAddFetchOldMBytes avar)
        --         , env (newMBytes (0 :: Int32)) $ \avar ->
        --             bench "atomicModifyMBytes_" $
        --             whnfIO $
        --             pooledForConcurrentlyN_
        --               c
        --               [1 .. fromIntegral n]
        --               (\i -> atomicModifyMBytes_ avar (+ i))
        --         ]
        --     , bgroup
        --         "Ref"
        --         [ env (newRef 0) $ \ref ->
        --             bench "atomicAddIntRef" $
        --             whnfIO $
        --             pooledForConcurrentlyN_ c [1 .. n] (atomicAddIntRef ref)
        --         , env (newRef 0) $ \ref ->
        --             bench "atomicModifyIntRef_" $
        --             whnfIO $
        --             pooledForConcurrentlyN_
        --               c
        --               [1 .. n]
        --               (\i -> atomicModifyIntRef_ ref (+ i))
        --         ]
        --     , bgroup
        --         "IORef"
        --         [ env (newIORef 0) $ \ref ->
        --             bench "atomicModifyIORef'" $
        --             whnfIO $
        --             pooledForConcurrentlyN_
        --               c
        --               [1 .. n]
        --               (\i -> atomicModifyIORef' ref (\x -> (x + i, ())))
        --         , env (newIORef 0) $ \ref ->
        --             bench "atomicModifyIORefCAS" $
        --             whnfIO $
        --             pooledForConcurrentlyN_
        --               c
        --               [1 .. n]
        --               (\i -> atomicModifyIORefCAS_ ref (+ i))
        --         ]
        --     , bgroup
        --         "Ref"
        --         [ bench "atomicModifyRef'" $
        --           whnfIO $
        --           pooledForConcurrentlyN_
        --             c
        --             [1 .. n]
        --             (\i -> atomicModifyRef' ref (\x -> (x + i, ())))
        --         ]
        --     ]
    ]
