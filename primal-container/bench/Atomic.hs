{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.DeepSeq
import Control.Concurrent
import Control.Monad
import Criterion.Main
import Data.Int
import Data.IORef as IO
import Data.Atomics
--import Data.Prim.Atomic
import Data.Prim.Memory
import Data.Prim.Memory.Bytes
import Data.Prim.Memory.PUArray
import Data.Prim.Memory.Addr
import Data.Prim.MArray.Boxed.Small
import Data.Prim.MRef
import Data.Prim.Ref
import Prelude as P
import UnliftIO.Async

newtype BogusNF a = BogusNF a

instance NFData (BogusNF a) where
  rnf (BogusNF a) = a `seq` ()

main :: IO ()
main = do
  c <- getNumCapabilities
  let !k = 17 :: Int
      !n = 100000 :: Int
      !e0 = 16 :: Int
      !off0 = 0 :: Off Int
      !toff0 = 0 :: Off (Int, Int)
      tup x = (x, x)
      addTup k' (x, y) =
        let a'@(!_, !_) = (x + k', y - k')
         in a'
      mkMBytes :: Prim e => (Int -> e) -> IO (MBytes 'Inc RW)
      mkMBytes f = singletonMBytes (f e0)
      mkPUMArray :: Prim e => (Int -> e) -> IO (PUMArray 'Inc e RW)
      mkPUMArray f = newMRef (f e0)
      mkRef :: (Int -> e) -> IO (Ref e RW)
      mkRef f = newMRef (f e0)
      mkSBMArray :: (Int -> e) -> IO (SBMArray e RW)
      mkSBMArray f = newMRef (f e0)
      mkIORef :: (Int -> e) -> IO (IO.IORef e)
      mkIORef f = newIORef (f e0)
      benchSeq mkEnv name f =
        env (BogusNF <$> mkEnv) $ \(BogusNF ref) -> bench name $ whnfIO $ forM_ [1 .. n] (f ref)
      benchConc mkEnv name f =
        env (BogusNF <$> mkEnv) $ \(BogusNF ref) ->
          bench name $ whnfIO $ pooledForConcurrentlyN_ c [1 .. n] (f ref)
  defaultMain
    [ bgroup
        "Single"
        [ bgroup
            "Read"
            [ env (singletonMBytes e0 :: IO (MBytes 'Inc RW)) $ \mb ->
                bgroup
                  "MBytes"
                  [ bench "readMBytes" $ nfIO (readOffMBytes mb off0)
                  , bench "atomicReadMBytes" $ nfIO (atomicReadMBytes mb off0)
                  ]
            , env (newMRef e0 :: IO (PUMArray 'Inc Int RW)) $ \mb ->
                bgroup
                  "PUMArray"
                  [ bench "readMRef" $ nfIO (readMRef mb)
                  , bench "atomicReadMRef" $ nfIO (atomicReadMRef mb)
                  ]
            , env (newMRef e0 :: IO (MAddr Int RW)) $ \mb ->
                bgroup
                  "MAddr"
                  [ bench "readMRef" $ nfIO (readMRef mb)
                  , bench "atomicReadMRef" $ nfIO (atomicReadMRef mb)
                  ]
            , env (BogusNF <$> newRef e0) $ \(BogusNF ref) ->
                bgroup
                  "Ref"
                  [ bench "readRef" $ nfIO (readRef ref)
                  , bench "atomicReadRef" $ nfIO (atomicReadRef ref)
                  , bench "atomicReadMRef" $ nfIO (atomicReadMRef ref)
                  ]
            , env (newIORef e0) $ \ioRef ->
                bgroup
                  "IORef"
                  [ bench "readIORef" $ nfIO (readIORef ioRef)
                  , bench "atomicReadIORef" $
                    nfIO (atomicModifyIORef' ioRef (\x -> (x,x)))
                  ]
            ]
        , bgroup
            "Write"
            [ env (singletonMBytes e0 :: IO (MBytes 'Inc RW)) $ \mb ->
                bgroup
                  "MBytes"
                  [ bench "writeMBytes" $
                    nfIO (writeOffMBytes mb off0 (1 :: Int))
                  , bench "atomicWriteMBytes" $
                    nfIO (atomicWriteMBytes mb off0 (1 :: Int))
                  ]
            , env (BogusNF <$> newRef e0) $ \(BogusNF ref) ->
                bgroup
                  "Ref"
                  [ bench "writeRef" $ nfIO (writeRef ref 1)
                  , bench "atomicWriteRef" $ nfIO (atomicWriteRef ref 1)
                  ]
            , env (newIORef e0) $ \ioRef ->
                bgroup
                  "IORef"
                  [ bench "writeIORef" $ nfIO (writeIORef ioRef 1)
                  , bench "atomicWriteIORef" $ nfIO (atomicWriteIORef ioRef 1)
                  ]
            ]
        , bgroup
            "AddFetchOld"
            [ env (singletonMBytes e0 :: IO (MBytes 'Inc RW)) $ \mb ->
                bgroup
                  "MBytes"
                  [ bench "modifyFetchOldMutMem" $
                    nfIO $ modifyFetchOldMutMem mb off0 (+ k)
                  , bench "atomicModifyFetchOldMBytes" $
                    nfIO $ atomicModifyFetchOldMBytes mb off0 (+ k)
                  , bench "atomicBoolModifyFetchOldMBytes" $
                    nfIO $ atomicBoolModifyFetchOldMBytes mb off0 (+ k)
                  , bench "atomicAddFetchOldMBytes" $
                    nfIO $ atomicAddFetchOldMBytes mb off0 k
                  ]
            , env (singletonMBytes (Atom e0) :: IO (MBytes 'Inc RW)) $ \mb ->
                bgroup
                  "MBytes (Atom)"
                  [ bench "modifyFetchOldMutMem" $
                    nfIO $ modifyFetchOldMutMem mb (coerce off0) (+ Atom k)
                  , bench "atomicModifyFetchOldMBytes" $
                    nfIO $
                    atomicModifyFetchOldMBytes mb (coerce off0) (+ Atom k)
                  ]
            , bgroup
                "PUMArray"
                [ env (newMRef e0 :: IO (PUMArray 'Inc Int RW)) $ \mb ->
                    bench "atomicAddFetchOldMRef" $
                    nfIO $ atomicAddFetchOldMRef mb k
                , env (newMRef (fromIntegral e0) :: IO (PUMArray 'Inc Int64 RW)) $ \mb ->
                    bench "atomicAddFetchOldMRef" $
                    nfIO $ atomicAddFetchOldMRef mb (fromIntegral k)
                ]
            , env (BogusNF <$> newRef 0) $ \(BogusNF ref) ->
                bgroup
                  "Ref"
                  [ bench "modifyFetchOldRef" $
                    nfIO $ modifyFetchOldRef ref (+ k)
                  , bench "modifyFetchOldMRef" $
                    nfIO $ modifyFetchOldMRef ref (+ k)
                  , bench "atomicModifyFetchOldRef" $
                    nfIO $ atomicModifyFetchOldRef ref (+ k)
                  , bench "atomicModifyFetchOldMRef" $
                    nfIO $ atomicModifyFetchOldMRef ref (+ k)
                  , bench "atomicAddFetchOldMRef" $
                    nfIO $ atomicAddFetchOldMRef ref k
                  ]
            , env (newIORef 0) $ \ioRef ->
                bgroup
                  "IORef"
                  [ bench "modifyFetchOldIORef" $
                    nfIO $ do
                      a <- readIORef ioRef
                      let a' = a + k
                      a' `seq` (a <$ writeIORef ioRef a')
                  , bench "atomicModifyIORef'" $
                    nfIO $ atomicModifyIORef' ioRef (\x -> (x + k, x))
                  , bench "atomicModifyIORefCAS" $
                    nfIO $ atomicModifyIORefCAS ioRef (\x -> (x + k, x))
                  ]
            ]
        , bgroup
            "AddMaybe"
            [ env (singletonMBytes (Just 0 :: Maybe Int) :: IO (MBytes 'Inc RW)) $ \mb ->
                let off0' = Off 0 :: Off (Maybe Int)
                 in bgroup
                      "MBytes"
                      [ bench "modifyFetchOldMutMem" $
                        nfIO $ modifyFetchOldMutMem mb off0' (fmap (+ k))
                      ]
            , env (BogusNF <$> newRef (Just 0)) $ \(BogusNF ref) ->
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
        "Sequential"
        [ bgroup
            "AddFetchOld"
            [ bgroup
                "MBytes"
                [ benchSeq (mkMBytes id) "modifyFetchOldMutMem (Int)" $ \mb k' ->
                    modifyFetchOldMutMem mb off0 (+ k')
                , benchSeq (mkMBytes tup) "modifyFetchOldMutMem (Int, Int)" $ \mb k' ->
                    modifyFetchOldMutMem
                      mb
                      (coerce toff0 :: Off (Int, Int))
                      (addTup k')
                , benchSeq (mkMBytes Atom) "modifyFetchOldMutMem (Atom Int)" $ \mb k' ->
                    modifyFetchOldMutMem mb (coerce off0) (+ Atom k')
                , benchSeq
                    (mkMBytes (Atom . tup))
                    "modifyFetchOldMutMem (Atom (Int, Int))" $ \mb k' ->
                    modifyFetchOldMutMem
                      mb
                      (coerce toff0 :: Off (Atom (Int, Int)))
                      (\(Atom a) -> Atom (addTup k' a))
                ]
            , bgroup
                "Ref"
                [ benchSeq (mkRef id) "modifyFetchOldRef  (Int)" $ \ref k' ->
                    modifyFetchOldRef ref (+ k')
                , benchSeq (mkRef tup) "modifyFetchOldRef (Int, Int)" $ \ref k' -> do
                    modifyFetchOldRef ref (addTup k')
                ]
            , bgroup
                "IORef"
                [ benchSeq (mkIORef id) "modifyFetchOldIORef (Int)" $ \ioRef k' -> do
                    a <- readIORef ioRef
                    writeIORef ioRef $! a + k'
                    pure a
                , benchSeq (mkIORef tup) "modifyFetchOldIORef (Int, Int)" $ \ioRef k' -> do
                    a <- readIORef ioRef
                    writeIORef ioRef $! addTup k' a
                    pure a
                ]
            ]
        ]
    , bgroup
        "Concurrent"
        [ bgroup
            "AddFetchOld"
            [ bgroup
                "MBytes"
                [ benchConc (mkMBytes id) "atomicModifyFetchOldMBytes" $ \mb k' ->
                    atomicModifyFetchOldMBytes mb off0 (+ k')
                , benchConc (mkMBytes id) "atomicBoolModifyFetchOldMBytes" $ \mb k' ->
                    atomicBoolModifyFetchOldMBytes mb off0 (+ k')
                , benchConc (mkMBytes id) "atomicAddFetchOldMBytes" $ \mb ->
                    atomicAddFetchOldMBytes mb off0
                ]
            , bgroup
                "MBytes (Atom)"
                [ benchConc (mkMBytes Atom) "atomicModifyFetchOldMBytes" $ \mb k' ->
                    atomicModifyFetchOldMBytes mb (coerce off0) (+ Atom k')
                ]
            , bgroup
                "PUMArray"
                [ benchConc (mkPUMArray id) "atomicModifyFetchOldMRef" $ \mb k' ->
                    atomicModifyFetchOldMRef mb (+ k')
                , benchConc (mkPUMArray id) "atomicAddFetchOldMRef" $ \mb ->
                    atomicAddFetchOldMRef mb
                , benchConc (mkPUMArray fromIntegral) "atomicAddFetchOldMRef" $ \mb k' ->
                    atomicAddFetchOldMRef mb (fromIntegral k' :: Int64)
                ]
            , bgroup
                "SBMArray"
                [ benchConc (mkSBMArray id) "atomicModifyFetchOldMRef" $ \ref k' ->
                    atomicModifyFetchOldMRef ref (+ k')
                , benchConc (mkSBMArray id) "atomicAddFetchOldMRef" $ \ref k' ->
                    atomicAddFetchOldMRef ref k'
                ]
            , bgroup
                "Ref"
                  -- benchConc (mkRef id) "atomicModifyRef" $ \ref k' ->
                  --     atomicModifyRef ref (\x -> (x + k', x))
                  -- , benchConc (mkRef id) "atomicModifyFetchOldRef" $ \ref k' ->
                  --     atomicModifyFetchOldRef ref (+ k')
                  -- ,
                [ benchConc (mkRef id) "atomicAddFetchOldMRef" $ \ref k' ->
                    atomicAddFetchOldMRef ref k'
                , benchConc (mkRef id) "atomicAddFetchNewMRef" $ \ref k' ->
                    atomicAddFetchOldMRef ref k'
                ]
            , bgroup
                "IORef"
                [ benchConc (mkIORef id) "atomicModifyIORef'" $ \ioRef k' ->
                    atomicModifyIORef' ioRef (\x -> (x + k', x))
                , benchConc (mkIORef id) "atomicModifyIORefCAS" $ \ioRef k' ->
                    atomicModifyIORefCAS ioRef (\x -> (x + k', x))
                ]
            ]
        ]
    , bgroup
        "TupleConcurrent"
        [ bgroup
            "AddFetchOld"
            [ bgroup
                "MBytes"
                [ benchConc (mkMBytes (Atom . tup)) "atomicModifyFetchOldMBytes" $ \mb k' ->
                    atomicModifyFetchOldMBytes
                      mb
                      (coerce toff0 :: Off (Atom (Int, Int)))
                      (\(Atom a) -> Atom (addTup k' a))
                ]
            , bgroup
                "PUMArray"
                [ benchConc
                    (mkPUMArray (Atom . tup))
                    "atomicModifyFetchOldMRef" $ \mb k' ->
                    atomicModifyFetchOldMRef
                      mb
                      (\(Atom a) -> Atom (addTup k' a))
                ]
            , bgroup
                "Ref"
                [ benchConc (mkRef tup) "atomicModifyFetchOldRef" $ \ref k' ->
                    atomicModifyFetchOldRef ref (addTup k')
                , benchConc (mkRef tup) "atomicModifyFetchOldMRef" $ \ref k' ->
                    atomicModifyFetchOldMRef ref (addTup k')
                ]
            , bgroup
                "IORef"
                [ benchConc (mkIORef tup) "atomicModifyIORef'" $ \ioRef k' ->
                    atomicModifyIORef' ioRef (\a -> (addTup k' a, a))
                , benchConc (mkIORef tup) "atomicModifyIORefCAS" $ \ioRef k' ->
                    atomicModifyIORefCAS ioRef (\a -> (addTup k' a, a))
                ]
            ]
        ]
        -- , bgroup
        --     "Sequential"
        --     [ bgroup
        --         "MBytes"
        --         [ env (newMBytes 0) $ \avar ->
        --             bench "modifyMBytes_" $
        --             nfIO $ forM_ [1 .. n] (\i -> modifyMBytes_ avar (+ i))
        --         ]
        --     , bgroup
        --         "Ref"
        --         [ env (newRef 0) $ \ref ->
        --             bench "modifyRef_" $
        --             nfIO $ forM_ [1 .. n] (\i -> modifyRef_ ref (+ i))
        --         ]
        --     , bgroup
        --         "IORef"
        --         [ env (newIORef 0) $ \ref ->
        --             bench "modifyIORef'" $
        --             nfIO $ forM_ [1 .. n] (\i -> modifyIORef' ref (+ i))
        --         ]
        --     ]
        -- , bgroup
        --     "Concurrent"
        --     [ bgroup
        --         "MBytes"
        --         [ env (newMBytes 0) $ \avar ->
        --             bench "atomicAddFetchOldMBytes" $
        --             nfIO $
        --             pooledForConcurrentlyN_
        --               c
        --               [1 .. n]
        --               (atomicAddFetchOldMBytes avar)
        --         , env (newMBytes 0) $ \avar ->
        --             bench "atomicModifyMBytes_" $
        --             nfIO $
        --             pooledForConcurrentlyN_
        --               c
        --               [1 .. n]
        --               (\i -> atomicModifyMBytes_ avar (+ i))
        --         ]
        --     , bgroup
        --         "MBytes (Int32) "
        --         [ env (newMBytes (0 :: Int32)) $ \avar ->
        --             bench "atomicAddFetchOldMBytes" $
        --             nfIO $
        --             pooledForConcurrentlyN_
        --               c
        --               [1 .. fromIntegral n]
        --               (atomicAddFetchOldMBytes avar)
        --         , env (newMBytes (0 :: Int32)) $ \avar ->
        --             bench "atomicModifyMBytes_" $
        --             nfIO $
        --             pooledForConcurrentlyN_
        --               c
        --               [1 .. fromIntegral n]
        --               (\i -> atomicModifyMBytes_ avar (+ i))
        --         ]
        --     , bgroup
        --         "Ref"
        --         [ env (newRef 0) $ \ref ->
        --             bench "atomicAddIntRef" $
        --             nfIO $
        --             pooledForConcurrentlyN_ c [1 .. n] (atomicAddIntRef ref)
        --         , env (newRef 0) $ \ref ->
        --             bench "atomicModifyIntRef_" $
        --             nfIO $
        --             pooledForConcurrentlyN_
        --               c
        --               [1 .. n]
        --               (\i -> atomicModifyIntRef_ ref (+ i))
        --         ]
        --     , bgroup
        --         "IORef"
        --         [ env (newIORef 0) $ \ref ->
        --             bench "atomicModifyIORef'" $
        --             nfIO $
        --             pooledForConcurrentlyN_
        --               c
        --               [1 .. n]
        --               (\i -> atomicModifyIORef' ref (\x -> (x + i, ())))
        --         , env (newIORef 0) $ \ref ->
        --             bench "atomicModifyIORefCAS" $
        --             nfIO $
        --             pooledForConcurrentlyN_
        --               c
        --               [1 .. n]
        --               (\i -> atomicModifyIORefCAS_ ref (+ i))
        --         ]
        --     , bgroup
        --         "Ref"
        --         [ bench "atomicModifyRef'" $
        --           nfIO $
        --           pooledForConcurrentlyN_
        --             c
        --             [1 .. n]
        --             (\i -> atomicModifyRef' ref (\x -> (x + i, ())))
        --         ]
        --     ]
    ]
