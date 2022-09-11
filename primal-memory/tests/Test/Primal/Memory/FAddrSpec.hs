{-# LANGUAGE ScopedTypeVariables #-}

module Test.Primal.Memory.FAddrSpec
  ( spec
  ) where

import Primal.Memory.FAddr
import Primal.Memory.Ptr
import Test.Primal.Memory.Common

propReadWrite ::
     (Show e, Eq e, Arbitrary e, Unbox e)
  => (Count e -> IO (FMAddr e RW))
  -> Positive (Count e)
  -> Off e
  -> e
  -> Property
propReadWrite alloc (Positive c) o e =
  monadicIO $
  run $ do
    fma <- alloc c
    let o' = o `mod` countToOff c
    writeOffFMAddr fma o' e
    readOffFMAddr fma o' `shouldReturn` e

allocFMAddrSpec :: forall e. (Show e, Eq e, Num e, Unbox e, Arbitrary e) => Proxy e -> Spec
allocFMAddrSpec _ = do
  describe "allocFMAddr" $ do
    prop "read/write" $ propReadWrite (allocFMAddr :: Count e -> IO (FMAddr e RW))
  describe "allocZeroFMAddr" $ do
    prop "read/write" $ propReadWrite (allocZeroFMAddr :: Count e -> IO (FMAddr e RW))
    prop "all zeros" $ \ (c :: Count e) -> monadicIO $ run $ do
      fma <- allocZeroFMAddr (c :: Count e)
      forM_ [0.. countToOff c - 1] $ \off ->
        readOffFMAddr fma off `shouldReturn` 0

reallocFMAddrSpec :: forall e. (Show e, Eq e, Num e, Unbox e, Arbitrary e) => Proxy e -> Spec
reallocFMAddrSpec _ =
  describe "reallocFMAddr" $ do
    prop "read/write" $ propReadWrite (reallocPtrFMAddr nullPtr :: Count e -> IO (FMAddr e RW))
    prop "grow" $ \(Positive (c :: Count e)) (o :: Off e) e -> do
      monadicIO $
        run $ do
          fma <- reallocPtrFMAddr nullPtr c
          let o' = o `mod` countToOff c
          writeOffFMAddr fma o' e
          fma' <- reallocFMAddr fma (c * 10)
          readOffFMAddr fma' o' `shouldReturn` e
    prop "shift+grow" $ \(Positive (c :: Count e)) (o :: Off e) e -> do
      monadicIO $
        run $ do
          fma <- reallocPtrFMAddr nullPtr c
          let o' = o `mod` countToOff c
          writeOffFMAddr fma o' e
          let fma' = plusOffFMAddr fma o'
          readFMAddr fma' `shouldReturn` e
          fma'' <- reallocFMAddr fma' (c * 10)
          readFMAddr fma'' `shouldReturn` e


spec :: Spec
spec = do
  describe "FMAddr" $
    describe "Allocation" $ do
      allocFMAddrSpec (Proxy :: Proxy Int)
      reallocFMAddrSpec (Proxy :: Proxy Int)
