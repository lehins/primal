{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Test.Primal.Memory.BytesSpec
  ( module Test.Primal.Memory.BytesSpec
  , module Primal.Memory.Bytes
  ) where

import Control.Concurrent
import Control.DeepSeq
import Control.Monad
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.List as List
import Foreign.Storable
import Numeric
import Primal.Array
import Primal.Foreign hiding (Any)
import Primal.Memory.Addr
import Primal.Memory.Bytes
import System.Timeout
import Test.Primal
import Test.Primal.Memory


type NEBytes p e = NEMem (MBytes p) e

-- This spec is called in MemSpec module
bytesSpec ::
     forall (p :: Pinned) e.
     (NFData e, Eq e, Show e, Unbox e, Arbitrary e, Typeable p, Typeable e)
  => Spec
bytesSpec = do
  let bytesTypeName =
        showsType (Proxy :: Proxy (Bytes p)) .
        (' ' :) . showsType (Proxy :: Proxy e) $
        ""
  describe bytesTypeName $ do
    memSpec @(MBytes p) @e
    describe "memset" $ do
      prop "empty" $ \(e :: e) -> do
        mb :: MBytes p RealWorld <- allocMBytes (0 :: Count e)
        setMBytes mb 0 0 e
        b <- freezeMBytes mb
        toListBytes b `shouldBe` ([] :: [e])
      prop "non-empty" $ \(NEMem off@(Off o) xs b :: NEBytes p e) (e :: e) -> do
        mb <- thawBytes b
        Count n :: Count e <- getCountMBytes mb
        let c = Count (n - o)
        setMBytes mb off c e
        zipWithM_ (\i x -> readOffMBytes mb i `shouldReturn` x) [0 ..] (take o xs)
        forM_ [o .. unCount c - 1] $ \i ->
            readOffMBytes mb (Off i) `shouldReturn` e
    describe "List" $ do
      prop "concatBytes (empty)" $ \ (NonNegative n) ->
        concatBytes (replicate n (emptyBytes :: Bytes p)) === (emptyBytes :: Bytes p)
      prop "concatBytes" $ \ (xs :: [Bytes p]) ->
        (concatBytes xs :: Bytes p) === fromListBytes (foldMap toList xs)
    describe "Allocation" $ do
      prop "resizeMBytes" $ prop_resizeMBytes @p @e
      prop "singletonBytes" $ \(e :: e) ->
        indexOffBytes (singletonBytes e :: Bytes p) 0 === e
    describe "moveMBytesToMBytes" $ do
      prop "copyBytesToMBytes" $
        \(NEMem i1 _ b1 :: NEBytes p e) (NEMem i2 _ b2 :: NEBytes p e) -> do
          let c = min (countBytes b1 - Count (unOff i1)) (countBytes b2 - Count (unOff i2))
          mb2x <- thawBytes b2
          copyBytesToMBytes b1 i1 mb2x i2 c
          by <- withCloneMBytes_ b2 $ \ mb2y -> do
            mb1 <- thawBytes b1
            moveMBytesToMBytes mb1 i1 mb2y i2 c
          bx <- freezeMBytes mb2x
          bx `shouldBe` by
      prop "moveInside" $ \(NEMem i xs b :: NEBytes p e) -> do
        let c = countBytes b - Count (unOff i)
        mb <- thawBytes b
        moveMBytesToMBytes mb i mb 0 c
        b' <- freezeMBytes mb
        take (unCount c) (toListBytes b') `shouldBe` drop (unOff i) xs

prop_resizeMBytes ::
     forall p e. (Unbox e, Eq e, Show e, Typeable p)
  => NEBytes p e
  -> NonNegative Int
  -> Property
prop_resizeMBytes (NEMem _ xs b) (NonNegative n') =
  monadicIO $
  run $ do
    mb <- thawBytes $ cloneBytes b
    mbr <- resizeMBytes mb (Count n' :: Count Int)
    br <- freezeMBytes mbr
    pure $ conjoin $ zipWith (===) xs (toListBytes br :: [e])


bytesBinarySpec ::
     forall (p :: Pinned). (Typeable p)
  => Spec
bytesBinarySpec = do
  let bytesTypeName = showsType (Proxy :: Proxy (Bytes p)) ""
  describe bytesTypeName $ do
    describe "calloc" $ do
      prop "callocMBytes" $ \(b :: Bytes p) -> do
        mb0 <- allocZeroMBytes (countBytes b :: Count Word8)
        mb <- thawBytes b
        zeroMBytes mb
        b0 <- freezeMBytes mb
        freezeMBytes mb0 `shouldReturn` b0
    describe "clone" $
      prop "Bytes" $ \(b :: Bytes p) -> do
        let bc = cloneBytes b
        bc `shouldBe` b
        unless (byteCountBytes b == 0) $ isSameBytes b bc `shouldBe` False
    describe "isSameBytes" $ do
      prop "True" $ \(b :: Bytes p) -> isSameBytes b b .&&. b == b
      it "(empty) True" $
        isSameBytes emptyBytes emptyBytes .&&. emptyBytes == emptyBytes
      prop "(non-empty) False" $ \(b1 :: Bytes p) (b2 :: Bytes p) ->
        not (isEmptyBytes b1 && isEmptyBytes b2) ==> not (isSameBytes b1 b2)
      prop "Pin (non-empty) False" $ \(b1 :: Bytes p) (b2 :: Bytes 'Pin) ->
        not (isEmptyBytes b1 && isEmptyBytes b2) ==> not (isSameBytes b1 b2)
    describe "isSameMBytes" $ do
      prop "True" $ \(b :: Bytes p) -> do
        mb <- thawBytes b
        isSameMBytes mb mb `shouldBe` True
      it "(empty) True" $ do
        mb1 <- thawBytes emptyBytes
        mb2 <- thawBytes emptyBytes
        isSameMBytes mb1 mb2 `shouldBe` True
      prop "(non-empty) False" $ \(b1 :: Bytes p) (b2 :: Bytes p) ->
        monadicIO $
        run $ do
          mb1 <- thawBytes b1
          mb2 <- thawBytes b2
          pure $
            not (isEmptyBytes b1 && isEmptyBytes b2) ==>
            not (isSameMBytes mb1 mb2)
      prop "Pin (non-empty) False" $ \(b1 :: Bytes p) (b2 :: Bytes 'Pin) ->
        not (isEmptyBytes b1 && isEmptyBytes b2) ==> not (isSameBytes b1 b2)
    describe "toList" $ do
      prop "Inc" $ \(b1 :: Bytes p) (b2 :: Bytes p) ->
        (b1 == b2) === (toListBytes b1 == (toListBytes b2 :: [Word8]))
      prop "Inc+Pin" $ \(b1 :: Bytes p) (b2 :: Bytes 'Pin) ->
        (toIncBytes b1 == relaxPinnedBytes b2) ===
        (toListBytes b1 == (toListBytes b2 :: [Word8]))
    describe "ensurePinned" $ do
      prop "Bytes" $ \(b :: Bytes p) ->
        let b' = ensurePinnedBytes b
         in isPinnedBytes b'
      prop "MBytes" $ \(b :: Bytes p) -> do
        mb <- thawBytes b
        mb' <- ensurePinnedMBytes mb
        isPinnedMBytes mb' `shouldBe` True
    describe "IsList" $ do
      prop "toList" $ \(NEMem _ xs b :: NEBytes p Word8) -> xs === toList b
      prop "fromList . toList" $ \(b :: Bytes p) -> b === fromList (toList b)
      prop "fromListN . toList" $ \(b :: Bytes p) ->
        b === fromListN (coerce (byteCountBytes b)) (toList b)
    describe "Show" $
      prop "fromBuilder . toList" $ \(b :: Bytes p) ->
        show b ===
        BSL8.unpack (toLazyByteString $
           "[" <>
           mconcat
             (List.intersperse "," ["0x" <> word8HexFixed w8 | w8 <- toList b]) <>
           "]")
    memBinarySpec @(MBytes p)


spec :: Spec
spec = do
  bytesBinarySpec @'Pin
  bytesBinarySpec @'Inc
  describe "Pinned Bytes" $ do
    describe "Allocation" $ do
      let mostThreshold = 3248 :: Count Word8
          leastThreshold = 3249 :: Count Word8
      -- Experimentally found the threshold to be 3249:
      --     mostThreshold = 3248
      --     leastThreshold = 3249
      -- Documented to be 3277, but seems to be different in practice.
      -- https://gitlab.haskell.org/ghc/ghc/-/blob/feb852e67e166f752c783978f5fecc3c28c966f9/docs/users_guide/ffi-chap.rst#L1008
      let pinnedExpectation alloc isPinned = do
            mb <- alloc
            isPinnedMBytes mb `shouldBe` isPinned
            b <- freezeMBytes mb
            isPinnedBytes b `shouldBe` isPinned
      it "Small (Inc) - isUnpinned" $
        pinnedExpectation (allocMBytes mostThreshold :: IO (MBytes 'Inc RealWorld)) False
      prop "Large (Inc) - isPinned" $ \(NonNegative n) ->
        pinnedExpectation (allocMBytes (n + leastThreshold) :: IO (MBytes 'Inc RealWorld)) True
      prop "Inc - isUninned" $ \(NonNegative n) ->
        (n <= mostThreshold) ==> monadicIO $ run $
        pinnedExpectation (allocMBytes n :: IO (MBytes 'Inc RealWorld)) False
      prop "Pin - isPinned" $ \(NonNegative (n :: Count Word8)) ->
        pinnedExpectation (allocMBytes n :: IO (MBytes 'Pin RealWorld)) True
      prop "Pin (aligned) - isPinned" $ \(NonNegative (n :: Count Word8)) ->
        pinnedExpectation (allocAlignedPinnedMBytes n) True
      prop "callocAlignedMBytes" $ \(b :: Bytes 'Pin) -> do
        mb0 <- allocZeroAlignedPinnedMBytes (byteCountBytes b)
        mb <- thawBytes b
        zeroMBytes mb
        b0 <- freezeMBytes mb
        freezeMBytes mb0 `shouldReturn` b0
    describe "isSamePinnedBytes" $ do
      prop "True" $ \(b :: Bytes 'Pin) -> isSamePinnedBytes b b .&&. b == b
      it "(empty) True" $
        isSamePinnedBytes emptyBytes emptyBytes .&&. emptyBytes == emptyBytes
      prop "Pin (non-empty) False" $ \(b1 :: Bytes 'Pin) (b2 :: Bytes 'Pin) ->
        not (isEmptyBytes b1 && isEmptyBytes b2) ==> not (isSamePinnedBytes b1 b2)
    describe "Ptr Access" $
      prop "Test avoidance of GHC bug #18061" $
        prop_WorkArounBugGHC18061 allocAlignedPinnedMBytes withNoHaltPtrMBytes


prop_WorkArounBugGHC18061 ::
     (Count Word32 -> IO t)
  -> (t -> (Ptr Word32 -> IO a) -> IO ())
  -> Positive Int
  -> IO ()
prop_WorkArounBugGHC18061 alloc withPtr (Positive n) =
  void $
  timeout (1000 * n) $ do
    replicateM_ 49 $ threadDelay 1
    fptr <- alloc (4 :: Count Word32)
    withPtr fptr $ \p ->
      forever $ do
        poke p (0xDEADBEEF :: Word32)
        threadDelay 10
        x <- peek p
        unless (x == 0xDEADBEEF) $
          error ("Heap corruption detected: deadbeef /= " ++ showHex x "")
{-# INLINE prop_WorkArounBugGHC18061 #-}

