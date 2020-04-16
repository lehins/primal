{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Prim.BytesSpec
  ( module Test.Prim.BytesSpec
  , module Data.Prim.Bytes
  ) where

import Control.Monad.ST
--import Control.DeepSeq
import Control.Monad
import Control.Monad.Prim
-- import Data.Bits
-- import Data.Foldable as F
--import Data.GenValidity
-- import Data.Int
-- import Data.List (intercalate, partition)
import Data.Prim.Bytes
import qualified Data.Primitive.ByteArray as BA
import Data.Typeable
import Test.Prim.Common
--import Test.QuickCheck.Random
import Control.Concurrent
--import Control.Monad
--import Data.Word
import Foreign.Storable
import Numeric
import System.Timeout

instance Arbitrary (Off a) where
  arbitrary = Off . getNonNegative <$> arbitrary

instance Arbitrary (Count a) where
  arbitrary = Count . getNonNegative <$> arbitrary

instance Arbitrary BA.ByteArray where
  arbitrary = toByteArray <$> (arbitrary :: Gen (Bytes 'Pin))

instance Typeable p => Arbitrary (Bytes p) where
  arbitrary = do
    NonNegative n <- arbitrary
    xs :: [Word8] <- vectorOf n arbitrary
    pure $
      runST $ do
        mb <- allocMBytes (Count n :: Count Word8)
        zipWithM_ (writeMBytes mb) [0 ..] xs
        freezeMBytes mb

data NEBytes p a = NEBytes (Off a) [a] (Bytes p)
  deriving (Eq)

instance (Show a, Prim a) => Show (NEBytes p a) where
  show (NEBytes o xs bs) =
    "(NEBytes " ++ show o ++ " " ++ show xs ++ " " ++ show (toListBytes bs :: [a]) ++ ")"

instance (Prim a, Arbitrary a, Typeable p) => Arbitrary (NEBytes p a) where
  arbitrary = do
    Positive n <- arbitrary
    NonNegative k <- arbitrary
    let i = k `mod` n
    xs :: [a] <- vectorOf n arbitrary
    pure $
      NEBytes (Off i) xs $
      runST $ do
        mb <- allocMBytes (Count n :: Count a)
        zipWithM_ (writeMBytes mb) [0 ..] xs
        freezeMBytes mb


toByteArray :: Bytes p -> BA.ByteArray
toByteArray (Bytes ba) = BA.ByteArray ba

primSpec :: forall (p :: Pinned) a. (Eq a, Show a, Prim a, Arbitrary a, Typeable p, Typeable a) => Spec
primSpec = do
  let bytesTypeName =
        showsType (Proxy :: Proxy (Bytes p)) .
        (' ' :) . showsType (Proxy :: Proxy a) $
        ""
  describe bytesTypeName $ do
    describe "memset" $ do
      prop "empty" $ \ (a :: a) -> do
        mb :: MBytes p RealWorld <- allocMBytes (0 :: Count a)
        setMBytes mb 0 0 a
        b <- freezeMBytes mb
        toListBytes b `shouldBe` ([] :: [a])
      prop "non-empty" $ \(NEBytes off@(Off o) xs b :: NEBytes p a) (a :: a) -> do
        mb <- thawBytes b
        Count n :: Count a <- getCountOfMBytes mb
        let c = Count (n - o)
        setMBytes mb off c a
        zipWithM_ (\i x -> readMBytes mb i `shouldReturn` x) [0 ..] (take o xs)
        forM_ [o .. unCount c - 1] $ \i ->
          readMBytes mb (Off i) `shouldReturn` a

primTypeSpec ::
     forall a. (Eq a, Show a, Prim a, Arbitrary a, Typeable a)
  => Spec
primTypeSpec = do
  primSpec @'Pin @a
  primSpec @'Inc @a

spec :: Spec
spec = do
  primTypeSpec @Bool
  primTypeSpec @Char
  primTypeSpec @Word
  primTypeSpec @Word8
  primTypeSpec @Word16
  primTypeSpec @Word32
  primTypeSpec @Word64
  primTypeSpec @Int
  primTypeSpec @Int8
  primTypeSpec @Int16
  primTypeSpec @Int32
  primTypeSpec @Int64
  describe "Eq" $ do
    describe "isSameBytes" $ do
      describe "Inc" $ do
        prop "True" $ \(b :: Bytes 'Inc) -> isSameBytes b b .&&. b == b
        it "(empty) True" $
          isSameBytes emptyBytes emptyBytes .&&. emptyBytes == emptyBytes
        prop "(non-empty) False" $ \(b1 :: Bytes 'Inc) (b2 :: Bytes 'Inc) ->
          not (isEmptyBytes b1 && isEmptyBytes b2) ==> not (isSameBytes b1 b2)
      describe "Pin" $ do
        prop "True" $ \(b :: Bytes 'Pin) -> isSameBytes b b .&&. b == b
        it "(empty) True" $
          isSameBytes emptyBytes emptyBytes .&&. emptyBytes == emptyBytes
        prop "(non-empty) False" $ \(b1 :: Bytes 'Pin) (b2 :: Bytes 'Pin) ->
          not (isEmptyBytes b1 && isEmptyBytes b2) ==> not (isSameBytes b1 b2)
      prop "Inc+Pin (non-empty) False" $ \(b1 :: Bytes 'Inc) (b2 :: Bytes 'Pin) ->
        not (isEmptyBytes b1 && isEmptyBytes b2) ==> not (isSameBytes b1 b2)
    describe "toList" $ do
      prop "Inc" $ \(b1 :: Bytes 'Inc) (b2 :: Bytes 'Inc) ->
        (b1 == b2) === (toListBytes b1 == (toListBytes b2 :: [Word8]))
      prop "Pin" $ \(b1 :: Bytes 'Pin) (b2 :: Bytes 'Pin) ->
        (b1 == b2) === (toListBytes b1 == (toListBytes b2 :: [Word8]))
      prop "Inc+Pin" $ \(b1 :: Bytes 'Inc) (b2 :: Bytes 'Pin) ->
        (b1 == relaxPinned b2) ===
        (toListBytes b1 == (toListBytes (relaxPinned b2) :: [Word8]))
  describe "List" $ do
    describe "toListBytes" $ do
      prop "Inc" $ prop_toListBytes @'Inc @Int
      prop "Pin" $ prop_toListBytes @'Pin @Int
  describe "Allocation" $ do
    describe "resizeMBytes" $ do
      prop "Inc" $ prop_resizeMBytes @'Inc @Int
      prop "Pin" $ prop_resizeMBytes @'Pin @Int
    describe "Pinned Memory" $ do
      let mostThreshold = 3248 :: Count Word8
          leastThreshold = 3277 :: Count Word8
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
        pinnedExpectation (allocPinnedAlignedMBytes n) True
    describe "Ptr Access" $
      prop "Test avoidance of GHC bug #18061" $
        prop_WorkArounBugGHC18061 allocPinnedMBytes withPtrMBytes


prop_toListBytes :: forall p a . (Prim a, Eq a, Show a) => NEBytes p a -> Property
prop_toListBytes (NEBytes _ xs b) = xs === (toListBytes b :: [a])

prop_resizeMBytes ::
     forall p a. (Prim a, Eq a, Show a, Typeable p)
  => NEBytes p a
  -> NonNegative Int
  -> Property
prop_resizeMBytes (NEBytes _ xs b) (NonNegative n') =
  monadicIO $
  run $ do
    mb <- thawBytes $ cloneBytes b
    mbr <- resizeMBytes mb (Count n' :: Count Int)
    br <- freezeMBytes mbr
    pure $ conjoin $ zipWith (===) xs (toListBytes br :: [a])

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