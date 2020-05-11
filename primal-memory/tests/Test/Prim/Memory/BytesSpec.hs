{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Prim.Memory.BytesSpec
  ( module Test.Prim.Memory.BytesSpec
  , module Data.Prim.Memory.Bytes
  ) where

import Data.Complex
import Data.Ratio
import Control.Concurrent
import Control.DeepSeq
import Control.Monad
import Control.Prim.Monad
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.List as List
import Data.Monoid
import Data.Functor.Identity
import Data.Prim.Memory.Bytes
import qualified Data.Primitive.ByteArray as BA
import Foreign.Prim hiding (Any)
import Foreign.Prim.Ptr
import Foreign.Prim.StablePtr
import Foreign.Storable
import GHC.Conc
import GHC.IO.Device
import GHC.Fingerprint.Type
import Numeric
import System.Timeout
import Test.Prim.Memory.Common


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
      createBytesST_ (Count n :: Count a) $ \mb ->
        zipWithM_ (writeOffMBytes mb) [0 ..] xs


toByteArray :: Bytes p -> BA.ByteArray
toByteArray (Bytes ba) = BA.ByteArray ba

primSpec ::
     forall (p :: Pinned) a.
     (NFData a, Eq a, Show a, Prim a, Arbitrary a, Typeable p, Typeable a)
  => Spec
primSpec = do
  let bytesTypeName =
        showsType (Proxy :: Proxy (Bytes p)) .
        (' ' :) . showsType (Proxy :: Proxy a) $
        ""
  describe bytesTypeName $ do
    describe "memset" $ do
      prop "empty" $ \(a :: a) -> do
        mb :: MBytes p RealWorld <- allocMBytes (0 :: Count a)
        setMBytes mb 0 0 a
        b <- freezeMBytes mb
        toListBytes b `shouldBe` ([] :: [a])
      prop "non-empty" $ \(NEBytes off@(Off o) xs b :: NEBytes p a) (a :: a) -> do
        mb <- thawBytes b
        Count n :: Count a <- getCountOfMBytes mb
        let c = Count (n - o)
        setMBytes mb off c a
        zipWithM_ (\i x -> readOffMBytes mb i `shouldReturn` x) [0 ..] (take o xs)
        forM_ [o .. unCount c - 1] $ \i ->
            readOffMBytes mb (Off i) `shouldReturn` a
    describe "List" $ do
      prop "toListBytes" $ \(NEBytes _ xs b :: NEBytes p a) -> xs === (toListBytes b :: [a])
      prop "toListBytes+fromBytes" $ \(NEBytes _ xs b :: NEBytes p a) ->
        let xs' = toListBytes b :: [a]
        in xs' === xs .&&. fromListBytes xs' === b
      prop "loadListMBytes" $ \ (xs :: [a]) (b :: Bytes p) -> do
        mb <- thawBytes b
        (Count n :: Count a, r) <- getCountRemOfMBytes mb
        loadListMBytes xs mb >>= \case
          GT ->
            zipWithM_ (\i x -> readOffMBytes mb (Off i :: Off a) `shouldReturn` x) [0.. n - 1] xs
          elt -> do
            when (elt == EQ) $ r `shouldBe` 0
            zipWithM_ (\i x -> readOffMBytes mb (i :: Off a) `shouldReturn` x) [0..] xs
      prop "fromListBytesN" $ \(NEBytes (Off i) xs b :: NEBytes p a) (Positive n') -> do
        let n = Count $ length xs
            (order, b') = fromListBytesN n xs
            (order', b'' :: Bytes p) = fromListBytesN (Count i) xs
            (order'', b''' :: Bytes p) = fromListBytesN (n + n') xs
        order `shouldBe` EQ
        b' `shouldBe` b
        order' `shouldBe` GT
        xs `shouldStartWith` toListBytes b''
        order'' `shouldBe` LT
        let xs' = toListBytes b'''
        xs' `deepseq` (xs' `shouldStartWith` xs)
      prop "concatBytes (empty)" $ \ (NonNegative n) ->
        concatBytes (replicate n (emptyBytes :: Bytes p)) === (emptyBytes :: Bytes p)
      prop "concatBytes" $ \ (xs :: [Bytes p]) ->
        (concatBytes xs :: Bytes p) === fromListBytes (foldMap toList xs)
    describe "Allocation" $ do
      prop "resizeMBytes" $ prop_resizeMBytes @p @a
      prop "singletonBytes" $ \(a :: a) ->
        indexBytes (singletonBytes a :: Bytes p) 0 === a
    describe "moveMBytesToMBytes" $ do
      prop "copyBytesToMBytes" $
        \(NEBytes i1 _ b1 :: NEBytes p a) (NEBytes i2 _ b2 :: NEBytes p a) -> do
          let c = min (countBytes b1 - Count (unOff i1)) (countBytes b2 - Count (unOff i2))
          mb2x <- thawBytes b2
          copyBytesToMBytes b1 i1 mb2x i2 c
          by <- withCloneMBytes_ b2 $ \ mb2y -> do
            mb1 <- thawBytes b1
            moveMBytesToMBytes mb1 i1 mb2y i2 c
          bx <- freezeMBytes mb2x
          bx `shouldBe` by
      prop "moveInside" $ \(NEBytes i xs b :: NEBytes p a) -> do
        let c = countBytes b - Count (unOff i)
        mb <- thawBytes b
        moveMBytesToMBytes mb i mb 0 c
        b' <- freezeMBytes mb
        take (unCount c) (toListBytes b') `shouldBe` drop (unOff i) xs

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


primTypeSpec ::
     forall a. (NFData a, Eq a, Show a, Prim a, Arbitrary a, Typeable a)
  => Spec
primTypeSpec = do
  primSpec @'Pin @a
  primSpec @'Inc @a

primBinarySpec ::
     forall (p :: Pinned). (Typeable p)
  => Spec
primBinarySpec = do
  let bytesTypeName = showsType (Proxy :: Proxy (Bytes p)) ""
  describe bytesTypeName $ do
    describe "calloc" $ do
      prop "callocMBytes" $ \(b :: Bytes p) -> do
        mb0 <- callocMBytes (countBytes b :: Count Word8)
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
        (relaxPinned b1 == relaxPinned b2) ===
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
      prop "toList" $ \(NEBytes _ xs b :: NEBytes p Word8) -> xs === toList b
      prop "fromList . toList" $ \(b :: Bytes p) -> b === fromList (toList b)
      prop "fromListN . toList" $ \(b :: Bytes p) ->
        b === fromListN (coerce (byteCountBytes b)) (toList b)
    describe "Show" $
      prop "fromList . toList" $ \(b :: Bytes p) ->
        show b ===
        BSL8.unpack (toLazyByteString $
           "[" <>
           mconcat
             (List.intersperse "," ["0x" <> word8HexFixed w8 | w8 <- toList b]) <>
           "]")


spec :: Spec
spec = do
  primBinarySpec @'Pin
  primBinarySpec @'Inc
  primTypeSpec @(Identity Word)
  primTypeSpec @(Down Word8)
  primTypeSpec @(Dual Word16)
  primTypeSpec @(Sum Word32)
  primTypeSpec @(Product Word64)
  primTypeSpec @(Ratio Int)
  primTypeSpec @(Complex Float)
  primTypeSpec @Double
  primTypeSpec @Ordering
  primTypeSpec @SeekMode
  primTypeSpec @(Int8, Int16)
  primTypeSpec @(Int32, Int64, Char)
  primTypeSpec @((), Ptr (), FunPtr (), StablePtr ())
  primTypeSpec @(All, Any, Fingerprint, IntPtr, WordPtr)
  describe "Allocation" $ do
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
        pinnedExpectation (allocAlignedMBytes n) True
      prop "callocAlignedMBytes" $ \(b :: Bytes 'Pin) -> do
        mb0 <- callocAlignedMBytes (byteCountBytes b)
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
        prop_WorkArounBugGHC18061 allocAlignedMBytes withNoHaltPtrMBytes


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




---- Orphans


instance NFData BlockReason where
  rnf br = seq br ()
instance Arbitrary BlockReason where
  arbitrary =
    elements
      [ BlockedOnMVar
      , BlockedOnBlackHole
      , BlockedOnException
      , BlockedOnSTM
      , BlockedOnForeignCall
      , BlockedOnOther
      ]


deriving instance NFData CDev
deriving instance Arbitrary CDev

deriving instance NFData CUid
deriving instance Arbitrary CUid

deriving instance NFData CCc
deriving instance Arbitrary CCc

deriving instance NFData CSpeed
deriving instance Arbitrary CSpeed

deriving instance NFData CMode
deriving instance Arbitrary CMode

deriving instance NFData CTcflag
deriving instance Arbitrary CTcflag

deriving instance NFData COff
deriving instance Arbitrary COff

deriving instance NFData CRLim
deriving instance Arbitrary CRLim

deriving instance NFData CPid
deriving instance Arbitrary CPid

deriving instance NFData CBlkSize
deriving instance Arbitrary CBlkSize

deriving instance NFData CSsize
deriving instance Arbitrary CSsize

deriving instance NFData CGid
deriving instance Arbitrary CGid

deriving instance NFData CNlink
deriving instance Arbitrary CNlink

deriving instance NFData CBlkCnt
deriving instance Arbitrary CBlkCnt

deriving instance NFData CClockId
deriving instance Arbitrary CClockId

deriving instance NFData CFsBlkCnt
deriving instance Arbitrary CFsBlkCnt

deriving instance NFData CFsFilCnt
deriving instance Arbitrary CFsFilCnt

deriving instance NFData CId
deriving instance Arbitrary CId

instance Arbitrary Fingerprint where
  arbitrary = Fingerprint <$> arbitrary <*> arbitrary

instance NFData SeekMode where
  rnf sm = seq sm ()
instance Arbitrary SeekMode where
  arbitrary = elements [toEnum 0 .. ]

instance Arbitrary (Ptr a) where
  arbitrary = intPtrToPtr <$> arbitrary

instance Arbitrary (FunPtr a) where
  arbitrary = castPtrToFunPtr <$> arbitrary

instance Arbitrary (StablePtr a) where
  arbitrary = castPtrToStablePtr <$> arbitrary

instance Arbitrary IntPtr where
  arbitrary = IntPtr <$> arbitrary

instance Arbitrary WordPtr where
  arbitrary = WordPtr <$> arbitrary

deriving instance NFData IntPtr

deriving instance NFData WordPtr

deriving instance Arbitrary CBool

deriving instance Arbitrary a => Arbitrary (Down a)

instance a ~ b => Arbitrary (a :~: b) where
  arbitrary = pure Refl

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
      createBytesST_ (Count n :: Count Word8) $ \mb ->
        zipWithM_ (writeOffMBytes mb) [0 ..] xs
