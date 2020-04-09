{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Prim.BytesSpec (spec, QCGen(..)) where

import Control.Monad.ST
--import Control.DeepSeq
import Control.Monad
import Data.Bits
import Data.Foldable as F
--import Data.GenValidity
import Data.Int
import Data.List (intercalate, partition)
import qualified Data.Primitive.ByteArray as BA
import Data.Typeable
import Data.Word
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import qualified Foreign.Storable as Storable
import Test.Prim.Common
import Data.Prim.Bytes
import Test.QuickCheck.Random

instance Arbitrary BA.ByteArray where
  arbitrary = toByteArray <$> allocBytesGen allocMBytes

instance Arbitrary (Bytes 'Inc) where
  arbitrary = allocBytesGen allocMBytes

instance Arbitrary (Bytes 'Pin) where
  arbitrary = allocBytesGen allocPinnedMBytes




allocBytesGen :: (forall s . Int -> ST s (MBytes p s)) -> Gen (Bytes p)
allocBytesGen alloc = do
  NonNegative n <- arbitrary
  xs :: [Word8] <- vectorOf n arbitrary
  pure $
    runST $ do
      mb <- alloc n
      zipWithM_ (writeMBytes mb) [0 ..] xs
      freezeMBytes mb


toByteArray :: Bytes p -> BA.ByteArray
toByteArray (Bytes ba) = BA.ByteArray ba

spec :: Spec
spec =
  describe "Eq" $
    prop "sameBytes" $ \ (b :: Bytes 'Pin) -> sameBytes b b
