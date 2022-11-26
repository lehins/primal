{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Primal.Memory.Common (
  module X,
) where

import Data.Proxy as X
import Data.Typeable as X
import Primal.Memory as X
import Primal.Memory.Addr as X
import Primal.Memory.ByteString as X
import Primal.Memory.PUArray as X
import Primal.Monad as X
import Test.Hspec as X
import Test.Hspec.QuickCheck as X
import Test.QuickCheck as X hiding ((.&.))
import Test.QuickCheck.Function as X
import Test.QuickCheck.Monadic as X

instance Arbitrary (Count e) where
  arbitrary = Count . getNonNegative <$> arbitrary

instance Arbitrary (Off e) where
  arbitrary = Off . getNonNegative <$> arbitrary

instance Typeable p => Arbitrary (Bytes p) where
  arbitrary = fromListMem <$> (arbitrary :: Gen [Word8])

instance (Arbitrary e, Unbox e) => Arbitrary (Addr e) where
  arbitrary = fromListMem <$> (arbitrary :: Gen [e])

instance (Typeable p, Arbitrary e, Unbox e) => Arbitrary (PUArray p e) where
  arbitrary = fromListMem <$> (arbitrary :: Gen [e])

instance Arbitrary ByteString where
  arbitrary = fromListMem <$> (arbitrary :: Gen [Word8])

instance Arbitrary ShortByteString where
  arbitrary = fromListMem <$> (arbitrary :: Gen [Word8])
