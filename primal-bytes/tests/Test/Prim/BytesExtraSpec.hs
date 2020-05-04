{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Prim.BytesExtraSpec (spec) where

import Data.Prim.Class
import Data.Typeable
import Test.Hspec
import Test.QuickCheck
import Test.Prim.BytesSpec (primTypeSpec)
import Foreign.Prim
import Control.DeepSeq

primBaseTypeSpec ::
     forall a. (NFData a, Eq a, Show a, Prim a, Arbitrary a, Typeable a)
  => Spec
primBaseTypeSpec = do
  primTypeSpec @a
  primTypeSpec @(a, a)
  primTypeSpec @(a, a, a)


spec :: Spec
spec = do
  primBaseTypeSpec @CBool
  primBaseTypeSpec @CChar
  primBaseTypeSpec @CSChar
  primBaseTypeSpec @CUChar
  primBaseTypeSpec @CShort
  primBaseTypeSpec @CUShort
  primBaseTypeSpec @CInt
  primBaseTypeSpec @CUInt
  primBaseTypeSpec @CLong
  primBaseTypeSpec @CULong
  primBaseTypeSpec @CPtrdiff
  primBaseTypeSpec @CSize
  primBaseTypeSpec @CWchar
  primBaseTypeSpec @CSigAtomic
  primBaseTypeSpec @CLLong
  primBaseTypeSpec @CULLong
  primBaseTypeSpec @CIntPtr
  primBaseTypeSpec @CUIntPtr
  primBaseTypeSpec @CIntMax
  primBaseTypeSpec @CUIntMax
  primTypeSpec @(Int32, Int16)
  primTypeSpec @((Int32, Int8), (Word64, Int16))
  primTypeSpec @(Maybe (Bool, Char, Int))
