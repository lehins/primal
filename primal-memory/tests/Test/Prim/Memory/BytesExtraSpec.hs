{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Test.Prim.Memory.BytesExtraSpec (spec) where

import Data.Typeable
import Data.Semigroup
import Test.Prim hiding (Arg)
import Test.Prim.Memory.BytesSpec (primTypeSpec)
import Foreign.Prim

spec :: Spec
spec = do
  primTypeSpec @CUIntPtr
  primTypeSpec @(String :~: String, CDev)
  primTypeSpec @(CMode, COff, CPid)
  primTypeSpec @(Maybe CNlink)
  primTypeSpec @(Either (CSsize, CUid, CCc, CSpeed, CTcflag, CRLim)
                        (CGid, CBlkSize, CBlkCnt, CClockId, CFsBlkCnt, CFsFilCnt, CId))
  primTypeSpec @( CBool
                , CChar
                , CSChar
                , CUChar
                , CShort
                , CUShort
                , CInt
                , CUInt
                )
  primTypeSpec @( CPtrdiff
                , CSize
                , CWchar
                , CSigAtomic
                , CLLong
                , CULLong
                , CIntPtr
                , CLong
                , CULong
                )


  primTypeSpec @(Arg CIntMax CUIntMax)


instance (Arbitrary a, Arbitrary b) => Arbitrary (Arg a b) where
  arbitrary = Arg <$> arbitrary <*> arbitrary
