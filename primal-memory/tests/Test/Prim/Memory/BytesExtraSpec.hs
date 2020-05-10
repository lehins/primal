{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Test.Prim.Memory.BytesExtraSpec (spec) where

import Data.Typeable
import Test.Prim.Memory.Common
import Test.Prim.Memory.BytesSpec (primTypeSpec)
import Foreign.Prim

spec :: Spec
spec = do

  primTypeSpec @CUIntPtr
  primTypeSpec @(Arg CIntMax CUIntMax)
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
