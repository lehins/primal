{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Test.Primal.Memory.BytesExtraSpec (spec) where

import Data.Typeable
import Data.Char
import System.IO
import GHC.Conc
import GHC.IO.Device
import Data.Semigroup
import Test.Primal
import Test.Primal.Memory.BytesSpec (primTypeSpec)
import Primal.Foreign

spec :: Spec
spec = do
  primTypeSpec @CUIntPtr
  primTypeSpec @(String :~: String, CDev)
  primTypeSpec @(CMode, COff, CPid)
  primTypeSpec @(Maybe CNlink)
  primTypeSpec @(Either (CSsize, CUid, CCc, CSpeed, CTcflag, CRLim)
                        (CGid, CBlkSize, CBlkCnt, CClockId, CFsBlkCnt, CFsFilCnt, CId))
  primTypeSpec @(Atom ( CBool
                      , CChar
                      , CSChar
                      , CUChar
                      , CShort
                      , CUShort
                      , CInt
                      , CUInt
                      ))
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
  primTypeSpec @(Atom ( IODeviceType
                      , SeekMode
                      , BlockReason
                      , ThreadStatus
                      , IOMode
                      , BufferMode
                      , Newline
                      , NewlineMode
                      , GeneralCategory
                      ))
