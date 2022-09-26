{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Test.Primal.Memory.MemExtraSpec (spec) where

import Data.Typeable
import Data.Char
import System.IO
import GHC.Conc
import GHC.IO.Device
import Data.Semigroup
import Test.Primal
import Test.Primal.Memory.MemSpec (eqTypeSpec)
import Primal.Foreign

spec :: Spec
spec = do
  eqTypeSpec @CUIntPtr
  eqTypeSpec @(String :~: String, CDev)
  eqTypeSpec @(CMode, COff, CPid)
  eqTypeSpec @(Maybe CNlink)
  eqTypeSpec @(Either (CSsize, CUid, CCc, CSpeed, CTcflag, CRLim)
                      (CGid, CBlkSize, CBlkCnt, CClockId, CFsBlkCnt, CFsFilCnt, CId))
  eqTypeSpec @(Atom ( CBool
                    , CChar
                    , CSChar
                    , CUChar
                    , CShort
                    , CUShort
                    , CInt
                    , CUInt
                    ))
  eqTypeSpec @( CPtrdiff
              , CSize
              , CWchar
              , CSigAtomic
              , CLLong
              , CULLong
              , CIntPtr
              , CLong
              , CULong
              )
  eqTypeSpec @(Arg CIntMax CUIntMax)
  eqTypeSpec @(Atom ( IODeviceType
                    , SeekMode
                    , BlockReason
                    , ThreadStatus
                    , IOMode
                    , BufferMode
                    , Newline
                    , NewlineMode
                    , GeneralCategory
                    ))
