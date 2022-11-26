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

module Test.Primal.Memory.MemSpec
  ( module Test.Primal.Memory.MemSpec
  , module Primal.Memory.Bytes
  ) where

import Control.DeepSeq
import Data.Complex
import Data.Functor.Identity
import Data.Ratio
import Data.Semigroup
import GHC.Fingerprint.Type
import GHC.IO.Device
import Primal.Array
import Primal.Foreign hiding (Any)
import Primal.Foreign.StablePtr
import Primal.Memory.Addr
import Primal.Memory.ByteString
import Primal.Memory.Bytes
import Primal.Memory.FAddr
import Primal.Memory.PUArray
import Primal.Memory.Text
import Test.Primal
import Test.Primal.Memory
import Test.Primal.Memory.BytesSpec

eqTypeSpec
  :: forall e
   . (NFData e, Eq e, Show e, Unbox e, Arbitrary e, Typeable e)
  => Spec
eqTypeSpec = do
  bytesSpec @'Pin @e
  bytesSpec @'Inc @e
  memSpec @(MBytes 'Inc) @e
  memSpec @(MBytes 'Pin) @e
  memSpec @(MAddr e) @e
  memSpec @(UMArray e) @e
  memSpec @(PUMArray 'Inc e) @e
  memSpec @(PUMArray 'Pin e) @e
  memSpec @MArray @e
  memSpec @MByteString @e

ordTypeSpec
  :: forall e
   . (NFData e, Ord e, Show e, Unbox e, Arbitrary e, Typeable e)
  => Spec
ordTypeSpec = do
  eqTypeSpec @e
  -- Containers below are with kind: * -> *
  memOrdSpec @MAddr @e
  memOrdSpec @FMAddr @e
  memOrdSpec @UMArray @e
  memOrdSpec @(PUMArray 'Inc) @e
  memOrdSpec @(PUMArray 'Pin) @e

spec :: Spec
spec = do
  memBinarySpec @(MBytes 'Inc)
  memBinarySpec @(MBytes 'Pin)
  memBinarySpec @(PUMArray 'Inc Word8)
  memBinarySpec @(PUMArray 'Pin Word8)
  memBinarySpec @(MAddr Word8)
  memBinarySpec @(FMAddr Word8)
  memBinarySpec @MArray
  memBinarySpec @MByteString

  ordTypeSpec @Char
  ordTypeSpec @Float
  ordTypeSpec @Double
  ordTypeSpec @Int
  ordTypeSpec @Int8
  ordTypeSpec @Int16
  ordTypeSpec @Int32
  ordTypeSpec @Int64
  ordTypeSpec @Word
  ordTypeSpec @Word8
  ordTypeSpec @Word16
  ordTypeSpec @Word32
  ordTypeSpec @Word64
  ordTypeSpec @(Ptr Char)

  eqTypeSpec @(Atom Word16)
  eqTypeSpec @(Identity Word)
  eqTypeSpec @(Down Word8)
  eqTypeSpec @(Dual Word16)
  eqTypeSpec @(Sum Word32)
  eqTypeSpec @(Product Word64)
  eqTypeSpec @(Ratio Int)
  eqTypeSpec @(Complex Float)
  eqTypeSpec @Ordering
  eqTypeSpec @SeekMode
  eqTypeSpec @((), Ptr (), FunPtr (), StablePtr ())
  eqTypeSpec @(All, Any, Fingerprint, IntPtr, WordPtr)
