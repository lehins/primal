{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Primal.Adaptive.Unpack
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module Primal.Adaptive.Unpack (
  Unpack (..),
) where

import Control.DeepSeq
import Data.Bits
import Primal.Unbox.Atomic
import Primal.Unbox.Class

newtype Unpack e = Unpack
  { unUnpack :: e
  }
  deriving
    ( Show
    , Eq
    , Ord
    , Num
    , Enum
    , Integral
    , Real
    , RealFrac
    , Fractional
    , Floating
    , RealFloat
    , Bits
    , NFData
    )

instance Unbox e => Unbox (Unpack e) where
  type PrimalState (Unpack e) = Unpack e
  type SizeOf (Unpack e) = SizeOf e
  type Alignment (Unpack e) = Alignment e

instance (Eq e, Unbox e) => Atomic (Unpack e)

instance (Num e, Eq e, Unbox e) => AtomicCount (Unpack e)

instance (Bits e, Eq e, Unbox e) => AtomicBits (Unpack e)
