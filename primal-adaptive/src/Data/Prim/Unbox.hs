{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.Prim.Unbox
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Unbox
  ( Unbox(..)
  ) where

import Control.DeepSeq
import Data.Bits
import Primal.Prim.Atomic
import Primal.Prim.Class

newtype Unbox e =
  Unbox
    { unUnbox :: e
    }
  deriving ( Show
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


instance Prim e => Prim (Unbox e) where
  type PrimBase (Unbox e) = Unbox e
  type SizeOf (Unbox e) = SizeOf e
  type Alignment (Unbox e) = Alignment e

instance (Eq e, Prim e) => Atomic (Unbox e)

instance (Num e, Eq e, Prim e) => AtomicCount (Unbox e)

instance (Bits e, Eq e, Prim e) => AtomicBits (Unbox e)
