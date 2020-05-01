{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
-- |
-- Module      : Foreign.Prim.Cmm
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Foreign.Prim.Cmm
  ( word32ToFloat#
  , floatToWord32#
  , word64ToDouble#
  , doubleToWord64#
  ) where


import GHC.Exts

#include "MachDeps.h"

-- | Cast a 32bit Word into a Float
foreign import prim "prim_core_stg_word32ToFloatzh"
  word32ToFloat# :: Word# -> Float#

-- | Cast a Float into a 32bit Word
foreign import prim "prim_core_stg_floatToWord32zh"
  floatToWord32# :: Float# -> Word#

-- | Cast a 64bit Word into a Double
foreign import prim "prim_core_stg_word64ToDoublezh"
#if WORD_SIZE_IN_BITS == 64
  word64ToDouble# :: Word# -> Double#
#else
  word64ToDouble# :: Word64# -> Double#
#endif

-- | Cast a Double into a 64bit Word
foreign import prim "prim_core_stg_doubleToWord64zh"
#if WORD_SIZE_IN_BITS == 64
  doubleToWord64# :: Double# -> Word#
#else
  doubleToWord64# :: Double# -> Word64#
#endif
