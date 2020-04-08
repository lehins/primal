{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
-- |
-- Module      : Data.Prim.Foreign.Cmm
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Foreign.Cmm
  ( word32ToFloat#
  , floatToWord32#
  , word64ToDouble#
  , doubleToWord64#
  ) where


import GHC.Exts

#include "MachDeps.h"

foreign import prim "prim_core_stg_word32ToFloatzh"
  word32ToFloat# :: Word# -> Float#

foreign import prim "prim_core_stg_floatToWord32zh"
  floatToWord32# :: Float# -> Word#

foreign import prim "prim_core_stg_word64ToDoublezh"
#if WORD_SIZE_IN_BITS == 64
  word64ToDouble# :: Word# -> Double#
#else
  word64ToDouble# :: Word64# -> Double#
#endif

foreign import prim "prim_core_stg_doubleToWord64zh"
#if WORD_SIZE_IN_BITS == 64
  doubleToWord64# :: Double# -> Word#
#else
  doubleToWord64# :: Double# -> Word64#
#endif
