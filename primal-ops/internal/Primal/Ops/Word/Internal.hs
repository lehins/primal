{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- |
-- Module      : Primal.Ops.Word.Internal
-- Copyright   : (c) Alexey Kuleshevich 2022-2023
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module Primal.Ops.Word.Internal (
  timesWord2#,
  Word8#,
  -- int8ToWord#,
  -- intToWord8#,
  -- uncheckedShiftLWord8#,
  -- uncheckedShiftRAWord8#,
  -- uncheckedShiftRLWord8#,
  -- int8ToWord8#,
  Word16#,
  -- int16ToWord#,
  -- intToWord16#,
  -- uncheckedShiftLWord16#,
  -- uncheckedShiftRAWord16#,
  -- uncheckedShiftRLWord16#,
  -- int16ToWord16#,

  -- * Word32#

  -- | Operations on 32-bit signed integers.
  Word32#,
  -- int32ToWord#,
  -- intToWord32#,
  -- negateWord32#,
  -- plusWord32#,
  -- subWord32#,
  -- timesWord32#,
  -- quotWord32#,
  -- remWord32#,
  -- quotRemWord32#,
  -- uncheckedShiftLWord32#,
  -- uncheckedShiftRAWord32#,
  -- uncheckedShiftRLWord32#,
  -- int32ToWord32#,
  -- eqWord32#,
  -- geWord32#,
  -- gtWord32#,
  -- leWord32#,
  -- ltWord32#,
  -- neWord32#,

  -- * Word64#

  -- | Operations on 64-bit signed integers.
  Word64#,
  -- int64ToWord#,
  -- intToWord64#,
  -- negateWord64#,
  -- plusWord64#,
  -- subWord64#,
  -- timesWord64#,
  -- quotWord64#,
  -- remWord64#,
  -- uncheckedIShiftRL64#,
  -- int64ToWord64#,
  -- eqWord64#,
  -- geWord64#,
  -- gtWord64#,
  -- leWord64#,
  -- ltWord64#,
  -- neWord64#,
) where

#include "MachDeps.h"

import GHC.Exts (Word#)
import qualified GHC.Exts as GHC (Int64#, Int32#, Int16#, Int8#)

-- import GHC.Exts (
--   (+#),
--   (-#),
--   (*#),
--   quotWord#,
--   remWord#,
--   quotRemWord#,
--   uncheckedIShiftL#,
--   uncheckedIShiftRA#,
--   uncheckedIShiftRL#,
--   narrow8Word#,
--   narrow16Word#,
--   narrow32Word#,
--   (==#),
--   (>=#),
--   (>#),
--   (<=#),
--   (<#),
--   (/=#),
--   )

#if WORD_SIZE_IN_BITS < 64
import GHC.Prim (Word64#)
#else
type Word64# = Word#
#endif

type Word32# = Word#
type Word16# = Word#
type Word8# = Word#

timesWord2# :: Word# -> Word# -> (# Word#, Word# #)
timesWord2# = undefined

-- int8ToWord# :: Word8# -> Word#
-- int8ToWord# = undefined

-- intToWord8# :: Word# -> Word8#
-- intToWord8# = undefined

-- uncheckedShiftLWord8# = undefined
-- uncheckedShiftRAWord8# = undefined
-- uncheckedShiftRLWord8# = undefined
-- int8ToWord8# = undefined
-- int16ToWord# = undefined
-- intToWord16# = undefined
-- uncheckedShiftLWord16# = uncheckedIShiftL#
-- uncheckedShiftRAWord16# = uncheckedIShiftRA#
-- uncheckedShiftRLWord16# = uncheckedIShiftRL#
-- int16ToWord16# = undefined

-- int32ToWord# :: Word32# -> Word#
-- int32ToWord# i32# = i32#

-- intToWord32# :: Word# -> Word32#
-- intToWord32# i32# = i32#

-- negateWord32# :: Word32# -> Word32#
-- negateWord32# = negateWord#

-- plusWord32# :: Word32# -> Word32# -> Word32#
-- plusWord32# x# y# = narrow32Word# (x# +# y#)

-- subWord32# :: Word32# -> Word32# -> Word32#
-- subWord32# = (-#)

-- timesWord32# :: Word32# -> Word32# -> Word32#
-- timesWord32# = (*#)

-- quotWord32# :: Word32# -> Word32# -> Word32#
-- quotWord32# = quotWord#

-- remWord32# :: Word32# -> Word32# -> Word32#
-- remWord32# = remWord#

-- quotRemWord32# :: Word32# -> Word32# -> (# Word32#, Word32# #)
-- quotRemWord32# = quotRemWord#

-- uncheckedShiftLWord32# :: Word32# -> Word# -> Word32#
-- uncheckedShiftLWord32# = uncheckedIShiftL#

-- uncheckedShiftRAWord32# :: Word32# -> Word# -> Word32#
-- uncheckedShiftRAWord32# = uncheckedIShiftRA#

-- uncheckedShiftRLWord32# :: Word32# -> Word# -> Word32#
-- uncheckedShiftRLWord32# = uncheckedIShiftRL#

-- eqWord32# = (==#)
-- geWord32# = (>=#)
-- gtWord32# = (>#)
-- leWord32# = (<=#)
-- ltWord32# = (<#)
-- neWord32# = (/=#)

-- int64ToWord# = undefined
-- intToWord64# = undefined
-- negateWord64# = undefined
-- plusWord64# = undefined
-- subWord64# = undefined
-- timesWord64# = undefined
-- quotWord64# = undefined
-- remWord64# = undefined
-- uncheckedIShiftRL64# = undefined
-- int64ToWord64# = undefined
-- eqWord64# = undefined
-- geWord64# = undefined
-- gtWord64# = undefined
-- leWord64# = undefined
-- ltWord64# = undefined
-- neWord64# = undefined
