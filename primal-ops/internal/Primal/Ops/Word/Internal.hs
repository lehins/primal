{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

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
  int64ToWord#,
  intToWord64#,
  negateWord64#,
  plusWord64#,
  subWord64#,
  timesWord64#,
  quotWord64#,
  remWord64#,
  and64#,
  or64#,
  xor64#,
  not64#,
  uncheckedShiftL64#,
  uncheckedShiftRL64#,
  int64ToWord64#,
  word64ToInt64#,
  eqWord64#,
  geWord64#,
  gtWord64#,
  leWord64#,
  ltWord64#,
  neWord64#,
) where

#include "MachDeps.h"

import GHC.Exts (Int#, Word#, int2Word#)
import qualified GHC.Exts as GHC (Int16#, Int32#, Int64#, Int8#)
#if __GLASGOW_HASKELL__ >= 904 || WORD_SIZE_IN_BITS < 64
import GHC.Exts (
  word64ToWord#,
  wordToWord64#,
  negateWord64#,
  plusWord64#,
  subWord64#,
  timesWord64#,
  quotWord64#,
  remWord64#,
  quotRemWord64#,
  uncheckedShiftL64#,
  uncheckedShiftRA64#,
  uncheckedShiftRL64#,
  eqWord64#,
  geWord64#,
  gtWord64#,
  leWord64#,
  ltWord64#,
  neWord64#,
  )
#endif

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

import GHC.Prim (Int64#, Word64#)

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
-- eqWord64# = undefined
-- geWord64# = undefined
-- gtWord64# = undefined
-- leWord64# = undefined
-- ltWord64# = undefined
-- neWord64# = undefined

#if __GLASGOW_HASKELL__ < 904 && WORD_SIZE_IN_BITS >= 64

foreign import ccall unsafe "primal_eqWord64"    eqWord64#     :: Word64# -> Word64# -> Int#
foreign import ccall unsafe "primal_neWord64"    neWord64#     :: Word64# -> Word64# -> Int#
foreign import ccall unsafe "primal_ltWord64"    ltWord64#     :: Word64# -> Word64# -> Int#
foreign import ccall unsafe "primal_leWord64"    leWord64#     :: Word64# -> Word64# -> Int#
foreign import ccall unsafe "primal_gtWord64"    gtWord64#     :: Word64# -> Word64# -> Int#
foreign import ccall unsafe "primal_geWord64"    geWord64#     :: Word64# -> Word64# -> Int#

foreign import ccall unsafe "primal_quotWord64"  quotWord64#   :: Word64# -> Word64# -> Word64#
foreign import ccall unsafe "primal_remWord64"   remWord64#    :: Word64# -> Word64# -> Word64#

-- Relies on an implicit cast over FFI: Int64<->Word64
foreign import ccall unsafe "primal_plusInt64"   plusWord64#    :: Word64# -> Word64# -> Word64#
foreign import ccall unsafe "primal_minusInt64"  minusWord64#   :: Word64# -> Word64# -> Word64#
foreign import ccall unsafe "primal_timesInt64"  timesWord64#   :: Word64# -> Word64# -> Word64#
foreign import ccall unsafe "primal_negateInt64" negateWord64#  :: Word64# -> Word64#

foreign import ccall unsafe "primal_and64"       and64#        :: Word64# -> Word64# -> Word64#
foreign import ccall unsafe "primal_or64"        or64#         :: Word64# -> Word64# -> Word64#
foreign import ccall unsafe "primal_xor64"       xor64#        :: Word64# -> Word64# -> Word64#
foreign import ccall unsafe "primal_not64"       not64#        :: Word64# -> Word64#

foreign import ccall unsafe "primal_uncheckedShiftL64"
  uncheckedShiftL64#  :: Word64# -> Int# -> Word64#
foreign import ccall unsafe "primal_uncheckedShiftRL64"
  uncheckedShiftRL64# :: Word64# -> Int# -> Word64#

foreign import ccall unsafe "primal_int64ToWord64"   int64ToWord64#   :: Int64# -> Word64#
foreign import ccall unsafe "primal_word64ToInt64"   word64ToInt64#   :: Word64# -> Int64#

foreign import ccall unsafe "primal_wordToWord64"    wordToWord64#    :: Word# -> Word64#
foreign import ccall unsafe "primal_word64ToWord"    word64ToWord#    :: Word64# -> Word#

#endif


subWord64# :: Word64# -> Word64# -> Word64#
subWord64# = minusWord64#

int64ToWord# :: Int64# -> Word#
int64ToWord# i64# = word64ToWord# (int64ToWord64# i64#)

intToWord64# :: Int# -> Word64#
intToWord64# i# = wordToWord64# (int2Word# i#)

