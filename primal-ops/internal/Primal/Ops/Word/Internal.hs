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

  -- * Word8#

  -- | Operations on 8-bit unsigned integers.
  Word8#,
  word8ToWord#,
  wordToWord8#,
  word8ToInt8#,
  plusWord8#,
  subWord8#,
  timesWord8#,
  quotWord8#,
  remWord8#,
  quotRemWord8#,
  andWord8#,
  orWord8#,
  xorWord8#,
  notWord8#,
  uncheckedShiftLWord8#,
  uncheckedShiftRLWord8#,
  eqWord8#,
  geWord8#,
  gtWord8#,
  leWord8#,
  ltWord8#,
  neWord8#,

  -- * Word16#

  -- | Operations on 16-bit unsigned integers.
  Word16#,
  word16ToWord#,
  wordToWord16#,
  word16ToInt16#,
  plusWord16#,
  subWord16#,
  timesWord16#,
  quotWord16#,
  remWord16#,
  quotRemWord16#,
  andWord16#,
  orWord16#,
  xorWord16#,
  notWord16#,
  uncheckedShiftLWord16#,
  uncheckedShiftRLWord16#,
  eqWord16#,
  geWord16#,
  gtWord16#,
  leWord16#,
  ltWord16#,
  neWord16#,

  -- * Word32#

  -- | Operations on 32-bit unsigned integers.
  Word32#,
  word32ToWord#,
  wordToWord32#,
  word32ToInt32#,
  plusWord32#,
  subWord32#,
  timesWord32#,
  quotWord32#,
  remWord32#,
  quotRemWord32#,
  andWord32#,
  orWord32#,
  xorWord32#,
  notWord32#,
  uncheckedShiftLWord32#,
  uncheckedShiftRLWord32#,
  eqWord32#,
  geWord32#,
  gtWord32#,
  leWord32#,
  ltWord32#,
  neWord32#,

  -- * Word64#

  -- | Operations on 64-bit unsigned integers.
  Word64#,
  int64ToWord#,
  intToWord64#,
  plusWord64#,
  subWord64#,
  timesWord64#,
  quotWord64#,
  remWord64#,
  quotRemWord64#,
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

-- import qualified GHC.Exts as GHC (Int16#, Int32#, Int64#, Int8#)
#if __GLASGOW_HASKELL__ >= 904 || WORD_SIZE_IN_BITS < 64
import GHC.Exts (
  word64ToWord#,
  wordToWord64#,
  int64ToWord64#,
  word64ToInt64#,
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
  eqWord64#,
  geWord64#,
  gtWord64#,
  leWord64#,
  ltWord64#,
  neWord64#,
  )
#endif

import GHC.Exts (
  Word#,
  int2Word#,
  narrow16Word#,
  narrow32Word#,
  narrow8Word#,
  quotRemWord#,
  quotWord#,
  remWord#,
  uncheckedIShiftL#,
  uncheckedIShiftRA#,
  uncheckedIShiftRL#,
  (*#),
  (+#),
  (-#),
  (/=#),
  (<#),
  (<=#),
  (==#),
  (>#),
  (>=#),
 )

#if __GLASGOW_HASKELL__ >= 902
import qualified GHC.Exts as GHC
#endif

#if __GLASGOW_HASKELL__ >= 904 || WORD_SIZE_IN_BITS < 64
import GHC.Exts (
  int64ToWord#,
  intToWord64#,
  plusWord64#,
  subWord64#,
  timesWord64#,
  quotWord64#,
  remWord64#,
  uncheckedIShiftL64#,
  uncheckedIShiftRA64#,
  uncheckedIShiftRL64#,
  eqWord64#,
  geWord64#,
  gtWord64#,
  leWord64#,
  ltWord64#,
  neWord64#,
  )
#endif

import GHC.Prim (Int64#, Word64#)

type Word32# = Word#
type Word16# = Word#
type Word8# = Word#

timesWord2# :: Word# -> Word# -> (# Word#, Word# #)
timesWord2# = undefined

#if __GLASGOW_HASKELL__ >= 902
toRealWord8# :: Word8# -> GHC.Word8#
toRealWord8# = GHC.wordToWord8#

fromRealWord8# :: GHC.Word8# -> Word8#
fromRealWord8# = GHC.word8ToWord#

toRealWord16# :: Word16# -> GHC.Word16#
toRealWord16# = GHC.wordToWord16#

fromRealWord16# :: GHC.Word16# -> Word16#
fromRealWord16# = GHC.word16ToWord#

toRealWord32# :: Word32# -> GHC.Word32#
toRealWord32# = GHC.wordToWord32#

fromRealWord32# :: GHC.Word32# -> Word32#
fromRealWord32# = GHC.word32ToWord#

#else
toRealWord8# :: Word8# -> Word8#
toRealWord8# x# = x#

fromRealWord8# :: Word8# -> Word8#
fromRealWord8# x# = x#

toRealWord16# :: Word16# -> Word16#
toRealWord16# x# = x#

fromRealWord16# :: Word16# -> Word16#
fromRealWord16# x# = x#

toRealWord32# :: Word32# -> Word32#
toRealWord32# x# = x#

fromRealWord32# :: Word32# -> Word32#
fromRealWord32# x# = x#

#endif

toRealWord64# :: Word64# -> Word64#
toRealWord64# x# = x#

fromRealWord64# :: Word64# -> Word64#
fromRealWord64# x# = x#

--------------------------------------------------------------------------------
-- Word8# ----------------------------------------------------------------------
--------------------------------------------------------------------------------

word8ToWord# :: Word8# -> Word#
word8ToWord# i8# = i8#

wordToWord8# :: Word# -> Word8#
wordToWord8# = narrow8Word#

plusWord8# :: Word8# -> Word8# -> Word8#
plusWord8# x# y# = narrow8Word# (x# `GHC.plusWord#` y#)

subWord8# :: Word8# -> Word8# -> Word8#
subWord8# x# y# = narrow8Word# (x# `GHC.minusWord#` y#)

timesWord8# :: Word8# -> Word8# -> Word8#
timesWord8# x# y# = narrow8Word# (x# `GHC.timesWord#` y#)

quotWord8# :: Word8# -> Word8# -> Word8#
quotWord8# = quotWord#

remWord8# :: Word8# -> Word8# -> Word8#
remWord8# = remWord#

quotRemWord8# :: Word8# -> Word8# -> (# Word8#, Word8# #)
quotRemWord8# = quotRemWord#

uncheckedShiftLWord8# :: Word8# -> Int# -> Word8#
uncheckedShiftLWord8# i8# i# = narrow8Word# (GHC.uncheckedShiftL# i8# i#)

uncheckedShiftRLWord8# :: Word8# -> Int# -> Word8#
uncheckedShiftRLWord8# = GHC.uncheckedShiftRL#

word8ToInt8# :: Word8# -> Int#
word8ToInt8# w8# = GHC.narrow8Int# (GHC.word2Int# w8#)

andWord8# :: Word8# -> Word8# -> Word8#
andWord8# = GHC.and#

orWord8# :: Word8# -> Word8# -> Word8#
orWord8# = GHC.or#

xorWord8# :: Word8# -> Word8# -> Word8#
xorWord8# = GHC.xor#

notWord8# :: Word8# -> Word8#
notWord8# = GHC.not#

eqWord8# :: Word8# -> Word8# -> Int#
eqWord8# = GHC.eqWord#

geWord8# :: Word8# -> Word8# -> Int#
geWord8# = GHC.geWord#

gtWord8# :: Word8# -> Word8# -> Int#
gtWord8# = GHC.gtWord#

leWord8# :: Word8# -> Word8# -> Int#
leWord8# = GHC.leWord#

ltWord8# :: Word8# -> Word8# -> Int#
ltWord8# = GHC.ltWord#

neWord8# :: Word8# -> Word8# -> Int#
neWord8# = GHC.neWord#

--------------------------------------------------------------------------------
-- Word16# ----------------------------------------------------------------------
--------------------------------------------------------------------------------

word16ToWord# :: Word16# -> Word#
word16ToWord# i16# = i16#

wordToWord16# :: Word# -> Word16#
wordToWord16# = narrow16Word#

plusWord16# :: Word16# -> Word16# -> Word16#
plusWord16# x# y# = narrow16Word# (x# `GHC.plusWord#` y#)

subWord16# :: Word16# -> Word16# -> Word16#
subWord16# x# y# = narrow16Word# (x# `GHC.minusWord#` y#)

timesWord16# :: Word16# -> Word16# -> Word16#
timesWord16# x# y# = narrow16Word# (x# `GHC.timesWord#` y#)

quotWord16# :: Word16# -> Word16# -> Word16#
quotWord16# = quotWord#

remWord16# :: Word16# -> Word16# -> Word16#
remWord16# = remWord#

quotRemWord16# :: Word16# -> Word16# -> (# Word16#, Word16# #)
quotRemWord16# = quotRemWord#

uncheckedShiftLWord16# :: Word16# -> Int# -> Word16#
uncheckedShiftLWord16# = GHC.uncheckedShiftL#

uncheckedShiftRLWord16# :: Word16# -> Int# -> Word16#
uncheckedShiftRLWord16# = GHC.uncheckedShiftRL#

word16ToInt16# :: Word16# -> Int#
word16ToInt16# w16# = GHC.narrow16Int# (GHC.word2Int# w16#)

andWord16# :: Word16# -> Word16# -> Word16#
andWord16# = GHC.and#

orWord16# :: Word16# -> Word16# -> Word16#
orWord16# = GHC.or#

xorWord16# :: Word16# -> Word16# -> Word16#
xorWord16# = GHC.xor#

notWord16# :: Word16# -> Word16#
notWord16# = GHC.not#

eqWord16# :: Word16# -> Word16# -> Int#
eqWord16# = GHC.eqWord#

geWord16# :: Word16# -> Word16# -> Int#
geWord16# = GHC.geWord#

gtWord16# :: Word16# -> Word16# -> Int#
gtWord16# = GHC.gtWord#

leWord16# :: Word16# -> Word16# -> Int#
leWord16# = GHC.leWord#

ltWord16# :: Word16# -> Word16# -> Int#
ltWord16# = GHC.ltWord#

neWord16# :: Word16# -> Word16# -> Int#
neWord16# = GHC.neWord#

--------------------------------------------------------------------------------
-- Word32# ----------------------------------------------------------------------
--------------------------------------------------------------------------------

word32ToWord# :: Word32# -> Word#
word32ToWord# i32# = i32#

wordToWord32# :: Word# -> Word32#
wordToWord32# = narrow32Word#

plusWord32# :: Word32# -> Word32# -> Word32#
plusWord32# x# y# = narrow32Word# (x# `GHC.plusWord#` y#)

subWord32# :: Word32# -> Word32# -> Word32#
subWord32# x# y# = narrow32Word# (x# `GHC.minusWord#` y#)

timesWord32# :: Word32# -> Word32# -> Word32#
timesWord32# x# y# = narrow32Word# (x# `GHC.timesWord#` y#)

quotWord32# :: Word32# -> Word32# -> Word32#
quotWord32# = quotWord#

remWord32# :: Word32# -> Word32# -> Word32#
remWord32# = remWord#

quotRemWord32# :: Word32# -> Word32# -> (# Word32#, Word32# #)
quotRemWord32# = quotRemWord#

uncheckedShiftLWord32# :: Word32# -> Int# -> Word32#
uncheckedShiftLWord32# = GHC.uncheckedShiftL#

uncheckedShiftRLWord32# :: Word32# -> Int# -> Word32#
uncheckedShiftRLWord32# = GHC.uncheckedShiftRL#

word32ToInt32# :: Word32# -> Int#
word32ToInt32# w32# = GHC.narrow32Int# (GHC.word2Int# w32#)

andWord32# :: Word32# -> Word32# -> Word32#
andWord32# = GHC.and#

orWord32# :: Word32# -> Word32# -> Word32#
orWord32# = GHC.or#

xorWord32# :: Word32# -> Word32# -> Word32#
xorWord32# = GHC.xor#

notWord32# :: Word32# -> Word32#
notWord32# = GHC.not#

eqWord32# :: Word32# -> Word32# -> Int#
eqWord32# = GHC.eqWord#

geWord32# :: Word32# -> Word32# -> Int#
geWord32# = GHC.geWord#

gtWord32# :: Word32# -> Word32# -> Int#
gtWord32# = GHC.gtWord#

leWord32# :: Word32# -> Word32# -> Int#
leWord32# = GHC.leWord#

ltWord32# :: Word32# -> Word32# -> Int#
ltWord32# = GHC.ltWord#

neWord32# :: Word32# -> Word32# -> Int#
neWord32# = GHC.neWord#

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


subWord64# :: Word64# -> Word64# -> Word64#
subWord64# = minusWord64#

#endif

int64ToWord# :: Int64# -> Word#
int64ToWord# i64# = word64ToWord# (int64ToWord64# i64#)

intToWord64# :: Int# -> Word64#
intToWord64# i# = wordToWord64# (int2Word# i#)

quotRemWord64# :: Word64# -> Word64# -> (# Word64#, Word64# #)
quotRemWord64# x# y# = (# quotWord64# x# y#, remWord64# x# y# #)
