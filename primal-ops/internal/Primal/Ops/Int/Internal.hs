{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- |
-- Module      : Primal.Ops.Int.Internal
-- Copyright   : (c) Alexey Kuleshevich 2022-2023
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module Primal.Ops.Int.Internal (
  -- * Int#
  timesInt2#,

  -- * Int8#
  Int8#,
  int8ToInt#,
  intToInt8#,
  negateInt8#,
  plusInt8#,
  subInt8#,
  timesInt8#,
  quotInt8#,
  remInt8#,
  quotRemInt8#,
  uncheckedShiftLInt8#,
  uncheckedShiftRAInt8#,
  uncheckedShiftRLInt8#,
  int8ToWord8#,
  eqInt8#,
  geInt8#,
  gtInt8#,
  leInt8#,
  ltInt8#,
  neInt8#,
  -- ** Testing
  toRealInt8#,
  fromRealInt8#,

  -- * Int16#
  Int16#,
  int16ToInt#,
  intToInt16#,
  negateInt16#,
  plusInt16#,
  subInt16#,
  timesInt16#,
  quotInt16#,
  remInt16#,
  quotRemInt16#,
  uncheckedShiftLInt16#,
  uncheckedShiftRAInt16#,
  uncheckedShiftRLInt16#,
  int16ToWord16#,
  eqInt16#,
  geInt16#,
  gtInt16#,
  leInt16#,
  ltInt16#,
  neInt16#,
  -- ** Testing
  toRealInt16#,
  fromRealInt16#,

  -- * Int32#
  Int32#,
  int32ToInt#,
  intToInt32#,
  negateInt32#,
  plusInt32#,
  subInt32#,
  timesInt32#,
  quotInt32#,
  remInt32#,
  quotRemInt32#,
  uncheckedShiftLInt32#,
  uncheckedShiftRAInt32#,
  uncheckedShiftRLInt32#,
  int32ToWord32#,
  eqInt32#,
  geInt32#,
  gtInt32#,
  leInt32#,
  ltInt32#,
  neInt32#,
  -- ** Testing
  toRealInt32#,
  fromRealInt32#,

  -- * Int64#
  Int64#,
  int64ToInt#,
  intToInt64#,
  negateInt64#,
  plusInt64#,
  subInt64#,
  timesInt64#,
  quotInt64#,
  remInt64#,
  quotRemInt64#,
  uncheckedIShiftL64#,
  uncheckedIShiftRA64#,
  uncheckedIShiftRL64#,
  int64ToWord64#,
  eqInt64#,
  geInt64#,
  gtInt64#,
  leInt64#,
  ltInt64#,
  neInt64#,
  -- ** Testing
  toRealInt64#,
  fromRealInt64#,
) where

#include "MachDeps.h"

import Primal.Ops.Word.Internal (Word16#, Word32#, Word64#, Word8#)

import GHC.Exts (
  Int#,
  negateInt#,
  (+#),
  (-#),
  (*#),
  quotInt#,
  remInt#,
  quotRemInt#,
  uncheckedIShiftL#,
  uncheckedIShiftRA#,
  uncheckedIShiftRL#,
  narrow8Int#,
  narrow16Int#,
  narrow32Int#,
  narrow8Word#,
  narrow16Word#,
  narrow32Word#,
  (==#),
  (>=#),
  (>#),
  (<=#),
  (<#),
  (/=#),
  int2Word#,
  )

#if __GLASGOW_HASKELL__ >= 902
import qualified GHC.Exts as GHC
#endif


#if WORD_SIZE_IN_BITS < 64
import GHC.Prim (Int64#)
#else
type Int64# = Int#
#endif

type Int32# = Int#
type Int16# = Int#
type Int8# = Int#


#if __GLASGOW_HASKELL__ >= 902
toRealInt8# :: Int8# -> GHC.Int8#
toRealInt8# = GHC.intToInt8#

fromRealInt8# :: GHC.Int8# -> Int8#
fromRealInt8# = GHC.int8ToInt#

toRealInt16# :: Int16# -> GHC.Int16#
toRealInt16# = GHC.intToInt16#

fromRealInt16# :: GHC.Int16# -> Int16#
fromRealInt16# = GHC.int16ToInt#

toRealInt32# :: Int32# -> GHC.Int32#
toRealInt32# = GHC.intToInt32#

fromRealInt32# :: GHC.Int32# -> Int32#
fromRealInt32# = GHC.int32ToInt#

toRealInt64# :: Int64# -> GHC.Int64#
toRealInt64# = GHC.intToInt64#

fromRealInt64# :: GHC.Int64# -> Int64#
fromRealInt64# = GHC.int64ToInt#

#else
toRealInt8# :: Int8# -> Int8#
toRealInt8# x# = x#

fromRealInt8# :: Int8# -> Int8#
fromRealInt8# x# = x#

toRealInt16# :: Int16# -> Int16#
toRealInt16# x# = x#

fromRealInt16# :: Int16# -> Int16#
fromRealInt16# x# = x#

toRealInt32# :: Int32# -> Int32#
toRealInt32# x# = x#

fromRealInt32# :: Int32# -> Int32#
fromRealInt32# x# = x#

toRealInt64# :: Int64# -> Int64#
toRealInt64# x# = x#

fromRealInt64# :: Int64# -> Int64#
fromRealInt64# x# = x#

#endif


timesInt2# :: Int# -> Int# -> (# Int#, Int#, Int# #)
timesInt2# = undefined

--------------------------------------------------------------------------------
-- Int8# ----------------------------------------------------------------------
--------------------------------------------------------------------------------

int8ToInt# :: Int8# -> Int#
int8ToInt# i8# = i8#

intToInt8# :: Int# -> Int8#
intToInt8# i8# = narrow8Int# i8#

negateInt8# :: Int8# -> Int8#
negateInt8# i8# = narrow8Int# (negateInt# i8#)

plusInt8# :: Int8# -> Int8# -> Int8#
plusInt8# x# y# = narrow8Int# (x# +# y#)

subInt8# :: Int8# -> Int8# -> Int8#
subInt8# x# y# = narrow8Int# (x# -# y#)

timesInt8# :: Int8# -> Int8# -> Int8#
timesInt8# x# y# = narrow8Int# (x# *# y#)

quotInt8# :: Int8# -> Int8# -> Int8#
quotInt8# = quotInt#

remInt8# :: Int8# -> Int8# -> Int8#
remInt8# = remInt#

quotRemInt8# :: Int8# -> Int8# -> (# Int8#, Int8# #)
quotRemInt8# = quotRemInt#

uncheckedShiftLInt8# :: Int8# -> Int# -> Int8#
uncheckedShiftLInt8# i8# i# = narrow8Int# (uncheckedIShiftL# i8# i#)

uncheckedShiftRAInt8# :: Int8# -> Int# -> Int8#
uncheckedShiftRAInt8# i8# i# = narrow8Int# (uncheckedIShiftRA# i8# i#)

uncheckedShiftRLInt8# :: Int8# -> Int# -> Int8#
uncheckedShiftRLInt8# i8# i# = uncheckedIShiftRL# i8# i#

int8ToWord8# :: Int8# -> Word8#
int8ToWord8# i8# = narrow8Word# (int2Word# i8#)

eqInt8# :: Int8# -> Int8# -> Int#
eqInt8# = (==#)

geInt8# :: Int8# -> Int8# -> Int#
geInt8# = (>=#)

gtInt8# :: Int8# -> Int8# -> Int#
gtInt8# = (>#)

leInt8# :: Int8# -> Int8# -> Int#
leInt8# = (<=#)

ltInt8# :: Int8# -> Int8# -> Int#
ltInt8# = (<#)

neInt8# :: Int8# -> Int8# -> Int#
neInt8# = (/=#)


--------------------------------------------------------------------------------
-- Int16# ----------------------------------------------------------------------
--------------------------------------------------------------------------------

int16ToInt# :: Int16# -> Int#
int16ToInt# i16# = i16#

intToInt16# :: Int# -> Int16#
intToInt16# i16# = narrow16Int# i16#

negateInt16# :: Int16# -> Int16#
negateInt16# i16# = narrow16Int# (negateInt# i16#)

plusInt16# :: Int16# -> Int16# -> Int16#
plusInt16# x# y# = narrow16Int# (x# +# y#)

subInt16# :: Int16# -> Int16# -> Int16#
subInt16# x# y# = narrow16Int# (x# -# y#)

timesInt16# :: Int16# -> Int16# -> Int16#
timesInt16# x# y# = narrow16Int# (x# *# y#)

quotInt16# :: Int16# -> Int16# -> Int16#
quotInt16# = quotInt#

remInt16# :: Int16# -> Int16# -> Int16#
remInt16# = remInt#

quotRemInt16# :: Int16# -> Int16# -> (# Int16#, Int16# #)
quotRemInt16# = quotRemInt#

uncheckedShiftLInt16# :: Int16# -> Int# -> Int16#
uncheckedShiftLInt16# = uncheckedIShiftL#

uncheckedShiftRAInt16# :: Int16# -> Int# -> Int16#
uncheckedShiftRAInt16# = uncheckedIShiftRA#

uncheckedShiftRLInt16# :: Int16# -> Int# -> Int16#
uncheckedShiftRLInt16# = uncheckedIShiftRL#

int16ToWord16# :: Int16# -> Word16#
int16ToWord16# i16# = narrow16Word# (int2Word# i16#)

eqInt16# :: Int16# -> Int16# -> Int#
eqInt16# = (==#)

geInt16# :: Int16# -> Int16# -> Int#
geInt16# = (>=#)

gtInt16# :: Int16# -> Int16# -> Int#
gtInt16# = (>#)

leInt16# :: Int16# -> Int16# -> Int#
leInt16# = (<=#)

ltInt16# :: Int16# -> Int16# -> Int#
ltInt16# = (<#)

neInt16# :: Int16# -> Int16# -> Int#
neInt16# = (/=#)

--------------------------------------------------------------------------------
-- Int32# ----------------------------------------------------------------------
--------------------------------------------------------------------------------

int32ToInt# :: Int32# -> Int#
int32ToInt# i32# = i32#

intToInt32# :: Int# -> Int32#
intToInt32# i32# = narrow32Int# i32#

negateInt32# :: Int32# -> Int32#
negateInt32# i32# = narrow32Int# (negateInt# i32#)

plusInt32# :: Int32# -> Int32# -> Int32#
plusInt32# x# y# = narrow32Int# (x# +# y#)

subInt32# :: Int32# -> Int32# -> Int32#
subInt32# x# y# = narrow32Int# (x# -# y#)

timesInt32# :: Int32# -> Int32# -> Int32#
timesInt32# x# y# = narrow32Int# (x# *# y#)

quotInt32# :: Int32# -> Int32# -> Int32#
quotInt32# = quotInt#

remInt32# :: Int32# -> Int32# -> Int32#
remInt32# = remInt#

quotRemInt32# :: Int32# -> Int32# -> (# Int32#, Int32# #)
quotRemInt32# = quotRemInt#

uncheckedShiftLInt32# :: Int32# -> Int# -> Int32#
uncheckedShiftLInt32# = uncheckedIShiftL#

uncheckedShiftRAInt32# :: Int32# -> Int# -> Int32#
uncheckedShiftRAInt32# = uncheckedIShiftRA#

uncheckedShiftRLInt32# :: Int32# -> Int# -> Int32#
uncheckedShiftRLInt32# = uncheckedIShiftRL#

int32ToWord32# :: Int32# -> Word32#
int32ToWord32# i32# = narrow32Word# (int2Word# i32#)

eqInt32# :: Int32# -> Int32# -> Int#
eqInt32# = (==#)

geInt32# :: Int32# -> Int32# -> Int#
geInt32# = (>=#)

gtInt32# :: Int32# -> Int32# -> Int#
gtInt32# = (>#)

leInt32# :: Int32# -> Int32# -> Int#
leInt32# = (<=#)

ltInt32# :: Int32# -> Int32# -> Int#
ltInt32# = (<#)

neInt32# :: Int32# -> Int32# -> Int#
neInt32# = (/=#)

--------------------------------------------------------------------------------
-- Int64# ----------------------------------------------------------------------
--------------------------------------------------------------------------------

#if WORD_SIZE_IN_BITS >= 64

int64ToInt# :: Int64# -> Int#
int64ToInt# i64# = i64#

intToInt64# :: Int# -> Int64#
intToInt64# i64# = i64#

negateInt64# :: Int64# -> Int64#
negateInt64# i64# = negateInt# i64#

plusInt64# :: Int64# -> Int64# -> Int64#
plusInt64# = (+#)

subInt64# :: Int64# -> Int64# -> Int64#
subInt64# = (-#)

timesInt64# :: Int64# -> Int64# -> Int64#
timesInt64# = (*#)

quotInt64# :: Int64# -> Int64# -> Int64#
quotInt64# = quotInt#

remInt64# :: Int64# -> Int64# -> Int64#
remInt64# = remInt#

quotRemInt64# :: Int64# -> Int64# -> (# Int64#, Int64# #)
quotRemInt64# = quotRemInt#

uncheckedIShiftL64# :: Int64# -> Int# -> Int64#
uncheckedIShiftL64# = uncheckedIShiftL#

uncheckedIShiftRA64#  :: Int64# -> Int# -> Int64#
uncheckedIShiftRA64# = uncheckedIShiftRA#

uncheckedIShiftRL64# :: Int64# -> Int# -> Int64#
uncheckedIShiftRL64# = uncheckedIShiftRL#

int64ToWord64# :: Int64# -> Word64#
int64ToWord64# i64# = int2Word# i64#

eqInt64# :: Int64# -> Int64# -> Int#
eqInt64# = (==#)

geInt64# :: Int64# -> Int64# -> Int#
geInt64# = (>=#)

gtInt64# :: Int64# -> Int64# -> Int#
gtInt64# = (>#)

leInt64# :: Int64# -> Int64# -> Int#
leInt64# = (<=#)

ltInt64# :: Int64# -> Int64# -> Int#
ltInt64# = (<#)

neInt64# :: Int64# -> Int64# -> Int#
neInt64# = (/=#)

#else

subInt64# :: Int64# -> Int64# -> Int64#
subInt64# = minusInt64#

quotRemInt64# :: Int64# -> Int64# -> (# Int64#, Int64# #)
quotRemInt64# x# y# = (# quotInt64# x# y#, remInt64# x# y# #)

uncheckedIShiftL64# :: Int64# -> Int# -> Int64#
uncheckedIShiftL64# = uncheckedIShiftL#

uncheckedIShiftRA64#  :: Int64# -> Int# -> Int64#
uncheckedIShiftRA64# = uncheckedIShiftRA#

uncheckedIShiftRL64# :: Int64# -> Int# -> Int64#
uncheckedIShiftRL64# = uncheckedIShiftRL#

int64ToWord64# :: Int64# -> Word64#
int64ToWord64# i64# = narrow64Word# (int2Word# i64#)

eqInt64# :: Int64# -> Int64# -> Int#
eqInt64# = (==#)

geInt64# :: Int64# -> Int64# -> Int#
geInt64# = (>=#)

gtInt64# :: Int64# -> Int64# -> Int#
gtInt64# = (>#)

leInt64# :: Int64# -> Int64# -> Int#
leInt64# = (<=#)

ltInt64# :: Int64# -> Int64# -> Int#
ltInt64# = (<#)

neInt64# :: Int64# -> Int64# -> Int#
neInt64# = (/=#)

#endif
