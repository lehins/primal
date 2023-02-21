{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_HADDOCK print-explicit-runtime-reps #-}

-- |
-- Module      : Primal.Ops.Int
-- Copyright   : (c) Alexey Kuleshevich 2022-2023
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
-- Fixed width signed integers
module Primal.Ops.Int (
  -- * Int

  -- | Native-size boxed signed integer (32+ bits).
  Int (..),

  -- ** Int#

  -- | Native-size primitive signed integer (32+ bits).
  Int#,

  -- ** int2Word#

  -- | Convert a primitive signed integer to a primitive unsigned integer:
  --
  -- >>> :set -XMagicHash
  -- >>> import Primal.Ops.Word (Word (W#))
  -- >>> W# (int2Word# 5#)
  -- 5
  --
  -- Subject to underflow:
  --
  -- >>> W# (int2Word# -5#)
  -- 18446744073709551611
  int2Word#,
  -- word2Int#,

  -- ** int2Float#

  -- | Convert a primitive signed integer to a primitive 32bit floating point value:
  --
  -- >>> :set -XMagicHash
  -- >>> import GHC.Float (Float (F#))
  -- >>> F# (int2Float# 5#)
  -- 5.0
  --
  -- Subject to precision loss:
  --
  -- >>> F# (int2Float# 9223372036854775807#)
  -- 9.223372e18
  int2Float#,

  -- ** int2Double#

  -- | Convert a primitive signed integer to a primitive 64bit floating point value:
  --
  -- >>> :set -XMagicHash
  -- >>> import GHC.Float (Double (D#))
  -- >>> D# (int2Double# 5#)
  -- 5.0
  --
  -- Subject to precision loss:
  --
  -- >>> D# (int2Double# 9223372036854775807#)
  -- 9.223372036854776e18
  int2Double#,

  -- ** negateInt#

  -- | Flip the sign of a primitive signed integer:
  --
  -- >>> :set -XMagicHash
  -- >>> I# (negateInt# 5#)
  -- -5
  --
  -- Subject to underflow for the smallest value:
  --
  -- >>> I# (negateInt# -9223372036854775808#)
  -- -9223372036854775808
  negateInt#,

  -- ** (+#)

  -- | Add two primitive signed integers:
  --
  -- >>> :set -XMagicHash
  -- >>> I# (4# +# 5#)
  -- 9
  --
  -- Subject to overflow and underflow
  (+#),

  -- ** (-#)

  -- | Subtract one primitive signed integers from another:
  --
  -- >>> :set -XMagicHash
  -- >>> I# (4# -# 5#)
  -- -1
  --
  -- Subject to overflow and underflow
  (-#),

  -- ** (*#)

  -- | Multiply two primitive signed integers:
  --
  -- >>> :set -XMagicHash
  -- >>> I# (4# *# 5#)
  -- 20
  --
  -- Subject to overflow and underflow
  (*#),

  -- ** timesInt2#

  -- | Multiply two primitive signed integers and return information about overflow:
  --
  -- > >>> :set -XMagicHash -XUnboxedTuples
  -- > >>> case timesInt2# 4# 5# of (# i#, h#, l# #) -> (isTrue# i#, I# h#, I# l#)
  -- > (False, 0, 20)
  -- > >>> case timesInt2# 3# 9223372036854775806# of (# i#, h#, l# #) -> (isTrue# i#, I# h#, I# l#)
  -- > (True,1,9223372036854775802)
  -- > >>> case timesInt2# -3# 9223372036854775806# of (# i#, h#, l# #) -> (isTrue# i#, I# h#, I# l#)
  -- > (True,-2,-9223372036854775802)
  timesInt2#,

  -- ** mulIntMayOflo#

  -- | Check whether multiplication of two primitive signed integers could potentially overflow:
  --
  -- >>> :set -XMagicHash
  -- >>> isTrue# (mulIntMayOflo# 4# 5#)
  -- False
  -- >>> isTrue# (mulIntMayOflo# 12# 9223372036854775806#)
  -- True
  --
  -- It is allowed to produce false positives
  -- >>> isTrue# (mulIntMayOflo# 4# 9223372036854775806#)
  -- False
  mulIntMayOflo#,

  -- ** quotInt#

  -- | Divide one primitive signed integer by another:
  --
  -- >>> :set -XMagicHash
  -- >>> I# (quotInt# 21# 5#)
  -- 4
  -- >>> I# (quotInt# -21# 5#)
  -- -4
  --
  -- Subject to underflow:
  --
  -- >>> I# (quotInt# -9223372036854775808# -1#)
  -- -9223372036854775808
  --
  -- Fails with an unrecoverable exception when second argument is @0@
  --
  -- > >>> I# (quotInt# 5# 0#)
  -- ><process is terminated>
  quotInt#,

  -- ** remInt#

  -- | Get the remainder of dividing one primitive signed integer by another:
  --
  -- >>> :set -XMagicHash
  -- >>> I# (remInt# 24# 5#)
  -- 4
  --
  -- Fails with an unrecoverable exception when second argument is @0@
  --
  -- > >>> I# (remInt# 5# 0#)
  -- ><process is terminated>
  remInt#,

  -- ** quotRemInt#

  -- | Combination of `quotInt#` and `remInt#` in one operation:
  --
  -- > >>> :set -XMagicHash -XUnboxedTuples
  -- > >>> case quotRemInt# 21# 5# of (# q#, r# #) -> (I# q#, I# r#)
  -- > (4, 1)
  -- > >>> case quotRemInt# -21# 5# of (# q#, r# #) -> (I# q#, I# r#)
  -- > (-4,-1)
  quotRemInt#,

  -- ** andI#

  -- | Binary AND of two primitive signed integers:
  --
  -- >>> :set -XMagicHash
  -- >>> I# (andI# 2# 3#)
  -- 2
  andI#,

  -- ** orI#

  -- | Binary OR of two primitive signed integers:
  --
  -- >>> :set -XMagicHash
  -- >>> I# (orI# 2# 3#)
  -- 3
  orI#,

  -- ** xorI#

  -- | Binary XOR of two primitive signed integers:
  --
  -- >>> :set -XMagicHash
  -- >>> I# (xorI# 2# 3#)
  -- 1
  xorI#,

  -- ** notI#

  -- | Binary NOT of a primitive signed integer:
  --
  -- >>> :set -XMagicHash
  -- >>> I# (notI# 1#)
  -- -2
  notI#,

  -- ** addIntC#

  -- | Add two primitive signed integers with indication of overflow:
  --
  -- >>> :set -XMagicHash -XUnboxedTuples
  -- >>> case addIntC# 1# 5# of (# r#, o# #) -> (I# r#, isTrue# o#)
  -- (6,False)
  -- >>> case addIntC# 1# 9223372036854775807# of (# r#, o# #) -> (I# r#, isTrue# o#)
  -- (-9223372036854775808,True)
  addIntC#,

  -- ** subIntC#

  -- | Subtract one primitive signed integer from another with indication of overflow:
  --
  -- >>> :set -XMagicHash -XUnboxedTuples
  -- >>> case subIntC# 1# 5# of (# r#, o# #) -> (I# r#, isTrue# o#)
  -- (-4,False)
  -- >>> case subIntC# -9223372036854775808# 1# of (# r#, o# #) -> (I# r#, isTrue# o#)
  -- (9223372036854775807,True)
  subIntC#,

  -- ** (==#)

  -- | Compare if one primitive signed integer is equal to another:
  --
  -- >>> :set -XMagicHash
  -- >>> isTrue# (4# ==# 5#)
  -- False
  -- >>> isTrue# (5# ==# 5#)
  -- True
  -- >>> isTrue# (6# ==# 5#)
  -- False
  (==#),

  -- ** (/=#)

  -- | Compare if one primitive signed integer is not equal to another:
  --
  -- >>> :set -XMagicHash
  -- >>> isTrue# (4# /=# 5#)
  -- True
  -- >>> isTrue# (5# /=# 5#)
  -- False
  -- >>> isTrue# (6# /=# 5#)
  -- True
  (/=#),

  -- ** (>=#)

  -- | Compare if one primitive signed integer is greater or equal than another:
  --
  -- >>> :set -XMagicHash
  -- >>> isTrue# (4# >=# 5#)
  -- False
  -- >>> isTrue# (5# >=# 5#)
  -- True
  -- >>> isTrue# (6# >=# 5#)
  -- True
  (>=#),

  -- ** (>#)

  -- | Compare if one primitive signed integer is greater than another:
  --
  -- >>> :set -XMagicHash
  -- >>> isTrue# (4# ># 5#)
  -- False
  -- >>> isTrue# (5# ># 5#)
  -- False
  -- >>> isTrue# (6# ># 5#)
  -- True
  (>#),

  -- ** (<=#)

  -- | Compare if one primitive signed integer is less or equal than another:
  --
  -- >>> :set -XMagicHash
  -- >>> isTrue# (4# <=# 5#)
  -- True
  -- >>> isTrue# (5# <=# 5#)
  -- True
  -- >>> isTrue# (6# <=# 5#)
  -- False
  (<=#),

  -- ** (<#)

  -- | Compare if one primitive signed integer is less than another:
  --
  -- >>> :set -XMagicHash
  -- >>> isTrue# (4# <# 5#)
  -- True
  -- >>> isTrue# (5# <# 5#)
  -- False
  -- >>> isTrue# (6# <# 5#)
  -- False
  (<#),

  -- ** uncheckedIShiftL#

  -- | Binary shift to the left of a primitive signed integer by a number of bits. Sign bit
  -- is preserved and unaffected by the shift operation.
  --
  -- >>> :set -XMagicHash
  -- >>> I# (uncheckedIShiftL# 8# 1#)
  -- 16
  -- >>> I# (uncheckedIShiftL# -8# 1#)
  -- -16
  uncheckedIShiftL#,

  -- ** uncheckedIShiftRA#

  -- | Arithmetic binary shift to the right of a primitive signed integer by a number of
  -- bits. Sign bit is preserved and unaffected by the shift operation.
  --
  -- >>> :set -XMagicHash
  -- >>> I# (uncheckedIShiftRA# 8# 1#)
  -- 4
  -- >>> I# (uncheckedIShiftRA# -8# 1#)
  -- -4
  uncheckedIShiftRA#,

  -- ** uncheckedIShiftRL#

  -- | Logical binary shift to the right of a primitive signed integer by a number of bits. Sign bit
  -- is affected by the shift.
  --
  -- >>> :set -XMagicHash
  -- >>> I# (uncheckedIShiftRL# 8# 1#)
  -- 4
  -- >>> I# (uncheckedIShiftRL# -8# 1#)
  -- 9223372036854775804
  uncheckedIShiftRL#,

  -- * Int8

  -- | 8-bit boxed signed integer.
  Int8 (..),

  -- ** Int8#

  -- | 8-bit primitive signed integer.
  Int8#,

  -- ** int8ToInt#

  -- | Convert an 8-bit primitive signed integer to a machine word size signed integer.
  --
  -- >>> :set -XMagicHash
  -- >>> I# (int8ToInt# (intToInt8# 8#))
  -- 8
  int8ToInt#,

  -- ** intToInt8#

  -- | Convert a machine word size signed integer to an 8-bit primitive signed integer.
  --
  -- >>> :set -XMagicHash
  -- >>> I8# (intToInt8# 8#)
  -- 8
  --
  -- Subject to overflow and underflow:
  --
  -- >>> I8# (intToInt8# 128#)
  -- -128
  intToInt8#,

  -- ** int8ToWord8#

  -- | Convert an 8-bit primitive signed integer to a 8-bit primitive unsigned integer.
  --
  -- >>> :set -XMagicHash
  -- >>> import Primal.Ops.Word (Word8 (W8#))
  -- >>> W8# (int8ToWord8# (intToInt8# 8#))
  -- 8
  --
  -- Subject to underflow
  int8ToWord8#,

  -- ** word8ToInt8#

  -- | Convert an 8-bit primitive signed integer to a 8-bit primitive unsigned integer.
  --
  -- >>> :set -XMagicHash
  -- >>> import Primal.Ops.Word (wordToWord8#)
  -- >>> I8# (word8ToInt8# (wordToWord8# 8##))
  -- 8
  word8ToInt8#,

  -- ** negateInt8#

  -- | Negate an 8-bit primitive signed integer.
  --
  -- >>> :set -XMagicHash
  -- >>> I8# (negateInt8# (intToInt8# 8#))
  -- -8
  --
  -- Subject to underflow for the smallest value:
  --
  -- >>> I8# (negateInt8# (intToInt8# -128#))
  -- -128
  negateInt8#,

  -- ** plusInt8#

  -- | Add two 8-bit primitive signed integers:
  --
  -- >>> :set -XMagicHash
  -- >>> I8# (intToInt8# -4# `plusInt8#` intToInt8# 5#)
  -- 1
  --
  -- Subject to overflow and underflow
  plusInt8#,

  -- ** subInt8#

  -- | Subtract one 8-bit primitive signed integer from another:
  --
  -- >>> :set -XMagicHash
  -- >>> I8# (intToInt8# -4# `subInt8#` intToInt8# 5#)
  -- -9
  --
  -- Subject to overflow and underflow
  subInt8#,

  -- ** timesInt8#

  -- | Multiply two 8-bit primitive signed integers:
  --
  -- >>> :set -XMagicHash
  -- >>> I8# (intToInt8# -4# `timesInt8#` intToInt8# 5#)
  -- -20
  --
  -- Subject to overflow and underflow
  timesInt8#,

  -- ** quotInt8#

  -- | Divide one 8-bit primitive signed integer by another. Rounds towards zero:
  --
  -- >>> :set -XMagicHash
  -- >>> I8# (intToInt8# -24# `quotInt8#` intToInt8# 5#)
  -- -4
  --
  -- Subject to underflow with newer versons of GHC
  --
  -- > >>> I8# (intToInt8# -128# `quotInt8#` intToInt8# -1#)
  -- > -128
  --
  -- Fails with an unrecoverable exception when second argument is @0@
  --
  -- > >>> I8# (intToInt8# 8# `quotInt8#` intToInt8# 0#)
  -- ><process is terminated>
  quotInt8#,

  -- ** remInt8#

  -- | Get the remainder of dividing one 8-bit primitive signed integer by another:
  --
  -- >>> :set -XMagicHash
  -- >>> I8# (remInt8# (intToInt8# 24#) (intToInt8# 5#))
  -- 4
  --
  -- Fails with an unrecoverable exception when second argument is @0@
  --
  -- > >>> I8# (remInt# (intToInt8# 58#) (intToInt8# 0#))
  -- ><process is terminated>
  remInt8#,

  -- ** quotRemInt8#

  -- | Combination of `quotInt8#` and `remInt8#` in one operation:
  --
  -- > >>> :set -XMagicHash -XUnboxedTuples
  -- > >>> case quotRemInt8# (intToInt8# 21#) (intToInt8# 5#) of (# q#, r# #) -> (I8# q#, I8# r#)
  -- > (4, 1)
  -- > >>> case quotRemInt8# (intToInt8# -21#) (intToInt8# 5#) of (# q#, r# #) -> (I8# q#, I8# r#)
  -- > (-4,-1)
  quotRemInt8#,

  -- ** (==#)

  -- | Compare if one 8-bit primitive signed integer is equal to another:
  --
  -- >>> :set -XMagicHash
  -- >>> isTrue# (intToInt8# 4# `eqInt8#` intToInt8# 5#)
  -- False
  -- >>> isTrue# (intToInt8# 5# `eqInt8#` intToInt8# 5#)
  -- True
  -- >>> isTrue# (intToInt8# 6# `eqInt8#` intToInt8# 5#)
  -- False
  eqInt8#,
  neInt8#,
  geInt8#,
  gtInt8#,
  leInt8#,
  ltInt8#,
  uncheckedShiftLInt8#,
  uncheckedShiftRAInt8#,
  uncheckedShiftRLInt8#,

  -- * Int16

  -- | 16-bit boxed signed integer.
  Int16 (..),

  -- ** Int16#

  -- | 16-bit primitive signed integer.
  Int16#,
  int16ToInt#,
  intToInt16#,
  int16ToWord16#,
  word16ToInt16#,
  negateInt16#,
  plusInt16#,
  subInt16#,
  timesInt16#,
  quotInt16#,
  remInt16#,
  quotRemInt16#,
  eqInt16#,
  neInt16#,
  geInt16#,
  gtInt16#,
  leInt16#,
  ltInt16#,
  uncheckedShiftLInt16#,
  uncheckedShiftRAInt16#,
  uncheckedShiftRLInt16#,

  -- * Int32

  -- | 32-bit boxed signed integer.
  Int32 (..),

  -- ** Int32#

  -- | 32-bit primitive signed integer.
  Int32#,
  int32ToInt#,
  intToInt32#,
  int32ToWord32#,
  word32ToInt32#,
  negateInt32#,
  plusInt32#,
  subInt32#,
  timesInt32#,
  quotInt32#,
  remInt32#,
  quotRemInt32#,
  eqInt32#,
  neInt32#,
  geInt32#,
  gtInt32#,
  leInt32#,
  ltInt32#,
  uncheckedShiftLInt32#,
  uncheckedShiftRAInt32#,
  uncheckedShiftRLInt32#,

  -- * Int64

  -- | 64-bit boxed signed integer.
  Int64 (..),

  -- ** Int64#

  -- | 64-bit primitive signed integer.
  Int64#,
  int64ToInt#,
  intToInt64#,
  int64ToWord64#,
  word64ToInt64#,
  negateInt64#,
  plusInt64#,
  subInt64#,
  timesInt64#,
  quotInt64#,
  remInt64#,
  quotRemInt64#,
  eqInt64#,
  neInt64#,
  geInt64#,
  gtInt64#,
  leInt64#,
  ltInt64#,
  uncheckedIShiftL64#,
  uncheckedIShiftRA64#,
  uncheckedIShiftRL64#,

  -- * Useful re-exports
  isTrue#,
) where

#include "MachDeps.h"

import GHC.Int (Int16 (..), Int32 (..), Int64 (..), Int8 (..))

#if __GLASGOW_HASKELL__ >= 904
import GHC.Exts hiding (Int)

#elif __GLASGOW_HASKELL__ >= 902
import GHC.Exts hiding (Int)

#if WORD_SIZE_IN_BITS >= 64
import Primal.Ops.Int.Internal (
  int64ToInt#,
  intToInt64#,
  negateInt64#,
  plusInt64#,
  subInt64#,
  timesInt64#,
  quotInt64#,
  remInt64#,
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
  )
#endif

#elif __GLASGOW_HASKELL__ >= 900
import GHC.Exts hiding (
  Int
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
  word64ToInt64#,
  eqInt64#,
  geInt64#,
  gtInt64#,
  leInt64#,
  ltInt64#,
  neInt64#,
  )
-- Despite that changelog mentions `timesInt2#` in ghc-prim-0.6.1 it actually comes with ghc-8.10
import Primal.Ops.Int.Internal hiding (timesInt2#)
import Primal.Ops.Word.Internal (
  word16ToInt16#,
  word32ToInt32#,
  word8ToInt8#,
 )

#elif __GLASGOW_HASKELL__ >= 810
import GHC.Exts hiding (
  Int8#,
  negateInt8#,
  plusInt8#,
  subInt8#,
  timesInt8#,
  quotInt8#,
  remInt8#,
  quotRemInt8#,
  eqInt8#,
  geInt8#,
  gtInt8#,
  leInt8#,
  ltInt8#,
  neInt8#,
  Int16#,
  negateInt16#,
  plusInt16#,
  subInt16#,
  timesInt16#,
  quotInt16#,
  remInt16#,
  quotRemInt16#,
  eqInt16#,
  geInt16#,
  gtInt16#,
  leInt16#,
  ltInt16#,
  neInt16#,
  Int32#,
  Int64#,
  uncheckedIShiftL64#,
  uncheckedIShiftRA64#,
 )
import Primal.Ops.Int.Internal
import Primal.Ops.Word.Internal (
  word16ToInt16#,
  word32ToInt32#,
  word8ToInt8#,
 )

#else

import GHC.Exts (
  Int(..),
  Int#,
  (*#),
  (+#),
  (-#),
  (/=#),
  (<#),
  (<=#),
  (==#),
  (>#),
  (>=#),
  negateInt#,
  quotInt#,
  remInt#,
  addIntC#,
  subIntC#,
  quotRemInt#,
  uncheckedIShiftL#,
  uncheckedIShiftRL#,
  uncheckedIShiftRA#,
  mulIntMayOflo#,
  andI#,
  orI#,
  xorI#,
  notI#,
  int2Word#,
  int2Float#,
  int2Double#,
  isTrue#,
  )
import Primal.Ops.Int.Internal

#endif

import Primal.Ops.Int.Internal (
  word64ToInt64#,
  -- int64ToWord64#,
 )

-- $setup
--
-- >>> import Primal.Ops.Int

quotRemInt64# :: Int64# -> Int64# -> (# Int64#, Int64# #)
quotRemInt64# x# y# = (# quotInt64# x# y#, remInt64# x# y# #)
