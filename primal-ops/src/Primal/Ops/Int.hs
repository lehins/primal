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
module Primal.Ops.Int (
  -- * Fixed width signed integers

  -- ** Int#

  -- |Operations on native-size signed integers (32+ bits).
  Int#,

  -- *** (+#)

  -- | Sum two primitive integers:
  --
  -- >>> :set -XMagicHash
  -- >>> I# (4# +# 5#)
  -- 9
  (+#),

  -- *** (-#)

  -- | Subtract one primitive integers from another:
  --
  -- >>> :set -XMagicHash
  -- >>> I# (4# -# 5#)
  -- -1
  (-#),

  -- *** (*#)

  -- | Multiply two primitive signed integers:
  --
  -- >>> :set -XMagicHash
  -- >>> I# (4# *# 5#)
  -- 20
  (*#),

  -- *** timesInt2#

  -- | Multiply two primitive signed integers and return information about overflow:
  --
  -- >>> :set -XMagicHash -XUnboxedTuples
  -- >>> :{
  -- let timesInt2 x# y# =
  --       case timesInt2# x# y# of
  --          (# isHighNeeded#, high#, low# #) -> (isTrue# isHighNeeded#, I# high#, I# low#)
  -- :}
  -- >>> timesInt2 4# 5#
  -- (False, 0, 20)
  -- >>> timesInt2 3# 9223372036854775806#
  -- (True,1,9223372036854775802)
  -- >>> timesInt2 -3# 9223372036854775806#
  -- (True,-2,-9223372036854775802)
  timesInt2#,

  -- *** mulIntMayOflo#

  -- | Check whether multiplication of two primitive signed integers could potentially overflow:
  --
  -- >>> :set -XMagicHash
  -- >>> isTrue# (mulIntMayOflo# 4# 5#)
  -- False
  -- >>> isTrue# (mulIntMayOflo# 4# 9223372036854775806#)
  -- True
  mulIntMayOflo#,

  -- *** quotInt#

  -- | Divide one primitive signed integer by another:
  --
  -- >>> :set -XMagicHash
  -- >>> I# (quotInt# 21# 5#)
  -- 4
  quotInt#,

  -- *** remInt#

  -- | Get the remainder of dividing one primitive signed integer by another:
  --
  -- >>> :set -XMagicHash
  -- >>> I# (quotInt# 21# 5#)
  -- 4
  remInt#,

  -- *** quotRemInt#

  -- | Combination of `quotInt#` and `remInt#` in one operation:
  --
  -- >>> :set -XMagicHash -XUnboxedTuples
  -- >>> case quotRemInt# 21# 5# of (# q#, r# #) -> (I# q#, I# r#)
  -- (4, 1)
  -- >>> case quotRemInt# -21# 5# of (# q#, r# #) -> (I# q#, I# r#)
  -- (-4,-1)
  quotRemInt#,

  -- *** andI#

  -- | Binary AND of two primitive signed integers:
  --
  -- >>> :set -XMagicHash
  -- >>> I# (andI# 2# 3#)
  -- 2
  andI#,

  -- *** orI#

  -- | Binary OR of two primitive signed integers:
  --
  -- >>> :set -XMagicHash
  -- >>> I# (orI# 2# 3#)
  -- 3
  orI#,

  -- *** xorI#

  -- | Binary XOR of two primitive signed integers:
  --
  -- >>> :set -XMagicHash
  -- >>> I# (xorI# 2# 3#)
  -- 1
  xorI#,

  -- *** notI#

  -- | Binary NOT of a primitive signed integer:
  --
  -- >>> :set -XMagicHash
  -- >>> I# (notI# 1#)
  -- -2
  notI#,

  -- *** negateInt#

  -- | Binary NOT of a primitive signed integer:
  --
  -- >>> :set -XMagicHash
  -- >>> I# (negateInt# 1#)
  -- -1
  negateInt#,

  -- *** addIntC#

  -- | Add two primitive signed integers with indication of overflow:
  --
  -- >>> :set -XMagicHash
  -- >>> toIntTuple# (addIntC# 1# 2#)
  -- (0, 3)
  -- >>> toIntTuple# (addIntC# 1# 9223372036854775807#)
  -- (1, -9223372036854775808)
  addIntC#,
  subIntC#,
  (>#),
  (>=#),
  (==#),
  (/=#),
  (<#),
  (<=#),
  chr#,
  int2Word#,
  int2Float#,
  int2Double#,
  word2Float#,
  word2Double#,
  uncheckedIShiftL#,
  uncheckedIShiftRA#,
  uncheckedIShiftRL#,

  -- ** Int8#

  -- | Operations on 8-bit signed integers.
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

  -- * Int16#

  -- | Operations on 16-bit signed integers.
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

  -- * Int32#

  -- | Operations on 32-bit signed integers.
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

  -- * Int64#

  -- | Operations on 64-bit signed integers.
  Int64#,
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
) where

#if __GLASGOW_HASKELL__ >= 902
import GHC.Exts
#else

#if __GLASGOW_HASKELL__ >= 900
import GHC.Exts hiding (
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
  eqInt64#,
  geInt64#,
  gtInt64#,
  leInt64#,
  ltInt64#,
  neInt64#,
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
#endif

import Primal.Ops.Int.Internal hiding (timesInt2#)
#if __GLASGOW_HASKELL__ < 900
-- Despite that changelog mentions `timesInt2#` in ghc-prim-0.6.1 which comes with ghc-8.10
import Primal.Ops.Int.Internal (timesInt2#)
#endif

#endif

-- $setup
--
-- >>> :set -XMagicHash
-- >>> import GHC.Int
-- >>> import GHC.Exts (isTrue#)
-- >>> import Primal.Ops.Int
