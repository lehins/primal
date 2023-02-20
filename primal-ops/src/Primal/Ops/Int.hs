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
  -- >>> isTrue# (mulIntMayOflo# 12# 9223372036854775806#)
  -- True
  --
  -- It is allowed to produce false positives
  -- >>> isTrue# (mulIntMayOflo# 4# 9223372036854775806#)
  -- False
  mulIntMayOflo#,

  -- *** quotInt#

  -- | Divide one primitive signed integer by another:
  --
  -- >>> :set -XMagicHash
  -- >>> I# (quotInt# 21# 5#)
  -- 4
  -- >>> I# (quotInt# -21# 5#)
  -- -4
  --
  -- Subject to overflow:
  --
  -- >>> I# (quotInt# -9223372036854775808# -1#)
  -- -9223372036854775808
  --
  -- Fails with unrecoverable exception when second argument is @0#@
  --
  -- > >>> I# (quotInt# 5# 0#)
  -- ><process is terminated>
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

  -- | Flip the sign of a primitive signed integer:
  --
  -- >>> :set -XMagicHash
  -- >>> I# (negateInt# 5#)
  -- -5
  --
  -- Subject to overflow for the smallest value:
  --
  -- >>> I# (negateInt# -9223372036854775808#)
  -- -9223372036854775808
  negateInt#,

  -- *** addIntC#

  -- | Add two primitive signed integers with indication of overflow:
  --
  -- >>> :set -XMagicHash -XUnboxedTuples
  -- >>> case addIntC# 1# 5# of (# r#, o# #) -> (I# r#, isTrue# o#)
  -- (6,False)
  -- >>> case addIntC# 1# 9223372036854775807# of (# r#, o# #) -> (I# r#, isTrue# o#)
  -- (-9223372036854775808,True)
  addIntC#,

  -- *** subIntC#

  -- | Subtract one primitive signed integer from another with indication of overflow:
  --
  -- >>> :set -XMagicHash -XUnboxedTuples
  -- >>> case subIntC# 1# 5# of (# r#, o# #) -> (I# r#, isTrue# o#)
  -- (-4,False)
  -- >>> case subIntC# -9223372036854775808# 1# of (# r#, o# #) -> (I# r#, isTrue# o#)
  -- (9223372036854775807,True)
  subIntC#,

  -- *** >#

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

  -- *** >=#

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

  -- *** ==#

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

  -- *** /=#

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

  -- *** <#

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

  -- *** <=#

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

  -- *** int2Word#

  -- | Convert a primitive signed integer to a primitive unsigned integer:
  --
  -- >>> :set -XMagicHash
  -- >>> import GHC.Word (Word (W#))
  -- >>> W# (int2Word# 5#)
  -- 5
  --
  -- Subject to overflow:
  --
  -- >>> W# (int2Word# -5#)
  -- 18446744073709551611

  int2Word#,
  int2Float#,
  int2Double#,
  uncheckedIShiftL#,
  uncheckedIShiftRA#,
  uncheckedIShiftRL#,

  -- * Int8

  -- | 8-bit boxed signed integers.
  Int8 (..),

  -- ** Int8#

  -- | 8-bit primitive signed integers.
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

  -- * Int16

  -- | 16-bit boxed signed integers.
  Int16 (..),

  -- ** Int16#

  -- | 16-bit primitive signed integers.
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

  -- * Int32

  -- | 32-bit boxed signed integers.
  Int32 (..),

  -- ** Int32#

  -- | 32-bit primitive signed integers.
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

  -- * Int64

  -- | 64-bit boxed signed integers.
  Int64 (..),

  -- ** Int64#

  -- | 64-bit primitive signed integers.
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
  -- * Useful re-exports
  isTrue#,
) where

#include "MachDeps.h"


import GHC.Int (Int(..), Int8(..), Int16(..), Int32(..), Int64(..))

#if __GLASGOW_HASKELL__ >= 904
import GHC.Exts

#elif __GLASGOW_HASKELL__ >= 902
import GHC.Exts


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
#endif

#elif __GLASGOW_HASKELL__ >= 900
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
-- Despite that changelog mentions `timesInt2#` in ghc-prim-0.6.1 it actually comes with ghc-8.10
import Primal.Ops.Int.Internal hiding (timesInt2#)

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

#else

import GHC.Exts (
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

-- $setup
--
-- >>> import Primal.Ops.Int
