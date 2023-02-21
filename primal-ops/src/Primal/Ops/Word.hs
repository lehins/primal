{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_HADDOCK print-explicit-runtime-reps #-}

-- |
-- Module      : Primal.Ops.Word
-- Copyright   : (c) Alexey Kuleshevich 2023
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
-- Fixed width unsigned integers
module Primal.Ops.Word (
  -- * Word

  -- | Native-size boxed unsigned integer (32+ bits).
  Word (..),

  -- ** Word#

  -- | Native-size primitive unsigned integer (32+ bits).
  Word#,

  -- ** plusWord#

  -- | Add two primitive unsigned integers:
  --
  -- >>> :set -XMagicHash
  -- >>> W# (4## `plusWord#` 5##)
  -- 9
  --
  -- Subject to overflow and underflow
  plusWord#,

  -- ** minusWord#

  -- | Subtract one primitive unsigned integers from another:
  --
  -- >>> :set -XMagicHash
  -- >>> W# (5## `minusWord#` 4##)
  -- 1
  --
  -- Subject to overflow and underflow
  -- >>> W# (4## `minusWord#` 5##)
  -- 18446744073709551615
  minusWord#,

  -- ** timesWord#

  -- | Multiply two primitive unsigned integers:
  --
  -- >>> :set -XMagicHash
  -- >>> W# (4## `timesWord#` 5##)
  -- 20
  --
  -- Subject to overflow and underflow
  timesWord#,

  -- ** timesWord2##

  -- | Multiply two primitive unsigned integers while accounting for potential
  -- overflow. Produced are the higher and the lower bits. Former will be zero when there
  -- is no overflow occurs.
  --
  -- > >>> :set -XMagicHash -XUnboxedTuples
  -- > >>> case timesWord2# 4## 5## of (# h#, l# #) -> (W# h#, W# l#)
  -- > (0, 20)
  -- > >>> case timesWord2# 10## 18446744073709551615## of (# h#, l# #) -> (W# h#, W# l#)
  -- > (9,18446744073709551606)
  timesWord2#,

  -- ** quotWord#

  -- | Divide one primitive unsigned integer by another:
  --
  -- >>> :set -XMagicHash
  -- >>> W# (quotWord# 21## 5##)
  -- 4
  --
  -- Fails with an unrecoverable exception when second argument is @0##@
  --
  -- > >>> W# (quotWord# 5## 0##)
  -- ><process is terminated>
  quotWord#,

  -- ** remWord#

  -- | Get the remainder of dividing one primitive unsigned integer by another:
  --
  -- >>> :set -XMagicHash
  -- >>> W# (remWord# 24## 5##)
  -- 4
  --
  -- Fails with an unrecoverable exception when second argument is @0##@
  --
  -- > >>> W# (remWord# 5## 0##)
  -- ><process is terminated>
  remWord#,

  -- ** quotRemWord#

  -- | Combination of `quotWord#` and `remWord#` in one operation:
  --
  -- > >>> :set -XMagicHash -XUnboxedTuples
  -- > >>> case quotRemWord# 21## 5## of (# q#, r# #) -> (W# q#, W# r#)
  -- > (4, 1)
  -- > >>> case quotRemWord# -21## 5## of (# q#, r# #) -> (W# q#, W# r#)
  -- > (-4,-1)
  quotRemWord#,

  -- ** and#

  -- | Binary AND of two primitive unsigned integers:
  --
  -- >>> :set -XMagicHash
  -- >>> W# (and# 2## 3##)
  -- 2
  and#,

  -- ** or#

  -- | Binary OR of two primitive unsigned integers:
  --
  -- >>> :set -XMagicHash
  -- >>> W# (or# 2## 3##)
  -- 3
  or#,

  -- ** xor#

  -- | Binary XOR of two primitive unsigned integers:
  --
  -- >>> :set -XMagicHash
  -- >>> W# (xor# 2## 3##)
  -- 1
  xor#,

  -- ** not#

  -- | Binary NOT of a primitive unsigned integer:
  --
  -- >>> :set -XMagicHash
  -- >>> W# (not# 1##)
  -- 18446744073709551614
  not#,

  -- ** addWordC#

  -- | Add two primitive unsigned integers with indication of overflow:
  --
  -- >>> :set -XMagicHash -XUnboxedTuples
  -- >>> case addWordC# 1## 5## of (# r#, o# #) -> (W# r#, isTrue# o#)
  -- (6,False)
  -- >>> case addWordC# 11## 18446744073709551615## of (# r#, o# #) -> (W# r#, isTrue# o#)
  -- (10,True)
  addWordC#,

  -- ** subWordC#

  -- | Subtract one primitive unsigned integer from another with indication of overflow:
  --
  -- >>> :set -XMagicHash -XUnboxedTuples
  -- >>> case subWordC# 5## 1## of (# r#, o# #) -> (W# r#, isTrue# o#)
  -- (4,False)
  -- >>> case subWordC# 1## 5## of (# r#, o# #) -> (W# r#, isTrue# o#)
  -- (18446744073709551612,True)
  subWordC#,

  -- ** eqWord#

  -- | Compare if one primitive unsigned integer is equal to another:
  --
  -- >>> :set -XMagicHash
  -- >>> isTrue# (4## `eqWord#` 5##)
  -- False
  -- >>> isTrue# (5## `eqWord#` 5##)
  -- True
  -- >>> isTrue# (6## `eqWord#` 5##)
  -- False
  eqWord#,

  -- ** neWord#

  -- | Compare if one primitive unsigned integer is not equal to another:
  --
  -- >>> :set -XMagicHash
  -- >>> isTrue# (4## `neWord#` 5##)
  -- True
  -- >>> isTrue# (5## `neWord#` 5##)
  -- False
  -- >>> isTrue# (6## `neWord#` 5##)
  -- True
  neWord#,

  -- ** geWord#

  -- | Compare if one primitive unsigned integer is greater or equal than another:
  --
  -- >>> :set -XMagicHash
  -- >>> isTrue# (4## `geWord#` 5##)
  -- False
  -- >>> isTrue# (5## `geWord#` 5##)
  -- True
  -- >>> isTrue# (6## `geWord#` 5##)
  -- True
  geWord#,

  -- ** gtWord#

  -- | Compare if one primitive unsigned integer is greater than another:
  --
  -- >>> :set -XMagicHash
  -- >>> isTrue# (4## `gtWord#` 5##)
  -- False
  -- >>> isTrue# (5## `gtWord#` 5##)
  -- False
  -- >>> isTrue# (6## `gtWord#` 5##)
  -- True
  gtWord#,

  -- ** leWord#

  -- | Compare if one primitive unsigned integer is less or equal than another:
  --
  -- >>> :set -XMagicHash
  -- >>> isTrue# (4## `leWord#` 5##)
  -- True
  -- >>> isTrue# (5## `leWord#` 5##)
  -- True
  -- >>> isTrue# (6## `leWord#` 5##)
  -- False
  leWord#,

  -- ** ltWord#

  -- | Compare if one primitive unsigned integer is less than another:
  --
  -- >>> :set -XMagicHash
  -- >>> isTrue# (4## `ltWord#` 5##)
  -- True
  -- >>> isTrue# (5## `ltWord#` 5##)
  -- False
  -- >>> isTrue# (6## `ltWord#` 5##)
  -- False
  ltWord#,

  -- ** word2Int#

  -- | Convert a primitive unsigned integer to a primitive signed integer:
  --
  -- >>> :set -XMagicHash
  -- >>> import Primal.Ops.Int (Int (I#))
  -- >>> I# (word2Int# 5##)
  -- 5
  --
  -- Subject to overflow:
  --
  -- >>> I# (word2Int# 18446744073709551614##)
  -- -2
  word2Int#,

  -- ** word2Float#

  -- | Convert a primitive unsigned integer to a primitive 32bit floating point value:
  --
  -- >>> :set -XMagicHash
  -- >>> import GHC.Float (Float (F#))
  -- >>> F# (word2Float# 5##)
  -- 5.0
  --
  -- Subject to precision loss:
  --
  -- >>> F# (word2Float# 18446744073709551615##)
  -- 1.8446744e19
  word2Float#,

  -- ** word2Double#

  -- | Convert a primitive unsigned integer to a primitive 64bit floating point value:
  --
  -- >>> :set -XMagicHash
  -- >>> import GHC.Float (Double (D#))
  -- >>> D# (word2Double# 5##)
  -- 5.0
  --
  -- Subject to precision loss:
  --
  -- >>> D# (word2Double# 18446744073709551615##)
  -- 1.8446744073709552e19
  word2Double#,

  -- ** uncheckedShiftL#

  -- | Binary shift to the left of a primitive unsigned integer by a number of bits.
  --
  -- >>> :set -XMagicHash
  -- >>> W# (uncheckedShiftL# 8## 1#)
  -- 16
  uncheckedShiftL#,

  -- ** uncheckedShiftRL#

  -- | Logical binary shift to the right of a primitive unsigned integer by a number of bits.
  --
  -- >>> :set -XMagicHash
  -- >>> W# (uncheckedShiftRL# 8## 1#)
  -- 4
  uncheckedShiftRL#,

  -- * Word8

  -- | 8-bit boxed unsigned integer.
  Word8 (..),

  -- ** Word8#

  -- | 8-bit primitive unsigned integer.
  Word8#,

  -- ** word8ToWord#

  -- | Convert an 8-bit primitive unsigned integer to a machine word size unsigned integer.
  --
  -- >>> :set -XMagicHash
  -- >>> W# (word8ToWord# (wordToWord8# 9##))
  -- 9
  word8ToWord#,

  -- ** wordToWord8#

  -- | Convert a machine word size unsigned integer to an 8-bit primitive unsigned integer.
  --
  -- >>> :set -XMagicHash
  -- >>> W8# (wordToWord8# 9##)
  -- 9
  --
  -- Subject to overflow:
  --
  -- >>> W8# (wordToWord8# 257##)
  -- 1
  wordToWord8#,

  -- ** int8ToWord8#

  -- | Convert an 8-bit primitive signed integer to an 8-bit primitive unsigned integer.
  --
  -- >>> :set -XMagicHash
  -- >>> W8# (int8ToWord8# (intToInt8# 9#))
  -- 9
  --
  -- Subject to overflow:
  --
  -- >>> W8# (int8ToWord8# (intToInt8# -9#))
  -- 247
  int8ToWord8#,

  -- ** word8ToWord8#

  -- | Convert an 8-bit primitive unsigned integer to an 8-bit primitive signed integer.
  --
  -- >>> :set -XMagicHash
  -- >>> import Primal.Ops.Int (Int8 (I8#))
  -- >>> I8# (word8ToInt8# (wordToWord8# 8##))
  -- 8
  word8ToInt8#,

  -- ** plusWord8#

  -- | Add two 8-bit primitive unsigned integers:
  --
  -- >>> :set -XMagicHash
  -- >>> W8# (wordToWord8# 4## `plusWord8#` wordToWord8# 5##)
  -- 9
  --
  -- Subject to overflow
  --
  -- >>> W8# (wordToWord8# 6## `plusWord8#` wordToWord8# 255##)
  -- 5
  plusWord8#,

  -- ** subWord8#

  -- | Subtract one 8-bit primitive unsigned integer from another:
  --
  -- >>> :set -XMagicHash
  -- >>> W8# (wordToWord8# 6## `subWord8#` wordToWord8# 3##)
  -- 3
  --
  -- Subject to underflow
  --
  -- >>> W8# (wordToWord8# 6## `subWord8#` wordToWord8# 9##)
  -- 253
  subWord8#,

  -- ** timesWord8#

  -- | Multiply two 8-bit primitive unsigned integers:
  --
  -- >>> :set -XMagicHash
  -- >>> W8# (wordToWord8# 4## `timesWord8#` wordToWord8# 5##)
  -- 20
  --
  -- Subject to overflow
  --
  -- W8# (wordToWord8# 4## `timesWord8#` wordToWord8# 128##)
  -- 0
  timesWord8#,

  -- ** quotWord8#

  -- | Divide one 8-bit primitive unsigned integer by another. Rounds towards zero:
  --
  -- >>> :set -XMagicHash
  -- >>> W8# (wordToWord8# 24## `quotWord8#` wordToWord8# 5##)
  -- 4
  --
  -- Fails with an unrecoverable exception when second argument is @0@
  --
  -- > >>> W8# (wordToWord8# 8## `quotWord8#` wordToWord8# 0##)
  -- ><process is terminated>
  quotWord8#,

  -- ** remWord8#

  -- | Get the remainder of dividing one 8-bit primitive unsigned integer by another:
  --
  -- >>> :set -XMagicHash
  -- >>> W8# (remWord8# (wordToWord8# 24##) (wordToWord8# 5##))
  -- 4
  --
  -- Fails with an unrecoverable exception when second argument is @0@
  --
  -- > >>> W8# (remWord8# (wordToWord8# 5##) (wordToWord8# 0##))
  -- ><process is terminated>
  remWord8#,

  -- ** quotRemWord8#

  -- | Combination of `quotWord8#` and `remWord8#` in one operation:
  --
  -- > >>> :set -XMagicHash -XUnboxedTuples
  -- > >>> case quotRemWord8# (wordToWord8# 21##) (wordToWord8# 5##) of (# q#, r# #) -> (W8# q#, W8# r#)
  -- > (4, 1)
  quotRemWord8#,

  -- ** (==#)

  -- | Compare if one 8-bit primitive unsigned integer is equal to another:
  --
  -- >>> :set -XMagicHash
  -- >>> isTrue# (wordToWord8# 4## `eqWord8#` wordToWord8# 5##)
  -- False
  -- >>> isTrue# (wordToWord8# 5## `eqWord8#` wordToWord8# 5##)
  -- True
  -- >>> isTrue# (wordToWord8# 6## `eqWord8#` wordToWord8# 5##)
  -- False
  eqWord8#,
  neWord8#,
  geWord8#,
  gtWord8#,
  leWord8#,
  ltWord8#,
  uncheckedShiftLWord8#,
  uncheckedShiftRLWord8#,

  -- * Word16

  -- | 16-bit boxed unsigned integer.
  Word16 (..),

  -- ** Word16#

  -- | 16-bit primitive unsigned integer.
  Word16#,
  word16ToWord#,
  wordToWord16#,
  int16ToWord16#,
  word16ToInt16#,
  plusWord16#,
  subWord16#,
  timesWord16#,
  quotWord16#,
  remWord16#,
  quotRemWord16#,
  eqWord16#,
  neWord16#,
  geWord16#,
  gtWord16#,
  leWord16#,
  ltWord16#,
  uncheckedShiftLWord16#,
  uncheckedShiftRLWord16#,

  -- * Word32

  -- | 32-bit boxed unsigned integer.
  Word32 (..),

  -- ** Word32#

  -- | 32-bit primitive unsigned integer.
  Word32#,
  word32ToWord#,
  wordToWord32#,
  int32ToWord32#,
  word32ToInt32#,
  plusWord32#,
  subWord32#,
  timesWord32#,
  quotWord32#,
  remWord32#,
  quotRemWord32#,
  eqWord32#,
  neWord32#,
  geWord32#,
  gtWord32#,
  leWord32#,
  ltWord32#,
  uncheckedShiftLWord32#,
  uncheckedShiftRLWord32#,

  -- * Word64

  -- | 64-bit boxed unsigned integer.
  Word64 (..),

  -- ** Word64#

  -- | 64-bit primitive unsigned integer.
  Word64#,
  int64ToWord#,
  intToWord64#,
  int64ToWord64#,
  word64ToInt64#,
  plusWord64#,
  subWord64#,
  timesWord64#,
  quotWord64#,
  remWord64#,
  quotRemWord64#,
  eqWord64#,
  neWord64#,
  geWord64#,
  gtWord64#,
  leWord64#,
  ltWord64#,
  uncheckedShiftL64#,
  uncheckedShiftRL64#,

  -- * Useful re-exports
  isTrue#,
) where

#include "MachDeps.h"

import GHC.Word (Word16 (..), Word32 (..), Word64 (..), Word8 (..))

#if __GLASGOW_HASKELL__ >= 904
import GHC.Exts hiding (Word)

#elif __GLASGOW_HASKELL__ >= 902
import GHC.Exts hiding (Word)

#if WORD_SIZE_IN_BITS >= 64
import Primal.Ops.Word.Internal (
  int64ToWord#,
  intToWord64#,
  plusWord64#,
  subWord64#,
  timesWord64#,
  quotWord64#,
  remWord64#,
  uncheckedShiftL64#,
  uncheckedShiftRL64#,
  int64ToWord64#,
  eqWord64#,
  geWord64#,
  gtWord64#,
  leWord64#,
  ltWord64#,
  neWord64#,
  )
#endif

#elif __GLASGOW_HASKELL__ >= 900
import GHC.Exts hiding (
  Word
  Word8#,
  int8ToWord#,
  intToWord8#,
  plusWord8#,
  subWord8#,
  timesWord8#,
  quotWord8#,
  remWord8#,
  quotRemWord8#,
  uncheckedShiftLWord8#,
  uncheckedShiftRLWord8#,
  int8ToWord8#,
  eqWord8#,
  geWord8#,
  gtWord8#,
  leWord8#,
  ltWord8#,
  neWord8#,
  Word16#,
  int16ToWord#,
  intToWord16#,
  plusWord16#,
  subWord16#,
  timesWord16#,
  quotWord16#,
  remWord16#,
  quotRemWord16#,
  uncheckedShiftLWord16#,
  uncheckedShiftRLWord16#,
  int16ToWord16#,
  eqWord16#,
  geWord16#,
  gtWord16#,
  leWord16#,
  ltWord16#,
  neWord16#,
  Word32#,
  int32ToWord#,
  intToWord32#,
  plusWord32#,
  subWord32#,
  timesWord32#,
  quotWord32#,
  remWord32#,
  quotRemWord32#,
  uncheckedShiftLWord32#,
  uncheckedShiftRLWord32#,
  int32ToWord32#,
  eqWord32#,
  geWord32#,
  gtWord32#,
  leWord32#,
  ltWord32#,
  neWord32#,
  Word64#,
  int64ToWord#,
  intToWord64#,
  plusWord64#,
  subWord64#,
  timesWord64#,
  quotWord64#,
  remWord64#,
  quotRemWord64#,
  uncheckedShiftL64#,
  uncheckedShiftRL64#,
  int64ToWord64#,
  word64ToWord64#,
  eqWord64#,
  geWord64#,
  gtWord64#,
  leWord64#,
  ltWord64#,
  neWord64#,
  )
-- Despite that changelog mentions `timesWord2#` in ghc-prim-0.6.1 it actually comes with ghc-8.10
import Primal.Ops.Word.Internal hiding (timesWord2#)
import Primal.Ops.Word.Internal (
  word16ToWord16#,
  word32ToWord32#,
  word8ToWord8#,
 )

#elif __GLASGOW_HASKELL__ >= 810
import GHC.Exts hiding (
  Word8#,
  plusWord8#,
  subWord8#,
  timesWord8#,
  quotWord8#,
  remWord8#,
  quotRemWord8#,
  eqWord8#,
  geWord8#,
  gtWord8#,
  leWord8#,
  ltWord8#,
  neWord8#,
  Word16#,
  plusWord16#,
  subWord16#,
  timesWord16#,
  quotWord16#,
  remWord16#,
  quotRemWord16#,
  eqWord16#,
  geWord16#,
  gtWord16#,
  leWord16#,
  ltWord16#,
  neWord16#,
  Word32#,
  Word64#,
  uncheckedShiftL64#,
 )
import Primal.Ops.Word.Internal
import Primal.Ops.Word.Internal (
  word16ToWord16#,
  word32ToWord32#,
  word8ToWord8#,
 )

#else

import GHC.Exts (
  Word(..),
  Word#,
  timesWord#,
  plusWord#,
  minusWord#,
  neWord#,
  ltWord#,
  leWord#,
  eqWord#,
  gtWord#,
  geWord#,
  quotWord#,
  remWord#,
  addWordC#,
  subWordC#,
  quotRemWord#,
  uncheckedShiftL#,
  uncheckedShiftRL#,
  and#,
  or#,
  xor#,
  not#,
  word2Int#,
  word2Float#,
  word2Double#,
  isTrue#,
  )
import Primal.Ops.Word.Internal
import Primal.Ops.Int.Internal (
  word16ToInt16#,
  word32ToInt32#,
  word8ToInt8#,
  int16ToWord16#,
  int32ToWord32#,
  int8ToWord8#,
 )
#endif

import Primal.Ops.Int.Internal (
  word64ToInt64#,
  -- int64ToWord64#,
 )

-- $setup
--
-- >>> import Primal.Ops.Word

quotRemWord64# :: Word64# -> Word64# -> (# Word64#, Word64# #)
quotRemWord64# x# y# = (# quotWord64# x# y#, remWord64# x# y# #)
