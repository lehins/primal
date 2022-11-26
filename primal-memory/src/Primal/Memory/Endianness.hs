{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Primal.Memory.Endianness
-- Copyright   : (c) Alexey Kuleshevich 2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module Primal.Memory.Endianness (
  ByteOrder (..),
  homeEndian,
  foreignEndian,
  wordSizeInBits,
  LE (..),
  BE (..),
  NativeEndian,
  pattern NativeEndian,
  ForeignEndian,
  pattern ForeignEndian,
  byteSwap,
  byteSwap16,
  byteSwap32,
  byteSwap64,
) where

import Data.Bits
import Foreign.C.Error (Errno (..))
import Foreign.Ptr
import GHC.ByteOrder
import Primal.Array (Size (..))
import Primal.Element.Unbox
import Primal.Foreign hiding (Any)

#include "MachDeps.h"
#include "HsBaseConfig.h"

-- | Size of the machine word in number of bits.
wordSizeInBits :: Int
wordSizeInBits = finiteBitSize (0 :: Word)

newtype BE a = BE a
  deriving (Eq, Ord, Show, Enum, Bounded)

newtype LE a = LE a
  deriving (Eq, Ord, Show, Enum, Bounded)

byteSwap :: Word -> Word
byteSwap (W# w#) = W# (byteSwap# w#)

#ifdef WORDS_BIGENDIAN
homeEndian :: ByteOrder
homeEndian = BigEndian

foreignEndian :: ByteOrder
foreignEndian = LittleEndian

type ForeignEndian = LE
pattern ForeignEndian :: a -> ForeignEndian a
pattern ForeignEndian a = LE a
{-# COMPLETE ForeignEndian #-}

type NativeEndian = BE
pattern NativeEndian :: a -> NativeEndian a
pattern NativeEndian a = BE a
{-# COMPLETE NativeEndian #-}

#else
homeEndian :: ByteOrder
homeEndian = LittleEndian

foreignEndian :: ByteOrder
foreignEndian = BigEndian

type ForeignEndian = BE
pattern ForeignEndian :: a -> ForeignEndian a
pattern ForeignEndian a = BE a
{-# COMPLETE ForeignEndian #-}

type NativeEndian = LE
pattern NativeEndian :: a -> NativeEndian a
pattern NativeEndian a = LE a
{-# COMPLETE NativeEndian #-}

#endif

-- One byte or less size types.

instance Unbox (BE Word8) where
  type UnboxIso (BE Word8) = Word8
instance Unbox (BE Int8) where
  type UnboxIso (BE Int8) = Int8
instance Unbox (BE Bool) where
  type UnboxIso (BE Bool) = Bool
instance Unbox (BE ()) where
  type UnboxIso (BE ()) = ()
instance Unbox (BE Size) where
  type UnboxIso (BE Size) = BE Int
instance Unbox (BE (Count e)) where
  type UnboxIso (BE (Count e)) = BE Int
instance Unbox (BE (Off e)) where
  type UnboxIso (BE (Off e)) = BE Int

instance Unbox (LE Word8) where
  type UnboxIso (LE Word8) = Word8
instance Unbox (LE Int8) where
  type UnboxIso (LE Int8) = Int8
instance Unbox (LE Bool) where
  type UnboxIso (LE Bool) = Bool
instance Unbox (LE ()) where
  type UnboxIso (LE ()) = ()

instance Unbox (LE Size) where
  type UnboxIso (LE Size) = LE Int
instance Unbox (LE (Count e)) where
  type UnboxIso (LE (Count e)) = LE Int
instance Unbox (LE (Off e)) where
  type UnboxIso (LE (Off e)) = LE Int

-- Wrappers or isomorphisms

instance Unbox (LE (FunPtr a)) where
  type UnboxIso (LE (FunPtr a)) = LE (Ptr a)
  fromUnboxIso (LE (Ptr addr#)) = LE (FunPtr addr#)
  {-# INLINE fromUnboxIso #-}
  toUnboxIso (LE (FunPtr addr#)) = LE (Ptr addr#)
  {-# INLINE toUnboxIso #-}

instance Unbox (LE IntPtr) where
  type UnboxIso (LE IntPtr) = LE Int
instance Unbox (LE WordPtr) where
  type UnboxIso (LE WordPtr) = LE Word
instance Unbox (LE CBool) where
  type UnboxIso (LE CBool) = LE HTYPE_BOOL
instance Unbox (LE CChar) where
  type UnboxIso (LE CChar) = LE HTYPE_CHAR
instance Unbox (LE CSChar) where
  type UnboxIso (LE CSChar) = LE HTYPE_SIGNED_CHAR
instance Unbox (LE CUChar) where
  type UnboxIso (LE CUChar) = LE HTYPE_UNSIGNED_CHAR
instance Unbox (LE CShort) where
  type UnboxIso (LE CShort) = LE HTYPE_SHORT
instance Unbox (LE CUShort) where
  type UnboxIso (LE CUShort) = LE HTYPE_UNSIGNED_SHORT
instance Unbox (LE CInt) where
  type UnboxIso (LE CInt) = LE HTYPE_INT
instance Unbox (LE CUInt) where
  type UnboxIso (LE CUInt) = LE HTYPE_UNSIGNED_INT
instance Unbox (LE CLong) where
  type UnboxIso (LE CLong) = LE HTYPE_LONG
instance Unbox (LE CULong) where
  type UnboxIso (LE CULong) = LE HTYPE_UNSIGNED_LONG
instance Unbox (LE CLLong) where
  type UnboxIso (LE CLLong) = LE HTYPE_LONG_LONG
instance Unbox (LE CULLong) where
  type UnboxIso (LE CULLong) = LE HTYPE_UNSIGNED_LONG_LONG
instance Unbox (LE CPtrdiff) where
  type UnboxIso (LE CPtrdiff) = LE HTYPE_PTRDIFF_T
instance Unbox (LE CSize) where
  type UnboxIso (LE CSize) = LE HTYPE_SIZE_T
instance Unbox (LE CWchar) where
  type UnboxIso (LE CWchar) = LE HTYPE_WCHAR_T
instance Unbox (LE CSigAtomic) where
  type UnboxIso (LE CSigAtomic) = LE HTYPE_SIG_ATOMIC_T
instance Unbox (LE CIntPtr) where
  type UnboxIso (LE CIntPtr) = LE HTYPE_INTPTR_T
instance Unbox (LE CUIntPtr) where
  type UnboxIso (LE CUIntPtr) = LE HTYPE_UINTPTR_T
instance Unbox (LE CIntMax) where
  type UnboxIso (LE CIntMax) = LE HTYPE_INTMAX_T
instance Unbox (LE CUIntMax) where
  type UnboxIso (LE CUIntMax) = LE HTYPE_UINTMAX_T
instance Unbox (LE CFloat) where
  type UnboxIso (LE CFloat) = LE HTYPE_FLOAT
instance Unbox (LE CDouble) where
  type UnboxIso (LE CDouble) = LE HTYPE_DOUBLE

instance Unbox (LE Fd) where
  type UnboxIso (LE Fd) = LE CInt
instance Unbox (LE Errno) where
  type UnboxIso (LE Errno) = LE CInt

#if defined(HTYPE_DEV_T)
instance Unbox (LE CDev) where
  type UnboxIso (LE CDev) = LE HTYPE_DEV_T
#endif
#if defined(HTYPE_INO_T)
instance Unbox (LE CIno) where
  type UnboxIso (LE CIno) = LE HTYPE_INO_T
#endif
#if defined(HTYPE_MODE_T)
instance Unbox (LE CMode) where
  type UnboxIso (LE CMode) = LE HTYPE_MODE_T
#endif
#if defined(HTYPE_OFF_T)
instance Unbox (LE COff) where
  type UnboxIso (LE COff) = LE HTYPE_OFF_T
#endif
#if defined(HTYPE_PID_T)
instance Unbox (LE CPid) where
  type UnboxIso (LE CPid) = LE HTYPE_PID_T
#endif
#if defined(HTYPE_SSIZE_T)
instance Unbox (LE CSsize) where
  type UnboxIso (LE CSsize) = LE HTYPE_SSIZE_T
#endif
#if defined(HTYPE_GID_T)
instance Unbox (LE CGid) where
  type UnboxIso (LE CGid) = LE HTYPE_GID_T
#endif
#if defined(HTYPE_NLINK_T)
instance Unbox (LE CNlink) where
  type UnboxIso (LE CNlink) = LE HTYPE_NLINK_T
#endif
#if defined(HTYPE_UID_T)
instance Unbox (LE CUid) where
  type UnboxIso (LE CUid) = LE HTYPE_UID_T
#endif
#if defined(HTYPE_CC_T)
instance Unbox (LE CCc) where
  type UnboxIso (LE CCc) = LE HTYPE_CC_T
#endif
#if defined(HTYPE_SPEED_T)
instance Unbox (LE CSpeed) where
  type UnboxIso (LE CSpeed) = LE HTYPE_SPEED_T
#endif
#if defined(HTYPE_TCFLAG_T)
instance Unbox (LE CTcflag) where
  type UnboxIso (LE CTcflag) = LE HTYPE_TCFLAG_T
#endif
#if defined(HTYPE_RLIM_T)
instance Unbox (LE CRLim) where
  type UnboxIso (LE CRLim) = LE HTYPE_RLIM_T
#endif

instance Unbox (BE (FunPtr a)) where
  type UnboxIso (BE (FunPtr a)) = BE (Ptr a)
  fromUnboxIso (BE (Ptr addr#)) = BE (FunPtr addr#)
  {-# INLINE fromUnboxIso #-}
  toUnboxIso (BE (FunPtr addr#)) = BE (Ptr addr#)
  {-# INLINE toUnboxIso #-}

instance Unbox (BE IntPtr) where
  type UnboxIso (BE IntPtr) = BE Int
instance Unbox (BE WordPtr) where
  type UnboxIso (BE WordPtr) = BE Word
instance Unbox (BE CBool) where
  type UnboxIso (BE CBool) = BE HTYPE_BOOL
instance Unbox (BE CChar) where
  type UnboxIso (BE CChar) = BE HTYPE_CHAR
instance Unbox (BE CSChar) where
  type UnboxIso (BE CSChar) = BE HTYPE_SIGNED_CHAR
instance Unbox (BE CUChar) where
  type UnboxIso (BE CUChar) = BE HTYPE_UNSIGNED_CHAR
instance Unbox (BE CShort) where
  type UnboxIso (BE CShort) = BE HTYPE_SHORT
instance Unbox (BE CUShort) where
  type UnboxIso (BE CUShort) = BE HTYPE_UNSIGNED_SHORT
instance Unbox (BE CInt) where
  type UnboxIso (BE CInt) = BE HTYPE_INT
instance Unbox (BE CUInt) where
  type UnboxIso (BE CUInt) = BE HTYPE_UNSIGNED_INT
instance Unbox (BE CLong) where
  type UnboxIso (BE CLong) = BE HTYPE_LONG
instance Unbox (BE CULong) where
  type UnboxIso (BE CULong) = BE HTYPE_UNSIGNED_LONG
instance Unbox (BE CLLong) where
  type UnboxIso (BE CLLong) = BE HTYPE_LONG_LONG
instance Unbox (BE CULLong) where
  type UnboxIso (BE CULLong) = BE HTYPE_UNSIGNED_LONG_LONG
instance Unbox (BE CPtrdiff) where
  type UnboxIso (BE CPtrdiff) = BE HTYPE_PTRDIFF_T
instance Unbox (BE CSize) where
  type UnboxIso (BE CSize) = BE HTYPE_SIZE_T
instance Unbox (BE CWchar) where
  type UnboxIso (BE CWchar) = BE HTYPE_WCHAR_T
instance Unbox (BE CSigAtomic) where
  type UnboxIso (BE CSigAtomic) = BE HTYPE_SIG_ATOMIC_T
instance Unbox (BE CIntPtr) where
  type UnboxIso (BE CIntPtr) = BE HTYPE_INTPTR_T
instance Unbox (BE CUIntPtr) where
  type UnboxIso (BE CUIntPtr) = BE HTYPE_UINTPTR_T
instance Unbox (BE CIntMax) where
  type UnboxIso (BE CIntMax) = BE HTYPE_INTMAX_T
instance Unbox (BE CUIntMax) where
  type UnboxIso (BE CUIntMax) = BE HTYPE_UINTMAX_T
instance Unbox (BE CFloat) where
  type UnboxIso (BE CFloat) = BE HTYPE_FLOAT
instance Unbox (BE CDouble) where
  type UnboxIso (BE CDouble) = BE HTYPE_DOUBLE

instance Unbox (BE Fd) where
  type UnboxIso (BE Fd) = BE CInt
instance Unbox (BE Errno) where
  type UnboxIso (BE Errno) = BE CInt

#if defined(HTYPE_DEV_T)
instance Unbox (BE CDev) where
  type UnboxIso (BE CDev) = BE HTYPE_DEV_T
#endif
#if defined(HTYPE_INO_T)
instance Unbox (BE CIno) where
  type UnboxIso (BE CIno) = BE HTYPE_INO_T
#endif
#if defined(HTYPE_MODE_T)
instance Unbox (BE CMode) where
  type UnboxIso (BE CMode) = BE HTYPE_MODE_T
#endif
#if defined(HTYPE_OFF_T)
instance Unbox (BE COff) where
  type UnboxIso (BE COff) = BE HTYPE_OFF_T
#endif
#if defined(HTYPE_PID_T)
instance Unbox (BE CPid) where
  type UnboxIso (BE CPid) = BE HTYPE_PID_T
#endif
#if defined(HTYPE_SSIZE_T)
instance Unbox (BE CSsize) where
  type UnboxIso (BE CSsize) = BE HTYPE_SSIZE_T
#endif
#if defined(HTYPE_GID_T)
instance Unbox (BE CGid) where
  type UnboxIso (BE CGid) = BE HTYPE_GID_T
#endif
#if defined(HTYPE_NLINK_T)
instance Unbox (BE CNlink) where
  type UnboxIso (BE CNlink) = BE HTYPE_NLINK_T
#endif
#if defined(HTYPE_UID_T)
instance Unbox (BE CUid) where
  type UnboxIso (BE CUid) = BE HTYPE_UID_T
#endif
#if defined(HTYPE_CC_T)
instance Unbox (BE CCc) where
  type UnboxIso (BE CCc) = BE HTYPE_CC_T
#endif
#if defined(HTYPE_SPEED_T)
instance Unbox (BE CSpeed) where
  type UnboxIso (BE CSpeed) = BE HTYPE_SPEED_T
#endif
#if defined(HTYPE_TCFLAG_T)
instance Unbox (BE CTcflag) where
  type UnboxIso (BE CTcflag) = BE HTYPE_TCFLAG_T
#endif
#if defined(HTYPE_RLIM_T)
instance Unbox (BE CRLim) where
  type UnboxIso (BE CRLim) = BE HTYPE_RLIM_T
#endif

-- Native byte order

instance Unbox (NativeEndian Char) where
  type UnboxIso (NativeEndian Char) = Char
instance Unbox (NativeEndian Word) where
  type UnboxIso (NativeEndian Word) = Word
instance Unbox (NativeEndian Word16) where
  type UnboxIso (NativeEndian Word16) = Word16
instance Unbox (NativeEndian Word32) where
  type UnboxIso (NativeEndian Word32) = Word32
instance Unbox (NativeEndian Word64) where
  type UnboxIso (NativeEndian Word64) = Word64
instance Unbox (NativeEndian Int) where
  type UnboxIso (NativeEndian Int) = Int
instance Unbox (NativeEndian Int16) where
  type UnboxIso (NativeEndian Int16) = Int16
instance Unbox (NativeEndian Int32) where
  type UnboxIso (NativeEndian Int32) = Int32
instance Unbox (NativeEndian Int64) where
  type UnboxIso (NativeEndian Int64) = Int64
instance Unbox (NativeEndian (Ptr e)) where
  type UnboxIso (NativeEndian (Ptr e)) = Ptr e
instance Unbox (NativeEndian Float) where
  type UnboxIso (NativeEndian Float) = Float
instance Unbox (NativeEndian Double) where
  type UnboxIso (NativeEndian Double) = Double

-- Foreign byte order

instance Unbox (ForeignEndian Word) where
  type UnboxIso (ForeignEndian Word) = Word
  fromUnboxIso (W# w#) = coerce (W# (byteSwap# w#))
  {-# INLINE fromUnboxIso #-}
  toUnboxIso w =
    case coerce w of
      W# w# -> W# (byteSwap# w#)
  {-# INLINE toUnboxIso #-}

instance Unbox (ForeignEndian Word16) where
  type UnboxIso (ForeignEndian Word16) = Word16
  fromUnboxIso = coerce . byteSwap16
  {-# INLINE fromUnboxIso #-}
  toUnboxIso = byteSwap16 . coerce
  {-# INLINE toUnboxIso #-}

instance Unbox (ForeignEndian Word32) where
  type UnboxIso (ForeignEndian Word32) = Word32
  fromUnboxIso = coerce . byteSwap32
  {-# INLINE fromUnboxIso #-}
  toUnboxIso = byteSwap32 . coerce
  {-# INLINE toUnboxIso #-}

instance Unbox (ForeignEndian Word64) where
  type UnboxIso (ForeignEndian Word64) = Word64
  fromUnboxIso = coerce . byteSwap64
  {-# INLINE fromUnboxIso #-}
  toUnboxIso = byteSwap64 . coerce
  {-# INLINE toUnboxIso #-}

instance Unbox (ForeignEndian Int) where
  type UnboxIso (ForeignEndian Int) = Word
  fromUnboxIso (W# w#) = coerce (I# (word2Int# (byteSwap# w#)))
  {-# INLINE fromUnboxIso #-}
  toUnboxIso i =
    case coerce i of
      I# i# -> W# (byteSwap# (int2Word# i#))
  {-# INLINE toUnboxIso #-}

instance Unbox (ForeignEndian Int16) where
  type UnboxIso (ForeignEndian Int16) = Word16
  fromUnboxIso w =
    case byteSwap16 w of
      W16# w# -> coerce (I16# (narrow16Int# (word2Int# w#)))
  {-# INLINE fromUnboxIso #-}
  toUnboxIso i =
    case coerce i of
      I16# i# -> byteSwap16 (W16# (int2Word# i#))
  {-# INLINE toUnboxIso #-}

instance Unbox (ForeignEndian Int32) where
  type UnboxIso (ForeignEndian Int32) = Word32
  fromUnboxIso w =
    case byteSwap32 w of
      W32# w# -> coerce (I32# (narrow32Int# (word2Int# w#)))
  {-# INLINE fromUnboxIso #-}
  toUnboxIso i =
    case coerce i of
      I32# i# -> byteSwap32 (W32# (int2Word# i#))
  {-# INLINE toUnboxIso #-}

instance Unbox (ForeignEndian Int64) where
  type UnboxIso (ForeignEndian Int64) = Word64
  fromUnboxIso w =
    case byteSwap64 w of
      W64# w# -> coerce (I64# (word2Int# w#))
  {-# INLINE fromUnboxIso #-}
  toUnboxIso i =
    case coerce i of
      I64# i# -> byteSwap64 (W64# (int2Word# i#))
  {-# INLINE toUnboxIso #-}

instance Unbox (ForeignEndian (Ptr e)) where
  type UnboxIso (ForeignEndian (Ptr e)) = ForeignEndian Int
  fromUnboxIso i =
    case coerce i of
      I# i# -> coerce (Ptr (int2Addr# i#))
  {-# INLINE fromUnboxIso #-}
  toUnboxIso p =
    case coerce p of
      Ptr a# -> coerce (I# (addr2Int# a#))
  {-# INLINE toUnboxIso #-}

instance Unbox (ForeignEndian Char) where
  type UnboxIso (ForeignEndian Char) = ForeignEndian Int32
  fromUnboxIso i =
    case coerce i of
      I32# i# -> coerce (C# (chr# i#))
  {-# INLINE fromUnboxIso #-}
  toUnboxIso p =
    case coerce p of
      C# c# -> coerce (I32# (ord# c#))
  {-# INLINE toUnboxIso #-}

instance Unbox (ForeignEndian Float) where
  type UnboxIso (ForeignEndian Float) = Word32
  fromUnboxIso w =
    case coerce w of
      W32# w# -> coerce (F# (word32ToFloat# (byteSwap32# w#)))
  {-# INLINE fromUnboxIso #-}
  toUnboxIso d =
    case coerce d of
      F# f# -> W32# (byteSwap32# (floatToWord32# f#))
  {-# INLINE toUnboxIso #-}

instance Unbox (ForeignEndian Double) where
  type UnboxIso (ForeignEndian Double) = Word64
  fromUnboxIso w =
    case byteSwap64 (coerce w) of
      W64# w# -> coerce (D# (word64ToDouble# w#))
  {-# INLINE fromUnboxIso #-}
  toUnboxIso d =
    case coerce d of
      D# d# -> byteSwap64 (W64# (doubleToWord64# d#))
  {-# INLINE toUnboxIso #-}
