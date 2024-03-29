{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Primal.Element.Unbox.Instances
-- Copyright   : (c) Alexey Kuleshevich 2020-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Element.Unbox.Instances
  ( bool2Int#
  , int2Bool#
  -- * Backwards compatibility
  , WordPtr(..)
  , ptrToWordPtr
  , wordPtrToPtr
  , IntPtr(..)
  , ptrToIntPtr
  , intPtrToPtr
  ) where


#include "MachDeps.h"
#include "HsBaseConfig.h"

import Data.Bits
import Data.Complex
import Data.Char
import Data.Type.Equality
import Foreign.C.Error (Errno(..))
import Primal.Element.Unbox.Class
import Primal.Element.Unbox.Tuples ()
import Primal.Element.Unbox.Primitive ()
import Primal.Foreign hiding (Any)
import GHC.Conc
import GHC.Real
import GHC.IO.Device
import GHC.Fingerprint.Type
import GHC.TypeLits as Nats
import Data.Functor.Compose
import Data.Functor.Identity
import qualified Data.Functor.Product as Functor
import Data.Monoid
import System.IO
import Data.Semigroup
#if __GLASGOW_HASKELL__ >= 800
import Data.Functor.Const
#endif /* __GLASGOW_HASKELL__ >= 800 */

#if __GLASGOW_HASKELL__ < 802
import qualified Foreign.Ptr as P
import Unsafe.Coerce
#include "primal_compat.h"

import Foreign.Storable (Storable)

-- | Replacement for `Foreign.Ptr.IntPtr` with exported constructor.
newtype IntPtr = IntPtr Int
  deriving (Eq, Ord, Num, Enum, Storable, Real, Bounded, Integral, Bits, FiniteBits, Read, Show)

-- | Replacement for `Foreign.Ptr.WordPtr` with exported constructor.
newtype WordPtr = WordPtr Word
  deriving (Eq, Ord, Num, Enum, Storable, Real, Bounded, Integral, Bits, FiniteBits, Read, Show)

-- | casts a @Ptr@ to a @WordPtr@
ptrToWordPtr :: Ptr a -> WordPtr
ptrToWordPtr (Ptr a#) = WordPtr (W# (int2Word# (addr2Int# a#)))

-- | casts a @WordPtr@ to a @Ptr@
wordPtrToPtr :: WordPtr -> Ptr a
wordPtrToPtr (WordPtr (W# w#)) = Ptr (int2Addr# (word2Int# w#))

-- | casts a @Ptr@ to an @IntPtr@
ptrToIntPtr :: Ptr a -> IntPtr
ptrToIntPtr (Ptr a#) = IntPtr (I# (addr2Int# a#))

-- | casts an @IntPtr@ to a @Ptr@
intPtrToPtr :: IntPtr -> Ptr a
intPtrToPtr (IntPtr (I# i#)) = Ptr (int2Addr# i#)

instance Unbox P.IntPtr where
  type UnboxIso P.IntPtr = IntPtr
  -- Constructor for newtype was not exported
  toUnboxIso = unsafeCoerce
  fromUnboxIso = unsafeCoerce

instance Unbox P.WordPtr where
  type UnboxIso P.WordPtr = WordPtr
  -- Constructor for newtype was not exported
  toUnboxIso = unsafeCoerce
  fromUnboxIso = unsafeCoerce
#else
import Foreign.Ptr
#endif


instance Unbox () where
  type UnboxIso () = ()
  type SizeOf () = 0
  type Alignment () = 1
  sizeOf# _ = 0#
  {-# INLINE sizeOf# #-}
  alignment# _ = 1#
  {-# INLINE alignment# #-}
  indexByteOffByteArray# _ _ = ()
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# _ _ = ()
  {-# INLINE indexByteArray# #-}
  indexOffAddr# _ _ = ()
  {-# INLINE indexOffAddr# #-}
  readByteOffMutableByteArray# _ _ s = (# s, () #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readMutableByteArray# _ _ s = (# s, () #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# _ _ s = (# s, () #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# _ _ () s = s
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# _ _ () s = s
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# _ _ () s = s
  {-# INLINE writeOffAddr# #-}
  setByteOffMutableByteArray# _ _ _ () s = s
  {-# INLINE setByteOffMutableByteArray# #-}
  setAddr# _ _ () s = s
  {-# INLINE setAddr# #-}

instance a ~ b => Unbox (a :~: b) where
  type UnboxIso (a :~: b) = ()
  toUnboxIso Refl = ()
  fromUnboxIso () = Refl


bool2Int# :: Bool -> Int#
bool2Int# b = if b then 1# else 0#
{-# INLINE bool2Int# #-}

int2Bool# :: Int# -> Bool
int2Bool# i# = isTrue# (i# /=# 0#) -- tagToEnum# (i# /=# 0#) -- (andI# i# 1#)
{-# INLINE int2Bool# #-}

instance Unbox Bool where
  type UnboxIso Bool = Int8
  fromUnboxIso (I8# i#) = int2Bool# i#
  {-# INLINE fromUnboxIso #-}
  toUnboxIso b = I8# (bool2Int# b)
  {-# INLINE toUnboxIso #-}

instance Unbox (FunPtr a) where
  type UnboxIso (FunPtr a) = Ptr a
  fromUnboxIso (Ptr addr#) = FunPtr addr#
  {-# INLINE fromUnboxIso #-}
  toUnboxIso (FunPtr addr#) = Ptr addr#
  {-# INLINE toUnboxIso #-}

instance Unbox IntPtr where
  type UnboxIso IntPtr = Int

instance Unbox WordPtr where
  type UnboxIso WordPtr = Word

instance Unbox CBool where
  type UnboxIso CBool = HTYPE_BOOL

instance Unbox CChar where
  type UnboxIso CChar = HTYPE_CHAR

instance Unbox CSChar where
  type UnboxIso CSChar = HTYPE_SIGNED_CHAR

instance Unbox CUChar where
  type UnboxIso CUChar = HTYPE_UNSIGNED_CHAR

instance Unbox CShort where
  type UnboxIso CShort = HTYPE_SHORT

instance Unbox CUShort where
  type UnboxIso CUShort = HTYPE_UNSIGNED_SHORT

instance Unbox CInt where
  type UnboxIso CInt = HTYPE_INT

instance Unbox CUInt where
  type UnboxIso CUInt = HTYPE_UNSIGNED_INT

instance Unbox CLong where
  type UnboxIso CLong = HTYPE_LONG

instance Unbox CULong where
  type UnboxIso CULong = HTYPE_UNSIGNED_LONG

instance Unbox CLLong where
  type UnboxIso CLLong = HTYPE_LONG_LONG

instance Unbox CULLong where
  type UnboxIso CULLong = HTYPE_UNSIGNED_LONG_LONG

instance Unbox CPtrdiff where
  type UnboxIso CPtrdiff = HTYPE_PTRDIFF_T

instance Unbox CSize where
  type UnboxIso CSize = HTYPE_SIZE_T

instance Unbox CWchar where
  type UnboxIso CWchar = HTYPE_WCHAR_T

instance Unbox CSigAtomic where
  type UnboxIso CSigAtomic = HTYPE_SIG_ATOMIC_T

instance Unbox CIntPtr where
  type UnboxIso CIntPtr = HTYPE_INTPTR_T

instance Unbox CUIntPtr where
  type UnboxIso CUIntPtr = HTYPE_UINTPTR_T

instance Unbox CIntMax where
  type UnboxIso CIntMax = HTYPE_INTMAX_T

instance Unbox CUIntMax where
  type UnboxIso CUIntMax = HTYPE_UINTMAX_T

instance Unbox CFloat where
  type UnboxIso CFloat = HTYPE_FLOAT

instance Unbox CDouble where
  type UnboxIso CDouble = HTYPE_DOUBLE


instance Unbox Fd where
  type UnboxIso Fd = CInt

instance Unbox Errno where
  type UnboxIso Errno = CInt


#if defined(HTYPE_DEV_T)
instance Unbox CDev where
  type UnboxIso CDev = HTYPE_DEV_T
#endif
#if defined(HTYPE_INO_T)
instance Unbox CIno where
  type UnboxIso CIno = HTYPE_INO_T
#endif
#if defined(HTYPE_MODE_T)
instance Unbox CMode where
  type UnboxIso CMode = HTYPE_MODE_T
#endif
#if defined(HTYPE_OFF_T)
instance Unbox COff where
  type UnboxIso COff = HTYPE_OFF_T
#endif
#if defined(HTYPE_PID_T)
instance Unbox CPid where
  type UnboxIso CPid = HTYPE_PID_T
#endif
#if defined(HTYPE_SSIZE_T)
instance Unbox CSsize where
  type UnboxIso CSsize = HTYPE_SSIZE_T
#endif
#if defined(HTYPE_GID_T)
instance Unbox CGid where
  type UnboxIso CGid = HTYPE_GID_T
#endif
#if defined(HTYPE_NLINK_T)
instance Unbox CNlink where
  type UnboxIso CNlink = HTYPE_NLINK_T
#endif
#if defined(HTYPE_UID_T)
instance Unbox CUid where
  type UnboxIso CUid = HTYPE_UID_T
#endif
#if defined(HTYPE_CC_T)
instance Unbox CCc where
  type UnboxIso CCc = HTYPE_CC_T
#endif
#if defined(HTYPE_SPEED_T)
instance Unbox CSpeed where
  type UnboxIso CSpeed = HTYPE_SPEED_T
#endif
#if defined(HTYPE_TCFLAG_T)
instance Unbox CTcflag where
  type UnboxIso CTcflag = HTYPE_TCFLAG_T
#endif
#if defined(HTYPE_RLIM_T)
instance Unbox CRLim where
  type UnboxIso CRLim = HTYPE_RLIM_T
#endif

instance Unbox a => Unbox (Max a) where
  type UnboxIso (Max a) = a
instance Unbox a => Unbox (Min a) where
  type UnboxIso (Min a) = a
instance Unbox a => Unbox (Data.Semigroup.First a) where
  type UnboxIso (Data.Semigroup.First a) = a
instance Unbox a => Unbox (Data.Semigroup.Last a) where
  type UnboxIso (Data.Semigroup.Last a) = a
instance Unbox a => Unbox (Data.Monoid.First a) where
  type UnboxIso (Data.Monoid.First a) = Maybe a
instance Unbox a => Unbox (Data.Monoid.Last a) where
  type UnboxIso (Data.Monoid.Last a) = Maybe a
instance (Unbox a, Unbox b) => Unbox (Arg a b) where
  type UnboxIso (Arg a b) = (a, b)
  toUnboxIso (Arg a b) = (a, b)
  fromUnboxIso (a, b) = Arg a b

#if __GLASGOW_HASKELL__ >= 800
instance Unbox a => Unbox (Const a b) where
  type UnboxIso (Const a b) = a
#endif /* __GLASGOW_HASKELL__ >= 800 */


#if __GLASGOW_HASKELL__ >= 802

instance a ~ b => Unbox (a :~~: b) where
  type UnboxIso (a :~~: b) = ()
  toUnboxIso HRefl = ()
  fromUnboxIso () = HRefl

#if defined(HTYPE_BLKSIZE_T)
instance Unbox CBlkSize where
  type UnboxIso CBlkSize = HTYPE_BLKSIZE_T
#endif
#if defined(HTYPE_BLKCNT_T)
instance Unbox CBlkCnt where
  type UnboxIso CBlkCnt = HTYPE_BLKCNT_T
#endif
#if defined(HTYPE_CLOCKID_T)
instance Unbox CClockId where
  type UnboxIso CClockId = HTYPE_CLOCKID_T
#endif
#if defined(HTYPE_FSBLKCNT_T)
instance Unbox CFsBlkCnt where
  type UnboxIso CFsBlkCnt = HTYPE_FSBLKCNT_T
#endif
#if defined(HTYPE_FSFILCNT_T)
instance Unbox CFsFilCnt where
  type UnboxIso CFsFilCnt = HTYPE_FSFILCNT_T
#endif
#if defined(HTYPE_ID_T)
instance Unbox CId where
  type UnboxIso CId = HTYPE_ID_T
#endif
#if defined(HTYPE_KEY_T)
instance Unbox CKey where
  type UnboxIso CKey = HTYPE_KEY_T
#endif
#if defined(HTYPE_TIMER_T)
instance Unbox CTimer where
  type UnboxIso CTimer = HTYPE_TIMER_T
#endif

#if __GLASGOW_HASKELL__ >= 810

#if defined(HTYPE_SOCKLEN_T)
instance Unbox CSocklen where
  type UnboxIso CSocklen = HTYPE_SOCKLEN_T
#endif
#if defined(HTYPE_NFDS_T)
instance Unbox CNfds where
  type UnboxIso CNfds = HTYPE_NFDS_T
#endif

#endif /* __GLASGOW_HASKELL__ >= 810 */

#if __GLASGOW_HASKELL__ >= 806
instance Unbox (f a) => Unbox (Ap f a) where
  type UnboxIso (Ap f a) = f a
#endif /* __GLASGOW_HASKELL__ >= 806 */


#endif /* __GLASGOW_HASKELL__ >= 802 */


instance (Unbox (f a), Unbox (g a)) => Unbox (Functor.Product f g a) where
  type UnboxIso (Functor.Product f g a) = (f a, g a)
  toUnboxIso (Functor.Pair fa ga) = (fa, ga)
  {-# INLINE toUnboxIso #-}
  fromUnboxIso (fa, ga) = Functor.Pair fa ga
  {-# INLINE fromUnboxIso #-}


instance Unbox (f (g a)) => Unbox (Compose f g a) where
  type UnboxIso (Compose f g a) = f (g a)

instance Unbox a => Unbox (Identity a) where
  type UnboxIso (Identity a) = a

instance Unbox (f a) => Unbox (Alt f a) where
  type UnboxIso (Alt f a) = f a

instance Unbox Ordering where
  type UnboxIso Ordering = Int8
  toUnboxIso o = I8# (fromOrdering# o)
  {-# INLINE toUnboxIso #-}
  fromUnboxIso (I8# i#) = toOrdering# i#
  {-# INLINE fromUnboxIso #-}

instance Unbox IODeviceType where
  type UnboxIso IODeviceType = Int8
  toUnboxIso =
    \case
      Directory -> 0
      Stream -> 1
      RegularFile -> 2
      RawDevice -> 3
  {-# INLINE toUnboxIso #-}
  fromUnboxIso =
    \case
      0 -> Directory
      1 -> Stream
      2 -> RegularFile
      _ -> RawDevice
  {-# INLINE fromUnboxIso #-}

instance Unbox SeekMode where
  type UnboxIso SeekMode = Int8
  toUnboxIso = \case
    AbsoluteSeek -> 0
    RelativeSeek -> 1
    SeekFromEnd  -> 2
  {-# INLINE toUnboxIso #-}
  fromUnboxIso = \case
    0 -> AbsoluteSeek
    1 -> RelativeSeek
    _ -> SeekFromEnd
  {-# INLINE fromUnboxIso #-}

instance Unbox BlockReason where
  type UnboxIso BlockReason = Int8
  toUnboxIso =
    \case
      BlockedOnMVar -> 0
      BlockedOnBlackHole -> 1
      BlockedOnException -> 2
      BlockedOnSTM -> 3
      BlockedOnForeignCall -> 4
#if __GLASGOW_HASKELL >= 900
      BlockedOnIOCompletion -> 5
#endif
      BlockedOnOther -> 6
  {-# INLINE toUnboxIso #-}
  fromUnboxIso =
    \case
      0 -> BlockedOnMVar
      1 -> BlockedOnBlackHole
      2 -> BlockedOnException
      3 -> BlockedOnSTM
      4 -> BlockedOnForeignCall
#if __GLASGOW_HASKELL >= 900
      5 -> BlockedOnIOCompletion
#endif
      _ -> BlockedOnOther
  {-# INLINE fromUnboxIso #-}


instance Unbox ThreadStatus where
  type UnboxIso ThreadStatus = Int8
  toUnboxIso =
    \case
      ThreadRunning -> 0x00
      ThreadFinished -> 0x10
      ThreadBlocked br -> 0x20 .|. toUnboxIso br
      ThreadDied -> 0x30
  {-# INLINE toUnboxIso #-}
  fromUnboxIso =
    \case
      0x00 -> ThreadRunning
      0x10 -> ThreadFinished
      0x30 -> ThreadDied
      x -> ThreadBlocked $ fromUnboxIso (x .&. 0xf)
  {-# INLINE fromUnboxIso #-}

instance Unbox IOMode where
  type UnboxIso IOMode = Int8
  toUnboxIso =
    \case
      ReadMode -> 0
      WriteMode -> 1
      AppendMode -> 2
      ReadWriteMode -> 3
  {-# INLINE toUnboxIso #-}
  fromUnboxIso =
    \case
      0 -> ReadMode
      1 -> WriteMode
      2 -> AppendMode
      _ -> ReadWriteMode
  {-# INLINE fromUnboxIso #-}

instance Unbox BufferMode where
  type UnboxIso BufferMode = (Int8, Maybe Int)
  toUnboxIso =
    \case
      NoBuffering -> (0, Nothing)
      LineBuffering -> (1, Nothing)
      BlockBuffering mb -> (2, mb)
  {-# INLINE toUnboxIso #-}
  fromUnboxIso =
    \case
      (0, _) -> NoBuffering
      (1, _) -> LineBuffering
      (_, mb) -> BlockBuffering mb
  {-# INLINE fromUnboxIso #-}

instance Unbox Newline where
  type UnboxIso Newline = Int8
  toUnboxIso =
    \case
      LF -> 0
      CRLF -> 1
  {-# INLINE toUnboxIso #-}
  fromUnboxIso =
    \case
      0 -> LF
      _ -> CRLF
  {-# INLINE fromUnboxIso #-}

instance Unbox NewlineMode where
  type UnboxIso NewlineMode = Int8
  toUnboxIso (NewlineMode i o) =
    (toUnboxIso i `unsafeShiftL` 1) .|. toUnboxIso o
  {-# INLINE toUnboxIso #-}
  fromUnboxIso p =
    NewlineMode
      (fromUnboxIso ((p `unsafeShiftR` 1) .&. 1))
      (fromUnboxIso (p .&. 1))
  {-# INLINE fromUnboxIso #-}

instance Unbox GeneralCategory where
  type UnboxIso GeneralCategory = Word8
  toUnboxIso = fromIntegral . fromEnum
  {-# INLINE toUnboxIso #-}
  fromUnboxIso p
    | ip > fromEnum (maxBound :: GeneralCategory) = NotAssigned
    | otherwise = toEnum ip
    where
      ip = fromIntegral p
  {-# INLINE fromUnboxIso #-}


instance Unbox a => Unbox (Down a) where
  type UnboxIso (Down a) = a

instance Unbox a => Unbox (Dual a) where
  type UnboxIso (Dual a) = a

instance Unbox a => Unbox (Sum a) where
  type UnboxIso (Sum a) = a

instance Unbox a => Unbox (Product a) where
  type UnboxIso (Product a) = a

instance Unbox All where
  type UnboxIso All = Bool

instance Unbox Any where
  type UnboxIso Any = Bool

instance Unbox Fingerprint where
  type UnboxIso Fingerprint = (Word64, Word64)
  type Alignment Fingerprint = Alignment Word64
  alignment# _ = alignment# (proxy# :: Proxy# Word64)
  toUnboxIso (Fingerprint a b) = (a, b)
  fromUnboxIso (a, b) = Fingerprint a b

instance Unbox a => Unbox (Ratio a) where
  type UnboxIso (Ratio a) = (a, a)
  type Alignment (Ratio a) = Alignment a
  alignment# _ = alignment# (proxy# :: Proxy# a)
  toUnboxIso (a :% b) = (a, b)
  fromUnboxIso (a, b) = a :% b

instance Unbox a => Unbox (Complex a) where
  type UnboxIso (Complex a) = (a, a)
  type Alignment (Complex a) = Alignment a
  alignment# _ = alignment# (proxy# :: Proxy# a)
  toUnboxIso (a :+ b) = (a, b)
  fromUnboxIso (a, b) = a :+ b


instance Unbox a => Unbox (Maybe a) where
  type UnboxIso (Maybe a) = Maybe a
  type SizeOf (Maybe a) = 1 + SizeOf a
  type Alignment (Maybe a) = 1 + Alignment a
  sizeOf# _ = 1# +# sizeOf# (proxy# :: Proxy# a)
  {-# INLINE sizeOf# #-}
  alignment# _ = 1# +# alignment# (proxy# :: Proxy# a)
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i# =
    case indexInt8Array# ba# i# of
      0# -> Nothing
      _  -> Just (indexByteOffByteArray# ba# (i# +# 1#))
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# =
    indexByteOffByteArray# ba# (i# *# sizeOf# (proxy# :: Proxy# (Maybe a)))
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# =
    let addr0# = addr# `plusAddr#` (i# *# sizeOf# (proxy# :: Proxy# (Maybe a)))
    in case indexInt8OffAddr# addr0# 0# of
      0# -> Nothing
      _  -> Just (indexOffAddr# (addr0# `plusAddr#` 1#) 0#)
  {-# INLINE indexOffAddr# #-}
  readByteOffMutableByteArray# mba# i# s =
    case readInt8Array# mba# i# s of
      (# s', 0# #) -> (# s', Nothing #)
      (# s', _  #) -> case readByteOffMutableByteArray# mba# (i# +# 1#) s' of
                        (# s'', a #) -> (# s'', Just a #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readMutableByteArray# mba# i# =
    let i0# = i# *# sizeOf# (proxy# :: Proxy# (Maybe a))
     in readByteOffMutableByteArray# mba# i0#
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# addr# i# s =
    let addr0# = addr# `plusAddr#` (i# *# sizeOf# (proxy# :: Proxy# (Maybe a)))
    in case readInt8OffAddr# addr0# 0# s of
         (# s', 0# #) -> (# s', Nothing #)
         (# s', _  #) -> case readOffAddr# (addr0# `plusAddr#` 1#) 0# s' of
                           (# s'', a #) -> (# s'', Just a #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i# mVal s =
    case mVal of
      Nothing -> setByteArray# mba# i# (sizeOf# (proxy# :: Proxy# (Maybe a))) 0# s
      Just a  -> writeByteOffMutableByteArray# mba# (i# +# 1#) a (writeInt8Array# mba# i# 1# s)
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# mVal s =
    let k# = sizeOf# (proxy# :: Proxy# (Maybe a))
        i0# = i# *# k#
    in case mVal of
         Nothing -> setByteArray# mba# i0# k# 0# s
         Just a  -> writeByteOffMutableByteArray# mba# (i0# +# 1#) a (writeInt8Array# mba# i0# 1# s)
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# addr# i# mVal s =
    let k# = sizeOf# (proxy# :: Proxy# (Maybe a))
        i0# = i# *# k#
    in case mVal of
         Nothing -> setOffAddr# addr# i0# k# (I8# 0#) s
         Just a  ->
           writeOffAddr# (addr# `plusAddr#` (i0# +# 1#)) 0# a (writeInt8OffAddr# addr# i0# 1# s)
  {-# INLINE writeOffAddr# #-}
  setByteOffMutableByteArray# mba# o# n# mVal s =
    case mVal of
      Nothing ->
        let k# = sizeOf# (proxy# :: Proxy# (Maybe a))
        in setByteArray# mba# o# (n# *# k#) 0# s
      _       -> setByteOffMutableByteArrayLoop# mba# o# n# mVal s
  {-# INLINE setByteOffMutableByteArray# #-}
  setAddr# addr# n# mVal s =
    case mVal of
      Nothing ->
        let k# = sizeOf# (proxy# :: Proxy# (Maybe a))
        in setAddr# addr# (n# *# k#) (I8# 0#) s
      _       ->  setAddrLoop# addr# n# mVal s
  {-# INLINE setAddr# #-}

maxInt# :: Int# -> Int# -> Int#
maxInt# x# y# =
  case x# <# y# of
    0# -> x#
    _  -> y#
{-# INLINE maxInt# #-}

type family MaxOrdering (o :: Ordering) (x :: Nat) (y :: Nat) where
  MaxOrdering 'LT x y = y
  MaxOrdering o  x y = x

type MaxOf (x :: Nat) (y :: Nat) = MaxOrdering (CmpNat x y) x y

instance (Unbox a, Unbox b) => Unbox (Either a b) where
  type UnboxIso (Either a b) = Either a b
  type SizeOf (Either a b) = 1 + MaxOf (SizeOf a) (SizeOf b)
  type Alignment (Either a b) = 1 + MaxOf (Alignment a) (Alignment b)
  sizeOf# _ = 1# +# maxInt# (sizeOf# (proxy# :: Proxy# a)) (sizeOf# (proxy# :: Proxy# b))
  {-# INLINE sizeOf# #-}
  alignment# _ = 1# +# maxInt# (alignment# (proxy# :: Proxy# a)) (alignment# (proxy# :: Proxy# b))
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i# =
    case indexInt8Array# ba# i# of
      0# -> Left (indexByteOffByteArray# ba# (i# +# 1#))
      _  -> Right (indexByteOffByteArray# ba# (i# +# 1#))
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# =
    indexByteOffByteArray# ba# (i# *# sizeOf# (proxy# :: Proxy# (Either a b)))
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# =
    let addr0# = addr# `plusAddr#` (i# *# sizeOf# (proxy# :: Proxy# (Either a b)))
    in case indexInt8OffAddr# addr0# 0# of
      0# -> Left (indexOffAddr# (addr0# `plusAddr#` 1#) 0#)
      _  -> Right (indexOffAddr# (addr0# `plusAddr#` 1#) 0#)
  {-# INLINE indexOffAddr# #-}
  readByteOffMutableByteArray# mba# i# s =
    case readInt8Array# mba# i# s of
      (# s', 0# #) -> case readByteOffMutableByteArray# mba# (i# +# 1#) s' of
                        (# s'', a #) -> (# s'', Left a #)
      (# s', _  #) -> case readByteOffMutableByteArray# mba# (i# +# 1#) s' of
                        (# s'', a #) -> (# s'', Right a #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readMutableByteArray# mba# i# =
    let i0# = i# *# sizeOf# (proxy# :: Proxy# (Either a b))
     in readByteOffMutableByteArray# mba# i0#
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# addr# i# s =
    let addr0# = addr# `plusAddr#` (i# *# sizeOf# (proxy# :: Proxy# (Either a b)))
    in case readInt8OffAddr# addr0# 0# s of
         (# s', 0# #) -> case readOffAddr# (addr0# `plusAddr#` 1#) 0# s' of
                           (# s'', a #) -> (# s'', Left a #)
         (# s', _  #) -> case readOffAddr# (addr0# `plusAddr#` 1#) 0# s' of
                           (# s'', a #) -> (# s'', Right a #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i# eVal s =
    let a# = sizeOf# (proxy# :: Proxy# a)
        b# = sizeOf# (proxy# :: Proxy# b)
        i1# = i# +# 1#
    in case eVal of
         Left a -> -- TODO: Optimize duplication away
           setByteArray# mba# (i1# +# a#) (maxInt# 0# (b# -# a#)) 0#
           (writeByteOffMutableByteArray# mba# i1# a (writeInt8Array# mba# i# 0# s))
         Right b ->
           setByteArray# mba# (i1# +# b#) (maxInt# 0# (a# -# b#)) 0#
           (writeByteOffMutableByteArray# mba# i1# b (writeInt8Array# mba# i# 1# s))
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# eVal s =
    let k# = sizeOf# (proxy# :: Proxy# (Either a b))
        i0# = i# *# k#
    in writeByteOffMutableByteArray# mba# i0# eVal s
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# addr# i# eVal s =
    let a# = sizeOf# (proxy# :: Proxy# a)
        b# = sizeOf# (proxy# :: Proxy# b)
        k# = sizeOf# (proxy# :: Proxy# (Either a b))
        addr0# = addr# `plusAddr#` (i# *# k#)
        addr1# = addr0# `plusAddr#` 1#
    in case eVal of
         Left a  ->
           setOffAddr# addr1# a# (maxInt# 0# (b# -# a#)) (I8# 0#)
           (writeOffAddr# addr1# 0# a (writeInt8OffAddr# addr0# 0# 0# s))
         Right b ->
           setOffAddr# addr1# b# (maxInt# 0# (a# -# b#)) (I8# 0#)
           (writeOffAddr# addr1# 0# b (writeInt8OffAddr# addr0# 0# 1# s))
  {-# INLINE writeOffAddr# #-}
  setByteOffMutableByteArray# = setByteOffMutableByteArrayLoop#
  {-# INLINE setByteOffMutableByteArray# #-}
  setAddr# = setAddrLoop#
  {-# INLINE setAddr# #-}
