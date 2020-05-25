{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Prim.Adaptive.Rep
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Adaptive.Rep
  ( AdaptRep
  , AtomicRep
  ) where

#include "MachDeps.h"
#include "HsBaseConfig.h"

import Data.Prim
import Data.Prim.Class
import Data.Kind
import Data.Prim.MArray.Unboxed
import Data.Prim.MArray.Unboxed.Ragged
import Foreign.Prim hiding (Any)
import Foreign.Prim.StablePtr

import Data.Complex
import Data.Type.Equality
import Foreign.C.Error (Errno(..))
import GHC.Conc
import GHC.Real
import GHC.IO.Device
import GHC.Fingerprint.Type
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Functor.Const
import Data.Semigroup


type family BestRep def r1 r2 where
  BestRep def (MRArray 0) r2               = def
  BestRep def r1               (MRArray 0) = def
  BestRep def r1               MUArray    = r1
  BestRep def r1               r2               = r2


type family AdaptRep def e :: Type -> Type -> Type where
  AdaptRep def () = MUArray
  AdaptRep def (a :~: b) = MUArray
  AdaptRep def Bool = MUArray
  AdaptRep def Char = MUArray
  AdaptRep def Int = MUArray
  AdaptRep def Int8 = MUArray
  AdaptRep def Int16 = MUArray
  AdaptRep def Int32 = MUArray
  AdaptRep def Int64 = MUArray
  AdaptRep def Word = MUArray
  AdaptRep def Word8 = MUArray
  AdaptRep def Word16 = MUArray
  AdaptRep def Word32 = MUArray
  AdaptRep def Word64 = MUArray
  AdaptRep def Float = MUArray
  AdaptRep def Double = MUArray
  AdaptRep def IntPtr = MUArray
  AdaptRep def WordPtr = MUArray
  AdaptRep def (Ptr a) = MUArray
  AdaptRep def (FunPtr a) = MUArray
  AdaptRep def (StablePtr a) = MUArray
  AdaptRep def CBool = MUArray
  AdaptRep def CChar = MUArray
  AdaptRep def CSChar = MUArray
  AdaptRep def CUChar = MUArray
  AdaptRep def CShort = MUArray
  AdaptRep def CUShort = MUArray
  AdaptRep def CInt = MUArray
  AdaptRep def CUInt = MUArray
  AdaptRep def CLong = MUArray
  AdaptRep def CULong = MUArray
  AdaptRep def CLLong = MUArray
  AdaptRep def CULLong = MUArray
  AdaptRep def CPtrdiff = MUArray
  AdaptRep def CSize = MUArray
  AdaptRep def CWchar = MUArray
  AdaptRep def CSigAtomic = MUArray
  AdaptRep def CIntPtr = MUArray
  AdaptRep def CUIntPtr = MUArray
  AdaptRep def CFloat = MUArray
  AdaptRep def CDouble = MUArray
  AdaptRep def Fd = MUArray
  AdaptRep def Errno = MUArray
#if defined(HTYPE_DEV_T)
  AdaptRep def CDev = MUArray
#endif
#if defined(HTYPE_INO_T)
  AdaptRep def CIno = MUArray
#endif
#if defined(HTYPE_MODE_T)
  AdaptRep def CMode = MUArray
#endif
#if defined(HTYPE_OFF_T)
  AdaptRep def COff = MUArray
#endif
#if defined(HTYPE_PID_T)
  AdaptRep def CPid = MUArray
#endif
#if defined(HTYPE_SSIZE_T)
  AdaptRep def CSsize = MUArray
#endif
#if defined(HTYPE_GID_T)
  AdaptRep def CGid = MUArray
#endif
#if defined(HTYPE_NLINK_T)
  AdaptRep def CNlink = MUArray
#endif
#if defined(HTYPE_UID_T)
  AdaptRep def CUid = MUArray
#endif
#if defined(HTYPE_CC_T)
  AdaptRep def CCc = MUArray
#endif
#if defined(HTYPE_SPEED_T)
  AdaptRep def CSpeed = MUArray
#endif
#if defined(HTYPE_TCFLAG_T)
  AdaptRep def CTcflag = MUArray
#endif
#if defined(HTYPE_RLIM_T)
  AdaptRep def CRLim = MUArray
#endif
#if defined(HTYPE_BLKSIZE_T)
  AdaptRep def CBlkSize = MUArray
#endif
#if defined(HTYPE_BLKCNT_T)
  AdaptRep def CBlkCnt = MUArray
#endif
#if defined(HTYPE_CLOCKID_T)
  AdaptRep def CClockId = MUArray
#endif
#if defined(HTYPE_FSBLKCNT_T)
  AdaptRep def CFsBlkCnt = MUArray
#endif
#if defined(HTYPE_FSFILCNT_T)
  AdaptRep def CFsFilCnt = MUArray
#endif
#if defined(HTYPE_ID_T)
  AdaptRep def CId = MUArray
#endif
#if defined(HTYPE_KEY_T)
  AdaptRep def CKey = MUArray
#endif
#if defined(HTYPE_TIMER_T)
  AdaptRep def CTimer = MUArray
#endif
#if defined(HTYPE_SOCKLEN_T)
  AdaptRep def CSocklen = MUArray
#endif
#if defined(HTYPE_NFDS_T)
  AdaptRep def CNfds = MUArray
#endif
#if __GLASGOW_HASKELL__ >= 802
  AdaptRep def (a :~~: b) = MUArray
#if __GLASGOW_HASKELL__ >= 806
  AdaptRep def (Ap f a) = AdaptRep def (f a)
#endif /* __GLASGOW_HASKELL__ >= 806 */
#endif /* __GLASGOW_HASKELL__ >= 802 */
  AdaptRep def (Max a) = AdaptRep def a
  AdaptRep def (Min a) = AdaptRep def a
  AdaptRep def (Data.Semigroup.First a) = AdaptRep def a
  AdaptRep def (Data.Semigroup.Last a) = AdaptRep def a
  AdaptRep def (Arg a b) = BestRep def (AdaptRep def a) (AdaptRep def b)
  AdaptRep def (Const a b) = AdaptRep def a
  AdaptRep def (Compose f g a) = AdaptRep def (f (g a))
  AdaptRep def (Identity a) = AdaptRep def a
  AdaptRep def (Alt f a) = AdaptRep def (f a)
  AdaptRep def Ordering = MUArray
  AdaptRep def IODeviceType = MUArray
  AdaptRep def SeekMode = MUArray
  AdaptRep def BlockReason = MUArray
  AdaptRep def (Down a) = AdaptRep def a
  AdaptRep def (Dual a) = AdaptRep def a
  AdaptRep def (Sum a) = AdaptRep def a
  AdaptRep def (Product a) = AdaptRep def a
  AdaptRep def All = MUArray
  AdaptRep def Any = MUArray
  AdaptRep def Fingerprint = MUArray
  AdaptRep def (Ratio a) = AdaptRep def a
  AdaptRep def (Complex a) = AdaptRep def a
  AdaptRep def (Maybe a) = AdaptRep def a
  AdaptRep def (Either a b) =
    BestRep def (AdaptRep def a) (AdaptRep def b)
  AdaptRep def (a, b) =
    BestRep def (AdaptRep def a) (AdaptRep def b)
  AdaptRep def (a, b, c) =
    BestRep def (BestRep def (AdaptRep def a) (AdaptRep def b)) (AdaptRep def c)
  AdaptRep def (a, b, c, d) =
    BestRep def (AdaptRep def (a, b)) (AdaptRep def (c, d))
  AdaptRep def (a, b, c, d, e) =
    BestRep def (AdaptRep def (a, b, c)) (AdaptRep def (d, e))
  AdaptRep def (a, b, c, d, e, f) =
    BestRep def (AdaptRep def (a, b, c)) (AdaptRep def (d, e, f))
  AdaptRep def (a, b, c, d, e, f, g) =
    BestRep def (AdaptRep def (a, b, c, d)) (AdaptRep def (e, f, g))
  AdaptRep def (a, b, c, d, e, f, g, h) =
    BestRep def (AdaptRep def (a, b, c, d)) (AdaptRep def (e, f, g, h))
  AdaptRep def (a, b, c, d, e, f, g, h, i) =
    BestRep def (AdaptRep def (a, b, c, d, e)) (AdaptRep def (f, g, h, i))
  AdaptRep def (UArray a) = MRArray 0
  AdaptRep def a = def


type family AtomicRep def e :: Type -> Type -> Type where
  AtomicRep def Bool = MUArray
  AtomicRep def Char = MUArray
  AtomicRep def Int = MUArray
  AtomicRep def Int8 = MUArray
  AtomicRep def Int16 = MUArray
  AtomicRep def Int32 = MUArray
  AtomicRep def Word = MUArray
  AtomicRep def Word8 = MUArray
  AtomicRep def Word16 = MUArray
  AtomicRep def Word32 = MUArray
#if WORD_SIZE_IN_BITS == 64
  AtomicRep def Int64 = MUArray
  AtomicRep def Word64 = MUArray
  AtomicRep def CLLong = MUArray
  AtomicRep def CULLong = MUArray
#endif
  AtomicRep def IntPtr = MUArray
  AtomicRep def WordPtr = MUArray
  AtomicRep def (Ptr a) = MUArray
  AtomicRep def (FunPtr a) = MUArray
  AtomicRep def (StablePtr a) = MUArray
  AtomicRep def CBool = MUArray
  AtomicRep def CChar = MUArray
  AtomicRep def CSChar = MUArray
  AtomicRep def CUChar = MUArray
  AtomicRep def CShort = MUArray
  AtomicRep def CUShort = MUArray
  AtomicRep def CInt = MUArray
  AtomicRep def CUInt = MUArray
  AtomicRep def CLong = MUArray
  AtomicRep def CULong = MUArray
  AtomicRep def CPtrdiff = MUArray
  AtomicRep def CSize = MUArray
  AtomicRep def CWchar = MUArray
  AtomicRep def CSigAtomic = MUArray
  AtomicRep def CIntPtr = MUArray
  AtomicRep def CUIntPtr = MUArray
  AtomicRep def Fd = MUArray
  AtomicRep def Errno = MUArray
#if defined(HTYPE_DEV_T)
  AtomicRep def CDev = MUArray
#endif
#if defined(HTYPE_INO_T)
  AtomicRep def CIno = MUArray
#endif
#if defined(HTYPE_MODE_T)
  AtomicRep def CMode = MUArray
#endif
#if defined(HTYPE_OFF_T)
  AtomicRep def COff = MUArray
#endif
#if defined(HTYPE_PID_T)
  AtomicRep def CPid = MUArray
#endif
#if defined(HTYPE_SSIZE_T)
  AtomicRep def CSsize = MUArray
#endif
#if defined(HTYPE_GID_T)
  AtomicRep def CGid = MUArray
#endif
#if defined(HTYPE_NLINK_T)
  AtomicRep def CNlink = MUArray
#endif
#if defined(HTYPE_UID_T)
  AtomicRep def CUid = MUArray
#endif
#if defined(HTYPE_CC_T)
  AtomicRep def CCc = MUArray
#endif
#if defined(HTYPE_SPEED_T)
  AtomicRep def CSpeed = MUArray
#endif
#if defined(HTYPE_TCFLAG_T)
  AtomicRep def CTcflag = MUArray
#endif
#if defined(HTYPE_RLIM_T)
  AtomicRep def CRLim = MUArray
#endif
#if defined(HTYPE_BLKSIZE_T)
  AtomicRep def CBlkSize = MUArray
#endif
#if defined(HTYPE_BLKCNT_T)
  AtomicRep def CBlkCnt = MUArray
#endif
#if defined(HTYPE_CLOCKID_T)
  AtomicRep def CClockId = MUArray
#endif
#if defined(HTYPE_FSBLKCNT_T)
  AtomicRep def CFsBlkCnt = MUArray
#endif
#if defined(HTYPE_FSFILCNT_T)
  AtomicRep def CFsFilCnt = MUArray
#endif
#if defined(HTYPE_ID_T)
  AtomicRep def CId = MUArray
#endif
#if defined(HTYPE_KEY_T)
  AtomicRep def CKey = MUArray
#endif
#if defined(HTYPE_TIMER_T)
  AtomicRep def CTimer = MUArray
#endif
#if defined(HTYPE_SOCKLEN_T)
  AtomicRep def CSocklen = MUArray
#endif
#if defined(HTYPE_NFDS_T)
  AtomicRep def CNfds = MUArray
#endif
#if __GLASGOW_HASKELL__ >= 806
  AtomicRep def (Ap f a) = AtomicRep def (f a)
#endif /* __GLASGOW_HASKELL__ >= 806 */
  AtomicRep def (Max a) = AtomicRep def a
  AtomicRep def (Min a) = AtomicRep def a
  AtomicRep def (Data.Semigroup.First a) = AtomicRep def a
  AtomicRep def (Data.Semigroup.Last a) = AtomicRep def a
  AtomicRep def (Const a b) = AtomicRep def a
  AtomicRep def (Compose f g a) = AtomicRep def (f (g a))
  AtomicRep def (Identity a) = AtomicRep def a
  AtomicRep def (Alt f a) = AtomicRep def (f a)
  AtomicRep def Ordering = MUArray
  AtomicRep def IODeviceType = MUArray
  AtomicRep def SeekMode = MUArray
  AtomicRep def BlockReason = MUArray
  AtomicRep def (Down a) = AtomicRep def a
  AtomicRep def (Dual a) = AtomicRep def a
  AtomicRep def (Sum a) = AtomicRep def a
  AtomicRep def (Product a) = AtomicRep def a
  AtomicRep def All = MUArray
  AtomicRep def Any = MUArray
  AtomicRep def a = def
