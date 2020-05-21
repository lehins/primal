{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Prim.Array.Smart.Rep
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Array.Smart.Rep
  ( SmartRep
  , AtomicRep
  ) where

#include "MachDeps.h"
#include "HsBaseConfig.h"

import Data.Prim
import Data.Prim.Class
import Data.Kind
import Data.Prim.Array.Unboxed
import Data.Prim.Array.Unboxed.Ragged
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
  BestRep def (RaggedMArray 0) r2               = def
  BestRep def r1               (RaggedMArray 0) = def
  BestRep def r1               UnboxedMArray    = r1
  BestRep def r1               r2               = r2


type family SmartRep def e :: Type -> Type -> Type where
  SmartRep def () = UnboxedMArray
  SmartRep def (a :~: b) = UnboxedMArray
  SmartRep def Bool = UnboxedMArray
  SmartRep def Char = UnboxedMArray
  SmartRep def Int = UnboxedMArray
  SmartRep def Int8 = UnboxedMArray
  SmartRep def Int16 = UnboxedMArray
  SmartRep def Int32 = UnboxedMArray
  SmartRep def Int64 = UnboxedMArray
  SmartRep def Word = UnboxedMArray
  SmartRep def Word8 = UnboxedMArray
  SmartRep def Word16 = UnboxedMArray
  SmartRep def Word32 = UnboxedMArray
  SmartRep def Word64 = UnboxedMArray
  SmartRep def Float = UnboxedMArray
  SmartRep def Double = UnboxedMArray
  SmartRep def IntPtr = UnboxedMArray
  SmartRep def WordPtr = UnboxedMArray
  SmartRep def (Ptr a) = UnboxedMArray
  SmartRep def (FunPtr a) = UnboxedMArray
  SmartRep def (StablePtr a) = UnboxedMArray
  SmartRep def CBool = UnboxedMArray
  SmartRep def CChar = UnboxedMArray
  SmartRep def CSChar = UnboxedMArray
  SmartRep def CUChar = UnboxedMArray
  SmartRep def CShort = UnboxedMArray
  SmartRep def CUShort = UnboxedMArray
  SmartRep def CInt = UnboxedMArray
  SmartRep def CUInt = UnboxedMArray
  SmartRep def CLong = UnboxedMArray
  SmartRep def CULong = UnboxedMArray
  SmartRep def CLLong = UnboxedMArray
  SmartRep def CULLong = UnboxedMArray
  SmartRep def CPtrdiff = UnboxedMArray
  SmartRep def CSize = UnboxedMArray
  SmartRep def CWchar = UnboxedMArray
  SmartRep def CSigAtomic = UnboxedMArray
  SmartRep def CIntPtr = UnboxedMArray
  SmartRep def CUIntPtr = UnboxedMArray
  SmartRep def CFloat = UnboxedMArray
  SmartRep def CDouble = UnboxedMArray
  SmartRep def Fd = UnboxedMArray
  SmartRep def Errno = UnboxedMArray
#if defined(HTYPE_DEV_T)
  SmartRep def CDev = UnboxedMArray
#endif
#if defined(HTYPE_INO_T)
  SmartRep def CIno = UnboxedMArray
#endif
#if defined(HTYPE_MODE_T)
  SmartRep def CMode = UnboxedMArray
#endif
#if defined(HTYPE_OFF_T)
  SmartRep def COff = UnboxedMArray
#endif
#if defined(HTYPE_PID_T)
  SmartRep def CPid = UnboxedMArray
#endif
#if defined(HTYPE_SSIZE_T)
  SmartRep def CSsize = UnboxedMArray
#endif
#if defined(HTYPE_GID_T)
  SmartRep def CGid = UnboxedMArray
#endif
#if defined(HTYPE_NLINK_T)
  SmartRep def CNlink = UnboxedMArray
#endif
#if defined(HTYPE_UID_T)
  SmartRep def CUid = UnboxedMArray
#endif
#if defined(HTYPE_CC_T)
  SmartRep def CCc = UnboxedMArray
#endif
#if defined(HTYPE_SPEED_T)
  SmartRep def CSpeed = UnboxedMArray
#endif
#if defined(HTYPE_TCFLAG_T)
  SmartRep def CTcflag = UnboxedMArray
#endif
#if defined(HTYPE_RLIM_T)
  SmartRep def CRLim = UnboxedMArray
#endif
#if defined(HTYPE_BLKSIZE_T)
  SmartRep def CBlkSize = UnboxedMArray
#endif
#if defined(HTYPE_BLKCNT_T)
  SmartRep def CBlkCnt = UnboxedMArray
#endif
#if defined(HTYPE_CLOCKID_T)
  SmartRep def CClockId = UnboxedMArray
#endif
#if defined(HTYPE_FSBLKCNT_T)
  SmartRep def CFsBlkCnt = UnboxedMArray
#endif
#if defined(HTYPE_FSFILCNT_T)
  SmartRep def CFsFilCnt = UnboxedMArray
#endif
#if defined(HTYPE_ID_T)
  SmartRep def CId = UnboxedMArray
#endif
#if defined(HTYPE_KEY_T)
  SmartRep def CKey = UnboxedMArray
#endif
#if defined(HTYPE_TIMER_T)
  SmartRep def CTimer = UnboxedMArray
#endif
#if defined(HTYPE_SOCKLEN_T)
  SmartRep def CSocklen = UnboxedMArray
#endif
#if defined(HTYPE_NFDS_T)
  SmartRep def CNfds = UnboxedMArray
#endif
#if __GLASGOW_HASKELL__ >= 802
  SmartRep def (a :~~: b) = UnboxedMArray
#if __GLASGOW_HASKELL__ >= 806
  SmartRep def (Ap f a) = SmartRep def (f a)
#endif /* __GLASGOW_HASKELL__ >= 806 */
#endif /* __GLASGOW_HASKELL__ >= 802 */
  SmartRep def (Max a) = SmartRep def a
  SmartRep def (Min a) = SmartRep def a
  SmartRep def (Data.Semigroup.First a) = SmartRep def a
  SmartRep def (Data.Semigroup.Last a) = SmartRep def a
  SmartRep def (Arg a b) = BestRep def (SmartRep def a) (SmartRep def b)
  SmartRep def (Const a b) = SmartRep def a
  SmartRep def (Compose f g a) = SmartRep def (f (g a))
  SmartRep def (Identity a) = SmartRep def a
  SmartRep def (Alt f a) = SmartRep def (f a)
  SmartRep def Ordering = UnboxedMArray
  SmartRep def IODeviceType = UnboxedMArray
  SmartRep def SeekMode = UnboxedMArray
  SmartRep def BlockReason = UnboxedMArray
  SmartRep def (Down a) = SmartRep def a
  SmartRep def (Dual a) = SmartRep def a
  SmartRep def (Sum a) = SmartRep def a
  SmartRep def (Product a) = SmartRep def a
  SmartRep def All = UnboxedMArray
  SmartRep def Any = UnboxedMArray
  SmartRep def Fingerprint = UnboxedMArray
  SmartRep def (Ratio a) = SmartRep def a
  SmartRep def (Complex a) = SmartRep def a
  SmartRep def (Maybe a) = SmartRep def a
  SmartRep def (Either a b) =
    BestRep def (SmartRep def a) (SmartRep def b)
  SmartRep def (a, b) =
    BestRep def (SmartRep def a) (SmartRep def b)
  SmartRep def (a, b, c) =
    BestRep def (BestRep def (SmartRep def a) (SmartRep def b)) (SmartRep def c)
  SmartRep def (a, b, c, d) =
    BestRep def (SmartRep def (a, b)) (SmartRep def (c, d))
  SmartRep def (a, b, c, d, e) =
    BestRep def (SmartRep def (a, b, c)) (SmartRep def (d, e))
  SmartRep def (a, b, c, d, e, f) =
    BestRep def (SmartRep def (a, b, c)) (SmartRep def (d, e, f))
  SmartRep def (a, b, c, d, e, f, g) =
    BestRep def (SmartRep def (a, b, c, d)) (SmartRep def (e, f, g))
  SmartRep def (a, b, c, d, e, f, g, h) =
    BestRep def (SmartRep def (a, b, c, d)) (SmartRep def (e, f, g, h))
  SmartRep def (a, b, c, d, e, f, g, h, i) =
    BestRep def (SmartRep def (a, b, c, d, e)) (SmartRep def (f, g, h, i))
  SmartRep def (UnboxedArray a) = RaggedMArray 0
  SmartRep def a = def


type family AtomicRep def e :: Type -> Type -> Type where
  AtomicRep def Bool = UnboxedMArray
  AtomicRep def Char = UnboxedMArray
  AtomicRep def Int = UnboxedMArray
  AtomicRep def Int8 = UnboxedMArray
  AtomicRep def Int16 = UnboxedMArray
  AtomicRep def Int32 = UnboxedMArray
  AtomicRep def Word = UnboxedMArray
  AtomicRep def Word8 = UnboxedMArray
  AtomicRep def Word16 = UnboxedMArray
  AtomicRep def Word32 = UnboxedMArray
#if WORD_SIZE_IN_BITS == 64
  AtomicRep def Int64 = UnboxedMArray
  AtomicRep def Word64 = UnboxedMArray
  AtomicRep def CLLong = UnboxedMArray
  AtomicRep def CULLong = UnboxedMArray
#endif
  AtomicRep def IntPtr = UnboxedMArray
  AtomicRep def WordPtr = UnboxedMArray
  AtomicRep def (Ptr a) = UnboxedMArray
  AtomicRep def (FunPtr a) = UnboxedMArray
  AtomicRep def (StablePtr a) = UnboxedMArray
  AtomicRep def CBool = UnboxedMArray
  AtomicRep def CChar = UnboxedMArray
  AtomicRep def CSChar = UnboxedMArray
  AtomicRep def CUChar = UnboxedMArray
  AtomicRep def CShort = UnboxedMArray
  AtomicRep def CUShort = UnboxedMArray
  AtomicRep def CInt = UnboxedMArray
  AtomicRep def CUInt = UnboxedMArray
  AtomicRep def CLong = UnboxedMArray
  AtomicRep def CULong = UnboxedMArray
  AtomicRep def CPtrdiff = UnboxedMArray
  AtomicRep def CSize = UnboxedMArray
  AtomicRep def CWchar = UnboxedMArray
  AtomicRep def CSigAtomic = UnboxedMArray
  AtomicRep def CIntPtr = UnboxedMArray
  AtomicRep def CUIntPtr = UnboxedMArray
  AtomicRep def Fd = UnboxedMArray
  AtomicRep def Errno = UnboxedMArray
#if defined(HTYPE_DEV_T)
  AtomicRep def CDev = UnboxedMArray
#endif
#if defined(HTYPE_INO_T)
  AtomicRep def CIno = UnboxedMArray
#endif
#if defined(HTYPE_MODE_T)
  AtomicRep def CMode = UnboxedMArray
#endif
#if defined(HTYPE_OFF_T)
  AtomicRep def COff = UnboxedMArray
#endif
#if defined(HTYPE_PID_T)
  AtomicRep def CPid = UnboxedMArray
#endif
#if defined(HTYPE_SSIZE_T)
  AtomicRep def CSsize = UnboxedMArray
#endif
#if defined(HTYPE_GID_T)
  AtomicRep def CGid = UnboxedMArray
#endif
#if defined(HTYPE_NLINK_T)
  AtomicRep def CNlink = UnboxedMArray
#endif
#if defined(HTYPE_UID_T)
  AtomicRep def CUid = UnboxedMArray
#endif
#if defined(HTYPE_CC_T)
  AtomicRep def CCc = UnboxedMArray
#endif
#if defined(HTYPE_SPEED_T)
  AtomicRep def CSpeed = UnboxedMArray
#endif
#if defined(HTYPE_TCFLAG_T)
  AtomicRep def CTcflag = UnboxedMArray
#endif
#if defined(HTYPE_RLIM_T)
  AtomicRep def CRLim = UnboxedMArray
#endif
#if defined(HTYPE_BLKSIZE_T)
  AtomicRep def CBlkSize = UnboxedMArray
#endif
#if defined(HTYPE_BLKCNT_T)
  AtomicRep def CBlkCnt = UnboxedMArray
#endif
#if defined(HTYPE_CLOCKID_T)
  AtomicRep def CClockId = UnboxedMArray
#endif
#if defined(HTYPE_FSBLKCNT_T)
  AtomicRep def CFsBlkCnt = UnboxedMArray
#endif
#if defined(HTYPE_FSFILCNT_T)
  AtomicRep def CFsFilCnt = UnboxedMArray
#endif
#if defined(HTYPE_ID_T)
  AtomicRep def CId = UnboxedMArray
#endif
#if defined(HTYPE_KEY_T)
  AtomicRep def CKey = UnboxedMArray
#endif
#if defined(HTYPE_TIMER_T)
  AtomicRep def CTimer = UnboxedMArray
#endif
#if defined(HTYPE_SOCKLEN_T)
  AtomicRep def CSocklen = UnboxedMArray
#endif
#if defined(HTYPE_NFDS_T)
  AtomicRep def CNfds = UnboxedMArray
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
  AtomicRep def Ordering = UnboxedMArray
  AtomicRep def IODeviceType = UnboxedMArray
  AtomicRep def SeekMode = UnboxedMArray
  AtomicRep def BlockReason = UnboxedMArray
  AtomicRep def (Down a) = AtomicRep def a
  AtomicRep def (Dual a) = AtomicRep def a
  AtomicRep def (Sum a) = AtomicRep def a
  AtomicRep def (Product a) = AtomicRep def a
  AtomicRep def All = UnboxedMArray
  AtomicRep def Any = UnboxedMArray
  AtomicRep def a = def
