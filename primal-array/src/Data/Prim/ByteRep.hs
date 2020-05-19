{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Prim.ByteRep
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.ByteRep
  ( UnboxRep
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


type family UnboxRep def a :: Type -> Type -> Type where
  UnboxRep def () = UnboxedMArray
  UnboxRep def (a :~: b) = UnboxedMArray
  UnboxRep def Bool = UnboxedMArray
  UnboxRep def Char = UnboxedMArray
  UnboxRep def Int = UnboxedMArray
  UnboxRep def Int8 = UnboxedMArray
  UnboxRep def Int16 = UnboxedMArray
  UnboxRep def Int32 = UnboxedMArray
  UnboxRep def Int64 = UnboxedMArray
  UnboxRep def Word = UnboxedMArray
  UnboxRep def Word8 = UnboxedMArray
  UnboxRep def Word16 = UnboxedMArray
  UnboxRep def Word32 = UnboxedMArray
  UnboxRep def Word64 = UnboxedMArray
  UnboxRep def Float = UnboxedMArray
  UnboxRep def Double = UnboxedMArray
  UnboxRep def IntPtr = UnboxedMArray
  UnboxRep def WordPtr = UnboxedMArray
  UnboxRep def (Ptr a) = UnboxedMArray
  UnboxRep def (FunPtr a) = UnboxedMArray
  UnboxRep def (StablePtr a) = UnboxedMArray
  UnboxRep def CBool = UnboxedMArray
  UnboxRep def CChar = UnboxedMArray
  UnboxRep def CSChar = UnboxedMArray
  UnboxRep def CUChar = UnboxedMArray
  UnboxRep def CShort = UnboxedMArray
  UnboxRep def CUShort = UnboxedMArray
  UnboxRep def CInt = UnboxedMArray
  UnboxRep def CUInt = UnboxedMArray
  UnboxRep def CLong = UnboxedMArray
  UnboxRep def CULong = UnboxedMArray
  UnboxRep def CLLong = UnboxedMArray
  UnboxRep def CULLong = UnboxedMArray
  UnboxRep def CPtrdiff = UnboxedMArray
  UnboxRep def CSize = UnboxedMArray
  UnboxRep def CWchar = UnboxedMArray
  UnboxRep def CSigAtomic = UnboxedMArray
  UnboxRep def CIntPtr = UnboxedMArray
  UnboxRep def CUIntPtr = UnboxedMArray
  UnboxRep def CFloat = UnboxedMArray
  UnboxRep def CDouble = UnboxedMArray
  UnboxRep def Fd = UnboxedMArray
  UnboxRep def Errno = UnboxedMArray
#if defined(HTYPE_DEV_T)
  UnboxRep def CDev = UnboxedMArray
#endif
#if defined(HTYPE_INO_T)
  UnboxRep def CIno = UnboxedMArray
#endif
#if defined(HTYPE_MODE_T)
  UnboxRep def CMode = UnboxedMArray
#endif
#if defined(HTYPE_OFF_T)
  UnboxRep def COff = UnboxedMArray
#endif
#if defined(HTYPE_PID_T)
  UnboxRep def CPid = UnboxedMArray
#endif
#if defined(HTYPE_SSIZE_T)
  UnboxRep def CSsize = UnboxedMArray
#endif
#if defined(HTYPE_GID_T)
  UnboxRep def CGid = UnboxedMArray
#endif
#if defined(HTYPE_NLINK_T)
  UnboxRep def CNlink = UnboxedMArray
#endif
#if defined(HTYPE_UID_T)
  UnboxRep def CUid = UnboxedMArray
#endif
#if defined(HTYPE_CC_T)
  UnboxRep def CCc = UnboxedMArray
#endif
#if defined(HTYPE_SPEED_T)
  UnboxRep def CSpeed = UnboxedMArray
#endif
#if defined(HTYPE_TCFLAG_T)
  UnboxRep def CTcflag = UnboxedMArray
#endif
#if defined(HTYPE_RLIM_T)
  UnboxRep def CRLim = UnboxedMArray
#endif
#if defined(HTYPE_BLKSIZE_T)
  UnboxRep def CBlkSize = UnboxedMArray
#endif
#if defined(HTYPE_BLKCNT_T)
  UnboxRep def CBlkCnt = UnboxedMArray
#endif
#if defined(HTYPE_CLOCKID_T)
  UnboxRep def CClockId = UnboxedMArray
#endif
#if defined(HTYPE_FSBLKCNT_T)
  UnboxRep def CFsBlkCnt = UnboxedMArray
#endif
#if defined(HTYPE_FSFILCNT_T)
  UnboxRep def CFsFilCnt = UnboxedMArray
#endif
#if defined(HTYPE_ID_T)
  UnboxRep def CId = UnboxedMArray
#endif
#if defined(HTYPE_KEY_T)
  UnboxRep def CKey = UnboxedMArray
#endif
#if defined(HTYPE_TIMER_T)
  UnboxRep def CTimer = UnboxedMArray
#endif
#if defined(HTYPE_SOCKLEN_T)
  UnboxRep def CSocklen = UnboxedMArray
#endif
#if defined(HTYPE_NFDS_T)
  UnboxRep def CNfds = UnboxedMArray
#endif
#if __GLASGOW_HASKELL__ >= 802
  UnboxRep def (a :~~: b) = UnboxedMArray
#if __GLASGOW_HASKELL__ >= 806
  UnboxRep def (Ap f a) = UnboxRep def (f a)
#endif /* __GLASGOW_HASKELL__ >= 806 */
#endif /* __GLASGOW_HASKELL__ >= 802 */
  UnboxRep def (Max a) = UnboxRep def a
  UnboxRep def (Min a) = UnboxRep def a
  UnboxRep def (Data.Semigroup.First a) = UnboxRep def a
  UnboxRep def (Data.Semigroup.Last a) = UnboxRep def a
  UnboxRep def (Arg a b) = BestRep def (UnboxRep def a) (UnboxRep def b)
  UnboxRep def (Const a b) = UnboxRep def a
  UnboxRep def (Compose f g a) = UnboxRep def (f (g a))
  UnboxRep def (Identity a) = UnboxRep def a
  UnboxRep def (Alt f a) = UnboxRep def (f a)
  UnboxRep def Ordering = UnboxedMArray
  UnboxRep def IODeviceType = UnboxedMArray
  UnboxRep def SeekMode = UnboxedMArray
  UnboxRep def BlockReason = UnboxedMArray
  UnboxRep def (Down a) = UnboxRep def a
  UnboxRep def (Dual a) = UnboxRep def a
  UnboxRep def (Sum a) = UnboxRep def a
  UnboxRep def (Product a) = UnboxRep def a
  UnboxRep def All = UnboxedMArray
  UnboxRep def Any = UnboxedMArray
  UnboxRep def Fingerprint = UnboxedMArray
  UnboxRep def (Ratio a) = UnboxRep def a
  UnboxRep def (Complex a) = UnboxRep def a
  UnboxRep def (Maybe a) = UnboxRep def a
  UnboxRep def (Either a b) =
    BestRep def (UnboxRep def a) (UnboxRep def b)
  UnboxRep def (a, b) =
    BestRep def (UnboxRep def a) (UnboxRep def b)
  UnboxRep def (a, b, c) =
    BestRep def (BestRep def (UnboxRep def a) (UnboxRep def b)) (UnboxRep def c)
  UnboxRep def (a, b, c, d) =
    BestRep def (UnboxRep def (a, b)) (UnboxRep def (c, d))
  UnboxRep def (a, b, c, d, e) =
    BestRep def (UnboxRep def (a, b, c)) (UnboxRep def (d, e))
  UnboxRep def (a, b, c, d, e, f) =
    BestRep def (UnboxRep def (a, b, c)) (UnboxRep def (d, e, f))
  UnboxRep def (a, b, c, d, e, f, g) =
    BestRep def (UnboxRep def (a, b, c, d)) (UnboxRep def (e, f, g))
  UnboxRep def (a, b, c, d, e, f, g, h) =
    BestRep def (UnboxRep def (a, b, c, d)) (UnboxRep def (e, f, g, h))
  UnboxRep def (a, b, c, d, e, f, g, h, i) =
    BestRep def (UnboxRep def (a, b, c, d, e)) (UnboxRep def (f, g, h, i))
  UnboxRep def (UnboxedArray a) = RaggedMArray 0
  UnboxRep def a = def


type family AtomicRep def a :: Type -> Type -> Type where
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
