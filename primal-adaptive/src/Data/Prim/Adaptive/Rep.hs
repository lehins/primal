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
  , IsAtomic
  , AWrap
  ) where

#include "MachDeps.h"
#include "HsBaseConfig.h"

import Data.Complex
import Data.Functor.Compose
import Data.Functor.Const
import Data.Functor.Identity
import Data.Kind
import Primal.Prim
import Primal.Prim.Class
import Primal.Data.Array
import Data.Prim.MArray.Unboxed.Ragged
import Primal.Memory.PArray
import Primal.Memory.Bytes
import Data.Prim.Unbox
import Data.Semigroup
import Data.Type.Equality
import Foreign.C.Error (Errno(..))
import Primal.Foreign hiding (Any)
import Primal.Foreign.StablePtr
import GHC.Conc
import GHC.Fingerprint.Type
import GHC.IO.Device
import GHC.Real
import GHC.TypeLits

type family BestRep def r1 r2 where
  BestRep def UMArray UMArray = UMArray
  BestRep def      r1      r2 = def

type family AtomicPrimRep def (isAtomic :: Bool) where
  AtomicPrimRep def 'True = UMArray
  AtomicPrimRep def 'False = def

type family AdaptRep def e :: Type -> Type -> Type where
  AdaptRep def () = UMArray
  AdaptRep def (a :~: b) = UMArray
  -- In case we are not on 64bit architecture
  AdaptRep def Int64 = UMArray
  AdaptRep def Word64 = UMArray
  AdaptRep def CLLong = UMArray
  AdaptRep def CULLong = UMArray
  AdaptRep def (Unbox a) = UMArray
#if __GLASGOW_HASKELL__ >= 802
  AdaptRep def (a :~~: b) = UMArray
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
  AdaptRep def (Down a) = AdaptRep def a
  AdaptRep def (Dual a) = AdaptRep def a
  AdaptRep def (Sum a) = AdaptRep def a
  AdaptRep def (Product a) = AdaptRep def a
  AdaptRep def Fingerprint = UMArray
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
  AdaptRep def (UArray a) = RMArray 0
  AdaptRep def (Bytes 'Inc) = RMArray 0
  AdaptRep def (PArray 'Inc a) = RMArray 0
  AdaptRep def (RArray n a) = RMArray (n + 1)
  AdaptRep def a = AtomicPrimRep def (IsBasicAtomic a)




type family IsBasicAtomic e :: Bool where
  IsBasicAtomic Bool = 'True
  IsBasicAtomic Char = 'True
  IsBasicAtomic Int = 'True
  IsBasicAtomic Int8 = 'True
  IsBasicAtomic Int16 = 'True
  IsBasicAtomic Int32 = 'True
  IsBasicAtomic Word = 'True
  IsBasicAtomic Word8 = 'True
  IsBasicAtomic Word16 = 'True
  IsBasicAtomic Word32 = 'True
#if WORD_SIZE_IN_BITS == 64
  IsBasicAtomic Int64 = 'True
  IsBasicAtomic Word64 = 'True
  IsBasicAtomic CLLong = 'True
  IsBasicAtomic CULLong = 'True
#endif
  IsBasicAtomic IntPtr = 'True
  IsBasicAtomic WordPtr = 'True
  IsBasicAtomic (Ptr a) = 'True
  IsBasicAtomic (FunPtr a) = 'True
  IsBasicAtomic (StablePtr a) = 'True
  IsBasicAtomic CBool = 'True
  IsBasicAtomic CChar = 'True
  IsBasicAtomic CSChar = 'True
  IsBasicAtomic CUChar = 'True
  IsBasicAtomic CShort = 'True
  IsBasicAtomic CUShort = 'True
  IsBasicAtomic CInt = 'True
  IsBasicAtomic CUInt = 'True
  IsBasicAtomic CLong = 'True
  IsBasicAtomic CULong = 'True
  IsBasicAtomic CPtrdiff = 'True
  IsBasicAtomic CSize = 'True
  IsBasicAtomic CWchar = 'True
  IsBasicAtomic CSigAtomic = 'True
  IsBasicAtomic CIntPtr = 'True
  IsBasicAtomic CUIntPtr = 'True
  IsBasicAtomic Fd = 'True
  IsBasicAtomic Errno = 'True
#if defined(HTYPE_DEV_T)
  IsBasicAtomic CDev = 'True
#endif
#if defined(HTYPE_INO_T)
  IsBasicAtomic CIno = 'True
#endif
#if defined(HTYPE_MODE_T)
  IsBasicAtomic CMode = 'True
#endif
#if defined(HTYPE_OFF_T)
  IsBasicAtomic COff = 'True
#endif
#if defined(HTYPE_PID_T)
  IsBasicAtomic CPid = 'True
#endif
#if defined(HTYPE_SSIZE_T)
  IsBasicAtomic CSsize = 'True
#endif
#if defined(HTYPE_GID_T)
  IsBasicAtomic CGid = 'True
#endif
#if defined(HTYPE_NLINK_T)
  IsBasicAtomic CNlink = 'True
#endif
#if defined(HTYPE_UID_T)
  IsBasicAtomic CUid = 'True
#endif
#if defined(HTYPE_CC_T)
  IsBasicAtomic CCc = 'True
#endif
#if defined(HTYPE_SPEED_T)
  IsBasicAtomic CSpeed = 'True
#endif
#if defined(HTYPE_TCFLAG_T)
  IsBasicAtomic CTcflag = 'True
#endif
#if defined(HTYPE_RLIM_T)
  IsBasicAtomic CRLim = 'True
#endif
#if defined(HTYPE_BLKSIZE_T)
  IsBasicAtomic CBlkSize = 'True
#endif
#if defined(HTYPE_BLKCNT_T)
  IsBasicAtomic CBlkCnt = 'True
#endif
#if defined(HTYPE_CLOCKID_T)
  IsBasicAtomic CClockId = 'True
#endif
#if defined(HTYPE_FSBLKCNT_T)
  IsBasicAtomic CFsBlkCnt = 'True
#endif
#if defined(HTYPE_FSFILCNT_T)
  IsBasicAtomic CFsFilCnt = 'True
#endif
#if defined(HTYPE_ID_T)
  IsBasicAtomic CId = 'True
#endif
#if defined(HTYPE_KEY_T)
  IsBasicAtomic CKey = 'True
#endif
#if defined(HTYPE_TIMER_T)
  IsBasicAtomic CTimer = 'True
#endif
#if defined(HTYPE_SOCKLEN_T)
  IsBasicAtomic CSocklen = 'True
#endif
#if defined(HTYPE_NFDS_T)
  IsBasicAtomic CNfds = 'True
#endif
  IsBasicAtomic All = 'True
  IsBasicAtomic Any = 'True
  IsBasicAtomic Ordering = 'True
  IsBasicAtomic IODeviceType = 'True
  IsBasicAtomic SeekMode = 'True
  IsBasicAtomic BlockReason = 'True
  IsBasicAtomic a = 'False

type family IsAtomic e :: Bool where
#if __GLASGOW_HASKELL__ >= 806
  IsAtomic (Ap f a) = IsAtomic (f a)
#endif /* __GLASGOW_HASKELL__ >= 806 */
  IsAtomic (Max a) = IsAtomic a
  IsAtomic (Min a) = IsAtomic a
  IsAtomic (Data.Semigroup.First a) = IsAtomic a
  IsAtomic (Data.Semigroup.Last a) = IsAtomic a
  IsAtomic (Const a b) = IsAtomic a
  IsAtomic (Compose f g a) = IsAtomic (f (g a))
  IsAtomic (Identity a) = IsAtomic a
  IsAtomic (Alt f a) = IsAtomic (f a)
  IsAtomic (Down a) = IsAtomic a
  IsAtomic (Dual a) = IsAtomic a
  IsAtomic (Sum a) = IsAtomic a
  IsAtomic (Product a) = IsAtomic a
  IsAtomic a = IsBasicAtomic a

type family AWrap rep (a :: Bool) e :: Type where
  AWrap UMArray 'False e = Atom e
  AWrap rep a e = e
