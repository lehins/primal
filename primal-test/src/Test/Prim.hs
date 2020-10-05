{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Prim
  ( module Test.Prim
  , module Test.Prim.Common
  ) where

import Control.DeepSeq
import Data.Char
import Data.Complex
import Data.Functor.Const
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Prim.Memory.Bytes
import Data.Ratio
import qualified Data.Semigroup as Semigroup
import Foreign.C.Error
import Foreign.Prim hiding (Any)
import Foreign.Prim.Ptr
import Foreign.Prim.StablePtr
import GHC.Conc
import GHC.Fingerprint.Type
import GHC.IO.Device
import System.IO
import Test.Prim.Common

#include "MachDeps.h"
#include "HsBaseConfig.h"

---- Orphans

deriving instance NFData CDev
deriving instance Arbitrary CDev

deriving instance NFData CUid
deriving instance Arbitrary CUid

deriving instance NFData CCc
deriving instance Arbitrary CCc

deriving instance NFData CSpeed
deriving instance Arbitrary CSpeed

deriving instance NFData CMode
deriving instance Arbitrary CMode

deriving instance NFData CTcflag
deriving instance Arbitrary CTcflag

deriving instance NFData COff
deriving instance Arbitrary COff

deriving instance NFData CRLim
deriving instance Arbitrary CRLim

deriving instance NFData CPid
deriving instance Arbitrary CPid

deriving instance NFData CBlkSize
deriving instance Arbitrary CBlkSize

deriving instance NFData CSsize
deriving instance Arbitrary CSsize

deriving instance NFData CGid
deriving instance Arbitrary CGid

deriving instance NFData CNlink
deriving instance Arbitrary CNlink

deriving instance NFData CBlkCnt
deriving instance Arbitrary CBlkCnt

deriving instance NFData CClockId
deriving instance Arbitrary CClockId

deriving instance NFData CFsBlkCnt
deriving instance Arbitrary CFsBlkCnt

deriving instance NFData CFsFilCnt
deriving instance Arbitrary CFsFilCnt

deriving instance NFData CId
deriving instance Arbitrary CId

instance Arbitrary Fingerprint where
  arbitrary = Fingerprint <$> arbitrary <*> arbitrary


instance Arbitrary (Ptr a) where
  arbitrary = intPtrToPtr <$> arbitrary

instance Arbitrary (FunPtr a) where
  arbitrary = castPtrToFunPtr <$> arbitrary

instance Arbitrary (StablePtr a) where
  arbitrary = castPtrToStablePtr <$> arbitrary

instance Arbitrary IntPtr where
  arbitrary = IntPtr <$> arbitrary

instance Arbitrary WordPtr where
  arbitrary = WordPtr <$> arbitrary

deriving instance NFData IntPtr

deriving instance NFData WordPtr

deriving instance Arbitrary CBool

deriving instance Arbitrary a => Arbitrary (Down a)

instance a ~ b => Arbitrary (a :~: b) where
  arbitrary = pure Refl

instance a ~ b => Arbitrary (a :~~: b) where
  arbitrary = pure HRefl

-- Foreign.C.Error

deriving instance Show Errno
deriving instance NFData Errno
instance Arbitrary Errno where
  arbitrary = do
    errno <- Errno <$> arbitrary
    if isValidErrno errno
      then pure errno
      else arbitrary

-- Data.Char

instance NFData GeneralCategory where
  rnf x = x `seq` ()

instance Arbitrary GeneralCategory where
  arbitrary = arbitraryBoundedEnum

-- System.Posix.Types

deriving instance NFData CIno
deriving instance Arbitrary CIno

deriving instance NFData CKey
deriving instance Arbitrary CKey

#if defined(HTYPE_TIMER_T)
deriving instance NFData CTimer
deriving instance Arbitrary CTimer
#endif

deriving instance NFData Fd
deriving instance Arbitrary Fd


-- System.IO

instance NFData BufferMode where
  rnf x = x `seq` ()
instance Arbitrary BufferMode where
  arbitrary =
    oneof [pure NoBuffering, pure LineBuffering, BlockBuffering <$> arbitrary]

instance NFData SeekMode where
  rnf sm = seq sm ()
instance Arbitrary SeekMode where
  arbitrary = elements [toEnum 0 .. ]

instance NFData Newline where
  rnf x = x `seq` ()

deriving instance Enum Newline
deriving instance Bounded Newline
instance Arbitrary Newline where
  arbitrary = arbitraryBoundedEnum

instance NFData NewlineMode where
  rnf (NewlineMode i o) = i `seq` o `seq` ()

instance Arbitrary NewlineMode where
  arbitrary = NewlineMode <$> arbitrary <*> arbitrary

instance NFData IOMode where
  rnf x = x `seq` ()
deriving instance Bounded IOMode
instance Arbitrary IOMode where
  arbitrary = arbitraryBoundedEnum

-- GHC.IO.Device

deriving instance Show IODeviceType
instance NFData IODeviceType where
  rnf x = x `seq` ()
instance Arbitrary IODeviceType where
  arbitrary = elements [Directory, Stream, RegularFile, RawDevice]


-- GHC.Conc


instance NFData BlockReason where
  rnf br = seq br ()
deriving instance Enum BlockReason
deriving instance Bounded BlockReason
instance Arbitrary BlockReason where
  arbitrary = arbitraryBoundedEnum

instance NFData ThreadStatus where
  rnf =
    \case
      ThreadRunning -> ()
      ThreadFinished -> ()
      ThreadBlocked br -> seq br ()
      ThreadDied -> ()
instance Arbitrary ThreadStatus where
  arbitrary =
    oneof
      [ pure ThreadRunning
      , pure ThreadFinished
      , ThreadBlocked <$> arbitrary
      , pure ThreadDied
      ]


-- Data.Monoid
#if __GLASGOW_HASKELL__ >= 806
instance Arbitrary (f a) => Arbitrary (Ap f a) where
  arbitrary = Ap <$> arbitrary
#endif
-- Data.Semigroup

instance Arbitrary a => Arbitrary (Semigroup.First a) where
  arbitrary = Semigroup.First <$> arbitrary

instance Arbitrary a => Arbitrary (Semigroup.Last a) where
  arbitrary = Semigroup.Last <$> arbitrary

instance Arbitrary a => Arbitrary (Semigroup.Min a) where
  arbitrary = Semigroup.Min <$> arbitrary

instance Arbitrary a => Arbitrary (Semigroup.Max a) where
  arbitrary = Semigroup.Max <$> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Semigroup.Arg a b) where
  arbitrary = Semigroup.Arg <$> arbitrary <*> arbitrary


-- Defined in `primal`


instance Arbitrary (Off a) where
  arbitrary = Off . getNonNegative <$> arbitrary

instance Arbitrary (Count a) where
  arbitrary = Count . getNonNegative <$> arbitrary

instance Arbitrary a => Arbitrary (Atom a) where
  arbitrary = (coerce :: Gen a -> Gen (Atom a)) arbitrary


-- When Adding new type, make sure to add the corresponding Arbitrary generator
data APrimType
  = AUnit (Proxy ())
  | ATyEq (Proxy (() :~: ()))
  | AHiKindTyEq (Proxy (Maybe :~~: Maybe))
  | ABool (Proxy Bool)
  | AChar (Proxy Char)
  | ADouble (Proxy Double)
  | AFloat (Proxy Float)
  | AInt (Proxy Int)
  | AInt8 (Proxy Int8)
  | AInt16 (Proxy Int16)
  | AInt32 (Proxy Int32)
  | AInt64 (Proxy Int64)
  | AOrdering (Proxy Ordering)
  | AWord (Proxy Word)
  | AWord8 (Proxy Word8)
  | AWord16 (Proxy Word16)
  | AWord32 (Proxy Word32)
  | AWord64 (Proxy Word64)
  | ABlockReason (Proxy BlockReason)
  | AThreadStatus (Proxy ThreadStatus)
  | ACDev (Proxy CDev)
  | ACIno (Proxy CIno)
  | ACMode (Proxy CMode)
  | ACOff (Proxy COff)
  | ACPid (Proxy CPid)
  | ACSsize (Proxy CSsize)
  | ACGid (Proxy CGid)
  | ACNlink (Proxy CNlink)
  | ACUid (Proxy CUid)
  | ACCc (Proxy CCc)
  | ACSpeed (Proxy CSpeed)
  | ACTcflag (Proxy CTcflag)
  | ACRLim (Proxy CRLim)
  | ACBlkSize (Proxy CBlkSize)
  | ACBlkCnt (Proxy CBlkCnt)
  | ACClockId (Proxy CClockId)
  | ACFsBlkCnt (Proxy CFsBlkCnt)
  | ACFsFilCnt (Proxy CFsFilCnt)
  | ACId (Proxy CId)
  | ACKey (Proxy CKey)
#if defined(HTYPE_TIMER_T)
  | ACTimer (Proxy CTimer)
#endif
  | AFd (Proxy Fd)
  | AErrno (Proxy Errno)
  | ABufferMode (Proxy BufferMode)
  | ANewline (Proxy Newline)
  | ANewlineMode (Proxy NewlineMode)
  | AIODeviceType (Proxy IODeviceType)
  | ASeekMode (Proxy SeekMode)
  | AAll (Proxy All)
  | AAny (Proxy Any)
  | ACChar (Proxy CChar)
  | ACSChar (Proxy CSChar)
  | ACUChar (Proxy CUChar)
  | ACShort (Proxy CShort)
  | ACUShort (Proxy CUShort)
  | ACInt (Proxy CInt)
  | ACUInt (Proxy CUInt)
  | ACLong (Proxy CLong)
  | ACULong (Proxy CULong)
  | ACLLong (Proxy CLLong)
  | ACULLong (Proxy CULLong)
  | ACBool (Proxy CBool)
  | ACFloat (Proxy CFloat)
  | ACDouble (Proxy CDouble)
  | ACPtrdiff (Proxy CPtrdiff)
  | ACSize (Proxy CSize)
  | ACWchar (Proxy CWchar)
  | ACSigAtomic (Proxy CSigAtomic)
  | ACIntPtr (Proxy CIntPtr)
  | ACUIntPtr (Proxy CUIntPtr)
  | ACIntMax (Proxy CIntMax)
  | ACUIntMax (Proxy CUIntMax)
  | AWordPtr (Proxy WordPtr)
  | AIntPtr (Proxy IntPtr)
  | AIOMode (Proxy IOMode)
  | AFingerprint (Proxy Fingerprint)
  | AGeneralCategory (Proxy GeneralCategory)
  | APtr (Proxy (Ptr ()))
  | AFunPtr (Proxy (FunPtr ()))
  | AStablePtr (Proxy (StablePtr ()))
  -- restricted
  | ARatio (Proxy (Ratio Int))
  | AComplex (Proxy (Complex Float))
  -- recursive polymorphic (built-in)
  | AAtom APrimType
  | AOff APrimType
  | ACount APrimType
  -- recursive polymorphic
  | AMaybe APrimType
  | AEither APrimType APrimType
  | AFirst APrimType
  | ALast APrimType
  | AIdentity APrimType
  | ADual APrimType
  | AMin APrimType
  | AMax APrimType
  | ASum APrimType
  | AProduct APrimType
  | ADown APrimType
  | ATuple2 APrimType APrimType
  | ATuple3 APrimType APrimType APrimType
  | ATuple4 APrimType APrimType APrimType APrimType
  | ATuple5 APrimType APrimType APrimType APrimType APrimType
  | ATuple6 APrimType APrimType APrimType APrimType APrimType APrimType
  | ATuple7 APrimType APrimType APrimType APrimType APrimType APrimType APrimType
  | ATuple8 APrimType APrimType APrimType APrimType APrimType APrimType APrimType APrimType
  | ATuple9 APrimType APrimType APrimType APrimType APrimType APrimType APrimType APrimType APrimType
  | AArg APrimType APrimType
  | AConst APrimType
#if __GLASGOW_HASKELL__ >= 806
  | AAp APrimType
#endif
  | AAlt APrimType
  | ACompose APrimType
  deriving Show

instance Arbitrary APrimType where
  arbitrary =
    frequency
      [ (10, pure $ AUnit Proxy)
      , (10, pure $ ATyEq Proxy)
      , (10, pure $ AHiKindTyEq Proxy)
      , (10, pure $ ABool Proxy)
      , (10, pure $ AChar Proxy)
      , (10, pure $ ADouble Proxy)
      , (10, pure $ AFloat Proxy)
      , (10, pure $ AInt Proxy)
      , (10, pure $ AInt8 Proxy)
      , (10, pure $ AInt16 Proxy)
      , (10, pure $ AInt32 Proxy)
      , (10, pure $ AInt64 Proxy)
      , (10, pure $ AOrdering Proxy)
      , (10, pure $ AWord Proxy)
      , (10, pure $ AWord8 Proxy)
      , (10, pure $ AWord16 Proxy)
      , (10, pure $ AWord32 Proxy)
      , (10, pure $ AWord64 Proxy)
      , (10, pure $ ABlockReason Proxy)
      , (10, pure $ AThreadStatus Proxy)
      , (10, pure $ ACDev Proxy)
      , (10, pure $ ACIno Proxy)
      , (10, pure $ ACMode Proxy)
      , (10, pure $ ACOff Proxy)
      , (10, pure $ ACPid Proxy)
      , (10, pure $ ACSsize Proxy)
      , (10, pure $ ACGid Proxy)
      , (10, pure $ ACNlink Proxy)
      , (10, pure $ ACUid Proxy)
      , (10, pure $ ACCc Proxy)
      , (10, pure $ ACSpeed Proxy)
      , (10, pure $ ACTcflag Proxy)
      , (10, pure $ ACRLim Proxy)
      , (10, pure $ ACBlkSize Proxy)
      , (10, pure $ ACBlkCnt Proxy)
      , (10, pure $ ACClockId Proxy)
      , (10, pure $ ACFsBlkCnt Proxy)
      , (10, pure $ ACFsFilCnt Proxy)
      , (10, pure $ ACId Proxy)
      , (10, pure $ ACKey Proxy)
#if defined(HTYPE_TIMER_T)
      , (10, pure $ ACTimer Proxy)
#endif
      , (10, pure $ AFd Proxy)
      , (10, pure $ AErrno Proxy)
      , (10, pure $ ABufferMode Proxy)
      , (10, pure $ ANewline Proxy)
      , (10, pure $ ANewlineMode Proxy)
      , (10, pure $ AIODeviceType Proxy)
      , (10, pure $ ASeekMode Proxy)
      , (10, pure $ AAll Proxy)
      , (10, pure $ AAny Proxy)
      , (10, pure $ ACChar Proxy)
      , (10, pure $ ACSChar Proxy)
      , (10, pure $ ACUChar Proxy)
      , (10, pure $ ACShort Proxy)
      , (10, pure $ ACUShort Proxy)
      , (10, pure $ ACInt Proxy)
      , (10, pure $ ACUInt Proxy)
      , (10, pure $ ACLong Proxy)
      , (10, pure $ ACULong Proxy)
      , (10, pure $ ACLLong Proxy)
      , (10, pure $ ACULLong Proxy)
      , (10, pure $ ACBool Proxy)
      , (10, pure $ ACFloat Proxy)
      , (10, pure $ ACDouble Proxy)
      , (10, pure $ ACPtrdiff Proxy)
      , (10, pure $ ACSize Proxy)
      , (10, pure $ ACWchar Proxy)
      , (10, pure $ ACSigAtomic Proxy)
      , (10, pure $ ACIntPtr Proxy)
      , (10, pure $ ACUIntPtr Proxy)
      , (10, pure $ ACIntMax Proxy)
      , (10, pure $ ACUIntMax Proxy)
      , (10, pure $ AWordPtr Proxy)
      , (10, pure $ AIntPtr Proxy)
      , (10, pure $ AIOMode Proxy)
      , (10, pure $ AFingerprint Proxy)
      , (10, pure $ AGeneralCategory Proxy)
      , (10, pure $ APtr Proxy)
      , (10, pure $ AFunPtr Proxy)
      , (10, pure $ AStablePtr Proxy)
      , (10, pure $ ARatio Proxy)
      , (10, pure $ AComplex Proxy)
      , (1, AAtom <$> arbitrary)
      , (1, AOff <$> arbitrary)
      , (1, ACount <$> arbitrary)
      , (1, AMaybe <$> arbitrary)
      , (1, AEither <$> arbitrary <*> arbitrary)
      , (1, AFirst <$> arbitrary)
      , (1, ALast <$> arbitrary)
      , (1, AIdentity <$> arbitrary)
      , (1, ADual <$> arbitrary)
      , (1, AMin <$> arbitrary)
      , (1, AMax <$> arbitrary)
      , (1, ASum <$> arbitrary)
      , (1, AProduct <$> arbitrary)
      , (1, ADown <$> arbitrary)
      , (1, ATuple2 <$> arbitrary <*> arbitrary)
      , (1, ATuple3 <$> arbitrary <*> arbitrary <*> arbitrary)
      , (1, ATuple4 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary)
      , ( 1
        , ATuple5 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*>
          arbitrary)
      , ( 1
        , ATuple6 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*>
          arbitrary <*>
          arbitrary)
      , ( 1
        , ATuple7 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*>
          arbitrary <*>
          arbitrary <*>
          arbitrary)
      , ( 1
        , ATuple8 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*>
          arbitrary <*>
          arbitrary <*>
          arbitrary <*>
          arbitrary)
      , ( 1
        , ATuple9 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*>
          arbitrary <*>
          arbitrary <*>
          arbitrary <*>
          arbitrary <*>
          arbitrary)
      , (1, AArg <$> arbitrary <*> arbitrary)
      , (1, AConst <$> arbitrary)
#if __GLASGOW_HASKELL__ >= 806
      , (1, AAp <$> arbitrary)
#endif
      , (1, AAlt <$> arbitrary)
      , (1, ACompose <$> arbitrary)
      ]

withAPrimType ::
     APrimType
  -> (forall e. (Prim e, Arbitrary e, Show e, Eq e, Typeable e) => Proxy e -> a)
  -> a
withAPrimType ty f =
  case ty of
    AUnit px -> f px
    ATyEq px -> f px
    AHiKindTyEq px -> f px
    ABool px -> f px
    AChar px -> f px
    ADouble px -> f px
    AFloat px -> f px
    AInt px -> f px
    AInt8 px -> f px
    AInt16 px -> f px
    AInt32 px -> f px
    AInt64 px -> f px
    AOrdering px -> f px
    AWord px -> f px
    AWord8 px -> f px
    AWord16 px -> f px
    AWord32 px -> f px
    AWord64 px -> f px
    ABlockReason px -> f px
    AThreadStatus px -> f px
    ACDev px -> f px
    ACIno px -> f px
    ACMode px -> f px
    ACOff px -> f px
    ACPid px -> f px
    ACSsize px -> f px
    ACGid px -> f px
    ACNlink px -> f px
    ACUid px -> f px
    ACCc px -> f px
    ACSpeed px -> f px
    ACTcflag px -> f px
    ACRLim px -> f px
    ACBlkSize px -> f px
    ACBlkCnt px -> f px
    ACClockId px -> f px
    ACFsBlkCnt px -> f px
    ACFsFilCnt px -> f px
    ACId px -> f px
    ACKey px -> f px
    ACTimer px -> f px
    AFd px -> f px
    AErrno px -> f px
    ABufferMode px -> f px
    ANewline px -> f px
    ANewlineMode px -> f px
    AIODeviceType px -> f px
    ASeekMode px -> f px
    AAll px -> f px
    AAny px -> f px
    ACChar px -> f px
    ACSChar px -> f px
    ACUChar px -> f px
    ACShort px -> f px
    ACUShort px -> f px
    ACInt px -> f px
    ACUInt px -> f px
    ACLong px -> f px
    ACULong px -> f px
    ACLLong px -> f px
    ACULLong px -> f px
    ACBool px -> f px
    ACFloat px -> f px
    ACDouble px -> f px
    ACPtrdiff px -> f px
    ACSize px -> f px
    ACWchar px -> f px
    ACSigAtomic px -> f px
    ACIntPtr px -> f px
    ACUIntPtr px -> f px
    ACIntMax px -> f px
    ACUIntMax px -> f px
    AWordPtr px -> f px
    AIntPtr px -> f px
    AIOMode px -> f px
    AFingerprint px -> f px
    AGeneralCategory px -> f px
    APtr px -> f px
    AFunPtr px -> f px
    AStablePtr px -> f px
    ARatio px -> f px
    AComplex px -> f px
    AAtom t -> withAPrimType t $ \(_ :: Proxy t) -> f (Proxy :: Proxy (Atom t))
    AOff t -> withAPrimType t $ \(_ :: Proxy t) -> f (Proxy :: Proxy (Off t))
    ACount t ->
      withAPrimType t $ \(_ :: Proxy t) -> f (Proxy :: Proxy (Count t))
    AMaybe t ->
      withAPrimType t $ \(_ :: Proxy t) -> f (Proxy :: Proxy (Maybe t))
    AEither t1 t2 ->
      withAPrimType t1 $ \(_ :: Proxy t1) ->
        withAPrimType t2 $ \(_ :: Proxy t2) -> f (Proxy :: Proxy (Either t1 t2))
    AFirst t ->
      withAPrimType t $ \(_ :: Proxy t) -> f (Proxy :: Proxy (First t))
    ALast t -> withAPrimType t $ \(_ :: Proxy t) -> f (Proxy :: Proxy (Last t))
    AIdentity t ->
      withAPrimType t $ \(_ :: Proxy t) -> f (Proxy :: Proxy (Identity t))
    ADual t -> withAPrimType t $ \(_ :: Proxy t) -> f (Proxy :: Proxy (Dual t))
    AMin t -> withAPrimType t $ \(_ :: Proxy t) -> f (Proxy :: Proxy (Min t))
    AMax t -> withAPrimType t $ \(_ :: Proxy t) -> f (Proxy :: Proxy (Max t))
    ASum t -> withAPrimType t $ \(_ :: Proxy t) -> f (Proxy :: Proxy (Sum t))
    AProduct t ->
      withAPrimType t $ \(_ :: Proxy t) -> f (Proxy :: Proxy (Product t))
    ADown t -> withAPrimType t $ \(_ :: Proxy t) -> f (Proxy :: Proxy (Down t))
    ATuple2 t1 t2 ->
      withAPrimType t1 $ \(_ :: Proxy t1) ->
        withAPrimType t2 $ \(_ :: Proxy t2) -> f (Proxy :: Proxy (t1, t2))
    ATuple3 t1 t2 t3 ->
      withAPrimType t1 $ \(_ :: Proxy t1) ->
        withAPrimType t2 $ \(_ :: Proxy t2) ->
          withAPrimType t3 $ \(_ :: Proxy t3) -> f (Proxy :: Proxy (t1, t2, t3))
    ATuple4 t1 t2 t3 t4 ->
      withAPrimType t1 $ \(_ :: Proxy t1) ->
        withAPrimType t2 $ \(_ :: Proxy t2) ->
          withAPrimType t3 $ \(_ :: Proxy t3) ->
            withAPrimType t4 $ \(_ :: Proxy t4) ->
              f (Proxy :: Proxy (t1, t2, t3, t4))
    ATuple5 t1 t2 t3 t4 t5 ->
      withAPrimType t1 $ \(_ :: Proxy t1) ->
        withAPrimType t2 $ \(_ :: Proxy t2) ->
          withAPrimType t3 $ \(_ :: Proxy t3) ->
            withAPrimType t4 $ \(_ :: Proxy t4) ->
              withAPrimType t5 $ \(_ :: Proxy t5) ->
                f (Proxy :: Proxy (t1, t2, t3, t4, t5))
    ATuple6 t1 t2 t3 t4 t5 t6 ->
      withAPrimType t1 $ \(_ :: Proxy t1) ->
        withAPrimType t2 $ \(_ :: Proxy t2) ->
          withAPrimType t3 $ \(_ :: Proxy t3) ->
            withAPrimType t4 $ \(_ :: Proxy t4) ->
              withAPrimType t5 $ \(_ :: Proxy t5) ->
                withAPrimType t6 $ \(_ :: Proxy t6) ->
                  f (Proxy :: Proxy (t1, t2, t3, t4, t5, t6))
    ATuple7 t1 t2 t3 t4 t5 t6 t7 ->
      withAPrimType t1 $ \(_ :: Proxy t1) ->
        withAPrimType t2 $ \(_ :: Proxy t2) ->
          withAPrimType t3 $ \(_ :: Proxy t3) ->
            withAPrimType t4 $ \(_ :: Proxy t4) ->
              withAPrimType t5 $ \(_ :: Proxy t5) ->
                withAPrimType t6 $ \(_ :: Proxy t6) ->
                  withAPrimType t7 $ \(_ :: Proxy t7) ->
                    f (Proxy :: Proxy (t1, t2, t3, t4, t5, t6, t7))
    ATuple8 t1 t2 t3 t4 t5 t6 t7 t8 ->
      withAPrimType t1 $ \(_ :: Proxy t1) ->
        withAPrimType t2 $ \(_ :: Proxy t2) ->
          withAPrimType t3 $ \(_ :: Proxy t3) ->
            withAPrimType t4 $ \(_ :: Proxy t4) ->
              withAPrimType t5 $ \(_ :: Proxy t5) ->
                withAPrimType t6 $ \(_ :: Proxy t6) ->
                  withAPrimType t7 $ \(_ :: Proxy t7) ->
                    withAPrimType t8 $ \(_ :: Proxy t8) ->
                      f (Proxy :: Proxy (t1, t2, t3, t4, t5, t6, t7, t8))
    ATuple9 t1 t2 t3 t4 t5 t6 t7 t8 t9 ->
      withAPrimType t1 $ \(_ :: Proxy t1) ->
        withAPrimType t2 $ \(_ :: Proxy t2) ->
          withAPrimType t3 $ \(_ :: Proxy t3) ->
            withAPrimType t4 $ \(_ :: Proxy t4) ->
              withAPrimType t5 $ \(_ :: Proxy t5) ->
                withAPrimType t6 $ \(_ :: Proxy t6) ->
                  withAPrimType t7 $ \(_ :: Proxy t7) ->
                    withAPrimType t8 $ \(_ :: Proxy t8) ->
                      withAPrimType t9 $ \(_ :: Proxy t9) ->
                        f (Proxy :: Proxy (t1, t2, t3, t4, t5, t6, t7, t8, t9))
    AArg t1 t2 ->
      withAPrimType t1 $ \(_ :: Proxy t1) ->
        withAPrimType t2 $ \(_ :: Proxy t2) -> f (Proxy :: Proxy (Arg t1 t2))
    AConst t ->
      withAPrimType t $ \(_ :: Proxy t) -> f (Proxy :: Proxy (Const t ()))
#if __GLASGOW_HASKELL__ >= 806
    AAp t ->
      withAPrimType t $ \(_ :: Proxy t) -> f (Proxy :: Proxy (Ap Maybe t))
#endif
    AAlt t ->
      withAPrimType t $ \(_ :: Proxy t) -> f (Proxy :: Proxy (Alt Maybe t))
    ACompose t ->
      withAPrimType t $ \(_ :: Proxy t) ->
        f (Proxy :: Proxy (Compose Identity Maybe t))

data APrim where
  APrim :: (Prim e, Arbitrary e, Show e, Eq e, Typeable e) => e -> APrim

instance Show APrim where
  showsPrec n (APrim a)
    | n < 1 = inner
    | otherwise = ('(' :) . inner . (")" ++)
    where
      inner = ("APrim (" ++) . shows a . (" :: " ++) . showsType [a] . (')':)

withAPrim :: APrim -> (forall e . (Prim e, Arbitrary e, Show e, Eq e, Typeable e) => e -> a) -> a
withAPrim (APrim e) f = f e


arbitraryProxy :: Arbitrary e => Proxy e -> Gen e
arbitraryProxy _ = arbitrary

instance Arbitrary APrim where
  arbitrary = do
    aPrimTy <- arbitrary
    withAPrimType aPrimTy (fmap APrim . arbitraryProxy)


data APrimList where
  APrimList :: (Prim e, Arbitrary e, Show e, Eq e, Typeable e) => [e] -> APrimList

instance Show APrimList where
  showsPrec n (APrimList a)
    | n < 1 = inner
    | otherwise = ('(' :) . inner . (")" ++)
    where
      inner = ("APrimList (" ++) . shows a . (" :: [" ++) . showsType a . ("])" ++)

withAPrimList ::
     APrimList
  -> (forall e. (Prim e, Arbitrary e, Show e, Eq e, Typeable e) => [e] -> a)
  -> a
withAPrimList (APrimList e) f = f e

instance Arbitrary APrimList where
  arbitrary = do
    aPrimTy <- arbitrary
    withAPrimType aPrimTy (fmap APrimList . listOf . arbitraryProxy)
