{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Prim
  ( module Test.Prim.Common
  ) where

import Control.DeepSeq
import Data.Prim.Memory.Bytes
import Foreign.Prim hiding (Any)
import Foreign.Prim.Ptr
import Foreign.Prim.StablePtr
import GHC.Conc
import GHC.IO.Device
import GHC.Fingerprint.Type
import Test.Prim.Common



---- Orphans


instance NFData BlockReason where
  rnf br = seq br ()
instance Arbitrary BlockReason where
  arbitrary =
    elements
      [ BlockedOnMVar
      , BlockedOnBlackHole
      , BlockedOnException
      , BlockedOnSTM
      , BlockedOnForeignCall
      , BlockedOnOther
      ]


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

instance NFData SeekMode where
  rnf sm = seq sm ()
instance Arbitrary SeekMode where
  arbitrary = elements [toEnum 0 .. ]

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

instance Arbitrary (Off a) where
  arbitrary = Off . getNonNegative <$> arbitrary

instance Arbitrary (Count a) where
  arbitrary = Count . getNonNegative <$> arbitrary

instance Arbitrary a => Arbitrary (Atom a) where
  arbitrary = (coerce :: Gen a -> Gen (Atom a)) arbitrary
