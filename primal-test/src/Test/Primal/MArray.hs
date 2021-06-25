{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Test.Prim.MArray
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Test.Prim.MArray where

import Primal.Exception
import Primal.Array
import Primal.Container.Array
import Primal.Container.Ref
import Primal.Unbox
import Test.Primal.Common
import Test.Primal.MRef

--type instance Elt MEArray

data MEArray ma e = MEArray ![e] !(Frozen (ma e))
deriving instance (Eq e, Eq (Frozen (ma e))) => Eq (MEArray ma e)
deriving instance (Show e, Show (Frozen (ma e))) => Show (MEArray ma e)

data MEMArray ma e s = MEMArray ![e] !(ma e s)


data NEArrayIx ma e = NEArrayIx !Int ![e] !(Frozen (ma e))
deriving instance (Eq e, Eq (Frozen (ma e))) => Eq (NEArrayIx ma e)
deriving instance (Show e, Show (Frozen (ma e))) => Show (NEArrayIx ma e)

data NEMArrayIx ma e s = NEMArrayIx !Int ![e] !(ma e s)


instance MRef ma e => MRef (MEMArray ma) e where
  newMRef e = MEMArray [e] <$> newMRef e
  {-# INLINE newMRef #-}
  newRawMRef = MEMArray [] <$> newRawMRef
  {-# INLINE newRawMRef #-}
  writeMRef (MEMArray _ ma) = writeMRef ma
  {-# INLINE writeMRef #-}
  readMRef (MEMArray _ ma) = readMRef ma
  {-# INLINE readMRef #-}

type instance Frozen (MEMArray ma e) = MEArray ma e

instance MArray ma e => MutFreeze (MEMArray ma e) where
  thawST (MEArray xs a) = MEMArray xs <$> thawArray a
  {-# INLINE thawST #-}
  freezeMutST (MEMArray xs ma) = MEArray xs <$> freezeMArray ma
  {-# INLINE freezeMutST #-}
  clone (MEArray xs a) = MEArray xs $ clone a
  {-# INLINE clone #-}
  cloneMutST (MEMArray xs ma) = MEMArray xs <$> cloneMutST ma
  {-# INLINE cloneMutST #-}
  thawCloneST (MEArray xs a) = MEMArray xs <$> thawCloneST a
  {-# INLINE thawCloneST #-}
  freezeCloneMutST (MEMArray xs ma) = MEArray xs <$> freezeCloneMutST ma
  {-# INLINE freezeCloneMutST #-}

-- TODO: cloneMutST is equivalent to (test it as such):
-- cloneMutST ma =
--    getSizeOfMArray ma >>= \sz -> newRawMArray sz >>= \ma' -> ma' <$ moveMArray ma 0 ma' 0 sz


instance MArray ma e => MArray (MEMArray ma) e where
  sizeOfArray (MEArray _ a) = sizeOfArray a
  {-# INLINE sizeOfArray #-}
  indexArray (MEArray _ a) = indexArray a
  {-# INLINE indexArray #-}
  getSizeOfMArray (MEMArray _ ma) = getSizeOfMArray ma
  {-# INLINE getSizeOfMArray #-}
  thawArray (MEArray xs a) = MEMArray xs <$> thawArray a
  {-# INLINE thawArray #-}
  freezeMArray (MEMArray xs ma) = MEArray xs <$> freezeMArray ma
  {-# INLINE freezeMArray #-}
  newRawMArray sz = MEMArray [] <$> newRawMArray sz
  {-# INLINE newRawMArray #-}
  newMArray sz e = MEMArray (replicate (unSize sz) e) <$> newMArray sz e
  {-# INLINE newMArray #-}
  writeMArray (MEMArray _ ma) = writeMArray ma
  {-# INLINE writeMArray #-}
  readMArray (MEMArray _ ma) = readMArray ma
  {-# INLINE readMArray #-}
  copyArray (MEArray _ src) isrc (MEMArray _ mdst) idst = copyArray src isrc mdst idst
  {-# INLINE copyArray #-}
  moveMArray (MEMArray _ msrc) isrc (MEMArray _ mdst) idst = moveMArray msrc isrc mdst idst
  {-# INLINE moveMArray #-}
  setMArray (MEMArray _ ma) = setMArray ma
  {-# INLINE setMArray #-}
  shrinkMArray (MEMArray xs ma) sz = MEMArray xs <$> shrinkMArray ma sz
  {-# INLINE shrinkMArray #-}
  resizeMArray (MEMArray xs ma) sz = MEMArray xs <$> resizeMArray ma sz
  {-# INLINE resizeMArray #-}


instance (MArray ma e, Arbitrary e) => Arbitrary (MEArray ma e) where
  arbitrary = do
    NonNegative n <- arbitrary
    xs :: [e] <- vectorOf n arbitrary
    pure $
      MEArray xs $
      createArrayST_ (Size n) $ \ma ->
        zipWithM_ (writeMArray ma) [0 ..] xs

instance MArray ma e => MRef (NEMArrayIx ma) e where
  newMRef e = NEMArrayIx 0 [e] <$> newMRef e
  {-# INLINE newMRef #-}
  newRawMRef = NEMArrayIx 0 [] <$> newRawMRef
  {-# INLINE newRawMRef #-}
  writeMRef (NEMArrayIx i _ ma) = writeMArray ma i
  {-# INLINE writeMRef #-}
  readMRef (NEMArrayIx i _ ma) = readMArray ma i
  {-# INLINE readMRef #-}

instance AtomicMArray ma e => AtomicMRef (NEMArrayIx ma) e where
  atomicReadMRef (NEMArrayIx i _ ma) = atomicReadMArray ma i
  {-# INLINE atomicReadMRef #-}
  atomicWriteMRef (NEMArrayIx i _ ma) = atomicWriteMArray ma i
  {-# INLINE atomicWriteMRef #-}
  casMRef (NEMArrayIx i _ ma) = casMArray ma i
  {-# INLINE casMRef #-}
  atomicModifyMRef (NEMArrayIx i _ ma) = atomicModifyMArray ma i
  {-# INLINE atomicModifyMRef #-}

instance AtomicCountMArray ma e => AtomicCountMRef (NEMArrayIx ma) e where
  atomicAddFetchOldMRef (NEMArrayIx i _ ma) = atomicAddFetchOldMArray ma i
  {-# INLINE atomicAddFetchOldMRef #-}
  atomicAddFetchNewMRef (NEMArrayIx i _ ma) = atomicAddFetchNewMArray ma i
  {-# INLINE atomicAddFetchNewMRef #-}
  atomicSubFetchOldMRef (NEMArrayIx i _ ma) = atomicSubFetchOldMArray ma i
  {-# INLINE atomicSubFetchOldMRef #-}
  atomicSubFetchNewMRef (NEMArrayIx i _ ma) = atomicSubFetchNewMArray ma i
  {-# INLINE atomicSubFetchNewMRef #-}

instance AtomicBitsMArray ma e => AtomicBitsMRef (NEMArrayIx ma) e where
  atomicAndFetchOldMRef (NEMArrayIx i _ ma) = atomicAndFetchOldMArray ma i
  {-# INLINE atomicAndFetchOldMRef #-}
  atomicAndFetchNewMRef (NEMArrayIx i _ ma) = atomicAndFetchNewMArray ma i
  {-# INLINE atomicAndFetchNewMRef #-}
  atomicNandFetchOldMRef (NEMArrayIx i _ ma) = atomicNandFetchOldMArray ma i
  {-# INLINE atomicNandFetchOldMRef #-}
  atomicNandFetchNewMRef (NEMArrayIx i _ ma) = atomicNandFetchNewMArray ma i
  {-# INLINE atomicNandFetchNewMRef #-}
  atomicOrFetchOldMRef (NEMArrayIx i _ ma) = atomicOrFetchOldMArray ma i
  {-# INLINE atomicOrFetchOldMRef #-}
  atomicOrFetchNewMRef (NEMArrayIx i _ ma) = atomicOrFetchNewMArray ma i
  {-# INLINE atomicOrFetchNewMRef #-}
  atomicXorFetchOldMRef (NEMArrayIx i _ ma) = atomicXorFetchOldMArray ma i
  {-# INLINE atomicXorFetchOldMRef #-}
  atomicXorFetchNewMRef (NEMArrayIx i _ ma) = atomicXorFetchNewMArray ma i
  {-# INLINE atomicXorFetchNewMRef #-}



type instance Frozen (NEMArrayIx ma e) = NEArrayIx ma e

instance MArray ma e => MutFreeze (NEMArrayIx ma e) where
  thawST (NEArrayIx i xs a) = NEMArrayIx i xs <$> thawArray a
  {-# INLINE thawST #-}
  freezeMutST (NEMArrayIx i xs ma) = NEArrayIx i xs <$> freezeMArray ma
  {-# INLINE freezeMutST #-}
  clone (NEArrayIx i xs a) = NEArrayIx i xs $ clone a
  {-# INLINE clone #-}
  cloneMutST (NEMArrayIx i xs ma) = NEMArrayIx i xs <$> cloneMutST ma
  {-# INLINE cloneMutST #-}
  thawCloneST (NEArrayIx i xs a) = NEMArrayIx i xs <$> thawCloneST a
  {-# INLINE thawCloneST #-}
  freezeCloneMutST (NEMArrayIx i xs ma) = NEArrayIx i xs <$> freezeCloneMutST ma
  {-# INLINE freezeCloneMutST #-}

instance MArray ma e => MArray (NEMArrayIx ma) e where
  sizeOfArray (NEArrayIx _ _ a) = sizeOfArray a
  {-# INLINE sizeOfArray #-}
  indexArray (NEArrayIx _ _ a) = indexArray a
  {-# INLINE indexArray #-}
  getSizeOfMArray (NEMArrayIx _ _ ma) = getSizeOfMArray ma
  {-# INLINE getSizeOfMArray #-}
  thawArray (NEArrayIx i xs a) = NEMArrayIx i xs <$> thawArray a
  {-# INLINE thawArray #-}
  thawCopyArray (NEArrayIx i xs a) o sz@(Size n)
    | n == 0 = error "NEArrayIx cannot be empty"
    | otherwise = NEMArrayIx (i `mod` n) (take n (drop o xs)) <$> thawCopyArray a o sz
  {-# INLINE thawCopyArray #-}
  freezeMArray (NEMArrayIx i xs ma) = NEArrayIx i xs <$> freezeMArray ma
  {-# INLINE freezeMArray #-}
  newRawMArray sz = NEMArrayIx 0 [] <$> newRawMArray sz
  {-# INLINE newRawMArray #-}
  newMArray sz e = NEMArrayIx 0 (replicate (unSize sz) e) <$> newMArray sz e
  {-# INLINE newMArray #-}
  writeMArray (NEMArrayIx _ _ ma) = writeMArray ma
  {-# INLINE writeMArray #-}
  readMArray (NEMArrayIx _ _ ma) = readMArray ma
  {-# INLINE readMArray #-}
  copyArray (NEArrayIx _ _ src) isrc (NEMArrayIx _ _ mdst) idst =
    copyArray src isrc mdst idst
  {-# INLINE copyArray #-}
  moveMArray (NEMArrayIx _ _ msrc) isrc (NEMArrayIx _ _ mdst) idst =
    moveMArray msrc isrc mdst idst
  {-# INLINE moveMArray #-}
  setMArray (NEMArrayIx _ _ ma) = setMArray ma
  {-# INLINE setMArray #-}
  shrinkMArray (NEMArrayIx i xs ma) sz = NEMArrayIx i xs <$> shrinkMArray ma sz
  {-# INLINE shrinkMArray #-}
  resizeMArray (NEMArrayIx i xs ma) sz = NEMArrayIx i xs <$> resizeMArray ma sz
  {-# INLINE resizeMArray #-}


instance (MArray ma e, Arbitrary e) => Arbitrary (NEArrayIx ma e) where
  arbitrary = do
    Positive n <- arbitrary
    NonNegative k <- arbitrary
    let i = k `mod` n
    xs :: [e] <- vectorOf n arbitrary
    pure $
      NEArrayIx i xs $
      createArrayST_ (Size n) $ \ma -> zipWithM_ (writeMArray ma) [0 ..] xs


prop_writeRead ::
     forall ma e. (Eq e, Show e, MArray ma e)
  => NEArrayIx ma e
  -> e
  -> Property
prop_writeRead nea e' = propIO $ do
  ma@(NEMArrayIx i xs _) <- thawArray nea
  e <- readMRef ma
  e `shouldBe` (xs !! i)
  expectWriteReadMRef ma e'

prop_writeReadAtomic ::
    forall ma e. (Eq e, Show e, AtomicMArray ma e)
  => NEArrayIx ma e
  -> e
  -> Property
prop_writeReadAtomic nea e' = propIO $ do
  ma@(NEMArrayIx i xs _) <- thawArray nea
  e <- atomicReadMRef ma
  e `shouldBe` (xs !! i)
  expectWriteReadAtomicMRef ma e'



specMArray ::
     forall ma e.
     ( Eq e
     , Show e
     , Arbitrary e
     , Show (Frozen (ma e))
     , AtomicMArray ma e
     , Typeable ma
     , Typeable e
     )
  => Spec
specMArray = do
  let mutTypeName = showsType (Proxy :: Proxy (NEArrayIx ma e)) ""
  describe mutTypeName $ do
    prop "writeRead" $ prop_writeRead @ma @e
    prop "writeReadAtomic" $ prop_writeReadAtomic @ma @e
