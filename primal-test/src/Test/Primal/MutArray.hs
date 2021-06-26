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
-- Module      : Test.Primal.MutArray
-- Copyright   : (c) Alexey Kuleshevich 2020-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Test.Primal.MutArray where

import Primal.Exception
import Primal.Array
import Primal.Container.Array
import Primal.Container.Ref
import Primal.Unbox
import Test.Primal.Common
import Test.Primal.MutRef

data MEArray ma e = MEArray ![e] !(Frozen (ma e))
deriving instance (Eq e, Eq (Frozen (ma e))) => Eq (MEArray ma e)
deriving instance (Show e, Show (Frozen (ma e))) => Show (MEArray ma e)

data MEMutArray ma e s = MEMutArray ![e] !(ma e s)

type instance Elt (MEMutArray ma) e = Elt ma e


data NEArrayIx ma e = NEArrayIx !Int ![e] !(Frozen (ma e))
deriving instance (Eq e, Eq (Frozen (ma e))) => Eq (NEArrayIx ma e)
deriving instance (Show e, Show (Frozen (ma e))) => Show (NEArrayIx ma e)

data NEMutArrayIx ma e s = NEMutArrayIx !Int ![e] !(ma e s)

type instance Elt (NEMutArrayIx ma) e = Elt ma e

instance MutRef ma => MutRef (MEMutArray ma) where
  newMutRefST e = MEMutArray [e] <$> newMutRef e
  {-# INLINE newMutRefST #-}
  newRawMutRefST = MEMutArray [] <$> newRawMutRef
  {-# INLINE newRawMutRefST #-}
  writeMutRefST (MEMutArray _ ma) = writeMutRef ma
  {-# INLINE writeMutRefST #-}
  readMutRefST (MEMutArray _ ma) = readMutRef ma
  {-# INLINE readMutRefST #-}

type instance Frozen (MEMutArray ma e) = MEArray ma e

instance MutFreeze (ma e) => MutFreeze (MEMutArray ma e) where
  thawST (MEArray xs a) = MEMutArray xs <$> thaw a
  {-# INLINE thawST #-}
  freezeMutST (MEMutArray xs ma) = MEArray xs <$> freezeMut ma
  {-# INLINE freezeMutST #-}
  clone (MEArray xs a) = MEArray xs $ clone a
  {-# INLINE clone #-}
  cloneMutST (MEMutArray xs ma) = MEMutArray xs <$> cloneMutST ma
  {-# INLINE cloneMutST #-}
  thawCloneST (MEArray xs a) = MEMutArray xs <$> thawCloneST a
  {-# INLINE thawCloneST #-}
  freezeCloneMutST (MEMutArray xs ma) = MEArray xs <$> freezeCloneMutST ma
  {-# INLINE freezeCloneMutST #-}

-- TODO: cloneMutST is equivalent to (test it as such):
-- cloneMutST ma =
--    getSizeOfMutArray ma >>= \sz -> newRawMutArray sz >>= \ma' -> ma' <$ moveMutArray ma 0 ma' 0 sz


instance MutArray ma => MutArray (MEMutArray ma) where
  sizeOfArray (MEArray _ a) = sizeOfArray a
  {-# INLINE sizeOfArray #-}
  indexArray (MEArray _ a) = indexArray a
  {-# INLINE indexArray #-}
  getSizeOfMutArrayST (MEMutArray _ ma) = getSizeOfMutArray ma
  {-# INLINE getSizeOfMutArrayST #-}
  thawArrayST (MEArray xs a) = MEMutArray xs <$> thawArray a
  {-# INLINE thawArrayST #-}
  freezeMutArrayST (MEMutArray xs ma) = MEArray xs <$> freezeMutArray ma
  {-# INLINE freezeMutArrayST #-}
  newRawMutArrayST sz = MEMutArray [] <$> newRawMutArray sz
  {-# INLINE newRawMutArrayST #-}
  newMutArrayST sz e = MEMutArray (replicate (unSize sz) e) <$> newMutArray sz e
  {-# INLINE newMutArrayST #-}
  writeMutArrayST (MEMutArray _ ma) = writeMutArray ma
  {-# INLINE writeMutArrayST #-}
  readMutArrayST (MEMutArray _ ma) = readMutArray ma
  {-# INLINE readMutArrayST #-}
  copyArrayST (MEArray _ src) isrc (MEMutArray _ mdst) idst = copyArray src isrc mdst idst
  {-# INLINE copyArrayST #-}
  moveMutArrayST (MEMutArray _ msrc) isrc (MEMutArray _ mdst) idst = moveMutArray msrc isrc mdst idst
  {-# INLINE moveMutArrayST #-}
  setMutArrayST (MEMutArray _ ma) = setMutArray ma
  {-# INLINE setMutArrayST #-}
  shrinkMutArrayST (MEMutArray xs ma) sz = MEMutArray xs <$> shrinkMutArray ma sz
  {-# INLINE shrinkMutArrayST #-}
  resizeMutArrayST (MEMutArray xs ma) sz = MEMutArray xs <$> resizeMutArray ma sz
  {-# INLINE resizeMutArrayST #-}


instance (MutArray ma, Elt ma e, Arbitrary e) => Arbitrary (MEArray ma e) where
  arbitrary = do
    NonNegative n <- arbitrary
    xs :: [e] <- vectorOf n arbitrary
    pure $
      MEArray xs $
      createArrayST_ (Size n) $ \ma ->
        zipWithM_ (writeMutArray ma) [0 ..] xs

instance MutArray ma => MutRef (NEMutArrayIx ma) where
  newMutRefST e = NEMutArrayIx 0 [e] <$> newMutRef e
  {-# INLINE newMutRefST #-}
  newRawMutRefST = NEMutArrayIx 0 [] <$> newRawMutRef
  {-# INLINE newRawMutRefST #-}
  writeMutRefST (NEMutArrayIx i _ ma) = writeMutArray ma i
  {-# INLINE writeMutRefST #-}
  readMutRefST (NEMutArrayIx i _ ma) = readMutArray ma i
  {-# INLINE readMutRefST #-}

instance AtomicMutArray ma => AtomicMutRef (NEMutArrayIx ma) where
  type AtomicElt (NEMutArrayIx ma) e = AtomicElt ma e
  atomicReadMutRefST (NEMutArrayIx i _ ma) = atomicReadMutArray ma i
  {-# INLINE atomicReadMutRefST #-}
  atomicWriteMutRefST (NEMutArrayIx i _ ma) = atomicWriteMutArray ma i
  {-# INLINE atomicWriteMutRefST #-}
  casMutRefST (NEMutArrayIx i _ ma) = casMutArray ma i
  {-# INLINE casMutRefST #-}
  atomicModifyMutRefST (NEMutArrayIx i _ ma) = atomicModifyMutArray ma i
  {-# INLINE atomicModifyMutRefST #-}

instance AtomicCountMutArray ma => AtomicCountMutRef (NEMutArrayIx ma) where
  type AtomicCountElt (NEMutArrayIx ma) e = AtomicCountElt ma e
  atomicAddFetchOldMutRefST (NEMutArrayIx i _ ma) = atomicAddFetchOldMutArray ma i
  {-# INLINE atomicAddFetchOldMutRefST #-}
  atomicAddFetchNewMutRefST (NEMutArrayIx i _ ma) = atomicAddFetchNewMutArray ma i
  {-# INLINE atomicAddFetchNewMutRefST #-}
  atomicSubFetchOldMutRefST (NEMutArrayIx i _ ma) = atomicSubFetchOldMutArray ma i
  {-# INLINE atomicSubFetchOldMutRefST #-}
  atomicSubFetchNewMutRefST (NEMutArrayIx i _ ma) = atomicSubFetchNewMutArray ma i
  {-# INLINE atomicSubFetchNewMutRefST #-}

instance AtomicBitsMutArray ma => AtomicBitsMutRef (NEMutArrayIx ma) where
  type AtomicBitsElt (NEMutArrayIx ma) e = AtomicBitsElt ma e
  atomicAndFetchOldMutRefST (NEMutArrayIx i _ ma) = atomicAndFetchOldMutArray ma i
  {-# INLINE atomicAndFetchOldMutRefST #-}
  atomicAndFetchNewMutRefST (NEMutArrayIx i _ ma) = atomicAndFetchNewMutArray ma i
  {-# INLINE atomicAndFetchNewMutRefST #-}
  atomicNandFetchOldMutRefST (NEMutArrayIx i _ ma) = atomicNandFetchOldMutArray ma i
  {-# INLINE atomicNandFetchOldMutRefST #-}
  atomicNandFetchNewMutRefST (NEMutArrayIx i _ ma) = atomicNandFetchNewMutArray ma i
  {-# INLINE atomicNandFetchNewMutRefST #-}
  atomicOrFetchOldMutRefST (NEMutArrayIx i _ ma) = atomicOrFetchOldMutArray ma i
  {-# INLINE atomicOrFetchOldMutRefST #-}
  atomicOrFetchNewMutRefST (NEMutArrayIx i _ ma) = atomicOrFetchNewMutArray ma i
  {-# INLINE atomicOrFetchNewMutRefST #-}
  atomicXorFetchOldMutRefST (NEMutArrayIx i _ ma) = atomicXorFetchOldMutArray ma i
  {-# INLINE atomicXorFetchOldMutRefST #-}
  atomicXorFetchNewMutRefST (NEMutArrayIx i _ ma) = atomicXorFetchNewMutArray ma i
  {-# INLINE atomicXorFetchNewMutRefST #-}
  atomicNotFetchOldMutRefST (NEMutArrayIx i _ ma) = atomicNotFetchOldMutArray ma i
  {-# INLINE atomicNotFetchOldMutRefST #-}
  atomicNotFetchNewMutRefST (NEMutArrayIx i _ ma) = atomicNotFetchNewMutArray ma i
  {-# INLINE atomicNotFetchNewMutRefST #-}



type instance Frozen (NEMutArrayIx ma e) = NEArrayIx ma e

instance MutFreeze (ma e) => MutFreeze (NEMutArrayIx ma e) where
  thawST (NEArrayIx i xs a) = NEMutArrayIx i xs <$> thaw a
  {-# INLINE thawST #-}
  freezeMutST (NEMutArrayIx i xs ma) = NEArrayIx i xs <$> freezeMut ma
  {-# INLINE freezeMutST #-}
  clone (NEArrayIx i xs a) = NEArrayIx i xs $ clone a
  {-# INLINE clone #-}
  cloneMutST (NEMutArrayIx i xs ma) = NEMutArrayIx i xs <$> cloneMutST ma
  {-# INLINE cloneMutST #-}
  thawCloneST (NEArrayIx i xs a) = NEMutArrayIx i xs <$> thawCloneST a
  {-# INLINE thawCloneST #-}
  freezeCloneMutST (NEMutArrayIx i xs ma) = NEArrayIx i xs <$> freezeCloneMutST ma
  {-# INLINE freezeCloneMutST #-}

instance MutArray ma => MutArray (NEMutArrayIx ma) where
  sizeOfArray (NEArrayIx _ _ a) = sizeOfArray a
  {-# INLINE sizeOfArray #-}
  indexArray (NEArrayIx _ _ a) = indexArray a
  {-# INLINE indexArray #-}
  getSizeOfMutArrayST (NEMutArrayIx _ _ ma) = getSizeOfMutArray ma
  {-# INLINE getSizeOfMutArrayST #-}
  thawArrayST (NEArrayIx i xs a) = NEMutArrayIx i xs <$> thawArray a
  {-# INLINE thawArrayST #-}
  thawCopyArrayST (NEArrayIx i xs a) o sz@(Size n)
    | n == 0 = error "NEArrayIx cannot be empty"
    | otherwise = NEMutArrayIx (i `mod` n) (take n (drop o xs)) <$> thawCopyArray a o sz
  {-# INLINE thawCopyArrayST #-}
  freezeMutArrayST (NEMutArrayIx i xs ma) = NEArrayIx i xs <$> freezeMutArray ma
  {-# INLINE freezeMutArrayST #-}
  newRawMutArrayST sz = NEMutArrayIx 0 [] <$> newRawMutArray sz
  {-# INLINE newRawMutArrayST #-}
  newMutArrayST sz e = NEMutArrayIx 0 (replicate (unSize sz) e) <$> newMutArray sz e
  {-# INLINE newMutArrayST #-}
  writeMutArrayST (NEMutArrayIx _ _ ma) = writeMutArray ma
  {-# INLINE writeMutArrayST #-}
  readMutArrayST (NEMutArrayIx _ _ ma) = readMutArray ma
  {-# INLINE readMutArrayST #-}
  copyArrayST (NEArrayIx _ _ src) isrc (NEMutArrayIx _ _ mdst) idst =
    copyArray src isrc mdst idst
  {-# INLINE copyArrayST #-}
  moveMutArrayST (NEMutArrayIx _ _ msrc) isrc (NEMutArrayIx _ _ mdst) idst =
    moveMutArray msrc isrc mdst idst
  {-# INLINE moveMutArrayST #-}
  setMutArrayST (NEMutArrayIx _ _ ma) = setMutArray ma
  {-# INLINE setMutArrayST #-}
  shrinkMutArrayST (NEMutArrayIx i xs ma) sz = NEMutArrayIx i xs <$> shrinkMutArray ma sz
  {-# INLINE shrinkMutArrayST #-}
  resizeMutArrayST (NEMutArrayIx i xs ma) sz = NEMutArrayIx i xs <$> resizeMutArray ma sz
  {-# INLINE resizeMutArrayST #-}


instance (MutArray ma, Elt ma e, Arbitrary e) => Arbitrary (NEArrayIx ma e) where
  arbitrary = do
    Positive n <- arbitrary
    NonNegative k <- arbitrary
    let i = k `mod` n
    xs :: [e] <- vectorOf n arbitrary
    pure $
      NEArrayIx i xs $
      createArrayST_ (Size n) $ \ma -> zipWithM_ (writeMutArray ma) [0 ..] xs


prop_writeRead ::
     forall ma e. (Eq e, Show e, MutArray ma, Elt ma e)
  => NEArrayIx ma e
  -> e
  -> Property
prop_writeRead nea e' = propIO $ do
  ma@(NEMutArrayIx i xs _) <- thawArray nea
  e <- readMutRef ma
  e `shouldBe` (xs !! i)
  expectWriteReadMutRef ma e'

prop_writeReadAtomic ::
    forall ma e. (Eq e, Show e, AtomicMutArray ma, AtomicElt ma e, Elt ma e)
  => NEArrayIx ma e
  -> e
  -> Property
prop_writeReadAtomic nea e' = propIO $ do
  ma@(NEMutArrayIx i xs _) <- thawArray nea
  e <- atomicReadMutRef ma
  e `shouldBe` (xs !! i)
  expectWriteReadAtomicMutRef ma e'



specMutArray ::
     forall ma e.
     ( Eq e
     , Show e
     , Arbitrary e
     , Show (Array ma e)
     , AtomicMutArray ma
     , AtomicElt ma e
     , Elt ma e
     , Typeable ma
     , Typeable e
     )
  => Spec
specMutArray = do
  let mutTypeName = showsType (Proxy :: Proxy (NEArrayIx ma e)) ""
  describe mutTypeName $ do
    prop "writeRead" $ prop_writeRead @ma @e
    prop "writeReadAtomic" $ prop_writeReadAtomic @ma @e
