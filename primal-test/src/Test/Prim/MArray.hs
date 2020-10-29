{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
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

import Control.Prim.Exception
import Data.Prim
import Data.Prim.Array
import Data.Prim.MArray
import Data.Prim.MRef
import Test.Prim.MRef
import Test.Prim.Common

data MEArray mut = MEArray ![Elt mut] !(Array mut)
deriving instance (Eq (Elt mut), Eq (Array mut)) => Eq (MEArray mut)
deriving instance (Show (Elt mut), Show (Array mut)) => Show (MEArray mut)

data MEMArray mut s = MEMArray ![Elt mut] !(mut s)


data NEArrayIx mut = NEArrayIx !Int ![Elt mut] !(Array mut)
deriving instance (Eq (Elt mut), Eq (Array mut)) => Eq (NEArrayIx mut)
deriving instance (Show (Elt mut), Show (Array mut)) => Show (NEArrayIx mut)

data NEMArrayIx mut s = NEMArrayIx !Int ![Elt mut] !(mut s)


instance MRef mut => MRef (MEMArray mut) where
  type Elt (MEMArray mut) = Elt mut
  newMRef e = MEMArray [e] <$> newMRef e
  {-# INLINE newMRef #-}
  newRawMRef = MEMArray [] <$> newRawMRef
  {-# INLINE newRawMRef #-}
  writeMRef (MEMArray _ ma) = writeMRef ma
  {-# INLINE writeMRef #-}
  readMRef (MEMArray _ ma) = readMRef ma
  {-# INLINE readMRef #-}


instance MArray mut => MArray (MEMArray mut) where
  type Array (MEMArray mut) = MEArray mut
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


instance (MArray mut, Arbitrary (Elt mut)) => Arbitrary (MEArray mut) where
  arbitrary = do
    NonNegative n <- arbitrary
    xs :: [Elt mut] <- vectorOf n arbitrary
    pure $
      MEArray xs $
      createArrayST_ (Size n) $ \ma ->
        zipWithM_ (writeMArray ma) [0 ..] xs


instance MArray mut => MRef (NEMArrayIx mut) where
  type Elt (NEMArrayIx mut) = Elt mut
  newMRef e = NEMArrayIx 0 [e] <$> newMRef e
  {-# INLINE newMRef #-}
  newRawMRef = NEMArrayIx 0 [] <$> newRawMRef
  {-# INLINE newRawMRef #-}
  writeMRef (NEMArrayIx i _ ma) = writeMArray ma i
  {-# INLINE writeMRef #-}
  readMRef (NEMArrayIx i _ ma) = readMArray ma i
  {-# INLINE readMRef #-}


instance AtomicMArray mut => AtomicMRef (NEMArrayIx mut) where
  atomicReadMRef (NEMArrayIx i _ ma) = atomicReadMArray ma i
  {-# INLINE atomicReadMRef #-}
  atomicWriteMRef (NEMArrayIx i _ ma) = atomicWriteMArray ma i
  {-# INLINE atomicWriteMRef #-}
  casMRef (NEMArrayIx i _ ma) = casMArray ma i
  {-# INLINE casMRef #-}
  atomicModifyMRef (NEMArrayIx i _ ma) = atomicModifyMArray ma i
  {-# INLINE atomicModifyMRef #-}

instance AtomicCountMArray mut => AtomicCountMRef (NEMArrayIx mut) where
  atomicAddFetchOldMRef (NEMArrayIx i _ ma) = atomicAddFetchOldMArray ma i
  {-# INLINE atomicAddFetchOldMRef #-}
  atomicAddFetchNewMRef (NEMArrayIx i _ ma) = atomicAddFetchNewMArray ma i
  {-# INLINE atomicAddFetchNewMRef #-}
  atomicSubFetchOldMRef (NEMArrayIx i _ ma) = atomicSubFetchOldMArray ma i
  {-# INLINE atomicSubFetchOldMRef #-}
  atomicSubFetchNewMRef (NEMArrayIx i _ ma) = atomicSubFetchNewMArray ma i
  {-# INLINE atomicSubFetchNewMRef #-}

instance AtomicBitsMArray mut => AtomicBitsMRef (NEMArrayIx mut) where
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



instance MArray mut => MArray (NEMArrayIx mut) where
  type Array (NEMArrayIx mut) = NEArrayIx mut
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


instance (MArray mut, Arbitrary (Elt mut)) => Arbitrary (NEArrayIx mut) where
  arbitrary = do
    Positive n <- arbitrary
    NonNegative k <- arbitrary
    let i = k `mod` n
    xs :: [Elt mut] <- vectorOf n arbitrary
    pure $
      NEArrayIx i xs $
      createArrayST_ (Size n) $ \ma -> zipWithM_ (writeMArray ma) [0 ..] xs


prop_writeRead ::
     (Eq (Elt mut), Show (Elt mut), MArray mut)
  => NEArrayIx mut
  -> Elt mut
  -> Property
prop_writeRead nea e' = propIO $ do
  ma@(NEMArrayIx i xs _) <- thawArray nea
  e <- readMRef ma
  e `shouldBe` (xs !! i)
  expectWriteReadMRef ma e'

prop_writeReadAtomic ::
     (Eq (Elt mut), Show (Elt mut), AtomicMArray mut)
  => NEArrayIx mut
  -> Elt mut
  -> Property
prop_writeReadAtomic nea e' = propIO $ do
  ma@(NEMArrayIx i xs _) <- thawArray nea
  e <- atomicReadMRef ma
  e `shouldBe` (xs !! i)
  expectWriteReadAtomicMRef ma e'


prop_writeReadException ::
     HasCallStack => NEArrayIx (BMArray (Maybe Integer)) -> Integer -> Property
prop_writeReadException nea e' = propIO $ do
  ma@(NEMArrayIx i xs m) <- thawCopyArray nea 0 (sizeOfArray nea)
  e <- readMRef ma
  e `shouldBe` (xs !! i)
  writeBMArray m i (impureThrow DivideByZero) `shouldThrow` (== DivideByZero)
  readBMArray m i `shouldReturn` e
  let i' = i + 1
  when (i' < length xs) $ do
    writeBMArray m i' (Just (e' `div` 0))
    a <- freezeBMArray m
    case indexBArray a i' of
      Just x -> do
        (pure $! x) `shouldThrow` (== DivideByZero)
      Nothing -> expectationFailure "Wrote Just got back Nothing"



specMArray ::
     forall mut.
     ( Eq (Elt mut)
     , Show (Elt mut)
     , Arbitrary (Elt mut)
     , Show (Array mut)
     , AtomicMArray mut
     , Typeable mut
     )
  => Spec
specMArray = do
  let mutTypeName = showsType (Proxy :: Proxy (NEArrayIx mut)) ""
  describe mutTypeName $ do
    prop "writeRead" $ prop_writeRead @mut
    prop "writeReadAtomic" $ prop_writeReadAtomic @mut
    prop "funny" prop_writeReadException
    prop "shrinkSBMArray" $ \ (a :: SBArray Integer) (NonNegative k) -> do
      ma <- thawSBArray a
      n <- getSizeOfSBMArray ma
      let k'
            | n == 0 = 0
            | otherwise = k `mod` n
      shrinkSBMArray ma k'
      getSizeOfSBMArray ma `shouldReturn` k'
      a' <- freezeSBMArray ma
      sizeOfSBArray a' `shouldBe` k'
    prop "shrinkBMArray" $ \ (a :: BArray Integer) (NonNegative k) -> do
      ma <- thawBArray a
      n <- getSizeOfBMArray ma
      let k'
            | n == 0 = 0
            | otherwise = k `mod` n
      shrinkBMArray ma k'
      getSizeOfBMArray ma `shouldReturn` k'
      a' <- freezeBMArray ma
      sizeOfBArray a' `shouldBe` k'

-- forAllIO :: (Show p, Testable t) => Gen p -> (p -> IO t) -> Property
-- forAllIO g propM = forAll g $ \v -> monadicIO $ run $ propM v

-- forAllMutIO ::
--      (Show p, Atomic p, Testable t)
--   => Gen p
--   -> (p -> r p IO -> IO t)
--   -> Property
-- forAllMutIO g propM = forAllIO g $ \v -> newMut v >>= propM v


-- propMutSpecIO ::
--      (Show p, Atomic p, Testable t)
--   => String
--   -> Gen p
--   -> (p -> r IO p -> IO t)
--   -> Spec
-- propMutSpecIO name gen action = prop name $ forAllAVarIO gen action

--atomicAddFetchOldProp :: (a -> f a s -> m b) ->
