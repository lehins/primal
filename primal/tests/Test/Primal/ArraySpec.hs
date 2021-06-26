{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Primal.ArraySpec (spec) where

import Primal.Unbox
import Primal.Foreign (IsList(..))
import Primal.Exception
import Primal.Array
import Primal.Mutable.Freeze
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Classes.Base
import Test.Hspec
import Test.Hspec.QuickCheck

lawsSpec :: Laws -> Spec
lawsSpec Laws {..} =
  describe lawsTypeclass $ mapM_ (uncurry prop) lawsProperties

instance Arbitrary Size where
  arbitrary = Size . getNonNegative <$> arbitrary

instance Arbitrary e => Arbitrary (BArray e) where
  arbitrary = arbitrary1

instance Arbitrary1 BArray where
  liftArbitrary gen = do
    sz@(Size n) <- arbitrary
    fromListBArrayN sz <$> vectorOf n gen

instance Arbitrary e => Arbitrary (SBArray e) where
  arbitrary = arbitrary1

instance Arbitrary1 SBArray where
  liftArbitrary gen = do
    sz@(Size n) <- arbitrary
    fromListSBArrayN sz <$> vectorOf n gen


instance (Unbox e, Arbitrary e) => Arbitrary (UArray e) where
  arbitrary = do
    sz@(Size n) <- arbitrary
    fromListUArrayN sz <$> vector n


arrayLawsSpec ::
     ( Ord a
     , IsList a
     , Show a
     , Show (Item a)
     , Arbitrary a
     , Arbitrary (Item a)
     , Monoid a
     , Semigroup a
     )
  => Proxy a
  -> Spec
arrayLawsSpec px = do
  lawsSpec $ eqLaws px
  lawsSpec $ ordLaws px
  lawsSpec $ showLaws px
  lawsSpec $ isListLaws px
  lawsSpec $ monoidLaws px
  lawsSpec $ semigroupLaws px
  lawsSpec $ semigroupMonoidLaws px

prop_writeBArrayException :: Integer -> Property
prop_writeBArrayException x = monadicIO $ run $ do
  ma <- newBMArray 4 (Nothing :: Maybe Integer)
  writeBMArray ma 2 (Just x)
  a <- freezeCloneMut ma
  a `shouldBe` fromListBArray [Nothing,Nothing,Just x,Nothing]

  writeBMArray ma 2 (impureThrow DivideByZero) `shouldThrow` (== DivideByZero)
  freezeCloneMut ma `shouldReturn` fromListBArray [Nothing,Nothing,Just x,Nothing]

  writeBMArray ma 3 (Just (x `div` 0))
  deepevalM (readBMArray ma 3) `shouldThrow` (== DivideByZero)
  deepevalM (freezeBMArray ma) `shouldThrow` (== DivideByZero)


prop_shrinkBMArray :: BArray Integer -> NonNegative Size -> Property
prop_shrinkBMArray a (NonNegative k) = monadicIO $ run $ do
  ma <- thawBArray a
  n <- getSizeOfBMArray ma
  let k'
        | n == 0 = 0
        | otherwise = k `mod` n
  shrinkBMArray ma k'
  getSizeOfBMArray ma `shouldReturn` k'
  a' <- freezeBMArray ma
  sizeOfBArray a' `shouldBe` k'


prop_writeSBArrayException :: Integer -> Property
prop_writeSBArrayException x = monadicIO $ run $ do
  ma <- newSBMArray 4 (Nothing :: Maybe Integer)
  writeSBMArray ma 2 (Just x)
  a <- freezeCloneMut ma
  a `shouldBe` fromListSBArray [Nothing,Nothing,Just x,Nothing]

  writeSBMArray ma 2 (impureThrow DivideByZero) `shouldThrow` (== DivideByZero)
  freezeCloneMut ma `shouldReturn` fromListSBArray [Nothing,Nothing,Just x,Nothing]

  writeSBMArray ma 3 (Just (x `div` 0))
  deepevalM (readSBMArray ma 3) `shouldThrow` (== DivideByZero)
  deepevalM (freezeSBMArray ma) `shouldThrow` (== DivideByZero)

prop_shrinkSBMArray :: SBArray Integer -> NonNegative Size -> Property
prop_shrinkSBMArray a (NonNegative k) = monadicIO $ run $ do
  ma <- thawSBArray a
  n <- getSizeOfSBMArray ma
  let k'
        | n == 0 = 0
        | otherwise = k `mod` n
  shrinkSBMArray ma k'
  getSizeOfSBMArray ma `shouldReturn` k'
  a' <- freezeSBMArray ma
  sizeOfSBArray a' `shouldBe` k'



spec :: Spec
spec = do
  describe "BArray" $ do
    arrayLawsSpec (Proxy :: Proxy (BArray Char))
    lawsSpec $ functorLaws (Proxy :: Proxy BArray)
    lawsSpec $ foldableLaws (Proxy :: Proxy BArray)
    prop "prop_writeBArrayException" prop_writeBArrayException
    prop "shrinkBMArray" prop_shrinkBMArray
  describe "SBArray" $ do
    arrayLawsSpec (Proxy :: Proxy (SBArray Char))
    lawsSpec $ functorLaws (Proxy :: Proxy SBArray)
    lawsSpec $ foldableLaws (Proxy :: Proxy SBArray)
    prop "prop_writeSBArrayException" prop_writeSBArrayException
    prop "shrinkSBMArray" prop_shrinkSBMArray
  describe "UArray" $ do
    arrayLawsSpec (Proxy :: Proxy (UArray Char))
