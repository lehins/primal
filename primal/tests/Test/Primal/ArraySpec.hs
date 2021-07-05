{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Primal.ArraySpec (spec, impreciseExpectedException, ExpectedException(..)) where

import Primal.Array
import Primal.Exception
import Primal.Eval
import Primal.Foreign (IsList(..))
import Primal.Mutable.Freeze
import Primal.Unbox
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Classes.Base
import Test.QuickCheck.Monadic

data ExpectedException =
  ExpectedException
  deriving (Show, Eq)

instance Exception ExpectedException

impreciseExpectedException :: ImpreciseException -> Bool
impreciseExpectedException (ImpreciseException exc _) =
  fromException exc == Just ExpectedException

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

prop_writeBArrayException :: HasCallStack => Integer -> Property
prop_writeBArrayException x = monadicIO $ run $ do
  ma <- newBMArray 4 (Nothing :: Maybe Integer)
  writeBMArray ma 2 (Just x)
  a <- freezeCloneMut ma
  a `shouldBe` fromListBArray [Nothing,Nothing,Just x,Nothing]

  writeBMArray ma 2 (raiseImprecise ExpectedException) `shouldThrow` impreciseExpectedException
  freezeCloneMut ma `shouldReturn` fromListBArray [Nothing,Nothing,Just x,Nothing]

  writeBMArray ma 3 (Just (raiseImprecise ExpectedException))
  deepevalM (readBMArray ma 3) `shouldThrow` impreciseExpectedException
  deepevalM (freezeBMArray ma) `shouldThrow` impreciseExpectedException


prop_shrinkBMArray :: HasCallStack => BArray Integer -> NonNegative Size -> Property
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


prop_writeSBArrayException :: HasCallStack => Integer -> Property
prop_writeSBArrayException x = monadicIO $ run $ do
  ma <- newSBMArray 4 (Nothing :: Maybe Integer)
  writeSBMArray ma 2 (Just x)
  a <- freezeCloneMut ma
  a `shouldBe` fromListSBArray [Nothing,Nothing,Just x,Nothing]

  writeSBMArray ma 2 (raiseImprecise ExpectedException) `shouldThrow` impreciseExpectedException
  freezeCloneMut ma `shouldReturn` fromListSBArray [Nothing,Nothing,Just x,Nothing]

  writeSBMArray ma 3 (Just (raiseImprecise ExpectedException))
  deepevalM (readSBMArray ma 3) `shouldThrow` impreciseExpectedException
  deepevalM (freezeSBMArray ma) `shouldThrow` impreciseExpectedException

prop_shrinkSBMArray :: HasCallStack => SBArray Integer -> NonNegative Size -> Property
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
    prop "writeBArrayException" prop_writeBArrayException
    prop "shrinkBMArray" prop_shrinkBMArray
  describe "SBArray" $ do
    arrayLawsSpec (Proxy :: Proxy (SBArray Char))
    lawsSpec $ functorLaws (Proxy :: Proxy SBArray)
    lawsSpec $ foldableLaws (Proxy :: Proxy SBArray)
    prop "writeSBArrayException" prop_writeSBArrayException
    prop "shrinkSBMArray" prop_shrinkSBMArray
  describe "UArray" $ do
    arrayLawsSpec (Proxy :: Proxy (UArray Char))
