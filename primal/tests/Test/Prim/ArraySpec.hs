{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Prim.ArraySpec (spec) where

import Data.Prim
import Foreign.Prim (IsList(..))
import Control.Prim.Exception
import Control.Prim.Eval
import Data.Prim.Array
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Classes.Base
import Test.Hspec
import Test.Hspec.QuickCheck
import Control.DeepSeq

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


instance (Prim e, Arbitrary e) => Arbitrary (UArray e) where
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
  a <- freezeBMArray ma
  a `shouldBe` fromListBArray [Nothing,Nothing,Just x,Nothing]

  writeBMArray ma 2 (impureThrow DivideByZero) `shouldThrow` (== DivideByZero)
  freezeBMArray ma `shouldReturn` fromListBArray [Nothing,Nothing,Just x,Nothing]

  writeBMArray ma 3 (Just (x `div` 0))
  deepevalM (readBMArray ma 3) `shouldThrow` (== DivideByZero)
  deepevalM (freezeBMArray ma) `shouldThrow` (== DivideByZero)


spec :: Spec
spec = do
  describe "BArray" $ do
    arrayLawsSpec (Proxy :: Proxy (BArray Char))
    lawsSpec $ functorLaws (Proxy :: Proxy BArray)
    lawsSpec $ foldableLaws (Proxy :: Proxy BArray)
    prop "prop_writeBArrayException" prop_writeBArrayException
  describe "SBArray" $ do
    arrayLawsSpec (Proxy :: Proxy (SBArray Char))
    lawsSpec $ functorLaws (Proxy :: Proxy SBArray)
    lawsSpec $ foldableLaws (Proxy :: Proxy SBArray)
  describe "UArray" $ do
    arrayLawsSpec (Proxy :: Proxy (UArray Char))
