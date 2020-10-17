{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Test.Prim.ArraySpec (spec) where

import Data.Prim
import Foreign.Prim (IsList(..))
import Data.Prim.Array
import Test.QuickCheck
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

spec :: Spec
spec = do
  describe "BArray" $ do
    arrayLawsSpec (Proxy :: Proxy (BArray Char))
    lawsSpec $ functorLaws (Proxy :: Proxy BArray)
    lawsSpec $ foldableLaws (Proxy :: Proxy BArray)
  describe "SBArray" $ do
    arrayLawsSpec (Proxy :: Proxy (SBArray Char))
    lawsSpec $ functorLaws (Proxy :: Proxy SBArray)
    lawsSpec $ foldableLaws (Proxy :: Proxy SBArray)
  describe "UArray" $ do
    arrayLawsSpec (Proxy :: Proxy (UArray Char))
