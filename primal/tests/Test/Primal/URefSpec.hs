{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Primal.URefSpec (spec) where

import qualified Control.Concurrent as Base
import Control.Concurrent.Async
import Control.DeepSeq
import Data.Bits
import Data.Foldable as F
import Data.List (partition)
import Data.Maybe
import Primal.Concurrent
import Primal.Exception
import Primal.Mem.Weak
import Primal.Ref
import Primal.Element.Unbox
import System.Mem (performGC)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Function (apply)
import Test.QuickCheck.Monadic

forAllIO :: (Show p, Testable t) => Gen p -> (p -> IO t) -> Property
forAllIO g propM = forAll g $ \v -> monadicIO $ run $ propM v

forAllST :: (Show p, Testable t) => Gen p -> (forall s. p -> ST s t) -> Property
forAllST g propM = forAll g $ \v -> monadicST $ run $ propM v


forAllURefST ::
     (Show p, Unbox p, Testable t)
  => Gen p
  -> (forall s. p -> URef p s -> ST s t)
  -> Property
forAllURefST g propM = forAllST g $ \v -> newURef v >>= propM v

forAllURefIO ::
     (Show p, Unbox p, Testable t)
  => Gen p
  -> (p -> URef p RealWorld -> IO t)
  -> Property
forAllURefIO g propM = forAllIO g $ \v -> newURef v >>= propM v

propURefST ::
     (Show p, Unbox p, Testable t)
  => String
  -> Gen p
  -> (forall s. p -> URef p s -> ST s t)
  -> Spec
propURefST name gen action = prop name $ forAllURefST gen action

propURefIO ::
     (Show p, Unbox p, Testable t)
  => String
  -> Gen p
  -> (p -> URef p RealWorld -> IO t)
  -> Spec
propURefIO name gen action = prop name $ forAllURefIO gen action

specUnbox ::
     ( Show p
     , Eq p
     , Unbox p
     , Typeable p
     , Arbitrary p
     , CoArbitrary p
     , Function p
     )
  => p -- ^ Zero value
  -> Gen p
  -> (Gen p -> Spec)
  -> Spec
specUnbox defZero gen extraSpec =
  describe ("URef s " ++ showsType gen "") $ do
    propURefIO "readURef" gen $ \v pvar -- deepseq is used for coverage only
     -> pvar `deepseq` readURef pvar `shouldReturn` v
    propURefIO "writeURef/readURef" gen $ \_ pvar ->
      return $
      forAll gen $ \v -> do
        writeURef pvar v
        readURef pvar `shouldReturn` v
    propURefIO "newPinnedURef" gen $ \a var -> do
      pinnedVar <- newPinnedURef a
      (===) <$> readURef var <*> readURef pinnedVar
    propURefIO "newAlignedPinnedURef" gen $ \a var -> do
      pinnedVar <- newAlignedPinnedURef a
      (===) <$> readURef var <*> readURef pinnedVar
    propURefIO "modifyURef_" gen $ \a pvar ->
      return $
      forAll arbitrary $ \f -> do
        modifyURef_ pvar (apply f)
        readURef pvar `shouldReturn` apply f a
    propURefIO "modifyURef" gen $ \a pvar ->
      return $
      forAll arbitrary $ \f -> do
        let (a', b :: Int) = apply f a
        modifyURef pvar (apply f) `shouldReturn` b
        readURef pvar `shouldReturn` a'
    propURefIO "modifyFetchOldURef" gen $ \a pvar ->
      return $
      forAll arbitrary $ \f -> do
        modifyFetchOldURef pvar (apply f) `shouldReturn` a
        readURef pvar `shouldReturn` apply f a
    propURefIO "modifyFetchOldURefM" gen $ \a pvar ->
      return $
      forAllIO arbitrary $ \f -> do
        a' <-
          modifyFetchOldURefM pvar $ \a' -> do
            a' `shouldBe` a
            pure $ apply f a'
        a' `shouldBe` a
        readURef pvar `shouldReturn` apply f a
    propURefIO "modifyFetchNewURef" gen $ \a pvar ->
      return $
      forAll arbitrary $ \f ->
        modifyFetchNewURef pvar (apply f) `shouldReturn` apply f a
    propURefIO "modifyFetchNewURefM" gen $ \a pvar ->
      return $
      forAllIO arbitrary $ \f -> do
        a' <-
          modifyFetchNewURefM pvar $ \a' -> do
            a' `shouldBe` a
            pure $ apply f a'
        a' `shouldBe` apply f a
    propURefIO "modifyURefM_" gen $ \a pvar ->
      return $
      forAllIO arbitrary $ \f -> do
        modifyURefM_ pvar $ \a' -> do
          a' `shouldBe` a
          pure $ apply f a'
        readURef pvar `shouldReturn` apply f a
    propURefIO "swapURefs" gen $ \a avar ->
      return $
      forAllURefIO gen $ \b bvar -> do
        swapURefs avar bvar `shouldReturn` (a, b)
        readURef avar `shouldReturn` b
        readURef bvar `shouldReturn` a
    propURefIO "swapURefs_" gen $ \a avar ->
      return $
      forAllURefIO gen $ \b bvar -> do
        swapURefs_ avar bvar
        readURef avar `shouldReturn` b
        readURef bvar `shouldReturn` a
    describe "Reset Memory" $
      propURefIO "zeroURef" gen $ \_ var -> do
        zeroURef var
        readURef var `shouldReturn` defZero
    extraSpec gen


specAtomic ::
     forall e.
     ( Show e
     , Eq e
     , Unbox e
     , Num e
     , AtomicCount e
     , AtomicBits e
     , CoArbitrary e
     , Arbitrary e
     , Function e
     )
  => Gen e
  -> Spec
specAtomic gen = do
  let basicAtomicFetchOldProp name atomicFun fun =
        propURefIO name gen $ \x var ->
          return $
          forAllIO gen $ \y -> do
            atomicFun var y `shouldReturn` x
            atomicReadURef var `shouldReturn` (x `fun` y)
      basicAtomicFetchNewProp name atomicFun fun =
        propURefIO name gen $ \x var ->
          return $
          forAllIO gen $ \y -> do
            atomicFun var y `shouldReturn` (x `fun` y)
            atomicReadURef var `shouldReturn` (x `fun` y)
  describe "Atomic (basic)" $ do
    describe "Basic" $ do
      basicAtomicFetchOldProp "atomicAddFetchOldURef" atomicAddFetchOldURef (+)
      basicAtomicFetchNewProp "atomicAddFetchNewURef" atomicAddFetchNewURef (+)
      basicAtomicFetchOldProp "atomicSubFetchOldURef" atomicSubFetchOldURef (-)
      basicAtomicFetchNewProp "atomicSubFetchNewURef" atomicSubFetchNewURef (-)
      basicAtomicFetchOldProp "atomicAndFetchOldURef" atomicAndFetchOldURef $ \x y ->
        x .&. y
      basicAtomicFetchNewProp "atomicAndFetchNewURef" atomicAndFetchNewURef $ \x y ->
        x .&. y
      basicAtomicFetchOldProp "atomicNandFetchOldURef" atomicNandFetchOldURef $ \x y ->
        complement (x .&. y)
      basicAtomicFetchNewProp "atomicNandFetchNewURef" atomicNandFetchNewURef $ \x y ->
        complement (x .&. y)
      basicAtomicFetchOldProp "atomicOrFetchOldURef" atomicOrFetchOldURef $ \x y ->
        x .|. y
      basicAtomicFetchNewProp "atomicOrFetchNewURef" atomicOrFetchNewURef $ \x y ->
        x .|. y
      basicAtomicFetchOldProp "atomicXorFetchOldURef" atomicXorFetchOldURef $ \x y ->
        x `xor` y
      basicAtomicFetchNewProp "atomicXorFetchNewURef" atomicXorFetchNewURef $ \x y ->
        x `xor` y
      propURefIO "atomicNotFetchOldURef" gen $ \x var -> do
        atomicNotFetchOldURef var `shouldReturn` x
        atomicReadURef var `shouldReturn` complement x
      propURefIO "atomicNotFetchNewURef" gen $ \x var -> do
        atomicNotFetchNewURef var `shouldReturn` complement x
        atomicReadURef var `shouldReturn` complement x
    describe "Concurrent" $ do
      propURefIO "atomicAndFetchOldURef" gen $ \x var ->
        return $
        forAllIO (listOf gen) $ \xs -> do
          xs' <- mapConcurrently (atomicAndFetchOldURef var) xs
          x' <- atomicReadURef var
          F.foldl' (.&.) x' xs' `shouldBe` F.foldl' (.&.) x xs
      propURefIO "atomicOrFetchOldURef" gen $ \x var ->
        return $
        forAllIO (listOf gen) $ \xs -> do
          xs' <- mapConcurrently (atomicOrFetchOldURef var) xs
          x' <- atomicReadURef var
          F.foldl' (.|.) x' xs' `shouldBe` F.foldl' (.|.) x xs
    describe "CAS-Concurrent" $ do
      propURefIO "casURef" gen $ \x var ->
        return $
        forAllIO ((,) <$> gen <*> gen) $ \(y, z) -> do
          casURef var x y `shouldReturn` x
          atomicReadURef var `shouldReturn` y
          atomicWriteURef var z
          atomicReadURef var `shouldReturn` z
      casProp_ "atomicAddFetchOldURef" (+) atomicAddFetchOldURef
      casProp_ "atomicSubFetchOldURef" subtract atomicSubFetchOldURef
      casSymAssocProp "atomicAndFetchOldURef" (.&.) atomicAndFetchOldURef
      casSymAssocProp "atomicOrFetchOldURef" (.|.) atomicOrFetchOldURef
      casProp_ "atomicXorFetchOldURef" xor atomicXorFetchOldURef
      propURefIO "atomicNotURef" gen $ \x xvar ->
        return $
        forAllIO arbitrary $ \(Positive n) -> do
          xs' <-
            mapConcurrently (\_ -> atomicNotFetchOldURef xvar) [1 :: Int .. n]
          x' <- atomicReadURef xvar
          yvar <- newURef x
          ys' <-
            mapConcurrently
              (\_ -> atomicModifyFetchOldURef yvar complement)
              [1 .. n]
          y' <- atomicReadURef yvar
          x' `shouldBe` y'
          -- binary negation of N times results in two values, both of which happen N/2
          -- times
          let sxs@(l, r) = partition (== x) (x' : xs')
              lenr = length r
              sys = partition (== x) (y' : ys')
          sxs `shouldBe` sys
          length l `shouldSatisfy` (\len -> len == lenr || len == lenr + 1)
      prop "atomicModifyURef" $
        forAll gen $ \z ->
          forAllIO (arbitrary :: Gen (Fun (e, Int) e, [Int])) $ \(f, xs) -> do
            zvar <- newURef $ Atom (Nothing, z)
            let --g = applyFun2 f
                g y x = y `xor` fromIntegral x
            mxs <-
              mapConcurrently
                (\x ->
                   atomicModifyURef
                     zvar
                     (\(Atom (mPrev, a)) -> (Atom (Just x, g a x), mPrev)))
                xs
            Atom (mLast, z') <- atomicReadURef zvar
            let xs' = catMaybes (mxs ++ [mLast])
            xs' `shouldMatchList` xs
            z' `shouldBe` F.foldl' g z xs'
  where
    casProp_ name f af =
      propURefIO name gen $ \x xvar ->
        return $
        forAllIO (listOf gen) $ \xs -> do
          void $ mapConcurrently (af xvar) xs
          x' <- atomicReadURef xvar
          yvar <- newURef x
          void $ mapConcurrently (atomicModifyURef_ yvar . f) xs
          atomicReadURef yvar `shouldReturn` x'
    casSymAssocProp name f af =
      propURefIO name gen $ \x xvar ->
        return $
        forAllIO (listOf gen) $ \xs -> do
          xs' <- mapConcurrently (af xvar) xs
          x' <- atomicReadURef xvar
          yvar <- newURef x
          ys' <-
            mapConcurrently (\a -> atomicModifyFetchOldURef yvar (`f` a)) xs
          atomicReadURef yvar `shouldReturn` x'
          atomicWriteURef yvar x
          ys'' <-
            mapConcurrently (\a -> atomicModifyFetchNewURef yvar (`f` a)) xs
          atomicReadURef yvar `shouldReturn` x'
          let res = F.foldl' f x xs
          F.foldl' f x' xs' `shouldBe` res
          F.foldl' f x' ys' `shouldBe` res
          F.foldl' f x ys'' `shouldBe` res

spec :: Spec
spec = do
  specUnbox 0 (arbitrary :: Gen Int) specAtomic
  specUnbox 0 (arbitrary :: Gen Int8) specAtomic
  specUnbox 0 (arbitrary :: Gen Int16) specAtomic
  specUnbox 0 (arbitrary :: Gen Int32) specAtomic
  specUnbox 0 (arbitrary :: Gen Int64) specAtomic
  specUnbox 0 (arbitrary :: Gen Word) specAtomic
  specUnbox 0 (arbitrary :: Gen Word8) specAtomic
  specUnbox 0 (arbitrary :: Gen Word16) specAtomic
  specUnbox 0 (arbitrary :: Gen Word32) specAtomic
  specUnbox 0 (arbitrary :: Gen Word64) specAtomic
  specUnbox '\0' (arbitrary :: Gen Char) (const (pure ()))
  specUnbox 0 (arbitrary :: Gen Float) (const (pure ()))
  specUnbox 0 (arbitrary :: Gen Double) (const (pure ()))
