{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
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
module Test.Prim.MRef where

import Data.Foldable as F
import Control.Concurrent.Async
import Control.Exception (throw)
import Control.Prim.Monad
import Data.Bits
import Data.Prim
import Data.Prim.MRef
import Test.Prim.Common

forAllIO :: (Show p, Testable t) => Gen p -> (p -> IO t) -> Property
forAllIO g propM = forAll g (propIO . propM)


forAllMRef ::
     forall mut a. (Show (Elt mut), Testable a, MRef mut)
  => Gen (Elt mut)
  -> (Elt mut -> mut RW -> IO a)
  -> Property
forAllMRef g propM = forAllIO g $ \v -> newMRef v >>= propM v

prop_newReadMRef ::
     forall mut. (Eq (Elt mut), Show (Elt mut), MRef mut)
  => Elt mut
  -> Property
prop_newReadMRef x = propIO $ do
  ref <- newMRef @mut x
  readMRef ref `shouldReturn` x

prop_writeReadMRef ::
     forall mut. (Eq (Elt mut), Show (Elt mut), Arbitrary (Elt mut), MRef mut)
  => Elt mut
  -> Property
prop_writeReadMRef x = forAllMRef @mut arbitrary $ \ e ref -> do
  e' <- readMRef ref
  e' `shouldBe` e
  expectWriteReadMRef ref x

expectWriteReadMRef ::
     (Eq (Elt mut), Show (Elt mut), MRef mut)
  => mut RW
  -> Elt mut
  -> Expectation
expectWriteReadMRef mut e' = do
  writeMRef mut e'
  -- ensure roundtrip
  readMRef mut `shouldReturn` e'
  -- ensure argument is strict
  shouldThrow (writeMRef mut (throw ExpectedException)) (ExpectedException ==)
  -- ensure survival of previous value
  readMRef mut `shouldReturn` e'


prop_writeReadAtomicMRef ::
     forall mut. (Eq (Elt mut), Show (Elt mut), Arbitrary (Elt mut), AtomicMRef mut)
  => Elt mut
  -> Property
prop_writeReadAtomicMRef x = forAllMRef @mut arbitrary $ \ e ref -> do
  e' <- atomicReadMRef ref
  e' `shouldBe` e
  expectWriteReadAtomicMRef ref x


expectWriteReadAtomicMRef ::
     (Eq (Elt mut), Show (Elt mut), AtomicMRef mut)
  => mut RW
  -> Elt mut
  -> Expectation
expectWriteReadAtomicMRef mut e' = do
  atomicWriteMRef mut e'
  -- ensure roundtrip
  atomicReadMRef mut `shouldReturn` e'
  -- ensure argument is strict
  shouldThrow (atomicWriteMRef mut (throw ExpectedException)) (ExpectedException ==)
  -- ensure survival of previous value
  readMRef mut `shouldReturn` e'


prop_opFetchOldMRef ::
     forall mut. (Eq (Elt mut), Show (Elt mut), Arbitrary (Elt mut), MRef mut)
  => (forall m s . MonadPrim s m => mut s -> Elt mut -> m (Elt mut)) -- ^ Monadic version
  -> (Elt mut -> Elt mut -> Elt mut) -- ^ Pure version
  -> Elt mut
  -> Property
prop_opFetchOldMRef opMRef op x =
  forAllMRef @mut arbitrary $ \ e ref -> do
    let y = e `op` x
    x' <- opMRef ref x
    x' `shouldBe` e
    readMRef ref `shouldReturn` y
    -- ensure argument is strict
    shouldThrow (opMRef ref (throw ExpectedException)) (ExpectedException ==)
    -- ensure survival of previous value
    readMRef ref `shouldReturn` y

prop_opFetchNewMRef ::
     forall mut. (Eq (Elt mut), Show (Elt mut), Arbitrary (Elt mut), MRef mut)
  => (forall m s . MonadPrim s m => mut s -> Elt mut -> m (Elt mut)) -- ^ Monadic version
  -> (Elt mut -> Elt mut -> Elt mut) -- ^ Pure version
  -> Elt mut
  -> Property
prop_opFetchNewMRef opMRef op x =
  forAllMRef @mut arbitrary $ \ e ref -> do
    let y = e `op` x
    x' <- opMRef ref x
    x' `shouldBe` y
    readMRef ref `shouldReturn` y
    -- ensure argument is strict
    shouldThrow (opMRef ref (throw ExpectedException)) (ExpectedException ==)
    -- ensure survival of previous value
    readMRef ref `shouldReturn` y

prop_CASMRef ::
     forall mut. (Eq (Elt mut), Show (Elt mut), Arbitrary (Elt mut), AtomicMRef mut)
  => Elt mut
  -> Elt mut
  -> Property
prop_CASMRef x y = forAllMRef @mut arbitrary $ \ e' ref -> do
  e <- readMRef ref
  e `shouldBe` e'
  (isSucc, x') <- casMRef ref e y
  -- ensure successful CAS
  isSucc `shouldBe` True
  x' `shouldBe` y
  readMRef ref `shouldReturn` y
  when (x /= y) $ do
    (isSucc', y') <- casMRef ref x e
    -- ensure failed CAS
    isSucc' `shouldBe` False
    y' `shouldBe` y
    readMRef ref `shouldReturn` y

asyncAssociativeProp ::
     forall mut. (Eq (Elt mut), Show (Elt mut), Arbitrary (Elt mut), AtomicMRef mut)
  => (forall m s . MonadPrim s m => mut s -> Elt mut -> m (Elt mut)) -- ^ Monadic version
  -> (Elt mut -> Elt mut -> Elt mut) -- ^ Pure version
  -> Property
asyncAssociativeProp opMRef op = asyncProp resExp modifyMRef_ atomicModifyMRef_ opMRef op
  where resExp (x, xs) (x', _xs') (y', _ys') (z', _zs') = do
          x' `shouldBe` y'
          -- ensure final value is the same
          x' `shouldBe` z'
          x' `shouldBe` F.foldr' op x xs

asyncAssociativeCommutativeOldProp ::
     forall mut. (Eq (Elt mut), Show (Elt mut), Arbitrary (Elt mut), AtomicMRef mut)
  => (forall m s . MonadPrim s m => mut s -> Elt mut -> m (Elt mut)) -- ^ Monadic version
  -> (Elt mut -> Elt mut -> Elt mut) -- ^ Pure version
  -> Property
asyncAssociativeCommutativeOldProp opMRef op =
  asyncProp resExp modifyFetchOldMRef atomicModifyFetchOldMRef opMRef op
  where resExp (x, xs) (x', xs') (y', ys') (z', zs') = do
          x' `shouldBe` y'
          -- ensure final value is the same
          x' `shouldBe` z'
          x' `shouldBe` F.foldr' op x xs
          let w = F.foldl' op x xs
          F.foldl' op x' xs' `shouldBe` w
          F.foldl' op y' ys' `shouldBe` w
          F.foldl' op z' zs' `shouldBe` w

asyncAssociativeCommutativeNewProp ::
     forall mut. (Eq (Elt mut), Show (Elt mut), Arbitrary (Elt mut), AtomicMRef mut)
  => (forall m s . MonadPrim s m => mut s -> Elt mut -> m (Elt mut)) -- ^ Monadic version
  -> (Elt mut -> Elt mut -> Elt mut) -- ^ Pure version
  -> Property
asyncAssociativeCommutativeNewProp opMRef op =
  asyncProp resExp modifyFetchNewMRef atomicModifyFetchNewMRef opMRef op
  where resExp (x, xs) (x', xs') (y', ys') (z', zs') = do
          x' `shouldBe` y'
          -- ensure final value is the same
          x' `shouldBe` z'
          x' `shouldBe` F.foldr' op x xs
          let w = F.foldl' op x xs
          F.foldl' op x xs' `shouldBe` w
          F.foldl' op x ys' `shouldBe` w
          F.foldl' op x zs' `shouldBe` w


asyncProp ::
     forall mut a. (Show (Elt mut), Arbitrary (Elt mut), AtomicMRef mut)
  => ((Elt mut, [Elt mut]) ->
      (Elt mut, [Elt mut]) ->
      (Elt mut, [a]) ->
      (Elt mut, [a]) ->
      Expectation)
  -> (forall m s . MonadPrim s m => mut s -> (Elt mut -> Elt mut) -> m a)
  -- ^ Modification
  -> (forall m s . MonadPrim s m => mut s -> (Elt mut -> Elt mut) -> m a)
  -- ^ Atomic modification
  -> (forall m s . MonadPrim s m => mut s -> Elt mut -> m (Elt mut)) -- ^ Monadic version
  -> (Elt mut -> Elt mut -> Elt mut) -- ^ Pure version
  -> Property
asyncProp extraExp modMRef atomicModMRef opMRef op =
  forAllMRef @mut arbitrary $ \x xref ->
    return $
    forAllIO (arbitrary @([Elt mut])) $ \xs -> do
      xs' <- mapConcurrently (opMRef xref) xs
      -- ensure order of operations did not affect the end result
      x' <- readMRef xref
      yref <- newMRef @mut x
      ys' <- mapConcurrently (atomicModMRef yref . op) xs
      -- ensure it matches the non-specialized modify
      y' <- readMRef yref
      zref <- newMRef @mut x
      zs' <- mapM (modMRef zref . op) xs
      -- ensure it matches the sequential non-specialized modify
      z' <- readMRef zref
      extraExp (x, xs) (x', xs') (y', ys') (z', zs')


spec ::
     forall mut.
     ( Show (Elt mut)
     , Arbitrary (Elt mut)
     , AtomicCountMRef mut
     , AtomicBitsMRef mut
     , Typeable mut
     )
  => Spec
spec = do
  let mutTypeName = showsType (Proxy :: Proxy (mut RW)) ""
  describe mutTypeName $ do
    specMRef @mut
    specAtomicMRef @mut
    specAtomicCountMRef @mut
    specAtomicBitsMRef @mut

specMRef ::
     forall mut. (Eq (Elt mut), Show (Elt mut), Arbitrary (Elt mut), MRef mut)
  => Spec
specMRef =
  describe "MRef" $ do
    prop "newRead" $ prop_newReadMRef @mut
    prop "writeRead" $ prop_writeReadMRef @mut


specAtomicMRef ::
     forall mut.
     ( Eq (Elt mut)
     , Show (Elt mut)
     , Arbitrary (Elt mut)
     , AtomicMRef mut
     )
  => Spec
specAtomicMRef =
  describe "AtomicMRef" $ do
    prop "CASMRef" $ prop_CASMRef @mut
    prop "writeReadAtomic" $ prop_writeReadAtomicMRef @mut


specAtomicCountMRef ::
     forall mut.
     (Eq (Elt mut), Show (Elt mut), Arbitrary (Elt mut), AtomicCountMRef mut)
  => Spec
specAtomicCountMRef =
  describe "AtomicCountMRef" $ do
    prop "atomicAddFetchOld" $ prop_opFetchOldMRef @mut atomicAddFetchOldMRef (+)
    prop "atomicAddFetchNew" $ prop_opFetchNewMRef @mut atomicAddFetchNewMRef (+)
    prop "atomicSubFetchOld" $ prop_opFetchOldMRef @mut atomicSubFetchOldMRef (-)
    prop "atomicSubFetchNew" $ prop_opFetchNewMRef @mut atomicSubFetchNewMRef (-)
    describe "Concurrent" $ do
      prop "atomicAddFetchOldMRef" $ asyncAssociativeProp @mut atomicAddFetchOldMRef (+)
      prop "atomicAddFetchNewMRef" $ asyncAssociativeProp @mut atomicAddFetchNewMRef (+)
      prop "atomicSubFetchOldMRef" $ asyncAssociativeProp @mut atomicSubFetchOldMRef subtract
      prop "atomicSubFetchNewMRef" $ asyncAssociativeProp @mut atomicSubFetchNewMRef subtract


specAtomicBitsMRef ::
     forall mut. (Show (Elt mut), Arbitrary (Elt mut), AtomicBitsMRef mut)
  => Spec
specAtomicBitsMRef = do
    describe "AtomicBitsMRef" $ do
      prop "atomicAndFetchOld" $ prop_opFetchOldMRef @mut atomicAndFetchOldMRef (.&.)
      prop "atomicAndFetchNew" $ prop_opFetchNewMRef @mut atomicAndFetchNewMRef (.&.)
      prop "atomicNandFetchOld" $
        prop_opFetchOldMRef @mut atomicNandFetchOldMRef (\x y -> complement (x .&. y))
      prop "atomicNandFetchNew" $
        prop_opFetchNewMRef @mut atomicNandFetchNewMRef (\x y -> complement (x .&. y))
      prop "atomicOrFetchOld" $ prop_opFetchOldMRef @mut atomicOrFetchOldMRef (.|.)
      prop "atomicOrFetchNew" $ prop_opFetchNewMRef @mut atomicOrFetchNewMRef (.|.)
      prop "atomicXorFetchOld" $ prop_opFetchOldMRef @mut atomicXorFetchOldMRef xor
      prop "atomicXorFetchNew" $ prop_opFetchNewMRef @mut atomicXorFetchNewMRef xor
      prop "atomicNotFetchOld" $ -- strictness is to simulate property applicable to above ops
        prop_opFetchOldMRef @mut (\ref !_ -> atomicNotFetchOldMRef ref) (\x !_ -> complement x)
      prop "atomicNotFetchNew" $
        prop_opFetchNewMRef @mut (\ref !_ -> atomicNotFetchNewMRef ref) (\x !_ -> complement x)
      describe "Concurrent" $ do
        prop "atomicXorFetchOldMRef" $ asyncAssociativeProp @mut atomicXorFetchOldMRef xor
        prop "atomicXorFetchNewMRef" $ asyncAssociativeProp @mut atomicXorFetchNewMRef xor

        prop "atomicAndFetchOldMRef" $
          asyncAssociativeCommutativeOldProp @mut atomicAndFetchOldMRef (.&.)
        prop "atomicAndFetchNewMRef" $
          asyncAssociativeCommutativeNewProp @mut atomicAndFetchNewMRef (.&.)
        prop "atomicOrFetchOldMRef" $
          asyncAssociativeCommutativeOldProp @mut atomicOrFetchOldMRef (.|.)
        prop "atomicOrFetchNewMRef" $
          asyncAssociativeCommutativeNewProp @mut atomicOrFetchNewMRef (.|.)

