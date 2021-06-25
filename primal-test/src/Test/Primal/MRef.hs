{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Test.Primal.MArray
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Test.Primal.MRef where

import Data.Foldable as F
import Control.Concurrent.Async
import Primal.Exception (impureThrow)
import Primal.Monad
import Primal.Unbox
import Data.Bits
import Primal.Container.Ref
import Test.Primal.Common

forAllIO :: (Show p, Testable t) => Gen p -> (p -> IO t) -> Property
forAllIO g propM = forAll g (propIO . propM)


forAllMRef ::
     forall mr e a. (Show e, Testable a, MRef mr e)
  => Gen e
  -> (e -> mr e RW -> IO a)
  -> Property
forAllMRef g propM = forAllIO g $ \v -> newMRef v >>= propM v

prop_newReadMRef ::
     forall mr e. (Eq e, Show e, MRef mr e)
  => e
  -> Property
prop_newReadMRef x = propIO $ do
  ref <- newMRef @mr @e x
  readMRef ref `shouldReturn` x

prop_writeReadMRef ::
     forall mr e. (Eq e, Show e, Arbitrary e, MRef mr e)
  => e
  -> Property
prop_writeReadMRef x = forAllMRef @mr @e arbitrary $ \ e ref -> do
  e' <- readMRef ref
  e' `shouldBe` e
  expectWriteReadMRef ref x

expectWriteReadMRef ::
     (Eq e, Show e, MRef mr e)
  => mr e RW
  -> e
  -> Expectation
expectWriteReadMRef mut e' = do
  writeMRef mut e'
  -- ensure roundtrip
  readMRef mut `shouldReturn` e'
  -- ensure argument is strict
  shouldThrow (writeMRef mut (impureThrow ExpectedException)) (ExpectedException ==)
  -- ensure survival of previous value
  readMRef mut `shouldReturn` e'


prop_writeReadAtomicMRef ::
     forall mr e. (Eq e, Show e, Arbitrary e, AtomicMRef mr e)
  => e
  -> Property
prop_writeReadAtomicMRef x =
  forAllMRef @mr @e arbitrary $ \ e ref -> do
    e' <- atomicReadMRef ref
    e' `shouldBe` e
    expectWriteReadAtomicMRef ref x


expectWriteReadAtomicMRef ::
     (Eq e, Show e, AtomicMRef mr e)
  => mr e RW
  -> e
  -> Expectation
expectWriteReadAtomicMRef mut e' = do
  atomicWriteMRef mut e'
  -- ensure roundtrip
  atomicReadMRef mut `shouldReturn` e'
  -- ensure argument is strict
  shouldThrow (atomicWriteMRef mut (impureThrow ExpectedException)) (ExpectedException ==)
  -- ensure survival of previous value
  readMRef mut `shouldReturn` e'


prop_opFetchOldMRef ::
     forall mr e. (Eq e, Show e, Arbitrary e, MRef mr e)
  => (forall m s . MonadPrim s m => mr e s -> e -> m e) -- ^ Monadic version
  -> (e -> e -> e) -- ^ Pure version
  -> e
  -> Property
prop_opFetchOldMRef opMRef op x =
  forAllMRef @mr @e arbitrary $ \ e ref -> do
    let y = e `op` x
    x' <- opMRef ref x
    x' `shouldBe` e
    readMRef ref `shouldReturn` y
    -- ensure argument is strict
    shouldThrow (opMRef ref (impureThrow ExpectedException)) (ExpectedException ==)
    -- ensure survival of previous value
    readMRef ref `shouldReturn` y

prop_opFetchNewMRef ::
     forall mr e. (Eq e, Show e, Arbitrary e, MRef mr e)
  => (forall m s . MonadPrim s m => mr e s -> e -> m e) -- ^ Monadic version
  -> (e -> e -> e) -- ^ Pure version
  -> e
  -> Property
prop_opFetchNewMRef opMRef op x =
  forAllMRef @mr @e arbitrary $ \ e ref -> do
    let y = e `op` x
    x' <- opMRef ref x
    x' `shouldBe` y
    readMRef ref `shouldReturn` y
    -- ensure argument is strict
    shouldThrow (opMRef ref (impureThrow ExpectedException)) (ExpectedException ==)
    -- ensure survival of previous value
    readMRef ref `shouldReturn` y

prop_CASMRef ::
     forall mr e. (Eq e, Show e, Arbitrary e, AtomicMRef mr e)
  => e
  -> e
  -> Property
prop_CASMRef x y = forAllMRef @mr @e arbitrary $ \ e' ref -> do
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
     forall mr e. (Eq e, Show e, Arbitrary e, AtomicMRef mr e)
  => (forall m s . MonadPrim s m => mr e s -> e -> m e) -- ^ Monadic version
  -> (e -> e -> e) -- ^ Pure version
  -> Property
asyncAssociativeProp opMRef op =
  asyncProp resExp modifyMRef_ atomicModifyMRef_ opMRef op
  where
    resExp (x, xs) (x', _xs') (y', _ys') (z', _zs') = do
      x' `shouldBe` y'
          -- ensure final value is the same
      x' `shouldBe` z'
      x' `shouldBe` F.foldr' op x xs

asyncAssociativeCommutativeOldProp ::
     forall mr e. (Eq e, Show e, Arbitrary e, AtomicMRef mr e)
  => (forall m s . MonadPrim s m => mr e s -> e -> m e) -- ^ Monadic version
  -> (e -> e -> e) -- ^ Pure version
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

-- asyncAssociativeCommutativeNewProp ::
--      forall mr e. (Eq e, Show e, Arbitrary e, AtomicMRef mr e)
--   => (forall m s . MonadPrim s m => mr e s -> e -> m e) -- ^ Monadic version
--   -> (e -> e -> e) -- ^ Pure version
--   -> Property
-- asyncAssociativeCommutativeNewProp opMRef op =
--   asyncProp resExp modifyFetchNewMRef atomicModifyFetchNewMRef opMRef op
--   where resExp (x, xs) (x', xs') (y', ys') (z', zs') = do
--           x' `shouldBe` y'
--           -- ensure final value is the same
--           x' `shouldBe` z'
--           x' `shouldBe` F.foldr' op x xs
--           let w = F.foldl' op x xs
--           F.foldl' op x xs' `shouldBe` w
--           F.foldl' op x ys' `shouldBe` w
--           F.foldl' op x zs' `shouldBe` w


asyncProp ::
     forall mr e a. (Show e, Arbitrary e, AtomicMRef mr e)
  => ((e, [e]) ->
      (e, [e]) ->
      (e, [a]) ->
      (e, [a]) ->
      Expectation)
  -> (forall m s . MonadPrim s m => mr e s -> (e -> e) -> m a)
  -- ^ Modification
  -> (forall m s . MonadPrim s m => mr e s -> (e -> e) -> m a)
  -- ^ Atomic modification
  -> (forall m s . MonadPrim s m => mr e s -> e -> m e) -- ^ Monadic version
  -> (e -> e -> e) -- ^ Pure version
  -> Property
asyncProp extraExp modMRef atomicModMRef opMRef op =
  forAllMRef @mr @e arbitrary $ \x xref ->
    return $
    forAllIO (arbitrary @([e])) $ \xs -> do
      xs' <- mapConcurrently (opMRef xref) xs
      -- ensure order of operations did not affect the end result
      x' <- readMRef xref
      yref <- newMRef @mr @e x
      ys' <- mapConcurrently (atomicModMRef yref . op) xs
      -- ensure it matches the non-specialized modify
      y' <- readMRef yref
      zref <- newMRef @mr @e x
      zs' <- mapM (modMRef zref . op) xs
      -- ensure it matches the sequential non-specialized modify
      z' <- readMRef zref
      extraExp (x, xs) (x', xs') (y', ys') (z', zs')


spec ::
     forall mr e.
     ( Show e
     , Arbitrary e
     , AtomicCountMRef mr e
     , AtomicBitsMRef mr e
     , Typeable mr
     , Typeable e
     )
  => Spec
spec = do
  let mutTypeName = showsType (Proxy :: Proxy (mr e RW)) ""
  describe mutTypeName $ do
    specMRef @mr @e
    specAtomicMRef @mr @e
    specAtomicCountMRef @mr @e
    specAtomicBitsMRef @mr @e

specMRef ::
     forall mr e. (Eq e, Show e, Arbitrary e, MRef mr e)
  => Spec
specMRef =
  describe "MRef" $ do
    prop "newRead" $ prop_newReadMRef @mr @e
    prop "writeRead" $ prop_writeReadMRef @mr @e


specAtomicMRef ::
     forall mr e.
     ( Eq e
     , Show e
     , Arbitrary e
     , AtomicMRef mr e
     )
  => Spec
specAtomicMRef =
  describe "AtomicMRef" $ do
    prop "CASMRef" $ prop_CASMRef @mr @e
    prop "writeReadAtomic" $ prop_writeReadAtomicMRef @mr @e


specAtomicCountMRef ::
     forall mr e.
     (Eq e, Show e, Arbitrary e, AtomicCountMRef mr e)
  => Spec
specAtomicCountMRef =
  describe "AtomicCountMRef" $ do
    prop "atomicAddFetchOld" $ prop_opFetchOldMRef @mr @e atomicAddFetchOldMRef (+)
    prop "atomicAddFetchNew" $ prop_opFetchNewMRef @mr @e atomicAddFetchNewMRef (+)
    prop "atomicSubFetchOld" $ prop_opFetchOldMRef @mr @e atomicSubFetchOldMRef (-)
    prop "atomicSubFetchNew" $ prop_opFetchNewMRef @mr @e atomicSubFetchNewMRef (-)
    describe "Concurrent" $ do
      prop "atomicAddFetchOldMRef" $ asyncAssociativeProp @mr @e atomicAddFetchOldMRef (+)
      prop "atomicAddFetchNewMRef" $ asyncAssociativeProp @mr @e atomicAddFetchNewMRef (+)
      prop "atomicSubFetchOldMRef" $ asyncAssociativeProp @mr @e atomicSubFetchOldMRef subtract
      prop "atomicSubFetchNewMRef" $ asyncAssociativeProp @mr @e atomicSubFetchNewMRef subtract


specAtomicBitsMRef ::
     forall mr e. (Show e, Arbitrary e, AtomicBitsMRef mr e)
  => Spec
specAtomicBitsMRef = do
    describe "AtomicBitsMRef" $ do
      prop "atomicAndFetchOld" $ prop_opFetchOldMRef @mr @e atomicAndFetchOldMRef (.&.)
      prop "atomicAndFetchNew" $ prop_opFetchNewMRef @mr @e atomicAndFetchNewMRef (.&.)
      prop "atomicNandFetchOld" $
        prop_opFetchOldMRef @mr @e atomicNandFetchOldMRef (\x y -> complement (x .&. y))
      prop "atomicNandFetchNew" $
        prop_opFetchNewMRef @mr @e atomicNandFetchNewMRef (\x y -> complement (x .&. y))
      prop "atomicOrFetchOld" $ prop_opFetchOldMRef @mr @e atomicOrFetchOldMRef (.|.)
      prop "atomicOrFetchNew" $ prop_opFetchNewMRef @mr @e atomicOrFetchNewMRef (.|.)
      prop "atomicXorFetchOld" $ prop_opFetchOldMRef @mr @e atomicXorFetchOldMRef xor
      prop "atomicXorFetchNew" $ prop_opFetchNewMRef @mr @e atomicXorFetchNewMRef xor
      prop "atomicNotFetchOld" $ -- strictness is to simulate property applicable to above ops
        prop_opFetchOldMRef @mr @e (\ref !_ -> atomicNotFetchOldMRef ref) (\x !_ -> complement x)
      prop "atomicNotFetchNew" $
        prop_opFetchNewMRef @mr @e (\ref !_ -> atomicNotFetchNewMRef ref) (\x !_ -> complement x)
      describe "Concurrent" $ do
        prop "atomicXorFetchOldMRef" $ asyncAssociativeProp @mr @e atomicXorFetchOldMRef xor
        prop "atomicXorFetchNewMRef" $ asyncAssociativeProp @mr @e atomicXorFetchNewMRef xor

        prop "atomicAndFetchOldMRef" $
          asyncAssociativeCommutativeOldProp @mr @e atomicAndFetchOldMRef (.&.)
        -- prop "atomicAndFetchNewMRef" $
        --   asyncAssociativeCommutativeNewProp @mr @e atomicAndFetchNewMRef (.&.)
        prop "atomicOrFetchOldMRef" $
          asyncAssociativeCommutativeOldProp @mr @e atomicOrFetchOldMRef (.|.)
        -- prop "atomicOrFetchNewMRef" $
        --   asyncAssociativeCommutativeNewProp @mr @e atomicOrFetchNewMRef (.|.)

