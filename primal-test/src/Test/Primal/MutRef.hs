{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Test.Primal.MutRef
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module Test.Primal.MutRef where

import Control.Concurrent.Async
import Data.Bits
import Data.Foldable as F
import Primal.Container.Ref
import Primal.Element.Unbox
import Primal.Exception (raiseImprecise)
import Primal.Monad
import Test.Primal.Common

forAllIO :: (Show p, Testable t) => Gen p -> (p -> IO t) -> Property
forAllIO g propM = forAll g (propIO . propM)

forAllMutRef
  :: forall mr e a
   . (Show e, Testable a, Elt mr e, MutRef mr)
  => Gen e
  -> (e -> mr e RW -> IO a)
  -> Property
forAllMutRef g propM = forAllIO g $ \v -> newMutRef v >>= propM v

prop_newReadMutRef
  :: forall mr e
   . (Eq e, Show e, Elt mr e, MutRef mr)
  => e
  -> Property
prop_newReadMutRef x = propIO $ do
  ref <- newMutRef @mr @e x
  readMutRef ref `shouldReturn` x

prop_writeReadMutRef
  :: forall mr e
   . (Eq e, Show e, Arbitrary e, Elt mr e, MutRef mr)
  => e
  -> Property
prop_writeReadMutRef x = forAllMutRef @mr @e arbitrary $ \e ref -> do
  e' <- readMutRef ref
  e' `shouldBe` e
  expectWriteReadMutRef ref x

expectWriteReadMutRef
  :: (Eq e, Show e, Elt mr e, MutRef mr)
  => mr e RW
  -> e
  -> Expectation
expectWriteReadMutRef mut e' = do
  writeMutRef mut e'
  -- ensure roundtrip
  readMutRef mut `shouldReturn` e'
  -- ensure argument is strict
  shouldThrow (writeMutRef mut (raiseImprecise ExpectedException)) impreciseExpectedException
  -- ensure survival of previous value
  readMutRef mut `shouldReturn` e'

prop_writeReadAtomicMutRef
  :: forall mr e
   . (Eq e, Show e, Arbitrary e, Elt mr e, AtomicElt mr e, AtomicMutRef mr)
  => e
  -> Property
prop_writeReadAtomicMutRef x =
  forAllMutRef @mr @e arbitrary $ \e ref -> do
    e' <- atomicReadMutRef ref
    e' `shouldBe` e
    expectWriteReadAtomicMutRef ref x

expectWriteReadAtomicMutRef
  :: (Eq e, Show e, Elt mr e, AtomicElt mr e, AtomicMutRef mr)
  => mr e RW
  -> e
  -> Expectation
expectWriteReadAtomicMutRef mut e' = do
  atomicWriteMutRef mut e'
  -- ensure roundtrip
  atomicReadMutRef mut `shouldReturn` e'
  -- ensure argument is strict
  shouldThrow (atomicWriteMutRef mut (raiseImprecise ExpectedException)) impreciseExpectedException
  -- ensure survival of previous value
  readMutRef mut `shouldReturn` e'

prop_opFetchOldMutRef
  :: forall mr e
   . (Eq e, Show e, Arbitrary e, Elt mr e, MutRef mr)
  => (forall m s. Primal s m => mr e s -> e -> m e)
  -- ^ Monadic version
  -> (e -> e -> e)
  -- ^ Pure version
  -> e
  -> Property
prop_opFetchOldMutRef opMutRef op x =
  forAllMutRef @mr @e arbitrary $ \e ref -> do
    let y = e `op` x
    x' <- opMutRef ref x
    x' `shouldBe` e
    readMutRef ref `shouldReturn` y
    -- ensure argument is strict
    shouldThrow (opMutRef ref (raiseImprecise ExpectedException)) impreciseExpectedException
    -- ensure survival of previous value
    readMutRef ref `shouldReturn` y

prop_opFetchNewMutRef
  :: forall mr e
   . (Eq e, Show e, Arbitrary e, Elt mr e, MutRef mr)
  => (forall m s. Primal s m => mr e s -> e -> m e)
  -- ^ Monadic version
  -> (e -> e -> e)
  -- ^ Pure version
  -> e
  -> Property
prop_opFetchNewMutRef opMutRef op x =
  forAllMutRef @mr @e arbitrary $ \e ref -> do
    let y = e `op` x
    x' <- opMutRef ref x
    x' `shouldBe` y
    readMutRef ref `shouldReturn` y
    -- ensure argument is strict
    shouldThrow (opMutRef ref (raiseImprecise ExpectedException)) impreciseExpectedException
    -- ensure survival of previous value
    readMutRef ref `shouldReturn` y

prop_CASMutRef
  :: forall mr e
   . (Eq e, Show e, Arbitrary e, Elt mr e, AtomicElt mr e, AtomicMutRef mr)
  => e
  -> e
  -> Property
prop_CASMutRef x y = forAllMutRef @mr @e arbitrary $ \e' ref -> do
  e <- readMutRef ref
  e `shouldBe` e'
  (isSucc, x') <- casMutRef ref e y
  -- ensure successful CAS
  isSucc `shouldBe` True
  x' `shouldBe` y
  readMutRef ref `shouldReturn` y
  when (x /= y) $ do
    (isSucc', y') <- casMutRef ref x e
    -- ensure failed CAS
    isSucc' `shouldBe` False
    y' `shouldBe` y
    readMutRef ref `shouldReturn` y

asyncAssociativeProp
  :: forall mr e
   . (Eq e, Show e, Arbitrary e, Elt mr e, AtomicElt mr e, AtomicMutRef mr)
  => (forall m s. Primal s m => mr e s -> e -> m e)
  -- ^ Monadic version
  -> (e -> e -> e)
  -- ^ Pure version
  -> Property
asyncAssociativeProp opMutRef op =
  asyncProp resExp modifyMutRef_ atomicModifyMutRef_ opMutRef op
  where
    resExp (x, xs) (x', _xs') (y', _ys') (z', _zs') = do
      x' `shouldBe` y'
      -- ensure final value is the same
      x' `shouldBe` z'
      x' `shouldBe` F.foldr' op x xs

asyncAssociativeCommutativeOldProp
  :: forall mr e
   . (Eq e, Show e, Arbitrary e, Elt mr e, AtomicElt mr e, AtomicMutRef mr)
  => (forall m s. Primal s m => mr e s -> e -> m e)
  -- ^ Monadic version
  -> (e -> e -> e)
  -- ^ Pure version
  -> Property
asyncAssociativeCommutativeOldProp opMutRef op =
  asyncProp resExp modifyFetchOldMutRef atomicModifyFetchOldMutRef opMutRef op
  where
    resExp (x, xs) (x', xs') (y', ys') (z', zs') = do
      x' `shouldBe` y'
      -- ensure final value is the same
      x' `shouldBe` z'
      x' `shouldBe` F.foldr' op x xs
      let w = F.foldl' op x xs
      F.foldl' op x' xs' `shouldBe` w
      F.foldl' op y' ys' `shouldBe` w
      F.foldl' op z' zs' `shouldBe` w

-- asyncAssociativeCommutativeNewProp ::
--      forall mr e. (Eq e, Show e, Arbitrary e, AtomicMutRef mr e)
--   => (forall m s . Primal s m => mr e s -> e -> m e) -- ^ Monadic version
--   -> (e -> e -> e) -- ^ Pure version
--   -> Property
-- asyncAssociativeCommutativeNewProp opMutRef op =
--   asyncProp resExp modifyFetchNewMutRef atomicModifyFetchNewMutRef opMutRef op
--   where resExp (x, xs) (x', xs') (y', ys') (z', zs') = do
--           x' `shouldBe` y'
--           -- ensure final value is the same
--           x' `shouldBe` z'
--           x' `shouldBe` F.foldr' op x xs
--           let w = F.foldl' op x xs
--           F.foldl' op x xs' `shouldBe` w
--           F.foldl' op x ys' `shouldBe` w
--           F.foldl' op x zs' `shouldBe` w

asyncProp
  :: forall mr e a
   . (Show e, Arbitrary e, AtomicElt mr e, Elt mr e, AtomicMutRef mr)
  => ( (e, [e])
       -> (e, [e])
       -> (e, [a])
       -> (e, [a])
       -> Expectation
     )
  -> (forall m s. Primal s m => mr e s -> (e -> e) -> m a)
  -- ^ Modification
  -> (forall m s. Primal s m => mr e s -> (e -> e) -> m a)
  -- ^ Atomic modification
  -> (forall m s. Primal s m => mr e s -> e -> m e)
  -- ^ Monadic version
  -> (e -> e -> e)
  -- ^ Pure version
  -> Property
asyncProp extraExp modMutRef atomicModMutRef opMutRef op =
  forAllMutRef @mr @e arbitrary $ \x xref ->
    return $
      forAllIO (arbitrary @([e])) $ \xs -> do
        xs' <- mapConcurrently (opMutRef xref) xs
        -- ensure order of operations did not affect the end result
        x' <- readMutRef xref
        yref <- newMutRef @mr @e x
        ys' <- mapConcurrently (atomicModMutRef yref . op) xs
        -- ensure it matches the non-specialized modify
        y' <- readMutRef yref
        zref <- newMutRef @mr @e x
        zs' <- mapM (modMutRef zref . op) xs
        -- ensure it matches the sequential non-specialized modify
        z' <- readMutRef zref
        extraExp (x, xs) (x', xs') (y', ys') (z', zs')

spec
  :: forall mr e
   . ( Show e
     , Bits e
     , Num e
     , Arbitrary e
     , AtomicCountMutRef mr
     , AtomicCountElt mr e
     , AtomicBitsMutRef mr
     , AtomicBitsElt mr e
     , Elt mr e
     , AtomicElt mr e
     , Typeable mr
     , Typeable e
     )
  => Spec
spec = do
  let mutTypeName = showsType (Proxy :: Proxy (mr e RW)) ""
  describe mutTypeName $ do
    specMutRef @mr @e
    specAtomicMutRef @mr @e
    specAtomicCountMutRef @mr @e
    specAtomicBitsMutRef @mr @e

specMutRef
  :: forall mr e
   . (Eq e, Show e, Arbitrary e, Elt mr e, MutRef mr)
  => Spec
specMutRef =
  describe "MutRef" $ do
    prop "newRead" $ prop_newReadMutRef @mr @e
    prop "writeRead" $ prop_writeReadMutRef @mr @e

specAtomicMutRef
  :: forall mr e
   . (Eq e, Show e, Arbitrary e, Elt mr e, AtomicElt mr e, AtomicMutRef mr)
  => Spec
specAtomicMutRef =
  describe "AtomicMutRef" $ do
    prop "CASMutRef" $ prop_CASMutRef @mr @e
    prop "writeReadAtomic" $ prop_writeReadAtomicMutRef @mr @e

specAtomicCountMutRef
  :: forall mr e
   . (Eq e, Show e, Num e, Arbitrary e, Elt mr e, AtomicElt mr e, AtomicCountMutRef mr, AtomicCountElt mr e)
  => Spec
specAtomicCountMutRef =
  describe "AtomicCountMutRef" $ do
    prop "atomicAddFetchOld" $ prop_opFetchOldMutRef @mr @e atomicAddFetchOldMutRef (+)
    prop "atomicAddFetchNew" $ prop_opFetchNewMutRef @mr @e atomicAddFetchNewMutRef (+)
    prop "atomicSubFetchOld" $ prop_opFetchOldMutRef @mr @e atomicSubFetchOldMutRef (-)
    prop "atomicSubFetchNew" $ prop_opFetchNewMutRef @mr @e atomicSubFetchNewMutRef (-)
    describe "Concurrent" $ do
      prop "atomicAddFetchOldMutRef" $ asyncAssociativeProp @mr @e atomicAddFetchOldMutRef (+)
      prop "atomicAddFetchNewMutRef" $ asyncAssociativeProp @mr @e atomicAddFetchNewMutRef (+)
      prop "atomicSubFetchOldMutRef" $ asyncAssociativeProp @mr @e atomicSubFetchOldMutRef subtract
      prop "atomicSubFetchNewMutRef" $ asyncAssociativeProp @mr @e atomicSubFetchNewMutRef subtract

specAtomicBitsMutRef
  :: forall mr e
   . ( Show e
     , Bits e
     , Arbitrary e
     , AtomicBitsMutRef mr
     , AtomicBitsElt mr e
     , AtomicElt mr e
     , Elt mr e
     )
  => Spec
specAtomicBitsMutRef = do
  describe "AtomicBitsMutRef" $ do
    prop "atomicAndFetchOld" $ prop_opFetchOldMutRef @mr @e atomicAndFetchOldMutRef (.&.)
    prop "atomicAndFetchNew" $ prop_opFetchNewMutRef @mr @e atomicAndFetchNewMutRef (.&.)
    prop "atomicNandFetchOld" $
      prop_opFetchOldMutRef @mr @e atomicNandFetchOldMutRef (\x y -> complement (x .&. y))
    prop "atomicNandFetchNew" $
      prop_opFetchNewMutRef @mr @e atomicNandFetchNewMutRef (\x y -> complement (x .&. y))
    prop "atomicOrFetchOld" $ prop_opFetchOldMutRef @mr @e atomicOrFetchOldMutRef (.|.)
    prop "atomicOrFetchNew" $ prop_opFetchNewMutRef @mr @e atomicOrFetchNewMutRef (.|.)
    prop "atomicXorFetchOld" $ prop_opFetchOldMutRef @mr @e atomicXorFetchOldMutRef xor
    prop "atomicXorFetchNew" $ prop_opFetchNewMutRef @mr @e atomicXorFetchNewMutRef xor
    prop "atomicNotFetchOld" $ -- strictness is to simulate property applicable to above ops
      prop_opFetchOldMutRef @mr @e (\ref !_ -> atomicNotFetchOldMutRef ref) (\x !_ -> complement x)
    prop "atomicNotFetchNew" $
      prop_opFetchNewMutRef @mr @e (\ref !_ -> atomicNotFetchNewMutRef ref) (\x !_ -> complement x)
    describe "Concurrent" $ do
      prop "atomicXorFetchOldMutRef" $ asyncAssociativeProp @mr @e atomicXorFetchOldMutRef xor
      prop "atomicXorFetchNewMutRef" $ asyncAssociativeProp @mr @e atomicXorFetchNewMutRef xor

      prop "atomicAndFetchOldMutRef" $
        asyncAssociativeCommutativeOldProp @mr @e atomicAndFetchOldMutRef (.&.)
      -- prop "atomicAndFetchNewMutRef" $
      --   asyncAssociativeCommutativeNewProp @mr @e atomicAndFetchNewMutRef (.&.)
      prop "atomicOrFetchOldMutRef" $
        asyncAssociativeCommutativeOldProp @mr @e atomicOrFetchOldMutRef (.|.)

-- prop "atomicOrFetchNewMutRef" $
--   asyncAssociativeCommutativeNewProp @mr @e atomicOrFetchNewMutRef (.|.)
