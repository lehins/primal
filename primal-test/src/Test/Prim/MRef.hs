{-# LANGUAGE AllowAmbiguousTypes #-}
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

import Data.Bits
import Control.Exception (throw)
import Control.Prim.Monad
import Data.Prim
import Data.Prim.MRef
import Test.Prim.Common

forAllIO :: (Show p, Testable t) => Gen p -> (p -> IO t) -> Property
forAllIO g propM = forAll g $ \v -> monadicIO $ run $ propM v


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
    x' <- opMRef ref x
    x' `shouldBe` e
    readMRef ref `shouldReturn` (x `op` e)

prop_opFetchNewMRef ::
     forall mut. (Eq (Elt mut), Show (Elt mut), Arbitrary (Elt mut), MRef mut)
  => (forall m s . MonadPrim s m => mut s -> Elt mut -> m (Elt mut)) -- ^ Monadic version
  -> (Elt mut -> Elt mut -> Elt mut) -- ^ Pure version
  -> Elt mut
  -> Property
prop_opFetchNewMRef opMRef op x =
  forAllMRef @mut arbitrary $ \ e ref -> do
    x' <- opMRef ref x
    x' `shouldBe` (x `op` e)
    readMRef ref `shouldReturn` (x `op` e)



specMRef ::
     forall mut.
     ( Eq (Elt mut)
     , Show (Elt mut)
     , Arbitrary (Elt mut)
     , MRef mut
     , Typeable mut
     )
  => Spec
specMRef = do
  let mutTypeName = showsType (Proxy :: Proxy (mut RW)) ""
  describe mutTypeName $ do
    describe "MRef" $ do
      prop "newRead" $ prop_newReadMRef @mut
      prop "writeRead" $ prop_writeReadMRef @mut


specAtomicMRef ::
     forall mut.
     ( Show (Elt mut)
     , Arbitrary (Elt mut)
     , AtomicCountMRef mut
     , AtomicBitsMRef mut
     , Typeable mut
     )
  => Spec
specAtomicMRef = do
  let mutTypeName = showsType (Proxy :: Proxy (mut RW)) ""
  describe mutTypeName $ do
    describe "AtomicMRef" $ do
      prop "writeReadAtomic" $ prop_writeReadAtomicMRef @mut
    describe "AtomicCountMRef" $ do
      prop "atomicAddFetchOld" $ prop_opFetchNewMRef @mut atomicAddFetchOldMRef (+)
      prop "atomicAddFetchNew" $ prop_opFetchNewMRef @mut atomicAddFetchNewMRef (+)
      prop "atomicSubFetchOld" $ prop_opFetchNewMRef @mut atomicSubFetchOldMRef (+)
      prop "atomicSubFetchNew" $ prop_opFetchNewMRef @mut atomicSubFetchNewMRef (+)
    describe "AtomicBitsMRef" $ do
      prop "atomicAndFetchOld" $ prop_opFetchNewMRef @mut atomicAndFetchOldMRef (.&.)
      prop "atomicAndFetchNew" $ prop_opFetchNewMRef @mut atomicAndFetchNewMRef (.&.)
      prop "atomicNandFetchOld" $
        prop_opFetchNewMRef @mut atomicNandFetchOldMRef (\x y -> complement (x .&. y))
      prop "atomicNandFetchNew" $
        prop_opFetchNewMRef @mut atomicNandFetchNewMRef (\x y -> complement (x .&. y))
      prop "atomicOrFetchOld" $ prop_opFetchNewMRef @mut atomicOrFetchOldMRef (.|.)
      prop "atomicOrFetchNew" $ prop_opFetchNewMRef @mut atomicOrFetchNewMRef (.|.)
      prop "atomicXorFetchOld" $ prop_opFetchNewMRef @mut atomicXorFetchOldMRef xor
      prop "atomicXorFetchNew" $ prop_opFetchNewMRef @mut atomicXorFetchNewMRef xor
      prop "atomicNotFetchOld" $
        prop_opFetchNewMRef @mut (\ref _ -> atomicNotFetchOldMRef ref) (const id)
      prop "atomicNotFetchNew" $
        prop_opFetchNewMRef @mut (\ref _ -> atomicNotFetchNewMRef ref) (const complement)

