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
module Test.Prim.MRef where

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
  -> IO ()
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
  -> IO ()
expectWriteReadAtomicMRef mut e' = do
  atomicWriteMRef mut e'
  -- ensure roundtrip
  atomicReadMRef mut `shouldReturn` e'
  -- ensure argument is strict
  shouldThrow (atomicWriteMRef mut (throw ExpectedException)) (ExpectedException ==)
  -- ensure survival of previous value
  readMRef mut `shouldReturn` e'


specMRef ::
     forall mut.
     ( Eq (Elt mut)
     , Show (Elt mut)
     , Arbitrary (Elt mut)
     , AtomicMRef mut
     , Typeable mut
     )
  => Spec
specMRef = do
  let mutTypeName = showsType (Proxy :: Proxy (mut RW)) ""
  describe mutTypeName $ do
    prop "writeRead" $ prop_writeReadMRef @mut
    prop "writeReadAtomic" $ prop_writeReadAtomicMRef @mut


-- -- forAllIO :: (Show p, Testable t) => Gen p -> (p -> IO t) -> Property
-- -- forAllIO g propM = forAll g $ \v -> monadicIO $ run $ propM v

-- -- forAllMutIO ::
-- --      (Show p, Atomic p, Testable t)
-- --   => Gen p
-- --   -> (p -> r p IO -> IO t)
-- --   -> Property
-- -- forAllMutIO g propM = forAllIO g $ \v -> newMut v >>= propM v


-- -- propMutSpecIO ::
-- --      (Show p, Atomic p, Testable t)
-- --   => String
-- --   -> Gen p
-- --   -> (p -> r IO p -> IO t)
-- --   -> Spec
-- -- propMutSpecIO name gen action = prop name $ forAllAVarIO gen action

-- --atomicAddFetchOldProp :: (a -> f a s -> m b) ->
