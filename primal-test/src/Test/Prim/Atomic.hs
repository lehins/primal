-- |
-- Module      : Test.Prim.Atomic
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Test.Prim.Atomic where

import Control.Prim.Monad
import Data.Prim.Atomic
import Test.Hspec
import Test.QuickCheck

-- data MutableIx mut s = MutableIx ![Elt mut] !(mut s)

-- data NEMutableIx mut s = NEMutableIx !Int ![Elt mut] !(mut s)


-- instance (a ~ Elt (ma a), MArray (ma a)) => MRef (NEMArrayIx ma) a where
--   newMRef a = MArrayIx 0 <$> newMArray 1 a
--   newRawMRef = MArrayIx 0 <$> newRawMArray 1
--   readMRef (MArrayIx i ma) = readMArray ma i
--   writeMRef (MArrayIx i ma) = writeMArray ma i





-- forAllIO :: (Show p, Testable t) => Gen p -> (p -> IO t) -> Property
-- forAllIO g propM = forAll g $ \v -> monadicIO $ run $ propM v

-- forAllMutIO ::
--      (Show p, Atomic p, Testable t)
--   => Gen p
--   -> (p -> r p IO -> IO t)
--   -> Property
-- forAllMutIO g propM = forAllIO g $ \v -> newMut v >>= propM v


-- propMutSpecIO ::
--      (Show p, Atomic p, Testable t)
--   => String
--   -> Gen p
--   -> (p -> r IO p -> IO t)
--   -> Spec
-- propMutSpecIO name gen action = prop name $ forAllAVarIO gen action

--atomicAddFetchOldProp :: (a -> f a s -> m b) ->
