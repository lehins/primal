{-# LANGUAGE TypeApplications #-}
module Test.Prim.MArraySpec
  ( spec
  ) where

import Data.Prim.MArray.Boxed as B
import Data.Prim.MRef.Ref
import Test.Prim
import Test.Prim.MArray
import Test.Prim.MRef

spec :: Spec
spec = do
  specMArray @(B.MBArray Int)
  specMArray @(B.MBArray Integer)
  specMRef @(Ref Int)
  specMRef @(Ref Integer)
