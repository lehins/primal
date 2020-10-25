{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Prim.MArraySpec
  ( spec
  ) where

import Data.Prim.MArray.Boxed
import Data.Prim.MArray.Boxed.Small
import Data.Prim.MArray.Unboxed
import Data.Prim.Memory.Addr
import Data.Prim.Memory.Bytes
import Data.Prim.Memory.PArray
import Data.Prim.Ref
import Test.Prim
import Test.Prim.MArray
import qualified Test.Prim.MRef as MRef


primMRefSpec ::
     forall e.
     (Show e, Arbitrary e, Typeable e, Num e, AtomicCount e, AtomicBits e)
  => Spec
primMRefSpec = do
  MRef.spec @(MAddr e)
  MRef.spec @(PMArray 'Inc e)
  MRef.spec @(PMArray 'Pin e)
  MRef.spec @(UMArray e)
  MRef.spec @(NEMArrayIx (MAddr e))
  MRef.spec @(NEMArrayIx (PMArray 'Inc e))
  MRef.spec @(NEMArrayIx (PMArray 'Pin e))
  MRef.spec @(NEMArrayIx (UMArray e))


spec :: Spec
spec = do
  primMRefSpec @Int
  primMRefSpec @Int8
  primMRefSpec @Int16
  primMRefSpec @Int32
  primMRefSpec @Int64
  primMRefSpec @Word
  primMRefSpec @Word8
  primMRefSpec @Word16
  primMRefSpec @Word32
  primMRefSpec @Word64
  MRef.spec @(SBMArray Int)
  MRef.spec @(BMArray Int)
  specMArray @(UMArray Int)
  -- specMArray @(BMArray Int)
  -- specMArray @(BMArray Integer)
  MRef.spec @(Ref Int)
  MRef.spec @(Ref Integer)
  MRef.spec @(MBytes 'Pin)
