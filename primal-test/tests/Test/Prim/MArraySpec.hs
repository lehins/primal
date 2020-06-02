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
import Data.Prim.MRef.Ref
import Data.Prim.Memory.Addr
import Data.Prim.Memory.Bytes
import Data.Prim.Memory.ByteArray
import Test.Prim
import Test.Prim.MArray
import qualified Test.Prim.MRef as MRef


primMRefSpec ::
     forall e.
     (Show e, Arbitrary e, Typeable e, Num e, AtomicCount e, AtomicBits e)
  => Spec
primMRefSpec = do
  MRef.spec @(MAddr e)
  MRef.spec @(MByteArray 'Inc e)
  MRef.spec @(MByteArray 'Pin e)
  MRef.spec @(MUArray e)
  MRef.spec @(NEMArrayIx (MAddr e))
  MRef.spec @(NEMArrayIx (MByteArray 'Inc e))
  MRef.spec @(NEMArrayIx (MByteArray 'Pin e))
  MRef.spec @(NEMArrayIx (MUArray e))


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
  MRef.spec @(MSBArray Int)
  MRef.spec @(MBArray Int)
  specMArray @(MUArray Int)
  -- specMArray @(MBArray Int)
  -- specMArray @(MBArray Integer)
  MRef.spec @(Ref Int)
  MRef.spec @(Ref Integer)
  MRef.spec @(MBytes 'Pin)
