{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Prim.MArraySpec
  ( spec
  ) where

import Data.Prim.MArray.Boxed
import Data.Prim.MArray.Unboxed
import Data.Prim.MRef.Ref
import Data.Prim.Memory.Addr
import Data.Prim.Memory.Bytes
import Data.Prim.Memory.ByteArray
import Test.Prim
import Test.Prim.MArray
import Test.Prim.MRef


primMRefSpec ::
     forall e.
     (Show e, Arbitrary e, Typeable e, Num e, AtomicCount e, AtomicBits e)
  => Spec
primMRefSpec = do
  specMRef @(MAddr e)
  specMRef @(MByteArray 'Inc e)
  specMRef @(MByteArray 'Pin e)
  specMRef @(MUArray e)
  specMRef @(NEMArrayIx (MAddr e))
  specMRef @(NEMArrayIx (MByteArray 'Inc e))
  specMRef @(NEMArrayIx (MByteArray 'Pin e))
  specMRef @(NEMArrayIx (MUArray e))
  specAtomicMRef @(MAddr e)
  specAtomicMRef @(MByteArray 'Inc e)
  specAtomicMRef @(MByteArray 'Pin e)
  specAtomicMRef @(MUArray e)
  specAtomicMRef @(NEMArrayIx (MAddr e))
  specAtomicMRef @(NEMArrayIx (MByteArray 'Inc e))
  specAtomicMRef @(NEMArrayIx (MByteArray 'Pin e))
  specAtomicMRef @(NEMArrayIx (MUArray e))


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
  specMRef @(MBArray Int)
  specMArray @(MUArray Int)
  -- specMArray @(MBArray Int)
  -- specMArray @(MBArray Integer)
  specMRef @(Ref Int)
  specMRef @(Ref Integer)
  specMRef @(MBytes 'Pin)
