{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Primal.MutArraySpec
  ( spec
  ) where

import Primal.Array.Boxed
import Primal.Array.Boxed.Small
import Primal.Array.Unboxed
import Primal.Memory.Addr
import Primal.Memory.FAddr
import Primal.Memory.Bytes
import Primal.Memory.PUArray
import Primal.Ref
import Test.Primal
import Test.Primal.MutArray
import qualified Test.Primal.MutRef as MutRef


primMutRefSpec ::
     forall e.
     (Show e, Arbitrary e, Typeable e, Num e, AtomicCount e, AtomicBits e)
  => Spec
primMutRefSpec = do
  MutRef.spec @MAddr @e
  MutRef.spec @FMAddr @e
  MutRef.spec @(PUMArray 'Inc) @e
  MutRef.spec @(PUMArray 'Pin) @e
  MutRef.spec @UMArray @e
  MutRef.spec @(NEMutArrayIx MAddr) @e
  MutRef.spec @(NEMutArrayIx (PUMArray 'Inc)) @e
  MutRef.spec @(NEMutArrayIx (PUMArray 'Pin)) @e
  MutRef.spec @(NEMutArrayIx UMArray) @e


spec :: Spec
spec = do
  primMutRefSpec @Int
  primMutRefSpec @Int8
  primMutRefSpec @Int16
  primMutRefSpec @Int32
  primMutRefSpec @Int64
  primMutRefSpec @Word
  primMutRefSpec @Word8
  primMutRefSpec @Word16
  primMutRefSpec @Word32
  primMutRefSpec @Word64
  MutRef.spec @SBMArray @Int
  MutRef.spec @BMArray @Int
  specMutArray @UMArray @Int
  -- specMArray @(BMArray Int)
  -- specMArray @(BMArray Integer)
  MutRef.spec @BRef @Int
  MutRef.spec @BRef @Integer
