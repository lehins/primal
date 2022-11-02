{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Primal.Memory.EndiannessSpec (spec) where

import Primal.Memory.Endianness
import Primal.Ref.Unboxed
import Test.Primal.Memory.Common



roundTripEndianness ::
     forall e. (Unbox (LE e), Unbox (BE e), Typeable e, Arbitrary e, Eq e, Show e)
  => Spec
roundTripEndianness =
  prop (showsType (Proxy :: Proxy e) "") $ \(e :: e) ->
  monadicIO $
  run $ do
    refBE@(URef mba) <- newURef (BE e)
    readURef refBE `shouldReturn` BE e
    let refLE = URef mba
    writeURef refLE (LE e)
    readURef refLE `shouldReturn` LE e

-- Sanity check. TODO more tests in primal-tests
spec :: Spec
spec = do
  describe "Endianness" $ do
    describe "roundtrip" $ do
      roundTripEndianness @Word
      roundTripEndianness @Word8
      roundTripEndianness @Word16
      roundTripEndianness @Word32
      roundTripEndianness @Word64
      roundTripEndianness @Int
      roundTripEndianness @Int8
      roundTripEndianness @Int16
      roundTripEndianness @Int32
      roundTripEndianness @Int64
      roundTripEndianness @Bool
      roundTripEndianness @Char
      roundTripEndianness @Float
      roundTripEndianness @Double
