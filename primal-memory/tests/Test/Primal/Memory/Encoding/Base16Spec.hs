{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Primal.Memory.Encoding.Base16Spec (spec) where

import Data.ByteString.Base16 as Base16
import Primal.Exception
import Primal.Memory.Encoding.Base16
import Test.Primal.Memory.Common


roundTripBase16 ::
     forall m ma. (Eq m, Show m, Arbitrary m, m ~ Frozen ma, MemAlloc ma)
  => m
  -> Property
roundTripBase16 mem =
  let hex = encodeBase16 mem
   in conjoin
        [ mem === raiseLeftImprecise (decodeBase16Mem hex)
        , encodeBase16Mem mem === Base16.encode (convertMem mem)
        ]

spec :: Spec
spec = do
  describe "Base16" $ do
    describe "match base16-bytestring" $ do
      prop "Encoding" $ \bs -> Base16.encode bs === encodeBase16Mem bs
      prop "Decoding" $ \bs ->
        let hex = Base16.encode bs
         in either error id (Base16.decode hex) === raiseLeftImprecise (decodeBase16Mem hex)
    describe "roundtrip" $ do
      prop "Bytes 'Pin" $ roundTripBase16 @(Bytes 'Pin)
      prop "Bytes 'Inc" $ roundTripBase16 @(Bytes 'Inc)
      prop "Addr Word8" $ roundTripBase16 @(Addr Word8)
      prop "PArray 'Inc Word8" $ roundTripBase16 @(PArray 'Inc Word8)
      prop "ByteString" $ roundTripBase16 @ByteString
      prop "ShortByteString" $ roundTripBase16 @ShortByteString
