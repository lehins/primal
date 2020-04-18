{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Prim.BytesExtraSpec (spec) where

import Test.Hspec
import Test.Prim.BytesSpec (primTypeSpec)
import Foreign.C.Types

spec :: Spec
spec = do
  primTypeSpec @CBool
  primTypeSpec @CChar
  primTypeSpec @CSChar
  primTypeSpec @CUChar
  primTypeSpec @CShort
  primTypeSpec @CUShort
  primTypeSpec @CInt
  primTypeSpec @CUInt
  primTypeSpec @CLong
  primTypeSpec @CULong
  primTypeSpec @CPtrdiff
  primTypeSpec @CSize
  primTypeSpec @CWchar
  primTypeSpec @CSigAtomic
  primTypeSpec @CLLong
  primTypeSpec @CULLong
  primTypeSpec @CIntPtr
  primTypeSpec @CUIntPtr
  primTypeSpec @CIntMax
  primTypeSpec @CUIntMax
