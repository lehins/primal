{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Prim.PtrSpec
  ( spec
  , primPtrSpec
  ) where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Prim
import Data.Prim.Ptr
import Data.Typeable
import Foreign.Ptr
import Test.Prim.BytesSpec hiding (spec)
import Test.Prim.Common

primPtrSpec ::
     forall a. (NFData a, Eq a, Show a, Prim a, Arbitrary a, Typeable a)
  => Spec
primPtrSpec = do
  let ptrTypeName = ("Ptr " ++) . showsType (Proxy :: Proxy a) $ ""
  let countIntersect (NEBytes i1 _ b1 :: NEBytes 'Pin a) (NEBytes i2 _ b2 :: NEBytes 'Pin a) =
        min
          (countOfBytes b1 - Count (unOff i1))
          (countOfBytes b2 - Count (unOff i2))
  describe ptrTypeName $ do
    describe "memset" $ do
      prop "empty" $ \(a :: a) -> do
        mb <- allocMBytes (0 :: Count a)
        withPtrMBytes mb $ \ptr -> setOffPtr ptr 0 0 a
        b <- freezeMBytes mb
        toListBytes b `shouldBe` ([] :: [a])
      prop "non-empty" $ \(NEBytes off@(Off o) xs b :: NEBytes 'Pin a) (a :: a) -> do
        mb <- thawBytes b
        Count n :: Count a <- getCountOfMBytes mb
        let c = Count (n - o)
        withPtrMBytes mb $ \ptr -> do
          setOffPtr ptr off c a
          zipWithM_
            (\i x -> readOffPtr ptr i `shouldReturn` x)
            [0 ..]
            (take o xs)
        forM_ [o .. unCount c - 1] $ \i ->
          readMBytes mb (Off i) `shouldReturn` a
    describe "moveMBytesToPtr" $ do
      prop "copyBytesToPtr" $ \n1@(NEBytes i1 _ b1) n2@(NEBytes i2 _ b2) -> do
        let c = countIntersect n1 n2
        mb2x <- thawBytes b2
        withPtrMBytes mb2x $ \xptr -> copyBytesToPtr b1 i1 xptr i2 c
        by <-
          withCloneMBytes_ b2 $ \mb2y -> do
            mb1 <- thawBytes b1
            withPtrMBytes mb2y $ \yptr -> moveMBytesToPtr mb1 i1 yptr i2 c
        bx <- freezeMBytes mb2x
        bx `shouldBe` by
      prop "movePtrToPtr" $ \(NEBytes i xs b :: NEBytes 'Pin a) -> do
        let c = countOfBytes b - Count (unOff i)
        mb <- thawBytes b
        withPtrMBytes mb $ \ptr -> movePtrToPtr ptr i ptr 0 c
        b' <- freezeMBytes mb
        take (unCount c) (toListBytes b') `shouldBe` drop (unOff i) xs
      prop "movePtrToMBytes" $ \(NEBytes i xs b :: NEBytes 'Pin a) -> do
        let c = countOfBytes b - Count (unOff i)
        mb <- thawBytes b
        withPtrMBytes mb $ \ptr -> movePtrToMBytes ptr i mb 0 c
        b' <- freezeMBytes mb
        take (unCount c) (toListBytes b') `shouldBe` drop (unOff i) xs
    describe "copyToPtr" $
      prop "copyMBytesToPtr" $ \n1@(NEBytes i1 xs b1) n2@(NEBytes i2 _ b2) -> do
        let c = countIntersect n1 n2
        mb1 <- thawBytes b1
        mb2x <- thawBytes b2
        withPtrMBytes mb2x $ \xptr -> copyMBytesToPtr mb1 i1 xptr i2 c
        bx <- freezeMBytes mb2x
        take (unCount c) (drop (unOff i2) (toListBytes bx)) `shouldBe`
          take (unCount c) (drop (unOff i1) xs)
    describe "PtrAccess" $ do
      prop "readPtr" $ \ (NEBytes _ xs b :: NEBytes 'Pin a) -> do
        x <- thawBytes b >>= \mb -> withPtrMBytes mb readPtr
        x `shouldBe` indexBytes b 0
        x `shouldBe` head xs
      prop "readOffPtr" $ \ (NEBytes i xs b :: NEBytes 'Pin a) -> do
        x <- thawBytes b >>= \mb -> withPtrMBytes mb (`readOffPtr` i)
        x `shouldBe` indexBytes b i
        x `shouldBe` (xs !! unOff i)
      prop "writePtr" $ \(NEBytes i xs b :: NEBytes 'Pin a) (a :: a) -> do
        mb <- thawBytes b
        withPtrMBytes mb $ \ptr -> do
          readOffPtr ptr i `shouldReturn` (xs !! unOff i)
          writePtr (plusPtrOff ptr i) a
        readMBytes mb i `shouldReturn` a
      prop "writeOffPtr" $ \(NEBytes i xs b :: NEBytes 'Pin a) (a :: a) -> do
        mb <- thawBytes b
        withPtrMBytes mb $ \ptr -> do
          readPtr (plusPtrOff ptr i) `shouldReturn` (xs !! unOff i)
          writeOffPtr ptr i a
        readMBytes mb i `shouldReturn` a

spec :: Spec
spec = do
  primPtrSpec @Word
  primPtrSpec @Word8
  primPtrSpec @Word16
  primPtrSpec @Word32
  primPtrSpec @Word64
  primPtrSpec @Int
  primPtrSpec @Int8
  primPtrSpec @Int16
  primPtrSpec @Int32
  primPtrSpec @Int64
  primPtrSpec @Char
  primPtrSpec @Bool
  primPtrSpec @IntPtr
  primPtrSpec @WordPtr
  primPtrSpec @(Ptr ())
  primPtrSpec @(FunPtr ())
  -- primPtrSpec @CBool
  -- primPtrSpec @CChar
  -- primPtrSpec @CSChar
  -- primPtrSpec @CUChar
  -- primPtrSpec @CShort
  -- primPtrSpec @CUShort
  -- primPtrSpec @CInt
  -- primPtrSpec @CUInt
  -- primPtrSpec @CLong
  -- primPtrSpec @CULong
  -- primPtrSpec @CPtrdiff
  -- primPtrSpec @CSize
  -- primPtrSpec @CWchar
  -- primPtrSpec @CSigAtomic
  -- primPtrSpec @CLLong
  -- primPtrSpec @CULLong
  -- primPtrSpec @CIntPtr
  -- primPtrSpec @CUIntPtr
  -- primPtrSpec @CIntMax
  -- primPtrSpec @CUIntMax
