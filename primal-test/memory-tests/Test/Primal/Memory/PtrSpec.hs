{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Prim.Memory.PtrSpec
  ( spec
  , primPtrSpec
  ) where

import Control.Monad
import Data.Prim.Memory.Ptr
import Test.Prim
import Test.Prim.Memory
import Test.Prim.Memory.BytesSpec hiding (spec)


primPtrSpec ::
     forall a. (Eq a, Show a, Prim a, Arbitrary a, Typeable a)
  => Spec
primPtrSpec = do
  let ptrTypeName = ("Ptr " ++) . showsType (Proxy :: Proxy a) $ ""
  let countIntersect (NEMem i1 _ b1 :: NEBytes 'Pin a) (NEMem i2 _ b2 :: NEBytes 'Pin a) =
        min
          (countBytes b1 - Count (unOff i1))
          (countBytes b2 - Count (unOff i2))
  describe ptrTypeName $ do
    describe "memset" $ do
      prop "empty" $ \(a :: a) -> do
        mb <- allocMBytes (0 :: Count a)
        withPtrMBytes mb $ \ptr -> setOffPtr ptr 0 0 a
        b <- freezeMBytes mb
        toListBytes b `shouldBe` ([] :: [a])
      prop "non-empty" $ \(NEMem off@(Off o) xs b :: NEBytes 'Pin a) (a :: a) -> do
        mb <- thawBytes b
        Count n :: Count a <- getCountMBytes mb
        let c = Count (n - o)
        withPtrMBytes mb $ \ptr -> do
          setOffPtr ptr off c a
          zipWithM_
            (\i x -> readOffPtr ptr i `shouldReturn` x)
            [0 ..]
            (take o xs)
        forM_ [o .. unCount c - 1] $ \i ->
          readOffMBytes mb (Off i) `shouldReturn` a
    describe "moveMBytesToPtr" $ do
      prop "copyBytesToPtr" $ \n1@(NEMem i1 _ b1) n2@(NEMem i2 _ b2) -> do
        let c = countIntersect n1 n2
        mb2x <- thawBytes b2
        withPtrMBytes mb2x $ \xptr -> copyBytesToPtr b1 i1 xptr i2 c
        by <-
          withCloneMBytes_ b2 $ \mb2y -> do
            mb1 <- thawBytes b1
            withPtrMBytes mb2y $ \yptr -> moveMBytesToPtr mb1 i1 yptr i2 c
        bx <- freezeMBytes mb2x
        bx `shouldBe` by
      prop "movePtrToPtr" $ \(NEMem i xs b :: NEBytes 'Pin a) -> do
        let c = countBytes b - Count (unOff i)
        mb <- thawBytes b
        withPtrMBytes mb $ \ptr -> movePtrToPtr ptr i ptr 0 c
        b' <- freezeMBytes mb
        take (unCount c) (toListBytes b') `shouldBe` drop (unOff i) xs
      prop "movePtrToMBytes" $ \(NEMem i xs b :: NEBytes 'Pin a) -> do
        let c = countBytes b - Count (unOff i)
        mb <- thawBytes b
        withPtrMBytes mb $ \ptr -> movePtrToMBytes ptr i mb 0 c
        b' <- freezeMBytes mb
        take (unCount c) (toListBytes b') `shouldBe` drop (unOff i) xs
    describe "copyToPtr" $
      prop "copyMBytesToPtr" $ \n1@(NEMem i1 xs b1) n2@(NEMem i2 _ b2) -> do
        let c = countIntersect n1 n2
        mb1 <- thawBytes b1
        mb2x <- thawBytes b2
        withPtrMBytes mb2x $ \xptr -> copyMBytesToPtr mb1 i1 xptr i2 c
        bx <- freezeMBytes mb2x
        take (unCount c) (drop (unOff i2) (toListBytes bx)) `shouldBe`
          take (unCount c) (drop (unOff i1) xs)
    describe "PtrAccess" $ do
      prop "readPtr" $ \ (NEMem _ xs b :: NEBytes 'Pin a) -> do
        x <- thawBytes b >>= \mb -> withPtrMBytes mb readPtr
        x `shouldBe` indexOffBytes b 0
        x `shouldBe` head xs
      prop "readOffPtr" $ \ (NEMem i xs b :: NEBytes 'Pin a) -> do
        x <- thawBytes b >>= \mb -> withPtrMBytes mb (`readOffPtr` i)
        x `shouldBe` indexOffBytes b i
        x `shouldBe` (xs !! unOff i)
      prop "writePtr" $ \(NEMem i xs b :: NEBytes 'Pin a) (a :: a) -> do
        mb <- thawBytes b
        withPtrMBytes mb $ \ptr -> do
          readOffPtr ptr i `shouldReturn` (xs !! unOff i)
          writePtr (plusOffPtr ptr i) a
        readOffMBytes mb i `shouldReturn` a
      prop "writeOffPtr" $ \(NEMem i xs b :: NEBytes 'Pin a) (a :: a) -> do
        mb <- thawBytes b
        withPtrMBytes mb $ \ptr -> do
          readPtr (plusOffPtr ptr i) `shouldReturn` (xs !! unOff i)
          writeOffPtr ptr i a
        readOffMBytes mb i `shouldReturn` a

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
