{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Primal.BRefSpec (spec) where

import Data.Maybe
import Primal.Concurrent
import Primal.Exception
import Primal.Ref
import Primal.Element.Unbox
import Test.Hspec
import Test.Primal.ArraySpec (ExpectedException(..), impreciseExpectedException)

instance Typeable a => Show (BRef a RW) where
  show _ = "BRef " ++ showsType (Proxy :: Proxy a) " RW"


spec :: Spec
spec = do
  describe "BRef" $ do
    it "isSameBRef" $ do
      ref1 <- newBRef ()
      isSameBRef ref1 ref1 `shouldBe` True
      ref1 `shouldBe` ref1
      ref2 <- newBRef ()
      isSameBRef ref1 ref2 `shouldBe` False
      ref1 `shouldSatisfy` (/= ref2)
    it "newBRef" $ do
      ref <- newBRef 'h'
      readBRef ref `shouldReturn` 'h'
      newBRef (raiseImprecise ExpectedException) `shouldThrow` impreciseExpectedException
      n :: BRef (Maybe Integer) RW <- newBRef (Just (raiseImprecise ExpectedException))
      mRes <- readBRef n
      mRes `shouldSatisfy` isJust
      deepeval mRes `shouldThrow` impreciseExpectedException
    it "newLazyBRef" $ do
      ref <- newLazyBRef 'h'
      readBRef ref `shouldReturn` 'h'
      n <- newLazyBRef (raiseImprecise ExpectedException)
      evalM (readBRef n) `shouldThrow` impreciseExpectedException
    it "newDeepBRef" $ do
      ref <- newDeepBRef 'h'
      readBRef ref `shouldReturn` 'h'
      newDeepBRef (raiseImprecise ExpectedException :: Int) `shouldThrow` impreciseExpectedException
      newDeepBRef (Just (raiseImprecise ExpectedException :: Integer)) `shouldThrow` impreciseExpectedException
    it "readBRef" $ do
      ref <- newBRef "Hello"
      readBRef ref `shouldReturn` "Hello"
    it "writeBRef" $ do
      ref <- newBRef "Hello"
      readBRef ref `shouldReturn` "Hello"
      writeBRef ref "World"
      readBRef ref `shouldReturn` "World"
    it "writeFetchOldBRef" $ do
      ref <- newBRef "Hello"
      writeFetchOldBRef ref "World" `shouldReturn` "Hello"
      writeFetchOldBRef ref (raiseImprecise ExpectedException) `shouldThrow` impreciseExpectedException
      readBRef ref `shouldReturn` "World"
    it "writeFetchOldLazyBRef" $ do
      ref <- newBRef "Hello"
      writeFetchOldLazyBRef ref "World" `shouldReturn` "Hello"
      readBRef ref `shouldReturn` "World"
      writeFetchOldLazyBRef ref (raiseImprecise ExpectedException) `shouldReturn` "World"
      res <- readBRef ref
      eval res `shouldThrow` impreciseExpectedException
    it "writeFetchOldDeepBRef" $ do
      ref <- newBRef "Hello"
      writeFetchOldDeepBRef ref "World" `shouldReturn` "Hello"
      writeFetchOldDeepBRef ref
        ("Booyah" ++ raiseImprecise ExpectedException) `shouldThrow` impreciseExpectedException
      readBRef ref `shouldReturn` "World"
    it "modifyBRef" $ do
      ref <- newBRef "Hello"
      modifyBRef ref (\x -> (x ++ " World", length x)) `shouldReturn` 5
      flip shouldThrow impreciseExpectedException $ modifyBRef ref $ \x -> (raiseImprecise ExpectedException, x)
      readBRef ref `shouldReturn` "Hello World"
      _ <- modifyBRef ref $ \x -> (x ++ "!!!", raiseImprecise ExpectedException)
      readBRef ref `shouldReturn` "Hello World!!!"
    -- it "modifyFetchOldBRef" $ do
    --   ref <- newBRef "Hello"
    --   modifyBRef ref (++ " World") `shouldReturn` "Hello"
    --   flip shouldThrow impreciseExpectedException $ modifyBRef ref $ \_ -> raiseImprecise ExpectedException
    --   readBRef ref `shouldReturn` "Hello World"
    it "modifyBRefM_" $ do
      ref <- newBRef "Hello"
      modifyBRefM_ ref $ \x -> do
        x `shouldBe` "Hello"
        pure $ x ++ " World"
      flip shouldThrow impreciseExpectedException $ modifyBRefM_ ref $ \x -> do
        x `shouldBe` "Hello World"
        pure $ raiseImprecise ExpectedException
      readBRef ref `shouldReturn` "Hello World"

    --   -- Verify value restoration on WHNF evaluation error
    --   modifyBRef_ ref (\x -> do
    --     isEmptyBRef ref  `shouldReturn` True
    --     x `shouldBe` "Hello World"
    --     pure $ raiseImprecise ExpectedException)
    --     `shouldThrow` impreciseExpectedException
    --   readBRef ref `shouldReturn` "Hello World"

    --   -- check that it is interruptible and that the value is overwritten
    --   timeout 50000 (modifyBRef_ ref (\_ -> putBRef ref "Foo" >> pure "Bar")) `shouldReturn` Nothing
    --   readBRef ref `shouldReturn` "Foo"

    --    -- check that it is interruptible in the exception handler and that the value is
    --    -- overwritten
    --   timeout 50000 (modifyBRef_ ref (\_ -> do
    --                                 putBRef ref "Goodbye"
    --                                 "World" <$ throw ExpectedException
    --                             )) `shouldReturn` Nothing
    --   takeBRef ref `shouldReturn` "Goodbye"

    --   -- check that it is interruptible on empty
    --   timeout 50000 (modifyBRef_ ref pure) `shouldReturn` Nothing
    -- it "modifyBRefMasked_" $ do
    --   ref <- newBRef "Hello"

    --   -- check masking state and actual modification
    --   modifyBRefMasked_ ref $ \x -> do
    --     x `shouldBe` "Hello"
    --     getMaskingState `shouldReturn` MaskedInterruptible
    --     pure $ x ++ " World"

    --   -- Verify value restoration on WHNF evaluation error
    --   modifyBRefMasked_ ref (\x -> do
    --     isEmptyBRef ref  `shouldReturn` True
    --     x `shouldBe` "Hello World"
    --     pure $ raiseImprecise ExpectedException)
    --     `shouldThrow` impreciseExpectedException
    --   readBRef ref `shouldReturn` "Hello World"

    --   -- check that it is interruptible and that the value is overwritten
    --   timeout 50000 (modifyBRefMasked_ ref (\_ -> putBRef ref "Foo" >> pure "Bar"))
    --     `shouldReturn` Nothing
    --   readBRef ref `shouldReturn` "Foo"

    --    -- check that it is interruptible in the exception handler and that the value is
    --    -- overwritten
    --   timeout 50000 (modifyBRefMasked_ ref (\_ -> do
    --                                 putBRef ref "Goodbye"
    --                                 "World" <$ throw ExpectedException
    --                             )) `shouldReturn` Nothing
    --   takeBRef ref `shouldReturn` "Goodbye"

    --   -- check that it is interruptible on empty
    --   timeout 50000 (modifyBRefMasked_ ref pure) `shouldReturn` Nothing
    -- it "modifyFetchOldBRef" $ do
    --   ref <- newBRef "Hello"
    --   modifyFetchOldBRef ref (pure . (++ " World")) `shouldReturn` "Hello"
    --   readBRef ref `shouldReturn` "Hello World"
    --   modifyFetchOldBRef ref (\ _ -> pure $ raiseImprecise ExpectedException)
    --     `shouldThrow` impreciseExpectedException
    --   takeBRef ref `shouldReturn` "Hello World"
    -- it "modifyFetchOldBRefMasked" $ do
    --   ref <- newBRef "Hello"
    --   modifyFetchOldBRefMasked ref (pure . (++ " World")) `shouldReturn` "Hello"
    --   readBRef ref `shouldReturn` "Hello World"
    --   modifyFetchOldBRefMasked ref (\ _ -> pure $ raiseImprecise ExpectedException)
    --     `shouldThrow` impreciseExpectedException
    --   takeBRef ref `shouldReturn` "Hello World"
    -- it "modifyFetchNewBRef" $ do
    --   ref <- newBRef "Hello"
    --   modifyFetchNewBRef ref (pure . (++ " World")) `shouldReturn` "Hello World"
    --   readBRef ref `shouldReturn` "Hello World"
    --   modifyFetchNewBRef ref (\ _ -> pure $ raiseImprecise ExpectedException)
    --     `shouldThrow` impreciseExpectedException
    --   takeBRef ref `shouldReturn` "Hello World"
    -- it "modifyFetchNewBRefMasked" $ do
    --   ref <- newBRef "Hello"
    --   modifyFetchNewBRefMasked ref (pure . (++ " World")) `shouldReturn` "Hello World"
    --   readBRef ref `shouldReturn` "Hello World"
    --   modifyFetchNewBRefMasked ref (\ _ -> pure $ raiseImprecise ExpectedException)
    --     `shouldThrow` impreciseExpectedException
    --   takeBRef ref `shouldReturn` "Hello World"
    -- -- xit "modifyBRef" (pure () :: IO ())
    -- -- xit "modifyBRefMasked" (pure () :: IO ())
    -- describe "mkWeakBRef" $ do
    --   it "performGC" $ do
    --     seref <- newEmptyBRef
    --     void $ fork $ do
    --       ref <- newEmptyBRef
    --       _weak <- mkWeakBRef ref $ putBRef seref ()
    --       performGC
    --     takeBRef seref `shouldReturn` ()
    --   it "finalizeWeak" $ do
    --     seref <- newEmptyBRef
    --     ref <- newBRef "Hello"
    --     weak <- mkWeakBRef ref $ putBRef seref ()
    --     deBRefWeak weak >>= \case
    --       Nothing -> expectationFailure "Empty weak ref"
    --       Just ref' -> do
    --         ref' `shouldBe` ref
    --         readBRef ref' `shouldReturn` "Hello"
    --     finalizeWeak weak
    --     takeBRef sem `shouldReturn` ()

