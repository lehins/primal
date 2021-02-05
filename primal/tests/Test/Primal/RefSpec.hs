{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Primal.RefSpec (spec) where

import qualified Control.Concurrent as Base
import Primal.Concurrent
import Primal.Ref
import Primal.Exception
import Data.Maybe
import Primal.Prim
import Primal.Mem.Weak
import Test.Hspec
import System.Mem (performGC)

instance Typeable a => Show (BRef a RW) where
  show _ = "BRef " ++ showsType (Proxy :: Proxy a) " RW"

data BRefException =
  BRefException
  deriving (Show, Eq)
instance Exception BRefException


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
      newBRef (impureThrow BRefException) `shouldThrow` (== BRefException)
      n :: BRef (Maybe Integer) RW <- newBRef (Just (impureThrow BRefException))
      mRes <- readBRef n
      mRes `shouldSatisfy` isJust
      deepeval mRes `shouldThrow` (== BRefException)
    it "newLazyBRef" $ do
      ref <- newLazyBRef 'h'
      readBRef ref `shouldReturn` 'h'
      n <- newLazyBRef (impureThrow BRefException)
      evalM (readBRef n) `shouldThrow` (== BRefException)
    it "newDeepBRef" $ do
      ref <- newDeepBRef 'h'
      readBRef ref `shouldReturn` 'h'
      newDeepBRef (impureThrow BRefException :: Int) `shouldThrow` (== BRefException)
      newDeepBRef (Just (impureThrow BRefException :: Integer)) `shouldThrow` (== BRefException)
    it "readBRef" $ do
      ref <- newBRef "Hello"
      readBRef ref `shouldReturn` "Hello"
    it "writeBRef" $ do
      ref <- newBRef "Hello"
      readBRef ref `shouldReturn` "Hello"
      writeBRef ref "World"
      readBRef ref `shouldReturn` "World"
    it "swapBRef" $ do
      ref <- newBRef "Hello"
      swapBRef ref "World" `shouldReturn` "Hello"
      swapBRef ref (impureThrow BRefException) `shouldThrow` (== BRefException)
      readBRef ref `shouldReturn` "World"
    it "swapLazyBRef" $ do
      ref <- newBRef "Hello"
      swapLazyBRef ref "World" `shouldReturn` "Hello"
      readBRef ref `shouldReturn` "World"
      swapLazyBRef ref (impureThrow BRefException) `shouldReturn` "World"
      res <- readBRef ref
      eval res `shouldThrow` (== BRefException)
    it "swapDeepBRef" $ do
      ref <- newBRef "Hello"
      swapDeepBRef ref "World" `shouldReturn` "Hello"
      swapDeepBRef ref ("Booyah" ++ impureThrow BRefException) `shouldThrow` (== BRefException)
      readBRef ref `shouldReturn` "World"
    it "modifyBRef" $ do
      ref <- newBRef "Hello"
      modifyBRef ref (\x -> (x ++ " World", length x)) `shouldReturn` 5
      flip shouldThrow (== BRefException) $ modifyBRef ref $ \x -> (impureThrow BRefException, x)
      readBRef ref `shouldReturn` "Hello World"
      _ <- modifyBRef ref $ \x -> (x ++ "!!!", impureThrow BRefException)
      readBRef ref `shouldReturn` "Hello World!!!"
    -- it "modifyFetchOldBRef" $ do
    --   ref <- newBRef "Hello"
    --   modifyBRef ref (++ " World") `shouldReturn` "Hello"
    --   flip shouldThrow (== BRefException) $ modifyBRef ref $ \_ -> impureThrow BRefException
    --   readBRef ref `shouldReturn` "Hello World"
    it "modifyBRefM_" $ do
      ref <- newBRef "Hello"
      modifyBRefM_ ref $ \x -> do
        x `shouldBe` "Hello"
        pure $ x ++ " World"
      flip shouldThrow (== BRefException) $ modifyBRefM_ ref $ \x -> do
        x `shouldBe` "Hello World"
        pure $ impureThrow BRefException
      readBRef ref `shouldReturn` "Hello World"

    --   -- Verify value restoration on WHNF evaluation error
    --   modifyBRef_ ref (\x -> do
    --     isEmptyBRef ref  `shouldReturn` True
    --     x `shouldBe` "Hello World"
    --     pure $ impureThrow BRefException)
    --     `shouldThrow` (==BRefException)
    --   readBRef ref `shouldReturn` "Hello World"

    --   -- check that it is interruptible and that the value is overwritten
    --   timeout 50000 (modifyBRef_ ref (\_ -> putBRef ref "Foo" >> pure "Bar")) `shouldReturn` Nothing
    --   readBRef ref `shouldReturn` "Foo"

    --    -- check that it is interruptible in the exception handler and that the value is
    --    -- overwritten
    --   timeout 50000 (modifyBRef_ ref (\_ -> do
    --                                 putBRef ref "Goodbye"
    --                                 "World" <$ throw BRefException
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
    --     pure $ impureThrow BRefException)
    --     `shouldThrow` (==BRefException)
    --   readBRef ref `shouldReturn` "Hello World"

    --   -- check that it is interruptible and that the value is overwritten
    --   timeout 50000 (modifyBRefMasked_ ref (\_ -> putBRef ref "Foo" >> pure "Bar"))
    --     `shouldReturn` Nothing
    --   readBRef ref `shouldReturn` "Foo"

    --    -- check that it is interruptible in the exception handler and that the value is
    --    -- overwritten
    --   timeout 50000 (modifyBRefMasked_ ref (\_ -> do
    --                                 putBRef ref "Goodbye"
    --                                 "World" <$ throw BRefException
    --                             )) `shouldReturn` Nothing
    --   takeBRef ref `shouldReturn` "Goodbye"

    --   -- check that it is interruptible on empty
    --   timeout 50000 (modifyBRefMasked_ ref pure) `shouldReturn` Nothing
    -- it "modifyFetchOldBRef" $ do
    --   ref <- newBRef "Hello"
    --   modifyFetchOldBRef ref (pure . (++ " World")) `shouldReturn` "Hello"
    --   readBRef ref `shouldReturn` "Hello World"
    --   modifyFetchOldBRef ref (\ _ -> pure $ impureThrow BRefException)
    --     `shouldThrow` (==BRefException)
    --   takeBRef ref `shouldReturn` "Hello World"
    -- it "modifyFetchOldBRefMasked" $ do
    --   ref <- newBRef "Hello"
    --   modifyFetchOldBRefMasked ref (pure . (++ " World")) `shouldReturn` "Hello"
    --   readBRef ref `shouldReturn` "Hello World"
    --   modifyFetchOldBRefMasked ref (\ _ -> pure $ impureThrow BRefException)
    --     `shouldThrow` (==BRefException)
    --   takeBRef ref `shouldReturn` "Hello World"
    -- it "modifyFetchNewBRef" $ do
    --   ref <- newBRef "Hello"
    --   modifyFetchNewBRef ref (pure . (++ " World")) `shouldReturn` "Hello World"
    --   readBRef ref `shouldReturn` "Hello World"
    --   modifyFetchNewBRef ref (\ _ -> pure $ impureThrow BRefException)
    --     `shouldThrow` (==BRefException)
    --   takeBRef ref `shouldReturn` "Hello World"
    -- it "modifyFetchNewBRefMasked" $ do
    --   ref <- newBRef "Hello"
    --   modifyFetchNewBRefMasked ref (pure . (++ " World")) `shouldReturn` "Hello World"
    --   readBRef ref `shouldReturn` "Hello World"
    --   modifyFetchNewBRefMasked ref (\ _ -> pure $ impureThrow BRefException)
    --     `shouldThrow` (==BRefException)
    --   takeBRef ref `shouldReturn` "Hello World"
    -- -- xit "modifyBRef" (pure () :: IO ())
    -- -- xit "modifyBRefMasked" (pure () :: IO ())
    -- it "toBaseBRef" $ do
    --   ref <- newBRef ()
    --   Base.takeBRef (toBaseBRef ref) `shouldReturn` ()
    --   isEmptyBRef ref `shouldReturn` True
    -- it "fromBaseBRef" $ do
    --   ref <- Base.newBRef ()
    --   takeBRef (fromBaseBRef ref) `shouldReturn` ()
    --   Base.isEmptyBRef ref `shouldReturn` True
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

