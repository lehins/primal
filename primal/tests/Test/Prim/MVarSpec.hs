{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Prim.MVarSpec (spec) where

import qualified Control.Concurrent as Base
import Control.DeepSeq
import Control.Prim.Concurrent
import Control.Prim.Concurrent.MVar
import Control.Prim.Exception
import Control.Prim.Monad
import Data.Maybe
import Data.Prim
import Foreign.Prim.WeakPtr
import Test.Hspec
import System.Mem (performGC)

instance Typeable a => Show (MVar a RW) where
  show _ = "MVar " ++ showsType (Proxy :: Proxy a) " RW"

data MVarException =
  MVarException
  deriving (Show, Eq)
instance Exception MVarException

failAfter :: Int -> Expectation -> Expectation
failAfter n test =
  timeout n test >>= \case
    Nothing -> expectationFailure $ "Did not finish within " ++ show (n `div` 1000) ++ " ms. "
    Just () -> pure ()

wit :: String -> Expectation-> Spec
wit n t = it n $ failAfter 1000000 t

spec :: Spec
spec = do
  describe "MVar" $ do
    wit "isEmptyMVar" $ do
      m <- newEmptyMVar @Int
      isEmptyMVar m `shouldReturn` True
      putMVar m 0
      isEmptyMVar m `shouldReturn` False
      (newMVar 'H' >>= isEmptyMVar) `shouldReturn` False
    wit "isSameMVar" $ do
      m1 <- newEmptyMVar @Int
      isSameMVar m1 m1 `shouldBe` True
      m1 `shouldBe` m1
      m2 <- newEmptyMVar @Int
      isSameMVar m1 m2 `shouldBe` False
      m1 `shouldSatisfy` (/= m2)
    wit "newMVar" $ do
      m <- newMVar 'h'
      readMVar m `shouldReturn` 'h'
      newMVar (impureThrow MVarException) `shouldThrow` (== MVarException)
      n <- newMVar @(Maybe Integer) (Just (impureThrow MVarException))
      mRes <- takeMVar n
      mRes `shouldSatisfy` isJust
      evaluate (force mRes) `shouldThrow` (== MVarException)
    wit "newLazyMVar" $ do
      m <- newLazyMVar 'h'
      tryTakeMVar m `shouldReturn` Just 'h'
      n <- newLazyMVar (impureThrow MVarException)
      res <- takeMVar n
      evaluate res `shouldThrow` (== MVarException)
    wit "newDeepMVar" $ do
      m <- newDeepMVar 'h'
      takeMVar m `shouldReturn` 'h'
      newDeepMVar @Int (impureThrow MVarException) `shouldThrow` (== MVarException)
      newDeepMVar @(Maybe Integer) (Just (impureThrow MVarException)) `shouldThrow` (== MVarException)
    wit "putMVar" $ do
      m <- newEmptyMVar
      void $ fork $ putMVar m "Hello"
      takeMVar m `shouldReturn` "Hello"

      n <- newMVar "World"
      timeout 50000 (putMVar n "Already full") `shouldReturn` Nothing
      void $ fork $ putMVar n (impureThrow MVarException)
      putMVar n (impureThrow MVarException) `shouldThrow` (== MVarException)
      takeMVar n `shouldReturn` "World"

      putMVar n ('f':impureThrow MVarException)
      res <- takeMVar n
      head res `shouldBe` 'f'
      evaluate (force res) `shouldThrow` (== MVarException)
    wit "putLazyMVar" $ do
      m <- newEmptyMVar
      void $ fork $ putLazyMVar m "Hello"
      readMVar m `shouldReturn` "Hello"
      timeout 50000 (putLazyMVar m "Already full") `shouldReturn` Nothing

      n <- newEmptyMVar
      void $ fork $ putLazyMVar n (impureThrow MVarException)
      res <- takeMVar n
      evaluate res `shouldThrow` (== MVarException)
    wit "putDeepMVar" $ do
      m <- newEmptyMVar
      void $ fork $ putDeepMVar m "Hello"
      readMVar m `shouldReturn` "Hello"
      timeout 50000 (putDeepMVar m "Already full") `shouldReturn` Nothing

      n <- newMVar "World"
      void $ fork $ putDeepMVar n ("Bar" ++ impureThrow MVarException)
      putDeepMVar n ("Foo" ++ impureThrow MVarException) `shouldThrow` (== MVarException)
      threadDelay 10000
      takeMVar n `shouldReturn` "World"
    wit "tryPutMVar" $ do
      m <- newEmptyMVar
      tryPutMVar m "Hello" `shouldReturn` True
      tryPutMVar m "World" `shouldReturn` False
      tryPutMVar m (impureThrow MVarException) `shouldThrow` (== MVarException)
      takeMVar m `shouldReturn` "Hello"

      n <- newEmptyMVar
      void $ fork $ void $ tryPutMVar n (impureThrow MVarException)
      tryPutMVar n (impureThrow MVarException) `shouldThrow` (== MVarException)
      threadDelay 10000
      isEmptyMVar n `shouldReturn` True
    wit "tryPutLazyMVar" $ do
      m <- newEmptyMVar
      tryPutLazyMVar m "Hello" `shouldReturn` True
      tryPutLazyMVar m "World" `shouldReturn` False
      tryPutLazyMVar m (impureThrow MVarException) `shouldReturn` False
      takeMVar m `shouldReturn` "Hello"

      done <- newEmptyMVar
      n <- newEmptyMVar
      void $ fork $ do
        res <- tryPutLazyMVar n (impureThrow MVarException)
        void $ tryPutLazyMVar done res
      takeMVar done `shouldReturn` True
      isEmptyMVar n `shouldReturn` False
      res <- takeMVar n
      evaluate res `shouldThrow` (== MVarException)
    wit "tryPutDeepMVar" $ do
      m <- newEmptyMVar
      tryPutMVar m "Hello" `shouldReturn` True
      tryPutMVar m "World" `shouldReturn` False
      tryPutDeepMVar m ("Happy" ++ impureThrow MVarException) `shouldThrow` (== MVarException)
      takeMVar m `shouldReturn` "Hello"

      n <- newEmptyMVar
      tryPutDeepMVar n ("World" ++ impureThrow MVarException) `shouldThrow` (== MVarException)
      isEmptyMVar n `shouldReturn` True
    wit "writeMVar" $ do
      m <- newEmptyMVar
      writeMVar m "Hello"
      readMVar m `shouldReturn` "Hello"
      writeMVar m "World"
      readMVar m `shouldReturn` "World"
    wit "swapMVar" $ do
      m <- newMVar "Hello"
      swapMVar m "World" `shouldReturn` "Hello"
      swapMVar m (impureThrow MVarException) `shouldThrow` (== MVarException)
      readMVar m `shouldReturn` "World"
    wit "swapLazyMVar" $ do
      m <- newMVar "Hello"
      swapLazyMVar m "World" `shouldReturn` "Hello"
      readMVar m `shouldReturn` "World"
      swapLazyMVar m (impureThrow MVarException) `shouldReturn` "World"
      res <- takeMVar m
      evaluate res `shouldThrow` (== MVarException)
    wit "swapDeepMVar" $ do
      m <- newMVar "Hello"
      swapDeepMVar m "World" `shouldReturn` "Hello"
      swapDeepMVar m ("Booyah" ++ impureThrow MVarException) `shouldThrow` (== MVarException)
      readMVar m `shouldReturn` "World"
    wit "takeMVar" $ do
      m <- newMVar "Hello"
      takeMVar m `shouldReturn` "Hello"
      isEmptyMVar m `shouldReturn` True
      timeout 50000 (takeMVar m) `shouldReturn` Nothing
    wit "tryTakeMVar" $ do
      m <- newMVar "Hello"
      tryTakeMVar m `shouldReturn` Just "Hello"
      isEmptyMVar m `shouldReturn` True
      tryTakeMVar m `shouldReturn` Nothing
    wit "readMVar" $ do
      m <- newMVar "Hello"
      readMVar m `shouldReturn` "Hello"
      isEmptyMVar m `shouldReturn` False
      clearMVar m
      timeout 50000 (readMVar m) `shouldReturn` Nothing
    wit "tryReadMVar" $ do
      m <- newEmptyMVar
      tryReadMVar m `shouldReturn` Nothing
      putMVar m "Hello"
      tryReadMVar m `shouldReturn` Just "Hello"
      isEmptyMVar m `shouldReturn` False
    wit "clearMVar" $ do
      m <- newEmptyMVar
      clearMVar m
      isEmptyMVar m `shouldReturn` True
      putMVar m "Hello"
      clearMVar m
      isEmptyMVar m `shouldReturn` True
    wit "withMVar" $ do
      m <- newEmptyMVar
      void $ fork $ putMVar m "Hello"
      -- check masking state
      res <- withMVar m $ \x -> do
        x `shouldBe` "Hello"
        getMaskingState
      res `shouldBe` Unmasked

      -- check restoration of value on exception
      withMVar m (\_ -> do
        isEmptyMVar m `shouldReturn` True
        throwM MVarException)
        `shouldThrow` (==MVarException)
      readMVar m `shouldReturn` "Hello"

       -- check that it is interruptible and that the value is overwritten
      timeout 50000 (withMVar m (\_ -> putMVar m "World")) `shouldReturn` Nothing
      readMVar m `shouldReturn` "World"

       -- check that it is interruptible in the exception handler and that the value is
       -- overwritten
      timeout 50000 (withMVar m (\_ -> do
                                    putMVar m "Goodbye"
                                    () <$ throwM MVarException
                                )) `shouldReturn` Nothing
      takeMVar m `shouldReturn` "Goodbye"

      -- -- check that it is interruptible on empty
      -- timeout 50000 (withMVar m pure) `shouldReturn` Nothing

    wit "withMVarMasked" $ do
      m <- newMVar "Hello"
      -- check masking state
      res <- withMVarMasked m $ \x -> do
        x `shouldBe` "Hello"
        getMaskingState
      res `shouldBe` MaskedInterruptible

      -- check restoration of value on exception
      withMVarMasked m (\_ -> do
        isEmptyMVar m `shouldReturn` True
        throw MVarException)
        `shouldThrow` (==MVarException)
      readMVar m `shouldReturn` "Hello"

      -- check that it is interruptible and that the value is overwritten
      timeout 50000 (withMVarMasked m (\_ -> putMVar m "World")) `shouldReturn` Nothing
      readMVar m `shouldReturn` "World"

       -- check that it is interruptible in the exception handler and that the value is
       -- overwritten
      timeout 50000 (withMVarMasked m (\_ -> do
                                    putMVar m "Goodbye"
                                    () <$ throw MVarException
                                )) `shouldReturn` Nothing
      takeMVar m `shouldReturn` "Goodbye"

      -- check that it is interruptible on empty
      timeout 50000 (withMVarMasked m pure) `shouldReturn` Nothing

    wit "modifyMVar_" $ do
      m <- newMVar "Hello"

      -- check masking state and actual modification
      modifyMVar_ m $ \x -> do
        x `shouldBe` "Hello"
        getMaskingState `shouldReturn` Unmasked
        pure $ x ++ " World"

      -- Verify value restoration on WHNF evaluation error
      modifyMVar_ m (\x -> do
        isEmptyMVar m  `shouldReturn` True
        x `shouldBe` "Hello World"
        pure $ impureThrow MVarException)
        `shouldThrow` (==MVarException)
      readMVar m `shouldReturn` "Hello World"

      -- check that it is interruptible and that the value is overwritten
      timeout 50000 (modifyMVar_ m (\_ -> putMVar m "Foo" >> pure "Bar")) `shouldReturn` Nothing
      readMVar m `shouldReturn` "Foo"

       -- check that it is interruptible in the exception handler and that the value is
       -- overwritten
      timeout 50000 (modifyMVar_ m (\_ -> do
                                    putMVar m "Goodbye"
                                    "World" <$ throw MVarException
                                )) `shouldReturn` Nothing
      takeMVar m `shouldReturn` "Goodbye"

      -- check that it is interruptible on empty
      timeout 50000 (modifyMVar_ m pure) `shouldReturn` Nothing
    wit "modifyMVarMasked_" $ do
      m <- newMVar "Hello"

      -- check masking state and actual modification
      modifyMVarMasked_ m $ \x -> do
        x `shouldBe` "Hello"
        getMaskingState `shouldReturn` MaskedInterruptible
        pure $ x ++ " World"

      -- Verify value restoration on WHNF evaluation error
      modifyMVarMasked_ m (\x -> do
        isEmptyMVar m  `shouldReturn` True
        x `shouldBe` "Hello World"
        pure $ impureThrow MVarException)
        `shouldThrow` (==MVarException)
      readMVar m `shouldReturn` "Hello World"

      -- check that it is interruptible and that the value is overwritten
      timeout 50000 (modifyMVarMasked_ m (\_ -> putMVar m "Foo" >> pure "Bar"))
        `shouldReturn` Nothing
      readMVar m `shouldReturn` "Foo"

       -- check that it is interruptible in the exception handler and that the value is
       -- overwritten
      timeout 50000 (modifyMVarMasked_ m (\_ -> do
                                    putMVar m "Goodbye"
                                    "World" <$ throw MVarException
                                )) `shouldReturn` Nothing
      takeMVar m `shouldReturn` "Goodbye"

      -- check that it is interruptible on empty
      timeout 50000 (modifyMVarMasked_ m pure) `shouldReturn` Nothing
    wit "modifyFetchOldMVar" $ do
      m <- newMVar "Hello"
      modifyFetchOldMVar m (pure . (++ " World")) `shouldReturn` "Hello"
      readMVar m `shouldReturn` "Hello World"
      modifyFetchOldMVar m (\ _ -> pure $ impureThrow MVarException)
        `shouldThrow` (==MVarException)
      takeMVar m `shouldReturn` "Hello World"
    wit "modifyFetchOldMVarMasked" $ do
      m <- newMVar "Hello"
      modifyFetchOldMVarMasked m (pure . (++ " World")) `shouldReturn` "Hello"
      readMVar m `shouldReturn` "Hello World"
      modifyFetchOldMVarMasked m (\ _ -> pure $ impureThrow MVarException)
        `shouldThrow` (==MVarException)
      takeMVar m `shouldReturn` "Hello World"
    wit "modifyFetchNewMVar" $ do
      m <- newMVar "Hello"
      modifyFetchNewMVar m (pure . (++ " World")) `shouldReturn` "Hello World"
      readMVar m `shouldReturn` "Hello World"
      modifyFetchNewMVar m (\ _ -> pure $ impureThrow MVarException)
        `shouldThrow` (==MVarException)
      takeMVar m `shouldReturn` "Hello World"
    wit "modifyFetchNewMVarMasked" $ do
      m <- newMVar "Hello"
      modifyFetchNewMVarMasked m (pure . (++ " World")) `shouldReturn` "Hello World"
      readMVar m `shouldReturn` "Hello World"
      modifyFetchNewMVarMasked m (\ _ -> pure $ impureThrow MVarException)
        `shouldThrow` (==MVarException)
      takeMVar m `shouldReturn` "Hello World"
    xit "modifyMVar" (pure () :: IO ())
    xit "modifyMVarMasked" (pure () :: IO ())
    it "toBaseMVar" $ do
      m <- newMVar ()
      Base.takeMVar (toBaseMVar m) `shouldReturn` ()
      isEmptyMVar m `shouldReturn` True
    it "fromBaseMVar" $ do
      m <- Base.newMVar ()
      takeMVar (fromBaseMVar m) `shouldReturn` ()
      Base.isEmptyMVar m `shouldReturn` True
    describe "mkWeakMVar" $ do
      wit "performGC" $ do
        sem <- newEmptyMVar
        void $ fork $ do
          m <- newEmptyMVar
          _weak <- mkWeakMVar m $ putMVar sem ()
          performGC
        takeMVar sem `shouldReturn` ()
      wit "finalizeWeak" $ do
        sem <- newEmptyMVar
        m <- newMVar "Hello"
        weak <- mkWeakMVar m $ putMVar sem ()
        deRefWeak weak >>= \case
          Nothing -> expectationFailure "Empty weak ref"
          Just m' -> do
            m' `shouldBe` m
            readMVar m' `shouldReturn` "Hello"
        finalizeWeak weak
        takeMVar sem `shouldReturn` ()

