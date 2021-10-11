{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Primal.MVarSpec (spec) where

import qualified Control.Concurrent as Base
import Data.Maybe
import Primal.Concurrent
import Primal.Concurrent.MVar
import Primal.Exception
import Primal.Memory.GC (performGC)
import Primal.Memory.Weak
import Primal.Element.Unbox
import Test.Hspec
import Test.Primal.ArraySpec (ExpectedException(..), impreciseExpectedException)

instance Typeable a => Show (MVar a RW) where
  show _ = "MVar " ++ showsType (Proxy :: Proxy a) " RW"

-- | Turn a deadlock into a failing test.
failAfter :: HasCallStack => Int -> Expectation -> Expectation
failAfter n test =
  timeout n test >>= \case
    Nothing -> expectationFailure $ "Did not finish within " ++ show (n `div` 1000) ++ " ms. "
    Just () -> pure ()

wit :: HasCallStack => String -> Expectation-> Spec
wit n t = it n $ failAfter 1000000 t

spec :: Spec
spec = do
  describe "MVar" $ do
    wit "isEmptyMVar" $ do
      m :: MVar Int RW <- newEmptyMVar
      isEmptyMVar m `shouldReturn` True
      putMVar m 0
      isEmptyMVar m `shouldReturn` False
      (newMVar 'H' >>= isEmptyMVar) `shouldReturn` False
    wit "isSameMVar" $ do
      m1 :: MVar Int RW <- newEmptyMVar
      isSameMVar m1 m1 `shouldBe` True
      m1 `shouldBe` m1
      m2 :: MVar Int RW <- newEmptyMVar
      isSameMVar m1 m2 `shouldBe` False
      m1 `shouldSatisfy` (/= m2)
    wit "newMVar" $ do
      m <- newMVar 'h'
      readMVar m `shouldReturn` 'h'
      newMVar (raiseImprecise ExpectedException) `shouldThrow` impreciseExpectedException
      n :: MVar (Maybe Integer) RW <- newMVar (Just (raiseImprecise ExpectedException))
      mRes <- takeMVar n
      mRes `shouldSatisfy` isJust
      deepeval mRes `shouldThrow` impreciseExpectedException
    wit "newLazyMVar" $ do
      m <- newLazyMVar 'h'
      tryTakeMVar m `shouldReturn` Just 'h'
      n <- newLazyMVar (raiseImprecise ExpectedException)
      evalM (takeMVar n) `shouldThrow` impreciseExpectedException
    wit "newDeepMVar" $ do
      m <- newDeepMVar 'h'
      takeMVar m `shouldReturn` 'h'
      newDeepMVar (raiseImprecise ExpectedException :: Int) `shouldThrow` impreciseExpectedException
      newDeepMVar (Just (raiseImprecise ExpectedException :: Integer)) `shouldThrow` impreciseExpectedException
    wit "putMVar" $ do
      m <- newEmptyMVar
      void $ fork $ putMVar m "Hello"
      takeMVar m `shouldReturn` "Hello"

      n <- newMVar "World"
      timeout 50000 (putMVar n "Already full") `shouldReturn` Nothing
      void $ fork $ putMVar n (raiseImprecise ExpectedException)
      putMVar n (raiseImprecise ExpectedException) `shouldThrow` impreciseExpectedException
      takeMVar n `shouldReturn` "World"

      putMVar n ('f':raiseImprecise ExpectedException)
      res <- takeMVar n
      head res `shouldBe` 'f'
      deepeval res `shouldThrow` impreciseExpectedException
    wit "putLazyMVar" $ do
      m <- newEmptyMVar
      void $ fork $ putLazyMVar m "Hello"
      readMVar m `shouldReturn` "Hello"
      timeout 50000 (putLazyMVar m "Already full") `shouldReturn` Nothing

      n <- newEmptyMVar
      void $ fork $ putLazyMVar n (raiseImprecise ExpectedException)
      res <- takeMVar n
      eval res `shouldThrow` impreciseExpectedException
    wit "putDeepMVar" $ do
      m <- newEmptyMVar
      void $ fork $ putDeepMVar m "Hello"
      readMVar m `shouldReturn` "Hello"
      timeout 50000 (putDeepMVar m "Already full") `shouldReturn` Nothing

      n <- newMVar "World"
      void $ fork $ putDeepMVar n ("Bar" ++ raiseImprecise ExpectedException)
      putDeepMVar n ("Foo" ++ raiseImprecise ExpectedException) `shouldThrow` impreciseExpectedException
      threadDelay 10000
      takeMVar n `shouldReturn` "World"
    wit "tryPutMVar" $ do
      m <- newEmptyMVar
      tryPutMVar m "Hello" `shouldReturn` True
      tryPutMVar m "World" `shouldReturn` False
      tryPutMVar m (raiseImprecise ExpectedException) `shouldThrow` impreciseExpectedException
      takeMVar m `shouldReturn` "Hello"

      n <- newEmptyMVar
      void $ fork $ void $ tryPutMVar n (raiseImprecise ExpectedException)
      tryPutMVar n (raiseImprecise ExpectedException) `shouldThrow` impreciseExpectedException
      threadDelay 10000
      isEmptyMVar n `shouldReturn` True
    wit "tryPutLazyMVar" $ do
      m <- newEmptyMVar
      tryPutLazyMVar m "Hello" `shouldReturn` True
      tryPutLazyMVar m "World" `shouldReturn` False
      tryPutLazyMVar m (raiseImprecise ExpectedException) `shouldReturn` False
      takeMVar m `shouldReturn` "Hello"

      done <- newEmptyMVar
      n <- newEmptyMVar
      void $ fork $ do
        res <- tryPutLazyMVar n (raiseImprecise ExpectedException)
        void $ tryPutLazyMVar done res
      takeMVar done `shouldReturn` True
      isEmptyMVar n `shouldReturn` False
      res <- takeMVar n
      eval res `shouldThrow` impreciseExpectedException
    wit "tryPutDeepMVar" $ do
      m <- newEmptyMVar
      tryPutMVar m "Hello" `shouldReturn` True
      tryPutMVar m "World" `shouldReturn` False
      tryPutDeepMVar m ("Happy" ++ raiseImprecise ExpectedException) `shouldThrow` impreciseExpectedException
      takeMVar m `shouldReturn` "Hello"

      n <- newEmptyMVar
      tryPutDeepMVar n ("World" ++ raiseImprecise ExpectedException) `shouldThrow` impreciseExpectedException
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
      swapMVar m (raiseImprecise ExpectedException) `shouldThrow` impreciseExpectedException
      readMVar m `shouldReturn` "World"
    wit "swapLazyMVar" $ do
      m <- newMVar "Hello"
      swapLazyMVar m "World" `shouldReturn` "Hello"
      readMVar m `shouldReturn` "World"
      swapLazyMVar m (raiseImprecise ExpectedException) `shouldReturn` "World"
      res <- takeMVar m
      eval res `shouldThrow` impreciseExpectedException
    wit "swapDeepMVar" $ do
      m <- newMVar "Hello"
      swapDeepMVar m "World" `shouldReturn` "Hello"
      swapDeepMVar m ("Booyah" ++ raiseImprecise ExpectedException) `shouldThrow` impreciseExpectedException
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
        raiseM ExpectedException)
        `shouldThrow` (== ExpectedException)
      readMVar m `shouldReturn` "Hello"

       -- check that it is interruptible and that the value is overwritten
      timeout 50000 (withMVar m (\_ -> putMVar m "World")) `shouldReturn` Nothing
      readMVar m `shouldReturn` "World"

       -- check that it is interruptible in the exception handler and that the value is
       -- overwritten
      timeout 50000 (withMVar m (\_ -> do
                                    putMVar m "Goodbye"
                                    () <$ raiseM ExpectedException
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
        raise ExpectedException)
        `shouldThrow` (== ExpectedException)
      readMVar m `shouldReturn` "Hello"

      -- check that it is interruptible and that the value is overwritten
      timeout 50000 (withMVarMasked m (\_ -> putMVar m "World")) `shouldReturn` Nothing
      readMVar m `shouldReturn` "World"

       -- check that it is interruptible in the exception handler and that the value is
       -- overwritten
      timeout 50000 (withMVarMasked m (\_ -> do
                                    putMVar m "Goodbye"
                                    () <$ raise ExpectedException
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
        pure $ raiseImprecise ExpectedException)
        `shouldThrow` impreciseExpectedException
      readMVar m `shouldReturn` "Hello World"

      -- check that it is interruptible and that the value is overwritten
      timeout 50000 (modifyMVar_ m (\_ -> putMVar m "Foo" >> pure "Bar")) `shouldReturn` Nothing
      readMVar m `shouldReturn` "Foo"

       -- check that it is interruptible in the exception handler and that the value is
       -- overwritten
      timeout 50000 (modifyMVar_ m (\_ -> do
                                    putMVar m "Goodbye"
                                    "World" <$ raise ExpectedException
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
        pure $ raiseImprecise ExpectedException)
        `shouldThrow` impreciseExpectedException
      readMVar m `shouldReturn` "Hello World"

      -- check that it is interruptible and that the value is overwritten
      timeout 50000 (modifyMVarMasked_ m (\_ -> putMVar m "Foo" >> pure "Bar"))
        `shouldReturn` Nothing
      readMVar m `shouldReturn` "Foo"

       -- check that it is interruptible in the exception handler and that the value is
       -- overwritten
      timeout 50000 (modifyMVarMasked_ m (\_ -> do
                                    putMVar m "Goodbye"
                                    "World" <$ raise ExpectedException
                                )) `shouldReturn` Nothing
      takeMVar m `shouldReturn` "Goodbye"

      -- check that it is interruptible on empty
      timeout 50000 (modifyMVarMasked_ m pure) `shouldReturn` Nothing
    wit "modifyFetchOldMVar" $ do
      m <- newMVar "Hello"
      modifyFetchOldMVar m (pure . (++ " World")) `shouldReturn` "Hello"
      readMVar m `shouldReturn` "Hello World"
      modifyFetchOldMVar m (\ _ -> pure $ raiseImprecise ExpectedException)
        `shouldThrow` impreciseExpectedException
      takeMVar m `shouldReturn` "Hello World"
    wit "modifyFetchOldMVarMasked" $ do
      m <- newMVar "Hello"
      modifyFetchOldMVarMasked m (pure . (++ " World")) `shouldReturn` "Hello"
      readMVar m `shouldReturn` "Hello World"
      modifyFetchOldMVarMasked m (\ _ -> pure $ raiseImprecise ExpectedException)
        `shouldThrow` impreciseExpectedException
      takeMVar m `shouldReturn` "Hello World"
    wit "modifyFetchNewMVar" $ do
      m <- newMVar "Hello"
      modifyFetchNewMVar m (pure . (++ " World")) `shouldReturn` "Hello World"
      readMVar m `shouldReturn` "Hello World"
      modifyFetchNewMVar m (\ _ -> pure $ raiseImprecise ExpectedException)
        `shouldThrow` impreciseExpectedException
      takeMVar m `shouldReturn` "Hello World"
    wit "modifyFetchNewMVarMasked" $ do
      m <- newMVar "Hello"
      modifyFetchNewMVarMasked m (pure . (++ " World")) `shouldReturn` "Hello World"
      readMVar m `shouldReturn` "Hello World"
      modifyFetchNewMVarMasked m (\ _ -> pure $ raiseImprecise ExpectedException)
        `shouldThrow` impreciseExpectedException
      takeMVar m `shouldReturn` "Hello World"
    -- xit "modifyMVar" (pure () :: IO ())
    -- xit "modifyMVarMasked" (pure () :: IO ())
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

