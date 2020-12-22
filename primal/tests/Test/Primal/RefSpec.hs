{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Primal.RefSpec (spec) where

import qualified Control.Concurrent as Base
import Primal.Concurrent
import Primal.Data.Ref
import Primal.Exception
import Data.Maybe
import Primal.Prim
import Primal.Mem.Weak
import Test.Hspec
import System.Mem (performGC)

instance Typeable a => Show (Ref a RW) where
  show _ = "Ref " ++ showsType (Proxy :: Proxy a) " RW"

data RefException =
  RefException
  deriving (Show, Eq)
instance Exception RefException


spec :: Spec
spec = do
  describe "Ref" $ do
    it "isSameRef" $ do
      ref1 <- newRef ()
      isSameRef ref1 ref1 `shouldBe` True
      ref1 `shouldBe` ref1
      ref2 <- newRef ()
      isSameRef ref1 ref2 `shouldBe` False
      ref1 `shouldSatisfy` (/= ref2)
    it "newRef" $ do
      ref <- newRef 'h'
      readRef ref `shouldReturn` 'h'
      newRef (impureThrow RefException) `shouldThrow` (== RefException)
      n :: Ref (Maybe Integer) RW <- newRef (Just (impureThrow RefException))
      mRes <- readRef n
      mRes `shouldSatisfy` isJust
      deepeval mRes `shouldThrow` (== RefException)
    it "newLazyRef" $ do
      ref <- newLazyRef 'h'
      readRef ref `shouldReturn` 'h'
      n <- newLazyRef (impureThrow RefException)
      evalM (readRef n) `shouldThrow` (== RefException)
    it "newDeepRef" $ do
      ref <- newDeepRef 'h'
      readRef ref `shouldReturn` 'h'
      newDeepRef (impureThrow RefException :: Int) `shouldThrow` (== RefException)
      newDeepRef (Just (impureThrow RefException :: Integer)) `shouldThrow` (== RefException)
    it "readRef" $ do
      ref <- newRef "Hello"
      readRef ref `shouldReturn` "Hello"
    it "writeRef" $ do
      ref <- newRef "Hello"
      readRef ref `shouldReturn` "Hello"
      writeRef ref "World"
      readRef ref `shouldReturn` "World"
    it "swapRef" $ do
      ref <- newRef "Hello"
      swapRef ref "World" `shouldReturn` "Hello"
      swapRef ref (impureThrow RefException) `shouldThrow` (== RefException)
      readRef ref `shouldReturn` "World"
    it "swapLazyRef" $ do
      ref <- newRef "Hello"
      swapLazyRef ref "World" `shouldReturn` "Hello"
      readRef ref `shouldReturn` "World"
      swapLazyRef ref (impureThrow RefException) `shouldReturn` "World"
      res <- readRef ref
      eval res `shouldThrow` (== RefException)
    it "swapDeepRef" $ do
      ref <- newRef "Hello"
      swapDeepRef ref "World" `shouldReturn` "Hello"
      swapDeepRef ref ("Booyah" ++ impureThrow RefException) `shouldThrow` (== RefException)
      readRef ref `shouldReturn` "World"
    it "modifyRef" $ do
      ref <- newRef "Hello"
      modifyRef ref (\x -> (x ++ " World", length x)) `shouldReturn` 5
      flip shouldThrow (== RefException) $ modifyRef ref $ \x -> (impureThrow RefException, x)
      readRef ref `shouldReturn` "Hello World"
      _ <- modifyRef ref $ \x -> (x ++ "!!!", impureThrow RefException)
      readRef ref `shouldReturn` "Hello World!!!"
    -- it "modifyFetchOldRef" $ do
    --   ref <- newRef "Hello"
    --   modifyRef ref (++ " World") `shouldReturn` "Hello"
    --   flip shouldThrow (== RefException) $ modifyRef ref $ \_ -> impureThrow RefException
    --   readRef ref `shouldReturn` "Hello World"
    it "modifyRefM_" $ do
      ref <- newRef "Hello"
      modifyRefM_ ref $ \x -> do
        x `shouldBe` "Hello"
        pure $ x ++ " World"
      flip shouldThrow (== RefException) $ modifyRefM_ ref $ \x -> do
        x `shouldBe` "Hello World"
        pure $ impureThrow RefException
      readRef ref `shouldReturn` "Hello World"

    --   -- Verify value restoration on WHNF evaluation error
    --   modifyRef_ ref (\x -> do
    --     isEmptyRef ref  `shouldReturn` True
    --     x `shouldBe` "Hello World"
    --     pure $ impureThrow RefException)
    --     `shouldThrow` (==RefException)
    --   readRef ref `shouldReturn` "Hello World"

    --   -- check that it is interruptible and that the value is overwritten
    --   timeout 50000 (modifyRef_ ref (\_ -> putRef ref "Foo" >> pure "Bar")) `shouldReturn` Nothing
    --   readRef ref `shouldReturn` "Foo"

    --    -- check that it is interruptible in the exception handler and that the value is
    --    -- overwritten
    --   timeout 50000 (modifyRef_ ref (\_ -> do
    --                                 putRef ref "Goodbye"
    --                                 "World" <$ throw RefException
    --                             )) `shouldReturn` Nothing
    --   takeRef ref `shouldReturn` "Goodbye"

    --   -- check that it is interruptible on empty
    --   timeout 50000 (modifyRef_ ref pure) `shouldReturn` Nothing
    -- it "modifyRefMasked_" $ do
    --   ref <- newRef "Hello"

    --   -- check masking state and actual modification
    --   modifyRefMasked_ ref $ \x -> do
    --     x `shouldBe` "Hello"
    --     getMaskingState `shouldReturn` MaskedInterruptible
    --     pure $ x ++ " World"

    --   -- Verify value restoration on WHNF evaluation error
    --   modifyRefMasked_ ref (\x -> do
    --     isEmptyRef ref  `shouldReturn` True
    --     x `shouldBe` "Hello World"
    --     pure $ impureThrow RefException)
    --     `shouldThrow` (==RefException)
    --   readRef ref `shouldReturn` "Hello World"

    --   -- check that it is interruptible and that the value is overwritten
    --   timeout 50000 (modifyRefMasked_ ref (\_ -> putRef ref "Foo" >> pure "Bar"))
    --     `shouldReturn` Nothing
    --   readRef ref `shouldReturn` "Foo"

    --    -- check that it is interruptible in the exception handler and that the value is
    --    -- overwritten
    --   timeout 50000 (modifyRefMasked_ ref (\_ -> do
    --                                 putRef ref "Goodbye"
    --                                 "World" <$ throw RefException
    --                             )) `shouldReturn` Nothing
    --   takeRef ref `shouldReturn` "Goodbye"

    --   -- check that it is interruptible on empty
    --   timeout 50000 (modifyRefMasked_ ref pure) `shouldReturn` Nothing
    -- it "modifyFetchOldRef" $ do
    --   ref <- newRef "Hello"
    --   modifyFetchOldRef ref (pure . (++ " World")) `shouldReturn` "Hello"
    --   readRef ref `shouldReturn` "Hello World"
    --   modifyFetchOldRef ref (\ _ -> pure $ impureThrow RefException)
    --     `shouldThrow` (==RefException)
    --   takeRef ref `shouldReturn` "Hello World"
    -- it "modifyFetchOldRefMasked" $ do
    --   ref <- newRef "Hello"
    --   modifyFetchOldRefMasked ref (pure . (++ " World")) `shouldReturn` "Hello"
    --   readRef ref `shouldReturn` "Hello World"
    --   modifyFetchOldRefMasked ref (\ _ -> pure $ impureThrow RefException)
    --     `shouldThrow` (==RefException)
    --   takeRef ref `shouldReturn` "Hello World"
    -- it "modifyFetchNewRef" $ do
    --   ref <- newRef "Hello"
    --   modifyFetchNewRef ref (pure . (++ " World")) `shouldReturn` "Hello World"
    --   readRef ref `shouldReturn` "Hello World"
    --   modifyFetchNewRef ref (\ _ -> pure $ impureThrow RefException)
    --     `shouldThrow` (==RefException)
    --   takeRef ref `shouldReturn` "Hello World"
    -- it "modifyFetchNewRefMasked" $ do
    --   ref <- newRef "Hello"
    --   modifyFetchNewRefMasked ref (pure . (++ " World")) `shouldReturn` "Hello World"
    --   readRef ref `shouldReturn` "Hello World"
    --   modifyFetchNewRefMasked ref (\ _ -> pure $ impureThrow RefException)
    --     `shouldThrow` (==RefException)
    --   takeRef ref `shouldReturn` "Hello World"
    -- -- xit "modifyRef" (pure () :: IO ())
    -- -- xit "modifyRefMasked" (pure () :: IO ())
    -- it "toBaseRef" $ do
    --   ref <- newRef ()
    --   Base.takeRef (toBaseRef ref) `shouldReturn` ()
    --   isEmptyRef ref `shouldReturn` True
    -- it "fromBaseRef" $ do
    --   ref <- Base.newRef ()
    --   takeRef (fromBaseRef ref) `shouldReturn` ()
    --   Base.isEmptyRef ref `shouldReturn` True
    -- describe "mkWeakRef" $ do
    --   it "performGC" $ do
    --     seref <- newEmptyRef
    --     void $ fork $ do
    --       ref <- newEmptyRef
    --       _weak <- mkWeakRef ref $ putRef seref ()
    --       performGC
    --     takeRef seref `shouldReturn` ()
    --   it "finalizeWeak" $ do
    --     seref <- newEmptyRef
    --     ref <- newRef "Hello"
    --     weak <- mkWeakRef ref $ putRef seref ()
    --     deRefWeak weak >>= \case
    --       Nothing -> expectationFailure "Empty weak ref"
    --       Just ref' -> do
    --         ref' `shouldBe` ref
    --         readRef ref' `shouldReturn` "Hello"
    --     finalizeWeak weak
    --     takeRef sem `shouldReturn` ()

