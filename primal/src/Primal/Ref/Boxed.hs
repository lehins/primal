{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Primal.Ref.Boxed
-- Copyright   : (c) Alexey Kuleshevich 2020-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Ref.Boxed
  ( BRef(..)
  -- * Create
  , newBRef
  , newDeepBRef
  , newLazyBRef
  , isSameBRef
  -- * Read/write
  , readBRef
  , swapBRef
  , swapDeepBRef
  , swapLazyBRef
  , writeBRef
  , writeDeepBRef
  , writeLazyBRef
  -- * Modify
  -- ** Pure
  , modifyBRef
  , modifyDeepBRef
  , modifyLazyBRef
  , modifyBRef_
  , modifyFetchNewBRef
  , modifyFetchOldBRef
  -- ** Monadic
  , modifyBRefM
  , modifyDeepBRefM
  , modifyLazyBRefM
  , modifyBRefM_
  , modifyFetchNewBRefM
  , modifyFetchOldBRefM
  -- * Conversion
  -- ** STBRef
  , toSTRef
  , fromSTRef
  -- ** IORef
  , toIORef
  , fromIORef
  -- * Weak Pointer
  , mkWeakBRef
  ) where

import Control.DeepSeq
import Primal.Monad
import Primal.Foreign
import Primal.Mem.Weak
import qualified GHC.IORef as IO
import qualified GHC.STRef as ST

-- | Mutable variable that can hold any value. This is just like `Data.STRef.STRef`, but
-- with type arguments flipped and is generalized to work in `MonadPrim`. It only stores a
-- reference to the value which means it works on boxed values. If the type can be unboxed
-- with `Primal.Prim.Class` class, consider using
-- [@PVar@](https://hackage.haskell.org/package/pvar) package instead.
--
-- @since 1.0.0
data BRef a s = BRef (MutVar# s a)

-- | Uses `isSameBRef`
instance Eq (BRef a s) where
  (==) = isSameBRef

-- | Check whether supplied `BRef`s refer to the exact same one or not.
--
-- @since 1.0.0
isSameBRef :: BRef a s -> BRef a s -> Bool
isSameBRef (BRef ref1#) (BRef ref2#) = isTrue# (sameMutVar# ref1# ref2#)
{-# INLINE isSameBRef #-}


-- | Create a new mutable variable. Initial value will be forced to WHNF (weak head normal form).
--
-- ==== __Examples__
--
-- >>> import Debug.Trace
-- >>> import Primal.Ref
-- >>> ref <- newBRef (trace "Initial value is evaluated" (217 :: Int))
-- Initial value is evaluated
-- >>> modifyFetchOldBRef ref succ
-- 217
-- >>> readBRef ref
-- 218
--
-- @since 1.0.0
newBRef :: MonadPrim s m => a -> m (BRef a s)
newBRef a = a `seq` newLazyBRef a
{-# INLINE newBRef #-}


-- | Create a new mutable variable. Same as `newBRef`, but ensures that value is evaluated
-- to normal form.
--
-- ==== __Examples__
--
-- >>> import Debug.Trace
-- >>> import Primal.Ref
-- >>> ref <- newDeepBRef (Just (trace "Initial value is evaluated" (217 :: Int)))
-- Initial value is evaluated
-- >>> readBRef ref
-- Just 217
--
-- @since 1.0.0
newDeepBRef :: (NFData a, MonadPrim s m) => a -> m (BRef a s)
newDeepBRef a = a `deepseq` newLazyBRef a
{-# INLINE newDeepBRef #-}

-- | Create a new mutable variable. Initial value stays unevaluated.
--
-- ==== __Examples__
--
-- In below example you will see that initial value is never evaluated.
--
-- >>> import Debug.Trace
-- >>> import Primal.Ref
-- >>> ref <- newLazyBRef (trace "Initial value is evaluated" (undefined :: Int))
-- >>> writeBRef ref 1024
-- >>> modifyFetchNewBRef ref succ
-- 1025
--
-- @since 1.0.0
newLazyBRef :: MonadPrim s m => a -> m (BRef a s)
newLazyBRef a =
  prim $ \s ->
    case newMutVar# a s of
      (# s', ref# #) -> (# s', BRef ref# #)
{-# INLINE newLazyBRef #-}

----------------
-- Read/Write --
----------------

-- | Read contents of the mutable variable
--
-- ==== __Examples__
--
-- >>> import Primal.Ref
-- >>> ref <- newBRef "Hello World!"
-- >>> readBRef ref
-- "Hello World!"
--
-- @since 1.0.0
readBRef :: MonadPrim s m => BRef a s -> m a
readBRef (BRef ref#) = prim (readMutVar# ref#)
{-# INLINE readBRef #-}


-- | Swap a value of a mutable variable with a new one, while retrieving the old one. New
-- value is evaluated prior to it being written to the variable.
--
-- ==== __Examples__
--
-- >>> ref <- newBRef (Left "Initial" :: Either String String)
-- >>> swapBRef ref (Right "Last")
-- Left "Initial"
-- >>> readBRef ref
-- Right "Last"
--
-- @since 1.0.0
swapBRef :: MonadPrim s m => BRef a s -> a -> m a
swapBRef ref a = readBRef ref <* writeBRef ref a
{-# INLINE swapBRef #-}


-- | Swap a value of a mutable variable with a new one lazily, while retrieving the old
-- one. New value is __not__ evaluated prior to it being written to the variable.
--
-- ==== __Examples__
--
-- >>> ref <- newBRef "Initial"
-- >>> swapLazyBRef ref undefined
-- "Initial"
-- >>> _ <- swapLazyBRef ref "Different"
-- >>> readBRef ref
-- "Different"
--
-- @since 1.0.0
swapLazyBRef :: MonadPrim s m => BRef a s -> a -> m a
swapLazyBRef ref a = readBRef ref <* writeLazyBRef ref a
{-# INLINE swapLazyBRef #-}


-- | Swap a value of a mutable variable with a new one, while retrieving the old one. New
-- value is evaluated to __normal__ form prior to it being written to the variable.
--
-- ==== __Examples__
--
-- >>> ref <- newBRef (Just "Initial")
-- >>> swapDeepBRef ref (Just (errorWithoutStackTrace "foo"))
-- *** Exception: foo
-- >>> readBRef ref
-- Just "Initial"
--
-- @since 1.0.0
swapDeepBRef :: (NFData a, MonadPrim s m) => BRef a s -> a -> m a
swapDeepBRef ref a = readBRef ref <* writeDeepBRef ref a
{-# INLINE swapDeepBRef #-}


-- | Write a value into a mutable variable strictly. If evaluating a value results in
-- exception, original value in the mutable variable will not be affected. Another great
-- benfit of this over `writeLazyBRef` is that it helps avoiding memory leaks.
--
-- ==== __Examples__
--
-- >>> ref <- newBRef "Original value"
-- >>> import Primal.Exception
-- >>> _ <- try $ writeBRef ref undefined :: IO (Either SomeException ())
-- >>> readBRef ref
-- "Original value"
-- >>> writeBRef ref "New total value"
-- >>> readBRef ref
-- "New total value"
--
-- @since 1.0.0
writeBRef :: MonadPrim s m => BRef a s -> a -> m ()
writeBRef ref !a = writeLazyBRef ref a
{-# INLINE writeBRef #-}


-- | Same as `writeBRef`, but will evaluate the argument to Normal Form prior to writing it
-- to the `BRef`
--
-- @since 1.0.0
writeDeepBRef :: (NFData a, MonadPrim s m) => BRef a s -> a -> m ()
writeDeepBRef ref a = a `deepseq` writeLazyBRef ref a
{-# INLINE writeDeepBRef #-}

-- | Write a value into a mutable variable lazily.
--
-- ==== __Examples__
--
-- >>> ref <- newBRef "Original value"
-- >>> import Debug.Trace
-- >>> writeLazyBRef ref (trace "'New string' is evaluated" "New string")
-- >>> x <- readBRef ref
-- >>> writeBRef ref (trace "'Totally new string' is evaluated" "Totally new string")
-- 'Totally new string' is evaluated
-- >>> putStrLn x
-- 'New string' is evaluated
-- New string
--
-- @since 1.0.0
writeLazyBRef :: MonadPrim s m => BRef a s -> a -> m ()
writeLazyBRef (BRef ref#) a = prim_ (writeMutVar# ref# a)
{-# INLINE writeLazyBRef #-}


------------
-- Modify --
------------


-- | Apply a pure function to the contents of a mutable variable strictly. Returns the
-- artifact produced by the modifying function. Artifact is not forced, therfore it cannot
-- affect the outcome of modification. This function is a faster alternative to
-- `atomicModifyBRef`, except without any guarantees of atomicity and ordering of mutable
-- operations during concurrent modification of the same `BRef`. For lazy version see
-- `modifyLazyBRef` and for strict evaluation to normal form see `modifyDeepBRef`.
--
-- @since 1.0.0
modifyBRef :: MonadPrim s m => BRef a s -> (a -> (a, b)) -> m b
modifyBRef ref f = modifyBRefM ref (pure . f)
{-# INLINE modifyBRef #-}

-- | Same as `modifyBRef`, except it will evaluate result of computation to normal form.
--
-- @since 1.0.0
modifyDeepBRef :: (NFData a, MonadPrim s m) => BRef a s -> (a -> (a, b)) -> m b
modifyDeepBRef ref f = modifyDeepBRefM ref (pure . f)
{-# INLINE modifyDeepBRef #-}

-- | Apply a pure function to the contents of a mutable variable strictly.
--
-- @since 1.0.0
modifyBRef_ :: MonadPrim s m => BRef a s -> (a -> a) -> m ()
modifyBRef_ ref f = modifyBRefM_ ref (pure . f)
{-# INLINE modifyBRef_ #-}

-- | Apply a pure function to the contents of a mutable variable strictly. Returns the new value.
--
-- @since 1.0.0
modifyFetchNewBRef :: MonadPrim s m => BRef a s -> (a -> a) -> m a
modifyFetchNewBRef ref f = modifyFetchNewBRefM ref (pure . f)
{-# INLINE modifyFetchNewBRef #-}

-- | Apply a pure function to the contents of a mutable variable strictly. Returns the old value.
--
-- ==== __Examples__
--
-- >>> ref1 <- newBRef (10 :: Int)
-- >>> ref2 <- newBRef (201 :: Int)
-- >>> modifyBRefM_ ref1 (\x -> modifyFetchOldBRef ref2 (* x))
-- >>> readBRef ref1
-- 201
-- >>> readBRef ref2
-- 2010
--
-- @since 1.0.0
modifyFetchOldBRef :: MonadPrim s m => BRef a s -> (a -> a) -> m a
modifyFetchOldBRef ref f = modifyFetchOldBRefM ref (pure . f)
{-# INLINE modifyFetchOldBRef #-}


-- | Apply a pure function to the contents of a mutable variable lazily. Returns the
-- artifact produced by the modifying function.
--
-- @since 1.0.0
modifyLazyBRef :: MonadPrim s m => BRef a s -> (a -> (a, b)) -> m b
modifyLazyBRef ref f = modifyLazyBRefM ref (pure . f)
{-# INLINE modifyLazyBRef #-}



-- | Modify value of a mutable variable with a monadic action. It is not strict in a
-- return value of type @b@, but the ne value written into the mutable variable is
-- evaluated to WHNF.
--
-- ==== __Examples__
--
modifyBRefM :: MonadPrim s m => BRef a s -> (a -> m (a, b)) -> m b
modifyBRefM ref f = do
  (a', b) <- f =<< readBRef ref
  b <$ writeBRef ref a'
{-# INLINE modifyBRefM #-}


-- | Same as `modifyBRefM`, except evaluates new value to normal form prior ot it being
-- written to the mutable ref.
modifyDeepBRefM :: (NFData a, MonadPrim s m) => BRef a s -> (a -> m (a, b)) -> m b
modifyDeepBRefM ref f = do
  (a', b) <- f =<< readBRef ref
  b <$ writeDeepBRef ref a'
{-# INLINE modifyDeepBRefM #-}


-- | Modify value of a mutable variable with a monadic action. Result is written strictly.
--
-- ==== __Examples__
--
-- >>> ref <- newBRef (Just "Some value")
-- >>> modifyBRefM_ ref $ \ mv -> Nothing <$ mapM_ putStrLn mv
-- Some value
-- >>> readBRef ref
-- Nothing
--
-- @since 1.0.0
modifyBRefM_ :: MonadPrim s m => BRef a s -> (a -> m a) -> m ()
modifyBRefM_ ref f = readBRef ref >>= f >>= writeBRef ref
{-# INLINE modifyBRefM_ #-}

-- | Apply a monadic action to the contents of a mutable variable strictly. Returns the old value.
--
-- ==== __Examples__
--
-- >>> refName <- newBRef "My name is: "
-- >>> refMyName <- newBRef "Alexey"
-- >>> myName <- modifyFetchOldBRefM refMyName $ \ name -> "Leo" <$ modifyBRef_ refName (++ name)
-- >>> readBRef refName >>= putStrLn
-- My name is: Alexey
-- >>> putStrLn myName
-- Alexey
-- >>> readBRef refMyName >>= putStrLn
-- Leo
--
-- @since 1.0.0
modifyFetchOldBRefM :: MonadPrim s m => BRef a s -> (a -> m a) -> m a
modifyFetchOldBRefM ref f = do
  a <- readBRef ref
  a <$ (writeBRef ref =<< f a)
{-# INLINE modifyFetchOldBRefM #-}


-- | Apply a monadic action to the contents of a mutable variable strictly. Returns the new value.
--
-- @since 1.0.0
modifyFetchNewBRefM :: MonadPrim s m => BRef a s -> (a -> m a) -> m a
modifyFetchNewBRefM ref f = do
  a <- readBRef ref
  a' <- f a
  a' <$ writeBRef ref a'
{-# INLINE modifyFetchNewBRefM #-}

-- | Same as `modifyBRefM`, but do not evaluate the new value written into the `BRef`.
--
-- @since 1.0.0
modifyLazyBRefM :: MonadPrim s m => BRef a s -> (a -> m (a, b)) -> m b
modifyLazyBRefM ref f = do
  a <- readBRef ref
  (a', b) <- f a
  b <$ writeLazyBRef ref a'
{-# INLINE modifyLazyBRefM #-}

-- | Convert `BRef` to `STRef`
--
-- @since 1.0.0
toSTRef :: BRef a s -> ST.STRef s a
toSTRef (BRef ref#) = ST.STRef ref#
{-# INLINE toSTRef #-}

-- | Convert `STRef` to `BRef`
--
-- @since 1.0.0
fromSTRef :: ST.STRef s a -> BRef a s
fromSTRef (ST.STRef ref#) = BRef ref#
{-# INLINE fromSTRef #-}

-- | Convert `BRef` to `IORef`
--
-- @since 1.0.0
toIORef :: BRef a RW -> IO.IORef a
toIORef = coerce . toSTRef
{-# INLINE toIORef #-}

-- | Convert `IORef` to `BRef`
--
-- @since 1.0.0
fromIORef :: IO.IORef a -> BRef a RW
fromIORef = fromSTRef . coerce
{-# INLINE fromIORef #-}




-- | Create a `Weak` pointer associated with the supplied `BRef`.
--
-- Same as `Data.IOBRef.mkWeakBRef` from @base@, but works in any `MonadPrim` with
-- `RealWorld` state token.
--
-- @since 1.0.0
mkWeakBRef ::
     forall a b m. MonadUnliftPrim RW m
  => BRef a RW
  -> m b -- ^ An action that will get executed whenever `BRef` gets garbage collected by
         -- the runtime.
  -> m (Weak (BRef a RW))
mkWeakBRef ref@(BRef ref#) !finalizer =
  runInPrimBase finalizer $ \f# s ->
    case mkWeak# ref# ref f# s of
      (# s', weak# #) -> (# s', Weak weak# #)
{-# INLINE mkWeakBRef #-}
