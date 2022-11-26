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
module Primal.Ref.Boxed
  ( BRef (..)

    -- * Create
  , newBRef
  , newDeepBRef
  , newLazyBRef
  , isSameBRef

    -- * Read/write
  , readBRef
  , writeBRef
  , writeDeepBRef
  , writeLazyBRef
  , writeFetchOldBRef
  , writeFetchOldDeepBRef
  , writeFetchOldLazyBRef

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
  , mkWeakNoFinalizerBRef
  ) where

import Control.DeepSeq
import qualified GHC.IORef as IO
import qualified GHC.STRef as ST
import Primal.Eval
import Primal.Foreign
import Primal.Memory.Weak
import Primal.Monad

-- | Mutable variable that can hold any value. This is just like `Data.STRef.STRef`, but
-- with type arguments flipped and is generalized to work in `Primal`. It only stores a
-- reference to the value which means it works on boxed values. If the type can be unboxed
-- with `Primal.Prim.Class` class, consider using
-- [@PVar@](https://hackage.haskell.org/package/pvar) package instead.
--
-- @since 1.0.0
data BRef e s = BRef (MutVar# s e)

-- | Uses `isSameBRef`
instance Eq (BRef e s) where
  (==) = isSameBRef
  {-# INLINE (==) #-}

instance NFData e => MutNFData (BRef e) where
  rnfMutST ref = rnf <$> readBRef ref
  {-# INLINE rnfMutST #-}

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
newBRef :: Primal s m => a -> m (BRef a s)
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
newDeepBRef :: (NFData a, Primal s m) => a -> m (BRef a s)
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
newLazyBRef :: Primal s m => a -> m (BRef a s)
newLazyBRef a =
  primal $ \s ->
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
readBRef :: Primal s m => BRef a s -> m a
readBRef (BRef ref#) = primal (readMutVar# ref#)
{-# INLINE readBRef #-}

-- | Swap the contents of a mutable variable with a new value, while retrieving the old
-- one. New value is evaluated prior to it being written to the variable.
--
-- ==== __Examples__
--
-- >>> ref <- newBRef (Left "Initial" :: Either String String)
-- >>> writeFetchOldBRef ref (Right "Last")
-- Left "Initial"
-- >>> readBRef ref
-- Right "Last"
--
-- @since 1.0.0
writeFetchOldBRef :: Primal s m => BRef a s -> a -> m a
writeFetchOldBRef ref a = readBRef ref <* writeBRef ref a
{-# INLINE writeFetchOldBRef #-}

-- | Swap the contents of a mutable variable with a new value lazily, while retrieving the old
-- one. New value is __not__ evaluated prior to it being written to the variable.
--
-- ==== __Examples__
--
-- >>> ref <- newBRef "Initial"
-- >>> writeFetchOldLazyBRef ref undefined
-- "Initial"
-- >>> _ <- writeFetchOldLazyBRef ref "Different"
-- >>> readBRef ref
-- "Different"
--
-- @since 1.0.0
writeFetchOldLazyBRef :: Primal s m => BRef a s -> a -> m a
writeFetchOldLazyBRef ref a = readBRef ref <* writeLazyBRef ref a
{-# INLINE writeFetchOldLazyBRef #-}

-- | Swap the contents of a mutable variable with a new value, while retrieving the old one. New
-- value is evaluated to __normal__ form prior to it being written to the variable.
--
-- ==== __Examples__
--
-- >>> ref <- newBRef (Just "Initial")
-- >>> writeFetchOldDeepBRef ref (Just (errorWithoutStackTrace "bar"))
-- *** Exception: bar
-- >>> readBRef ref
-- Just "Initial"
--
-- @since 1.0.0
writeFetchOldDeepBRef :: (NFData a, Primal s m) => BRef a s -> a -> m a
writeFetchOldDeepBRef ref a = readBRef ref <* writeDeepBRef ref a
{-# INLINE writeFetchOldDeepBRef #-}

-- | Write a value into a mutable variable strictly. If evaluating a value results in
-- exception, original value in the mutable variable will not be affected. Another great
-- benfit of this over `writeLazyBRef` is that it helps avoiding memory leaks.
--
-- ==== __Examples__
--
-- >>> ref <- newBRef "Original value"
-- >>> import Primal.Exception
-- >>> Left _exc <- tryAll $ writeBRef ref undefined
-- >>> readBRef ref
-- "Original value"
-- >>> writeBRef ref "New total value"
-- >>> readBRef ref
-- "New total value"
--
-- @since 1.0.0
writeBRef :: Primal s m => BRef a s -> a -> m ()
writeBRef ref !a = writeLazyBRef ref a
{-# INLINE writeBRef #-}

-- | Same as `writeBRef`, but will evaluate the argument to Normal Form prior to writing it
-- to the `BRef`
--
-- @since 1.0.0
writeDeepBRef :: (NFData a, Primal s m) => BRef a s -> a -> m ()
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
writeLazyBRef :: Primal s m => BRef a s -> a -> m ()
writeLazyBRef (BRef ref#) a = primal_ (writeMutVar# ref# a)
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
modifyBRef :: Primal s m => BRef a s -> (a -> (a, b)) -> m b
modifyBRef ref f = modifyBRefM ref (pure . f)
{-# INLINE modifyBRef #-}

-- | Same as `modifyBRef`, except it will evaluate result of computation to normal form.
--
-- @since 1.0.0
modifyDeepBRef :: (NFData a, Primal s m) => BRef a s -> (a -> (a, b)) -> m b
modifyDeepBRef ref f = modifyDeepBRefM ref (pure . f)
{-# INLINE modifyDeepBRef #-}

-- | Apply a pure function to the contents of a mutable variable strictly.
--
-- @since 1.0.0
modifyBRef_ :: Primal s m => BRef a s -> (a -> a) -> m ()
modifyBRef_ ref f = modifyBRefM_ ref (pure . f)
{-# INLINE modifyBRef_ #-}

-- | Apply a pure function to the contents of a mutable variable strictly. Returns the new value.
--
-- @since 1.0.0
modifyFetchNewBRef :: Primal s m => BRef a s -> (a -> a) -> m a
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
modifyFetchOldBRef :: Primal s m => BRef a s -> (a -> a) -> m a
modifyFetchOldBRef ref f = modifyFetchOldBRefM ref (pure . f)
{-# INLINE modifyFetchOldBRef #-}

-- | Apply a pure function to the contents of a mutable variable lazily. Returns the
-- artifact produced by the modifying function.
--
-- @since 1.0.0
modifyLazyBRef :: Primal s m => BRef a s -> (a -> (a, b)) -> m b
modifyLazyBRef ref f = modifyLazyBRefM ref (pure . f)
{-# INLINE modifyLazyBRef #-}

-- | Modify value of a mutable variable with a monadic action. It is not strict in a
-- return value of type @b@, but the new value written into the mutable variable is
-- evaluated to WHNF.
--
-- ==== __Examples__
modifyBRefM :: Primal s m => BRef a s -> (a -> m (a, b)) -> m b
modifyBRefM ref f = do
  (a, b) <- f =<< readBRef ref
  b <$ writeBRef ref a
{-# INLINE modifyBRefM #-}

-- | Same as `modifyBRefM`, except evaluates new value to normal form prior ot it being
-- written to the mutable ref.
modifyDeepBRefM :: (NFData a, Primal s m) => BRef a s -> (a -> m (a, b)) -> m b
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
modifyBRefM_ :: Primal s m => BRef a s -> (a -> m a) -> m ()
modifyBRefM_ ref f = readBRef ref >>= f >>= writeBRef ref
{-# INLINE modifyBRefM_ #-}

-- | Apply a monadic action to the contents of a mutable variable strictly. Returns the old value.
--
-- ==== __Examples__
--
-- >>> refGreeting <- newBRef "Hello "
-- >>> refName <- newBRef "Alexey"
-- >>> oldName <- modifyFetchOldBRefM refName $ \ name -> "Leo" <$ modifyBRef_ refGreeting (++ name)
-- >>> putStrLn =<< readBRef refGreeting
-- Hello Alexey
-- >>> putStrLn oldName
-- Alexey
-- >>> putStrLn =<< readBRef refName
-- Leo
--
-- @since 1.0.0
modifyFetchOldBRefM :: Primal s m => BRef a s -> (a -> m a) -> m a
modifyFetchOldBRefM ref f = do
  a <- readBRef ref
  a <$ (writeBRef ref =<< f a)
{-# INLINE modifyFetchOldBRefM #-}

-- | Apply a monadic action to the contents of a mutable variable strictly. Returns the new value.
--
-- @since 1.0.0
modifyFetchNewBRefM :: Primal s m => BRef a s -> (a -> m a) -> m a
modifyFetchNewBRefM ref f = do
  a <- readBRef ref
  a' <- f a
  a' <$ writeBRef ref a'
{-# INLINE modifyFetchNewBRefM #-}

-- | Same as `modifyBRefM`, but do not evaluate the new value written into the `BRef`.
--
-- @since 1.0.0
modifyLazyBRefM :: Primal s m => BRef a s -> (a -> m (a, b)) -> m b
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
-- Similar to `Data.IORef.mkWeakRef` from @base@, but works in any `UnliftPrimal` with
-- `RealWorld` state token and accepts another value to be stored in a weak ptr.
--
-- @since 1.0.0
mkWeakBRef
  :: forall a b c m
   . UnliftPrimal RW m
  => BRef a RW
  -- ^ Reference that will act as a key for the newly created weak pointer
  -> b
  -- ^ Value that can be later dereferenced with `deRefWeak`.
  -> m c
  -- ^ An action that will get executed whenever `BRef` gets garbage collected by
  -- the runtime.
  -> m (Weak b)
mkWeakBRef (BRef ref#) val finalizer =
  runInPrimalState finalizer $ \f# s ->
    case mkWeak# ref# val f# s of
      (# s', weak# #) -> (# s', Weak weak# #)
{-# INLINE mkWeakBRef #-}

-- | Create a `Weak` pointer associated with the supplied `BRef`.
--
-- Similar to `mkWeakBRef`, except it does not require a finalizer. This is useful for FFI
-- finalizers. One can be added later with `addCFinalizer` or `addCFinalizerEnv`
--
-- @since 1.0.0
mkWeakNoFinalizerBRef
  :: forall a b m
   . UnliftPrimal RW m
  => BRef a RW
  -- ^ Reference that will act as a key for the newly created weak pointer
  -> b
  -- ^ Value that can be later dereferenced with `deRefWeak`
  -> m (Weak b)
mkWeakNoFinalizerBRef (BRef ref#) val =
  primal $ \s ->
    case mkWeakNoFinalizer# ref# val s of
      (# s', weak# #) -> (# s', Weak weak# #)
{-# INLINE mkWeakNoFinalizerBRef #-}
