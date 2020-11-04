{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Data.Prim.Ref
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Ref
  ( Ref(..)
  , IORef
  , STRef
  -- * Create
  , newRef
  , isSameRef
  -- * Read/write
  , readRef
  , swapRef
  , writeRef
  , writeDeepRef
  -- * Modify
  -- ** Pure
  , modifyRef
  , modifyRef_
  , modifyFetchNewRef
  , modifyFetchOldRef
  -- ** Monadic
  , modifyRefM
  , modifyRefM_
  , modifyFetchNewRefM
  , modifyFetchOldRefM
  -- * Atomic
  , atomicReadRef
  , atomicSwapRef
  , atomicWriteRef
  , atomicModifyRef
  , atomicModifyRef_
  , atomicModifyFetchRef
  , atomicModifyFetchNewRef
  , atomicModifyFetchOldRef
  , atomicModifyFetchBothRef
  -- ** Original
  , casRef
  , atomicModifyRef2
  , atomicModifyRef2_
  , atomicModifyFetchNewRef2
  , atomicModifyFetchOldRef2
  , atomicModifyFetchBothRef2
  , atomicModifyFetchRef2
  -- * Lazy
  -- It is recommended to refrain from usage of lazy functions because they are a memory
  -- leak waiting to happen
  , newLazyRef
  , writeLazyRef
  , modifyLazyRef
  , modifyLazyRefM
  , atomicWriteLazyRef
  , atomicModifyLazyRef
  , atomicModifyFetchNewLazyRef
  , atomicModifyFetchOldLazyRef
  , atomicModifyFetchBothLazyRef
  , atomicModifyFetchLazyRef
  -- * Conversion
  -- ** STRef
  , toSTRef
  , fromSTRef
  -- ** IORef
  , toIORef
  , fromIORef
  -- * Weak Pointer
  , mkWeakRef
  ) where

import Control.DeepSeq
import Control.Prim.Monad
import Foreign.Prim
import Foreign.Prim.WeakPtr
import qualified GHC.IORef as IO
import qualified GHC.STRef as ST

-- | Mutable variable that can hold any value. This is just like `Data.STRef.STRef`, but
-- with type arguments flipped and is generalized to work in `MonadPrim`. It only stores a
-- reference to the value which means it works on boxed values. If the type can be unboxed
-- with `Data.Prim.Class.Prim` class, consider using
-- [@PVar@](https://hackage.haskell.org/package/pvar) package instead.
--
-- @since 0.3.0
data Ref a s = Ref (MutVar# s a)

-- | Uses `isSameRef`
instance Eq (Ref a s) where
  (==) = isSameRef

-- | Compatibility synonym
type IORef a = Ref a RW

-- | Compatibility synonym
type STRef s a = Ref a s

-- | Check whether supplied `Ref`s refer to the exact same one or not.
--
-- @since 0.3.0
isSameRef :: Ref a s -> Ref a s -> Bool
isSameRef (Ref ref1#) (Ref ref2#) = isTrue# (sameMutVar# ref1# ref2#)
{-# INLINE isSameRef #-}


-- | Create a new mutable variable. Initial value will be forced to WHNF (weak head normal form).
--
-- ==== __Examples__
--
-- >>> import Debug.Trace
-- >>> import Data.Prim.Ref
-- >>> ref <- newRef (trace "Initial value is evaluated" (217 :: Int))
-- Initial value is evaluated
-- >>> modifyFetchOldRef ref succ
-- 217
-- >>> readRef ref
-- 218
--
-- @since 0.3.0
newRef :: MonadPrim s m => a -> m (Ref a s)
newRef a = a `seq` newLazyRef a
{-# INLINE newRef #-}

-- | Create a new mutable variable. Initial value stays unevaluated.
--
-- ==== __Examples__
--
-- In below example you will see that initial value is never evaluated.
--
-- >>> import Debug.Trace
-- >>> import Data.Prim.Ref
-- >>> ref <- newLazyRef (trace "Initial value is evaluated" (undefined :: Int))
-- >>> writeRef ref 1024
-- >>> modifyFetchNewRef ref succ
-- 1025
--
-- @since 0.3.0
newLazyRef :: MonadPrim s m => a -> m (Ref a s)
newLazyRef a =
  prim $ \s ->
    case newMutVar# a s of
      (# s', ref# #) -> (# s', Ref ref# #)
{-# INLINE newLazyRef #-}

----------------
-- Read/Write --
----------------

-- | Read contents of the mutable variable
--
-- ==== __Examples__
--
-- >>> import Data.Prim.Ref
-- >>> ref <- newRef "Hello World!"
-- >>> readRef ref
-- "Hello World!"
--
-- @since 0.3.0
readRef :: MonadPrim s m => Ref a s -> m a
readRef (Ref ref#) = prim (readMutVar# ref#)
{-# INLINE readRef #-}


-- | Swap a value of a mutable variable with a new one, while retrieving the old one. New
-- value is evaluated prior to it being written to the variable.
--
-- ==== __Examples__
--
-- >>> ref <- newRef (Left "Initial" :: Either String String)
-- >>> swapRef ref (Right "Last")
-- Left "Initial"
-- >>> readRef ref
-- Right "Last"
--
-- @since 0.3.0
swapRef :: MonadPrim s m => Ref a s -> a -> m a
swapRef ref a = readRef ref <* writeRef ref a
{-# INLINE swapRef #-}


-- | Write a value into a mutable variable strictly. If evaluating a value results in
-- exception, original value in the mutable variable will not be affected. Another great
-- benfit of this over `writeLazyRef` is that it helps avoiding memory leaks.
--
-- ==== __Examples__
--
-- >>> ref <- newRef "Original value"
-- >>> import Control.Prim.Exception
-- >>> _ <- try $ writeRef ref undefined :: IO (Either SomeException ())
-- >>> readRef ref
-- "Original value"
-- >>> writeRef ref "New total value"
-- >>> readRef ref
-- "New total value"
--
-- @since 0.3.0
writeRef :: MonadPrim s m => Ref a s -> a -> m ()
writeRef ref !a = writeLazyRef ref a
{-# INLINE writeRef #-}

-- | Same as `writeRef`, but will evaluate the argument to Normal Form prior to writing it
-- to the `Ref`
--
-- @since 0.3.0
writeDeepRef :: (NFData a, MonadPrim s m) => Ref a s -> a -> m ()
writeDeepRef ref a = a `deepseq` writeLazyRef ref a
{-# INLINE writeDeepRef #-}

-- | Write a value into a mutable variable lazily.
--
-- ==== __Examples__
--
-- >>> ref <- newRef "Original value"
-- >>> import Debug.Trace
-- >>> writeLazyRef ref (trace "'New string' is evaluated" "New string")
-- >>> x <- readRef ref
-- >>> writeRef ref (trace "'Totally new string' is evaluated" "Totally new string")
-- 'Totally new string' is evaluated
-- >>> putStrLn x
-- 'New string' is evaluated
-- New string
--
-- @since 0.3.0
writeLazyRef :: MonadPrim s m => Ref a s -> a -> m ()
writeLazyRef (Ref ref#) a = prim_ (writeMutVar# ref# a)
{-# INLINE writeLazyRef #-}


------------
-- Modify --
------------


-- | Apply a pure function to the contents of a mutable variable strictly. Returns the
-- artifact produced by the modifying function. This function is a faster alternative to
-- `atomicModifyRef`, without any atomicity guarantees. For lazy version checkout
-- `modifyLazyRef`
--
-- @since 0.3.0
modifyRef :: MonadPrim s m => Ref a s -> (a -> (a, b)) -> m b
modifyRef ref f = modifyRefM ref (pure . f)
{-# INLINE modifyRef #-}

-- | Apply a pure function to the contents of a mutable variable strictly.
--
-- @since 0.3.0
modifyRef_ :: MonadPrim s m => Ref a s -> (a -> a) -> m ()
modifyRef_ ref f = modifyRefM_ ref (pure . f)
{-# INLINE modifyRef_ #-}

-- | Apply a pure function to the contents of a mutable variable strictly. Returns the new value.
--
-- @since 0.3.0
modifyFetchNewRef :: MonadPrim s m => Ref a s -> (a -> a) -> m a
modifyFetchNewRef ref f = modifyFetchNewRefM ref (pure . f)
{-# INLINE modifyFetchNewRef #-}

-- | Apply a pure function to the contents of a mutable variable strictly. Returns the old value.
--
-- ==== __Examples__
--
-- >>> ref1 <- newRef (10 :: Int)
-- >>> ref2 <- newRef (201 :: Int)
-- >>> modifyRefM_ ref1 (\x -> modifyFetchOldRef ref2 (* x))
-- >>> readRef ref1
-- 201
-- >>> readRef ref2
-- 2010
--
-- @since 0.3.0
modifyFetchOldRef :: MonadPrim s m => Ref a s -> (a -> a) -> m a
modifyFetchOldRef ref f = modifyFetchOldRefM ref (pure . f)
{-# INLINE modifyFetchOldRef #-}


-- | Apply a pure function to the contents of a mutable variable lazily. Returns the
-- artifact produced by the modifying function.
--
-- @since 0.3.0
modifyLazyRef :: MonadPrim s m => Ref a s -> (a -> (a, b)) -> m b
modifyLazyRef ref f = modifyLazyRefM ref (pure . f)
{-# INLINE modifyLazyRef #-}



-- | Modify value of a mutable variable with a monadic action. It is not strict in a
-- return value of type @b@, but the ne value written into the mutable variable is
-- evaluated to WHNF.
--
-- ==== __Examples__
--
modifyRefM :: MonadPrim s m => Ref a s -> (a -> m (a, b)) -> m b
modifyRefM ref f = do
  a <- readRef ref
  (a', b) <- f a
  b <$ writeRef ref a'
{-# INLINE modifyRefM #-}


-- | Modify value of a mutable variable with a monadic action. Result is written strictly.
--
-- ==== __Examples__
--
-- >>> ref <- newRef (Just "Some value")
-- >>> modifyRefM_ ref $ \ mv -> Nothing <$ mapM_ putStrLn mv
-- Some value
-- >>> readRef ref
-- Nothing
--
-- @since 0.3.0
modifyRefM_ :: MonadPrim s m => Ref a s -> (a -> m a) -> m ()
modifyRefM_ ref f = readRef ref >>= f >>= writeRef ref
{-# INLINE modifyRefM_ #-}

-- | Apply a monadic action to the contents of a mutable variable strictly. Returns the old value.
--
-- ==== __Examples__
--
-- >>> refName <- newRef "My name is: "
-- >>> refMyName <- newRef "Alexey"
-- >>> myName <- modifyFetchOldRefM refMyName $ \ name -> "Leo" <$ modifyRef_ refName (++ name)
-- >>> readRef refName >>= putStrLn
-- My name is: Alexey
-- >>> putStrLn myName
-- Alexey
-- >>> readRef refMyName >>= putStrLn
-- Leo
--
-- @since 0.3.0
modifyFetchOldRefM :: MonadPrim s m => Ref a s -> (a -> m a) -> m a
modifyFetchOldRefM ref f = do
  a <- readRef ref
  a <$ (writeRef ref =<< f a)
{-# INLINE modifyFetchOldRefM #-}


-- | Apply a monadic action to the contents of a mutable variable strictly. Returns the new value.
--
-- @since 0.3.0
modifyFetchNewRefM :: MonadPrim s m => Ref a s -> (a -> m a) -> m a
modifyFetchNewRefM ref f = do
  a <- readRef ref
  a' <- f a
  a' <$ writeRef ref a'
{-# INLINE modifyFetchNewRefM #-}

-- | Same as `modifyRefM`, but do not evaluate the new value written into the `Ref`.
--
-- @since 0.3.0
modifyLazyRefM :: MonadPrim s m => Ref a s -> (a -> m (a, b)) -> m b
modifyLazyRefM ref f = do
  a <- readRef ref
  (a', b) <- f a
  b <$ writeLazyRef ref a'
{-# INLINE modifyLazyRefM #-}

------------
-- Atomic --
------------


-- | Evaluate a value and write it atomically into a `Ref`. This is different from
-- `writeRef` because [a memory barrier](https://en.wikipedia.org/wiki/Memory_barrier)
-- will be issued. Use this instead of `writeRef` in order to guarantee the ordering of
-- operations in a concurrent environment.
--
-- @since 0.3.0
atomicWriteRef :: MonadPrim s m => Ref e s -> e -> m ()
atomicWriteRef ref !x = atomicModifyRef_ ref (const x)
{-# INLINE atomicWriteRef #-}

-- | This will behave exactly the same as `readRef` when the `Ref` is accessed within a
-- single thread only. However, despite being slower, it can help with with restricting
-- order of operations in cases when multiple threads perform modifications to the `Ref`
-- because it implies a memory barrier.
--
-- @since 0.3.0
atomicReadRef :: MonadPrim s m => Ref e s -> m e
atomicReadRef ref = atomicModifyFetchOldRef ref id

-- | Same as `atomicWriteRef`, but also returns the old value.
--
-- @since 0.3.0
atomicSwapRef :: MonadPrim s m => Ref e s -> e -> m e
atomicSwapRef ref x = atomicModifyFetchOldRef ref (const x)
{-# INLINE atomicSwapRef #-}

numTriesCAS :: Int
numTriesCAS = 35

-- | Appy a function to the value in mutable `Ref` atomically
--
-- @since 0.3.0
atomicModifyRef :: MonadPrim s m => Ref a s -> (a -> (a, b)) -> m b
atomicModifyRef ref f = readRef ref >>= loop (0 :: Int)
  where
    loop i old
      | i < numTriesCAS = do
        case f old of
          (!new, result) -> do
            (success, current) <- casRef ref old new
            if success
              then pure result
              else loop (i + 1) current
      | otherwise = atomicModifyRef2 ref f
{-# INLINE atomicModifyRef #-}



atomicModifyRef2 :: MonadPrim s m => Ref a s -> (a -> (a, b)) -> m b
atomicModifyRef2 (Ref ref#) f =
#if __GLASGOW_HASKELL__ <= 806
  let g prev =
        case f prev of
          r@(!_new, _result) -> r
   in prim (atomicModifyMutVar# ref# g)
#else
  prim $ \s ->
    case atomicModifyMutVar2# ref# f s of
      (# s', _old, (!_new, result) #) -> (# s', result #)
#endif
{-# INLINE atomicModifyRef2 #-}


atomicModifyRef_ :: MonadPrim s m => Ref a s -> (a -> a) -> m ()
atomicModifyRef_ ref f = readRef ref >>= loop (0 :: Int)
  where
    loop i old
      | i < numTriesCAS = do
        (success, current) <- casRef ref old $! f old
        unless success $ loop (i + 1) current
      | otherwise = atomicModifyRef2_ ref f
{-# INLINE atomicModifyRef_ #-}

atomicModifyRef2_ :: MonadPrim s m => Ref a s -> (a -> a) -> m ()
atomicModifyRef2_ (Ref ref#) f =
  prim_ $ \s ->
    case atomicModifyMutVar_# ref# f s of
      (# s', _prev, !_cur #) -> s'
{-# INLINE atomicModifyRef2_ #-}

atomicModifyFetchOldRef :: MonadPrim s m => Ref a s -> (a -> a) -> m a
atomicModifyFetchOldRef ref f = readRef ref >>= loop (0 :: Int)
  where
    loop i old
      | i < numTriesCAS = do
        (success, current) <- casRef ref old $! f old
        if success
          then pure old
          else loop (i + 1) current
      | otherwise = atomicModifyFetchOldRef2 ref f
{-# INLINE atomicModifyFetchNewRef #-}

atomicModifyFetchOldRef2 :: MonadPrim s m => Ref a s -> (a -> a) -> m a
atomicModifyFetchOldRef2 (Ref ref#) f =
  prim $ \s ->
    case atomicModifyMutVar_# ref# f s of
      (# s', _prev, !_cur #) -> (# s', _prev #)
{-# INLINE atomicModifyFetchOldRef2 #-}


atomicModifyFetchNewRef :: MonadPrim s m => Ref a s -> (a -> a) -> m a
atomicModifyFetchNewRef ref f = readRef ref >>= loop (0 :: Int)
  where
    loop i old
      | i < numTriesCAS = do
        (success, current) <- casRef ref old $! f old
        if success
          then pure current
          else loop (i + 1) current
      | otherwise = atomicModifyFetchNewRef2 ref f
{-# INLINE atomicModifyFetchOldRef #-}

atomicModifyFetchNewRef2 :: MonadPrim s m => Ref a s -> (a -> a) -> m a
atomicModifyFetchNewRef2 (Ref ref#) f =
  prim $ \s ->
    case atomicModifyMutVar_# ref# f s of
      (# s', _prev, !cur #) -> (# s', cur #)
{-# INLINE atomicModifyFetchNewRef2 #-}



atomicModifyFetchBothRef :: MonadPrim s m => Ref a s -> (a -> a) -> m (a, a)
atomicModifyFetchBothRef ref f = readRef ref >>= loop (0 :: Int)
  where
    loop i old
      | i < numTriesCAS = do
        (success, current) <- casRef ref old $! f old
        if success
          then pure (old, current)
          else loop (i + 1) current
      | otherwise = atomicModifyFetchBothRef2 ref f
{-# INLINE atomicModifyFetchBothRef #-}


atomicModifyFetchBothRef2 :: MonadPrim s m => Ref a s -> (a -> a) -> m (a, a)
atomicModifyFetchBothRef2 (Ref ref#) f =
  prim $ \s ->
    case atomicModifyMutVar_# ref# f s of
      (# s', prev, !cur #) -> (# s', (prev, cur) #)
{-# INLINE atomicModifyFetchBothRef2 #-}

-- | Appy a function to the value in mutable `Ref` atomically
--
-- @since 0.3.0
atomicModifyFetchRef :: MonadPrim s m => Ref a s -> (a -> (a, b)) -> m (a, a, b)
atomicModifyFetchRef ref f = readRef ref >>= loop (0 :: Int)
  where
    loop i old
      | i < numTriesCAS = do
        case f old of
          (!new, result) -> do
            (success, current) <- casRef ref old new
            if success
              then pure (old, new, result)
              else loop (i + 1) current
      | otherwise = atomicModifyFetchRef2 ref f
{-# INLINE atomicModifyRef #-}


-- TODO: Test this property
-- @atomicModifyIORef' ref (\x -> (x+1, undefined))@
--
-- will increment the 'IORef' and then throw an exception in the calling
-- thread.

atomicModifyFetchRef2 :: MonadPrim s m => Ref a s -> (a -> (a, b)) -> m (a, a, b)
atomicModifyFetchRef2 ref f =
  atomicModifyFetchLazyRef ref $ \current ->
    case f current of
      r@(!_new, _res) -> r
{-# INLINE atomicModifyFetchRef2 #-}


-- atomicModifyRef2 :: MonadPrim s m => Ref a s -> (a -> (a, b)) -> m (a, a, b)
-- (Ref ref#) f =
--   let g a =
--         case f a of
--           t@(a', _) -> a' `seq` t
--    in prim $ \s ->
--         case atomicModifyMutVar2# ref# g s of
--           (# s', old, (new, b) #) ->
--             case seq# new s' of
--               (# s'', new' #) ->
--                 case seq# b s'' of
--                   (# s''', b' #) -> (# s''', (old, new', b') #)

-- atomicModifyRef2 :: MonadPrim s m => Ref a s -> (a -> (a, b)) -> m b
-- atomicModifyRef2 (Ref ref#) f =
--   prim $ \s ->
--     case atomicModifyMutVar2# ref# f s of
--       (# s', _old, res #) -> (# s', res #)
-- {-# INLINE atomicModifyRef2 #-}






atomicModifyFetchBothLazyRef :: MonadPrim s m => Ref a s -> (a -> a) -> m (a, a)
atomicModifyFetchBothLazyRef (Ref ref#) f =
  prim $ \s ->
    case atomicModifyMutVar_# ref# f s of
      (# s', prev, cur #) -> (# s', (prev, cur) #)
{-# INLINE atomicModifyFetchBothLazyRef #-}


casRef :: MonadPrim s m => Ref a s -> a -> a -> m (Bool, a)
casRef (Ref ref#) expOld new =
  prim $ \s ->
    case casMutVar# ref# expOld new s of
      (# s', failed#, actualOld #) ->
        (# s', (isTrue# (failed# ==# 0#), actualOld) #)
{-# INLINE casRef #-}

atomicModifyFetchLazyRef :: MonadPrim s m => Ref a s -> (a -> (a, b)) -> m (a, a, b)
atomicModifyFetchLazyRef (Ref ref#) f =
  prim $ \s ->
    case atomicModifyMutVar2# ref# f s of
      (# s', old, ~(new, res) #) -> (# s', (old, new, res) #)
{-# INLINE atomicModifyFetchLazyRef #-}


atomicModifyLazyRef :: MonadPrim s m => Ref a s -> (a -> (a, b)) -> m b
atomicModifyLazyRef (Ref ref#) f = prim (atomicModifyMutVar# ref# f)
{-# INLINE atomicModifyLazyRef #-}

atomicModifyLazyRef_ :: MonadPrim s m => Ref a s -> (a -> a) -> m ()
atomicModifyLazyRef_ (Ref ref#) f =
  prim_ $ \s ->
    case atomicModifyMutVar_# ref# f s of
      (# s', _prev, _cur #) -> s'
{-# INLINE atomicModifyLazyRef_ #-}

atomicModifyFetchOldLazyRef :: MonadPrim s m => Ref a s -> (a -> a) -> m a
atomicModifyFetchOldLazyRef (Ref ref#) f =
  prim $ \s ->
    case atomicModifyMutVar_# ref# f s of
      (# s', prev, _cur #) -> (# s', prev #)

atomicModifyFetchNewLazyRef :: MonadPrim s m => Ref a s -> (a -> a) -> m a
atomicModifyFetchNewLazyRef (Ref ref#) f =
  prim $ \s ->
    case atomicModifyMutVar_# ref# f s of
      (# s', _prev, cur #) -> (# s', cur #)

atomicWriteLazyRef :: MonadPrim s m => Ref b s -> b -> m ()
atomicWriteLazyRef ref x = atomicModifyLazyRef_ ref (const x)


-- | Convert `Ref` to `STRef`
--
-- @since 0.3.0
toSTRef :: Ref a s -> ST.STRef s a
toSTRef (Ref ref#) = ST.STRef ref#
{-# INLINE toSTRef #-}

-- | Convert `STRef` to `Ref`
--
-- @since 0.3.0
fromSTRef :: ST.STRef s a -> Ref a s
fromSTRef (ST.STRef ref#) = Ref ref#
{-# INLINE fromSTRef #-}

-- | Convert `Ref` to `IORef`
--
-- @since 0.3.0
toIORef :: Ref a RW -> IO.IORef a
toIORef = coerce . toSTRef
{-# INLINE toIORef #-}

-- | Convert `IORef` to `Ref`
--
-- @since 0.3.0
fromIORef :: IO.IORef a -> Ref a RW
fromIORef = fromSTRef . coerce
{-# INLINE fromIORef #-}




-- | Create a `Weak` pointer associated with the supplied `Ref`.
--
-- Same as `Data.IORef.mkWeakRef` from @base@, but works in any `MonadPrim` with
-- `RealWorld` state token.
--
-- @since 0.3.0
mkWeakRef ::
     forall a b m. MonadUnliftPrim RW m
  => Ref a RW
  -> m b -- ^ An action that will get executed whenever `Ref` gets garbage collected by
         -- the runtime.
  -> m (Weak (Ref a RW))
mkWeakRef ref@(Ref ref#) !finalizer =
  runInPrimBase finalizer $ \f# s ->
    case mkWeak# ref# ref f# s of
      (# s', weak# #) -> (# s', Weak weak# #)
{-# INLINE mkWeakRef #-}



-- atomicModifyIORef' :: IORef a -> (a -> (a,b)) -> IO b
-- -- See Note [atomicModifyIORef' definition]
-- atomicModifyIORef' ref f = do
--   (_old, (_new, !res)) <- atomicModifyIORef2 ref $
--     \old -> case f old of
--        r@(!_new, _res) -> r
--   pure res
-- atomicModifyIORef' :: IORef a -> (a -> (a,b)) -> IO b
-- atomicModifyIORef' (IORef (STRef r#)) f =
--   IO
--     (\s ->
--        case atomicModifyMutVar2# r# f s of
--          (# s', old, res@(!_new, _) #) -> (# s', (old, res) #))

-- atomicModifyIORef2 :: IORef a -> (a -> (a,b)) -> IO (a, (a, b))
-- atomicModifyIORef2 ref f = do
--   r@(_old, (_new, _res)) <- atomicModifyIORef2Lazy ref f
--   return r

-- atomicModifyIORef2Lazy :: IORef a -> (a -> (a,b)) -> IO (a, (a, b))
-- atomicModifyIORef2Lazy (IORef (STRef r#)) f =
--   IO (\s -> case atomicModifyMutVar2# r# f s of
--               (# s', old, res #) -> (# s', (old, res) #))


