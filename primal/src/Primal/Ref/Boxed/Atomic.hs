{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Primal.Ref.Boxed.Atomic
-- Copyright   : (c) Alexey Kuleshevich 2020-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Ref.Boxed.Atomic
  ( BRef
  -- * Create
  , newBRef
  , newDeepBRef
  , newLazyBRef
  -- * Atomic
  , atomicReadBRef
  , atomicSwapBRef
  , atomicWriteBRef
  , atomicModifyBRef
  , atomicModifyBRef_
  , atomicModifyFetchBRef
  , atomicModifyFetchNewBRef
  , atomicModifyFetchOldBRef
  , atomicModifyFetchBothBRef
  -- ** Original
  , casBRef
  , atomicModifyBRef2
  , atomicModifyBRef2_
  , atomicModifyFetchNewBRef2
  , atomicModifyFetchOldBRef2
  , atomicModifyFetchBothBRef2
  , atomicModifyFetchBRef2
  -- * Lazy
  -- It is recommended to refrain from usage of lazy functions because they are a memory
  -- leak waiting to happen
  , atomicWriteLazyBRef
  , atomicModifyLazyBRef
  , atomicModifyFetchNewLazyBRef
  , atomicModifyFetchOldLazyBRef
  , atomicModifyFetchBothLazyBRef
  , atomicModifyFetchLazyBRef
  ) where

import Primal.Ref.Boxed
import Primal.Monad
import Primal.Foreign

------------
-- Atomic --
------------


-- | Evaluate a value and write it atomically into a `BRef`. This is different from
-- `writeBRef` because [a memory barrier](https://en.wikipedia.org/wiki/Memory_barrier)
-- will be issued. Use this instead of `writeBRef` in order to guarantee the ordering of
-- operations in a concurrent environment.
--
-- @since 1.0.0
atomicWriteBRef :: Primal s m => BRef e s -> e -> m ()
atomicWriteBRef ref !x = atomicModifyBRef_ ref (const x)
{-# INLINE atomicWriteBRef #-}

-- | This will behave exactly the same as `readBRef` when the `BRef` is accessed within a
-- single thread only. However, despite being slower, it can help with with restricting
-- order of operations in cases when multiple threads perform modifications to the `BRef`
-- because it implies a memory barrier.
--
-- @since 1.0.0
atomicReadBRef :: Primal s m => BRef e s -> m e
atomicReadBRef ref = atomicModifyFetchOldBRef ref id

-- | Same as `atomicWriteBRef`, but also returns the old value.
--
-- @since 1.0.0
atomicSwapBRef :: Primal s m => BRef e s -> e -> m e
atomicSwapBRef ref x = atomicModifyFetchOldBRef ref (const x)
{-# INLINE atomicSwapBRef #-}

numTriesCAS :: Int
numTriesCAS = 35

-- | Apply a function to the value stored in a mutable `BRef` atomically. Function is
-- applied strictly with respect to the newly returned value, which matches the semantics
-- of `atomicModifyIOBRef'`, however the difference is that the artifact returned by the
-- action is not evaluated.
--
-- ====__Example__
--
-- >>> 
--
-- @since 1.0.0
atomicModifyBRef :: Primal s m => BRef a s -> (a -> (a, b)) -> m b
atomicModifyBRef ref f = readBRef ref >>= loop (0 :: Int)
  where
    loop i old
      | i < numTriesCAS = do
        case f old of
          (!new, result) -> do
            (success, current) <- casBRef ref old new
            if success
              then pure result
              else loop (i + 1) current
      | otherwise = atomicModifyBRef2 ref f
{-# INLINE atomicModifyBRef #-}



atomicModifyBRef2 :: Primal s m => BRef a s -> (a -> (a, b)) -> m b
atomicModifyBRef2 (BRef ref#) f =
#if __GLASGOW_HASKELL__ <= 806
  let g prev =
        case f prev of
          r@(!_new, _result) -> r
   in primal (atomicModifyMutVar# ref# g)
#else
  primal $ \s ->
    case atomicModifyMutVar2# ref# f s of
      (# s', _old, (!_new, result) #) -> (# s', result #)
#endif
{-# INLINE atomicModifyBRef2 #-}


atomicModifyBRef_ :: Primal s m => BRef a s -> (a -> a) -> m ()
atomicModifyBRef_ ref f = readBRef ref >>= loop (0 :: Int)
  where
    loop i old
      | i < numTriesCAS = do
        (success, current) <- casBRef ref old $! f old
        unless success $ loop (i + 1) current
      | otherwise = atomicModifyBRef2_ ref f
{-# INLINE atomicModifyBRef_ #-}

atomicModifyBRef2_ :: Primal s m => BRef a s -> (a -> a) -> m ()
atomicModifyBRef2_ (BRef ref#) f =
  primal_ $ \s ->
    case atomicModifyMutVar_# ref# f s of
      (# s', _prev, !_cur #) -> s'
{-# INLINE atomicModifyBRef2_ #-}

atomicModifyFetchOldBRef :: Primal s m => BRef a s -> (a -> a) -> m a
atomicModifyFetchOldBRef ref f = readBRef ref >>= loop (0 :: Int)
  where
    loop i old
      | i < numTriesCAS = do
        (success, current) <- casBRef ref old $! f old
        if success
          then pure old
          else loop (i + 1) current
      | otherwise = atomicModifyFetchOldBRef2 ref f
{-# INLINE atomicModifyFetchNewBRef #-}

atomicModifyFetchOldBRef2 :: Primal s m => BRef a s -> (a -> a) -> m a
atomicModifyFetchOldBRef2 (BRef ref#) f =
  primal $ \s ->
    case atomicModifyMutVar_# ref# f s of
      (# s', _prev, !_cur #) -> (# s', _prev #)
{-# INLINE atomicModifyFetchOldBRef2 #-}


atomicModifyFetchNewBRef :: Primal s m => BRef a s -> (a -> a) -> m a
atomicModifyFetchNewBRef ref f = readBRef ref >>= loop (0 :: Int)
  where
    loop i old
      | i < numTriesCAS = do
        (success, current) <- casBRef ref old $! f old
        if success
          then pure current
          else loop (i + 1) current
      | otherwise = atomicModifyFetchNewBRef2 ref f
{-# INLINE atomicModifyFetchOldBRef #-}

atomicModifyFetchNewBRef2 :: Primal s m => BRef a s -> (a -> a) -> m a
atomicModifyFetchNewBRef2 (BRef ref#) f =
  primal $ \s ->
    case atomicModifyMutVar_# ref# f s of
      (# s', _prev, !cur #) -> (# s', cur #)
{-# INLINE atomicModifyFetchNewBRef2 #-}



atomicModifyFetchBothBRef :: Primal s m => BRef a s -> (a -> a) -> m (a, a)
atomicModifyFetchBothBRef ref f = readBRef ref >>= loop (0 :: Int)
  where
    loop i old
      | i < numTriesCAS = do
        (success, current) <- casBRef ref old $! f old
        if success
          then pure (old, current)
          else loop (i + 1) current
      | otherwise = atomicModifyFetchBothBRef2 ref f
{-# INLINE atomicModifyFetchBothBRef #-}


atomicModifyFetchBothBRef2 :: Primal s m => BRef a s -> (a -> a) -> m (a, a)
atomicModifyFetchBothBRef2 (BRef ref#) f =
  primal $ \s ->
    case atomicModifyMutVar_# ref# f s of
      (# s', prev, !cur #) -> (# s', (prev, cur) #)
{-# INLINE atomicModifyFetchBothBRef2 #-}

-- | Apply a function to the value in mutable `BRef` atomically
--
-- @since 1.0.0
atomicModifyFetchBRef :: Primal s m => BRef a s -> (a -> (a, b)) -> m (a, a, b)
atomicModifyFetchBRef ref f = readBRef ref >>= loop (0 :: Int)
  where
    loop i old
      | i < numTriesCAS = do
        case f old of
          (!new, result) -> do
            (success, current) <- casBRef ref old new
            if success
              then pure (old, new, result)
              else loop (i + 1) current
      | otherwise = atomicModifyFetchBRef2 ref f
{-# INLINE atomicModifyFetchBRef #-}


-- TODO: Test this property
-- @atomicModifyIOBRef' ref (\x -> (x+1, undefined))@
--
-- will increment the 'IOBRef' and then throw an exception in the calling
-- thread.

atomicModifyFetchBRef2 :: Primal s m => BRef a s -> (a -> (a, b)) -> m (a, a, b)
atomicModifyFetchBRef2 ref f =
  atomicModifyFetchLazyBRef ref $ \current ->
    case f current of
      r@(!_new, _res) -> r
{-# INLINE atomicModifyFetchBRef2 #-}


-- atomicModifyBRef2 :: Primal s m => BRef a s -> (a -> (a, b)) -> m (a, a, b)
-- (BRef ref#) f =
--   let g a =
--         case f a of
--           t@(a', _) -> a' `seq` t
--    in primal $ \s ->
--         case atomicModifyMutVar2# ref# g s of
--           (# s', old, (new, b) #) ->
--             case seq# new s' of
--               (# s'', new' #) ->
--                 case seq# b s'' of
--                   (# s''', b' #) -> (# s''', (old, new', b') #)

-- atomicModifyBRef2 :: Primal s m => BRef a s -> (a -> (a, b)) -> m b
-- atomicModifyBRef2 (BRef ref#) f =
--   primal $ \s ->
--     case atomicModifyMutVar2# ref# f s of
--       (# s', _old, res #) -> (# s', res #)
-- {-# INLINE atomicModifyBRef2 #-}






atomicModifyFetchBothLazyBRef :: Primal s m => BRef a s -> (a -> a) -> m (a, a)
atomicModifyFetchBothLazyBRef (BRef ref#) f =
  primal $ \s ->
    case atomicModifyMutVar_# ref# f s of
      (# s', prev, cur #) -> (# s', (prev, cur) #)
{-# INLINE atomicModifyFetchBothLazyBRef #-}


casBRef :: Primal s m => BRef a s -> a -> a -> m (Bool, a)
casBRef (BRef ref#) expOld new =
  primal $ \s ->
    case casMutVar# ref# expOld new s of
      (# s', failed#, actualOld #) ->
        (# s', (isTrue# (failed# ==# 0#), actualOld) #)
{-# INLINE casBRef #-}

atomicModifyFetchLazyBRef :: Primal s m => BRef a s -> (a -> (a, b)) -> m (a, a, b)
atomicModifyFetchLazyBRef (BRef ref#) f =
  primal $ \s ->
    case atomicModifyMutVar2# ref# f s of
      (# s', old, ~(new, res) #) -> (# s', (old, new, res) #)
{-# INLINE atomicModifyFetchLazyBRef #-}


atomicModifyLazyBRef :: Primal s m => BRef a s -> (a -> (a, b)) -> m b
atomicModifyLazyBRef (BRef ref#) f = primal (atomicModifyMutVar# ref# f)
{-# INLINE atomicModifyLazyBRef #-}

atomicModifyLazyBRef_ :: Primal s m => BRef a s -> (a -> a) -> m ()
atomicModifyLazyBRef_ (BRef ref#) f =
  primal_ $ \s ->
    case atomicModifyMutVar_# ref# f s of
      (# s', _prev, _cur #) -> s'
{-# INLINE atomicModifyLazyBRef_ #-}

atomicModifyFetchOldLazyBRef :: Primal s m => BRef a s -> (a -> a) -> m a
atomicModifyFetchOldLazyBRef (BRef ref#) f =
  primal $ \s ->
    case atomicModifyMutVar_# ref# f s of
      (# s', prev, _cur #) -> (# s', prev #)

atomicModifyFetchNewLazyBRef :: Primal s m => BRef a s -> (a -> a) -> m a
atomicModifyFetchNewLazyBRef (BRef ref#) f =
  primal $ \s ->
    case atomicModifyMutVar_# ref# f s of
      (# s', _prev, cur #) -> (# s', cur #)

atomicWriteLazyBRef :: Primal s m => BRef b s -> b -> m ()
atomicWriteLazyBRef ref x = atomicModifyLazyBRef_ ref (const x)



-- atomicModifyIOBRef' :: IOBRef a -> (a -> (a,b)) -> IO b
-- -- See Note [atomicModifyIOBRef' definition]
-- atomicModifyIOBRef' ref f = do
--   (_old, (_new, !res)) <- atomicModifyIOBRef2 ref $
--     \old -> case f old of
--        r@(!_new, _res) -> r
--   pure res
-- atomicModifyIOBRef' :: IOBRef a -> (a -> (a,b)) -> IO b
-- atomicModifyIOBRef' (IOBRef (STBRef r#)) f =
--   IO
--     (\s ->
--        case atomicModifyMutVar2# r# f s of
--          (# s', old, res@(!_new, _) #) -> (# s', (old, res) #))

-- atomicModifyIOBRef2 :: IOBRef a -> (a -> (a,b)) -> IO (a, (a, b))
-- atomicModifyIOBRef2 ref f = do
--   r@(_old, (_new, _res)) <- atomicModifyIOBRef2Lazy ref f
--   return r

-- atomicModifyIOBRef2Lazy :: IOBRef a -> (a -> (a,b)) -> IO (a, (a, b))
-- atomicModifyIOBRef2Lazy (IOBRef (STBRef r#)) f =
--   IO (\s -> case atomicModifyMutVar2# r# f s of
--               (# s', old, res #) -> (# s', (old, res) #))


