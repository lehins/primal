{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
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
  , newRef
  , newRefLazy
  ) where

import Control.Monad (void)
import Control.Prim.Monad
import GHC.Exts
import GHC.STRef
import GHC.IORef

-- | Mutable variable. This is just like `Data.STRef.STRef`, but with type arguments
-- flipped and is generalized to work in `MonadPrim`
data Ref a s = Ref (MutVar# s a)

-- | Checks whether supplied `Ref`s refer to the exact same `Ref` or not.
instance Eq (Ref a s) where
  Ref ref1# == Ref ref2# = isTrue# (sameMutVar# ref1# ref2#)


newRef :: MonadPrim s m => a -> m (Ref a s)
newRef a = seqPrim a >>= newRefLazy

newRefLazy :: MonadPrim s m => a -> m (Ref a s)
newRefLazy a =
  prim $ \s ->
    case newMutVar# a s of
      (# s', ref# #) -> (# s', Ref ref# #)

readRef :: MonadPrim s m => Ref a s -> m a
readRef (Ref ref#) = prim (readMutVar# ref#)

writeRef :: MonadPrim s m => Ref a s -> a -> m ()
writeRef ref a = seqPrim a >>= writeRefLazy ref

writeRefLazy :: MonadPrim s m => Ref a s -> a -> m ()
writeRefLazy (Ref ref#) a = prim_ (writeMutVar# ref# a)

modifyRefM :: MonadPrim s m => Ref a s -> (a -> m (a, b)) -> m b
modifyRefM ref f = do
  a <- readRef ref
  (a', b) <- f a
  b <$ writeRef ref a'
{-# INLINE modifyRefM #-}

modifyRefM_ :: MonadPrim s m => Ref a s -> (a -> m a) -> m ()
modifyRefM_ ref f = readRef ref >>= f >>= writeRef ref
{-# INLINE modifyRefM_ #-}

-- | Apply a monadic action to the contents of a mutable variable strictly. Returns the old value.
--
-- @since 0.2.0
fetchModifyRefM :: MonadPrim s m => Ref a s -> (a -> m a) -> m a
fetchModifyRefM ref f = do
  a <- readRef ref
  a <$ (writeRef ref =<< f a)
{-# INLINE fetchModifyRefM #-}


-- | Apply a pure function to the contents of a mutable variable strictly. Returns the old value.
--
-- @since 0.2.0
fetchModifyRef :: MonadPrim s m => Ref a s -> (a -> a) -> m a
fetchModifyRef ref f = fetchModifyRefM ref (pure . f)
{-# INLINE fetchModifyRef #-}



atomicModifyRef2_ :: MonadPrim s m => Ref a s -> (a -> a) -> m (a, a)
atomicModifyRef2_ (Ref ref#) f =
  prim $ \s ->
    case atomicModifyMutVar_# ref# f s of
      (# s', prev, cur #) ->
        case seq# cur s' of
          (# s'', cur' #) -> (# s'', (prev, cur') #)

atomicModifyRef_ :: MonadPrim s m => Ref a s -> (a -> a) -> m ()
atomicModifyRef_ (Ref ref#) f =
  prim_ $ \s ->
    case atomicModifyMutVar_# ref# f s of
      (# s', _prev, cur #) ->
        case seq# cur s' of
          (# s'', _cur' #) -> s''

atomicModifyRef :: MonadPrim s m => Ref a s -> (a -> (a, b)) -> m b
atomicModifyRef (Ref ref#) f =
  let g a =
        case f a of
          t@(a', _) -> a' `seq` t
   in prim $ \s ->
        case atomicModifyMutVar# ref# g s of
          (# s', b #) -> seq# b s'

-- TODO: Test this property
-- @atomicModifyIORef' ref (\x -> (x+1, undefined))@
--
-- will increment the 'IORef' and then throw an exception in the calling
-- thread.

atomicModifyRef2 :: MonadPrim s m => Ref a s -> (a -> (a, b)) -> m (a, a, b)
atomicModifyRef2 (Ref ref#) f =
  let g a =
        case f a of
          t@(a', _) -> a' `seq` t
   in prim $ \s ->
        case atomicModifyMutVar2# ref# g s of
          (# s', old, (new, b) #) ->
            case seq# new s' of
              (# s'', new' #) ->
                case seq# b s'' of
                  (# s''', b' #) -> (# s''', (old, new', b') #)

atomicWriteRef :: MonadPrim s m => Ref b s -> b -> m b
atomicWriteRef ref !x = atomicFetchModifyRef ref (const x)

atomicWriteRef_ :: MonadPrim s m => Ref b s -> b -> m ()
atomicWriteRef_ ref x = void $ atomicWriteRef ref x

atomicFetchModifyRef :: MonadPrim s m => Ref a s -> (a -> a) -> m a
atomicFetchModifyRef (Ref ref#) f =
  prim $ \s ->
    case atomicModifyMutVar_# ref# f s of
      (# s', prev, _cur #) -> seq# prev s'

atomicModifyFetchRef :: MonadPrim s m => Ref a s -> (a -> a) -> m a
atomicModifyFetchRef (Ref ref#) f =
  prim $ \s ->
    case atomicModifyMutVar_# ref# f s of
      (# s', _prev, cur #) -> seq# cur s'

casRef :: MonadPrim s m => Ref a s -> a -> a -> m (Bool, a)
casRef (Ref ref#) expOld new = prim $ \ s ->
  case casMutVar# ref# expOld new s of
    (# s', success#, actualOld #) -> (# s', (isTrue# success#, actualOld) #)



atomicModifyRef2Lazy :: MonadPrim s m => Ref a s -> (a -> (a, b)) -> m (a, a, b)
atomicModifyRef2Lazy (Ref ref#) f =
  prim $ \s ->
    case atomicModifyMutVar2# ref# f s of
      (# s', old, ~(new, b) #) -> (# s', (old, new, b) #)


atomicModifyRefLazy :: MonadPrim s m => Ref a s -> (a -> (a, b)) -> m b
atomicModifyRefLazy (Ref ref#) f = prim (atomicModifyMutVar# ref# f)

atomicFetchModifyRefLazy :: MonadPrim s m => Ref a s -> (a -> a) -> m a
atomicFetchModifyRefLazy (Ref ref#) f =
  prim $ \s ->
    case atomicModifyMutVar_# ref# f s of
      (# s', prev, _cur #) -> (# s', prev #)

atomicModifyFetchRefLazy :: MonadPrim s m => Ref a s -> (a -> a) -> m a
atomicModifyFetchRefLazy (Ref ref#) f =
  prim $ \s ->
    case atomicModifyMutVar_# ref# f s of
      (# s', _prev, cur #) -> (# s', cur #)

atomicWriteRefLazy :: MonadPrim s m => Ref b s -> b -> m b
atomicWriteRefLazy ref x = atomicFetchModifyRefLazy ref (const x)

atomicWriteRefLazy_ :: MonadPrim s m => Ref b s -> b -> m ()
atomicWriteRefLazy_ ref x = void $ atomicWriteRefLazy ref x



toSTRef :: Ref a s -> STRef s a
toSTRef (Ref ref#) = STRef ref#

fromSTRef :: STRef s a -> Ref a s
fromSTRef (STRef ref#) = Ref ref#

toIORef :: Ref a RW -> IORef a
toIORef (Ref ref#) = IORef (STRef ref#)

fromIORef :: IORef a -> Ref a RW
fromIORef (IORef (STRef ref#)) = Ref ref#
