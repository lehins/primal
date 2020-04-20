{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Data.Prim.Ref.Lazy
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Ref.Lazy where

import Control.Monad (void)
import Control.Monad.Prim
import GHC.Exts

-- | Same as `Data.Prim.Ref.Ref`, but all operations defined on it are done lazily
data Ref a s = Ref (MutVar# s a)

-- | Only compares pointers, so check whether it is exactly the same `Ref` or not.
instance Eq (Ref a s) where
  Ref ref1# == Ref ref2# = isTrue# (sameMutVar# ref1# ref2#)

newRef :: MonadPrim s m => a -> m (Ref a s)
newRef a =
  prim $ \s# ->
    case newMutVar# a s# of
      (# s'#, ref# #) -> (# s'#, Ref ref# #)


readRef :: MonadPrim s m => Ref a s -> m a
readRef (Ref ref#) = prim (readMutVar# ref#)

writeRef :: MonadPrim s m => Ref a s -> a -> m ()
writeRef (Ref ref#) a = prim_ (writeMutVar# ref# a)


atomicModifyRef2Lazy :: MonadPrim s m => Ref a s -> (a -> (a, b)) -> m (a, a, b)
atomicModifyRef2Lazy (Ref ref#) f =
  prim $ \s# ->
    case atomicModifyMutVar2# ref# f s# of
      (# s'#, old, ~(new, b) #) -> (# s'#, (old, new, b) #)


atomicModifyRefLazy :: MonadPrim s m => Ref a s -> (a -> (a, b)) -> m b
atomicModifyRefLazy (Ref ref#) f = prim (atomicModifyMutVar# ref# f)

atomicFetchModifyRefLazy :: MonadPrim s m => Ref a s -> (a -> a) -> m a
atomicFetchModifyRefLazy (Ref ref#) f =
  prim $ \s# ->
    case atomicModifyMutVar_# ref# f s# of
      (# s'#, prev, _cur #) -> (# s'#, prev #)

atomicModifyFetchRefLazy :: MonadPrim s m => Ref a s -> (a -> a) -> m a
atomicModifyFetchRefLazy (Ref ref#) f =
  prim $ \s# ->
    case atomicModifyMutVar_# ref# f s# of
      (# s'#, _prev, cur #) -> (# s'#, cur #)

atomicWriteRefLazy :: MonadPrim s m => Ref b s -> b -> m b
atomicWriteRefLazy ref x = atomicFetchModifyRefLazy ref (const x)

atomicWriteRefLazy_ :: MonadPrim s m => Ref b s -> b -> m ()
atomicWriteRefLazy_ ref x = void $ atomicWriteRefLazy ref x

casRef :: MonadPrim s m => Ref a s -> a -> a -> m (Bool, a)
casRef (Ref ref#) expOld new = prim $ \ s# ->
  case casMutVar# ref# expOld new s# of
    (# s'#, success#, actualOld #) -> (# s'#, (isTrue# success#, actualOld) #)
