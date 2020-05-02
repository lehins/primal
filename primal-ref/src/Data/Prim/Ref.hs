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
module Data.Prim.Ref where

import Control.Monad (void)
import Control.Prim.Monad
import GHC.Exts
import qualified Data.Prim.Ref.Lazy as Lazy

-- | This is just like `Data.STRef.STRef`, but with type arguments flipped and is
-- generalized to work in `MonadPrim`
data Ref a s = Ref (MutVar# s a)

-- | Only compares pointers, so check whether it is exactly the same `Ref` or not.
instance Eq (Ref a s) where
  Ref ref1# == Ref ref2# = isTrue# (sameMutVar# ref1# ref2#)

class MutInit f where

  initPrim :: MonadPrim s m => a -> m (f a s)

class MutFunctor f where
  mapPrim :: MonadPrim s m => (a -> m b) -> f a s -> m (f b s)

  mutPrim :: MonadPrim s m => (a -> m a) -> f a s -> m ()

instance MutFunctor Ref where
  mapPrim f ref = readRef ref >>= f >>= newRef

  mutPrim f ref = readRef ref >>= f >>= writeRef ref


toLazyRef :: Ref a s -> Lazy.Ref a s
toLazyRef (Ref ref#) = Lazy.Ref ref#

fromLazyRef :: Lazy.Ref a s -> Ref a s
fromLazyRef (Lazy.Ref ref#) = Ref ref#

newRef :: MonadPrim s m => a -> m (Ref a s)
newRef !a =
  prim $ \s ->
    case newMutVar# a s of
      (# s', ref# #) -> (# s', Ref ref# #)


readRef :: MonadPrim s m => Ref a s -> m a
readRef (Ref ref#) = prim (readMutVar# ref#)

writeRef :: MonadPrim s m => Ref a s -> a -> m ()
writeRef (Ref ref#) !a = prim_ (writeMutVar# ref# a)

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
