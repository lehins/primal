{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Data.Prim.MRef.Ref
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.MRef.Ref
  ( Ref(..)
  -- * Create
  , newRef
  , newRefLazy
  -- * Read/write
  , readRef
  , swapRef
  , writeRef
  , writeRefLazy
  -- * Modify
  -- ** Pure
  , modifyRef
  , modifyRef_
  , modifyFetchNewRef
  , modifyFetchOldRef
  -- *** Lazy
  , modifyRefLazy
  -- ** Monadic
  , modifyRefM
  , modifyRefM_
  , modifyFetchNewRefM
  , modifyFetchOldRefM
  -- *** Lazy
  , modifyRefLazyM
  -- * Atomic
  -- ** Write
  , atomicSwapRef
  , atomicReadRef
  , atomicWriteRef
  -- ** Modify
  , atomicModifyRef
  , atomicModifyRef_
  , atomicModifyRef2
  , atomicModifyRef2_
  , atomicModifyFetchNewRef
  , atomicModifyFetchOldRef
  , casRef
  -- *** Lazy
  , atomicWriteRefLazy
  , atomicWriteRefLazy_
  , atomicModifyRefLazy
  , atomicModifyRef2Lazy
  , atomicModifyFetchNewRefLazy
  , atomicModifyFetchOldRefLazy
  -- * Conversion
  -- ** STRef
  , toSTRef
  , fromSTRef
  -- ** IORef
  , toIORef
  , fromIORef
  ) where

import Control.DeepSeq
import Control.Prim.Monad
import Data.Prim.MRef.Atomic
import Data.Prim.MRef.Internal
import Foreign.Prim
import GHC.STRef
import GHC.IORef

-- | Mutable variable that can store any boxed value. Because it stores just a reference
-- to the value it is named "Ref". This is just like `Data.STRef.STRef`,
-- but with type arguments flipped and is generalized to work in `MonadPrim`
--
-- @since 0.1.0
data Ref a s = Ref (MutVar# s a)

-- | Checks whether supplied `Ref`s refer to the exact same `Ref` or not.
instance Eq (Ref a s) where
  Ref ref1# == Ref ref2# = isTrue# (sameMutVar# ref1# ref2#)

instance NFData (Ref a s) where
  rnf (Ref _ref#) = ()

instance MRef (Ref a) where
  type Elt (Ref a) = a
  newMRef = newRef
  {-# INLINE newMRef #-}
  newRawMRef = newRef (uninitialized "Data.Prim.MRef.Ref" "newRawMRef")
  {-# INLINE newRawMRef #-}
  writeMRef = writeRef
  {-# INLINE writeMRef #-}
  readMRef = readRef
  {-# INLINE readMRef #-}

instance AtomicMRef (Ref a) where
  atomicReadMRef = atomicReadRef
  {-# INLINE atomicReadMRef #-}
  atomicWriteMRef = atomicWriteRef
  {-# INLINE atomicWriteMRef #-}
  casMRef = casRef
  {-# INLINE casMRef #-}

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
-- @since 0.1.0
newRef :: MonadPrim s m => a -> m (Ref a s)
newRef a = a `seq` newRefLazy a
{-# INLINE newRef #-}

-- | Create a new mutable variable. Initial value stays unevaluated.
--
-- ==== __Examples__
--
-- In below example you will see that initial value is never evaluated.
--
-- >>> import Debug.Trace
-- >>> import Data.Prim.Ref
-- >>> ref <- newRefLazy (trace "Initial value is evaluated" (undefined :: Int))
-- >>> writeRef ref 1024
-- >>> modifyFetchNewRef ref succ
-- 1025
--
-- @since 0.1.0
newRefLazy :: MonadPrim s m => a -> m (Ref a s)
newRefLazy a =
  prim $ \s ->
    case newMutVar# a s of
      (# s', ref# #) -> (# s', Ref ref# #)
{-# INLINE newRefLazy #-}

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
-- @since 0.1.0
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
-- @since 0.1.0
swapRef :: MonadPrim s m => Ref a s -> a -> m a
swapRef ref a = do
  a' <- readRef ref
  a' <$ writeRef ref a
{-# INLINE swapRef #-}


-- | Write a value into a mutable variable strictly. If evaluating a value results in
-- exception, original value in the mutable variable will not be affected. Another great
-- benfit of this over `writeRefLazy` is that it helps avoiding memory leaks.
--
-- ==== __Examples__
--
-- >>> ref <- newRef "Original value"
-- >>> import Control.Exception
-- >>> _ <- try $ writeRef ref undefined :: IO (Either SomeException ())
-- >>> readRef ref
-- "Original value"
-- >>> writeRef ref "New total value"
-- >>> readRef ref
-- "New total value"
--
-- @since 0.1.0
writeRef :: MonadPrim s m => Ref a s -> a -> m ()
writeRef ref a =
  a `seq` writeRefLazy ref a
{-# INLINE writeRef #-}

-- | Write a value into a mutable variable lazily.
--
-- ==== __Examples__
--
-- >>> ref <- newRef "Original value"
-- >>> import Debug.Trace
-- >>> writeRefLazy ref (trace "'New string' is evaluated" "New string")
-- >>> x <- readRef ref
-- >>> writeRef ref (trace "'Totally new string' is evaluated" "Totally new string")
-- 'Totally new string' is evaluated
-- >>> putStrLn x
-- 'New string' is evaluated
-- New string
--
-- @since 0.1.0
writeRefLazy :: MonadPrim s m => Ref a s -> a -> m ()
writeRefLazy (Ref ref#) a = prim_ (writeMutVar# ref# a)
{-# INLINE writeRefLazy #-}


------------
-- Modify --
------------


-- | Apply a pure function to the contents of a mutable variable strictly. Returns the
-- artifact produced by the modifying function. This function is a faster alternative to
-- `atomicModifyRef`, without any atomicity guarantees. For lazy version checkout
-- `modifyRefLazy`
--
-- @since 0.1.0
modifyRef :: MonadPrim s m => Ref a s -> (a -> (a, b)) -> m b
modifyRef ref f = modifyRefM ref (pure . f)
{-# INLINE modifyRef #-}

-- | Apply a pure function to the contents of a mutable variable strictly.
--
-- @since 0.1.0
modifyRef_ :: MonadPrim s m => Ref a s -> (a -> a) -> m ()
modifyRef_ ref f = modifyRefM_ ref (pure . f)
{-# INLINE modifyRef_ #-}

-- | Apply a pure function to the contents of a mutable variable strictly. Returns the new value.
--
-- @since 0.1.0
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
-- @since 0.1.0
modifyFetchOldRef :: MonadPrim s m => Ref a s -> (a -> a) -> m a
modifyFetchOldRef ref f = modifyFetchOldRefM ref (pure . f)
{-# INLINE modifyFetchOldRef #-}


-- | Apply a pure function to the contents of a mutable variable lazily. Returns the
-- artifact produced by the modifying function.
--
-- @since 0.1.0
modifyRefLazy :: MonadPrim s m => Ref a s -> (a -> (a, b)) -> m b
modifyRefLazy ref f = modifyRefLazyM ref (pure . f)
{-# INLINE modifyRefLazy #-}



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
-- @since 0.1.0
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
-- @since 0.1.0
modifyFetchOldRefM :: MonadPrim s m => Ref a s -> (a -> m a) -> m a
modifyFetchOldRefM ref f = do
  a <- readRef ref
  a <$ (writeRef ref =<< f a)
{-# INLINE modifyFetchOldRefM #-}


-- | Apply a monadic action to the contents of a mutable variable strictly. Returns the new value.
--
-- @since 0.1.0
modifyFetchNewRefM :: MonadPrim s m => Ref a s -> (a -> m a) -> m a
modifyFetchNewRefM ref f = do
  a <- readRef ref
  a' <- f a
  a' <$ writeRef ref a'
{-# INLINE modifyFetchNewRefM #-}


modifyRefLazyM :: MonadPrim s m => Ref a s -> (a -> m (a, b)) -> m b
modifyRefLazyM ref f = do
  a <- readRef ref
  (a', b) <- f a
  b <$ writeRefLazy ref a'
{-# INLINE modifyRefLazyM #-}

------------
-- Atomic --
------------
data Unit a = Unit a

-- | Evaluate a value and write it atomically into a `Ref`. This is different from
-- `writeRef` because [a memory barrier](https://en.wikipedia.org/wiki/Memory_barrier)
-- will be issued.
--
-- @since 0.1.0
atomicWriteRef :: MonadPrim s m => Ref e s -> e -> m ()
atomicWriteRef (Ref ref#) x =
  x `seq`
  prim $ \s ->
    case atomicModifyMutVar2# ref# (\_ -> Unit x) s of
      (# s', _prev, Unit _cur #) -> (# s', () #)
  -- prim $ \s ->
  --   case atomicModifyMutVar_# ref# (\_ -> x) s of
  --     (# s', _prev, _cur #) -> (# s', () #)
  -- x `seq` prim_ $ \s ->
  --   -- case seq# x s of
  --   --   (# s', x' #) ->
  --       case atomicModifyMutVar_# ref# (const x) s of
  --         (# s'', _prev, _cur #) -> s''
{-# INLINE atomicWriteRef #-}

atomicReadRef :: MonadPrim s m => Ref e s -> m e
atomicReadRef ref = fst <$> atomicModifyRef2_ ref id

-- | Same as `atomicWriteRef`, but also returns the old value.
--
-- @since 0.1.0
atomicSwapRef :: MonadPrim s m => Ref e s -> e -> m e
atomicSwapRef ref x = atomicModifyFetchOldRef ref (const x)
{-# INLINE atomicSwapRef #-}


atomicModifyRef :: MonadPrim s m => Ref a s -> (a -> (a, b)) -> m b
atomicModifyRef (Ref ref#) f =
  let g a =
        case f a of
          t@(a', _) -> a' `seq` t
   in prim $ \s ->
        case atomicModifyMutVar# ref# g s of
          (# s', b #) -> seq# b s'
{-# INLINE atomicModifyRef #-}

atomicModifyRef_ :: MonadPrim s m => Ref a s -> (a -> a) -> m ()
atomicModifyRef_ (Ref ref#) f =
  prim_ $ \s ->
    case atomicModifyMutVar_# ref# f s of
      (# s', _prev, cur #) ->
        case seq# cur s' of
          (# s'', _cur' #) -> s''
{-# INLINE atomicModifyRef_ #-}

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
{-# INLINE atomicModifyRef2 #-}

atomicModifyRef2_ :: MonadPrim s m => Ref a s -> (a -> a) -> m (a, a)
atomicModifyRef2_ (Ref ref#) f =
  prim $ \s ->
    case atomicModifyMutVar_# ref# f s of
      (# s', prev, cur #) ->
        case seq# cur s' of
          (# s'', cur' #) -> (# s'', (prev, cur') #)
{-# INLINE atomicModifyRef2_ #-}




atomicModifyFetchOldRef :: MonadPrim s m => Ref a s -> (a -> a) -> m a
atomicModifyFetchOldRef (Ref ref#) f =
  prim $ \s ->
    case atomicModifyMutVar_# ref# f s of
      (# s', prev, _cur #) -> (# s', prev #)

atomicModifyFetchNewRef :: MonadPrim s m => Ref a s -> (a -> a) -> m a
atomicModifyFetchNewRef (Ref ref#) f =
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

atomicModifyFetchOldRefLazy :: MonadPrim s m => Ref a s -> (a -> a) -> m a
atomicModifyFetchOldRefLazy (Ref ref#) f =
  prim $ \s ->
    case atomicModifyMutVar_# ref# f s of
      (# s', prev, _cur #) -> (# s', prev #)

atomicModifyFetchNewRefLazy :: MonadPrim s m => Ref a s -> (a -> a) -> m a
atomicModifyFetchNewRefLazy (Ref ref#) f =
  prim $ \s ->
    case atomicModifyMutVar_# ref# f s of
      (# s', _prev, cur #) -> (# s', cur #)

atomicWriteRefLazy :: MonadPrim s m => Ref b s -> b -> m b
atomicWriteRefLazy ref x = atomicModifyFetchOldRefLazy ref (const x)

atomicWriteRefLazy_ :: MonadPrim s m => Ref b s -> b -> m ()
atomicWriteRefLazy_ ref x = void $ atomicWriteRefLazy ref x


-- | Convert `Ref` to `STRef`
toSTRef :: Ref a s -> STRef s a
toSTRef (Ref ref#) = STRef ref#
{-# INLINE toSTRef #-}

-- | Convert `STRef` to `Ref`
fromSTRef :: STRef s a -> Ref a s
fromSTRef (STRef ref#) = Ref ref#
{-# INLINE fromSTRef #-}

-- | Convert `Ref` to `IORef`
toIORef :: Ref a RW -> IORef a
toIORef = coerce . toSTRef

-- | Convert `IORef` to `Ref`
fromIORef :: IORef a -> Ref a RW
fromIORef = fromSTRef . coerce
