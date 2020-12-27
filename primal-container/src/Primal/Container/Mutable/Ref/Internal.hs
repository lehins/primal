{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-- |
-- Module      : Primal.Container.Mutable.Ref.Internal
-- Copyright   : (c) Alexey Kuleshevich 2020-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Container.Mutable.Ref.Internal
  ( MRef(..)
  , modifyMRef
  , modifyMRef_
  , modifyFetchOldMRef
  , modifyFetchNewMRef
  , modifyMRefM
  , modifyMRefM_
  , modifyFetchOldMRefM
  , modifyFetchNewMRefM
  ) where

import Primal.Concurrent.MVar
import Primal.Container.Internal
import Primal.Data.Array
import Primal.Data.Ref
import Primal.Memory
import Primal.Memory.Addr
import Primal.Memory.PArray

-- TODO: make MSingleton superclass
class Elt c e => MRef c e where

  newMRef :: MonadPrim s m => e -> m (c e s)
  newMRef a = newRawMRef >>= \mut -> mut <$ writeMRef mut a
  {-# INLINE newMRef #-}

  newRawMRef :: MonadPrim s m => m (c e s)

  readMRef :: MonadPrim s m => c e s -> m e

  writeMRef :: MonadPrim s m => c e s -> e -> m ()

-- | Read/write aren't atomic - /not/ thread safe.
instance MRef MVar a where
  newMRef = newMVar
  {-# INLINE newMRef #-}
  newRawMRef = newEmptyMVar
  {-# INLINE newRawMRef #-}
  writeMRef = writeMVar
  {-# INLINE writeMRef #-}
  readMRef = readMVar
  {-# INLINE readMRef #-}


instance Prim e => MRef MAddr e where
  newRawMRef = allocMAddr 1
  {-# INLINE newRawMRef #-}
  writeMRef = writeMAddr
  {-# INLINE writeMRef #-}
  readMRef = readMAddr
  {-# INLINE readMRef #-}


instance (Typeable p, Prim e) => MRef (PMArray p) e where
  newRawMRef = allocPMArray 1
  {-# INLINE newRawMRef #-}
  writeMRef mba = writePMArray mba 0
  {-# INLINE writeMRef #-}
  readMRef mba = readPMArray mba 0
  {-# INLINE readMRef #-}



instance MRef Ref a where
  newMRef = newRef
  {-# INLINE newMRef #-}
  newRawMRef = newRef (uninitialized "Primal.Container.Mutable.Ref.Internal" "newRawMRef")
  {-# INLINE newRawMRef #-}
  writeMRef = writeRef
  {-# INLINE writeMRef #-}
  readMRef = readRef
  {-# INLINE readMRef #-}


modifyMRef ::
     (MRef c e, MonadPrim s m) => c e s -> (e -> (e, a)) -> m a
modifyMRef ref f = modifyMRefM ref (pure . f)
{-# INLINE modifyMRef #-}


modifyMRef_ :: (MRef c e, MonadPrim s m) => c e s -> (e -> e) -> m ()
modifyMRef_ ref f = modifyMRefM_ ref (pure . f)
{-# INLINE modifyMRef_ #-}



modifyFetchOldMRef ::
     (MRef c e, MonadPrim s m)
  => c e s
  -> (e -> e)
  -> m (e)
modifyFetchOldMRef ref f = modifyFetchOldMRefM ref (pure . f)
{-# INLINE modifyFetchOldMRef #-}


-- | Apply a monadic action to the contents of a mutable variable strictly. Returns the new value.
--
-- @since 0.1.0
modifyFetchNewMRef ::
     (MRef c e, MonadPrim s m)
  => c e s
  -> (e -> e)
  -> m (e)
modifyFetchNewMRef ref f = modifyFetchNewMRefM ref (pure . f)
{-# INLINE modifyFetchNewMRef #-}




-- | Modify value of a mutable variable with a monadic action. Result is written strictly.
--
-- ==== __Examples__
--
-- >>> ref <- newMRef (Just "Some value")
-- >>> modifyMRefM_ ref $ \ mv -> Nothing <$ mapM_ putStrLn mv
-- Some value
-- >>> readMRef ref
-- Nothing
--
-- @since 0.1.0
modifyMRefM_ :: (MRef c e, MonadPrim s m) => c e s -> (e -> m (e)) -> m ()
modifyMRefM_ ref f = readMRef ref >>= f >>= writeMRef ref
{-# INLINE modifyMRefM_ #-}




-- | Modify value of a mutable variable with a monadic action. It is not strict in a
-- return value of type @b@, but the ne value written into the mutable variable is
-- evaluated to WHNF.
--
-- ==== __Examples__
--
modifyMRefM ::
     (MRef c e, MonadPrim s m) => c e s -> (e -> m (e, a)) -> m a
modifyMRefM ref f = do
  a <- readMRef ref
  (a', b) <- f a
  b <$ writeMRef ref a'
{-# INLINE modifyMRefM #-}


-- | Apply a monadic action to the contents of a mutable variable strictly. Returns the old value.
--
-- ==== __Examples__
--
-- >>> refName <- newMRef "My name is: "
-- >>> refMyName <- newMRef "Alexey"
-- >>> myName <- modifyFetchOldMRefM refMyName $ \ name -> "Leo" <$ modifyMRef_ refName (++ name)
-- >>> readMRef refName >>= putStrLn
-- My name is: Alexey
-- >>> putStrLn myName
-- Alexey
-- >>> readMRef refMyName >>= putStrLn
-- Leo
--
-- @since 0.1.0
modifyFetchOldMRefM ::
     (MRef c e, MonadPrim s m)
  => c e s
  -> (e -> m (e))
  -> m (e)
modifyFetchOldMRefM ref f = do
  a <- readMRef ref
  a <$ (writeMRef ref =<< f a)
{-# INLINE modifyFetchOldMRefM #-}


-- | Apply a monadic action to the contents of a mutable variable strictly. Returns the new value.
--
-- @since 0.1.0
modifyFetchNewMRefM ::
     (MRef c e, MonadPrim s m)
  => c e s
  -> (e -> m (e))
  -> m (e)
modifyFetchNewMRefM ref f = do
  a <- readMRef ref
  a' <- f a
  a' <$ writeMRef ref a'
{-# INLINE modifyFetchNewMRefM #-}
