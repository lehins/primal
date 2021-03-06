{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.Prim.MRef.Internal
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.MRef.Internal
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

import Control.Prim.Concurrent.MVar
import Control.Prim.Monad
import Data.Prim.Array
import Data.Prim.Memory
import Data.Prim.Memory.Addr
import Data.Prim.Memory.Bytes
import Data.Prim.Memory.PArray
import Data.Prim.Ref

class MRef mut where
  type Elt mut :: *

  newMRef :: MonadPrim s m => Elt mut -> m (mut s)
  newMRef a = newRawMRef >>= \mut -> mut <$ writeMRef mut a
  {-# INLINE newMRef #-}

  newRawMRef :: MonadPrim s m => m (mut s)

  readMRef :: MonadPrim s m => mut s -> m (Elt mut)

  writeMRef :: MonadPrim s m => mut s -> Elt mut -> m ()

-- | Not thread safe.
instance MRef (MVar a) where
  type Elt (MVar a) = a
  newMRef = newMVar
  {-# INLINE newMRef #-}
  newRawMRef = newEmptyMVar
  {-# INLINE newRawMRef #-}
  writeMRef = writeMVar
  {-# INLINE writeMRef #-}
  readMRef = readMVar
  {-# INLINE readMRef #-}

instance Typeable p => MRef (MBytes p) where
  type Elt (MBytes p) = Word8

  newRawMRef = allocMutMem (1 :: Count Word8)
  {-# INLINE newRawMRef #-}

  writeMRef mb = writeOffMBytes mb 0
  {-# INLINE writeMRef #-}

  readMRef mb = readOffMBytes mb 0
  {-# INLINE readMRef #-}



instance Prim e => MRef (MAddr e) where
  type Elt (MAddr e) = e

  newRawMRef = allocMAddr 1
  {-# INLINE newRawMRef #-}

  writeMRef = writeMAddr
  {-# INLINE writeMRef #-}

  readMRef = readMAddr
  {-# INLINE readMRef #-}



instance (Typeable p, Prim e) => MRef (PMArray p e) where
  type Elt (PMArray p e) = e

  newRawMRef = allocPMArray 1
  {-# INLINE newRawMRef #-}

  writeMRef mba = writePMArray mba 0
  {-# INLINE writeMRef #-}

  readMRef mba = readPMArray mba 0
  {-# INLINE readMRef #-}



instance MRef (Ref a) where
  type Elt (Ref a) = a
  newMRef = newRef
  {-# INLINE newMRef #-}
  newRawMRef = newRef (uninitialized "Data.Prim.MRef.Internal" "newRawMRef")
  {-# INLINE newRawMRef #-}
  writeMRef = writeRef
  {-# INLINE writeMRef #-}
  readMRef = readRef
  {-# INLINE readMRef #-}


-- modifyRefM :: MonadPrim s m => Ref a s -> (a -> m (a, b)) -> m b
-- modifyRefM ref f = do
--   a <- readRef ref
--   (a', b) <- f a
--   b <$ writeRef ref a'
-- {-# INLINE modifyRefM #-}

modifyMRef ::
     (MRef mut, MonadPrim s m) => mut s -> (Elt mut -> (Elt mut, a)) -> m a
modifyMRef ref f = modifyMRefM ref (pure . f)
{-# INLINE modifyMRef #-}


modifyMRef_ :: (MRef mut, MonadPrim s m) => mut s -> (Elt mut -> Elt mut) -> m ()
modifyMRef_ ref f = modifyMRefM_ ref (pure . f)
{-# INLINE modifyMRef_ #-}



modifyFetchOldMRef ::
     (MRef mut, MonadPrim s m)
  => mut s
  -> (Elt mut -> Elt mut)
  -> m (Elt mut)
modifyFetchOldMRef ref f = modifyFetchOldMRefM ref (pure . f)
{-# INLINE modifyFetchOldMRef #-}


-- | Apply a monadic action to the contents of a mutable variable strictly. Returns the new value.
--
-- @since 0.1.0
modifyFetchNewMRef ::
     (MRef mut, MonadPrim s m)
  => mut s
  -> (Elt mut -> Elt mut)
  -> m (Elt mut)
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
modifyMRefM_ :: (MRef mut, MonadPrim s m) => mut s -> (Elt mut -> m (Elt mut)) -> m ()
modifyMRefM_ ref f = readMRef ref >>= f >>= writeMRef ref
{-# INLINE modifyMRefM_ #-}




-- | Modify value of a mutable variable with a monadic action. It is not strict in a
-- return value of type @b@, but the ne value written into the mutable variable is
-- evaluated to WHNF.
--
-- ==== __Examples__
--
modifyMRefM ::
     (MRef mut, MonadPrim s m) => mut s -> (Elt mut -> m (Elt mut, a)) -> m a
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
     (MRef mut, MonadPrim s m)
  => mut s
  -> (Elt mut -> m (Elt mut))
  -> m (Elt mut)
modifyFetchOldMRefM ref f = do
  a <- readMRef ref
  a <$ (writeMRef ref =<< f a)
{-# INLINE modifyFetchOldMRefM #-}


-- | Apply a monadic action to the contents of a mutable variable strictly. Returns the new value.
--
-- @since 0.1.0
modifyFetchNewMRefM ::
     (MRef mut, MonadPrim s m)
  => mut s
  -> (Elt mut -> m (Elt mut))
  -> m (Elt mut)
modifyFetchNewMRefM ref f = do
  a <- readMRef ref
  a' <- f a
  a' <$ writeMRef ref a'
{-# INLINE modifyFetchNewMRefM #-}
