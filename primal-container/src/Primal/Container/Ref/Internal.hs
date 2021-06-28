{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-- |
-- Module      : Primal.Container.Ref.Internal
-- Copyright   : (c) Alexey Kuleshevich 2020-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Container.Ref.Internal
  ( MutRef(..)
  , newMutRef
  , newRawMutRef
  , readMutRef
  , writeMutRef
  , modifyMutRef
  , modifyMutRef_
  , modifyFetchOldMutRef
  , modifyFetchNewMutRef
  , modifyMutRefM
  , modifyMutRefM_
  , modifyFetchOldMutRefM
  , modifyFetchNewMutRefM
  , module Primal.Container.Internal
  ) where

import Primal.Monad
import Primal.Concurrent.MVar
import Primal.Container.Internal
import Primal.Array
import Primal.Ref
import Primal.Memory
import Primal.Memory.Addr
import Primal.Memory.PArray

class MutRef mr where

  newMutRefST :: Elt mr e => e -> ST s (mr e s)
  newMutRefST a = newRawMutRef >>= \mut -> mut <$ writeMutRefST mut a
  {-# INLINE newMutRefST #-}

  newRawMutRefST :: Elt mr e => ST s (mr e s)

  readMutRefST :: Elt mr e => mr e s -> ST s e

  writeMutRefST :: Elt mr e => mr e s -> e -> ST s ()


-- | Read\/write aren't atomic - /not/ thread safe.
instance MutRef MVar where
  newMutRefST = newMVar
  {-# INLINE newMutRefST #-}
  newRawMutRefST = newEmptyMVar
  {-# INLINE newRawMutRefST #-}
  writeMutRefST = writeMVar
  {-# INLINE writeMutRefST #-}
  readMutRefST = readMVar
  {-# INLINE readMutRefST #-}


instance MutRef MAddr where
  newRawMutRefST = allocMAddr 1
  {-# INLINE newRawMutRefST #-}
  writeMutRefST = writeMAddr
  {-# INLINE writeMutRefST #-}
  readMutRefST = readMAddr
  {-# INLINE readMutRefST #-}


instance Typeable p => MutRef (PMArray p) where
  newRawMutRefST = allocPMArray 1
  {-# INLINE newRawMutRefST #-}
  writeMutRefST mba = writePMArray mba 0
  {-# INLINE writeMutRefST #-}
  readMutRefST mba = readPMArray mba 0
  {-# INLINE readMutRefST #-}



instance MutRef BRef where
  newMutRefST = newBRef
  {-# INLINE newMutRefST #-}
  newRawMutRefST = newBRef (uninitialized "Primal.Container.Mutable.Ref.Internal" "newRawMutRefST")
  {-# INLINE newRawMutRefST #-}
  writeMutRefST = writeBRef
  {-# INLINE writeMutRefST #-}
  readMutRefST = readBRef
  {-# INLINE readMutRefST #-}



instance MutRef BMArray where
  newRawMutRefST = newRawBMArray 1
  {-# INLINE newRawMutRefST #-}
  readMutRefST mba = readBMArray mba 0
  {-# INLINE readMutRefST #-}
  writeMutRefST mba = writeBMArray mba 0
  {-# INLINE writeMutRefST #-}
  newMutRefST = newBMArray 1
  {-# INLINE newMutRefST #-}


instance MutRef SBMArray where
  newRawMutRefST = newRawSBMArray 1
  {-# INLINE newRawMutRefST #-}
  readMutRefST mba = readSBMArray mba 0
  {-# INLINE readMutRefST #-}
  writeMutRefST mba = writeSBMArray mba 0
  {-# INLINE writeMutRefST #-}
  newMutRefST = newSBMArray 1
  {-# INLINE newMutRefST #-}


instance MutRef UMArray where
  newRawMutRefST = newRawUMArray 1
  {-# INLINE newRawMutRefST #-}
  readMutRefST mba = readUMArray mba 0
  {-# INLINE readMutRefST #-}
  writeMutRefST mba = writeUMArray mba 0
  {-# INLINE writeMutRefST #-}
  newMutRefST = newUMArray 1
  {-# INLINE newMutRefST #-}

modifyMutRef ::
     (MutRef mr, Elt mr e, Primal s m) => mr e s -> (e -> (e, a)) -> m a
modifyMutRef ref f = modifyMutRefM ref (pure . f)
{-# INLINE modifyMutRef #-}


modifyMutRef_ :: (MutRef mr, Elt mr e, Primal s m) => mr e s -> (e -> e) -> m ()
modifyMutRef_ ref f = modifyMutRefM_ ref (pure . f)
{-# INLINE modifyMutRef_ #-}



modifyFetchOldMutRef ::
     (MutRef mr, Elt mr e, Primal s m)
  => mr e s
  -> (e -> e)
  -> m e
modifyFetchOldMutRef ref f = modifyFetchOldMutRefM ref (pure . f)
{-# INLINE modifyFetchOldMutRef #-}


-- | Apply a monadic action to the contents of a mutable variable strictly. Returns the new value.
--
-- @since 0.1.0
modifyFetchNewMutRef ::
     (MutRef mr, Elt mr e, Primal s m)
  => mr e s
  -> (e -> e)
  -> m e
modifyFetchNewMutRef ref f = modifyFetchNewMutRefM ref (pure . f)
{-# INLINE modifyFetchNewMutRef #-}




-- | Modify value of a mutable variable with a monadic action. Result is written strictly.
--
-- ==== __Examples__
--
-- >>> ref <- newMutRef (Just "Some value")
-- >>> modifyMutRefM_ ref $ \ mv -> Nothing <$ mapM_ putStrLn mv
-- Some value
-- >>> readMutRef ref
-- Nothing
--
-- @since 0.1.0
modifyMutRefM_ :: (MutRef mr, Elt mr e, Primal s m) => mr e s -> (e -> m e) -> m ()
modifyMutRefM_ ref f = readMutRef ref >>= f >>= writeMutRef ref
{-# INLINE modifyMutRefM_ #-}




-- | Modify value of a mutable variable with a monadic action. It is not strict in a
-- return value of type @b@, but the ne value written into the mutable variable is
-- evaluated to WHNF.
--
-- ==== __Examples__
--
modifyMutRefM ::
     (MutRef mr, Elt mr e, Primal s m) => mr e s -> (e -> m (e, a)) -> m a
modifyMutRefM ref f = do
  a <- readMutRef ref
  (a', b) <- f a
  b <$ writeMutRef ref a'
{-# INLINE modifyMutRefM #-}


-- | Apply a monadic action to the contents of a mutable variable strictly. Returns the old value.
--
-- ==== __Examples__
--
-- >>> refName <- newMutRef "My name is: "
-- >>> refMyName <- newMutRef "Alexey"
-- >>> myName <- modifyFetchOldMutRefM refMyName $ \ name -> "Leo" <$ modifyMutRef_ refName (++ name)
-- >>> readMutRef refName >>= putStrLn
-- My name is: Alexey
-- >>> putStrLn myName
-- Alexey
-- >>> readMutRef refMyName >>= putStrLn
-- Leo
--
-- @since 0.1.0
modifyFetchOldMutRefM ::
     (MutRef mr, Elt mr e, Primal s m)
  => mr e s
  -> (e -> m e)
  -> m e
modifyFetchOldMutRefM ref f = do
  a <- readMutRef ref
  a <$ (writeMutRef ref =<< f a)
{-# INLINE modifyFetchOldMutRefM #-}


-- | Apply a monadic action to the contents of a mutable variable strictly. Returns the new value.
--
-- @since 0.1.0
modifyFetchNewMutRefM ::
     (MutRef mr, Elt mr e, Primal s m)
  => mr e s
  -> (e -> m e)
  -> m e
modifyFetchNewMutRefM ref f = do
  a <- readMutRef ref
  a' <- f a
  a' <$ writeMutRef ref a'
{-# INLINE modifyFetchNewMutRefM #-}



newMutRef :: (MutRef mr, Elt mr e, Primal s m) => e -> m (mr e s)
newMutRef = liftST . newMutRefST
{-# INLINE newMutRef #-}

newRawMutRef :: (MutRef mr, Elt mr e, Primal s m) => m (mr e s)
newRawMutRef = liftST newRawMutRefST
{-# INLINE newRawMutRef #-}

readMutRef :: (MutRef mr, Elt mr e, Primal s m) => mr e s -> m e
readMutRef = liftST . readMutRefST
{-# INLINE readMutRef #-}

writeMutRef :: (MutRef mr, Elt mr e, Primal s m) => mr e s -> e -> m ()
writeMutRef mr = liftST . writeMutRefST mr
{-# INLINE writeMutRef #-}

