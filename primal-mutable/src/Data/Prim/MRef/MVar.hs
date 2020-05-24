{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Data.Prim.MRef.MVar
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.MRef.MVar
  ( -- * MVar
    MVar(..)
  , isEmptyMVar
    -- ** Create
  , newMVar
  , newEmptyMVar
    -- ** Write
  , putMVar
  , tryPutMVar
    -- ** Read
  , readMVar
  , tryReadMVar
  , takeMVar
  , tryTakeMVar
  , clearMVar
  -- ** Modify
  , swapMVar
  , withMVar
  , withMVarMasked
  , modifyMVar_
  , modifyMVar
  , modifyMVarMasked_
  , modifyMVarMasked
  -- ** Weak Pointer
  , mkWeakMVar
  -- ** Conversion
  , toBaseMVar
  , fromBaseMVar
  ) where

import Control.Prim.Monad
import Data.Prim.MRef.Internal
import GHC.Exts
import GHC.Weak
import qualified Control.Concurrent.MVar as GHC
import qualified GHC.MVar as GHC


data MVar a s = MVar (MVar# s a)

-- | Checks whether supplied `MVar`s refer to the exact same one.
instance Eq (MVar a s) where
  MVar mvar1# == MVar mvar2# = isTrue# (sameMVar# mvar1# mvar2#)

instance MRef (MVar a) where
  type Elt (MVar a) = a
  newMRef = newMVar
  {-# INLINE newMRef #-}
  newRawMRef = newEmptyMVar
  {-# INLINE newRawMRef #-}
  writeMRef mvar a = tryTakeMVar mvar >> putMVar mvar a
  {-# INLINE writeMRef #-}
  readMRef = readMVar
  {-# INLINE readMRef #-}


newMVar :: MonadPrim s m => a -> m (MVar a s)
newMVar a = newEmptyMVar >>= \mvar -> mvar <$ putMVar mvar a

newEmptyMVar :: MonadPrim s m => m (MVar a s)
newEmptyMVar =
  prim $ \s ->
    case newMVar# s of
      (# s', mvar# #) -> (# s', MVar mvar# #)

putMVar :: MonadPrim s m => MVar a s -> a -> m ()
putMVar (MVar mvar#) x = prim_ (putMVar# mvar# x)

tryPutMVar :: MonadPrim s m => MVar a s -> a -> m Bool
tryPutMVar (MVar mvar#) x =
  prim $ \s ->
    case tryPutMVar# mvar# x s of
      (# s', b# #) -> (# s', isTrue# b# #)


readMVar :: MonadPrim s m => MVar a s -> m a
readMVar (MVar mvar#) = prim (readMVar# mvar#)

clearMVar :: MonadPrim s m => MVar a s -> m ()
clearMVar (MVar mvar#) =
  prim $ \s ->
    case tryReadMVar# mvar# s of
      (# s', _, _ #) -> (# s', () #)

tryReadMVar :: MonadPrim s m => MVar a s -> m (Maybe a)
tryReadMVar (MVar mvar#) =
  prim $ \s ->
    case tryReadMVar# mvar# s of
      (# s', 0#, _ #) -> (# s', Nothing #)
      (# s', _, a #) -> (# s', Just a #)

takeMVar :: MonadPrim s m => MVar a s -> m a
takeMVar (MVar mvar#) = prim $ \ s# -> takeMVar# mvar# s#



tryTakeMVar :: MonadPrim s m => MVar a s -> m (Maybe a)
tryTakeMVar (MVar mvar#) =
  prim $ \s ->
    case tryTakeMVar# mvar# s of
      (# s', 0#, _ #) -> (# s', Nothing #)
      (# s', _, a #)  -> (# s', Just a #)


swapMVar :: MonadPrim RW m => MVar a RW -> a -> m a
swapMVar (MVar mvar#) = liftPrimBase . GHC.swapMVar (GHC.MVar mvar#)
{-# INLINE swapMVar #-}

isEmptyMVar :: MonadPrim s m => MVar a s -> m Bool
isEmptyMVar (MVar mvar#) =
  prim $ \s ->
    case isEmptyMVar# mvar# s of
      (# s', isEmpty# #) -> (# s', isTrue# isEmpty# #)


mkWeakMVar :: MonadUnliftPrim RW m => MVar a RW -> m b -> m (Weak (MVar a RW))
mkWeakMVar mvar@(MVar mvar#) finalizer =
  runInPrimBase finalizer $ \f s ->
    case mkWeak# mvar# mvar f s of
      (# s', weak# #) -> (# s', Weak weak# #)

withMVar :: MonadUnliftPrim RW m => MVar a RW -> (a -> m b) -> m b
withMVar (MVar mvar#) action =
  withRunInPrimBase $ \run -> GHC.withMVar (GHC.MVar mvar#) (run . action)
{-# INLINE withMVar #-}


withMVarMasked :: MonadUnliftPrim RW m => MVar a RW -> (a -> m b) -> m b
withMVarMasked (MVar mvar#) action =
  withRunInPrimBase $ \run -> GHC.withMVarMasked (GHC.MVar mvar#) (run . action)
{-# INLINE withMVarMasked #-}

modifyMVar :: MonadUnliftPrim RW m => MVar a RW -> (a -> m (a, b)) -> m b
modifyMVar (MVar mvar#) action =
  withRunInPrimBase $ \run -> GHC.modifyMVar (GHC.MVar mvar#) (run . action)
{-# INLINE modifyMVar #-}

modifyMVar_ :: MonadUnliftPrim RW m => MVar a RW -> (a -> m a) -> m ()
modifyMVar_ (MVar mvar#) action =
  withRunInPrimBase $ \run -> GHC.modifyMVar_ (GHC.MVar mvar#) (run . action)
{-# INLINE modifyMVar_ #-}



modifyMVarMasked :: MonadUnliftPrim RW m => MVar a RW -> (a -> m (a, b)) -> m b
modifyMVarMasked (MVar mvar#) action =
  withRunInPrimBase $ \run -> GHC.modifyMVarMasked (GHC.MVar mvar#) (run . action)
{-# INLINE modifyMVarMasked #-}

modifyMVarMasked_ :: MonadUnliftPrim RW m => MVar a RW -> (a -> m a) -> m ()
modifyMVarMasked_ (MVar mvar#) action =
  withRunInPrimBase $ \run -> GHC.modifyMVarMasked_ (GHC.MVar mvar#) (run . action)
{-# INLINE modifyMVarMasked_ #-}

-- | Cast `MVar` into and the `GHC.MVar` from "Control.Concurrent.MVar"
toBaseMVar :: MVar a RW -> GHC.MVar a
toBaseMVar (MVar mvar#) = GHC.MVar mvar#

-- | Cast `GHC.MVar` from "Control.Concurrent.MVar" into `MVar` from @primal@
fromBaseMVar :: GHC.MVar a -> MVar a RW
fromBaseMVar (GHC.MVar mvar#) = MVar mvar#
