{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Data.Prim.PVar.Internal
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.PVar.Internal
  ( PVar(..)
  , newPVar
  , newPinnedPVar
  , newAlignedPinnedPVar
  , allocPVar
  , allocPinnedPVar
  , allocAlignedPinnedPVar
  , unsafeToPtrPVar
  , readPVar
  , writePVar
  , isPinnedPVar
  , sizeOfPVar
  , alignmentPVar
  , atomicModifyPVar
  , atomicModifyPVar_
  )
  where

import Control.DeepSeq
import Control.Prim.Monad
import Data.Prim
import Data.Prim.Atomic
import Data.Prim.Class
import qualified Foreign.Storable as S
import GHC.Exts

-- | Mutable variable with primitive value.
--
-- @since 0.1.0
data PVar e s = PVar (MutableByteArray# s)

-- | @`S.poke`+`S.peek`@ will result in a new copy of a `PVar`
instance Prim e => S.Storable (PVar e RW) where
  sizeOf = sizeOfPVar
  {-# INLINE sizeOf #-}
  alignment = alignmentPVar
  {-# INLINE alignment #-}
  peekElemOff (Ptr addr#) (I# i#) = do
    a <- prim (readOffAddr# addr# i#)
    newAlignedPinnedPVar a
  {-# INLINE peekElemOff #-}
  pokeElemOff (Ptr addr#) (I# i#) pvar = do
    a <- readPVar pvar
    prim_ (writeOffAddr# addr# i# a)
  {-# INLINE pokeElemOff #-}

-- | Values are already written into `PVar` in NF, this instance is trivial.
instance NFData (PVar e s) where
  rnf (PVar _) = ()

-- | Create a mutable variable in unpinned memory (i.e. GC can move it) with an initial
-- value. This is a prefered way to create a mutable variable, since it will not
-- contribute to memory fragmentation. For pinned memory versions see `newPinnedPVar` and
-- `newAlignedPinnedPVar`
--
-- @since 0.1.0
newPVar :: (MonadPrim s m, Prim e) => e -> m (PVar e s)
newPVar v = do
  pvar <- allocPVar
  pvar <$ writePVar pvar v
{-# INLINE newPVar #-}

-- | Create a mutable variable in unpinned and unititialized memory
--
-- @since 0.1.0
allocPVar ::
     forall e m s. (MonadPrim s m, Prim e)
  => m (PVar e s)
allocPVar =
  prim $ \s ->
    case newByteArray# (sizeOf# (proxy# :: Proxy# e)) s of
      (# s', mba# #) -> (# s', PVar mba# #)
{-# INLINE allocPVar #-}


-- | Create a mutable variable in pinned memory with an initial value.
--
-- @since 0.1.0
newPinnedPVar :: (MonadPrim s m, Prim e) => e -> m (PVar e s)
newPinnedPVar v = do
  pvar <- allocPinnedPVar
  pvar <$ writePVar pvar v
{-# INLINE newPinnedPVar #-}

-- | Create a mutable variable in pinned memory with uninitialized memory.
--
-- @since 0.1.0
allocPinnedPVar ::
     forall e m s. (MonadPrim s m, Prim e)
  => m (PVar e s)
allocPinnedPVar =
  prim $ \s ->
    case newPinnedByteArray# (sizeOf# (proxy# :: Proxy# e)) s of
      (# s', mba# #) -> (# s', PVar mba# #)
{-# INLINE allocPinnedPVar #-}


-- | Create a mutable variable in pinned memory with an initial value and aligned
-- according to its `Data.Prim.Types.alignment`
--
-- @since 0.1.0
newAlignedPinnedPVar :: (MonadPrim s m, Prim e) => e -> m (PVar e s)
newAlignedPinnedPVar v = do
  pvar <- allocAlignedPinnedPVar
  pvar <$ writePVar pvar v
{-# INLINE newAlignedPinnedPVar #-}


-- | Create a mutable variable in pinned uninitialized memory.
--
-- @since 0.1.0
allocAlignedPinnedPVar ::
     forall e m s. (MonadPrim s m, Prim e)
  => m (PVar e s)
allocAlignedPinnedPVar =
  let px# = proxy# :: Proxy# e
   in prim $ \s ->
        case newAlignedPinnedByteArray# (sizeOf# px#) (alignment# px#) s of
          (# s', mba# #) -> (# s', PVar mba# #)
{-# INLINE allocAlignedPinnedPVar #-}


-- | Get the address to the contents. This is highly unsafe, espcially if memory is not pinned
--
-- @since 0.1.0
unsafeToPtrPVar :: PVar e s -> Ptr a
unsafeToPtrPVar (PVar mba#) = Ptr (byteArrayContents# (unsafeCoerce# mba#))
{-# INLINE unsafeToPtrPVar #-}

-- | Read a value from a mutable variable
--
-- @since 0.1.0
readPVar :: (MonadPrim s m, Prim e) => PVar e s -> m e
readPVar (PVar mba#) = prim (readMutableByteArray# mba# 0#)
{-# INLINE readPVar #-}

-- | Write a value into a mutable variable
--
-- @since 0.1.0
writePVar :: (MonadPrim s m, Prim e) => PVar e s -> e -> m ()
writePVar (PVar mba#) v = prim_ (writeMutableByteArray# mba# 0# v)
{-# INLINE writePVar #-}

-- | Size in bytes of a value stored inside the mutable variable. `PVar` itself is neither
-- accessed nor evaluated.
--
-- @since 0.1.0
sizeOfPVar :: forall e s. Prim e => PVar e s -> Int
sizeOfPVar _ = I# (sizeOf# (proxy# :: Proxy# e))
{-# INLINE sizeOfPVar #-}

-- | Alignment in bytes of the value stored inside of the mutable variable. `PVar` itself is
-- neither accessed nor evaluated.
--
-- @since 0.1.0
alignmentPVar :: forall e s. Prim e => PVar e s -> Int
alignmentPVar _ = I# (alignment# (proxy# :: Proxy# e))
{-# INLINE alignmentPVar #-}


-- | Check if `PVar` is backed by pinned memory or not
--
-- @since 0.1.0
isPinnedPVar :: PVar e s -> Bool
isPinnedPVar (PVar mba#) = isTrue# (isMutableByteArrayPinned# mba#)
{-# INLINE isPinnedPVar #-}


-- | Apply a function to an integer element of a `PVar` atomically. Implies a full memory
-- barrier.
--
-- @since 0.1.0
atomicModifyPVar ::
     (MonadPrim s m, Atomic e) => PVar e s -> (e -> (e, b)) -> m b
atomicModifyPVar (PVar mba#) f =
  prim $
  atomicModifyMutableByteArray# mba# 0# $ \a ->
    case f a of
      (a', b) -> (# a', b #)
{-# INLINE atomicModifyPVar #-}


-- | Apply a function to an integer element of a `PVar` atomically. Returns the old
-- value. Implies a full memory barrier.
--
-- @since 0.1.0
atomicModifyPVar_ ::
     (MonadPrim s m, Atomic e) => PVar e s -> (e -> e) -> m ()
atomicModifyPVar_ (PVar mba#) f =
  prim_ (atomicModifyMutableByteArray_# mba# 0# f)
{-# INLINE atomicModifyPVar_ #-}

