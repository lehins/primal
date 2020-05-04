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
  , rawPVar
  , rawPinnedPVar
  , rawAlignedPinnedPVar
  , unsafeToPtrPVar
  , readPVar
  , writePVar
  , isPinnedPVar
  , sizeOfPVar
  , alignmentPVar
  , atomicModifyPVar
  , atomicModifyPVar_
  , unI#
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
data PVar a s = PVar (MutableByteArray# s)

-- | @`S.poke`+`S.peek`@ will result in a new copy of a `PVar`
instance Prim a => S.Storable (PVar a RW) where
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
instance NFData (PVar a s) where
  rnf (PVar _) = ()

unI# :: Int -> Int#
unI# (I# i#) = i#
{-# INLINE unI# #-}

-- | Create a mutable variable in unpinned memory (i.e. GC can move it) with an initial
-- value. This is a prefered way to create a mutable variable, since it will not
-- contribute to memory fragmentation. For pinned memory versions see `newPinnedPVar` and
-- `newAlignedPinnedPVar`
--
-- @since 0.1.0
newPVar :: (MonadPrim s m, Prim a) => a -> m (PVar a s)
newPVar v = do
  pvar <- rawPVar
  pvar <$ writePVar pvar v
{-# INLINE newPVar #-}

-- | Create a mutable variable in unpinned and unititialized memory
--
-- @since 0.1.0
rawPVar ::
     forall a m s. (MonadPrim s m, Prim a)
  => m (PVar a s)
rawPVar =
  prim $ \s# ->
    case newByteArray# (sizeOf# (proxy# :: Proxy# a)) s# of
      (# s'#, mba# #) -> (# s'#, PVar mba# #)
{-# INLINE rawPVar #-}


-- | Create a mutable variable in pinned memory with an initial value.
--
-- @since 0.1.0
newPinnedPVar :: (MonadPrim s m, Prim a) => a -> m (PVar a s)
newPinnedPVar v = do
  pvar <- rawPinnedPVar
  pvar <$ writePVar pvar v
{-# INLINE newPinnedPVar #-}

-- | Create a mutable variable in pinned memory with uninitialized memory.
--
-- @since 0.1.0
rawPinnedPVar ::
     forall a m s. (MonadPrim s m, Prim a)
  => m (PVar a s)
rawPinnedPVar =
  prim $ \s# ->
    case newPinnedByteArray# (sizeOf# (proxy# :: Proxy# a)) s# of
      (# s'#, mba# #) -> (# s'#, PVar mba# #)
{-# INLINE rawPinnedPVar #-}


-- | Create a mutable variable in pinned memory with an initial value and aligned
-- according to its `Data.Prim.Types.alignment`
--
-- @since 0.1.0
newAlignedPinnedPVar :: (MonadPrim s m, Prim a) => a -> m (PVar a s)
newAlignedPinnedPVar v = do
  pvar <- rawAlignedPinnedPVar
  pvar <$ writePVar pvar v
{-# INLINE newAlignedPinnedPVar #-}


-- | Create a mutable variable in pinned uninitialized memory.
--
-- @since 0.1.0
rawAlignedPinnedPVar ::
     forall a m s. (MonadPrim s m, Prim a)
  => m (PVar a s)
rawAlignedPinnedPVar =
  let dummy = proxy# :: Proxy# a
   in prim $ \s# ->
        case newAlignedPinnedByteArray# (sizeOf# dummy) (alignment# dummy) s# of
          (# s'#, mba# #) -> (# s'#, PVar mba# #)
{-# INLINE rawAlignedPinnedPVar #-}


-- | Get the address to the contents. This is highly unsafe, espcially if memory is not pinned
--
-- @since 0.1.0
unsafeToPtrPVar :: PVar a s -> Ptr a
unsafeToPtrPVar (PVar mba#) = Ptr (byteArrayContents# (unsafeCoerce# mba#))
{-# INLINE unsafeToPtrPVar #-}

-- | Read a value from a mutable variable
--
-- @since 0.1.0
readPVar :: (MonadPrim s m, Prim a) => PVar a s -> m a
readPVar (PVar mba#) = prim (readMutableByteArray# mba# 0#)
{-# INLINE readPVar #-}

-- | Write a value into a mutable variable
--
-- @since 0.1.0
writePVar :: (MonadPrim s m, Prim a) => PVar a s -> a -> m ()
writePVar (PVar mba#) v = prim_ (writeMutableByteArray# mba# 0# v)
{-# INLINE writePVar #-}

-- | Size in bytes of a value stored inside the mutable variable. `PVar` itself is neither
-- accessed nor evaluated.
--
-- @since 0.1.0
sizeOfPVar :: forall a s. Prim a => PVar a s -> Int
sizeOfPVar _ = I# (sizeOf# (proxy# :: Proxy# a))
{-# INLINE sizeOfPVar #-}

-- | Alignment in bytes of the value stored inside of the mutable variable. `PVar` itself is
-- neither accessed nor evaluated.
--
-- @since 0.1.0
alignmentPVar :: forall a s. Prim a => PVar a s -> Int
alignmentPVar _ = I# (alignment# (proxy# :: Proxy# a))
{-# INLINE alignmentPVar #-}


-- | Check if `PVar` is backed by pinned memory or not
--
-- @since 0.1.0
isPinnedPVar :: PVar a s -> Bool
isPinnedPVar (PVar mba#) = isTrue# (isMutableByteArrayPinned# mba#)
{-# INLINE isPinnedPVar #-}


-- | Apply a function to an integer element of a `PVar` atomically. Implies a full memory
-- barrier.
--
-- @since 0.1.0
atomicModifyPVar ::
     (MonadPrim s m, Atomic a) => PVar a s -> (a -> (a, b)) -> m b
atomicModifyPVar (PVar mba#) f =
  prim (atomicModifyMutableByteArray# mba# 0# g)
  where
    g a =
      case f a of
        (a', b) -> (# a', b #)
    {-# INLINE g #-}
{-# INLINE atomicModifyPVar #-}


-- | Apply a function to an integer element of a `PVar` atomically. Returns the old
-- value. Implies a full memory barrier.
--
-- @since 0.1.0
atomicModifyPVar_ ::
     (MonadPrim s m, Atomic a) => PVar a s -> (a -> a) -> m ()
atomicModifyPVar_ (PVar mba#) f =
  prim_ (atomicModifyMutableByteArray_# mba# 0# f)
{-# INLINE atomicModifyPVar_ #-}

