{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif
-- |
-- Module      : Data.Prim.PVar
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.PVar
  ( -- | `PVar` has significantly better performance characteristics over
    -- `Data.IORef.IORef`, `Data.STRef.STRef` and `Data.Prim.MutVar.MutVar`. This is
    -- because value is mutated directly in memory instead of following an extra
    -- pointer. Besides better performance there is another consequence of direct
    -- mutation, namely the value is always evaluated to normal form when being written
    -- into a `PVar`

    PVar
  -- * Creation
  , newPVar
  , withPVarST
  -- * Mutable Operations
  , readPVar
  , writePVar
  , modifyPVar
  , modifyPVar_
  , modifyFetchOldPVar
  , modifyFetchNewPVar
  , modifyPVarM
  , modifyPVarM_
  , modifyFetchOldPVarM
  , modifyFetchNewPVarM
  , swapPVars_
  , swapPVars
  , copyPVar
  , sizeOfPVar
  , alignmentPVar
  -- * Pinned memory
  , newPinnedPVar
  , newAlignedPinnedPVar
  , withPtrPVar
  , copyPVarToPtr
  , toForeignPtrPVar
  , isPinnedPVar
  , ensurePinnedPVar
  -- * Atomic Operations
  , atomicReadPVar
  , atomicWritePVar
  , atomicModifyPVar
  , atomicModifyPVar_
  , atomicModifyFetchOldPVar
  , atomicModifyFetchNewPVar
  , casPVar
  -- ** Arithmetic
  -- *** Addition
  , (!+)
  , atomicAddFetchOldPVar
  , atomicAddFetchNewPVar
  -- *** Subtraction
  , (!-)
  , atomicSubFetchOldPVar
  , atomicSubFetchNewPVar
  -- ** Binary
  -- *** AND
  , (!&)
  , atomicAndFetchOldPVar
  , atomicAndFetchNewPVar
  -- *** NAND
  , (!~&)
  , atomicNandFetchOldPVar
  , atomicNandFetchNewPVar
  -- *** OR
  , (!|)
  , atomicOrFetchOldPVar
  , atomicOrFetchNewPVar
  -- *** XOR
  , (!^)
  , atomicXorFetchOldPVar
  , atomicXorFetchNewPVar
  -- *** NOT
  , (!~)
  , atomicNotFetchOldPVar
  , atomicNotFetchNewPVar
  -- * Re-export
  , module Data.Prim
  ) where

import Control.Prim.Monad
import Control.Monad.ST (ST, runST)
import Data.Prim
import Data.Prim.Atomic
import Data.Prim.PVar.Internal
import Data.Prim.PVar.Unsafe
import Foreign.Prim


-- | Run an `ST` action on a mutable variable.
--
-- @since 0.1.0
withPVarST ::
     Prim e
  => e -- ^ Initial value assigned to the mutable variable
  -> (forall s. PVar e s -> ST s a) -- ^ Action to run
  -> a -- ^ Result produced by the `ST` action
withPVarST x st = runST (newPVar x >>= st)
{-# INLINE withPVarST #-}

-- | Apply an action to the `Ptr` that references the mutable variable, but only if it is
-- backed by pinned memory, cause otherwise it would be unsafe.
--
-- @since 0.1.0
withPtrPVar :: (MonadPrim s m, Prim e) => PVar e s -> (Ptr e -> m b) -> m (Maybe b)
withPtrPVar pvar f =
  forM (toPtrPVar pvar) $ \ptr -> do
    r <- f ptr
    r <$ touch pvar
{-# INLINE withPtrPVar #-}

-- | Convert `PVar` into a `ForeignPtr`, but only if it is backed by pinned memory.
--
-- @since 0.1.0
toForeignPtrPVar :: PVar e s -> Maybe (ForeignPtr e)
toForeignPtrPVar pvar
  | isPinnedPVar pvar = Just $ unsafeToForeignPtrPVar pvar
  | otherwise = Nothing
{-# INLINE toForeignPtrPVar #-}

-- | Make sure that `PVar` is backed by pinned memory by making a new instance with the
-- same value, unless it is already backed by pinned memory, in which case the same
-- variable is returned.
--
-- @since 0.1.0
ensurePinnedPVar :: (MonadPrim s m, Prim e) => PVar e s -> m (PVar e s)
ensurePinnedPVar pvar
  | isPinnedPVar pvar = pure pvar
  | otherwise = readPVar pvar >>= newAlignedPinnedPVar
{-# INLINE ensurePinnedPVar #-}


-- | Copy contents of one mutable variable `PVar` into another
--
-- @since 0.1.0
copyPVar ::
     (MonadPrim s m, Prim e)
  => PVar e s -- ^ Source variable
  -> PVar e s -- ^ Destination variable
  -> m ()
copyPVar pvar@(PVar mbas#) (PVar mbad#) =
  prim_ (copyMutableByteArray# mbas# 0# mbad# 0# (unInt# (sizeOfPVar pvar)))
{-# INLINE copyPVar #-}

-- | Copy contents of a mutable variable `PVar` into a pointer `Ptr`
--
-- @since 0.1.0
copyPVarToPtr :: (MonadPrim s m, Prim e) => PVar e s -> Ptr e -> m ()
copyPVarToPtr pvar@(PVar mbas#) (Ptr addr#) =
  prim_ (copyMutableByteArrayToAddr# mbas# 0# addr# (unInt# (sizeOfPVar pvar)))
{-# INLINE copyPVarToPtr #-}

-- | Apply a pure function to the contents of a mutable variable. Returns the artifact of
-- computation.
--
-- @since 0.1.0
modifyPVar :: (MonadPrim s m, Prim e) => PVar e s -> (e -> (e, b)) -> m b
modifyPVar pvar f = modifyPVarM pvar (return . f)
{-# INLINE modifyPVar #-}

-- | Apply a pure function to the contents of a mutable variable.
--
-- @since 0.1.0
modifyPVar_ :: (MonadPrim s m, Prim e) => PVar e s -> (e -> e) -> m ()
modifyPVar_ pvar f = modifyPVarM_ pvar (return . f)
{-# INLINE modifyPVar_ #-}


-- | Apply a pure function to the contents of a mutable variable. Returns the old value.
--
-- @since 0.1.0
modifyFetchOldPVar :: (MonadPrim s m, Prim e) => PVar e s -> (e -> e) -> m e
modifyFetchOldPVar pvar f = modifyFetchOldPVarM pvar (return . f)
{-# INLINE modifyFetchOldPVar #-}

-- | Apply a pure function to the contents of a mutable variable. Returns the new value.
--
-- @since 0.1.0
modifyFetchNewPVar :: (MonadPrim s m, Prim e) => PVar e s -> (e -> e) -> m e
modifyFetchNewPVar pvar f = modifyFetchNewPVarM pvar (return . f)
{-# INLINE modifyFetchNewPVar #-}


-- | Apply a monadic action to the contents of a mutable variable. Returns the artifact of
-- computation.
--
-- @since 0.1.0
modifyPVarM :: (MonadPrim s m, Prim e) => PVar e s -> (e -> m (e, b)) -> m b
modifyPVarM pvar f = do
  a <- readPVar pvar
  (a', b) <- f a
  b <$ writePVar pvar a'
{-# INLINE modifyPVarM #-}

-- | Apply a monadic action to the contents of a mutable variable. Returns the old value.
--
-- @since 0.1.0
modifyFetchOldPVarM :: (MonadPrim s m, Prim e) => PVar e s -> (e -> m e) -> m e
modifyFetchOldPVarM pvar f = do
  a <- readPVar pvar
  a <$ (writePVar pvar =<< f a)
{-# INLINE modifyFetchOldPVarM #-}


-- | Apply a monadic action to the contents of a mutable variable. Returns the new value.
--
-- @since 0.1.0
modifyFetchNewPVarM :: (MonadPrim s m, Prim e) => PVar e s -> (e -> m e) -> m e
modifyFetchNewPVarM pvar f = do
  a <- readPVar pvar
  a' <- f a
  a' <$ writePVar pvar a'
{-# INLINE modifyFetchNewPVarM #-}


-- | Apply a monadic action to the contents of a mutable variable.
--
-- @since 0.1.0
modifyPVarM_ :: (MonadPrim s m, Prim e) => PVar e s -> (e -> m e) -> m ()
modifyPVarM_ pvar f = readPVar pvar >>= f >>= writePVar pvar
{-# INLINE modifyPVarM_ #-}

-- | Swap contents of two mutable variables. Returns their old values.
--
-- @since 0.1.0
swapPVars :: (MonadPrim s m, Prim e) => PVar e s -> PVar e s -> m (e, e)
swapPVars pvar1 pvar2 = do
  a1 <- readPVar pvar1
  a2 <- modifyFetchOldPVar pvar2 (const a1)
  (a1, a2) <$ writePVar pvar1 a2
{-# INLINE swapPVars #-}

-- | Swap contents of two mutable variables.
--
-- @since 0.1.0
swapPVars_ :: (MonadPrim s m, Prim e) => PVar e s -> PVar e s -> m ()
swapPVars_ pvar1 pvar2 = void $ swapPVars pvar1 pvar2
{-# INLINE swapPVars_ #-}



-- | Create a new `PVar` in pinned memory with an initial value in it aligned on the size of
-- an `Int`. Implies a full memory barrier.
--
-- @since 0.1.0
atomicReadPVar :: (MonadPrim s m, Atomic e) => PVar e s -> m e
atomicReadPVar (PVar mba#) =
  prim $ atomicReadMutableByteArray# mba# 0#
{-# INLINE atomicReadPVar #-}

-- | Write a value into an `PVar` atomically. Implies a full memory barrier.
--
-- @since 0.1.0
atomicWritePVar :: MonadPrim s m => PVar e s -> Int -> m ()
atomicWritePVar (PVar mba#) a = prim_ (atomicWriteMutableByteArray# mba# 0# a)
{-# INLINE atomicWritePVar #-}


-- | Apply a function to the value of `PVar` atomically. Implies a full memory
-- barrier. Returns the new value.
--
-- @since 0.1.0
atomicModifyFetchOldPVar :: (MonadPrim s m, Atomic e) => PVar e s -> (e -> e) -> m e
atomicModifyFetchOldPVar (PVar mba#) f =
  prim $ atomicModifyFetchOldMutableByteArray# mba# 0# f
{-# INLINE atomicModifyFetchOldPVar #-}

-- | Apply a function to an integer element of a `PVar` atomically. Implies a full memory
-- barrier. Returns the new value.
--
-- @since 0.1.0
atomicModifyFetchNewPVar :: (MonadPrim s m, Atomic e) => PVar e s -> (e -> e) -> m e
atomicModifyFetchNewPVar (PVar mba#) f =
  prim $ atomicModifyFetchNewMutableByteArray# mba# 0# f
{-# INLINE atomicModifyFetchNewPVar #-}


-- | Compare and swap. This is also a function that is used to implement
-- `atomicModifyPVar`. Implies a full memory barrier.
--
-- @since 0.1.0
casPVar ::
     (MonadPrim s m, Atomic e)
  => PVar e s -- ^ Variable to mutate
  -> e -- ^ Old expected value
  -> e -- ^ New value
  -> m e -- ^ Old actual value
casPVar (PVar mba#) old new = prim $ casMutableByteArray# mba# 0# old new
{-# INLINE casPVar #-}


-- | Add a number to mutable variable, corresponds to @(`+`)@ done atomically. Implies
-- a full memory barrier. Returns the previous value.
--
-- @since 0.1.0
atomicAddFetchOldPVar :: (MonadPrim s m, AtomicCount e) => PVar e s -> e -> m e
atomicAddFetchOldPVar (PVar mba#) a = prim $ atomicAddFetchOldMutableByteArray# mba# 0# a
{-# INLINE atomicAddFetchOldPVar #-}

-- | Add a number to mutable variable, corresponds to @(`+`)@ done atomically. Implies
-- a full memory barrier. Returns the new value.
--
-- @since 0.1.0
atomicAddFetchNewPVar :: (MonadPrim s m, AtomicCount e) => PVar e s -> e -> m e
atomicAddFetchNewPVar (PVar mba#) a = prim $ atomicAddFetchNewMutableByteArray# mba# 0# a
{-# INLINE atomicAddFetchNewPVar #-}

-- | Add a number to mutable variable, corresponds to @(`+`)@ done atomically. Implies a
-- full memory barrier.
--
-- @since 0.1.0
(!+) :: (MonadPrim s m, AtomicCount e) => PVar e s -> e -> m ()
(!+) var = void . atomicAddFetchOldPVar var
{-# INLINE (!+) #-}


-- | Subtract a number from mutable variable, corresponds to @(`-`)@ done
-- atomically. Implies a full memory barrier. Returns the previous value
--
-- @since 0.1.0
atomicSubFetchOldPVar :: (MonadPrim s m, AtomicCount e) => PVar e s -> e -> m e
atomicSubFetchOldPVar (PVar mba#) a = prim $ atomicSubFetchOldMutableByteArray# mba# 0# a
{-# INLINE atomicSubFetchOldPVar #-}

-- | Subtract a number from mutable variable, corresponds to @(`-`)@ done
-- atomically. Implies a full memory barrier. Returns the new value
--
-- @since 0.1.0
atomicSubFetchNewPVar :: (MonadPrim s m, AtomicCount e) => PVar e s -> e -> m e
atomicSubFetchNewPVar (PVar mba#) a = prim $ atomicSubFetchNewMutableByteArray# mba# 0# a
{-# INLINE atomicSubFetchNewPVar #-}

-- | Subtract a number from a mutable variable, corresponds to @(`-`)@ done atomically. Implies a
-- full memory barrier.
--
-- @since 0.1.0
(!-) :: (MonadPrim s m, AtomicCount e) => PVar e s -> e -> m ()
(!-) var = void . atomicSubFetchOldPVar var
{-# INLINE (!-) #-}

-- | Binary conjuction (AND), corresponds to @(`Data.Bits..&.`)@ done atomically. Implies
-- a full memory barrier. Returns the previous value.
--
-- @since 0.1.0
atomicAndFetchOldPVar :: (MonadPrim s m, AtomicBits e) => PVar e s -> e -> m e
atomicAndFetchOldPVar (PVar mba#) a = prim $ atomicAndFetchOldMutableByteArray# mba# 0# a
{-# INLINE atomicAndFetchOldPVar #-}


-- | Binary conjuction (AND), corresponds to @(`Data.Bits..&.`)@ done atomically. Implies
-- a full memory barrier. Returns the new value.
--
-- @since 0.1.0
atomicAndFetchNewPVar :: (MonadPrim s m, AtomicBits e) => PVar e s -> e -> m e
atomicAndFetchNewPVar (PVar mba#) a = prim $ atomicAndFetchNewMutableByteArray# mba# 0# a
{-# INLINE atomicAndFetchNewPVar #-}

-- | Binary conjuction (AND), corresponds to @(`Data.Bits..&.`)@ done atomically. Implies
-- a full memory barrier.
--
-- @since 0.1.0
(!&) :: (MonadPrim s m, AtomicBits e) => PVar e s -> e -> m ()
(!&) var = void . atomicAndFetchOldPVar var
{-# INLINE (!&) #-}

-- | Binary negation of conjuction (NAND), corresponds to @\\x y -> `Data.Bits.complement` (x `.&.`
-- y)@ done atomically. Implies a full memory barrier. Returns the previous value.
--
-- @since 0.1.0
atomicNandFetchOldPVar :: (MonadPrim s m, AtomicBits e) => PVar e s -> e -> m e
atomicNandFetchOldPVar (PVar mba#) a = prim $ atomicNandFetchOldMutableByteArray# mba# 0# a
{-# INLINE atomicNandFetchOldPVar #-}


-- | Binary negation of conjuction (NAND), corresponds to @\\x y -> `Data.Bits.complement` (x `.&.`
-- y)@ done atomically. Implies a full memory barrier. Returns the new value.
--
-- @since 0.1.0
atomicNandFetchNewPVar :: (MonadPrim s m, AtomicBits e) => PVar e s -> e -> m e
atomicNandFetchNewPVar (PVar mba#) a = prim $ atomicNandFetchNewMutableByteArray# mba# 0# a
{-# INLINE atomicNandFetchNewPVar #-}

-- | Binary negation of conjuction (NAND), corresponds to @\\x y -> `Data.Bits.complement` (x `.&.`
-- y)@ done atomically. Implies a full memory barrier. Returns the previous value.
--
-- @since 0.1.0
(!~&) :: (MonadPrim s m, AtomicBits e) => PVar e s -> e -> m ()
(!~&) var = void . atomicNandFetchOldPVar var
{-# INLINE (!~&) #-}

-- | Binary disjunction (OR), corresponds to @(`Data.Bits..|.`)@ done atomically. Implies
-- a full memory barrier. Returns the previous value.
--
-- @since 0.1.0
atomicOrFetchOldPVar :: (MonadPrim s m, AtomicBits e) => PVar e s -> e -> m e
atomicOrFetchOldPVar (PVar mba#) a = prim $ atomicOrFetchOldMutableByteArray# mba# 0# a
{-# INLINE atomicOrFetchOldPVar #-}


-- | Binary disjunction (OR), corresponds to @(`Data.Bits..|.`)@ done atomically. Implies
-- a full memory barrier. Returns the new value.
--
-- @since 0.1.0
atomicOrFetchNewPVar :: (MonadPrim s m, AtomicBits e) => PVar e s -> e -> m e
atomicOrFetchNewPVar (PVar mba#) a = prim $ atomicOrFetchNewMutableByteArray# mba# 0# a
{-# INLINE atomicOrFetchNewPVar #-}

-- | Binary disjunction (OR), corresponds to @(`Data.Bits..|.`)@ done atomically. Implies
-- a full memory barrier.
--
-- @since 0.1.0
(!|) :: (MonadPrim s m, AtomicBits e) => PVar e s -> e -> m ()
(!|) var = void . atomicOrFetchOldPVar var
{-# INLINE (!|) #-}

-- | Binary exclusive OR (XOR), corresponds to @(`Data.Bits.xor`)@ done atomically. Implies
-- a full memory barrier. Returns the previous value.
--
-- @since 0.1.0
atomicXorFetchOldPVar :: (MonadPrim s m, AtomicBits e) => PVar e s -> e -> m e
atomicXorFetchOldPVar (PVar mba#) a = prim $ atomicXorFetchOldMutableByteArray# mba# 0# a
{-# INLINE atomicXorFetchOldPVar #-}


-- | Binary exclusive OR (XOR), corresponds to @(`Data.Bits.xor`)@ done atomically. Implies
-- a full memory barrier. Returns the new value.
--
-- @since 0.1.0
atomicXorFetchNewPVar :: (MonadPrim s m, AtomicBits e) => PVar e s -> e -> m e
atomicXorFetchNewPVar (PVar mba#) a = prim $ atomicXorFetchNewMutableByteArray# mba# 0# a
{-# INLINE atomicXorFetchNewPVar #-}

-- | Binary exclusive OR (XOR), corresponds to @(`Data.Bits.xor`)@ done atomically. Implies
-- a full memory barrier.
--
-- @since 0.1.0
(!^) :: (MonadPrim s m, AtomicBits e) => PVar e s -> e -> m ()
(!^) var = void . atomicXorFetchOldPVar var
{-# INLINE (!^) #-}

-- | Binary negation (NOT), corresponds to @(`Data.Bits.complement`)@ done atomically. Implies
-- a full memory barrier. Returns the previous value.
--
-- @since 0.1.0
atomicNotFetchOldPVar :: (MonadPrim s m, AtomicBits e) => PVar e s -> m e
atomicNotFetchOldPVar (PVar mba#) = prim $ atomicNotFetchOldMutableByteArray# mba# 0#
{-# INLINE atomicNotFetchOldPVar #-}


-- | Binary negation (NOT), corresponds to @(`Data.Bits.complement`)@ done atomically. Implies
-- a full memory barrier. Returns the new value.
--
-- @since 0.1.0
atomicNotFetchNewPVar :: (MonadPrim s m, AtomicBits e) => PVar e s -> m e
atomicNotFetchNewPVar (PVar mba#) = prim $ atomicNotFetchNewMutableByteArray# mba# 0#
{-# INLINE atomicNotFetchNewPVar #-}

-- | Binary negation (NOT), corresponds to @(`Data.Bits.complement`)@ done atomically. Implies
-- a full memory barrier.
--
-- @since 0.1.0
(!~) :: (MonadPrim s m, AtomicBits e) => PVar e s -> m ()
(!~) var = void $ atomicNotFetchOldPVar var
{-# INLINE (!~) #-}
