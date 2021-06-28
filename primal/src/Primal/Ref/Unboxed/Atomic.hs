{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Primal.Ref.Unboxed.Atomic
-- Copyright   : (c) Alexey Kuleshevich 2020-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Ref.Unboxed.Atomic
  ( atomicModifyURef
  , atomicModifyURef_
  , atomicModifyFetchOldURef
  , atomicModifyFetchNewURef
  , atomicReadURef
  , atomicWriteURef
  , atomicAddFetchOldURef
  , atomicAddFetchNewURef
  , atomicSubFetchOldURef
  , atomicSubFetchNewURef
  , atomicAndFetchOldURef
  , atomicAndFetchNewURef
  , atomicNandFetchOldURef
  , atomicNandFetchNewURef
  , atomicOrFetchOldURef
  , atomicOrFetchNewURef
  , atomicXorFetchOldURef
  , atomicXorFetchNewURef
  , atomicNotFetchOldURef
  , atomicNotFetchNewURef
  , casURef
  ) where

import Primal.Monad
import Primal.Unbox
import Primal.Unbox.Atomic
import Primal.Ref.Unboxed


-- | Apply a function to an element of a `URef` atomically. Implies a full memory
-- barrier. Returns some arbitrary artifact of computation.
--
-- @since 1.0.0
atomicModifyURef ::
     forall e a m s. (Atomic e, Primal s m)
  => URef e s
  -> (e -> (e, a))
  -> m a
atomicModifyURef (URef mba#) f = primal (atomicModifyMutableByteArray# mba# 0# g)
  where
    g e =
      case f e of
        (e', a) -> (# e', a #)
    {-# INLINE g #-}
{-# INLINE atomicModifyURef #-}


-- | Apply a function to the contents of a `URef` atomically. Implies a full memory
-- barrier.
--
-- @since 1.0.0
atomicModifyURef_ ::
     forall e m s. (Atomic e, Primal s m)
  => URef e s
  -> (e -> e)
  -> m ()
atomicModifyURef_ (URef mba#) f = primal_ (atomicModifyMutableByteArray_# mba# 0# f)
{-# INLINE atomicModifyURef_ #-}


-- | Read a value from `URef` atomically. Implies a full memory barrier.
--
-- @since 1.0.0
atomicReadURef ::
     forall e m s. (Atomic e, Primal s m)
  => URef e s
  -> m e
atomicReadURef (URef mba#) = primal $ atomicReadMutableByteArray# mba# 0#
{-# INLINE atomicReadURef #-}


-- | Write a value into an `URef` atomically. Implies a full memory barrier.
--
-- @since 1.0.0
atomicWriteURef ::
     forall e m s. (Atomic e, Primal s m)
  => URef e s
  -> e
  -> m ()
atomicWriteURef (URef mba#) a = primal_ (atomicWriteMutableByteArray# mba# 0# a)
{-# INLINE atomicWriteURef #-}


-- | Apply a function to the value of a `URef` atomically. Implies a full memory
-- barrier. Returns the new value.
--
-- @since 1.0.0
atomicModifyFetchNewURef ::
     forall e m s. (Atomic e, Primal s m)
  => URef e s
  -> (e -> e)
  -> m e
atomicModifyFetchNewURef (URef mba#) f = primal $ atomicModifyFetchNewMutableByteArray# mba# 0# f
{-# INLINE atomicModifyFetchNewURef #-}


-- | Apply a function to the value of a `URef` atomically. Implies a full memory
-- barrier. Returns the previous value.
--
-- @since 1.0.0
atomicModifyFetchOldURef ::
     forall e m s. (Atomic e, Primal s m)
  => URef e s
  -> (e -> e)
  -> m e
atomicModifyFetchOldURef (URef mba#) f = primal $ atomicModifyFetchOldMutableByteArray# mba# 0# f
{-# INLINE atomicModifyFetchOldURef #-}


-- | Compare and swap. Implies a full memory barrier. There is no `Eq` constraint because
-- comparison is done on the binary in memory representation of two elements.
--
-- @since 1.0.0
casURef ::
     forall e m s. (Atomic e, Primal s m)
  => URef e s -- ^ Variable to mutate
  -> e -- ^ Old expected value
  -> e -- ^ New value
  -> m e -- ^ Old actual value
casURef (URef mba#) old new = primal $ casMutableByteArray# mba# 0# old new
{-# INLINE casURef #-}


-- | Add two numbers, corresponds to @(`+`)@ done atomically. Returns the previous value of
-- the mutable variable. Implies a full memory barrier.
--
-- @since 1.0.0
atomicAddFetchOldURef ::
     forall e m s. (AtomicCount e, Primal s m)
  => URef e s
  -> e
  -> m e
atomicAddFetchOldURef (URef mba#) a = primal $ atomicAddFetchOldMutableByteArray# mba# 0# a
{-# INLINE atomicAddFetchOldURef #-}

-- | Add two numbers, corresponds to @(`+`)@ done atomically. Returns the new value of
-- the mutable variable. Implies a full memory barrier.
--
-- @since 1.0.0
atomicAddFetchNewURef ::
     forall e m s. (AtomicCount e, Primal s m)
  => URef e s
  -> e
  -> m e
atomicAddFetchNewURef (URef mba#) a = primal $ atomicAddFetchNewMutableByteArray# mba# 0# a
{-# INLINE atomicAddFetchNewURef #-}


-- | Subtract two numbers, corresponds to @(`-`)@ done atomically. Returns the
-- previous value of the mutable variable. Implies a full memory barrier.
--
-- @since 1.0.0
atomicSubFetchOldURef ::
     forall e m s. (AtomicCount e, Primal s m)
  => URef e s
  -> e
  -> m e
atomicSubFetchOldURef (URef mba#) a = primal $ atomicSubFetchOldMutableByteArray# mba# 0# a
{-# INLINE atomicSubFetchOldURef #-}


-- | Subtract two numbers, corresponds to @(`-`)@ done atomically. Returns the
-- new value of the mutable variable. Implies a full memory barrier.
--
-- @since 1.0.0
atomicSubFetchNewURef ::
     forall e m s. (AtomicCount e, Primal s m)
  => URef e s
  -> e
  -> m e
atomicSubFetchNewURef (URef mba#) a = primal $ atomicSubFetchNewMutableByteArray# mba# 0# a
{-# INLINE atomicSubFetchNewURef #-}


-- | Binary conjuction (AND), corresponds to @(`Data.Bits..&.`)@ done atomically. Returns
-- the previous value of the mutable variable. Implies a full memory barrier.
--
-- @since 1.0.0
atomicAndFetchOldURef ::
     forall e m s. (AtomicBits e, Primal s m)
  => URef e s
  -> e
  -> m e
atomicAndFetchOldURef (URef mba#) a = primal $ atomicAndFetchOldMutableByteArray# mba# 0# a
{-# INLINE atomicAndFetchOldURef #-}


-- | Binary conjuction (AND), corresponds to @(`Data.Bits..&.`)@ done atomically. Returns
-- the new value of the mutable variable. Implies a full memory barrier.
--
-- @since 1.0.0
atomicAndFetchNewURef ::
     forall e m s. (AtomicBits e, Primal s m)
  => URef e s
  -> e
  -> m e
atomicAndFetchNewURef (URef mba#) a = primal $ atomicAndFetchNewMutableByteArray# mba# 0# a
{-# INLINE atomicAndFetchNewURef #-}


-- | Binary negation of conjuction (NAND), corresponds to @\\x y -> `Data.Bits.complement` (x
-- `Data.Bits..&.` y)@ done atomically. Returns the previous value of the mutable variable. Implies
-- a full memory barrier.
--
-- @since 1.0.0
atomicNandFetchOldURef ::
     forall e m s. (AtomicBits e, Primal s m)
  => URef e s
  -> e
  -> m e
atomicNandFetchOldURef (URef mba#) a = primal $ atomicNandFetchOldMutableByteArray# mba# 0# a
{-# INLINE atomicNandFetchOldURef #-}


-- | Binary negation of conjuction (NAND), corresponds to @\\x y -> `Data.Bits.complement` (x
-- `Data.Bits..&.` y)@ done atomically. Returns the new value of the mutable variable. Implies
-- a full memory barrier.
--
-- @since 1.0.0
atomicNandFetchNewURef ::
     forall e m s. (AtomicBits e, Primal s m)
  => URef e s
  -> e
  -> m e
atomicNandFetchNewURef (URef mba#) a = primal $ atomicNandFetchNewMutableByteArray# mba# 0# a
{-# INLINE atomicNandFetchNewURef #-}


-- | Binary disjunction (OR), corresponds to @(`Data.Bits..|.`)@ done atomically. Returns the previous
-- value of the mutable variable. Implies a full memory barrier.
--
-- @since 1.0.0
atomicOrFetchOldURef ::
     forall e m s. (AtomicBits e, Primal s m)
  => URef e s
  -> e
  -> m e
atomicOrFetchOldURef (URef mba#) a = primal $ atomicOrFetchOldMutableByteArray# mba# 0# a
{-# INLINE atomicOrFetchOldURef #-}


-- | Binary disjunction (OR), corresponds to @(`Data.Bits..|.`)@ done atomically. Returns the new
-- value of the mutable variable. Implies a full memory barrier.
--
-- @since 1.0.0
atomicOrFetchNewURef ::
     forall e m s. (AtomicBits e, Primal s m)
  => URef e s
  -> e
  -> m e
atomicOrFetchNewURef (URef mba#) a = primal $ atomicOrFetchNewMutableByteArray# mba# 0# a
{-# INLINE atomicOrFetchNewURef #-}


-- | Binary exclusive disjunction (XOR), corresponds to @`Data.Bits.xor`@ done atomically. Returns the
-- previous value of the mutable variable. Implies a full memory barrier.
--
-- @since 1.0.0
atomicXorFetchOldURef ::
     forall e m s. (AtomicBits e, Primal s m)
  => URef e s
  -> e
  -> m e
atomicXorFetchOldURef (URef mba#) a = primal $ atomicXorFetchOldMutableByteArray# mba# 0# a
{-# INLINE atomicXorFetchOldURef #-}


-- | Binary exclusive disjunction (XOR), corresponds to @`Data.Bits.xor`@ done atomically. Returns the
-- previous value of the mutable variable. Implies a full memory barrier.
--
-- @since 1.0.0
atomicXorFetchNewURef ::
     forall e m s. (AtomicBits e, Primal s m)
  => URef e s
  -> e
  -> m e
atomicXorFetchNewURef (URef mba#) a = primal $ atomicXorFetchNewMutableByteArray# mba# 0# a
{-# INLINE atomicXorFetchNewURef #-}


-- | Binary negation (NOT), corresponds to ones' @`Data.Bits.complement`@ done atomically. Returns the
-- previous value of the mutable variable. Implies a full memory barrier.
--
-- @since 1.0.0
atomicNotFetchOldURef ::
     forall e m s. (AtomicBits e, Primal s m)
  => URef e s
  -> m e
atomicNotFetchOldURef (URef mba#) = primal $ atomicNotFetchOldMutableByteArray# mba# 0#
{-# INLINE atomicNotFetchOldURef #-}


-- | Binary negation (NOT), corresponds to ones' @`Data.Bits.complement`@ done atomically. Returns the
-- new value of the mutable variable. Implies a full memory barrier.
--
-- @since 1.0.0
atomicNotFetchNewURef ::
     forall e m s. (AtomicBits e, Primal s m)
  => URef e s
  -> m e
atomicNotFetchNewURef (URef mba#) = primal $ atomicNotFetchNewMutableByteArray# mba# 0#
{-# INLINE atomicNotFetchNewURef #-}
