{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.Prim.MRef.Atomic
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.MRef.Atomic
  ( AtomicMRef(..)
  , AtomicCountMRef(..)
  , AtomicBitsMRef(..)
  , atomicModifyMRef_
  ) where

import Control.Prim.Monad
import Data.Bits
import Data.Prim.MRef.Internal
import Data.Prim.Memory.Addr
import Data.Prim.Memory.PArray
import Data.Prim.Memory.Bytes



class MRef mut => AtomicMRef mut where

  atomicReadMRef ::
    MonadPrim s m
    => mut s -- ^ Mutable variable to read an element from
    -> m (Elt mut)
  atomicReadMRef mut = atomicModifyMRef mut (\x -> (x, x))
  {-# INLINE atomicReadMRef #-}

  -- | Write an element into `MutableByteArray#` atomically. Implies full memory barrier.
  atomicWriteMRef ::
       MonadPrim s m
    => mut s -- ^ Mutable variable to write an element into
    -> Elt mut -- ^ Element to write
    -> m ()
  atomicWriteMRef mut !y = atomicModifyMRef mut (const (y, ()))
  {-# INLINE atomicWriteMRef #-}

  -- | Compare-and-swap (CAS) operation. Given a mutable array, offset in number of
  -- elements, an old value and a new value atomically swap the old value for the new one,
  -- but only if the actual old value was an exact match to the expetced old one that was
  -- supplied. Returns the actual old value, which if compared to supplied expected one
  -- will tell us whether atomic swap occured or not.
  casMRef ::
       MonadPrim s m
    => mut s -- ^ Variable to be mutated
    -> Elt mut -- ^ Expected old value
    -> Elt mut -- ^ New value
    -> m (Bool, Elt mut) -- ^ Was compare and swap successfull and the actual value

  -- | Perform atomic an modification of an element in a mutable structure.
  --
  -- @since 0.1.0
  atomicModifyMRef ::
       MonadPrim s m
    => mut s -- ^ Variable to be mutated
    -> (Elt mut -> (Elt mut, b)) -- ^ Function to be applied atomically to the element
    -> m b
  atomicModifyMRef mut f =
    let go expected =
          case f expected of
            (new, artifact) -> do
              (isCasSucc, actual) <- casMRef mut expected new
              if isCasSucc
                then pure artifact
                else go actual
     in readMRef mut >>= go
  {-# INLINE atomicModifyMRef #-}

  -- | Perform atomic modification of an element in a mutable structure and return the
  -- previous value.
  --
  -- @since 0.1.0
  atomicModifyFetchOldMRef ::
       MonadPrim s m
    => mut s -- ^ Variable to be mutated
    -> (Elt mut -> Elt mut) -- ^ Function to be applied atomically to the element
    -> m (Elt mut)
  atomicModifyFetchOldMRef mut f = atomicModifyMRef mut (\ x -> let x' = f x in (x', x))
  {-# INLINE atomicModifyFetchOldMRef #-}

  -- | Perform atomic modification of an element in a mutable structure and return the
  -- new value.
  --
  -- @since 0.1.0
  atomicModifyFetchNewMRef ::
       MonadPrim s m
    => mut s -- ^ Variable to be mutated
    -> (Elt mut -> Elt mut) -- ^ Function to be applied atomically to the element
    -> m (Elt mut)
  atomicModifyFetchNewMRef mut f = atomicModifyMRef mut (\ x -> let x' = f x in (x', x'))
  {-# INLINE atomicModifyFetchNewMRef #-}



class (Num (Elt mut), AtomicMRef mut) => AtomicCountMRef mut where
  atomicAddFetchOldMRef :: MonadPrim s m => mut s -> Elt mut -> m (Elt mut)
  atomicAddFetchOldMRef mut !y = atomicModifyFetchOldMRef mut (+ y)
  {-# INLINE atomicAddFetchOldMRef #-}

  atomicAddFetchNewMRef :: MonadPrim s m => mut s -> Elt mut -> m (Elt mut)
  atomicAddFetchNewMRef mut !y = atomicModifyFetchNewMRef mut (+ y)
  {-# INLINE atomicAddFetchNewMRef #-}

  atomicSubFetchOldMRef :: MonadPrim s m => mut s -> Elt mut -> m (Elt mut)
  atomicSubFetchOldMRef mut !y = atomicModifyFetchOldMRef mut (subtract y)
  {-# INLINE atomicSubFetchOldMRef #-}

  atomicSubFetchNewMRef :: MonadPrim s m => mut s -> Elt mut -> m (Elt mut)
  atomicSubFetchNewMRef mut !y = atomicModifyFetchNewMRef mut (subtract y)
  {-# INLINE atomicSubFetchNewMRef #-}


class (Bits (Elt mut), AtomicMRef mut) => AtomicBitsMRef mut where
  atomicAndFetchOldMRef :: MonadPrim s m => mut s -> Elt mut -> m (Elt mut)
  atomicAndFetchOldMRef mut !y = atomicModifyFetchOldMRef mut (.&. y)
  {-# INLINE atomicAndFetchOldMRef #-}

  atomicAndFetchNewMRef :: MonadPrim s m => mut s -> Elt mut -> m (Elt mut)
  atomicAndFetchNewMRef mut !y = atomicModifyFetchNewMRef mut (.&. y)
  {-# INLINE atomicAndFetchNewMRef #-}

  atomicNandFetchOldMRef :: MonadPrim s m => mut s -> Elt mut -> m (Elt mut)
  atomicNandFetchOldMRef mut !y = atomicModifyFetchOldMRef mut (\x -> complement (x .&. y))
  {-# INLINE atomicNandFetchOldMRef #-}

  atomicNandFetchNewMRef :: MonadPrim s m => mut s -> Elt mut -> m (Elt mut)
  atomicNandFetchNewMRef mut !y = atomicModifyFetchNewMRef mut (\x -> complement (x .&. y))
  {-# INLINE atomicNandFetchNewMRef #-}

  atomicOrFetchOldMRef :: MonadPrim s m => mut s -> Elt mut -> m (Elt mut)
  atomicOrFetchOldMRef mut !y = atomicModifyFetchOldMRef mut (.|. y)
  {-# INLINE atomicOrFetchOldMRef #-}

  atomicOrFetchNewMRef :: MonadPrim s m => mut s -> Elt mut -> m (Elt mut)
  atomicOrFetchNewMRef mut !y = atomicModifyFetchNewMRef mut (.|. y)
  {-# INLINE atomicOrFetchNewMRef #-}

  atomicXorFetchOldMRef :: MonadPrim s m => mut s -> Elt mut -> m (Elt mut)
  atomicXorFetchOldMRef mut !y = atomicModifyFetchOldMRef mut (`xor` y)
  {-# INLINE atomicXorFetchOldMRef #-}

  atomicXorFetchNewMRef :: MonadPrim s m => mut s -> Elt mut -> m (Elt mut)
  atomicXorFetchNewMRef mut !y = atomicModifyFetchNewMRef mut (`xor` y)
  {-# INLINE atomicXorFetchNewMRef #-}

  atomicNotFetchOldMRef :: MonadPrim s m => mut s -> m (Elt mut)
  atomicNotFetchOldMRef mut = atomicModifyFetchOldMRef mut complement
  {-# INLINE atomicNotFetchOldMRef #-}

  atomicNotFetchNewMRef :: MonadPrim s m => mut s -> m (Elt mut)
  atomicNotFetchNewMRef mut = atomicModifyFetchNewMRef mut complement
  {-# INLINE atomicNotFetchNewMRef #-}



instance Typeable p => AtomicMRef (MBytes p) where
  atomicReadMRef mb = atomicReadMBytes mb (0 :: Off Word8)
  {-# INLINE atomicReadMRef #-}
  atomicWriteMRef mb = atomicWriteMBytes mb (0 :: Off Word8)
  {-# INLINE atomicWriteMRef #-}
  casMRef mb = casBoolFetchMBytes mb (0 :: Off Word8)
  {-# INLINE casMRef #-}
  atomicModifyMRef mb = atomicModifyMBytes mb (0 :: Off Word8)
  {-# INLINE atomicModifyMRef #-}


instance Typeable p => AtomicCountMRef (MBytes p) where
  atomicAddFetchOldMRef mb = atomicAddFetchOldMBytes mb (0 :: Off Word8)
  {-# INLINE atomicAddFetchOldMRef #-}
  atomicAddFetchNewMRef mb = atomicAddFetchNewMBytes mb (0 :: Off Word8)
  {-# INLINE atomicAddFetchNewMRef #-}
  atomicSubFetchOldMRef mb = atomicSubFetchOldMBytes mb (0 :: Off Word8)
  {-# INLINE atomicSubFetchOldMRef #-}
  atomicSubFetchNewMRef mb = atomicSubFetchNewMBytes mb (0 :: Off Word8)
  {-# INLINE atomicSubFetchNewMRef #-}


instance Typeable p => AtomicBitsMRef (MBytes p) where
  atomicAndFetchOldMRef mb = atomicAndFetchOldMBytes mb (0 :: Off Word8)
  {-# INLINE atomicAndFetchOldMRef #-}
  atomicAndFetchNewMRef mb = atomicAndFetchNewMBytes mb (0 :: Off Word8)
  {-# INLINE atomicAndFetchNewMRef #-}
  atomicNandFetchOldMRef mb = atomicNandFetchOldMBytes mb (0 :: Off Word8)
  {-# INLINE atomicNandFetchOldMRef #-}
  atomicNandFetchNewMRef mb = atomicNandFetchNewMBytes mb (0 :: Off Word8)
  {-# INLINE atomicNandFetchNewMRef #-}
  atomicOrFetchOldMRef mb = atomicOrFetchOldMBytes mb (0 :: Off Word8)
  {-# INLINE atomicOrFetchOldMRef #-}
  atomicOrFetchNewMRef mb = atomicOrFetchNewMBytes mb (0 :: Off Word8)
  {-# INLINE atomicOrFetchNewMRef #-}
  atomicXorFetchOldMRef mb = atomicXorFetchOldMBytes mb (0 :: Off Word8)
  {-# INLINE atomicXorFetchOldMRef #-}
  atomicXorFetchNewMRef mb = atomicXorFetchNewMBytes mb (0 :: Off Word8)
  {-# INLINE atomicXorFetchNewMRef #-}
  atomicNotFetchOldMRef mb = atomicNotFetchOldMBytes mb (0 :: Off Word8)
  {-# INLINE atomicNotFetchOldMRef #-}
  atomicNotFetchNewMRef mb = atomicNotFetchNewMBytes mb (0 :: Off Word8)
  {-# INLINE atomicNotFetchNewMRef #-}

instance (Typeable p, Atomic e) => AtomicMRef (PMArray p e) where
  atomicReadMRef mba = atomicReadMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicReadMRef #-}
  atomicWriteMRef mba = atomicWriteMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicWriteMRef #-}
  casMRef mba = casBoolFetchMBytes (coerce mba) (0 :: Off e)
  {-# INLINE casMRef #-}
  atomicModifyMRef mba = atomicModifyMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicModifyMRef #-}


instance (Typeable p, Num e, AtomicCount e) => AtomicCountMRef (PMArray p e) where
  atomicAddFetchOldMRef mba = atomicAddFetchOldMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicAddFetchOldMRef #-}
  atomicAddFetchNewMRef mba = atomicAddFetchNewMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicAddFetchNewMRef #-}
  atomicSubFetchOldMRef mba = atomicSubFetchOldMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicSubFetchOldMRef #-}
  atomicSubFetchNewMRef mba = atomicSubFetchNewMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicSubFetchNewMRef #-}


instance (Typeable p, Bits e, AtomicBits e) => AtomicBitsMRef (PMArray p e) where
  atomicAndFetchOldMRef mba = atomicAndFetchOldMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicAndFetchOldMRef #-}
  atomicAndFetchNewMRef mba = atomicAndFetchNewMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicAndFetchNewMRef #-}
  atomicNandFetchOldMRef mba = atomicNandFetchOldMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicNandFetchOldMRef #-}
  atomicNandFetchNewMRef mba = atomicNandFetchNewMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicNandFetchNewMRef #-}
  atomicOrFetchOldMRef mba = atomicOrFetchOldMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicOrFetchOldMRef #-}
  atomicOrFetchNewMRef mba = atomicOrFetchNewMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicOrFetchNewMRef #-}
  atomicXorFetchOldMRef mba = atomicXorFetchOldMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicXorFetchOldMRef #-}
  atomicXorFetchNewMRef mba = atomicXorFetchNewMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicXorFetchNewMRef #-}
  atomicNotFetchOldMRef mba = atomicNotFetchOldMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicNotFetchOldMRef #-}
  atomicNotFetchNewMRef mba = atomicNotFetchNewMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicNotFetchNewMRef #-}


instance Atomic e => AtomicMRef (MAddr e) where
  atomicReadMRef maddr = atomicReadOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicReadMRef #-}
  atomicWriteMRef maddr = atomicWriteOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicWriteMRef #-}
  casMRef maddr = casBoolFetchOffMAddr maddr (0 :: Off e)
  {-# INLINE casMRef #-}
  atomicModifyMRef maddr = atomicModifyOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicModifyMRef #-}


instance (Num e, AtomicCount e) => AtomicCountMRef (MAddr e) where
  atomicAddFetchOldMRef maddr = atomicAddFetchOldOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicAddFetchOldMRef #-}
  atomicAddFetchNewMRef maddr = atomicAddFetchNewOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicAddFetchNewMRef #-}
  atomicSubFetchOldMRef maddr = atomicSubFetchOldOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicSubFetchOldMRef #-}
  atomicSubFetchNewMRef maddr = atomicSubFetchNewOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicSubFetchNewMRef #-}


instance (Bits e, AtomicBits e) => AtomicBitsMRef (MAddr e) where
  atomicAndFetchOldMRef maddr = atomicAndFetchOldOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicAndFetchOldMRef #-}
  atomicAndFetchNewMRef maddr = atomicAndFetchNewOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicAndFetchNewMRef #-}
  atomicNandFetchOldMRef maddr = atomicNandFetchOldOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicNandFetchOldMRef #-}
  atomicNandFetchNewMRef maddr = atomicNandFetchNewOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicNandFetchNewMRef #-}
  atomicOrFetchOldMRef maddr = atomicOrFetchOldOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicOrFetchOldMRef #-}
  atomicOrFetchNewMRef maddr = atomicOrFetchNewOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicOrFetchNewMRef #-}
  atomicXorFetchOldMRef maddr = atomicXorFetchOldOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicXorFetchOldMRef #-}
  atomicXorFetchNewMRef maddr = atomicXorFetchNewOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicXorFetchNewMRef #-}
  atomicNotFetchOldMRef maddr = atomicNotFetchOldOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicNotFetchOldMRef #-}
  atomicNotFetchNewMRef maddr = atomicNotFetchNewOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicNotFetchNewMRef #-}


atomicModifyMRef_ :: (AtomicMRef mut, MonadPrim s m) => mut s -> (Elt mut -> Elt mut) -> m ()
atomicModifyMRef_ ref f = atomicModifyMRef ref $ \e -> (f e, ())
{-# INLINE atomicModifyMRef_ #-}
