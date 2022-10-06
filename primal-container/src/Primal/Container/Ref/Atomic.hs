{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-- |
-- Module      : Primal.Container.Ref.Atomic
-- Copyright   : (c) Alexey Kuleshevich 2020-2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Container.Ref.Atomic
  ( AtomicMutRef(..)
  , AtomicCountMutRef(..)
  , AtomicBitsMutRef(..)
  , atomicReadMutRef
  , atomicWriteMutRef
  , casMutRef
  , atomicModifyMutRef
  , atomicModifyFetchOldMutRef
  , atomicModifyFetchNewMutRef
  , atomicAddFetchOldMutRef
  , atomicAddFetchNewMutRef
  , atomicSubFetchOldMutRef
  , atomicSubFetchNewMutRef
  , atomicAndFetchOldMutRef
  , atomicAndFetchNewMutRef
  , atomicNandFetchOldMutRef
  , atomicNandFetchNewMutRef
  , atomicOrFetchOldMutRef
  , atomicOrFetchNewMutRef
  , atomicXorFetchOldMutRef
  , atomicXorFetchNewMutRef
  , atomicNotFetchOldMutRef
  , atomicNotFetchNewMutRef
  , atomicModifyMutRef_
  ) where

import Data.Bits
import Primal.Ref
import Primal.Array
import Primal.Monad
import Primal.Container.Internal
import Primal.Container.Ref.Internal
import Primal.Memory.Addr
import Primal.Memory.FAddr
import Primal.Memory.Bytes
import Primal.Memory.PUArray
import GHC.Exts


class MutRef mr => AtomicMutRef mr where
  type AtomicElt mr e :: Constraint
  type AtomicElt mr e = ()

  atomicReadMutRefST ::
    AtomicElt mr e
    => mr e s -- ^ Mutable variable to read an element from
    -> ST s e
  atomicReadMutRefST mut = atomicModifyMutRefST mut (\x -> (x, x))
  {-# INLINE atomicReadMutRefST #-}

  -- | Write an element into `MutableByteArray#` atomically. Implies full memory barrier.
  atomicWriteMutRefST ::
       AtomicElt mr e
    => mr e s -- ^ Mutable variable to write an element into
    -> e -- ^ Element to write
    -> ST s ()
  atomicWriteMutRefST mut !y = atomicModifyMutRefST mut (const (y, ()))
  {-# INLINE atomicWriteMutRefST #-}

  -- | Compare-and-swap (CAS) operation. Given a mutable array, offset in number of
  -- elements, an old value and a new value atomically swap the old value for the new one,
  -- but only if the actual old value was an exact match to the expetced old one that was
  -- supplied. Returns the actual old value, which if compared to supplied expected one
  -- will tell us whether atomic swap occured or not.
  casMutRefST ::
       AtomicElt mr e
    => mr e s -- ^ Variable to be mutated
    -> e -- ^ Expected old value
    -> e -- ^ New value
    -> ST s (Bool, e) -- ^ Was compare and swap successfull and the actual value

  -- | Perform atomic an modification of an element in a mutable structure.
  --
  -- @since 1.0.0
  atomicModifyMutRefST ::
       AtomicElt mr e
    => mr e s -- ^ Variable to be mutated
    -> (e -> (e, b)) -- ^ Function to be applied atomically to the element
    -> ST s b
  default atomicModifyMutRefST ::
       (AtomicElt mr e, Elt mr e)
    => mr e s -- ^ Variable to be mutated
    -> (e -> (e, b)) -- ^ Function to be applied atomically to the element
    -> ST s b
  atomicModifyMutRefST mut f =
    let go expected =
          case f expected of
            (new, artifact) -> do
              (isCasSucc, actual) <- casMutRefST mut expected new
              if isCasSucc
                then pure artifact
                else go actual
     in readMutRefST mut >>= go
  {-# INLINE atomicModifyMutRefST #-}

  -- | Perform atomic modification of an element in a mutable structure and return the
  -- previous value.
  --
  -- @since 1.0.0
  atomicModifyFetchOldMutRefST ::
       AtomicElt mr e
    => mr e s -- ^ Variable to be mutated
    -> (e -> e) -- ^ Function to be applied atomically to the element
    -> ST s e
  atomicModifyFetchOldMutRefST mut f = atomicModifyMutRefST mut (\ x -> let x' = f x in (x', x))
  {-# INLINE atomicModifyFetchOldMutRefST #-}

  -- | Perform atomic modification of an element in a mutable structure and return the
  -- new value.
  --
  -- @since 1.0.0
  atomicModifyFetchNewMutRefST ::
       AtomicElt mr e
    => mr e s -- ^ Variable to be mutated
    -> (e -> e) -- ^ Function to be applied atomically to the element
    -> ST s e
  atomicModifyFetchNewMutRefST mut f = atomicModifyMutRefST mut (\ x -> let x' = f x in (x', x'))
  {-# INLINE atomicModifyFetchNewMutRefST #-}



class AtomicMutRef mr => AtomicCountMutRef mr where
  type AtomicCountElt mr e :: Constraint
  type AtomicCountElt mr e = ()
  atomicAddFetchOldMutRefST :: AtomicCountElt mr e => mr e s -> e -> ST s e
  default atomicAddFetchOldMutRefST
    :: (AtomicCountElt mr e, AtomicElt mr e, Num e) => mr e s -> e -> ST s e
  atomicAddFetchOldMutRefST mut !y = atomicModifyFetchOldMutRefST mut (+ y)
  {-# INLINE atomicAddFetchOldMutRefST #-}

  atomicAddFetchNewMutRefST :: AtomicCountElt mr e => mr e s -> e -> ST s e
  default atomicAddFetchNewMutRefST
    :: (AtomicCountElt mr e, AtomicElt mr e, Num e) => mr e s -> e -> ST s e
  atomicAddFetchNewMutRefST mut !y = atomicModifyFetchNewMutRefST mut (+ y)
  {-# INLINE atomicAddFetchNewMutRefST #-}

  atomicSubFetchOldMutRefST :: AtomicCountElt mr e => mr e s -> e -> ST s e
  default atomicSubFetchOldMutRefST
    :: (AtomicCountElt mr e, AtomicElt mr e, Num e) => mr e s -> e -> ST s e
  atomicSubFetchOldMutRefST mut !y = atomicModifyFetchOldMutRefST mut (subtract y)
  {-# INLINE atomicSubFetchOldMutRefST #-}

  atomicSubFetchNewMutRefST :: AtomicCountElt mr e => mr e s -> e -> ST s e
  default atomicSubFetchNewMutRefST
    :: (AtomicCountElt mr e, AtomicElt mr e, Num e) => mr e s -> e -> ST s e
  atomicSubFetchNewMutRefST mut !y = atomicModifyFetchNewMutRefST mut (subtract y)
  {-# INLINE atomicSubFetchNewMutRefST #-}


class AtomicMutRef mr => AtomicBitsMutRef mr where
  type AtomicBitsElt mr e :: Constraint
  type AtomicBitsElt mr e = ()
  atomicAndFetchOldMutRefST :: AtomicBitsElt mr e => mr e s -> e -> ST s e
  default atomicAndFetchOldMutRefST
    :: (AtomicBitsElt mr e, AtomicElt mr e, Bits e) => mr e s -> e -> ST s e
  atomicAndFetchOldMutRefST mut !y = atomicModifyFetchOldMutRefST mut (.&. y)
  {-# INLINE atomicAndFetchOldMutRefST #-}

  atomicAndFetchNewMutRefST :: AtomicBitsElt mr e => mr e s -> e -> ST s e
  default atomicAndFetchNewMutRefST
    :: (AtomicBitsElt mr e, AtomicElt mr e, Bits e) => mr e s -> e -> ST s e
  atomicAndFetchNewMutRefST mut !y = atomicModifyFetchNewMutRefST mut (.&. y)
  {-# INLINE atomicAndFetchNewMutRefST #-}

  atomicNandFetchOldMutRefST :: AtomicBitsElt mr e => mr e s -> e -> ST s e
  default atomicNandFetchOldMutRefST
    :: (AtomicBitsElt mr e, AtomicElt mr e, Bits e) => mr e s -> e -> ST s e
  atomicNandFetchOldMutRefST mut !y = atomicModifyFetchOldMutRefST mut (\x -> complement (x .&. y))
  {-# INLINE atomicNandFetchOldMutRefST #-}

  atomicNandFetchNewMutRefST :: AtomicBitsElt mr e => mr e s -> e -> ST s e
  default atomicNandFetchNewMutRefST
    :: (AtomicBitsElt mr e, AtomicElt mr e, Bits e) => mr e s -> e -> ST s e
  atomicNandFetchNewMutRefST mut !y = atomicModifyFetchNewMutRefST mut (\x -> complement (x .&. y))
  {-# INLINE atomicNandFetchNewMutRefST #-}

  atomicOrFetchOldMutRefST :: AtomicBitsElt mr e => mr e s -> e -> ST s e
  default atomicOrFetchOldMutRefST
    :: (AtomicBitsElt mr e, AtomicElt mr e, Bits e) => mr e s -> e -> ST s e
  atomicOrFetchOldMutRefST mut !y = atomicModifyFetchOldMutRefST mut (.|. y)
  {-# INLINE atomicOrFetchOldMutRefST #-}

  atomicOrFetchNewMutRefST :: AtomicBitsElt mr e => mr e s -> e -> ST s e
  default atomicOrFetchNewMutRefST
    :: (AtomicBitsElt mr e, AtomicElt mr e, Bits e) => mr e s -> e -> ST s e
  atomicOrFetchNewMutRefST mut !y = atomicModifyFetchNewMutRefST mut (.|. y)
  {-# INLINE atomicOrFetchNewMutRefST #-}

  atomicXorFetchOldMutRefST :: AtomicBitsElt mr e => mr e s -> e -> ST s e
  default atomicXorFetchOldMutRefST
    :: (AtomicBitsElt mr e, AtomicElt mr e, Bits e) => mr e s -> e -> ST s e
  atomicXorFetchOldMutRefST mut !y = atomicModifyFetchOldMutRefST mut (`xor` y)
  {-# INLINE atomicXorFetchOldMutRefST #-}

  atomicXorFetchNewMutRefST :: AtomicBitsElt mr e => mr e s -> e -> ST s e
  default atomicXorFetchNewMutRefST
    :: (AtomicBitsElt mr e, AtomicElt mr e, Bits e) => mr e s -> e -> ST s e
  atomicXorFetchNewMutRefST mut !y = atomicModifyFetchNewMutRefST mut (`xor` y)
  {-# INLINE atomicXorFetchNewMutRefST #-}

  atomicNotFetchOldMutRefST :: AtomicBitsElt mr e => mr e s -> ST s e
  default atomicNotFetchOldMutRefST
    :: (AtomicBitsElt mr e, AtomicElt mr e, Bits e) => mr e s -> ST s e
  atomicNotFetchOldMutRefST mut = atomicModifyFetchOldMutRefST mut complement
  {-# INLINE atomicNotFetchOldMutRefST #-}

  atomicNotFetchNewMutRefST :: AtomicBitsElt mr e => mr e s -> ST s e
  default atomicNotFetchNewMutRefST
    :: (AtomicBitsElt mr e, AtomicElt mr e, Bits e) => mr e s -> ST s e
  atomicNotFetchNewMutRefST mut = atomicModifyFetchNewMutRefST mut complement
  {-# INLINE atomicNotFetchNewMutRefST #-}



instance AtomicMutRef BRef where
  atomicReadMutRefST = atomicReadBRef
  {-# INLINE atomicReadMutRefST #-}
  atomicWriteMutRefST = atomicWriteBRef
  {-# INLINE atomicWriteMutRefST #-}
  casMutRefST = casBRef
  {-# INLINE casMutRefST #-}
  atomicModifyMutRefST = atomicModifyBRef
  {-# INLINE atomicModifyMutRefST #-}
  atomicModifyFetchOldMutRefST = atomicModifyFetchOldBRef
  {-# INLINE atomicModifyFetchOldMutRefST #-}
  atomicModifyFetchNewMutRefST = atomicModifyFetchNewBRef
  {-# INLINE atomicModifyFetchNewMutRefST #-}


instance AtomicCountMutRef BRef where
  type AtomicCountElt BRef e = Num e

instance AtomicBitsMutRef BRef where
  type AtomicBitsElt BRef e = Bits e


instance Typeable p => AtomicMutRef (PUMArray p) where
  type AtomicElt (PUMArray p) e = Atomic e
  atomicReadMutRefST mba = atomicReadMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicReadMutRefST #-}
  atomicWriteMutRefST mba = atomicWriteMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicWriteMutRefST #-}
  casMutRefST mba = casBoolFetchMBytes (coerce mba) (0 :: Off e)
  {-# INLINE casMutRefST #-}
  atomicModifyMutRefST mba = atomicModifyMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicModifyMutRefST #-}


instance Typeable p => AtomicCountMutRef (PUMArray p) where
  type AtomicCountElt (PUMArray p) e = AtomicCount e
  atomicAddFetchOldMutRefST mba = atomicAddFetchOldMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicAddFetchOldMutRefST #-}
  atomicAddFetchNewMutRefST mba = atomicAddFetchNewMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicAddFetchNewMutRefST #-}
  atomicSubFetchOldMutRefST mba = atomicSubFetchOldMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicSubFetchOldMutRefST #-}
  atomicSubFetchNewMutRefST mba = atomicSubFetchNewMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicSubFetchNewMutRefST #-}


instance Typeable p => AtomicBitsMutRef (PUMArray p) where
  type AtomicBitsElt (PUMArray p) e = AtomicBits e
  atomicAndFetchOldMutRefST mba = atomicAndFetchOldMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicAndFetchOldMutRefST #-}
  atomicAndFetchNewMutRefST mba = atomicAndFetchNewMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicAndFetchNewMutRefST #-}
  atomicNandFetchOldMutRefST mba = atomicNandFetchOldMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicNandFetchOldMutRefST #-}
  atomicNandFetchNewMutRefST mba = atomicNandFetchNewMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicNandFetchNewMutRefST #-}
  atomicOrFetchOldMutRefST mba = atomicOrFetchOldMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicOrFetchOldMutRefST #-}
  atomicOrFetchNewMutRefST mba = atomicOrFetchNewMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicOrFetchNewMutRefST #-}
  atomicXorFetchOldMutRefST mba = atomicXorFetchOldMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicXorFetchOldMutRefST #-}
  atomicXorFetchNewMutRefST mba = atomicXorFetchNewMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicXorFetchNewMutRefST #-}
  atomicNotFetchOldMutRefST mba = atomicNotFetchOldMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicNotFetchOldMutRefST #-}
  atomicNotFetchNewMutRefST mba = atomicNotFetchNewMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicNotFetchNewMutRefST #-}


instance AtomicMutRef MAddr where
  type AtomicElt MAddr e = Atomic e
  atomicReadMutRefST maddr = atomicReadOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicReadMutRefST #-}
  atomicWriteMutRefST maddr = atomicWriteOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicWriteMutRefST #-}
  casMutRefST maddr = casBoolFetchOffMAddr maddr (0 :: Off e)
  {-# INLINE casMutRefST #-}
  atomicModifyMutRefST maddr = atomicModifyOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicModifyMutRefST #-}


instance AtomicCountMutRef MAddr where
  type AtomicCountElt MAddr e = AtomicCount e
  atomicAddFetchOldMutRefST maddr = atomicAddFetchOldOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicAddFetchOldMutRefST #-}
  atomicAddFetchNewMutRefST maddr = atomicAddFetchNewOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicAddFetchNewMutRefST #-}
  atomicSubFetchOldMutRefST maddr = atomicSubFetchOldOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicSubFetchOldMutRefST #-}
  atomicSubFetchNewMutRefST maddr = atomicSubFetchNewOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicSubFetchNewMutRefST #-}


instance AtomicBitsMutRef MAddr where
  type AtomicBitsElt MAddr e = AtomicBits e
  atomicAndFetchOldMutRefST maddr = atomicAndFetchOldOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicAndFetchOldMutRefST #-}
  atomicAndFetchNewMutRefST maddr = atomicAndFetchNewOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicAndFetchNewMutRefST #-}
  atomicNandFetchOldMutRefST maddr = atomicNandFetchOldOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicNandFetchOldMutRefST #-}
  atomicNandFetchNewMutRefST maddr = atomicNandFetchNewOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicNandFetchNewMutRefST #-}
  atomicOrFetchOldMutRefST maddr = atomicOrFetchOldOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicOrFetchOldMutRefST #-}
  atomicOrFetchNewMutRefST maddr = atomicOrFetchNewOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicOrFetchNewMutRefST #-}
  atomicXorFetchOldMutRefST maddr = atomicXorFetchOldOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicXorFetchOldMutRefST #-}
  atomicXorFetchNewMutRefST maddr = atomicXorFetchNewOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicXorFetchNewMutRefST #-}
  atomicNotFetchOldMutRefST maddr = atomicNotFetchOldOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicNotFetchOldMutRefST #-}
  atomicNotFetchNewMutRefST maddr = atomicNotFetchNewOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicNotFetchNewMutRefST #-}


instance AtomicMutRef FMAddr where
  type AtomicElt FMAddr e = Atomic e
  atomicReadMutRefST maddr = atomicReadOffFMAddr maddr (0 :: Off e)
  {-# INLINE atomicReadMutRefST #-}
  atomicWriteMutRefST maddr = atomicWriteOffFMAddr maddr (0 :: Off e)
  {-# INLINE atomicWriteMutRefST #-}
  casMutRefST maddr = casBoolFetchOffFMAddr maddr (0 :: Off e)
  {-# INLINE casMutRefST #-}
  atomicModifyMutRefST maddr = atomicModifyOffFMAddr maddr (0 :: Off e)
  {-# INLINE atomicModifyMutRefST #-}


instance AtomicCountMutRef FMAddr where
  type AtomicCountElt FMAddr e = AtomicCount e
  atomicAddFetchOldMutRefST maddr = atomicAddFetchOldOffFMAddr maddr (0 :: Off e)
  {-# INLINE atomicAddFetchOldMutRefST #-}
  atomicAddFetchNewMutRefST maddr = atomicAddFetchNewOffFMAddr maddr (0 :: Off e)
  {-# INLINE atomicAddFetchNewMutRefST #-}
  atomicSubFetchOldMutRefST maddr = atomicSubFetchOldOffFMAddr maddr (0 :: Off e)
  {-# INLINE atomicSubFetchOldMutRefST #-}
  atomicSubFetchNewMutRefST maddr = atomicSubFetchNewOffFMAddr maddr (0 :: Off e)
  {-# INLINE atomicSubFetchNewMutRefST #-}


instance AtomicBitsMutRef FMAddr where
  type AtomicBitsElt FMAddr e = AtomicBits e
  atomicAndFetchOldMutRefST maddr = atomicAndFetchOldOffFMAddr maddr (0 :: Off e)
  {-# INLINE atomicAndFetchOldMutRefST #-}
  atomicAndFetchNewMutRefST maddr = atomicAndFetchNewOffFMAddr maddr (0 :: Off e)
  {-# INLINE atomicAndFetchNewMutRefST #-}
  atomicNandFetchOldMutRefST maddr = atomicNandFetchOldOffFMAddr maddr (0 :: Off e)
  {-# INLINE atomicNandFetchOldMutRefST #-}
  atomicNandFetchNewMutRefST maddr = atomicNandFetchNewOffFMAddr maddr (0 :: Off e)
  {-# INLINE atomicNandFetchNewMutRefST #-}
  atomicOrFetchOldMutRefST maddr = atomicOrFetchOldOffFMAddr maddr (0 :: Off e)
  {-# INLINE atomicOrFetchOldMutRefST #-}
  atomicOrFetchNewMutRefST maddr = atomicOrFetchNewOffFMAddr maddr (0 :: Off e)
  {-# INLINE atomicOrFetchNewMutRefST #-}
  atomicXorFetchOldMutRefST maddr = atomicXorFetchOldOffFMAddr maddr (0 :: Off e)
  {-# INLINE atomicXorFetchOldMutRefST #-}
  atomicXorFetchNewMutRefST maddr = atomicXorFetchNewOffFMAddr maddr (0 :: Off e)
  {-# INLINE atomicXorFetchNewMutRefST #-}
  atomicNotFetchOldMutRefST maddr = atomicNotFetchOldOffFMAddr maddr (0 :: Off e)
  {-# INLINE atomicNotFetchOldMutRefST #-}
  atomicNotFetchNewMutRefST maddr = atomicNotFetchNewOffFMAddr maddr (0 :: Off e)
  {-# INLINE atomicNotFetchNewMutRefST #-}


instance AtomicMutRef UMArray where
  type AtomicElt UMArray e = Atomic e
  atomicReadMutRefST ma = atomicReadMBytes (fromUMArrayMBytes ma) (0 :: Off e)
  {-# INLINE atomicReadMutRefST #-}
  atomicWriteMutRefST ma = atomicWriteMBytes (fromUMArrayMBytes ma) (0 :: Off e)
  {-# INLINE atomicWriteMutRefST #-}
  casMutRefST ma = casBoolFetchMBytes (fromUMArrayMBytes ma) (0 :: Off e)
  {-# INLINE casMutRefST #-}
  atomicModifyMutRefST ma = atomicModifyMBytes (fromUMArrayMBytes ma) (0 :: Off e)
  {-# INLINE atomicModifyMutRefST #-}


instance AtomicCountMutRef UMArray where
  type AtomicCountElt UMArray e = AtomicCount e
  atomicAddFetchOldMutRefST ma = atomicAddFetchOldMBytes (fromUMArrayMBytes ma) (0 :: Off e)
  {-# INLINE atomicAddFetchOldMutRefST #-}
  atomicAddFetchNewMutRefST ma = atomicAddFetchNewMBytes (fromUMArrayMBytes ma) (0 :: Off e)
  {-# INLINE atomicAddFetchNewMutRefST #-}
  atomicSubFetchOldMutRefST ma = atomicSubFetchOldMBytes (fromUMArrayMBytes ma) (0 :: Off e)
  {-# INLINE atomicSubFetchOldMutRefST #-}
  atomicSubFetchNewMutRefST ma = atomicSubFetchNewMBytes (fromUMArrayMBytes ma) (0 :: Off e)
  {-# INLINE atomicSubFetchNewMutRefST #-}


instance AtomicBitsMutRef UMArray where
  type AtomicBitsElt UMArray e = AtomicBits e
  atomicAndFetchOldMutRefST ma = atomicAndFetchOldMBytes (fromUMArrayMBytes ma) (0 :: Off e)
  {-# INLINE atomicAndFetchOldMutRefST #-}
  atomicAndFetchNewMutRefST ma = atomicAndFetchNewMBytes (fromUMArrayMBytes ma) (0 :: Off e)
  {-# INLINE atomicAndFetchNewMutRefST #-}
  atomicNandFetchOldMutRefST ma = atomicNandFetchOldMBytes (fromUMArrayMBytes ma) (0 :: Off e)
  {-# INLINE atomicNandFetchOldMutRefST #-}
  atomicNandFetchNewMutRefST ma = atomicNandFetchNewMBytes (fromUMArrayMBytes ma) (0 :: Off e)
  {-# INLINE atomicNandFetchNewMutRefST #-}
  atomicOrFetchOldMutRefST ma = atomicOrFetchOldMBytes (fromUMArrayMBytes ma) (0 :: Off e)
  {-# INLINE atomicOrFetchOldMutRefST #-}
  atomicOrFetchNewMutRefST ma = atomicOrFetchNewMBytes (fromUMArrayMBytes ma) (0 :: Off e)
  {-# INLINE atomicOrFetchNewMutRefST #-}
  atomicXorFetchOldMutRefST ma = atomicXorFetchOldMBytes (fromUMArrayMBytes ma) (0 :: Off e)
  {-# INLINE atomicXorFetchOldMutRefST #-}
  atomicXorFetchNewMutRefST ma = atomicXorFetchNewMBytes (fromUMArrayMBytes ma) (0 :: Off e)
  {-# INLINE atomicXorFetchNewMutRefST #-}
  atomicNotFetchOldMutRefST ma = atomicNotFetchOldMBytes (fromUMArrayMBytes ma) (0 :: Off e)
  {-# INLINE atomicNotFetchOldMutRefST #-}
  atomicNotFetchNewMutRefST ma = atomicNotFetchNewMBytes (fromUMArrayMBytes ma) (0 :: Off e)
  {-# INLINE atomicNotFetchNewMutRefST #-}


instance AtomicMutRef BMArray where
  casMutRefST mba = casBMArray mba 0
  {-# INLINE casMutRefST #-}

instance AtomicCountMutRef BMArray where
  type AtomicCountElt BMArray e = Num e
instance AtomicBitsMutRef BMArray where
  type AtomicBitsElt BMArray e = Bits e


instance AtomicMutRef SBMArray where
  casMutRefST mba = casSBMArray mba 0
  {-# INLINE casMutRefST #-}

instance AtomicCountMutRef SBMArray where
  type AtomicCountElt SBMArray e = Num e
instance AtomicBitsMutRef SBMArray where
  type AtomicBitsElt SBMArray e = Bits e










atomicReadMutRef ::
  (AtomicMutRef mr, AtomicElt mr e, Primal s m)
  => mr e s -- ^ Mutable variable to read an element from
  -> m e
atomicReadMutRef = liftST . atomicReadMutRefST
{-# INLINE atomicReadMutRef #-}

-- | Write an element into `MutableByteArray#` atomically. Implies full memory barrier.
atomicWriteMutRef ::
     (AtomicMutRef mr, AtomicElt mr e, Primal s m)
  => mr e s -- ^ Mutable variable to write an element into
  -> e -- ^ Element to write
  -> m ()
atomicWriteMutRef mut = liftST . atomicWriteMutRefST mut
{-# INLINE atomicWriteMutRef #-}

-- | Compare-and-swap (CAS) operation. Given a mutable array, offset in number of
-- elements, an old value and a new value atomically swap the old value for the new one,
-- but only if the actual old value was an exact match to the expetced old one that was
-- supplied. Returns the actual old value, which if compared to supplied expected one
-- will tell us whether atomic swap occured or not.
casMutRef ::
     (AtomicMutRef mr, AtomicElt mr e, Primal s m)
  => mr e s -- ^ Variable to be mutated
  -> e -- ^ Expected old value
  -> e -- ^ New value
  -> m (Bool, e) -- ^ Was compare and swap successfull and the actual value
casMutRef mut x = liftST . casMutRefST mut x
{-# INLINE casMutRef #-}

-- | Perform atomic an modification of an element in a mutable structure.
--
-- @since 0.1.0
atomicModifyMutRef ::
     (AtomicMutRef mr, AtomicElt mr e, Primal s m)
  => mr e s -- ^ Variable to be mutated
  -> (e -> (e, b)) -- ^ Function to be applied atomically to the element
  -> m b
atomicModifyMutRef mut = liftST . atomicModifyMutRefST mut
{-# INLINE atomicModifyMutRef #-}

-- | Perform atomic modification of an element in a mutable structure and return the
-- previous value.
--
-- @since 0.1.0
atomicModifyFetchOldMutRef ::
     (AtomicMutRef mr, AtomicElt mr e, Primal s m)
  => mr e s -- ^ Variable to be mutated
  -> (e -> e) -- ^ Function to be applied atomically to the element
  -> m e
atomicModifyFetchOldMutRef mut = liftST . atomicModifyFetchOldMutRefST mut
{-# INLINE atomicModifyFetchOldMutRef #-}

-- | Perform atomic modification of an element in a mutable structure and return the
-- new value.
--
-- @since 0.1.0
atomicModifyFetchNewMutRef ::
     (AtomicMutRef mr, AtomicElt mr e, Primal s m)
  => mr e s -- ^ Variable to be mutated
  -> (e -> e) -- ^ Function to be applied atomically to the element
  -> m e
atomicModifyFetchNewMutRef mut = liftST . atomicModifyFetchNewMutRefST mut
{-# INLINE atomicModifyFetchNewMutRef #-}


atomicAddFetchOldMutRef ::
     (AtomicCountMutRef mr, AtomicCountElt mr e, Primal s m)
  => mr e s
  -> e
  -> m e
atomicAddFetchOldMutRef mut = liftST . atomicAddFetchOldMutRefST mut
{-# INLINE atomicAddFetchOldMutRef #-}

atomicAddFetchNewMutRef ::
     (AtomicCountMutRef mr, AtomicCountElt mr e, Primal s m)
  => mr e s
  -> e
  -> m e
atomicAddFetchNewMutRef mut = liftST . atomicAddFetchNewMutRefST mut
{-# INLINE atomicAddFetchNewMutRef #-}

atomicSubFetchOldMutRef ::
     (AtomicCountMutRef mr, AtomicCountElt mr e, Primal s m)
  => mr e s
  -> e
  -> m e
atomicSubFetchOldMutRef mut = liftST . atomicSubFetchOldMutRefST mut
{-# INLINE atomicSubFetchOldMutRef #-}

atomicSubFetchNewMutRef ::
     (AtomicCountMutRef mr, AtomicCountElt mr e, Primal s m)
  => mr e s
  -> e
  -> m e
atomicSubFetchNewMutRef mut = liftST . atomicSubFetchNewMutRefST mut
{-# INLINE atomicSubFetchNewMutRef #-}

atomicAndFetchOldMutRef ::
     (AtomicBitsMutRef mr, AtomicBitsElt mr e, Primal s m)
  => mr e s
  -> e
  -> m e
atomicAndFetchOldMutRef mut = liftST . atomicAndFetchOldMutRefST mut
{-# INLINE atomicAndFetchOldMutRef #-}

atomicAndFetchNewMutRef ::
     (AtomicBitsMutRef mr, AtomicBitsElt mr e, Primal s m)
  => mr e s
  -> e
  -> m e
atomicAndFetchNewMutRef mut = liftST . atomicAndFetchNewMutRefST mut
{-# INLINE atomicAndFetchNewMutRef #-}

atomicNandFetchOldMutRef ::
     (AtomicBitsMutRef mr, AtomicBitsElt mr e, Primal s m)
  => mr e s
  -> e
  -> m e
atomicNandFetchOldMutRef mut = liftST . atomicNandFetchOldMutRefST mut
{-# INLINE atomicNandFetchOldMutRef #-}

atomicNandFetchNewMutRef ::
     (AtomicBitsMutRef mr, AtomicBitsElt mr e, Primal s m)
  => mr e s
  -> e
  -> m e
atomicNandFetchNewMutRef mut = liftST . atomicNandFetchNewMutRefST mut
{-# INLINE atomicNandFetchNewMutRef #-}

atomicOrFetchOldMutRef ::
     (AtomicBitsMutRef mr, AtomicBitsElt mr e, Primal s m)
  => mr e s
  -> e
  -> m e
atomicOrFetchOldMutRef mut = liftST . atomicOrFetchOldMutRefST mut
{-# INLINE atomicOrFetchOldMutRef #-}

atomicOrFetchNewMutRef ::
     (AtomicBitsMutRef mr, AtomicBitsElt mr e, Primal s m)
  => mr e s
  -> e
  -> m e
atomicOrFetchNewMutRef mut = liftST . atomicOrFetchNewMutRefST mut
{-# INLINE atomicOrFetchNewMutRef #-}

atomicXorFetchOldMutRef ::
     (AtomicBitsMutRef mr, AtomicBitsElt mr e, Primal s m)
  => mr e s
  -> e
  -> m e
atomicXorFetchOldMutRef mut = liftST . atomicXorFetchOldMutRefST mut
{-# INLINE atomicXorFetchOldMutRef #-}

atomicXorFetchNewMutRef ::
     (AtomicBitsMutRef mr, AtomicBitsElt mr e, Primal s m)
  => mr e s
  -> e
  -> m e
atomicXorFetchNewMutRef mut =  liftST . atomicXorFetchNewMutRefST mut
{-# INLINE atomicXorFetchNewMutRef #-}

atomicNotFetchOldMutRef ::
     (AtomicBitsMutRef mr, AtomicBitsElt mr e, Primal s m) => mr e s -> m e
atomicNotFetchOldMutRef = liftST . atomicNotFetchOldMutRefST
{-# INLINE atomicNotFetchOldMutRef #-}

atomicNotFetchNewMutRef ::
     (AtomicBitsMutRef mr, AtomicBitsElt mr e, Primal s m) => mr e s -> m e
atomicNotFetchNewMutRef = liftST . atomicNotFetchNewMutRefST
{-# INLINE atomicNotFetchNewMutRef #-}





atomicModifyMutRef_ :: (AtomicMutRef mr, AtomicElt mr e, Primal s m) => mr e s -> (e -> e) -> m ()
atomicModifyMutRef_ ref f = atomicModifyMutRef ref $ \e -> (f e, ())
{-# INLINE atomicModifyMutRef_ #-}
