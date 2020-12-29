{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-- |
-- Module      : Primal.Container.Mutable.Ref.Atomic
-- Copyright   : (c) Alexey Kuleshevich 2020-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Container.Mutable.Ref.Atomic
  ( AtomicMRef(..)
  , AtomicCountMRef(..)
  , AtomicBitsMRef(..)
  , atomicModifyMRef_
  ) where

import Data.Bits
import Primal.Data.Ref
import Primal.Data.Array
import Primal.Container.Mutable.Ref.Internal
import Primal.Memory.Addr
import Primal.Memory.Bytes
import Primal.Memory.PArray



class MRef c e => AtomicMRef c e where

  atomicReadMRef ::
    MonadPrim s m
    => c e s -- ^ Mutable variable to read an element from
    -> m e
  atomicReadMRef mut = atomicModifyMRef mut (\x -> (x, x))
  {-# INLINE atomicReadMRef #-}

  -- | Write an element into `MutableByteArray#` atomically. Implies full memory barrier.
  atomicWriteMRef ::
       MonadPrim s m
    => c e s -- ^ Mutable variable to write an element into
    -> e -- ^ Element to write
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
    => c e s -- ^ Variable to be mutated
    -> e -- ^ Expected old value
    -> e -- ^ New value
    -> m (Bool, e) -- ^ Was compare and swap successfull and the actual value

  -- | Perform atomic an modification of an element in a mutable structure.
  --
  -- @since 0.1.0
  atomicModifyMRef ::
       MonadPrim s m
    => c e s -- ^ Variable to be mutated
    -> (e -> (e, b)) -- ^ Function to be applied atomically to the element
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
    => c e s -- ^ Variable to be mutated
    -> (e -> e) -- ^ Function to be applied atomically to the element
    -> m e
  atomicModifyFetchOldMRef mut f = atomicModifyMRef mut (\ x -> let x' = f x in (x', x))
  {-# INLINE atomicModifyFetchOldMRef #-}

  -- | Perform atomic modification of an element in a mutable structure and return the
  -- new value.
  --
  -- @since 0.1.0
  atomicModifyFetchNewMRef ::
       MonadPrim s m
    => c e s -- ^ Variable to be mutated
    -> (e -> e) -- ^ Function to be applied atomically to the element
    -> m e
  atomicModifyFetchNewMRef mut f = atomicModifyMRef mut (\ x -> let x' = f x in (x', x'))
  {-# INLINE atomicModifyFetchNewMRef #-}



class (Num e, AtomicMRef c e) => AtomicCountMRef c e where
  atomicAddFetchOldMRef :: MonadPrim s m => c e s -> e -> m e
  atomicAddFetchOldMRef mut !y = atomicModifyFetchOldMRef mut (+ y)
  {-# INLINE atomicAddFetchOldMRef #-}

  atomicAddFetchNewMRef :: MonadPrim s m => c e s -> e -> m e
  atomicAddFetchNewMRef mut !y = atomicModifyFetchNewMRef mut (+ y)
  {-# INLINE atomicAddFetchNewMRef #-}

  atomicSubFetchOldMRef :: MonadPrim s m => c e s -> e -> m e
  atomicSubFetchOldMRef mut !y = atomicModifyFetchOldMRef mut (subtract y)
  {-# INLINE atomicSubFetchOldMRef #-}

  atomicSubFetchNewMRef :: MonadPrim s m => c e s -> e -> m e
  atomicSubFetchNewMRef mut !y = atomicModifyFetchNewMRef mut (subtract y)
  {-# INLINE atomicSubFetchNewMRef #-}


class (Bits e, AtomicMRef c e) => AtomicBitsMRef c e where
  atomicAndFetchOldMRef :: MonadPrim s m => c e s -> e -> m e
  atomicAndFetchOldMRef mut !y = atomicModifyFetchOldMRef mut (.&. y)
  {-# INLINE atomicAndFetchOldMRef #-}

  atomicAndFetchNewMRef :: MonadPrim s m => c e s -> e -> m e
  atomicAndFetchNewMRef mut !y = atomicModifyFetchNewMRef mut (.&. y)
  {-# INLINE atomicAndFetchNewMRef #-}

  atomicNandFetchOldMRef :: MonadPrim s m => c e s -> e -> m e
  atomicNandFetchOldMRef mut !y = atomicModifyFetchOldMRef mut (\x -> complement (x .&. y))
  {-# INLINE atomicNandFetchOldMRef #-}

  atomicNandFetchNewMRef :: MonadPrim s m => c e s -> e -> m e
  atomicNandFetchNewMRef mut !y = atomicModifyFetchNewMRef mut (\x -> complement (x .&. y))
  {-# INLINE atomicNandFetchNewMRef #-}

  atomicOrFetchOldMRef :: MonadPrim s m => c e s -> e -> m e
  atomicOrFetchOldMRef mut !y = atomicModifyFetchOldMRef mut (.|. y)
  {-# INLINE atomicOrFetchOldMRef #-}

  atomicOrFetchNewMRef :: MonadPrim s m => c e s -> e -> m e
  atomicOrFetchNewMRef mut !y = atomicModifyFetchNewMRef mut (.|. y)
  {-# INLINE atomicOrFetchNewMRef #-}

  atomicXorFetchOldMRef :: MonadPrim s m => c e s -> e -> m e
  atomicXorFetchOldMRef mut !y = atomicModifyFetchOldMRef mut (`xor` y)
  {-# INLINE atomicXorFetchOldMRef #-}

  atomicXorFetchNewMRef :: MonadPrim s m => c e s -> e -> m e
  atomicXorFetchNewMRef mut !y = atomicModifyFetchNewMRef mut (`xor` y)
  {-# INLINE atomicXorFetchNewMRef #-}

  atomicNotFetchOldMRef :: MonadPrim s m => c e s -> m e
  atomicNotFetchOldMRef mut = atomicModifyFetchOldMRef mut complement
  {-# INLINE atomicNotFetchOldMRef #-}

  atomicNotFetchNewMRef :: MonadPrim s m => c e s -> m e
  atomicNotFetchNewMRef mut = atomicModifyFetchNewMRef mut complement
  {-# INLINE atomicNotFetchNewMRef #-}



instance AtomicMRef Ref a where
  atomicReadMRef = atomicReadRef
  {-# INLINE atomicReadMRef #-}
  atomicWriteMRef = atomicWriteRef
  {-# INLINE atomicWriteMRef #-}
  casMRef = casRef
  {-# INLINE casMRef #-}
  atomicModifyMRef = atomicModifyRef
  {-# INLINE atomicModifyMRef #-}
  atomicModifyFetchOldMRef = atomicModifyFetchOldRef
  {-# INLINE atomicModifyFetchOldMRef #-}
  atomicModifyFetchNewMRef = atomicModifyFetchNewRef
  {-# INLINE atomicModifyFetchNewMRef #-}


instance Num a => AtomicCountMRef Ref a

instance Bits a => AtomicBitsMRef Ref a


instance (Typeable p, Atomic e) => AtomicMRef (PMArray p) e where
  atomicReadMRef mba = atomicReadMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicReadMRef #-}
  atomicWriteMRef mba = atomicWriteMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicWriteMRef #-}
  casMRef mba = casBoolFetchMBytes (coerce mba) (0 :: Off e)
  {-# INLINE casMRef #-}
  atomicModifyMRef mba = atomicModifyMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicModifyMRef #-}


instance (Typeable p, Num e, AtomicCount e) => AtomicCountMRef (PMArray p) e where
  atomicAddFetchOldMRef mba = atomicAddFetchOldMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicAddFetchOldMRef #-}
  atomicAddFetchNewMRef mba = atomicAddFetchNewMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicAddFetchNewMRef #-}
  atomicSubFetchOldMRef mba = atomicSubFetchOldMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicSubFetchOldMRef #-}
  atomicSubFetchNewMRef mba = atomicSubFetchNewMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicSubFetchNewMRef #-}


instance (Typeable p, Bits e, AtomicBits e) => AtomicBitsMRef (PMArray p) e where
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


instance Atomic e => AtomicMRef MAddr e where
  atomicReadMRef maddr = atomicReadOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicReadMRef #-}
  atomicWriteMRef maddr = atomicWriteOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicWriteMRef #-}
  casMRef maddr = casBoolFetchOffMAddr maddr (0 :: Off e)
  {-# INLINE casMRef #-}
  atomicModifyMRef maddr = atomicModifyOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicModifyMRef #-}


instance (Num e, AtomicCount e) => AtomicCountMRef MAddr e where
  atomicAddFetchOldMRef maddr = atomicAddFetchOldOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicAddFetchOldMRef #-}
  atomicAddFetchNewMRef maddr = atomicAddFetchNewOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicAddFetchNewMRef #-}
  atomicSubFetchOldMRef maddr = atomicSubFetchOldOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicSubFetchOldMRef #-}
  atomicSubFetchNewMRef maddr = atomicSubFetchNewOffMAddr maddr (0 :: Off e)
  {-# INLINE atomicSubFetchNewMRef #-}


instance (Bits e, AtomicBits e) => AtomicBitsMRef MAddr e where
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


instance Atomic e => AtomicMRef UMArray e where
  atomicReadMRef ma = atomicReadMBytes (fromUMArrayMBytes ma) (0 :: Off e)
  {-# INLINE atomicReadMRef #-}
  atomicWriteMRef ma = atomicWriteMBytes (fromUMArrayMBytes ma) (0 :: Off e)
  {-# INLINE atomicWriteMRef #-}
  casMRef ma = casBoolFetchMBytes (fromUMArrayMBytes ma) (0 :: Off e)
  {-# INLINE casMRef #-}
  atomicModifyMRef ma = atomicModifyMBytes (fromUMArrayMBytes ma) (0 :: Off e)
  {-# INLINE atomicModifyMRef #-}


instance (Num e, AtomicCount e) => AtomicCountMRef UMArray e where
  atomicAddFetchOldMRef ma = atomicAddFetchOldMBytes (fromUMArrayMBytes ma) (0 :: Off e)
  {-# INLINE atomicAddFetchOldMRef #-}
  atomicAddFetchNewMRef ma = atomicAddFetchNewMBytes (fromUMArrayMBytes ma) (0 :: Off e)
  {-# INLINE atomicAddFetchNewMRef #-}
  atomicSubFetchOldMRef ma = atomicSubFetchOldMBytes (fromUMArrayMBytes ma) (0 :: Off e)
  {-# INLINE atomicSubFetchOldMRef #-}
  atomicSubFetchNewMRef ma = atomicSubFetchNewMBytes (fromUMArrayMBytes ma) (0 :: Off e)
  {-# INLINE atomicSubFetchNewMRef #-}


instance (Bits e, AtomicBits e) => AtomicBitsMRef UMArray e where
  atomicAndFetchOldMRef ma = atomicAndFetchOldMBytes (fromUMArrayMBytes ma) (0 :: Off e)
  {-# INLINE atomicAndFetchOldMRef #-}
  atomicAndFetchNewMRef ma = atomicAndFetchNewMBytes (fromUMArrayMBytes ma) (0 :: Off e)
  {-# INLINE atomicAndFetchNewMRef #-}
  atomicNandFetchOldMRef ma = atomicNandFetchOldMBytes (fromUMArrayMBytes ma) (0 :: Off e)
  {-# INLINE atomicNandFetchOldMRef #-}
  atomicNandFetchNewMRef ma = atomicNandFetchNewMBytes (fromUMArrayMBytes ma) (0 :: Off e)
  {-# INLINE atomicNandFetchNewMRef #-}
  atomicOrFetchOldMRef ma = atomicOrFetchOldMBytes (fromUMArrayMBytes ma) (0 :: Off e)
  {-# INLINE atomicOrFetchOldMRef #-}
  atomicOrFetchNewMRef ma = atomicOrFetchNewMBytes (fromUMArrayMBytes ma) (0 :: Off e)
  {-# INLINE atomicOrFetchNewMRef #-}
  atomicXorFetchOldMRef ma = atomicXorFetchOldMBytes (fromUMArrayMBytes ma) (0 :: Off e)
  {-# INLINE atomicXorFetchOldMRef #-}
  atomicXorFetchNewMRef ma = atomicXorFetchNewMBytes (fromUMArrayMBytes ma) (0 :: Off e)
  {-# INLINE atomicXorFetchNewMRef #-}
  atomicNotFetchOldMRef ma = atomicNotFetchOldMBytes (fromUMArrayMBytes ma) (0 :: Off e)
  {-# INLINE atomicNotFetchOldMRef #-}
  atomicNotFetchNewMRef ma = atomicNotFetchNewMBytes (fromUMArrayMBytes ma) (0 :: Off e)
  {-# INLINE atomicNotFetchNewMRef #-}


instance AtomicMRef BMArray e where
  casMRef mba = casBMArray mba 0
  {-# INLINE casMRef #-}

instance Num e => AtomicCountMRef BMArray e
instance Bits e => AtomicBitsMRef BMArray e


instance AtomicMRef SBMArray e where
  casMRef mba = casSBMArray mba 0
  {-# INLINE casMRef #-}

instance Num e => AtomicCountMRef SBMArray e
instance Bits e => AtomicBitsMRef SBMArray e


atomicModifyMRef_ :: (AtomicMRef c e, MonadPrim s m) => c e s -> (e -> e) -> m ()
atomicModifyMRef_ ref f = atomicModifyMRef ref $ \e -> (f e, ())
{-# INLINE atomicModifyMRef_ #-}
