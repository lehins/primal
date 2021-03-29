{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-- |
-- Module      : Primal.Container.Array.Atomic
-- Copyright   : (c) Alexey Kuleshevich 2020-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Container.Array.Atomic
  ( AtomicMArray(..)
  , AtomicCountMArray(..)
  , AtomicBitsMArray(..)
  ) where

import Primal.Monad
import Data.Bits
import Primal.Container.Array.Internal
import Primal.Array
import Primal.Memory.Addr
import Primal.Memory.Bytes
import Primal.Memory.PArray



class MArray ma e => AtomicMArray ma e where

  -- | Compare-and-swap (CAS) operation. Given a mutable array, offset in number of
  -- elements, an old expected value and a new value, swap the actual old value for the
  -- new one atomically, but only if the actual old value was an exact match to the
  -- expected old one that was supplied. Returns the boolean that indicate success or
  -- failure of the swap as well as the actual old value.
  --
  -- It is important to note that comparison does not require an `Eq` instance on the
  -- element, because in order for compare operation to succeed for boxed values epxected
  -- and actual must be refer to the exact same value, while for unboxed values it must
  -- have exact same unpacked representation.
  --
  -- @since 0.1.0
  casMArray ::
       MonadPrim s m
    => ma e s -- ^ Array to be mutated
    -> Int
    -- ^ Offset into the array
    --
    -- [Unsafe /offset/] /Unchecked precondition:/ @offset >= 0 && offset < `getSizeOfMArray` mut@
    -> e -- ^ Expected old value
    -> e -- ^ New value to write
    -> m (Bool, e) -- ^ Was compare and swap successfull and the actual value

  -- | Read an element from an array atomically. It is different from a regular
  -- `readMArray`, because it might perform steps to guaranty atomicity. Default
  -- implementation uses `atomicModifyMArray`
  --
  -- @since 0.1.0
  atomicReadMArray ::
    MonadPrim s m
    => ma e s -- ^ Mutable array to read an element from
    -> Int
    -- ^ Offset into the array
    --
    -- [Unsafe /offset/] /Unchecked precondition:/ @offset >= 0 && offset < `getSizeOfMArray` mut@
    -> m e
  atomicReadMArray mut i = atomicModifyMArray mut i (\x -> (x, x))
  {-# INLINE atomicReadMArray #-}

  -- | Write an element into mutable array atomically. It is different from a regular
  -- `readMArray`, because it might perform steps to guaranty atomicity. Default
  -- implementation uses `atomicModifyMArray`
  --
  -- @since 0.1.0
  atomicWriteMArray ::
       MonadPrim s m
    => ma e s -- ^ Mutable array to write an element into
    -> Int
    -- ^ Offset into the array
    --
    -- [Unsafe /offset/] /Unchecked precondition:/ @offset >= 0 && offset < `getSizeOfMArray` mut@
    -> e -- ^ Element to write
    -> m ()
  atomicWriteMArray mut i !y = atomicModifyMArray mut i (const (y, ()))
  {-# INLINE atomicWriteMArray #-}

  -- | Perform atomic an modification of an element in a mutable structure.
  --
  -- @since 0.1.0
  atomicModifyMArray ::
       MonadPrim s m
    => ma e s -- ^ Array to be mutated
    -> Int -- ^ Offset into the array
    -> (e -> (e, b)) -- ^ Function to be applied atomically to the element
    -> m b
  atomicModifyMArray mut i f =
    let go expected =
          case f expected of
            (new, artifact) -> do
              (isCasSucc, actual) <- casMArray mut i expected new
              if isCasSucc
                then pure artifact
                else go actual
     in readMArray mut i >>= go
  {-# INLINE atomicModifyMArray #-}






class (Num e, AtomicMArray ma e) => AtomicCountMArray ma e where
  atomicAddFetchOldMArray :: MonadPrim s m => ma e s -> Int -> e -> m e
  atomicAddFetchOldMArray mut i !y = atomicModifyMArray mut i (\x -> let x' = x + y in (x', x))
  {-# INLINE atomicAddFetchOldMArray #-}

  atomicAddFetchNewMArray :: MonadPrim s m => ma e s -> Int -> e -> m e
  atomicAddFetchNewMArray mut i !y = atomicModifyMArray mut i (\x -> let x' = x + y in (x', x'))
  {-# INLINE atomicAddFetchNewMArray #-}

  atomicSubFetchOldMArray :: MonadPrim s m => ma e s -> Int -> e -> m e
  atomicSubFetchOldMArray mut i !y = atomicModifyMArray mut i (\x -> let x' = x - y in (x', x))
  {-# INLINE atomicSubFetchOldMArray #-}

  atomicSubFetchNewMArray :: MonadPrim s m => ma e s -> Int -> e -> m e
  atomicSubFetchNewMArray mut i !y = atomicModifyMArray mut i (\x -> let x' = x - y in (x', x'))
  {-# INLINE atomicSubFetchNewMArray #-}


class (Bits e, AtomicMArray ma e) => AtomicBitsMArray ma e where
  atomicAndFetchOldMArray :: MonadPrim s m => ma e s -> Int -> e -> m e
  atomicAndFetchOldMArray mut i !y = atomicModifyMArray mut i (\x -> let x' = x .&. y in (x', x))
  {-# INLINE atomicAndFetchOldMArray #-}

  atomicAndFetchNewMArray :: MonadPrim s m => ma e s -> Int -> e -> m e
  atomicAndFetchNewMArray mut i !y = atomicModifyMArray mut i (\x -> let x' = x .&. y in (x', x'))
  {-# INLINE atomicAndFetchNewMArray #-}

  atomicNandFetchOldMArray :: MonadPrim s m => ma e s -> Int -> e -> m e
  atomicNandFetchOldMArray mut i !y =
    atomicModifyMArray mut i (\x -> let x' = complement (x .&. y) in (x', x))
  {-# INLINE atomicNandFetchOldMArray #-}

  atomicNandFetchNewMArray :: MonadPrim s m => ma e s -> Int -> e -> m e
  atomicNandFetchNewMArray mut i !y =
    atomicModifyMArray mut i (\x -> let x' = complement (x .&. y) in (x', x'))
  {-# INLINE atomicNandFetchNewMArray #-}

  atomicOrFetchOldMArray :: MonadPrim s m => ma e s -> Int -> e -> m e
  atomicOrFetchOldMArray mut i !y = atomicModifyMArray mut i (\x -> let x' = x .|. y in (x', x))
  {-# INLINE atomicOrFetchOldMArray #-}

  atomicOrFetchNewMArray :: MonadPrim s m => ma e s -> Int -> e -> m e
  atomicOrFetchNewMArray mut i !y = atomicModifyMArray mut i (\x -> let x' = x .|. y in (x', x'))
  {-# INLINE atomicOrFetchNewMArray #-}

  atomicXorFetchOldMArray :: MonadPrim s m => ma e s -> Int -> e -> m e
  atomicXorFetchOldMArray mut i !y = atomicModifyMArray mut i (\x -> let x' = x `xor` y in (x', x))
  {-# INLINE atomicXorFetchOldMArray #-}

  atomicXorFetchNewMArray :: MonadPrim s m => ma e s -> Int -> e -> m e
  atomicXorFetchNewMArray mut i !y = atomicModifyMArray mut i (\x -> let x' = x `xor` y in (x', x'))
  {-# INLINE atomicXorFetchNewMArray #-}

  atomicNotFetchOldMArray :: MonadPrim s m => ma e s -> Int -> m e
  atomicNotFetchOldMArray mut i = atomicModifyMArray mut i (\x -> let x' = complement x in (x', x))
  {-# INLINE atomicNotFetchOldMArray #-}

  atomicNotFetchNewMArray :: MonadPrim s m => ma e s -> Int -> m e
  atomicNotFetchNewMArray mut i = atomicModifyMArray mut i (\x -> let x' = complement x in (x', x'))
  {-# INLINE atomicNotFetchNewMArray #-}


instance (Typeable p, Atomic e) => AtomicMArray (PMArray p) e where
  atomicReadMArray mba i = atomicReadMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicReadMArray #-}
  atomicWriteMArray mba i = atomicWriteMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicWriteMArray #-}
  casMArray mba i = casBoolFetchMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE casMArray #-}
  atomicModifyMArray mba i = atomicModifyMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicModifyMArray #-}


instance (Typeable p, Num e, AtomicCount e) => AtomicCountMArray (PMArray p) e where
  atomicAddFetchOldMArray mba i = atomicAddFetchOldMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicAddFetchOldMArray #-}
  atomicAddFetchNewMArray mba i = atomicAddFetchNewMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicAddFetchNewMArray #-}
  atomicSubFetchOldMArray mba i = atomicSubFetchOldMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicSubFetchOldMArray #-}
  atomicSubFetchNewMArray mba i = atomicSubFetchNewMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicSubFetchNewMArray #-}


instance (Typeable p, Bits e, AtomicBits e) => AtomicBitsMArray (PMArray p) e where
  atomicAndFetchOldMArray mba i = atomicAndFetchOldMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicAndFetchOldMArray #-}
  atomicAndFetchNewMArray mba i = atomicAndFetchNewMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicAndFetchNewMArray #-}
  atomicNandFetchOldMArray mba i = atomicNandFetchOldMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicNandFetchOldMArray #-}
  atomicNandFetchNewMArray mba i = atomicNandFetchNewMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicNandFetchNewMArray #-}
  atomicOrFetchOldMArray mba i = atomicOrFetchOldMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicOrFetchOldMArray #-}
  atomicOrFetchNewMArray mba i = atomicOrFetchNewMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicOrFetchNewMArray #-}
  atomicXorFetchOldMArray mba i = atomicXorFetchOldMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicXorFetchOldMArray #-}
  atomicXorFetchNewMArray mba i = atomicXorFetchNewMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicXorFetchNewMArray #-}
  atomicNotFetchOldMArray mba i = atomicNotFetchOldMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicNotFetchOldMArray #-}
  atomicNotFetchNewMArray mba i = atomicNotFetchNewMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicNotFetchNewMArray #-}




instance Atomic e => AtomicMArray MAddr e where
  atomicReadMArray maddr i = atomicReadOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicReadMArray #-}
  atomicWriteMArray maddr i = atomicWriteOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicWriteMArray #-}
  casMArray maddr i = casBoolFetchOffMAddr maddr (coerce i :: Off e)
  {-# INLINE casMArray #-}
  atomicModifyMArray maddr i = atomicModifyOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicModifyMArray #-}


instance (Num e, AtomicCount e) => AtomicCountMArray MAddr e where
  atomicAddFetchOldMArray maddr i = atomicAddFetchOldOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicAddFetchOldMArray #-}
  atomicAddFetchNewMArray maddr i = atomicAddFetchNewOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicAddFetchNewMArray #-}
  atomicSubFetchOldMArray maddr i = atomicSubFetchOldOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicSubFetchOldMArray #-}
  atomicSubFetchNewMArray maddr i = atomicSubFetchNewOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicSubFetchNewMArray #-}


instance (Bits e, AtomicBits e) => AtomicBitsMArray MAddr e where
  atomicAndFetchOldMArray maddr i = atomicAndFetchOldOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicAndFetchOldMArray #-}
  atomicAndFetchNewMArray maddr i = atomicAndFetchNewOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicAndFetchNewMArray #-}
  atomicNandFetchOldMArray maddr i = atomicNandFetchOldOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicNandFetchOldMArray #-}
  atomicNandFetchNewMArray maddr i = atomicNandFetchNewOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicNandFetchNewMArray #-}
  atomicOrFetchOldMArray maddr i = atomicOrFetchOldOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicOrFetchOldMArray #-}
  atomicOrFetchNewMArray maddr i = atomicOrFetchNewOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicOrFetchNewMArray #-}
  atomicXorFetchOldMArray maddr i = atomicXorFetchOldOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicXorFetchOldMArray #-}
  atomicXorFetchNewMArray maddr i = atomicXorFetchNewOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicXorFetchNewMArray #-}
  atomicNotFetchOldMArray maddr i = atomicNotFetchOldOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicNotFetchOldMArray #-}
  atomicNotFetchNewMArray maddr i = atomicNotFetchNewOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicNotFetchNewMArray #-}


instance Atomic e => AtomicMArray UMArray e where
  atomicReadMArray mba i = atomicReadMBytes (fromUMArrayMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicReadMArray #-}
  atomicWriteMArray mba i = atomicWriteMBytes (fromUMArrayMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicWriteMArray #-}
  casMArray mba i = casBoolFetchMBytes (fromUMArrayMBytes mba) (coerce i :: Off e)
  {-# INLINE casMArray #-}
  atomicModifyMArray mba i = atomicModifyMBytes (fromUMArrayMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicModifyMArray #-}


instance (Num e, AtomicCount e) => AtomicCountMArray UMArray e where
  atomicAddFetchOldMArray mba i = atomicAddFetchOldMBytes (fromUMArrayMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicAddFetchOldMArray #-}
  atomicAddFetchNewMArray mba i = atomicAddFetchNewMBytes (fromUMArrayMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicAddFetchNewMArray #-}
  atomicSubFetchOldMArray mba i = atomicSubFetchOldMBytes (fromUMArrayMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicSubFetchOldMArray #-}
  atomicSubFetchNewMArray mba i = atomicSubFetchNewMBytes (fromUMArrayMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicSubFetchNewMArray #-}


instance (Bits e, AtomicBits e) => AtomicBitsMArray UMArray e where
  atomicAndFetchOldMArray mba i = atomicAndFetchOldMBytes (fromUMArrayMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicAndFetchOldMArray #-}
  atomicAndFetchNewMArray mba i = atomicAndFetchNewMBytes (fromUMArrayMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicAndFetchNewMArray #-}
  atomicNandFetchOldMArray mba i = atomicNandFetchOldMBytes (fromUMArrayMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicNandFetchOldMArray #-}
  atomicNandFetchNewMArray mba i = atomicNandFetchNewMBytes (fromUMArrayMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicNandFetchNewMArray #-}
  atomicOrFetchOldMArray mba i = atomicOrFetchOldMBytes (fromUMArrayMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicOrFetchOldMArray #-}
  atomicOrFetchNewMArray mba i = atomicOrFetchNewMBytes (fromUMArrayMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicOrFetchNewMArray #-}
  atomicXorFetchOldMArray mba i = atomicXorFetchOldMBytes (fromUMArrayMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicXorFetchOldMArray #-}
  atomicXorFetchNewMArray mba i = atomicXorFetchNewMBytes (fromUMArrayMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicXorFetchNewMArray #-}
  atomicNotFetchOldMArray mba i = atomicNotFetchOldMBytes (fromUMArrayMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicNotFetchOldMArray #-}
  atomicNotFetchNewMArray mba i = atomicNotFetchNewMBytes (fromUMArrayMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicNotFetchNewMArray #-}

instance AtomicMArray BMArray e where
  casMArray = casBMArray
  {-# INLINE casMArray #-}

instance Num e => AtomicCountMArray BMArray e
instance Bits e => AtomicBitsMArray BMArray e


instance AtomicMArray SBMArray e where
  casMArray = casSBMArray
  {-# INLINE casMArray #-}

instance Num e => AtomicCountMArray SBMArray e
instance Bits e => AtomicBitsMArray SBMArray e
