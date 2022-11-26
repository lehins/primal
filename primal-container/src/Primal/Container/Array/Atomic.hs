{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- |
-- Module      : Primal.Container.Array.Atomic
-- Copyright   : (c) Alexey Kuleshevich 2020-2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module Primal.Container.Array.Atomic (
  AtomicMutArray (..),
  AtomicCountMutArray (..),
  AtomicBitsMutArray (..),
  casMutArray,
  atomicReadMutArray,
  atomicWriteMutArray,
  atomicModifyMutArray,
  atomicAddFetchOldMutArray,
  atomicAddFetchNewMutArray,
  atomicSubFetchOldMutArray,
  atomicSubFetchNewMutArray,
  atomicAndFetchOldMutArray,
  atomicAndFetchNewMutArray,
  atomicNandFetchOldMutArray,
  atomicNandFetchNewMutArray,
  atomicOrFetchOldMutArray,
  atomicOrFetchNewMutArray,
  atomicXorFetchOldMutArray,
  atomicXorFetchNewMutArray,
  atomicNotFetchOldMutArray,
  atomicNotFetchNewMutArray,
  ifoldAtomicMutArray,
  ifindAtomicMutArray,
) where

import Data.Bits
import Primal.Array
import Primal.Container.Array.Internal
import Primal.Container.Internal
import Primal.Container.Ref.Atomic
import Primal.Memory.Addr
import Primal.Memory.Bytes
import Primal.Memory.PUArray
import Primal.Monad

class (AtomicMutRef ma, MutArray ma) => AtomicMutArray ma where
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
  -- @since 1.0.0
  casMutArrayST
    :: AtomicElt ma e
    => ma e s
    -- ^ Array to be mutated
    -> Int
    -- ^ Offset into the array
    --
    -- [Unsafe /offset/] /Unchecked precondition:/ @offset >= 0 && offset < `getSizeOfMutArrayST` mut@
    -> e
    -- ^ Expected old value
    -> e
    -- ^ New value to write
    -> ST s (Bool, e)
    -- ^ Was compare and swap successfull and the actual value

  -- | Read an element from an array atomically. It is different from a regular
  -- `readMutArrayST`, because it might perform steps to guaranty atomicity. Default
  -- implementation uses `atomicModifyMutArrayST`
  --
  -- @since 1.0.0
  atomicReadMutArrayST
    :: AtomicElt ma e
    => ma e s
    -- ^ Mutable array to read an element from
    -> Int
    -- ^ Offset into the array
    --
    -- [Unsafe /offset/] /Unchecked precondition:/ @offset >= 0 && offset < `getSizeOfMutArrayST` mut@
    -> ST s e
  atomicReadMutArrayST mut !i = atomicModifyMutArrayST mut i (\x -> (x, x))
  {-# INLINE atomicReadMutArrayST #-}

  -- | Write an element into mutable array atomically. It is different from a regular
  -- `readMutArrayST`, because it might perform steps to guaranty atomicity. Default
  -- implementation uses `atomicModifyMutArrayST`
  --
  -- @since 1.0.0
  atomicWriteMutArrayST
    :: AtomicElt ma e
    => ma e s
    -- ^ Mutable array to write an element into
    -> Int
    -- ^ Offset into the array
    --
    -- [Unsafe /offset/] /Unchecked precondition:/ @offset >= 0 && offset < `getSizeOfMutArrayST` mut@
    -> e
    -- ^ Element to write
    -> ST s ()
  atomicWriteMutArrayST mut !i !y = atomicModifyMutArrayST mut i (const (y, ()))
  {-# INLINE atomicWriteMutArrayST #-}

  -- | Perform atomic an modification of an element in a mutable structure.
  --
  -- @since 1.0.0
  atomicModifyMutArrayST
    :: AtomicElt ma e
    => ma e s
    -- ^ Array to be mutated
    -> Int
    -- ^ Offset into the array
    -> (e -> (e, b))
    -- ^ Function to be applied atomically to the element
    -> ST s b
  default atomicModifyMutArrayST
    :: (AtomicElt ma e, Elt ma e)
    => ma e s
    -- ^ Array to be mutated
    -> Int
    -- ^ Offset into the array
    -> (e -> (e, b))
    -- ^ Function to be applied atomically to the element
    -> ST s b
  atomicModifyMutArrayST mut !i f =
    let go expected =
          case f expected of
            (new, artifact) -> do
              (isCasSucc, actual) <- casMutArrayST mut i expected new
              if isCasSucc
                then pure artifact
                else go actual
     in readMutArrayST mut i >>= go
  {-# INLINE atomicModifyMutArrayST #-}

class (AtomicCountMutRef ma, AtomicMutArray ma) => AtomicCountMutArray ma where
  atomicAddFetchOldMutArrayST :: AtomicCountElt ma e => ma e s -> Int -> e -> ST s e
  default atomicAddFetchOldMutArrayST
    :: (AtomicCountElt ma e, AtomicElt ma e, Num e) => ma e s -> Int -> e -> ST s e
  atomicAddFetchOldMutArrayST mut i !y =
    atomicModifyMutArrayST mut i (\x -> let x' = x + y in (x', x))
  {-# INLINE atomicAddFetchOldMutArrayST #-}

  atomicAddFetchNewMutArrayST :: AtomicCountElt ma e => ma e s -> Int -> e -> ST s e
  default atomicAddFetchNewMutArrayST
    :: (AtomicCountElt ma e, AtomicElt ma e, Num e) => ma e s -> Int -> e -> ST s e
  atomicAddFetchNewMutArrayST mut i !y =
    atomicModifyMutArrayST mut i (\x -> let x' = x + y in (x', x'))
  {-# INLINE atomicAddFetchNewMutArrayST #-}

  atomicSubFetchOldMutArrayST :: AtomicCountElt ma e => ma e s -> Int -> e -> ST s e
  default atomicSubFetchOldMutArrayST
    :: (AtomicCountElt ma e, AtomicElt ma e, Num e) => ma e s -> Int -> e -> ST s e
  atomicSubFetchOldMutArrayST mut i !y =
    atomicModifyMutArrayST mut i (\x -> let x' = x - y in (x', x))
  {-# INLINE atomicSubFetchOldMutArrayST #-}

  atomicSubFetchNewMutArrayST :: AtomicCountElt ma e => ma e s -> Int -> e -> ST s e
  default atomicSubFetchNewMutArrayST
    :: (AtomicCountElt ma e, AtomicElt ma e, Num e) => ma e s -> Int -> e -> ST s e
  atomicSubFetchNewMutArrayST mut i !y =
    atomicModifyMutArrayST mut i (\x -> let x' = x - y in (x', x'))
  {-# INLINE atomicSubFetchNewMutArrayST #-}

class (AtomicBitsMutRef ma, AtomicMutArray ma) => AtomicBitsMutArray ma where
  atomicAndFetchOldMutArrayST :: AtomicBitsElt ma e => ma e s -> Int -> e -> ST s e
  default atomicAndFetchOldMutArrayST
    :: (AtomicBitsElt ma e, AtomicElt ma e, Bits e) => ma e s -> Int -> e -> ST s e
  atomicAndFetchOldMutArrayST mut i !y =
    atomicModifyMutArrayST mut i (\x -> let x' = x .&. y in (x', x))
  {-# INLINE atomicAndFetchOldMutArrayST #-}

  atomicAndFetchNewMutArrayST :: AtomicBitsElt ma e => ma e s -> Int -> e -> ST s e
  default atomicAndFetchNewMutArrayST
    :: (AtomicBitsElt ma e, AtomicElt ma e, Bits e) => ma e s -> Int -> e -> ST s e
  atomicAndFetchNewMutArrayST mut i !y =
    atomicModifyMutArrayST mut i (\x -> let x' = x .&. y in (x', x'))
  {-# INLINE atomicAndFetchNewMutArrayST #-}

  atomicNandFetchOldMutArrayST :: AtomicBitsElt ma e => ma e s -> Int -> e -> ST s e
  default atomicNandFetchOldMutArrayST
    :: (AtomicBitsElt ma e, AtomicElt ma e, Bits e) => ma e s -> Int -> e -> ST s e
  atomicNandFetchOldMutArrayST mut i !y =
    atomicModifyMutArrayST mut i (\x -> let x' = complement (x .&. y) in (x', x))
  {-# INLINE atomicNandFetchOldMutArrayST #-}

  atomicNandFetchNewMutArrayST :: AtomicBitsElt ma e => ma e s -> Int -> e -> ST s e
  default atomicNandFetchNewMutArrayST
    :: (AtomicBitsElt ma e, AtomicElt ma e, Bits e) => ma e s -> Int -> e -> ST s e
  atomicNandFetchNewMutArrayST mut i !y =
    atomicModifyMutArrayST mut i (\x -> let x' = complement (x .&. y) in (x', x'))
  {-# INLINE atomicNandFetchNewMutArrayST #-}

  atomicOrFetchOldMutArrayST :: AtomicBitsElt ma e => ma e s -> Int -> e -> ST s e
  default atomicOrFetchOldMutArrayST
    :: (AtomicBitsElt ma e, AtomicElt ma e, Bits e) => ma e s -> Int -> e -> ST s e
  atomicOrFetchOldMutArrayST mut i !y =
    atomicModifyMutArrayST mut i (\x -> let x' = x .|. y in (x', x))
  {-# INLINE atomicOrFetchOldMutArrayST #-}

  atomicOrFetchNewMutArrayST :: AtomicBitsElt ma e => ma e s -> Int -> e -> ST s e
  default atomicOrFetchNewMutArrayST
    :: (AtomicBitsElt ma e, AtomicElt ma e, Bits e) => ma e s -> Int -> e -> ST s e
  atomicOrFetchNewMutArrayST mut i !y =
    atomicModifyMutArrayST mut i (\x -> let x' = x .|. y in (x', x'))
  {-# INLINE atomicOrFetchNewMutArrayST #-}

  atomicXorFetchOldMutArrayST :: AtomicBitsElt ma e => ma e s -> Int -> e -> ST s e
  default atomicXorFetchOldMutArrayST
    :: (AtomicBitsElt ma e, AtomicElt ma e, Bits e) => ma e s -> Int -> e -> ST s e
  atomicXorFetchOldMutArrayST mut i !y =
    atomicModifyMutArrayST mut i (\x -> let x' = x `xor` y in (x', x))
  {-# INLINE atomicXorFetchOldMutArrayST #-}

  atomicXorFetchNewMutArrayST :: AtomicBitsElt ma e => ma e s -> Int -> e -> ST s e
  default atomicXorFetchNewMutArrayST
    :: (AtomicBitsElt ma e, AtomicElt ma e, Bits e) => ma e s -> Int -> e -> ST s e
  atomicXorFetchNewMutArrayST mut i !y =
    atomicModifyMutArrayST mut i (\x -> let x' = x `xor` y in (x', x'))
  {-# INLINE atomicXorFetchNewMutArrayST #-}

  atomicNotFetchOldMutArrayST :: AtomicBitsElt ma e => ma e s -> Int -> ST s e
  default atomicNotFetchOldMutArrayST
    :: (AtomicBitsElt ma e, AtomicElt ma e, Bits e) => ma e s -> Int -> ST s e
  atomicNotFetchOldMutArrayST mut i =
    atomicModifyMutArrayST mut i (\x -> let x' = complement x in (x', x))
  {-# INLINE atomicNotFetchOldMutArrayST #-}

  atomicNotFetchNewMutArrayST :: AtomicBitsElt ma e => ma e s -> Int -> ST s e
  default atomicNotFetchNewMutArrayST
    :: (AtomicBitsElt ma e, AtomicElt ma e, Bits e) => ma e s -> Int -> ST s e
  atomicNotFetchNewMutArrayST mut i =
    atomicModifyMutArrayST mut i (\x -> let x' = complement x in (x', x'))
  {-# INLINE atomicNotFetchNewMutArrayST #-}

instance Typeable p => AtomicMutArray (PUMArray p) where
  atomicReadMutArrayST mba i = atomicReadMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicReadMutArrayST #-}
  atomicWriteMutArrayST mba i = atomicWriteMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicWriteMutArrayST #-}
  casMutArrayST mba i = casBoolFetchMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE casMutArrayST #-}
  atomicModifyMutArrayST mba i = atomicModifyMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicModifyMutArrayST #-}

instance Typeable p => AtomicCountMutArray (PUMArray p) where
  atomicAddFetchOldMutArrayST mba i = atomicAddFetchOldMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicAddFetchOldMutArrayST #-}
  atomicAddFetchNewMutArrayST mba i = atomicAddFetchNewMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicAddFetchNewMutArrayST #-}
  atomicSubFetchOldMutArrayST mba i = atomicSubFetchOldMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicSubFetchOldMutArrayST #-}
  atomicSubFetchNewMutArrayST mba i = atomicSubFetchNewMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicSubFetchNewMutArrayST #-}

instance Typeable p => AtomicBitsMutArray (PUMArray p) where
  atomicAndFetchOldMutArrayST mba i = atomicAndFetchOldMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicAndFetchOldMutArrayST #-}
  atomicAndFetchNewMutArrayST mba i = atomicAndFetchNewMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicAndFetchNewMutArrayST #-}
  atomicNandFetchOldMutArrayST mba i = atomicNandFetchOldMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicNandFetchOldMutArrayST #-}
  atomicNandFetchNewMutArrayST mba i = atomicNandFetchNewMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicNandFetchNewMutArrayST #-}
  atomicOrFetchOldMutArrayST mba i = atomicOrFetchOldMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicOrFetchOldMutArrayST #-}
  atomicOrFetchNewMutArrayST mba i = atomicOrFetchNewMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicOrFetchNewMutArrayST #-}
  atomicXorFetchOldMutArrayST mba i = atomicXorFetchOldMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicXorFetchOldMutArrayST #-}
  atomicXorFetchNewMutArrayST mba i = atomicXorFetchNewMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicXorFetchNewMutArrayST #-}
  atomicNotFetchOldMutArrayST mba i = atomicNotFetchOldMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicNotFetchOldMutArrayST #-}
  atomicNotFetchNewMutArrayST mba i = atomicNotFetchNewMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicNotFetchNewMutArrayST #-}

instance AtomicMutArray MAddr where
  atomicReadMutArrayST maddr i = atomicReadOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicReadMutArrayST #-}
  atomicWriteMutArrayST maddr i = atomicWriteOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicWriteMutArrayST #-}
  casMutArrayST maddr i = casBoolFetchOffMAddr maddr (coerce i :: Off e)
  {-# INLINE casMutArrayST #-}
  atomicModifyMutArrayST maddr i = atomicModifyOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicModifyMutArrayST #-}

instance AtomicCountMutArray MAddr where
  atomicAddFetchOldMutArrayST maddr i = atomicAddFetchOldOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicAddFetchOldMutArrayST #-}
  atomicAddFetchNewMutArrayST maddr i = atomicAddFetchNewOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicAddFetchNewMutArrayST #-}
  atomicSubFetchOldMutArrayST maddr i = atomicSubFetchOldOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicSubFetchOldMutArrayST #-}
  atomicSubFetchNewMutArrayST maddr i = atomicSubFetchNewOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicSubFetchNewMutArrayST #-}

instance AtomicBitsMutArray MAddr where
  atomicAndFetchOldMutArrayST maddr i = atomicAndFetchOldOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicAndFetchOldMutArrayST #-}
  atomicAndFetchNewMutArrayST maddr i = atomicAndFetchNewOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicAndFetchNewMutArrayST #-}
  atomicNandFetchOldMutArrayST maddr i = atomicNandFetchOldOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicNandFetchOldMutArrayST #-}
  atomicNandFetchNewMutArrayST maddr i = atomicNandFetchNewOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicNandFetchNewMutArrayST #-}
  atomicOrFetchOldMutArrayST maddr i = atomicOrFetchOldOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicOrFetchOldMutArrayST #-}
  atomicOrFetchNewMutArrayST maddr i = atomicOrFetchNewOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicOrFetchNewMutArrayST #-}
  atomicXorFetchOldMutArrayST maddr i = atomicXorFetchOldOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicXorFetchOldMutArrayST #-}
  atomicXorFetchNewMutArrayST maddr i = atomicXorFetchNewOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicXorFetchNewMutArrayST #-}
  atomicNotFetchOldMutArrayST maddr i = atomicNotFetchOldOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicNotFetchOldMutArrayST #-}
  atomicNotFetchNewMutArrayST maddr i = atomicNotFetchNewOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicNotFetchNewMutArrayST #-}

instance AtomicMutArray UMArray where
  atomicReadMutArrayST mba i = atomicReadMBytes (fromUMArrayMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicReadMutArrayST #-}
  atomicWriteMutArrayST mba i = atomicWriteMBytes (fromUMArrayMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicWriteMutArrayST #-}
  casMutArrayST mba i = casBoolFetchMBytes (fromUMArrayMBytes mba) (coerce i :: Off e)
  {-# INLINE casMutArrayST #-}
  atomicModifyMutArrayST mba i = atomicModifyMBytes (fromUMArrayMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicModifyMutArrayST #-}

instance AtomicCountMutArray UMArray where
  atomicAddFetchOldMutArrayST mba i =
    atomicAddFetchOldMBytes (fromUMArrayMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicAddFetchOldMutArrayST #-}
  atomicAddFetchNewMutArrayST mba i =
    atomicAddFetchNewMBytes (fromUMArrayMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicAddFetchNewMutArrayST #-}
  atomicSubFetchOldMutArrayST mba i =
    atomicSubFetchOldMBytes (fromUMArrayMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicSubFetchOldMutArrayST #-}
  atomicSubFetchNewMutArrayST mba i =
    atomicSubFetchNewMBytes (fromUMArrayMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicSubFetchNewMutArrayST #-}

instance AtomicBitsMutArray UMArray where
  atomicAndFetchOldMutArrayST mba i =
    atomicAndFetchOldMBytes (fromUMArrayMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicAndFetchOldMutArrayST #-}
  atomicAndFetchNewMutArrayST mba i =
    atomicAndFetchNewMBytes (fromUMArrayMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicAndFetchNewMutArrayST #-}
  atomicNandFetchOldMutArrayST mba i =
    atomicNandFetchOldMBytes (fromUMArrayMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicNandFetchOldMutArrayST #-}
  atomicNandFetchNewMutArrayST mba i =
    atomicNandFetchNewMBytes (fromUMArrayMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicNandFetchNewMutArrayST #-}
  atomicOrFetchOldMutArrayST mba i =
    atomicOrFetchOldMBytes (fromUMArrayMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicOrFetchOldMutArrayST #-}
  atomicOrFetchNewMutArrayST mba i =
    atomicOrFetchNewMBytes (fromUMArrayMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicOrFetchNewMutArrayST #-}
  atomicXorFetchOldMutArrayST mba i =
    atomicXorFetchOldMBytes (fromUMArrayMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicXorFetchOldMutArrayST #-}
  atomicXorFetchNewMutArrayST mba i =
    atomicXorFetchNewMBytes (fromUMArrayMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicXorFetchNewMutArrayST #-}
  atomicNotFetchOldMutArrayST mba i =
    atomicNotFetchOldMBytes (fromUMArrayMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicNotFetchOldMutArrayST #-}
  atomicNotFetchNewMutArrayST mba i =
    atomicNotFetchNewMBytes (fromUMArrayMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicNotFetchNewMutArrayST #-}

instance AtomicMutArray BMArray where
  casMutArrayST = casBMArray
  {-# INLINE casMutArrayST #-}

instance AtomicCountMutArray BMArray
instance AtomicBitsMutArray BMArray

instance AtomicMutArray SBMArray where
  casMutArrayST = casSBMArray
  {-# INLINE casMutArrayST #-}

instance AtomicCountMutArray SBMArray
instance AtomicBitsMutArray SBMArray

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
casMutArray
  :: forall ma e m s
   . (AtomicMutArray ma, AtomicElt ma e, Primal s m)
  => ma e s
  -- ^ Array to be mutated
  -> Int
  -- ^ Offset into the array
  --
  -- [Unsafe /offset/] /Unchecked precondition:/ @offset >= 0 && offset < `getSizeOfMutArray` mut@
  -> e
  -- ^ Expected old value
  -> e
  -- ^ New value to write
  -> m (Bool, e)
  -- ^ Was compare and swap successfull and the actual value
casMutArray mut i x = liftST . casMutArrayST mut i x
{-# INLINE casMutArray #-}

-- | Read an element from an array atomically. It is different from a regular
-- `readMutArray`, because it might perform steps to guaranty atomicity. Default
-- implementation uses `atomicModifyMutArray`
--
-- @since 0.1.0
atomicReadMutArray
  :: forall ma e m s
   . (AtomicMutArray ma, AtomicElt ma e, Primal s m)
  => ma e s
  -- ^ Mutable array to read an element from
  -> Int
  -- ^ Offset into the array
  --
  -- [Unsafe /offset/] /Unchecked precondition:/ @offset >= 0 && offset < `getSizeOfMutArray` mut@
  -> m e
atomicReadMutArray mut = liftST . atomicReadMutArrayST mut
{-# INLINE atomicReadMutArray #-}

-- | Write an element into mutable array atomically. It is different from a regular
-- `readMutArray`, because it might perform steps to guaranty atomicity. Default
-- implementation uses `atomicModifyMutArray`
--
-- @since 0.1.0
atomicWriteMutArray
  :: forall ma e m s
   . (AtomicMutArray ma, AtomicElt ma e, Primal s m)
  => ma e s
  -- ^ Mutable array to write an element into
  -> Int
  -- ^ Offset into the array
  --
  -- [Unsafe /offset/] /Unchecked precondition:/ @offset >= 0 && offset < `getSizeOfMutArray` mut@
  -> e
  -- ^ Element to write
  -> m ()
atomicWriteMutArray mut i = liftST . atomicWriteMutArrayST mut i
{-# INLINE atomicWriteMutArray #-}

-- | Perform atomic an modification of an element in a mutable structure.
--
-- @since 0.1.0
atomicModifyMutArray
  :: forall ma e b m s
   . (AtomicMutArray ma, AtomicElt ma e, Primal s m)
  => ma e s
  -- ^ Array to be mutated
  -> Int
  -- ^ Offset into the array
  -> (e -> (e, b))
  -- ^ Function to be applied atomically to the element
  -> m b
atomicModifyMutArray mut i = liftST . atomicModifyMutArrayST mut i
{-# INLINE atomicModifyMutArray #-}

atomicAddFetchOldMutArray
  :: forall ma e m s
   . (AtomicCountMutArray ma, AtomicCountElt ma e, Primal s m)
  => ma e s
  -> Int
  -> e
  -> m e
atomicAddFetchOldMutArray mut i = liftST . atomicAddFetchOldMutArrayST mut i
{-# INLINE atomicAddFetchOldMutArray #-}

atomicAddFetchNewMutArray
  :: forall ma e m s
   . (AtomicCountMutArray ma, AtomicCountElt ma e, Primal s m)
  => ma e s
  -> Int
  -> e
  -> m e
atomicAddFetchNewMutArray mut i = liftST . atomicAddFetchNewMutArrayST mut i
{-# INLINE atomicAddFetchNewMutArray #-}

atomicSubFetchOldMutArray
  :: forall ma e m s
   . (AtomicCountMutArray ma, AtomicCountElt ma e, Primal s m)
  => ma e s
  -> Int
  -> e
  -> m e
atomicSubFetchOldMutArray mut i = liftST . atomicSubFetchOldMutArrayST mut i
{-# INLINE atomicSubFetchOldMutArray #-}

atomicSubFetchNewMutArray
  :: forall ma e m s
   . (AtomicCountMutArray ma, AtomicCountElt ma e, Primal s m)
  => ma e s
  -> Int
  -> e
  -> m e
atomicSubFetchNewMutArray mut i = liftST . atomicSubFetchNewMutArrayST mut i
{-# INLINE atomicSubFetchNewMutArray #-}

atomicAndFetchOldMutArray
  :: forall ma e m s
   . (AtomicBitsMutArray ma, AtomicBitsElt ma e, Primal s m)
  => ma e s
  -> Int
  -> e
  -> m e
atomicAndFetchOldMutArray mut i = liftST . atomicAndFetchOldMutArrayST mut i
{-# INLINE atomicAndFetchOldMutArray #-}

atomicAndFetchNewMutArray
  :: forall ma e m s
   . (AtomicBitsMutArray ma, AtomicBitsElt ma e, Primal s m)
  => ma e s
  -> Int
  -> e
  -> m e
atomicAndFetchNewMutArray mut i = liftST . atomicAndFetchNewMutArrayST mut i
{-# INLINE atomicAndFetchNewMutArray #-}

atomicNandFetchOldMutArray
  :: forall ma e m s
   . (AtomicBitsMutArray ma, AtomicBitsElt ma e, Primal s m)
  => ma e s
  -> Int
  -> e
  -> m e
atomicNandFetchOldMutArray mut i = liftST . atomicNandFetchOldMutArrayST mut i
{-# INLINE atomicNandFetchOldMutArray #-}

atomicNandFetchNewMutArray
  :: forall ma e m s
   . (AtomicBitsMutArray ma, AtomicBitsElt ma e, Primal s m)
  => ma e s
  -> Int
  -> e
  -> m e
atomicNandFetchNewMutArray mut i = liftST . atomicNandFetchNewMutArrayST mut i
{-# INLINE atomicNandFetchNewMutArray #-}

atomicOrFetchOldMutArray
  :: forall ma e m s
   . (AtomicBitsMutArray ma, AtomicBitsElt ma e, Primal s m)
  => ma e s
  -> Int
  -> e
  -> m e
atomicOrFetchOldMutArray mut i = liftST . atomicOrFetchOldMutArrayST mut i
{-# INLINE atomicOrFetchOldMutArray #-}

atomicOrFetchNewMutArray
  :: forall ma e m s
   . (AtomicBitsMutArray ma, AtomicBitsElt ma e, Primal s m)
  => ma e s
  -> Int
  -> e
  -> m e
atomicOrFetchNewMutArray mut i = liftST . atomicOrFetchNewMutArrayST mut i
{-# INLINE atomicOrFetchNewMutArray #-}

atomicXorFetchOldMutArray
  :: forall ma e m s
   . (AtomicBitsMutArray ma, AtomicBitsElt ma e, Primal s m)
  => ma e s
  -> Int
  -> e
  -> m e
atomicXorFetchOldMutArray mut i = liftST . atomicXorFetchOldMutArrayST mut i
{-# INLINE atomicXorFetchOldMutArray #-}

atomicXorFetchNewMutArray
  :: forall ma e m s
   . (AtomicBitsMutArray ma, AtomicBitsElt ma e, Primal s m)
  => ma e s
  -> Int
  -> e
  -> m e
atomicXorFetchNewMutArray mut i = liftST . atomicXorFetchNewMutArrayST mut i
{-# INLINE atomicXorFetchNewMutArray #-}

atomicNotFetchOldMutArray
  :: forall ma e m s
   . (AtomicBitsMutArray ma, AtomicBitsElt ma e, Primal s m)
  => ma e s
  -> Int
  -> m e
atomicNotFetchOldMutArray mut = liftST . atomicNotFetchOldMutArrayST mut
{-# INLINE atomicNotFetchOldMutArray #-}

atomicNotFetchNewMutArray
  :: forall ma e m s
   . (AtomicBitsMutArray ma, AtomicBitsElt ma e, Primal s m)
  => ma e s
  -> Int
  -> m e
atomicNotFetchNewMutArray mut = liftST . atomicNotFetchNewMutArrayST mut
{-# INLINE atomicNotFetchNewMutArray #-}

ifindAtomicMutArray
  :: (Primal s m, AtomicMutArray ma, AtomicElt ma e, Elt ma e)
  => ma e s
  -> (Int -> e -> (e, Maybe a))
  -> m (Maybe a)
ifindAtomicMutArray ma f = do
  Size n <- getSizeOfMutArray ma
  let go i
        | i >= n = pure Nothing
        | otherwise =
            atomicModifyMutArray ma i (f i) >>= \case
              Nothing -> go (i + 1)
              Just a -> pure $! Just a
  go 0
{-# INLINE ifindAtomicMutArray #-}

ifoldAtomicMutArray
  :: (Primal s m, AtomicMutArray ma, AtomicElt ma e, Elt ma e)
  => ma e s
  -> c
  -> (Int -> c -> e -> (e, (c, Maybe a)))
  -> m (c, Maybe a)
ifoldAtomicMutArray ma acc0 f = do
  Size n <- getSizeOfMutArray ma
  let go i !acc
        | i >= n = pure (acc, Nothing)
        | otherwise = do
            atomicModifyMutArray ma i (f i acc) >>= \case
              (acc', Nothing) -> go (i + 1) acc'
              res -> pure res
  go 0 acc0
{-# INLINE ifoldAtomicMutArray #-}
