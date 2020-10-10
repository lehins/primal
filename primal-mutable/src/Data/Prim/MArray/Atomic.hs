{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.Prim.MArray.Atomic
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.MArray.Atomic
  ( AtomicMArray(..)
  , AtomicCountMArray(..)
  , AtomicBitsMArray(..)
  ) where

import Control.Prim.Monad
import Data.Bits
import qualified Data.Prim.MArray.Boxed as B
import qualified Data.Prim.MArray.Boxed.Small as SB
import Data.Prim.MArray.Internal
import qualified Data.Prim.MArray.Unboxed as U
import Data.Prim.Memory.Addr
import Data.Prim.Memory.Bytes
import Data.Prim.Memory.PArray



class MArray mut => AtomicMArray mut where

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
    => mut s -- ^ Array to be mutated
    -> Int
    -- ^ Offset into the array
    --
    -- [Unsafe /offset/] /Unchecked precondition:/ @offset >= 0 && offset < `getSizeOfMArray` mut@
    -> Elt mut -- ^ Expected old value
    -> Elt mut -- ^ New value to write
    -> m (Bool, Elt mut) -- ^ Was compare and swap successfull and the actual value

  -- | Read an element from an array atomically. It is different from a regular
  -- `readMArray`, because it might perform steps to guaranty atomicity. Default
  -- implementation uses `atomicModifyMArray`
  --
  -- @since 0.1.0
  atomicReadMArray ::
    MonadPrim s m
    => mut s -- ^ Mutable array to read an element from
    -> Int
    -- ^ Offset into the array
    --
    -- [Unsafe /offset/] /Unchecked precondition:/ @offset >= 0 && offset < `getSizeOfMArray` mut@
    -> m (Elt mut)
  atomicReadMArray mut i = atomicModifyMArray mut i (\x -> (x, x))
  {-# INLINE atomicReadMArray #-}

  -- | Write an element into mutable array atomically. It is different from a regular
  -- `readMArray`, because it might perform steps to guaranty atomicity. Default
  -- implementation uses `atomicModifyMArray`
  --
  -- @since 0.1.0
  atomicWriteMArray ::
       MonadPrim s m
    => mut s -- ^ Mutable array to write an element into
    -> Int
    -- ^ Offset into the array
    --
    -- [Unsafe /offset/] /Unchecked precondition:/ @offset >= 0 && offset < `getSizeOfMArray` mut@
    -> Elt mut -- ^ Element to write
    -> m ()
  atomicWriteMArray mut i !y = atomicModifyMArray mut i (const (y, ()))
  {-# INLINE atomicWriteMArray #-}

  -- | Perform atomic an modification of an element in a mutable structure.
  --
  -- @since 0.1.0
  atomicModifyMArray ::
       MonadPrim s m
    => mut s -- ^ Array to be mutated
    -> Int -- ^ Offset into the array
    -> (Elt mut -> (Elt mut, b)) -- ^ Function to be applied atomically to the element
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






class (Num (Elt mut), AtomicMArray mut) => AtomicCountMArray mut where
  atomicAddFetchOldMArray :: MonadPrim s m => mut s -> Int -> Elt mut -> m (Elt mut)
  atomicAddFetchOldMArray mut i !y = atomicModifyMArray mut i (\x -> let x' = x + y in (x', x))
  {-# INLINE atomicAddFetchOldMArray #-}

  atomicAddFetchNewMArray :: MonadPrim s m => mut s -> Int -> Elt mut -> m (Elt mut)
  atomicAddFetchNewMArray mut i !y = atomicModifyMArray mut i (\x -> let x' = x + y in (x', x'))
  {-# INLINE atomicAddFetchNewMArray #-}

  atomicSubFetchOldMArray :: MonadPrim s m => mut s -> Int -> Elt mut -> m (Elt mut)
  atomicSubFetchOldMArray mut i !y = atomicModifyMArray mut i (\x -> let x' = x - y in (x', x))
  {-# INLINE atomicSubFetchOldMArray #-}

  atomicSubFetchNewMArray :: MonadPrim s m => mut s -> Int -> Elt mut -> m (Elt mut)
  atomicSubFetchNewMArray mut i !y = atomicModifyMArray mut i (\x -> let x' = x - y in (x', x'))
  {-# INLINE atomicSubFetchNewMArray #-}


class (Bits (Elt mut), AtomicMArray mut) => AtomicBitsMArray mut where
  atomicAndFetchOldMArray :: MonadPrim s m => mut s -> Int -> Elt mut -> m (Elt mut)
  atomicAndFetchOldMArray mut i !y = atomicModifyMArray mut i (\x -> let x' = x .&. y in (x', x))
  {-# INLINE atomicAndFetchOldMArray #-}

  atomicAndFetchNewMArray :: MonadPrim s m => mut s -> Int -> Elt mut -> m (Elt mut)
  atomicAndFetchNewMArray mut i !y = atomicModifyMArray mut i (\x -> let x' = x .&. y in (x', x'))
  {-# INLINE atomicAndFetchNewMArray #-}

  atomicNandFetchOldMArray :: MonadPrim s m => mut s -> Int -> Elt mut -> m (Elt mut)
  atomicNandFetchOldMArray mut i !y =
    atomicModifyMArray mut i (\x -> let x' = complement (x .&. y) in (x', x))
  {-# INLINE atomicNandFetchOldMArray #-}

  atomicNandFetchNewMArray :: MonadPrim s m => mut s -> Int -> Elt mut -> m (Elt mut)
  atomicNandFetchNewMArray mut i !y =
    atomicModifyMArray mut i (\x -> let x' = complement (x .&. y) in (x', x'))
  {-# INLINE atomicNandFetchNewMArray #-}

  atomicOrFetchOldMArray :: MonadPrim s m => mut s -> Int -> Elt mut -> m (Elt mut)
  atomicOrFetchOldMArray mut i !y = atomicModifyMArray mut i (\x -> let x' = x .|. y in (x', x))
  {-# INLINE atomicOrFetchOldMArray #-}

  atomicOrFetchNewMArray :: MonadPrim s m => mut s -> Int -> Elt mut -> m (Elt mut)
  atomicOrFetchNewMArray mut i !y = atomicModifyMArray mut i (\x -> let x' = x .|. y in (x', x'))
  {-# INLINE atomicOrFetchNewMArray #-}

  atomicXorFetchOldMArray :: MonadPrim s m => mut s -> Int -> Elt mut -> m (Elt mut)
  atomicXorFetchOldMArray mut i !y = atomicModifyMArray mut i (\x -> let x' = x `xor` y in (x', x))
  {-# INLINE atomicXorFetchOldMArray #-}

  atomicXorFetchNewMArray :: MonadPrim s m => mut s -> Int -> Elt mut -> m (Elt mut)
  atomicXorFetchNewMArray mut i !y = atomicModifyMArray mut i (\x -> let x' = x `xor` y in (x', x'))
  {-# INLINE atomicXorFetchNewMArray #-}

  atomicNotFetchOldMArray :: MonadPrim s m => mut s -> Int -> m (Elt mut)
  atomicNotFetchOldMArray mut i = atomicModifyMArray mut i (\x -> let x' = complement x in (x', x))
  {-# INLINE atomicNotFetchOldMArray #-}

  atomicNotFetchNewMArray :: MonadPrim s m => mut s -> Int -> m (Elt mut)
  atomicNotFetchNewMArray mut i = atomicModifyMArray mut i (\x -> let x' = complement x in (x', x'))
  {-# INLINE atomicNotFetchNewMArray #-}


instance Typeable p => AtomicMArray (MBytes p) where
  atomicReadMArray mb i = atomicReadMBytes mb (coerce i :: Off Word8)
  {-# INLINE atomicReadMArray #-}
  atomicWriteMArray mb i = atomicWriteMBytes mb (coerce i :: Off Word8)
  {-# INLINE atomicWriteMArray #-}
  casMArray mb i = casBoolFetchMBytes mb (coerce i :: Off Word8)
  {-# INLINE casMArray #-}
  atomicModifyMArray mb i = atomicModifyMBytes mb (coerce i :: Off Word8)
  {-# INLINE atomicModifyMArray #-}


instance Typeable p => AtomicCountMArray (MBytes p) where
  atomicAddFetchOldMArray mb i = atomicAddFetchOldMBytes mb (coerce i :: Off Word8)
  {-# INLINE atomicAddFetchOldMArray #-}
  atomicAddFetchNewMArray mb i = atomicAddFetchNewMBytes mb (coerce i :: Off Word8)
  {-# INLINE atomicAddFetchNewMArray #-}
  atomicSubFetchOldMArray mb i = atomicSubFetchOldMBytes mb (coerce i :: Off Word8)
  {-# INLINE atomicSubFetchOldMArray #-}
  atomicSubFetchNewMArray mb i = atomicSubFetchNewMBytes mb (coerce i :: Off Word8)
  {-# INLINE atomicSubFetchNewMArray #-}


instance Typeable p => AtomicBitsMArray (MBytes p) where
  atomicAndFetchOldMArray mb i = atomicAndFetchOldMBytes mb (coerce i :: Off Word8)
  {-# INLINE atomicAndFetchOldMArray #-}
  atomicAndFetchNewMArray mb i = atomicAndFetchNewMBytes mb (coerce i :: Off Word8)
  {-# INLINE atomicAndFetchNewMArray #-}
  atomicNandFetchOldMArray mb i = atomicNandFetchOldMBytes mb (coerce i :: Off Word8)
  {-# INLINE atomicNandFetchOldMArray #-}
  atomicNandFetchNewMArray mb i = atomicNandFetchNewMBytes mb (coerce i :: Off Word8)
  {-# INLINE atomicNandFetchNewMArray #-}
  atomicOrFetchOldMArray mb i = atomicOrFetchOldMBytes mb (coerce i :: Off Word8)
  {-# INLINE atomicOrFetchOldMArray #-}
  atomicOrFetchNewMArray mb i = atomicOrFetchNewMBytes mb (coerce i :: Off Word8)
  {-# INLINE atomicOrFetchNewMArray #-}
  atomicXorFetchOldMArray mb i = atomicXorFetchOldMBytes mb (coerce i :: Off Word8)
  {-# INLINE atomicXorFetchOldMArray #-}
  atomicXorFetchNewMArray mb i = atomicXorFetchNewMBytes mb (coerce i :: Off Word8)
  {-# INLINE atomicXorFetchNewMArray #-}
  atomicNotFetchOldMArray mb i = atomicNotFetchOldMBytes mb (coerce i :: Off Word8)
  {-# INLINE atomicNotFetchOldMArray #-}
  atomicNotFetchNewMArray mb i = atomicNotFetchNewMBytes mb (coerce i :: Off Word8)
  {-# INLINE atomicNotFetchNewMArray #-}



instance (Typeable p, Atomic e) => AtomicMArray (PMArray p e) where
  atomicReadMArray mba i = atomicReadMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicReadMArray #-}
  atomicWriteMArray mba i = atomicWriteMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicWriteMArray #-}
  casMArray mba i = casBoolFetchMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE casMArray #-}
  atomicModifyMArray mba i = atomicModifyMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicModifyMArray #-}


instance (Typeable p, Num e, AtomicCount e) => AtomicCountMArray (PMArray p e) where
  atomicAddFetchOldMArray mba i = atomicAddFetchOldMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicAddFetchOldMArray #-}
  atomicAddFetchNewMArray mba i = atomicAddFetchNewMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicAddFetchNewMArray #-}
  atomicSubFetchOldMArray mba i = atomicSubFetchOldMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicSubFetchOldMArray #-}
  atomicSubFetchNewMArray mba i = atomicSubFetchNewMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicSubFetchNewMArray #-}


instance (Typeable p, Bits e, AtomicBits e) => AtomicBitsMArray (PMArray p e) where
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




instance Atomic e => AtomicMArray (MAddr e) where
  atomicReadMArray maddr i = atomicReadOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicReadMArray #-}
  atomicWriteMArray maddr i = atomicWriteOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicWriteMArray #-}
  casMArray maddr i = casBoolFetchOffMAddr maddr (coerce i :: Off e)
  {-# INLINE casMArray #-}
  atomicModifyMArray maddr i = atomicModifyOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicModifyMArray #-}


instance (Num e, AtomicCount e) => AtomicCountMArray (MAddr e) where
  atomicAddFetchOldMArray maddr i = atomicAddFetchOldOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicAddFetchOldMArray #-}
  atomicAddFetchNewMArray maddr i = atomicAddFetchNewOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicAddFetchNewMArray #-}
  atomicSubFetchOldMArray maddr i = atomicSubFetchOldOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicSubFetchOldMArray #-}
  atomicSubFetchNewMArray maddr i = atomicSubFetchNewOffMAddr maddr (coerce i :: Off e)
  {-# INLINE atomicSubFetchNewMArray #-}


instance (Bits e, AtomicBits e) => AtomicBitsMArray (MAddr e) where
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


instance Atomic e => AtomicMArray (U.UMArray e) where
  atomicReadMArray mba i = atomicReadMBytes (U.toMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicReadMArray #-}
  atomicWriteMArray mba i = atomicWriteMBytes (U.toMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicWriteMArray #-}
  casMArray mba i = casBoolFetchMBytes (U.toMBytes mba) (coerce i :: Off e)
  {-# INLINE casMArray #-}
  atomicModifyMArray mba i = atomicModifyMBytes (U.toMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicModifyMArray #-}


instance (Num e, AtomicCount e) => AtomicCountMArray (U.UMArray e) where
  atomicAddFetchOldMArray mba i = atomicAddFetchOldMBytes (U.toMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicAddFetchOldMArray #-}
  atomicAddFetchNewMArray mba i = atomicAddFetchNewMBytes (U.toMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicAddFetchNewMArray #-}
  atomicSubFetchOldMArray mba i = atomicSubFetchOldMBytes (U.toMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicSubFetchOldMArray #-}
  atomicSubFetchNewMArray mba i = atomicSubFetchNewMBytes (U.toMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicSubFetchNewMArray #-}


instance (Bits e, AtomicBits e) => AtomicBitsMArray (U.UMArray e) where
  atomicAndFetchOldMArray mba i = atomicAndFetchOldMBytes (U.toMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicAndFetchOldMArray #-}
  atomicAndFetchNewMArray mba i = atomicAndFetchNewMBytes (U.toMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicAndFetchNewMArray #-}
  atomicNandFetchOldMArray mba i = atomicNandFetchOldMBytes (U.toMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicNandFetchOldMArray #-}
  atomicNandFetchNewMArray mba i = atomicNandFetchNewMBytes (U.toMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicNandFetchNewMArray #-}
  atomicOrFetchOldMArray mba i = atomicOrFetchOldMBytes (U.toMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicOrFetchOldMArray #-}
  atomicOrFetchNewMArray mba i = atomicOrFetchNewMBytes (U.toMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicOrFetchNewMArray #-}
  atomicXorFetchOldMArray mba i = atomicXorFetchOldMBytes (U.toMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicXorFetchOldMArray #-}
  atomicXorFetchNewMArray mba i = atomicXorFetchNewMBytes (U.toMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicXorFetchNewMArray #-}
  atomicNotFetchOldMArray mba i = atomicNotFetchOldMBytes (U.toMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicNotFetchOldMArray #-}
  atomicNotFetchNewMArray mba i = atomicNotFetchNewMBytes (U.toMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicNotFetchNewMArray #-}

instance AtomicMArray (B.BMArray e) where
  casMArray = B.casBMArray
  {-# INLINE casMArray #-}

instance Num e => AtomicCountMArray (B.BMArray e)
instance Bits e => AtomicBitsMArray (B.BMArray e)


instance AtomicMArray (SB.SBMArray e) where
  casMArray = SB.casSBMArray
  {-# INLINE casMArray #-}

instance Num e => AtomicCountMArray (SB.SBMArray e)
instance Bits e => AtomicBitsMArray (SB.SBMArray e)
