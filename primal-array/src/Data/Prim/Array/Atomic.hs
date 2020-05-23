{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.Prim.Array.Atomic
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Array.Atomic
  ( AtomicMRef(..)
  , AtomicCountMRef(..)
  , AtomicBitsMRef(..)
  , AtomicMArray(..)
  , AtomicCountMArray(..)
  , AtomicBitsMArray(..)
  ) where

import Control.Prim.Monad
import Data.Bits
import Data.Prim.Array.Internal
import qualified Data.Prim.Array.Boxed as B
import qualified Data.Prim.Array.Boxed.Small as SB
import qualified Data.Prim.Array.Unboxed as U
import Data.Prim.Memory.Addr
import Data.Prim.Memory.ByteArray
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
  atomicWriteMRef mut y = atomicModifyMRef mut (const (y, ()))
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




class (Num (Elt mut), AtomicMRef mut) => AtomicCountMRef mut where
  atomicAddFetchOldMRef :: MonadPrim s m => mut s -> Elt mut -> m (Elt mut)
  atomicAddFetchOldMRef mut y = atomicModifyMRef mut (\x -> let x' = x + y in (x', x))
  {-# INLINE atomicAddFetchOldMRef #-}

  atomicAddFetchNewMRef :: MonadPrim s m => mut s -> Elt mut -> m (Elt mut)
  atomicAddFetchNewMRef mut y = atomicModifyMRef mut (\x -> let x' = x + y in (x', x'))
  {-# INLINE atomicAddFetchNewMRef #-}

  atomicSubFetchOldMRef :: MonadPrim s m => mut s -> Elt mut -> m (Elt mut)
  atomicSubFetchOldMRef mut y = atomicModifyMRef mut (\x -> let x' = x - y in (x', x))
  {-# INLINE atomicSubFetchOldMRef #-}

  atomicSubFetchNewMRef :: MonadPrim s m => mut s -> Elt mut -> m (Elt mut)
  atomicSubFetchNewMRef mut y = atomicModifyMRef mut (\x -> let x' = x - y in (x', x'))
  {-# INLINE atomicSubFetchNewMRef #-}


class (Bits (Elt mut), AtomicMRef mut) => AtomicBitsMRef mut where
  atomicAndFetchOldMRef :: MonadPrim s m => mut s -> Elt mut -> m (Elt mut)
  atomicAndFetchOldMRef mut y = atomicModifyMRef mut (\x -> let x' = x .&. y in (x', x))
  {-# INLINE atomicAndFetchOldMRef #-}

  atomicAndFetchNewMRef :: MonadPrim s m => mut s -> Elt mut -> m (Elt mut)
  atomicAndFetchNewMRef mut y = atomicModifyMRef mut (\x -> let x' = x .&. y in (x', x'))
  {-# INLINE atomicAndFetchNewMRef #-}

  atomicNandFetchOldMRef :: MonadPrim s m => mut s -> Elt mut -> m (Elt mut)
  atomicNandFetchOldMRef mut y =
    atomicModifyMRef mut (\x -> let x' = complement (x .&. y) in (x', x))
  {-# INLINE atomicNandFetchOldMRef #-}

  atomicNandFetchNewMRef :: MonadPrim s m => mut s -> Elt mut -> m (Elt mut)
  atomicNandFetchNewMRef mut y =
    atomicModifyMRef mut (\x -> let x' = complement (x .&. y) in (x', x'))
  {-# INLINE atomicNandFetchNewMRef #-}

  atomicOrFetchOldMRef :: MonadPrim s m => mut s -> Elt mut -> m (Elt mut)
  atomicOrFetchOldMRef mut y = atomicModifyMRef mut (\x -> let x' = x .|. y in (x', x))
  {-# INLINE atomicOrFetchOldMRef #-}

  atomicOrFetchNewMRef :: MonadPrim s m => mut s -> Elt mut -> m (Elt mut)
  atomicOrFetchNewMRef mut y = atomicModifyMRef mut (\x -> let x' = x .|. y in (x', x'))
  {-# INLINE atomicOrFetchNewMRef #-}

  atomicXorFetchOldMRef :: MonadPrim s m => mut s -> Elt mut -> m (Elt mut)
  atomicXorFetchOldMRef mut y = atomicModifyMRef mut (\x -> let x' = x `xor` y in (x', x))
  {-# INLINE atomicXorFetchOldMRef #-}

  atomicXorFetchNewMRef :: MonadPrim s m => mut s -> Elt mut -> m (Elt mut)
  atomicXorFetchNewMRef mut y = atomicModifyMRef mut (\x -> let x' = x `xor` y in (x', x'))
  {-# INLINE atomicXorFetchNewMRef #-}

  atomicNotFetchOldMRef :: MonadPrim s m => mut s -> m (Elt mut)
  atomicNotFetchOldMRef mut = atomicModifyMRef mut (\x -> let x' = complement x in (x', x))
  {-# INLINE atomicNotFetchOldMRef #-}

  atomicNotFetchNewMRef :: MonadPrim s m => mut s -> m (Elt mut)
  atomicNotFetchNewMRef mut = atomicModifyMRef mut (\x -> let x' = complement x in (x', x'))
  {-# INLINE atomicNotFetchNewMRef #-}





class MArray mut => AtomicMArray mut where

  atomicReadMArray ::
    MonadPrim s m
    => mut s -- ^ Mutable array to read an element from
    -> Int -- ^ Offset into the array
    -> m (Elt mut)
  atomicReadMArray mut i = atomicModifyMArray mut i (\x -> (x, x))
  {-# INLINE atomicReadMArray #-}

  -- | Write an element into `MutableByteArray#` atomically. Implies full memory barrier.
  atomicWriteMArray ::
       MonadPrim s m
    => mut s -- ^ Mutable array to write an element into
    -> Int -- ^ Offset into the array
    -> Elt mut -- ^ Element to write
    -> m ()
  atomicWriteMArray mut i y = atomicModifyMArray mut i (const (y, ()))
  {-# INLINE atomicWriteMArray #-}

  -- | Compare-and-swap (CAS) operation. Given a mutable array, offset in number of
  -- elements, an old value and a new value atomically swap the old value for the new one,
  -- but only if the actual old value was an exact match to the expetced old one that was
  -- supplied. Returns the actual old value, which if compared to supplied expected one
  -- will tell us whether atomic swap occured or not.
  casMArray ::
       MonadPrim s m
    => mut s -- ^ Array to be mutated
    -> Int -- ^ Offset into the array
    -> Elt mut -- ^ Expected old value
    -> Elt mut -- ^ New value
    -> m (Bool, Elt mut) -- ^ Was compare and swap successfull and the actual value

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
  atomicAddFetchOldMArray mut i y = atomicModifyMArray mut i (\x -> let x' = x + y in (x', x))
  {-# INLINE atomicAddFetchOldMArray #-}

  atomicAddFetchNewMArray :: MonadPrim s m => mut s -> Int -> Elt mut -> m (Elt mut)
  atomicAddFetchNewMArray mut i y = atomicModifyMArray mut i (\x -> let x' = x + y in (x', x'))
  {-# INLINE atomicAddFetchNewMArray #-}

  atomicSubFetchOldMArray :: MonadPrim s m => mut s -> Int -> Elt mut -> m (Elt mut)
  atomicSubFetchOldMArray mut i y = atomicModifyMArray mut i (\x -> let x' = x - y in (x', x))
  {-# INLINE atomicSubFetchOldMArray #-}

  atomicSubFetchNewMArray :: MonadPrim s m => mut s -> Int -> Elt mut -> m (Elt mut)
  atomicSubFetchNewMArray mut i y = atomicModifyMArray mut i (\x -> let x' = x - y in (x', x'))
  {-# INLINE atomicSubFetchNewMArray #-}


class (Bits (Elt mut), AtomicMArray mut) => AtomicBitsMArray mut where
  atomicAndFetchOldMArray :: MonadPrim s m => mut s -> Int -> Elt mut -> m (Elt mut)
  atomicAndFetchOldMArray mut i y = atomicModifyMArray mut i (\x -> let x' = x .&. y in (x', x))
  {-# INLINE atomicAndFetchOldMArray #-}

  atomicAndFetchNewMArray :: MonadPrim s m => mut s -> Int -> Elt mut -> m (Elt mut)
  atomicAndFetchNewMArray mut i y = atomicModifyMArray mut i (\x -> let x' = x .&. y in (x', x'))
  {-# INLINE atomicAndFetchNewMArray #-}

  atomicNandFetchOldMArray :: MonadPrim s m => mut s -> Int -> Elt mut -> m (Elt mut)
  atomicNandFetchOldMArray mut i y =
    atomicModifyMArray mut i (\x -> let x' = complement (x .&. y) in (x', x))
  {-# INLINE atomicNandFetchOldMArray #-}

  atomicNandFetchNewMArray :: MonadPrim s m => mut s -> Int -> Elt mut -> m (Elt mut)
  atomicNandFetchNewMArray mut i y =
    atomicModifyMArray mut i (\x -> let x' = complement (x .&. y) in (x', x'))
  {-# INLINE atomicNandFetchNewMArray #-}

  atomicOrFetchOldMArray :: MonadPrim s m => mut s -> Int -> Elt mut -> m (Elt mut)
  atomicOrFetchOldMArray mut i y = atomicModifyMArray mut i (\x -> let x' = x .|. y in (x', x))
  {-# INLINE atomicOrFetchOldMArray #-}

  atomicOrFetchNewMArray :: MonadPrim s m => mut s -> Int -> Elt mut -> m (Elt mut)
  atomicOrFetchNewMArray mut i y = atomicModifyMArray mut i (\x -> let x' = x .|. y in (x', x'))
  {-# INLINE atomicOrFetchNewMArray #-}

  atomicXorFetchOldMArray :: MonadPrim s m => mut s -> Int -> Elt mut -> m (Elt mut)
  atomicXorFetchOldMArray mut i y = atomicModifyMArray mut i (\x -> let x' = x `xor` y in (x', x))
  {-# INLINE atomicXorFetchOldMArray #-}

  atomicXorFetchNewMArray :: MonadPrim s m => mut s -> Int -> Elt mut -> m (Elt mut)
  atomicXorFetchNewMArray mut i y = atomicModifyMArray mut i (\x -> let x' = x `xor` y in (x', x'))
  {-# INLINE atomicXorFetchNewMArray #-}

  atomicNotFetchOldMArray :: MonadPrim s m => mut s -> Int -> m (Elt mut)
  atomicNotFetchOldMArray mut i = atomicModifyMArray mut i (\x -> let x' = complement x in (x', x))
  {-# INLINE atomicNotFetchOldMArray #-}

  atomicNotFetchNewMArray :: MonadPrim s m => mut s -> Int -> m (Elt mut)
  atomicNotFetchNewMArray mut i = atomicModifyMArray mut i (\x -> let x' = complement x in (x', x'))
  {-# INLINE atomicNotFetchNewMArray #-}


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



instance (Typeable p, Atomic e) => AtomicMRef (MByteArray p e) where
  atomicReadMRef mba = atomicReadMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicReadMRef #-}
  atomicWriteMRef mba = atomicWriteMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicWriteMRef #-}
  casMRef mba = casBoolFetchMBytes (coerce mba) (0 :: Off e)
  {-# INLINE casMRef #-}
  atomicModifyMRef mba = atomicModifyMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicModifyMRef #-}


instance (Typeable p, Num e, AtomicCount e) => AtomicCountMRef (MByteArray p e) where
  atomicAddFetchOldMRef mba = atomicAddFetchOldMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicAddFetchOldMRef #-}
  atomicAddFetchNewMRef mba = atomicAddFetchNewMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicAddFetchNewMRef #-}
  atomicSubFetchOldMRef mba = atomicSubFetchOldMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicSubFetchOldMRef #-}
  atomicSubFetchNewMRef mba = atomicSubFetchNewMBytes (coerce mba) (0 :: Off e)
  {-# INLINE atomicSubFetchNewMRef #-}


instance (Typeable p, Bits e, AtomicBits e) => AtomicBitsMRef (MByteArray p e) where
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


instance (Typeable p, Atomic e) => AtomicMArray (MByteArray p e) where
  atomicReadMArray mba i = atomicReadMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicReadMArray #-}
  atomicWriteMArray mba i = atomicWriteMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicWriteMArray #-}
  casMArray mba i = casBoolFetchMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE casMArray #-}
  atomicModifyMArray mba i = atomicModifyMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicModifyMArray #-}


instance (Typeable p, Num e, AtomicCount e) => AtomicCountMArray (MByteArray p e) where
  atomicAddFetchOldMArray mba i = atomicAddFetchOldMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicAddFetchOldMArray #-}
  atomicAddFetchNewMArray mba i = atomicAddFetchNewMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicAddFetchNewMArray #-}
  atomicSubFetchOldMArray mba i = atomicSubFetchOldMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicSubFetchOldMArray #-}
  atomicSubFetchNewMArray mba i = atomicSubFetchNewMBytes (coerce mba) (coerce i :: Off e)
  {-# INLINE atomicSubFetchNewMArray #-}


instance (Typeable p, Bits e, AtomicBits e) => AtomicBitsMArray (MByteArray p e) where
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


instance Atomic e => AtomicMRef (U.MUArray e) where
  atomicReadMRef mba = atomicReadMBytes (U.toMBytes mba) (0 :: Off e)
  {-# INLINE atomicReadMRef #-}
  atomicWriteMRef mba = atomicWriteMBytes (U.toMBytes mba) (0 :: Off e)
  {-# INLINE atomicWriteMRef #-}
  casMRef mba = casBoolFetchMBytes (U.toMBytes mba) (0 :: Off e)
  {-# INLINE casMRef #-}
  atomicModifyMRef mba = atomicModifyMBytes (U.toMBytes mba) (0 :: Off e)
  {-# INLINE atomicModifyMRef #-}


instance (Num e, AtomicCount e) => AtomicCountMRef (U.MUArray e) where
  atomicAddFetchOldMRef mba = atomicAddFetchOldMBytes (U.toMBytes mba) (0 :: Off e)
  {-# INLINE atomicAddFetchOldMRef #-}
  atomicAddFetchNewMRef mba = atomicAddFetchNewMBytes (U.toMBytes mba) (0 :: Off e)
  {-# INLINE atomicAddFetchNewMRef #-}
  atomicSubFetchOldMRef mba = atomicSubFetchOldMBytes (U.toMBytes mba) (0 :: Off e)
  {-# INLINE atomicSubFetchOldMRef #-}
  atomicSubFetchNewMRef mba = atomicSubFetchNewMBytes (U.toMBytes mba) (0 :: Off e)
  {-# INLINE atomicSubFetchNewMRef #-}


instance (Bits e, AtomicBits e) => AtomicBitsMRef (U.MUArray e) where
  atomicAndFetchOldMRef mba = atomicAndFetchOldMBytes (U.toMBytes mba) (0 :: Off e)
  {-# INLINE atomicAndFetchOldMRef #-}
  atomicAndFetchNewMRef mba = atomicAndFetchNewMBytes (U.toMBytes mba) (0 :: Off e)
  {-# INLINE atomicAndFetchNewMRef #-}
  atomicNandFetchOldMRef mba = atomicNandFetchOldMBytes (U.toMBytes mba) (0 :: Off e)
  {-# INLINE atomicNandFetchOldMRef #-}
  atomicNandFetchNewMRef mba = atomicNandFetchNewMBytes (U.toMBytes mba) (0 :: Off e)
  {-# INLINE atomicNandFetchNewMRef #-}
  atomicOrFetchOldMRef mba = atomicOrFetchOldMBytes (U.toMBytes mba) (0 :: Off e)
  {-# INLINE atomicOrFetchOldMRef #-}
  atomicOrFetchNewMRef mba = atomicOrFetchNewMBytes (U.toMBytes mba) (0 :: Off e)
  {-# INLINE atomicOrFetchNewMRef #-}
  atomicXorFetchOldMRef mba = atomicXorFetchOldMBytes (U.toMBytes mba) (0 :: Off e)
  {-# INLINE atomicXorFetchOldMRef #-}
  atomicXorFetchNewMRef mba = atomicXorFetchNewMBytes (U.toMBytes mba) (0 :: Off e)
  {-# INLINE atomicXorFetchNewMRef #-}
  atomicNotFetchOldMRef mba = atomicNotFetchOldMBytes (U.toMBytes mba) (0 :: Off e)
  {-# INLINE atomicNotFetchOldMRef #-}
  atomicNotFetchNewMRef mba = atomicNotFetchNewMBytes (U.toMBytes mba) (0 :: Off e)
  {-# INLINE atomicNotFetchNewMRef #-}


instance Atomic e => AtomicMArray (U.MUArray e) where
  atomicReadMArray mba i = atomicReadMBytes (U.toMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicReadMArray #-}
  atomicWriteMArray mba i = atomicWriteMBytes (U.toMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicWriteMArray #-}
  casMArray mba i = casBoolFetchMBytes (U.toMBytes mba) (coerce i :: Off e)
  {-# INLINE casMArray #-}
  atomicModifyMArray mba i = atomicModifyMBytes (U.toMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicModifyMArray #-}


instance (Num e, AtomicCount e) => AtomicCountMArray (U.MUArray e) where
  atomicAddFetchOldMArray mba i = atomicAddFetchOldMBytes (U.toMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicAddFetchOldMArray #-}
  atomicAddFetchNewMArray mba i = atomicAddFetchNewMBytes (U.toMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicAddFetchNewMArray #-}
  atomicSubFetchOldMArray mba i = atomicSubFetchOldMBytes (U.toMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicSubFetchOldMArray #-}
  atomicSubFetchNewMArray mba i = atomicSubFetchNewMBytes (U.toMBytes mba) (coerce i :: Off e)
  {-# INLINE atomicSubFetchNewMArray #-}


instance (Bits e, AtomicBits e) => AtomicBitsMArray (U.MUArray e) where
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


instance AtomicMRef (B.MBArray e) where
  casMRef mba = B.casMArray mba 0
  {-# INLINE casMRef #-}

instance Num e => AtomicCountMRef (B.MBArray e)
instance Bits e => AtomicBitsMRef (B.MBArray e)

instance AtomicMArray (B.MBArray e) where
  casMArray = B.casMArray
  {-# INLINE casMArray #-}

instance Num e => AtomicCountMArray (B.MBArray e)
instance Bits e => AtomicBitsMArray (B.MBArray e)



instance AtomicMRef (SB.MSBArray e) where
  casMRef msba = SB.casMArray msba 0
  {-# INLINE casMRef #-}

instance Num e => AtomicCountMRef (SB.MSBArray e)
instance Bits e => AtomicBitsMRef (SB.MSBArray e)

instance AtomicMArray (SB.MSBArray e) where
  casMArray = SB.casMArray
  {-# INLINE casMArray #-}

instance Num e => AtomicCountMArray (SB.MSBArray e)
instance Bits e => AtomicBitsMArray (SB.MSBArray e)
