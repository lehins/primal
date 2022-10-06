{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Primal.Memory.Sized
-- Copyright   : (c) Alexey Kuleshevich 2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--

module Primal.Memory.PUArray.Sized where

import Primal.Memory
import Primal.Memory.PUArray
import GHC.TypeLits

newtype SzPUArray (n :: Nat) (p :: Pinned) e = SzPUArray (PUArray p e)
type role SzPUArray nominal nominal nominal


newtype SzPUMArray (n :: Nat) (p :: Pinned) e s = SzPUMArray (PUMArray p e s)
type role SzPUMArray nominal nominal nominal nominal


singletonSzPUArray :: (Typeable p, Unbox e) => e -> SzPUArray 1 p e
singletonSzPUArray = SzPUArray . singletonMem

indexSzPUArray ::
     (Typeable p, Unbox e, KnownNat ix, KnownNat n, CmpNat ix n ~ 'LT)
  => SzPUArray n p e
  -> Ix ix
  -> e
indexSzPUArray (SzPUArray pua) = indexOffMem pua . ixToOff



data Ix (ix :: Nat) = Ix

ixToOff :: forall e ix. KnownNat ix => Ix ix -> Off e
ixToOff ix = fromInteger (natVal ix)

data Sz (sz :: Nat) = Sz

szToCount :: forall e sz. KnownNat sz => Sz sz -> Count e
szToCount sz = fromInteger (natVal sz)

newtype SzMem (n :: Nat) mr e = SzMem (mr e)
type role SzMem nominal nominal nominal


newtype SzMutMem (n :: Nat) mw e s = SzMutMem (mw e s)
type role SzMutMem nominal nominal nominal nominal

-- singletonSzMem :: (MemUnbox e) => e -> SzMem 1 mr e
-- singletonSzMem = SzMem . singletonMem

indexSzMem ::
     (MemRead (mr e), Unbox e, KnownNat ix, KnownNat sz, CmpNat ix sz ~ 'LT)
  => SzMem sz mr e
  -> Ix ix
  -> e
indexSzMem (SzMem pua) = indexOffMem pua . ixToOff


allocSzMutMem ::
  forall e s m sz ma.
  (Primal s m, MemAlloc (ma e), Unbox e, KnownNat sz) => Sz sz -> m (SzMutMem sz ma e s)
allocSzMutMem sz = SzMutMem <$> allocMutMem (szToCount @e sz)



data family PackedBytes (sz :: Nat) :: *

data instance PackedBytes 0 = PackedBytes0
newtype instance PackedBytes 1 = PackedBytes1 Word8
newtype instance PackedBytes 2 = PackedBytes2 Word16
newtype instance PackedBytes 3 = PackedBytes3 Word32
newtype instance PackedBytes 4 = PackedBytes4 Word32
newtype instance PackedBytes 5 = PackedBytes5 Word64
newtype instance PackedBytes 6 = PackedBytes6 Word64
newtype instance PackedBytes 7 = PackedBytes7 Word64
newtype instance PackedBytes 8 = PackedBytes8 Word64

data instance PackedBytes 16 = PackedBytes16 !Word64 !Word64
