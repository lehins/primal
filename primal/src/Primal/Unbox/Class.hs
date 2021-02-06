{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Primal.Unbox.Class
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Unbox.Class
  ( Unbox(..)
  , setMutableByteArrayLoop#
  , setOffAddrLoop#
  , errorImpossible
  , bool2Int#
  , int2Bool#
  -- * Backwards compatibility
  , WordPtr(..)
  , ptrToWordPtr
  , wordPtrToPtr
  , IntPtr(..)
  , ptrToIntPtr
  , intPtrToPtr
  ) where


#include "MachDeps.h"
#include "HsBaseConfig.h"

import Primal.Monad.Unsafe
import Data.Bits
import Data.Complex
import Data.Char
import Data.Type.Equality
import Foreign.C.Error (Errno(..))
import Primal.Foreign hiding (Any)
import GHC.Conc
import GHC.Stable
import GHC.Real
import GHC.IO.Device
import GHC.Fingerprint.Type
import GHC.TypeLits as Nats
import Data.Functor.Compose
import Data.Functor.Identity
import qualified Data.Functor.Product as Functor
import Data.Monoid
import System.IO
import Data.Semigroup
#if __GLASGOW_HASKELL__ >= 800
import Data.Functor.Const
#endif /* __GLASGOW_HASKELL__ >= 800 */

#if __GLASGOW_HASKELL__ < 802
import qualified Foreign.Ptr as P
import Unsafe.Coerce
#include "primal_compat.h"

import Foreign.Storable (Storable)

-- | Replacement for `Foreign.Ptr.IntPtr` with exported constructor.
newtype IntPtr = IntPtr Int
  deriving (Eq, Ord, Num, Enum, Storable, Real, Bounded, Integral, Bits, FiniteBits, Read, Show)

-- | Replacement for `Foreign.Ptr.WordPtr` with exported constructor.
newtype WordPtr = WordPtr Word
  deriving (Eq, Ord, Num, Enum, Storable, Real, Bounded, Integral, Bits, FiniteBits, Read, Show)

-- | casts a @Ptr@ to a @WordPtr@
ptrToWordPtr :: Ptr a -> WordPtr
ptrToWordPtr (Ptr a#) = WordPtr (W# (int2Word# (addr2Int# a#)))

-- | casts a @WordPtr@ to a @Ptr@
wordPtrToPtr :: WordPtr -> Ptr a
wordPtrToPtr (WordPtr (W# w#)) = Ptr (int2Addr# (word2Int# w#))

-- | casts a @Ptr@ to an @IntPtr@
ptrToIntPtr :: Ptr a -> IntPtr
ptrToIntPtr (Ptr a#) = IntPtr (I# (addr2Int# a#))

-- | casts an @IntPtr@ to a @Ptr@
intPtrToPtr :: IntPtr -> Ptr a
intPtrToPtr (IntPtr (I# i#)) = Ptr (int2Addr# i#)

instance Unbox P.IntPtr where
  type PrimBase P.IntPtr = IntPtr
  -- Constructor for newtype was not exported
  toPrimBase = unsafeCoerce
  fromPrimBase = unsafeCoerce

instance Unbox P.WordPtr where
  type PrimBase P.WordPtr = WordPtr
  -- Constructor for newtype was not exported
  toPrimBase = unsafeCoerce
  fromPrimBase = unsafeCoerce
#else
import Foreign.Ptr
#endif

-- | Invariants:
--
-- * Reading should never fail on memory that contains only zeros
--
-- * Writing should always overwrite all of the bytes allocated for the element. In other
--   words, writing to a dirty (uninitilized) region of memory should never leave any
--   garbage around. For example, if a type requires 31 bytes of memory then on any write
--   all 31 bytes must be overwritten.
--
-- * A single thread write/read sequence must always roundtrip
--
-- * This is not a class for serialization, therefore memory layout of unpacked datatype
--   is selfcontained in `Prim` class and representation is not expected to stay the same
--   between different versions of software. Primitive types like `Int`, `Word`, `Char`
--   are an exception to this rule for obvious reasons.
--
class Unbox a where
  type PrimBase a :: *

  type SizeOf a :: Nat
  type SizeOf a = SizeOf (PrimBase a)
  type Alignment a :: Nat
  type Alignment a = Alignment (PrimBase a)

  toPrimBase :: a -> PrimBase a
  default toPrimBase :: Coercible a (PrimBase a) => a -> PrimBase a
  toPrimBase = coerce

  fromPrimBase :: PrimBase a -> a
  default fromPrimBase :: Coercible a (PrimBase a) => PrimBase a -> a
  fromPrimBase = coerce

  -- | Returned value must match the `SizeOf` type level Nat
  sizeOf# :: Proxy# a -> Int#
  default sizeOf# :: Unbox (PrimBase a) => Proxy# a -> Int#
  sizeOf# _ = sizeOf# (proxy# :: Proxy# (PrimBase a))
  {-# INLINE sizeOf# #-}

  -- | Returned value must match the `Alignment` type level Nat
  alignment# :: Proxy# a -> Int#
  default alignment# :: Unbox (PrimBase a) => Proxy# a -> Int#
  alignment# _ = alignment# (proxy# :: Proxy# (PrimBase a))
  {-# INLINE alignment# #-}


  indexByteOffByteArray# :: ByteArray# -> Int# -> a
  default indexByteOffByteArray# :: Unbox (PrimBase a) => ByteArray# -> Int# -> a
  indexByteOffByteArray# ba# i# = fromPrimBase (indexByteOffByteArray# ba# i# :: PrimBase a)
  {-# INLINE indexByteOffByteArray# #-}

  --
  -- These equalities hold:
  --
  -- > indexByteArray# ba# i# == indexOffAddr# (byteArrayContents# ba#) i#
  --
  -- > indexByteArray# ba# i# == indexByteOffByteArray# ba# (i# *# sizeOf (proxy# :: Proxy# a))
  --
  indexByteArray# :: ByteArray# -> Int# -> a
  default indexByteArray# :: Unbox (PrimBase a) => ByteArray# -> Int# -> a
  indexByteArray# ba# i# = fromPrimBase (indexByteArray# ba# i# :: PrimBase a)
  {-# INLINE indexByteArray# #-}

  indexOffAddr# :: Addr# -> Int# -> a
  default indexOffAddr# :: Unbox (PrimBase a) => Addr# -> Int# -> a
  indexOffAddr# addr# i# = fromPrimBase (indexOffAddr# addr# i# :: PrimBase a)
  {-# INLINE indexOffAddr# #-}


  readByteOffMutableByteArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
  default readByteOffMutableByteArray# ::
    Unbox (PrimBase a) => MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
  readByteOffMutableByteArray# mba# i# s = case readByteOffMutableByteArray# mba# i# s of
                                             (# s', pa :: PrimBase a #) -> (# s', fromPrimBase pa #)
  {-# INLINE readByteOffMutableByteArray# #-}

  readMutableByteArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
  default readMutableByteArray# ::
    Unbox (PrimBase a) => MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
  readMutableByteArray# mba# i# s = case readMutableByteArray# mba# i# s of
                                      (# s', pa :: PrimBase a #) -> (# s', fromPrimBase pa #)
  {-# INLINE readMutableByteArray# #-}

  readOffAddr# :: Addr# -> Int# -> State# s -> (# State# s, a #)
  default readOffAddr# ::
    Unbox (PrimBase a) => Addr# -> Int# -> State# s -> (# State# s, a #)
  readOffAddr# addr# i# s = case readOffAddr# addr# i# s of
                              (# s', pa :: PrimBase a #) -> (# s', fromPrimBase pa #)
  {-# INLINE readOffAddr# #-}


  writeByteOffMutableByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> State# s
  default writeByteOffMutableByteArray# ::
    Unbox (PrimBase a) => MutableByteArray# s -> Int# -> a -> State# s -> State# s
  writeByteOffMutableByteArray# mba# i# a =
    writeByteOffMutableByteArray# mba# i# (toPrimBase a :: PrimBase a)
  {-# INLINE writeByteOffMutableByteArray# #-}

  writeMutableByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> State# s
  default writeMutableByteArray# ::
    Unbox (PrimBase a) => MutableByteArray# s -> Int# -> a -> State# s -> State# s
  writeMutableByteArray# mba# i# a = writeMutableByteArray# mba# i# (toPrimBase a :: PrimBase a)
  {-# INLINE writeMutableByteArray# #-}

  writeOffAddr# :: Addr# -> Int# -> a -> State# s -> State# s
  default writeOffAddr# :: Unbox (PrimBase a) => Addr# -> Int# -> a -> State# s -> State# s
  writeOffAddr# addr# i# a = writeOffAddr# addr# i# (toPrimBase a)
  {-# INLINE writeOffAddr# #-}

  -- TODO: implement
  -- setByteOffMutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> a -> State# s -> State# s
  -- default setMutableByteArray# ::
  --   Unbox (PrimBase a) => MutableByteArray# s -> Int# -> Int# -> a -> State# s -> State# s
  -- setByteOffMutableByteArray# mba# i# n# a = setByteOffMutableByteArray# mba# i# n# (toPrimBase a)
  -- {-# INLINE setByteOffMutableByteArray# #-}

  -- | Set the region of MutableByteArray to the same value. Offset is in number of elements
  setMutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> a -> State# s -> State# s
  default setMutableByteArray# ::
    Unbox (PrimBase a) => MutableByteArray# s -> Int# -> Int# -> a -> State# s -> State# s
  setMutableByteArray# mba# i# n# a = setMutableByteArray# mba# i# n# (toPrimBase a)
  {-# INLINE setMutableByteArray# #-}

  -- | Set the region of memory to the same value. Offset is in number of elements
  setOffAddr# :: Addr# -> Int# -> Int# -> a -> State# s -> State# s
  default setOffAddr# ::
    Unbox (PrimBase a) => Addr# -> Int# -> Int# -> a -> State# s -> State# s
  setOffAddr# addr# i# n# a = setOffAddr# addr# i# n# (toPrimBase a)
  {-# INLINE setOffAddr# #-}


instance Unbox () where
  type PrimBase () = ()
  type SizeOf () = 0
  type Alignment () = 1
  sizeOf# _ = 0#
  {-# INLINE sizeOf# #-}
  alignment# _ = 1#
  {-# INLINE alignment# #-}
  indexByteOffByteArray# _ _ = ()
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# _ _ = ()
  {-# INLINE indexByteArray# #-}
  indexOffAddr# _ _ = ()
  {-# INLINE indexOffAddr# #-}
  readByteOffMutableByteArray# _ _ s = (# s, () #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readMutableByteArray# _ _ s = (# s, () #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# _ _ s = (# s, () #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# _ _ () s = s
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# _ _ () s = s
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# _ _ () s = s
  {-# INLINE writeOffAddr# #-}
  setMutableByteArray# _ _ _ () s = s
  {-# INLINE setMutableByteArray# #-}
  setOffAddr# _ _ _ () s = s
  {-# INLINE setOffAddr# #-}

instance a ~ b => Unbox (a :~: b) where
  type PrimBase (a :~: b) = ()
  toPrimBase Refl = ()
  fromPrimBase () = Refl


instance Unbox Int where
  type PrimBase Int = Int
  type SizeOf Int = SIZEOF_HSINT
  type Alignment Int = ALIGNMENT_HSINT
  sizeOf# _ = SIZEOF_HSINT#
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_HSINT#
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i# = I# (indexWord8ArrayAsInt# ba# i#)
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# = I# (indexIntArray# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# = I# (indexIntOffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readByteOffMutableByteArray# mba# i# s = case readWord8ArrayAsInt# mba# i# s of
                                             (# s', a# #) -> (# s', I# a# #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readMutableByteArray# mba# i# s = case readIntArray# mba# i# s of
                                      (# s', a# #) -> (# s', I# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readIntOffAddr# mba# i# s of
                             (# s', a# #) -> (# s', I# a# #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i# (I# a#) = writeWord8ArrayAsInt# mba# i# a#
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# (I# a#) = writeIntArray# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (I# a#) = writeIntOffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
#if WORD_SIZE_IN_BITS >= 64
  setMutableByteArray# mba# o# n# (I# a#) = setMutableByteArray# mba# o# n# (I64# a#)
  setOffAddr# addr# o# n# (I# a#) = setOffAddr# addr# o# n# (I64# a#)
#else
  setMutableByteArray# mba# o# n# (I# a#) = setMutableByteArray# mba# o# n# (I32# a#)
  setOffAddr# addr# o# n# (I# a#) = setOffAddr# addr# o# n# (I32# a#)
#endif
  {-# INLINE setMutableByteArray# #-}
  {-# INLINE setOffAddr# #-}

instance Unbox Int8 where
  type PrimBase Int8 = Int8
  type SizeOf Int8 = SIZEOF_INT8
  type Alignment Int8 = ALIGNMENT_INT8
  sizeOf# _ = SIZEOF_INT8#
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_INT8#
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i# = I8# (indexInt8Array# ba# i#)
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# = I8# (indexInt8Array# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# = I8# (indexInt8OffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readByteOffMutableByteArray# mba# i# s = case readInt8Array# mba# i# s of
                                             (# s', a# #) -> (# s', I8# a# #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readMutableByteArray# mba# i# s = case readInt8Array# mba# i# s of
                                      (# s', a# #) -> (# s', I8# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readInt8OffAddr# mba# i# s of
                             (# s', a# #) -> (# s', I8# a# #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i# (I8# a#) = writeInt8Array# mba# i# a#
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# (I8# a#) = writeInt8Array# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (I8# a#) = writeInt8OffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
  setMutableByteArray# mba# i# n# (I8# a#) = setByteArray# mba# i# n# a#
  {-# INLINE setMutableByteArray# #-}
  setOffAddr# addr# o# n# a = unsafePrimBase_ (memsetInt8Addr# addr# o# n# a)
  {-# INLINE setOffAddr# #-}

instance Unbox Int16 where
  type PrimBase Int16 = Int16
  type SizeOf Int16 = SIZEOF_INT16
  type Alignment Int16 = ALIGNMENT_INT16
  sizeOf# _ = SIZEOF_INT16#
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_INT16#
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i# = I16# (indexWord8ArrayAsInt16# ba# i#)
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# = I16# (indexInt16Array# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# = I16# (indexInt16OffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readByteOffMutableByteArray# mba# i# s = case readWord8ArrayAsInt16# mba# i# s of
                                             (# s', a# #) -> (# s', I16# a# #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readMutableByteArray# mba# i# s = case readInt16Array# mba# i# s of
                                      (# s', a# #) -> (# s', I16# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readInt16OffAddr# mba# i# s of
                             (# s', a# #) -> (# s', I16# a# #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i# (I16# a#) = writeWord8ArrayAsInt16# mba# i# a#
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# (I16# a#) = writeInt16Array# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (I16# a#) = writeInt16OffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
  setMutableByteArray# mba# o# n# a = unsafePrimBase_ (memsetInt16MutableByteArray# mba# o# n# a)
  {-# INLINE setMutableByteArray# #-}
  setOffAddr# addr# o# n# a = unsafePrimBase_ (memsetInt16Addr# addr# o# n# a)
  {-# INLINE setOffAddr# #-}

instance Unbox Int32 where
  type PrimBase Int32 = Int32
  type SizeOf Int32 = SIZEOF_INT32
  type Alignment Int32 = ALIGNMENT_INT32
  sizeOf# _ = SIZEOF_INT32#
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_INT32#
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i# = I32# (indexWord8ArrayAsInt32# ba# i#)
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# = I32# (indexInt32Array# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# = I32# (indexInt32OffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readByteOffMutableByteArray# mba# i# s = case readWord8ArrayAsInt32# mba# i# s of
                                             (# s', a# #) -> (# s', I32# a# #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readMutableByteArray# mba# i# s = case readInt32Array# mba# i# s of
                                      (# s', a# #) -> (# s', I32# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readInt32OffAddr# mba# i# s of
                             (# s', a# #) -> (# s', I32# a# #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i# (I32# a#) = writeWord8ArrayAsInt32# mba# i# a#
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# (I32# a#) = writeInt32Array# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (I32# a#) = writeInt32OffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
  setMutableByteArray# mba# o# n# a = unsafePrimBase_ (memsetInt32MutableByteArray# mba# o# n# a)
  {-# INLINE setMutableByteArray# #-}
  setOffAddr# addr# o# n# a = unsafePrimBase_ (memsetInt32Addr# addr# o# n# a)
  {-# INLINE setOffAddr# #-}

instance Unbox Int64 where
  type PrimBase Int64 = Int64
  type SizeOf Int64 = SIZEOF_INT64
  type Alignment Int64 = ALIGNMENT_INT64
  sizeOf# _ = SIZEOF_INT64#
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_INT64#
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i# = I64# (indexWord8ArrayAsInt64# ba# i#)
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# = I64# (indexInt64Array# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# = I64# (indexInt64OffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readByteOffMutableByteArray# mba# i# s = case readWord8ArrayAsInt64# mba# i# s of
                                             (# s', a# #) -> (# s', I64# a# #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readMutableByteArray# mba# i# s = case readInt64Array# mba# i# s of
                                      (# s', a# #) -> (# s', I64# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readInt64OffAddr# mba# i# s of
                             (# s', a# #) -> (# s', I64# a# #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i# (I64# a#) = writeWord8ArrayAsInt64# mba# i# a#
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# (I64# a#) = writeInt64Array# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (I64# a#) = writeInt64OffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
  setMutableByteArray# mba# o# n# a = unsafePrimBase_ (memsetInt64MutableByteArray# mba# o# n# a)
  {-# INLINE setMutableByteArray# #-}
  setOffAddr# addr# o# n# a = unsafePrimBase_ (memsetInt64Addr# addr# o# n# a)
  {-# INLINE setOffAddr# #-}


instance Unbox Word where
  type PrimBase Word = Word
  type SizeOf Word = SIZEOF_HSWORD
  type Alignment Word = ALIGNMENT_HSWORD
  sizeOf# _ = SIZEOF_HSWORD#
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_HSWORD#
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i# = W# (indexWord8ArrayAsWord# ba# i#)
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# = W# (indexWordArray# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# = W# (indexWordOffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readByteOffMutableByteArray# mba# i# s = case readWord8ArrayAsWord# mba# i# s of
                                             (# s', a# #) -> (# s', W# a# #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readMutableByteArray# mba# i# s = case readWordArray# mba# i# s of
                                      (# s', a# #) -> (# s', W# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readWordOffAddr# mba# i# s of
                             (# s', a# #) -> (# s', W# a# #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i# (W# a#) = writeWord8ArrayAsWord# mba# i# a#
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# (W# a#) = writeWordArray# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (W# a#) = writeWordOffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
#if WORD_SIZE_IN_BITS >= 64
  setMutableByteArray# mba# o# n# (W# a#) = setMutableByteArray# mba# o# n# (W64# a#)
  setOffAddr# addr# o# n# (W# a#) = setOffAddr# addr# o# n# (W64# a#)
#else
  setMutableByteArray# mba# o# n# (W# a#) = setMutableByteArray# mba# o# n# (W32# a#)
  setOffAddr# addr# o# n# (W# a#) = setOffAddr# addr# o# n# (W32# a#)
#endif
  {-# INLINE setMutableByteArray# #-}
  {-# INLINE setOffAddr# #-}

instance Unbox Word8 where
  type PrimBase Word8 = Word8
  type SizeOf Word8 = SIZEOF_WORD8
  type Alignment Word8 = ALIGNMENT_WORD8
  sizeOf# _ = SIZEOF_WORD8#
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_WORD8#
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i# = W8# (indexWord8Array# ba# i#)
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# = W8# (indexWord8Array# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# = W8# (indexWord8OffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readByteOffMutableByteArray# mba# i# s = case readWord8Array# mba# i# s of
                                             (# s', a# #) -> (# s', W8# a# #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readMutableByteArray# mba# i# s = case readWord8Array# mba# i# s of
                                      (# s', a# #) -> (# s', W8# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readWord8OffAddr# mba# i# s of
                             (# s', a# #) -> (# s', W8# a# #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i# (W8# a#) = writeWord8Array# mba# i# a#
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# (W8# a#) = writeWord8Array# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (W8# a#) = writeWord8OffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
  setMutableByteArray# mba# i# n# (W8# a#) = setByteArray# mba# i# n# (word2Int# a#)
  {-# INLINE setMutableByteArray# #-}
  setOffAddr# addr# o# n# a = unsafePrimBase_ (memsetWord8Addr# addr# o# n# a)
  {-# INLINE setOffAddr# #-}

instance Unbox Word16 where
  type PrimBase Word16 = Word16
  type SizeOf Word16 = SIZEOF_WORD16
  type Alignment Word16 = ALIGNMENT_WORD16
  sizeOf# _ = SIZEOF_WORD16#
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_WORD16#
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i# = W16# (indexWord8ArrayAsWord16# ba# i#)
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# = W16# (indexWord16Array# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# = W16# (indexWord16OffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readByteOffMutableByteArray# mba# i# s = case readWord8ArrayAsWord16# mba# i# s of
                                             (# s', a# #) -> (# s', W16# a# #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readMutableByteArray# mba# i# s = case readWord16Array# mba# i# s of
                                      (# s', a# #) -> (# s', W16# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readWord16OffAddr# mba# i# s of
                             (# s', a# #) -> (# s', W16# a# #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i# (W16# a#) = writeWord8ArrayAsWord16# mba# i# a#
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# (W16# a#) = writeWord16Array# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (W16# a#) = writeWord16OffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
  setMutableByteArray# mba# o# n# a =
    unsafePrimBase_ (memsetWord16MutableByteArray# mba# o# n# a)
  {-# INLINE setMutableByteArray# #-}
  setOffAddr# addr# o# n# a = unsafePrimBase_ (memsetWord16Addr# addr# o# n# a)
  {-# INLINE setOffAddr# #-}

instance Unbox Word32 where
  type PrimBase Word32 = Word32
  type SizeOf Word32 = SIZEOF_WORD32
  type Alignment Word32 = ALIGNMENT_WORD32
  sizeOf# _ = SIZEOF_WORD32#
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_WORD32#
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i# = W32# (indexWord8ArrayAsWord32# ba# i#)
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# = W32# (indexWord32Array# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# = W32# (indexWord32OffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readByteOffMutableByteArray# mba# i# s = case readWord8ArrayAsWord32# mba# i# s of
                                             (# s', a# #) -> (# s', W32# a# #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readMutableByteArray# mba# i# s = case readWord32Array# mba# i# s of
                                      (# s', a# #) -> (# s', W32# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readWord32OffAddr# mba# i# s of
                             (# s', a# #) -> (# s', W32# a# #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i# (W32# a#) = writeWord8ArrayAsWord32# mba# i# a#
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# (W32# a#) = writeWord32Array# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (W32# a#) = writeWord32OffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
  setMutableByteArray# mba# o# n# a = unsafePrimBase_ (memsetWord32MutableByteArray# mba# o# n# a)
  {-# INLINE setMutableByteArray# #-}
  setOffAddr# addr# o# n# a = unsafePrimBase_ (memsetWord32Addr# addr# o# n# a)
  {-# INLINE setOffAddr# #-}

instance Unbox Word64 where
  type PrimBase Word64 = Word64
  type SizeOf Word64 = SIZEOF_WORD64
  type Alignment Word64 = ALIGNMENT_WORD64
  sizeOf# _ = SIZEOF_WORD64#
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_WORD64#
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i# = W64# (indexWord8ArrayAsWord64# ba# i#)
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# = W64# (indexWord64Array# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# = W64# (indexWord64OffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readByteOffMutableByteArray# mba# i# s = case readWord8ArrayAsWord64# mba# i# s of
                                             (# s', a# #) -> (# s', W64# a# #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readMutableByteArray# mba# i# s = case readWord64Array# mba# i# s of
                                      (# s', a# #) -> (# s', W64# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readWord64OffAddr# mba# i# s of
                             (# s', a# #) -> (# s', W64# a# #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i# (W64# a#) = writeWord8ArrayAsWord64# mba# i# a#
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# (W64# a#) = writeWord64Array# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (W64# a#) = writeWord64OffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
  setMutableByteArray# mba# o# n# a = unsafePrimBase_ (memsetWord64MutableByteArray# mba# o# n# a)
  {-# INLINE setMutableByteArray# #-}
  setOffAddr# addr# o# n# a = unsafePrimBase_ (memsetWord64Addr# addr# o# n# a)
  {-# INLINE setOffAddr# #-}


instance Unbox Float where
  type PrimBase Float = Float
  type SizeOf Float = SIZEOF_FLOAT
  type Alignment Float = ALIGNMENT_FLOAT
  sizeOf# _ = SIZEOF_FLOAT#
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_FLOAT#
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i# = F# (indexWord8ArrayAsFloat# ba# i#)
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# = F# (indexFloatArray# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# = F# (indexFloatOffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readByteOffMutableByteArray# mba# i# s = case readWord8ArrayAsFloat# mba# i# s of
                                             (# s', a# #) -> (# s', F# a# #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readMutableByteArray# mba# i# s = case readFloatArray# mba# i# s of
                                      (# s', a# #) -> (# s', F# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readFloatOffAddr# mba# i# s of
                             (# s', a# #) -> (# s', F# a# #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i# (F# a#) = writeWord8ArrayAsFloat# mba# i# a#
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# (F# a#) = writeFloatArray# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (F# a#) = writeFloatOffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
  setMutableByteArray# mba# o# n# (F# f#) =
    unsafePrimBase_ (memsetWord32MutableByteArray# mba# o# n# (W32# (floatToWord32# f#)))
  {-# INLINE setMutableByteArray# #-}
  setOffAddr# addr# o# n# (F# f#) =
    unsafePrimBase_ (memsetWord32Addr# addr# o# n# (W32# (floatToWord32# f#)))

instance Unbox Double where
  type PrimBase Double = Double
  type SizeOf Double = SIZEOF_DOUBLE
  type Alignment Double = ALIGNMENT_DOUBLE
  sizeOf# _ = SIZEOF_DOUBLE#
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_DOUBLE#
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i# = D# (indexWord8ArrayAsDouble# ba# i#)
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# = D# (indexDoubleArray# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# = D# (indexDoubleOffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readByteOffMutableByteArray# mba# i# s = case readWord8ArrayAsDouble# mba# i# s of
                                             (# s', a# #) -> (# s', D# a# #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readMutableByteArray# mba# i# s = case readDoubleArray# mba# i# s of
                                      (# s', a# #) -> (# s', D# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readDoubleOffAddr# mba# i# s of
                             (# s', a# #) -> (# s', D# a# #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i# (D# a#) = writeWord8ArrayAsDouble# mba# i# a#
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# (D# a#) = writeDoubleArray# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (D# a#) = writeDoubleOffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
  setMutableByteArray# mba# o# n# (D# d#) =
    unsafePrimBase_ (memsetWord64MutableByteArray# mba# o# n# (W64# (doubleToWord64# d#)))
  {-# INLINE setMutableByteArray# #-}
  setOffAddr# addr# o# n# (D# d#) =
    unsafePrimBase_ (memsetWord64Addr# addr# o# n# (W64# (doubleToWord64# d#)))
  {-# INLINE setOffAddr# #-}

bool2Int# :: Bool -> Int#
bool2Int# b = if b then 1# else 0#
{-# INLINE bool2Int# #-}

int2Bool# :: Int# -> Bool
int2Bool# i# = isTrue# (i# /=# 0#) -- tagToEnum# (i# /=# 0#) -- (andI# i# 1#)
{-# INLINE int2Bool# #-}

instance Unbox Bool where
  type PrimBase Bool = Int8
  fromPrimBase (I8# i#) = int2Bool# i#
  {-# INLINE fromPrimBase #-}
  toPrimBase b = I8# (bool2Int# b)
  {-# INLINE toPrimBase #-}

instance Unbox Char where
  type PrimBase Char = Char
  type SizeOf Char = SIZEOF_HSCHAR
  type Alignment Char = ALIGNMENT_HSCHAR
  sizeOf# _ = SIZEOF_HSCHAR#
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_HSCHAR#
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i# = C# (indexWord8ArrayAsWideChar# ba# i#)
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# = C# (indexWideCharArray# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# = C# (indexWideCharOffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readByteOffMutableByteArray# mba# i# s = case readWord8ArrayAsWideChar# mba# i# s of
                                             (# s', a# #) -> (# s', C# a# #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readMutableByteArray# mba# i# s = case readWideCharArray# mba# i# s of
                                      (# s', a# #) -> (# s', C# a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readWideCharOffAddr# mba# i# s of
                             (# s', a# #) -> (# s', C# a# #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i# (C# a#) = writeWord8ArrayAsWideChar# mba# i# a#
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# (C# a#) = writeWideCharArray# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (C# a#) = writeWideCharOffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
  setMutableByteArray# mba# o# n# (C# a#) = setMutableByteArray# mba# o# n# (I32# (ord# a#))
  {-# INLINE setMutableByteArray# #-}
  setOffAddr# addr# o# n# (C# a#) = setOffAddr# addr# o# n# (I32# (ord# a#))
  {-# INLINE setOffAddr# #-}

instance Unbox (Ptr a) where
  type PrimBase (Ptr a) = Ptr a
  type SizeOf (Ptr a) = SIZEOF_HSPTR
  type Alignment (Ptr a) = ALIGNMENT_HSPTR
  sizeOf# _ = SIZEOF_HSINT#
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_HSINT#
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i# = Ptr (indexWord8ArrayAsAddr# ba# i#)
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# = Ptr (indexAddrArray# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# = Ptr (indexAddrOffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readByteOffMutableByteArray# mba# i# s = case readWord8ArrayAsAddr# mba# i# s of
                                             (# s', a# #) -> (# s', Ptr a# #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readMutableByteArray# mba# i# s = case readAddrArray# mba# i# s of
                                      (# s', a# #) -> (# s', Ptr a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readAddrOffAddr# mba# i# s of
                             (# s', a# #) -> (# s', Ptr a# #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i# (Ptr a#) = writeWord8ArrayAsAddr# mba# i# a#
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# (Ptr a#) = writeAddrArray# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (Ptr a#) = writeAddrOffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
#if SIZEOF_HSPTR == SIZEOF_INT64
  setMutableByteArray# mba# o# n# (Ptr a#) = setMutableByteArray# mba# o# n# (I64# (addr2Int# a#))
  setOffAddr# addr# o# n# (Ptr a#) = setOffAddr# addr# o# n# (I64# (addr2Int# a#))
#elif SIZEOF_HSPTR == SIZEOF_INT32
  setMutableByteArray# mba# o# n# (Ptr a#) = setMutableByteArray# mba# o# n# (I32# (addr2Int# a#))
  setOffAddr# addr# o# n# (Ptr a#) = setOffAddr# addr# o# n# (I32# (addr2Int# a#))
#else
#error Ptr is of unsupported size SIZEOF_HSPTR
#endif
  {-# INLINE setMutableByteArray# #-}
  {-# INLINE setOffAddr# #-}

instance Unbox (FunPtr a) where
  type PrimBase (FunPtr a) = Ptr a
  toPrimBase (FunPtr addr#) = Ptr addr#
  fromPrimBase (Ptr addr#) = FunPtr addr#


instance Unbox (StablePtr a) where
  type PrimBase (StablePtr a) = StablePtr a
  type SizeOf (StablePtr a) = SIZEOF_HSSTABLEPTR
  type Alignment (StablePtr a) = ALIGNMENT_HSSTABLEPTR
  sizeOf# _ = SIZEOF_HSINT#
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_HSINT#
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i# = StablePtr (indexWord8ArrayAsStablePtr# ba# i#)
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# = StablePtr (indexStablePtrArray# ba# i#)
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# = StablePtr (indexStablePtrOffAddr# addr# i#)
  {-# INLINE indexOffAddr# #-}
  readByteOffMutableByteArray# mba# i# s = case readWord8ArrayAsStablePtr# mba# i# s of
                                             (# s', a# #) -> (# s', StablePtr a# #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readMutableByteArray# mba# i# s = case readStablePtrArray# mba# i# s of
                                      (# s', a# #) -> (# s', StablePtr a# #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# mba# i# s = case readStablePtrOffAddr# mba# i# s of
                             (# s', a# #) -> (# s', StablePtr a# #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i# (StablePtr a#) = writeWord8ArrayAsStablePtr# mba# i# a#
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# (StablePtr a#) = writeStablePtrArray# mba# i# a#
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# mba# i# (StablePtr a#) = writeStablePtrOffAddr# mba# i# a#
  {-# INLINE writeOffAddr# #-}
#if SIZEOF_HSSTABLEPTR == SIZEOF_INT64
  setMutableByteArray# mba# o# n# (StablePtr a#) = setMutableByteArray# mba# o# n# (I64# (unsafeCoerce# a#))
  setOffAddr# addr# o# n# (StablePtr a#) = setOffAddr# addr# o# n# (I64# (unsafeCoerce# a#))
#elif SIZEOF_HSSTABLEPTR == SIZEOF_INT32
  setMutableByteArray# mba# o# n# (StablePtr a#) = setMutableByteArray# mba# o# n# (I32# (unsafeCoerce# a#))
  setOffAddr# addr# o# n# (StablePtr a#) = setOffAddr# addr# o# n# (I32# (unsafeCoerce# a#))
#else
#error StablePtr is of unsupported size SIZEOF_HSSTABLEPTR
#endif
  {-# INLINE setMutableByteArray# #-}
  {-# INLINE setOffAddr# #-}


instance Unbox IntPtr where
  type PrimBase IntPtr = Int

instance Unbox WordPtr where
  type PrimBase WordPtr = Word

instance Unbox CBool where
  type PrimBase CBool = HTYPE_BOOL

instance Unbox CChar where
  type PrimBase CChar = HTYPE_CHAR

instance Unbox CSChar where
  type PrimBase CSChar = HTYPE_SIGNED_CHAR

instance Unbox CUChar where
  type PrimBase CUChar = HTYPE_UNSIGNED_CHAR

instance Unbox CShort where
  type PrimBase CShort = HTYPE_SHORT

instance Unbox CUShort where
  type PrimBase CUShort = HTYPE_UNSIGNED_SHORT

instance Unbox CInt where
  type PrimBase CInt = HTYPE_INT

instance Unbox CUInt where
  type PrimBase CUInt = HTYPE_UNSIGNED_INT

instance Unbox CLong where
  type PrimBase CLong = HTYPE_LONG

instance Unbox CULong where
  type PrimBase CULong = HTYPE_UNSIGNED_LONG

instance Unbox CLLong where
  type PrimBase CLLong = HTYPE_LONG_LONG

instance Unbox CULLong where
  type PrimBase CULLong = HTYPE_UNSIGNED_LONG_LONG

instance Unbox CPtrdiff where
  type PrimBase CPtrdiff = HTYPE_PTRDIFF_T

instance Unbox CSize where
  type PrimBase CSize = HTYPE_SIZE_T

instance Unbox CWchar where
  type PrimBase CWchar = HTYPE_WCHAR_T

instance Unbox CSigAtomic where
  type PrimBase CSigAtomic = HTYPE_SIG_ATOMIC_T

instance Unbox CIntPtr where
  type PrimBase CIntPtr = HTYPE_INTPTR_T

instance Unbox CUIntPtr where
  type PrimBase CUIntPtr = HTYPE_UINTPTR_T

instance Unbox CIntMax where
  type PrimBase CIntMax = HTYPE_INTMAX_T

instance Unbox CUIntMax where
  type PrimBase CUIntMax = HTYPE_UINTMAX_T

instance Unbox CFloat where
  type PrimBase CFloat = HTYPE_FLOAT

instance Unbox CDouble where
  type PrimBase CDouble = HTYPE_DOUBLE


instance Unbox Fd where
  type PrimBase Fd = CInt

instance Unbox Errno where
  type PrimBase Errno = CInt


#if defined(HTYPE_DEV_T)
instance Unbox CDev where
  type PrimBase CDev = HTYPE_DEV_T
#endif
#if defined(HTYPE_INO_T)
instance Unbox CIno where
  type PrimBase CIno = HTYPE_INO_T
#endif
#if defined(HTYPE_MODE_T)
instance Unbox CMode where
  type PrimBase CMode = HTYPE_MODE_T
#endif
#if defined(HTYPE_OFF_T)
instance Unbox COff where
  type PrimBase COff = HTYPE_OFF_T
#endif
#if defined(HTYPE_PID_T)
instance Unbox CPid where
  type PrimBase CPid = HTYPE_PID_T
#endif
#if defined(HTYPE_SSIZE_T)
instance Unbox CSsize where
  type PrimBase CSsize = HTYPE_SSIZE_T
#endif
#if defined(HTYPE_GID_T)
instance Unbox CGid where
  type PrimBase CGid = HTYPE_GID_T
#endif
#if defined(HTYPE_NLINK_T)
instance Unbox CNlink where
  type PrimBase CNlink = HTYPE_NLINK_T
#endif
#if defined(HTYPE_UID_T)
instance Unbox CUid where
  type PrimBase CUid = HTYPE_UID_T
#endif
#if defined(HTYPE_CC_T)
instance Unbox CCc where
  type PrimBase CCc = HTYPE_CC_T
#endif
#if defined(HTYPE_SPEED_T)
instance Unbox CSpeed where
  type PrimBase CSpeed = HTYPE_SPEED_T
#endif
#if defined(HTYPE_TCFLAG_T)
instance Unbox CTcflag where
  type PrimBase CTcflag = HTYPE_TCFLAG_T
#endif
#if defined(HTYPE_RLIM_T)
instance Unbox CRLim where
  type PrimBase CRLim = HTYPE_RLIM_T
#endif

instance Unbox a => Unbox (Max a) where
  type PrimBase (Max a) = a
instance Unbox a => Unbox (Min a) where
  type PrimBase (Min a) = a
instance Unbox a => Unbox (Data.Semigroup.First a) where
  type PrimBase (Data.Semigroup.First a) = a
instance Unbox a => Unbox (Data.Semigroup.Last a) where
  type PrimBase (Data.Semigroup.Last a) = a
instance (Unbox a, Unbox b) => Unbox (Arg a b) where
  type PrimBase (Arg a b) = (a, b)
  toPrimBase (Arg a b) = (a, b)
  fromPrimBase (a, b) = Arg a b

#if __GLASGOW_HASKELL__ >= 800
instance Unbox a => Unbox (Const a b) where
  type PrimBase (Const a b) = a
#endif /* __GLASGOW_HASKELL__ >= 800 */


#if __GLASGOW_HASKELL__ >= 802

instance a ~ b => Unbox (a :~~: b) where
  type PrimBase (a :~~: b) = ()
  toPrimBase HRefl = ()
  fromPrimBase () = HRefl

#if defined(HTYPE_BLKSIZE_T)
instance Unbox CBlkSize where
  type PrimBase CBlkSize = HTYPE_BLKSIZE_T
#endif
#if defined(HTYPE_BLKCNT_T)
instance Unbox CBlkCnt where
  type PrimBase CBlkCnt = HTYPE_BLKCNT_T
#endif
#if defined(HTYPE_CLOCKID_T)
instance Unbox CClockId where
  type PrimBase CClockId = HTYPE_CLOCKID_T
#endif
#if defined(HTYPE_FSBLKCNT_T)
instance Unbox CFsBlkCnt where
  type PrimBase CFsBlkCnt = HTYPE_FSBLKCNT_T
#endif
#if defined(HTYPE_FSFILCNT_T)
instance Unbox CFsFilCnt where
  type PrimBase CFsFilCnt = HTYPE_FSFILCNT_T
#endif
#if defined(HTYPE_ID_T)
instance Unbox CId where
  type PrimBase CId = HTYPE_ID_T
#endif
#if defined(HTYPE_KEY_T)
instance Unbox CKey where
  type PrimBase CKey = HTYPE_KEY_T
#endif
#if defined(HTYPE_TIMER_T)
instance Unbox CTimer where
  type PrimBase CTimer = HTYPE_TIMER_T
#endif

#if __GLASGOW_HASKELL__ >= 810

#if defined(HTYPE_SOCKLEN_T)
instance Unbox CSocklen where
  type PrimBase CSocklen = HTYPE_SOCKLEN_T
#endif
#if defined(HTYPE_NFDS_T)
instance Unbox CNfds where
  type PrimBase CNfds = HTYPE_NFDS_T
#endif

#endif /* __GLASGOW_HASKELL__ >= 810 */

#if __GLASGOW_HASKELL__ >= 806
instance Unbox (f a) => Unbox (Ap f a) where
  type PrimBase (Ap f a) = f a
#endif /* __GLASGOW_HASKELL__ >= 806 */


#endif /* __GLASGOW_HASKELL__ >= 802 */


instance (Unbox (f a), Unbox (g a)) => Unbox (Functor.Product f g a) where
  type PrimBase (Functor.Product f g a) = (f a, g a)
  toPrimBase (Functor.Pair fa ga) = (fa, ga)
  {-# INLINE toPrimBase #-}
  fromPrimBase (fa, ga) = Functor.Pair fa ga
  {-# INLINE fromPrimBase #-}


instance Unbox (f (g a)) => Unbox (Compose f g a) where
  type PrimBase (Compose f g a) = f (g a)

instance Unbox a => Unbox (Identity a) where
  type PrimBase (Identity a) = a

instance Unbox (f a) => Unbox (Alt f a) where
  type PrimBase (Alt f a) = f a

instance Unbox Ordering where
  type PrimBase Ordering = Int8
  toPrimBase o = I8# (fromOrdering# o)
  {-# INLINE toPrimBase #-}
  fromPrimBase (I8# i#) = toOrdering# i#
  {-# INLINE fromPrimBase #-}

instance Unbox IODeviceType where
  type PrimBase IODeviceType = Int8
  toPrimBase =
    \case
      Directory -> 0
      Stream -> 1
      RegularFile -> 2
      RawDevice -> 3
  {-# INLINE toPrimBase #-}
  fromPrimBase =
    \case
      0 -> Directory
      1 -> Stream
      2 -> RegularFile
      _ -> RawDevice
  {-# INLINE fromPrimBase #-}

instance Unbox SeekMode where
  type PrimBase SeekMode = Int8
  toPrimBase = \case
    AbsoluteSeek -> 0
    RelativeSeek -> 1
    SeekFromEnd  -> 2
  {-# INLINE toPrimBase #-}
  fromPrimBase = \case
    0 -> AbsoluteSeek
    1 -> RelativeSeek
    _ -> SeekFromEnd
  {-# INLINE fromPrimBase #-}

instance Unbox BlockReason where
  type PrimBase BlockReason = Int8
  toPrimBase =
    \case
      BlockedOnMVar -> 0
      BlockedOnBlackHole -> 1
      BlockedOnException -> 2
      BlockedOnSTM -> 3
      BlockedOnForeignCall -> 4
      BlockedOnOther -> 5
  {-# INLINE toPrimBase #-}
  fromPrimBase =
    \case
      0 -> BlockedOnMVar
      1 -> BlockedOnBlackHole
      2 -> BlockedOnException
      3 -> BlockedOnSTM
      4 -> BlockedOnForeignCall
      _ -> BlockedOnOther
  {-# INLINE fromPrimBase #-}


instance Unbox ThreadStatus where
  type PrimBase ThreadStatus = Int8
  toPrimBase =
    \case
      ThreadRunning -> 0x00
      ThreadFinished -> 0x10
      ThreadBlocked br -> 0x20 .|. toPrimBase br
      ThreadDied -> 0x30
  {-# INLINE toPrimBase #-}
  fromPrimBase =
    \case
      0x00 -> ThreadRunning
      0x10 -> ThreadFinished
      0x30 -> ThreadDied
      x -> ThreadBlocked $ fromPrimBase (x .&. 0xf)
  {-# INLINE fromPrimBase #-}

instance Unbox IOMode where
  type PrimBase IOMode = Int8
  toPrimBase =
    \case
      ReadMode -> 0
      WriteMode -> 1
      AppendMode -> 2
      ReadWriteMode -> 3
  {-# INLINE toPrimBase #-}
  fromPrimBase =
    \case
      0 -> ReadMode
      1 -> WriteMode
      2 -> AppendMode
      _ -> ReadWriteMode
  {-# INLINE fromPrimBase #-}

instance Unbox BufferMode where
  type PrimBase BufferMode = (Int8, Maybe Int)
  toPrimBase =
    \case
      NoBuffering -> (0, Nothing)
      LineBuffering -> (1, Nothing)
      BlockBuffering mb -> (2, mb)
  {-# INLINE toPrimBase #-}
  fromPrimBase =
    \case
      (0, _) -> NoBuffering
      (1, _) -> LineBuffering
      (_, mb) -> BlockBuffering mb
  {-# INLINE fromPrimBase #-}

instance Unbox Newline where
  type PrimBase Newline = Int8
  toPrimBase =
    \case
      LF -> 0
      CRLF -> 1
  {-# INLINE toPrimBase #-}
  fromPrimBase =
    \case
      0 -> LF
      _ -> CRLF
  {-# INLINE fromPrimBase #-}

instance Unbox NewlineMode where
  type PrimBase NewlineMode = Int8
  toPrimBase (NewlineMode i o) =
    (toPrimBase i `unsafeShiftL` 1) .|. toPrimBase o
  {-# INLINE toPrimBase #-}
  fromPrimBase p =
    NewlineMode
      (fromPrimBase ((p `unsafeShiftR` 1) .&. 1))
      (fromPrimBase (p .&. 1))
  {-# INLINE fromPrimBase #-}

instance Unbox GeneralCategory where
  type PrimBase GeneralCategory = Word8
  toPrimBase = fromIntegral . fromEnum
  {-# INLINE toPrimBase #-}
  fromPrimBase p
    | ip > fromEnum (maxBound :: GeneralCategory) = NotAssigned
    | otherwise = toEnum ip
    where
      ip = fromIntegral p
  {-# INLINE fromPrimBase #-}


instance Unbox a => Unbox (Down a) where
  type PrimBase (Down a) = a

instance Unbox a => Unbox (Dual a) where
  type PrimBase (Dual a) = a

instance Unbox a => Unbox (Sum a) where
  type PrimBase (Sum a) = a

instance Unbox a => Unbox (Product a) where
  type PrimBase (Product a) = a

instance Unbox All where
  type PrimBase All = Bool

instance Unbox Any where
  type PrimBase Any = Bool

instance Unbox Fingerprint where
  type PrimBase Fingerprint = (Word64, Word64)
  type Alignment Fingerprint = Alignment Word64
  alignment# _ = alignment# (proxy# :: Proxy# Word64)
  toPrimBase (Fingerprint a b) = (a, b)
  fromPrimBase (a, b) = Fingerprint a b

instance Unbox a => Unbox (Ratio a) where
  type PrimBase (Ratio a) = (a, a)
  type Alignment (Ratio a) = Alignment a
  alignment# _ = alignment# (proxy# :: Proxy# a)
  toPrimBase (a :% b) = (a, b)
  fromPrimBase (a, b) = a :% b

instance Unbox a => Unbox (Complex a) where
  type PrimBase (Complex a) = (a, a)
  type Alignment (Complex a) = Alignment a
  alignment# _ = alignment# (proxy# :: Proxy# a)
  toPrimBase (a :+ b) = (a, b)
  fromPrimBase (a, b) = a :+ b

instance (Unbox a, Unbox b) => Unbox (a, b) where
  type PrimBase (a, b) = (a, b)
  type SizeOf (a, b) = SizeOf a + SizeOf b
  type Alignment (a, b) = Alignment a + Alignment b
  sizeOf# _ = sizeOf# (proxy# :: Proxy# a) +# sizeOf# (proxy# :: Proxy# b)
  {-# INLINE sizeOf# #-}
  alignment# _ =
    alignment# (proxy# :: Proxy# a) +# alignment# (proxy# :: Proxy# b)
  {-# INLINE alignment# #-}
  indexByteArray# ba# i# =
    let i0# = i# *# sizeOf# (proxy# :: Proxy# (a, b))
     in indexByteOffByteArray# ba# i0#
  {-# INLINE indexByteArray# #-}
  indexByteOffByteArray# ba# i0# =
    let i1# = i0# +# sizeOf# (proxy# :: Proxy# a)
     in (indexByteOffByteArray# ba# i0#, indexByteOffByteArray# ba# i1#)
  {-# INLINE indexByteOffByteArray# #-}
  indexOffAddr# addr# i# =
    let addr0# = addr# `plusAddr#` (i# *# sizeOf# (proxy# :: Proxy# (a, b)))
        addr1# = addr0# `plusAddr#` sizeOf# (proxy# :: Proxy# a)
     in (indexOffAddr# addr0# 0#, indexOffAddr# addr1# 0#)
  {-# INLINE indexOffAddr# #-}
  readMutableByteArray# mba# i# =
    let i0# = i# *# sizeOf# (proxy# :: Proxy# (a, b))
     in readByteOffMutableByteArray# mba# i0#
  {-# INLINE readMutableByteArray# #-}
  readByteOffMutableByteArray# mba# i0# s =
    let i1# = i0# +# sizeOf# (proxy# :: Proxy# a)
     in case readByteOffMutableByteArray# mba# i0# s of
          (# s', a0 #) ->
            case readByteOffMutableByteArray# mba# i1# s' of
              (# s'', a1 #) -> (# s'', (a0, a1) #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readOffAddr# addr# i# s =
    let addr0# = addr# `plusAddr#` (i# *# sizeOf# (proxy# :: Proxy# (a, b)))
        addr1# = addr0# `plusAddr#` sizeOf# (proxy# :: Proxy# a)
    in case readOffAddr# addr0# 0# s of
         (# s', a0 #) ->
           case readOffAddr# addr1# 0# s' of
             (# s'', a1 #) -> (# s'', (a0, a1) #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i0# (a0, a1) s =
    let i1# = i0# +# sizeOf# (proxy# :: Proxy# a)
    in writeByteOffMutableByteArray# mba# i1# a1 (writeByteOffMutableByteArray# mba# i0# a0 s)
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# a =
    let i0# = i# *# sizeOf# (proxy# :: Proxy# (a, b))
    in writeByteOffMutableByteArray# mba# i0# a
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# addr# i# (a0, a1) s =
    let addr0# = addr# `plusAddr#` (i# *# sizeOf# (proxy# :: Proxy# (a, b)))
        addr1# = addr0# `plusAddr#` sizeOf# (proxy# :: Proxy# a)
    in writeOffAddr# addr1# 0# a1 (writeOffAddr# addr0# 0# a0 s)
  {-# INLINE writeOffAddr# #-}
  setMutableByteArray# = setMutableByteArrayLoop#
    -- TODO: optimize with rewrite rules?
    --  | a0 == a1 = setMutableByteArray# mba# (o# *# 2#) (n# *# 2#) a0 s
  {-# INLINE setMutableByteArray# #-}
  setOffAddr# = setOffAddrLoop#
  {-# INLINE setOffAddr# #-}
  --   | a0 == a1 = setOffAddr# addr# (o# *# 2#) (n# *# 2#) a0 s


instance (Unbox a, Unbox b, Unbox c) => Unbox (a, b, c) where
  type PrimBase (a, b, c) = (a, b, c)
  type SizeOf (a, b, c) = SizeOf a + SizeOf b + SizeOf c
  type Alignment (a, b, c) = Alignment a + Alignment b + Alignment c
  sizeOf# _ = sizeOf# (proxy# :: Proxy# a)
           +# sizeOf# (proxy# :: Proxy# b)
           +# sizeOf# (proxy# :: Proxy# c)
  {-# INLINE sizeOf# #-}
  alignment# _ = alignment# (proxy# :: Proxy# a)
              +# alignment# (proxy# :: Proxy# b)
              +# alignment# (proxy# :: Proxy# c)
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i0# =
    let i1# = i0# +# sizeOf# (proxy# :: Proxy# a)
        i2# = i1# +# sizeOf# (proxy# :: Proxy# b)
    in ( indexByteOffByteArray# ba# i0#
       , indexByteOffByteArray# ba# i1#
       , indexByteOffByteArray# ba# i2#
       )
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# =
    let i0# = i# *# sizeOf# (proxy# :: Proxy# (a, b, c))
    in indexByteOffByteArray# ba# i0#
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# =
    let i0# = i# *# sizeOf# (proxy# :: Proxy# (a, b, c))
        i1# = i0# +# sizeOf# (proxy# :: Proxy# a)
        i2# = i1# +# sizeOf# (proxy# :: Proxy# b)
    in ( indexOffAddr# (addr# `plusAddr#` i0#) 0#
       , indexOffAddr# (addr# `plusAddr#` i1#) 0#
       , indexOffAddr# (addr# `plusAddr#` i2#) 0#
       )
  {-# INLINE indexOffAddr# #-}
  readMutableByteArray# mba# i# =
    let i0# = i# *# sizeOf# (proxy# :: Proxy# (a, b, c))
    in readByteOffMutableByteArray# mba# i0#
  {-# INLINE readMutableByteArray# #-}
  readByteOffMutableByteArray# mba# i0# s =
    let i1# = i0# +# sizeOf# (proxy# :: Proxy# a)
        i2# = i1# +# sizeOf# (proxy# :: Proxy# b)
    in case readByteOffMutableByteArray# mba# i0# s  of { (# s0, a0 #) ->
       case readByteOffMutableByteArray# mba# i1# s0 of { (# s1, a1 #) ->
       case readByteOffMutableByteArray# mba# i2# s1 of { (# s2, a2 #) ->
         (# s2, (a0, a1, a2) #)
       }}}
  {-# INLINE readByteOffMutableByteArray# #-}
  readOffAddr# addr# i# s =
    let addr0# = addr#  `plusAddr#` (i# *# sizeOf# (proxy# :: Proxy# (a, b, c)))
        addr1# = addr0# `plusAddr#` sizeOf# (proxy# :: Proxy# a)
        addr2# = addr1# `plusAddr#` sizeOf# (proxy# :: Proxy# b)
    in case readOffAddr# addr0# 0# s  of { (# s0, a0 #) ->
       case readOffAddr# addr1# 0# s0 of { (# s1, a1 #) ->
       case readOffAddr# addr2# 0# s1 of { (# s2, a2 #) ->
         (# s2, (a0, a1, a2) #)
       }}}
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i0# (a0, a1, a2) s =
    let i1# = i0# +# sizeOf# (proxy# :: Proxy# a)
        i2# = i1# +# sizeOf# (proxy# :: Proxy# b)
    in writeByteOffMutableByteArray# mba# i2# a2
       (writeByteOffMutableByteArray# mba# i1# a1
        (writeByteOffMutableByteArray# mba# i0# a0 s))
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# a s =
    let i0# = i# *# sizeOf# (proxy# :: Proxy# (a, b, c))
    in writeByteOffMutableByteArray# mba# i0# a s
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# addr# i# (a0, a1, a2) s =
    let addr0# = addr#  `plusAddr#` (i# *# sizeOf# (proxy# :: Proxy# (a, b, c)))
        addr1# = addr0# `plusAddr#` sizeOf# (proxy# :: Proxy# a)
        addr2# = addr1# `plusAddr#` sizeOf# (proxy# :: Proxy# b)
    in writeOffAddr# addr2# 0# a2
       (writeOffAddr# addr1# 0# a1
        (writeOffAddr# addr0# 0# a0 s))
  {-# INLINE writeOffAddr# #-}
  setMutableByteArray# = setMutableByteArrayLoop#
    --  | a0 == a1 && a1 == a2 = setMutableByteArray# mba# (o# *# 3#) (n# *# 3#) a0 s
  {-# INLINE setMutableByteArray# #-}
  setOffAddr# = setOffAddrLoop#
    --  | a0 == a1 && a1 == a2 = setOffAddr# addr# (o# *# 3#) (n# *# 3#) a0 s
  {-# INLINE setOffAddr# #-}

-- TODO: Write optimized versions for larger tuples
instance (Unbox a, Unbox b, Unbox c, Unbox d) => Unbox (a, b, c, d) where
  type PrimBase (a, b, c, d) = ((a, b), (c, d))
  toPrimBase (a, b, c, d) = ((a, b), (c, d))
  {-# INLINE toPrimBase #-}
  fromPrimBase ((a, b), (c, d)) = (a, b, c, d)
  {-# INLINE fromPrimBase #-}

instance (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e) => Unbox (a, b, c, d, e) where
  type PrimBase (a, b, c, d, e) = ((a, b), (c, d), e)
  toPrimBase (a, b, c, d, e) = ((a, b), (c, d), e)
  {-# INLINE toPrimBase #-}
  fromPrimBase ((a, b), (c, d), e) = (a, b, c, d, e)
  {-# INLINE fromPrimBase #-}

instance (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e, Unbox f) => Unbox (a, b, c, d, e, f) where
  type PrimBase (a, b, c, d, e, f) = ((a, b), (c, d), (e, f))
  toPrimBase (a, b, c, d, e, f) = ((a, b), (c, d), (e, f))
  {-# INLINE toPrimBase #-}
  fromPrimBase ((a, b), (c, d), (e, f)) = (a, b, c, d, e, f)
  {-# INLINE fromPrimBase #-}

instance (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e, Unbox f, Unbox g) => Unbox (a, b, c, d, e, f, g) where
  type PrimBase (a, b, c, d, e, f, g) = ((a, b, c), (d, e, f), g)
  toPrimBase (a, b, c, d, e, f, g) = ((a, b, c), (d, e, f), g)
  {-# INLINE toPrimBase #-}
  fromPrimBase ((a, b, c), (d, e, f), g) = (a, b, c, d, e, f, g)
  {-# INLINE fromPrimBase #-}

instance (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e, Unbox f, Unbox g, Unbox h) =>
  Unbox (a, b, c, d, e, f, g, h) where
  type PrimBase (a, b, c, d, e, f, g, h) = ((a, b, c), (d, e, f), (g, h))
  toPrimBase (a, b, c, d, e, f, g, h) = ((a, b, c), (d, e, f), (g, h))
  {-# INLINE toPrimBase #-}
  fromPrimBase ((a, b, c), (d, e, f), (g, h)) = (a, b, c, d, e, f, g, h)
  {-# INLINE fromPrimBase #-}

instance (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e, Unbox f, Unbox g, Unbox h, Unbox i) =>
  Unbox (a, b, c, d, e, f, g, h, i) where
  type PrimBase (a, b, c, d, e, f, g, h, i) = ((a, b, c), (d, e, f), (g, h, i))
  toPrimBase (a, b, c, d, e, f, g, h, i) = ((a, b, c), (d, e, f), (g, h, i))
  {-# INLINE toPrimBase #-}
  fromPrimBase ((a, b, c), (d, e, f), (g, h, i)) = (a, b, c, d, e, f, g, h, i)
  {-# INLINE fromPrimBase #-}


instance Unbox a => Unbox (Maybe a) where
  type PrimBase (Maybe a) = Maybe a
  type SizeOf (Maybe a) = 1 + SizeOf a
  type Alignment (Maybe a) = 1 + Alignment a
  sizeOf# _ = 1# +# sizeOf# (proxy# :: Proxy# a)
  {-# INLINE sizeOf# #-}
  alignment# _ = 1# +# alignment# (proxy# :: Proxy# a)
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i# =
    case indexInt8Array# ba# i# of
      0# -> Nothing
      _  -> Just (indexByteOffByteArray# ba# (i# +# 1#))
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# =
    indexByteOffByteArray# ba# (i# *# sizeOf# (proxy# :: Proxy# (Maybe a)))
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# =
    let addr0# = addr# `plusAddr#` (i# *# sizeOf# (proxy# :: Proxy# (Maybe a)))
    in case indexInt8OffAddr# addr0# 0# of
      0# -> Nothing
      _  -> Just (indexOffAddr# (addr0# `plusAddr#` 1#) 0#)
  {-# INLINE indexOffAddr# #-}
  readByteOffMutableByteArray# mba# i# s =
    case readInt8Array# mba# i# s of
      (# s', 0# #) -> (# s', Nothing #)
      (# s', _  #) -> case readByteOffMutableByteArray# mba# (i# +# 1#) s' of
                        (# s'', a #) -> (# s'', Just a #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readMutableByteArray# mba# i# =
    let i0# = i# *# sizeOf# (proxy# :: Proxy# (Maybe a))
     in readByteOffMutableByteArray# mba# i0#
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# addr# i# s =
    let addr0# = addr# `plusAddr#` (i# *# sizeOf# (proxy# :: Proxy# (Maybe a)))
    in case readInt8OffAddr# addr0# 0# s of
         (# s', 0# #) -> (# s', Nothing #)
         (# s', _  #) -> case readOffAddr# (addr0# `plusAddr#` 1#) 0# s' of
                           (# s'', a #) -> (# s'', Just a #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i# mVal s =
    case mVal of
      Nothing -> setByteArray# mba# i# (sizeOf# (proxy# :: Proxy# (Maybe a))) 0# s
      Just a  -> writeByteOffMutableByteArray# mba# (i# +# 1#) a (writeInt8Array# mba# i# 1# s)
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# mVal s =
    let k# = sizeOf# (proxy# :: Proxy# (Maybe a))
        i0# = i# *# k# -- Not using writeByteOffMutableByteArray# to avoid k# recomputation
    in case mVal of
         Nothing -> setByteArray# mba# i0# k# 0# s
         Just a  -> writeByteOffMutableByteArray# mba# (i0# +# 1#) a (writeInt8Array# mba# i0# 1# s)
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# addr# i# mVal s =
    let k# = sizeOf# (proxy# :: Proxy# (Maybe a))
        i0# = i# *# k#
    in case mVal of
         Nothing -> setOffAddr# addr# i0# k# (I8# 0#) s
         Just a  ->
           writeOffAddr# (addr# `plusAddr#` (i0# +# 1#)) 0# a (writeInt8OffAddr# addr# i0# 1# s)
  {-# INLINE writeOffAddr# #-}
  setMutableByteArray# mba# o# n# mVal s =
    case mVal of
      Nothing ->
        let k# = sizeOf# (proxy# :: Proxy# (Maybe a))
        in setByteArray# mba# (o# *# k#) (n# *# k#) 0# s
      _       -> setMutableByteArrayLoop# mba# o# n# mVal s
  {-# INLINE setMutableByteArray# #-}
  setOffAddr# addr# o# n# mVal s =
    case mVal of
      Nothing ->
        let k# = sizeOf# (proxy# :: Proxy# (Maybe a))
        in setOffAddr# addr# (o# *# k#) (n# *# k#) (I8# 0#) s
      _       ->  setOffAddrLoop# addr# o# n# mVal s
  {-# INLINE setOffAddr# #-}

maxInt# :: Int# -> Int# -> Int#
maxInt# x# y# =
  case x# <# y# of
    0# -> x#
    _  -> y#
{-# INLINE maxInt# #-}

type family MaxOrdering (o :: Ordering) (x :: Nat) (y :: Nat) where
  MaxOrdering 'LT x y = y
  MaxOrdering o  x y = x

type MaxOf (x :: Nat) (y :: Nat) = MaxOrdering (CmpNat x y) x y

instance (Unbox a, Unbox b) => Unbox (Either a b) where
  type PrimBase (Either a b) = Either a b
  type SizeOf (Either a b) = 1 + MaxOf (SizeOf a) (SizeOf b)
  type Alignment (Either a b) = 1 + MaxOf (Alignment a) (Alignment b)
  sizeOf# _ = 1# +# maxInt# (sizeOf# (proxy# :: Proxy# a)) (sizeOf# (proxy# :: Proxy# b))
  {-# INLINE sizeOf# #-}
  alignment# _ = 1# +# maxInt# (alignment# (proxy# :: Proxy# a)) (alignment# (proxy# :: Proxy# b))
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i# =
    case indexInt8Array# ba# i# of
      0# -> Left (indexByteOffByteArray# ba# (i# +# 1#))
      _  -> Right (indexByteOffByteArray# ba# (i# +# 1#))
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# =
    indexByteOffByteArray# ba# (i# *# sizeOf# (proxy# :: Proxy# (Either a b)))
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# =
    let addr0# = addr# `plusAddr#` (i# *# sizeOf# (proxy# :: Proxy# (Either a b)))
    in case indexInt8OffAddr# addr0# 0# of
      0# -> Left (indexOffAddr# (addr0# `plusAddr#` 1#) 0#)
      _  -> Right (indexOffAddr# (addr0# `plusAddr#` 1#) 0#)
  {-# INLINE indexOffAddr# #-}
  readByteOffMutableByteArray# mba# i# s =
    case readInt8Array# mba# i# s of
      (# s', 0# #) -> case readByteOffMutableByteArray# mba# (i# +# 1#) s' of
                        (# s'', a #) -> (# s'', Left a #)
      (# s', _  #) -> case readByteOffMutableByteArray# mba# (i# +# 1#) s' of
                        (# s'', a #) -> (# s'', Right a #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readMutableByteArray# mba# i# =
    let i0# = i# *# sizeOf# (proxy# :: Proxy# (Either a b))
     in readByteOffMutableByteArray# mba# i0#
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# addr# i# s =
    let addr0# = addr# `plusAddr#` (i# *# sizeOf# (proxy# :: Proxy# (Either a b)))
    in case readInt8OffAddr# addr0# 0# s of
         (# s', 0# #) -> case readOffAddr# (addr0# `plusAddr#` 1#) 0# s' of
                           (# s'', a #) -> (# s'', Left a #)
         (# s', _  #) -> case readOffAddr# (addr0# `plusAddr#` 1#) 0# s' of
                           (# s'', a #) -> (# s'', Right a #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i# eVal s =
    let a# = sizeOf# (proxy# :: Proxy# a)
        b# = sizeOf# (proxy# :: Proxy# b)
        i1# = i# +# 1#
    in case eVal of
         Left a -> -- TODO: Optimize duplication away
           setByteArray# mba# (i1# +# a#) (maxInt# 0# (b# -# a#)) 0#
           (writeByteOffMutableByteArray# mba# i1# a (writeInt8Array# mba# i# 0# s))
         Right b ->
           setByteArray# mba# (i1# +# b#) (maxInt# 0# (a# -# b#)) 0#
           (writeByteOffMutableByteArray# mba# i1# b (writeInt8Array# mba# i# 1# s))
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# eVal s =
    let k# = sizeOf# (proxy# :: Proxy# (Either a b))
        i0# = i# *# k#
    in writeByteOffMutableByteArray# mba# i0# eVal s
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# addr# i# eVal s =
    let a# = sizeOf# (proxy# :: Proxy# a)
        b# = sizeOf# (proxy# :: Proxy# b)
        k# = sizeOf# (proxy# :: Proxy# (Either a b))
        addr0# = addr# `plusAddr#` (i# *# k#)
        addr1# = addr0# `plusAddr#` 1#
    in case eVal of
         Left a  ->
           setOffAddr# addr1# a# (maxInt# 0# (b# -# a#)) (I8# 0#)
           (writeOffAddr# addr1# 0# a (writeInt8OffAddr# addr0# 0# 0# s))
         Right b ->
           setOffAddr# addr1# b# (maxInt# 0# (a# -# b#)) (I8# 0#)
           (writeOffAddr# addr1# 0# b (writeInt8OffAddr# addr0# 0# 1# s))
  {-# INLINE writeOffAddr# #-}
  setMutableByteArray# = setMutableByteArrayLoop#
  {-# INLINE setMutableByteArray# #-}
  setOffAddr# = setOffAddrLoop#
  {-# INLINE setOffAddr# #-}


-- | A loop that uses `writeMutableByteArray#` to set the values in the region. It is a
-- suboptimal way to fill the memory with a single value that is why it is only provided
-- here for convenience
setMutableByteArrayLoop# ::
     Unbox a => MutableByteArray# s -> Int# -> Int# -> a -> State# s -> State# s
setMutableByteArrayLoop# mba# o# n# a = go o#
  where
    k# = o# +# n#
    go i# s
      | isTrue# (i# <# k#) = go (i# +# 1#) (writeMutableByteArray# mba# i# a s)
      | otherwise = s
{-# INLINE setMutableByteArrayLoop# #-}

setOffAddrLoop# :: Unbox a => Addr# -> Int# -> Int# -> a -> State# s -> State# s
setOffAddrLoop# addr# o# n# a = go o#
  where
    k# = o# +# n#
    go i# s
      | isTrue# (i# <# k#) = go (i# +# 1#) (writeOffAddr# addr# i# a s)
      | otherwise = s
{-# INLINE setOffAddrLoop# #-}


errorImpossible :: String -> String -> a
errorImpossible fname msg =
#if __GLASGOW_HASKELL__ < 800
  error
#else
  errorWithoutStackTrace
#endif
  $ "Impossible <" ++ fname ++ ">:" ++ msg
{-# NOINLINE errorImpossible #-}
