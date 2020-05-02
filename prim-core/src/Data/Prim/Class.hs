{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Prim.Class
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Class
  ( Prim(..)
  , thawByteArray#
  , mutableByteArrayContents#
  , setMutableByteArrayLoop#
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

import Control.Prim.Monad.Unsafe
import Foreign.Prim hiding (Any)
import Foreign.C.Error (Errno(..))
import Data.Complex
import GHC.Conc
import GHC.Stable
import GHC.Real
import GHC.IO.Device
import GHC.Fingerprint.Type
import GHC.TypeLits as Nats
import Data.Functor.Const
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Monoid
#if __GLASGOW_HASKELL__ >= 800
import Data.Semigroup
#endif /* __GLASGOW_HASKELL__ >= 800 */

#if __GLASGOW_HASKELL__ < 802
import qualified Foreign.Ptr as P
import Foreign.Ptr hiding
  ( IntPtr
  , WordPtr
  , intPtrToPtr
  , ptrToIntPtr
  , ptrToWordPtr
  , wordPtrToPtr
  )
import Unsafe.Coerce
#include "prim_core_compat.h"

import Data.Bits (Bits, FiniteBits)
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

instance Prim P.IntPtr where
  type PrimBase P.IntPtr = IntPtr
  -- Constructor for newtype was not exported
  toPrim = unsafeCoerce
  fromPrim = unsafeCoerce

instance Prim P.WordPtr where
  type PrimBase P.WordPtr = WordPtr
  -- Constructor for newtype was not exported
  toPrim = unsafeCoerce
  fromPrim = unsafeCoerce
#else
import Foreign.Ptr
#endif

class Prim a where
  type PrimBase a :: *

  type SizeOf a :: Nat
  type SizeOf a = SizeOf (PrimBase a)
  type Alignment a :: Nat
  type Alignment a = Alignment (PrimBase a)

  toPrim :: a -> PrimBase a
  default toPrim :: Coercible a (PrimBase a) => a -> PrimBase a
  toPrim = coerce

  fromPrim :: PrimBase a -> a
  default fromPrim :: Coercible a (PrimBase a) => PrimBase a -> a
  fromPrim = coerce

  sizeOf# :: Proxy# a -> Int
  default sizeOf# :: Prim (PrimBase a) => Proxy# a -> Int
  sizeOf# _ = sizeOf# (proxy# :: Proxy# (PrimBase a))
  {-# INLINE sizeOf# #-}

  alignment# :: Proxy# a -> Int
  default alignment# :: Prim (PrimBase a) => Proxy# a -> Int
  alignment# _ = alignment# (proxy# :: Proxy# (PrimBase a))
  {-# INLINE alignment# #-}


  indexByteOffByteArray# :: ByteArray# -> Int# -> a
  default indexByteOffByteArray# :: Prim (PrimBase a) => ByteArray# -> Int# -> a
  indexByteOffByteArray# ba# i# = fromPrim (indexByteOffByteArray# ba# i# :: PrimBase a)
  {-# INLINE indexByteOffByteArray# #-}

  --
  -- These equalities hold:
  --
  -- > indexByteArray# ba# i# == indexOffAddr# (byteArrayContents# ba#) i#
  --
  -- > indexByteArray# ba# i# == indexByteOffByteArray# ba# (i# *# sizeOf (proxy# :: Proxy# a))
  --
  indexByteArray# :: ByteArray# -> Int# -> a
  default indexByteArray# :: Prim (PrimBase a) => ByteArray# -> Int# -> a
  indexByteArray# ba# i# = fromPrim (indexByteArray# ba# i# :: PrimBase a)
  {-# INLINE indexByteArray# #-}

  indexOffAddr# :: Addr# -> Int# -> a
  default indexOffAddr# :: Prim (PrimBase a) => Addr# -> Int# -> a
  indexOffAddr# addr# i# = fromPrim (indexOffAddr# addr# i# :: PrimBase a)
  {-# INLINE indexOffAddr# #-}


  readByteOffMutableByteArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
  default readByteOffMutableByteArray# ::
    Prim (PrimBase a) => MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
  readByteOffMutableByteArray# mba# i# s = case readByteOffMutableByteArray# mba# i# s of
                                             (# s', pa :: PrimBase a #) -> (# s', fromPrim pa #)
  {-# INLINE readByteOffMutableByteArray# #-}

  readMutableByteArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
  default readMutableByteArray# ::
    Prim (PrimBase a) => MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
  readMutableByteArray# mba# i# s = case readMutableByteArray# mba# i# s of
                                      (# s', pa :: PrimBase a #) -> (# s', fromPrim pa #)
  {-# INLINE readMutableByteArray# #-}

  readOffAddr# :: Addr# -> Int# -> State# s -> (# State# s, a #)
  default readOffAddr# ::
    Prim (PrimBase a) => Addr# -> Int# -> State# s -> (# State# s, a #)
  readOffAddr# addr# i# s = case readOffAddr# addr# i# s of
                              (# s', pa :: PrimBase a #) -> (# s', fromPrim pa #)
  {-# INLINE readOffAddr# #-}


  writeByteOffMutableByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> State# s
  default writeByteOffMutableByteArray# ::
    Prim (PrimBase a) => MutableByteArray# s -> Int# -> a -> State# s -> State# s
  writeByteOffMutableByteArray# mba# i# a =
    writeByteOffMutableByteArray# mba# i# (toPrim a :: PrimBase a)
  {-# INLINE writeByteOffMutableByteArray# #-}

  writeMutableByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> State# s
  default writeMutableByteArray# ::
    Prim (PrimBase a) => MutableByteArray# s -> Int# -> a -> State# s -> State# s
  writeMutableByteArray# mba# i# a = writeMutableByteArray# mba# i# (toPrim a :: PrimBase a)
  {-# INLINE writeMutableByteArray# #-}

  writeOffAddr# :: Addr# -> Int# -> a -> State# s -> State# s
  default writeOffAddr# :: Prim (PrimBase a) => Addr# -> Int# -> a -> State# s -> State# s
  writeOffAddr# mba# i# a = writeOffAddr# mba# i# (toPrim a)
  {-# INLINE writeOffAddr# #-}

  -- TODO: implement
  -- setByteOffMutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> a -> State# s -> State# s
  -- default setMutableByteArray# ::
  --   Prim (PrimBase a) => MutableByteArray# s -> Int# -> Int# -> a -> State# s -> State# s
  -- setByteOffMutableByteArray# mba# i# n# a = setByteOffMutableByteArray# mba# i# n# (toPrim a)
  -- {-# INLINE setByteOffMutableByteArray# #-}

  setMutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> a -> State# s -> State# s
  default setMutableByteArray# ::
    Prim (PrimBase a) => MutableByteArray# s -> Int# -> Int# -> a -> State# s -> State# s
  setMutableByteArray# mba# i# n# a = setMutableByteArray# mba# i# n# (toPrim a)
  {-# INLINE setMutableByteArray# #-}

  setOffAddr# :: Addr# -> Int# -> Int# -> a -> State# s -> State# s
  default setOffAddr# ::
    Prim (PrimBase a) => Addr# -> Int# -> Int# -> a -> State# s -> State# s
  setOffAddr# mba# i# n# a = setOffAddr# mba# i# n# (toPrim a)
  {-# INLINE setOffAddr# #-}


instance Prim () where
  type PrimBase () = ()
  type SizeOf () = 0
  type Alignment () = 0
  sizeOf# _ = 0
  {-# INLINE sizeOf# #-}
  alignment# _ = 0
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


instance Prim Int where
  type PrimBase Int = Int
  type SizeOf Int = SIZEOF_HSINT
  type Alignment Int = ALIGNMENT_HSINT
  sizeOf# _ = SIZEOF_HSINT
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_HSINT
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

instance Prim Int8 where
  type PrimBase Int8 = Int8
  type SizeOf Int8 = SIZEOF_INT8
  type Alignment Int8 = ALIGNMENT_INT8
  sizeOf# _ = SIZEOF_INT8
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_INT8
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

instance Prim Int16 where
  type PrimBase Int16 = Int16
  type SizeOf Int16 = SIZEOF_INT16
  type Alignment Int16 = ALIGNMENT_INT16
  sizeOf# _ = SIZEOF_INT16
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_INT16
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

instance Prim Int32 where
  type PrimBase Int32 = Int32
  type SizeOf Int32 = SIZEOF_INT32
  type Alignment Int32 = ALIGNMENT_INT32
  sizeOf# _ = SIZEOF_INT32
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_INT32
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

instance Prim Int64 where
  type PrimBase Int64 = Int64
  type SizeOf Int64 = SIZEOF_INT64
  type Alignment Int64 = ALIGNMENT_INT64
  sizeOf# _ = SIZEOF_INT64
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_INT64
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


instance Prim Word where
  type PrimBase Word = Word
  type SizeOf Word = SIZEOF_HSWORD
  type Alignment Word = ALIGNMENT_HSWORD
  sizeOf# _ = SIZEOF_HSWORD
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_HSWORD
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

instance Prim Word8 where
  type PrimBase Word8 = Word8
  type SizeOf Word8 = SIZEOF_WORD8
  type Alignment Word8 = ALIGNMENT_WORD8
  sizeOf# _ = SIZEOF_WORD8
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_WORD8
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

instance Prim Word16 where
  type PrimBase Word16 = Word16
  type SizeOf Word16 = SIZEOF_WORD16
  type Alignment Word16 = ALIGNMENT_WORD16
  sizeOf# _ = SIZEOF_WORD16
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_WORD16
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

instance Prim Word32 where
  type PrimBase Word32 = Word32
  type SizeOf Word32 = SIZEOF_WORD32
  type Alignment Word32 = ALIGNMENT_WORD32
  sizeOf# _ = SIZEOF_WORD32
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_WORD32
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

instance Prim Word64 where
  type PrimBase Word64 = Word64
  type SizeOf Word64 = SIZEOF_WORD64
  type Alignment Word64 = ALIGNMENT_WORD64
  sizeOf# _ = SIZEOF_WORD64
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_WORD64
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


instance Prim Float where
  type PrimBase Float = Float
  type SizeOf Float = SIZEOF_FLOAT
  type Alignment Float = ALIGNMENT_FLOAT
  sizeOf# _ = SIZEOF_FLOAT
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_FLOAT
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

instance Prim Double where
  type PrimBase Double = Double
  type SizeOf Double = SIZEOF_DOUBLE
  type Alignment Double = ALIGNMENT_DOUBLE
  sizeOf# _ = SIZEOF_DOUBLE
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_DOUBLE
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

instance Prim Bool where
  type PrimBase Bool = Int8
  fromPrim (I8# i#) = int2Bool# i#
  {-# INLINE fromPrim #-}
  toPrim b = I8# (bool2Int# b)
  {-# INLINE toPrim #-}
  -- sizeOf# _ = SIZEOF_INT8
  -- {-# INLINE sizeOf# #-}
  -- alignment# _ = ALIGNMENT_INT8
  -- {-# INLINE alignment# #-}
  -- indexByteArray# ba# i# = int2Bool# (indexInt8Array# ba# i#)
  -- {-# INLINE indexByteArray# #-}
  -- indexOffAddr# addr# i# = int2Bool# (indexInt8OffAddr# addr# i#)
  -- {-# INLINE indexOffAddr# #-}
  -- readMutableByteArray# mba# i# s = case readInt8Array# mba# i# s of
  --                                      (# s', a# #) -> (# s', int2Bool# a# #)
  -- {-# INLINE readMutableByteArray# #-}
  -- readOffAddr# mba# i# s = case readInt8OffAddr# mba# i# s of
  --                             (# s', a# #) -> (# s', int2Bool# a# #)
  -- {-# INLINE readOffAddr# #-}
  -- writeMutableByteArray# mba# i# b = writeInt8Array# mba# i# (bool2Int# b)
  -- {-# INLINE writeMutableByteArray# #-}
  -- writeOffAddr# mba# i# b = writeInt8OffAddr# mba# i# (bool2Int# b)
  -- {-# INLINE writeOffAddr# #-}
  -- setMutableByteArray# mba# o# n# b = setByteArray# mba# o# n# (bool2Int# b)
  -- {-# INLINE setMutableByteArray# #-}
  -- setOffAddr# addr# o# n# b = setOffAddr# addr# o# n# (I8# (bool2Int# b))
  -- {-# INLINE setOffAddr# #-}

instance Prim Char where
  type PrimBase Char = Char
  type SizeOf Char = SIZEOF_HSCHAR
  type Alignment Char = ALIGNMENT_HSCHAR
  sizeOf# _ = SIZEOF_HSCHAR
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_HSCHAR
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

instance Prim (Ptr a) where
  type PrimBase (Ptr a) = Ptr a
  type SizeOf (Ptr a) = SIZEOF_HSPTR
  type Alignment (Ptr a) = ALIGNMENT_HSPTR
  sizeOf# _ = SIZEOF_HSINT
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_HSINT
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

instance Prim (FunPtr a) where
  type PrimBase (FunPtr a) = Ptr a
  toPrim (FunPtr addr#) = Ptr addr#
  fromPrim (Ptr addr#) = FunPtr addr#


instance Prim (StablePtr a) where
  type PrimBase (StablePtr a) = StablePtr a
  type SizeOf (StablePtr a) = SIZEOF_HSSTABLEPTR
  type Alignment (StablePtr a) = ALIGNMENT_HSSTABLEPTR
  sizeOf# _ = SIZEOF_HSINT
  {-# INLINE sizeOf# #-}
  alignment# _ = ALIGNMENT_HSINT
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


instance Prim IntPtr where
  type PrimBase IntPtr = Int

instance Prim WordPtr where
  type PrimBase WordPtr = Word

instance Prim CBool where
  type PrimBase CBool = HTYPE_BOOL

instance Prim CChar where
  type PrimBase CChar = HTYPE_CHAR

instance Prim CSChar where
  type PrimBase CSChar = HTYPE_SIGNED_CHAR

instance Prim CUChar where
  type PrimBase CUChar = HTYPE_UNSIGNED_CHAR

instance Prim CShort where
  type PrimBase CShort = HTYPE_SHORT

instance Prim CUShort where
  type PrimBase CUShort = HTYPE_UNSIGNED_SHORT

instance Prim CInt where
  type PrimBase CInt = HTYPE_INT

instance Prim CUInt where
  type PrimBase CUInt = HTYPE_UNSIGNED_INT

instance Prim CLong where
  type PrimBase CLong = HTYPE_LONG

instance Prim CULong where
  type PrimBase CULong = HTYPE_UNSIGNED_LONG

instance Prim CLLong where
  type PrimBase CLLong = HTYPE_LONG_LONG

instance Prim CULLong where
  type PrimBase CULLong = HTYPE_UNSIGNED_LONG_LONG

instance Prim CPtrdiff where
  type PrimBase CPtrdiff = HTYPE_PTRDIFF_T

instance Prim CSize where
  type PrimBase CSize = HTYPE_SIZE_T

instance Prim CWchar where
  type PrimBase CWchar = HTYPE_WCHAR_T

instance Prim CSigAtomic where
  type PrimBase CSigAtomic = HTYPE_SIG_ATOMIC_T

instance Prim CIntPtr where
  type PrimBase CIntPtr = HTYPE_INTPTR_T

instance Prim CUIntPtr where
  type PrimBase CUIntPtr = HTYPE_UINTPTR_T

instance Prim CIntMax where
  type PrimBase CIntMax = HTYPE_INTMAX_T

instance Prim CUIntMax where
  type PrimBase CUIntMax = HTYPE_UINTMAX_T

instance Prim CFloat where
  type PrimBase CFloat = HTYPE_FLOAT

instance Prim CDouble where
  type PrimBase CDouble = HTYPE_DOUBLE


instance Prim Fd where
  type PrimBase Fd = CInt

instance Prim Errno where
  type PrimBase Errno = CInt


#if defined(HTYPE_DEV_T)
instance Prim CDev where
  type PrimBase CDev = HTYPE_DEV_T
#endif
#if defined(HTYPE_INO_T)
instance Prim CIno where
  type PrimBase CIno = HTYPE_INO_T
#endif
#if defined(HTYPE_MODE_T)
instance Prim CMode where
  type PrimBase CMode = HTYPE_MODE_T
#endif
#if defined(HTYPE_OFF_T)
instance Prim COff where
  type PrimBase COff = HTYPE_OFF_T
#endif
#if defined(HTYPE_PID_T)
instance Prim CPid where
  type PrimBase CPid = HTYPE_PID_T
#endif
#if defined(HTYPE_SSIZE_T)
instance Prim CSsize where
  type PrimBase CSsize = HTYPE_SSIZE_T
#endif
#if defined(HTYPE_GID_T)
instance Prim CGid where
  type PrimBase CGid = HTYPE_GID_T
#endif
#if defined(HTYPE_NLINK_T)
instance Prim CNlink where
  type PrimBase CNlink = HTYPE_NLINK_T
#endif
#if defined(HTYPE_UID_T)
instance Prim CUid where
  type PrimBase CUid = HTYPE_UID_T
#endif
#if defined(HTYPE_CC_T)
instance Prim CCc where
  type PrimBase CCc = HTYPE_CC_T
#endif
#if defined(HTYPE_SPEED_T)
instance Prim CSpeed where
  type PrimBase CSpeed = HTYPE_SPEED_T
#endif
#if defined(HTYPE_TCFLAG_T)
instance Prim CTcflag where
  type PrimBase CTcflag = HTYPE_TCFLAG_T
#endif
#if defined(HTYPE_RLIM_T)
instance Prim CRLim where
  type PrimBase CRLim = HTYPE_RLIM_T
#endif

#if __GLASGOW_HASKELL__ >= 802

#if defined(HTYPE_BLKSIZE_T)
instance Prim CBlkSize where
  type PrimBase CBlkSize = HTYPE_BLKSIZE_T
#endif
#if defined(HTYPE_BLKCNT_T)
instance Prim CBlkCnt where
  type PrimBase CBlkCnt = HTYPE_BLKCNT_T
#endif
#if defined(HTYPE_CLOCKID_T)
instance Prim CClockId where
  type PrimBase CClockId = HTYPE_CLOCKID_T
#endif
#if defined(HTYPE_FSBLKCNT_T)
instance Prim CFsBlkCnt where
  type PrimBase CFsBlkCnt = HTYPE_FSBLKCNT_T
#endif
#if defined(HTYPE_FSFILCNT_T)
instance Prim CFsFilCnt where
  type PrimBase CFsFilCnt = HTYPE_FSFILCNT_T
#endif
#if defined(HTYPE_ID_T)
instance Prim CId where
  type PrimBase CId = HTYPE_ID_T
#endif
#if defined(HTYPE_KEY_T)
instance Prim CKey where
  type PrimBase CKey = HTYPE_KEY_T
#endif
#if defined(HTYPE_TIMER_T)
instance Prim CTimer where
  type PrimBase CTimer = HTYPE_TIMER_T
#endif

#if __GLASGOW_HASKELL__ >= 810

#if defined(HTYPE_SOCKLEN_T)
instance Prim CSocklen where
  type PrimBase CSocklen = HTYPE_SOCKLEN_T
#endif
#if defined(HTYPE_NFDS_T)
instance Prim CNfds where
  type PrimBase CNfds = HTYPE_NFDS_T
#endif

#endif /* __GLASGOW_HASKELL__ >= 810 */

#endif /* __GLASGOW_HASKELL__ >= 802 */

#if __GLASGOW_HASKELL__ >= 800
instance Prim a => Prim (Max a) where
  type PrimBase (Max a) = a
instance Prim a => Prim (Min a) where
  type PrimBase (Min a) = a
instance Prim a => Prim (Data.Semigroup.First a) where
  type PrimBase (Data.Semigroup.First a) = a
instance Prim a => Prim (Data.Semigroup.Last a) where
  type PrimBase (Data.Semigroup.Last a) = a
instance (Eq a, Prim a) => Prim (Arg a a) where
  type PrimBase (Arg a a) = (a, a)
  toPrim (Arg a b) = (a, b)
  fromPrim (a, b) = Arg a b

instance Prim (f (g a)) => Prim (Compose f g a) where
  type PrimBase (Compose f g a) = f (g a)

#endif /* __GLASGOW_HASKELL__ >= 800 */

instance Prim a => Prim (Const a b) where
  type PrimBase (Const a b) = a

instance Prim a => Prim (Identity a) where
  type PrimBase (Identity a) = a

instance Prim Ordering where
  type PrimBase Ordering = Int8
  toPrim o = I8# (fromOrdering# o)
  fromPrim (I8# i#) = toOrdering# i#

instance Prim IODeviceType where
  type PrimBase IODeviceType = Int8
  toPrim o = I8# (dataToTag# o)
  fromPrim (I8# i#) = tagToEnum# i#

instance Prim SeekMode where
  type PrimBase SeekMode = Int8
  toPrim o = I8# (dataToTag# o)
  fromPrim (I8# i#) = tagToEnum# i#

instance Prim BlockReason where
  type PrimBase BlockReason = Int8
  toPrim o = I8# (dataToTag# o)
  fromPrim (I8# i#) = tagToEnum# i#


instance Prim a => Prim (Down a) where
  type PrimBase (Down a) = a

instance Prim a => Prim (Dual a) where
  type PrimBase (Dual a) = a

instance Prim a => Prim (Sum a) where
  type PrimBase (Sum a) = a

instance Prim a => Prim (Product a) where
  type PrimBase (Product a) = a

instance Prim All where
  type PrimBase All = Bool

instance Prim Any where
  type PrimBase Any = Bool

instance Prim Fingerprint where
  type PrimBase Fingerprint = (Word64, Word64)
  toPrim (Fingerprint a b) = (a, b)
  fromPrim (a, b) = Fingerprint a b

instance (Prim a, Eq a) => Prim (Ratio a) where
  type PrimBase (Ratio a) = (a, a)
  toPrim (a :% b) = (a, b)
  fromPrim (a, b) = a :% b

instance (Prim a, Eq a) => Prim (Complex a) where
  type PrimBase (Complex a) = (a, a)
  toPrim (a :+ b) = (a, b)
  fromPrim (a, b) = a :+ b

instance (Eq a, Prim a) => Prim (a, a) where
  type PrimBase (a, a) = (a, a)
  type SizeOf (a, a) = 2 Nats.* SizeOf a
  type Alignment (a, a) = 2 Nats.* Alignment a
  sizeOf# _ = 2 * sizeOf# (proxy# :: Proxy# a)
  {-# INLINE sizeOf# #-}
  alignment# _ = 2 * alignment# (proxy# :: Proxy# a)
  {-# INLINE alignment# #-}
  indexByteArray# ba# i# =
    let i2# = 2# *# i#
    in (indexByteArray# ba# i2#, indexByteArray# ba# (i2# +# 1#))
  {-# INLINE indexByteArray# #-}
  indexByteOffByteArray# ba# i# =
    let i2# = 2# *# i#
    in (indexByteOffByteArray# ba# i2#, indexByteOffByteArray# ba# (i2# +# 1#))
  {-# INLINE indexByteOffByteArray# #-}
  indexOffAddr# addr# i# =
    let i2# = 2# *# i#
    in (indexOffAddr# addr# i2#, indexOffAddr# addr# (i2# +# 1#))
  {-# INLINE indexOffAddr# #-}
  readByteOffMutableByteArray# mba# i# s =
    let i2# = 2# *# i#
    in case readByteOffMutableByteArray# mba# i2# s of
         (# s', a0 #) ->
           case readByteOffMutableByteArray# mba# (i2# +# 1#) s' of
             (# s'', a1 #) -> (# s'', (a0, a1) #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readMutableByteArray# mba# i# s =
    let i2# = 2# *# i#
    in case readMutableByteArray# mba# i2# s of
         (# s', a0 #) ->
           case readMutableByteArray# mba# (i2# +# 1#) s' of
             (# s'', a1 #) -> (# s'', (a0, a1) #)
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# addr# i# s =
    let i2# = 2# *# i#
    in case readOffAddr# addr# i2# s of
         (# s', a0 #) ->
           case readOffAddr# addr# (i2# +# 1#) s' of
             (# s'', a1 #) -> (# s'', (a0, a1) #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i# (a0, a1) s =
    let i2# = 2# *# i#
    in writeByteOffMutableByteArray# mba# (i2# +# 1#) a1 (writeByteOffMutableByteArray# mba# i2# a0 s)
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# (a0, a1) s =
    let i2# = 2# *# i#
    in writeMutableByteArray# mba# (i2# +# 1#) a1 (writeMutableByteArray# mba# i2# a0 s)
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# addr# i# (a0, a1) s =
    let i2# = 2# *# i#
    in writeOffAddr# addr# (i2# +# 1#) a1
       (writeOffAddr# addr# i2# a0 s)
  {-# INLINE writeOffAddr# #-}
  setMutableByteArray# mba# o# n# a@(a0, a1) s
    | a0 == a1 = setMutableByteArray# mba# (o# *# 2#) (n# *# 2#) a0 s
    | otherwise = setMutableByteArrayLoop# mba# o# n# a s
  {-# INLINE setMutableByteArray# #-}
  setOffAddr# addr# o# n# a@(a0, a1) s
    | a0 == a1 = setOffAddr# addr# (o# *# 2#) (n# *# 2#) a0 s
    | otherwise = setOffAddr# addr# o# n# a s
  {-# INLINE setOffAddr# #-}


instance (Eq a, Prim a) => Prim (a, a, a) where
  type PrimBase (a, a, a) = (a, a, a)
  type SizeOf (a, a, a) = 3 Nats.* SizeOf a
  type Alignment (a, a, a) = 3 Nats.* Alignment a
  sizeOf# _ = 3 * sizeOf# (proxy# :: Proxy# a)
  {-# INLINE sizeOf# #-}
  alignment# _ = 3 * alignment# (proxy# :: Proxy# a)
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i# =
    let i3# = 3# *# i#
    in ( indexByteOffByteArray# ba# i3#
       , indexByteOffByteArray# ba# (i3# +# 1#)
       , indexByteOffByteArray# ba# (i3# +# 2#)
       )
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# =
    let i3# = 3# *# i#
    in ( indexByteArray# ba# i3#
       , indexByteArray# ba# (i3# +# 1#)
       , indexByteArray# ba# (i3# +# 2#)
       )
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# =
    let i3# = 3# *# i#
    in ( indexOffAddr# addr# i3#
       , indexOffAddr# addr# (i3# +# 1#)
       , indexOffAddr# addr# (i3# +# 2#)
       )
  {-# INLINE indexOffAddr# #-}
  readMutableByteArray# mba# i# s =
    let i3# = 3# *# i#
    in case readMutableByteArray# mba# i3#         s  of { (# s0, a0 #) ->
       case readMutableByteArray# mba# (i3# +# 1#) s0 of { (# s1, a1 #) ->
       case readMutableByteArray# mba# (i3# +# 2#) s1 of { (# s2, a2 #) ->
         (# s2, (a0, a1, a2) #)
       }}}
  {-# INLINE readMutableByteArray# #-}
  readByteOffMutableByteArray# mba# i# s =
    let i3# = 3# *# i#
    in case readByteOffMutableByteArray# mba# i3#         s  of { (# s0, a0 #) ->
       case readByteOffMutableByteArray# mba# (i3# +# 1#) s0 of { (# s1, a1 #) ->
       case readByteOffMutableByteArray# mba# (i3# +# 2#) s1 of { (# s2, a2 #) ->
         (# s2, (a0, a1, a2) #)
       }}}
  {-# INLINE readByteOffMutableByteArray# #-}
  readOffAddr# addr# i# s =
    let i3# = 3# *# i#
    in case readOffAddr# addr# i3#         s  of { (# s0, a0 #) ->
       case readOffAddr# addr# (i3# +# 1#) s0 of { (# s1, a1 #) ->
       case readOffAddr# addr# (i3# +# 2#) s1 of { (# s2, a2 #) ->
         (# s2, (a0, a1, a2) #)
       }}}
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i# (a0, a1, a2) s =
    let i3# = 3# *# i#
    in writeByteOffMutableByteArray# mba# (i3# +# 2#) a2
       (writeByteOffMutableByteArray# mba# (i3# +# 1#) a1
        (writeByteOffMutableByteArray# mba# i3# a0 s))
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# (a0, a1, a2) s =
    let i3# = 3# *# i#
    in writeMutableByteArray# mba# (i3# +# 2#) a2
       (writeMutableByteArray# mba# (i3# +# 1#) a1
        (writeMutableByteArray# mba# i3# a0 s))
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# addr# i# (a0, a1, a2) s =
    let i3# = 3# *# i#
    in writeOffAddr# addr# (i3# +# 2#) a2
       (writeOffAddr# addr# (i3# +# 1#) a1
        (writeOffAddr# addr# i3# a0 s))
  {-# INLINE writeOffAddr# #-}
  setMutableByteArray# mba# o# n# a@(a0, a1, a2) s
    | a0 == a1 && a1 == a2 = setMutableByteArray# mba# (o# *# 3#) (n# *# 3#) a0 s
    | otherwise = setMutableByteArrayLoop# mba# o# n# a s
  {-# INLINE setMutableByteArray# #-}
  setOffAddr# addr# o# n# a@(a0, a1, a2) s
    | a0 == a1 && a1 == a2 = setOffAddr# addr# (o# *# 3#) (n# *# 3#) a0 s
    | otherwise = setOffAddr# addr# o# n# a s
  {-# INLINE setOffAddr# #-}

instance (Eq a, Prim a) => Prim (a, a, a, a) where
  type PrimBase (a, a, a, a) = (a, a, a, a)
  type SizeOf (a, a, a, a) = 4 Nats.* SizeOf a
  type Alignment (a, a, a, a) = 4 Nats.* Alignment a
  sizeOf# _ = 4 * sizeOf# (proxy# :: Proxy# a)
  {-# INLINE sizeOf# #-}
  alignment# _ = 4 * alignment# (proxy# :: Proxy# a)
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i# =
    let i4# = 4# *# i#
    in ( indexByteOffByteArray# ba# i4#
       , indexByteOffByteArray# ba# (i4# +# 1#)
       , indexByteOffByteArray# ba# (i4# +# 2#)
       , indexByteOffByteArray# ba# (i4# +# 3#)
       )
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# =
    let i4# = 4# *# i#
    in ( indexByteArray# ba# i4#
       , indexByteArray# ba# (i4# +# 1#)
       , indexByteArray# ba# (i4# +# 2#)
       , indexByteArray# ba# (i4# +# 3#)
       )
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# =
    let i4# = 4# *# i#
    in ( indexOffAddr# addr# i4#
       , indexOffAddr# addr# (i4# +# 1#)
       , indexOffAddr# addr# (i4# +# 2#)
       , indexOffAddr# addr# (i4# +# 3#)
       )
  {-# INLINE indexOffAddr# #-}
  readMutableByteArray# mba# i# s =
    let i4# = 4# *# i#
    in case readMutableByteArray# mba# i4#         s  of { (# s0, a0 #) ->
       case readMutableByteArray# mba# (i4# +# 1#) s0 of { (# s1, a1 #) ->
       case readMutableByteArray# mba# (i4# +# 2#) s1 of { (# s2, a2 #) ->
       case readMutableByteArray# mba# (i4# +# 3#) s2 of { (# s3, a3 #) ->
         (# s3, (a0, a1, a2, a3) #)
       }}}}
  {-# INLINE readMutableByteArray# #-}
  readByteOffMutableByteArray# mba# i# s =
    let i4# = 4# *# i#
    in case readByteOffMutableByteArray# mba# i4#         s  of { (# s0, a0 #) ->
       case readByteOffMutableByteArray# mba# (i4# +# 1#) s0 of { (# s1, a1 #) ->
       case readByteOffMutableByteArray# mba# (i4# +# 2#) s1 of { (# s2, a2 #) ->
       case readByteOffMutableByteArray# mba# (i4# +# 3#) s2 of { (# s3, a3 #) ->
         (# s3, (a0, a1, a2, a3) #)
       }}}}
  {-# INLINE readByteOffMutableByteArray# #-}
  readOffAddr# addr# i# s =
    let i4# = 4# *# i#
    in case readOffAddr# addr# i4#         s  of { (# s0, a0 #) ->
       case readOffAddr# addr# (i4# +# 1#) s0 of { (# s1, a1 #) ->
       case readOffAddr# addr# (i4# +# 2#) s1 of { (# s2, a2 #) ->
       case readOffAddr# addr# (i4# +# 3#) s2 of { (# s3, a3 #) ->
         (# s3, (a0, a1, a2, a3) #)
       }}}}
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i# (a0, a1, a2, a3) s =
    let i4# = 4# *# i#
    in writeByteOffMutableByteArray# mba# (i4# +# 3#) a3
       (writeByteOffMutableByteArray# mba# (i4# +# 2#) a2
        (writeByteOffMutableByteArray# mba# (i4# +# 1#) a1
         (writeByteOffMutableByteArray# mba# i4# a0 s)))
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# (a0, a1, a2, a3) s =
    let i4# = 4# *# i#
    in writeMutableByteArray# mba# (i4# +# 3#) a3
       (writeMutableByteArray# mba# (i4# +# 2#) a2
        (writeMutableByteArray# mba# (i4# +# 1#) a1
         (writeMutableByteArray# mba# i4# a0 s)))
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# addr# i# (a0, a1, a2, a3) s =
    let i4# = 4# *# i#
    in writeOffAddr# addr# (i4# +# 3#) a3
       (writeOffAddr# addr# (i4# +# 2#) a2
        (writeOffAddr# addr# (i4# +# 1#) a1
         (writeOffAddr# addr# i4# a0 s)))
  {-# INLINE writeOffAddr# #-}
  setMutableByteArray# mba# o# n# a@(a0, a1, a2, a3) s
    | a0 == a1 && a1 == a2 && a2 == a3 = setMutableByteArray# mba# (o# *# 4#) (n# *# 4#) a0 s
    | otherwise = setMutableByteArrayLoop# mba# o# n# a s
  {-# INLINE setMutableByteArray# #-}
  setOffAddr# addr# o# n# a@(a0, a1, a2, a3) s
    | a0 == a1 && a1 == a2 && a2 == a3 = setOffAddr# addr# (o# *# 4#) (n# *# 4#) a0 s
    | otherwise = setOffAddr# addr# o# n# a s
  {-# INLINE setOffAddr# #-}




thawByteArray# :: ByteArray# -> State# s -> (# State# s, MutableByteArray# s #)
thawByteArray# ba# s = (# s, unsafeCoerce# ba# #)
{-# INLINE thawByteArray# #-}

mutableByteArrayContents# :: MutableByteArray# s -> Addr#
mutableByteArrayContents# mba# = byteArrayContents# (unsafeCoerce# mba#)
{-# INLINE mutableByteArrayContents# #-}

-- | A loop that uses `writeMutableByteArray#` to set the values in the region. It is a
-- suboptimal way to fill the memory with a single value that is why it is only provided
-- here for convenience
setMutableByteArrayLoop# ::
     Prim a => MutableByteArray# s -> Int# -> Int# -> a -> State# s -> State# s
setMutableByteArrayLoop# mba# o# n# a = go o#
  where
    k# = o# +# n#
    go i# s
      | isTrue# (i# <# k#) = go (i# +# 1#) (writeMutableByteArray# mba# i# a s)
      | otherwise = s
{-# INLINE setMutableByteArrayLoop# #-}


errorImpossible :: String -> String -> a
errorImpossible fname msg =
#if __GLASGOW_HASKELL__ < 800
  error
#else
  errorWithoutStackTrace
#endif
  $ "Impossible <" ++ fname ++ ">:" ++ msg
{-# NOINLINE errorImpossible #-}
