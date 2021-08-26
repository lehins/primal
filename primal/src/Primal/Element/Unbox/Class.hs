{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Primal.Element.Unbox.Class
-- Copyright   : (c) Alexey Kuleshevich 2020-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Element.Unbox.Class
  ( Unbox(..)
  , setMutableByteArray#
  , setOffAddr#
  , setByteOffAddr#
  , setByteOffMutableByteArrayLoop#
  , setAddrLoop#
  , errorImpossible
  ) where

import Data.Kind
import Primal.Foreign hiding (Any)
import qualified Primal.Exception as Exception (errorWithoutStackTrace)
import GHC.TypeLits as Nats


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
--   is selfcontained in `Unbox` class and representation is not expected to stay the same
--   between different versions of this library. Primitive types like `Int`, `Word`,
--   `Char` are an exception to this rule for obvious reasons.
--
class Unbox a where
  type UnboxIso a :: Type

  type SizeOf a :: Nat
  type SizeOf a = SizeOf (UnboxIso a)
  type Alignment a :: Nat
  type Alignment a = Alignment (UnboxIso a)

  toUnboxIso :: a -> UnboxIso a
  default toUnboxIso :: Coercible a (UnboxIso a) => a -> UnboxIso a
  toUnboxIso = coerce

  fromUnboxIso :: UnboxIso a -> a
  default fromUnboxIso :: Coercible a (UnboxIso a) => UnboxIso a -> a
  fromUnboxIso = coerce

  -- | Returned value must match the `SizeOf` type level Nat
  sizeOf# :: Proxy# a -> Int#
  default sizeOf# :: Unbox (UnboxIso a) => Proxy# a -> Int#
  sizeOf# _ = sizeOf# (proxy# :: Proxy# (UnboxIso a))
  {-# INLINE sizeOf# #-}

  -- | Returned value must match the `Alignment` type level Nat
  alignment# :: Proxy# a -> Int#
  default alignment# :: Unbox (UnboxIso a) => Proxy# a -> Int#
  alignment# _ = alignment# (proxy# :: Proxy# (UnboxIso a))
  {-# INLINE alignment# #-}


  indexByteOffByteArray# :: ByteArray# -> Int# -> a
  default indexByteOffByteArray# :: Unbox (UnboxIso a) => ByteArray# -> Int# -> a
  indexByteOffByteArray# ba# i# = fromUnboxIso (indexByteOffByteArray# ba# i# :: UnboxIso a)
  {-# INLINE indexByteOffByteArray# #-}

  --
  -- These equalities hold:
  --
  -- > indexByteArray# ba# i# == indexOffAddr# (byteArrayContents# ba#) i#
  --
  -- > indexByteArray# ba# i# == indexByteOffByteArray# ba# (i# *# sizeOf (proxy# :: Proxy# a))
  --
  indexByteArray# :: ByteArray# -> Int# -> a
  default indexByteArray# :: Unbox (UnboxIso a) => ByteArray# -> Int# -> a
  indexByteArray# ba# i# = fromUnboxIso (indexByteArray# ba# i# :: UnboxIso a)
  {-# INLINE indexByteArray# #-}

  indexOffAddr# :: Addr# -> Int# -> a
  default indexOffAddr# :: Unbox (UnboxIso a) => Addr# -> Int# -> a
  indexOffAddr# addr# i# = fromUnboxIso (indexOffAddr# addr# i# :: UnboxIso a)
  {-# INLINE indexOffAddr# #-}

  indexByteOffAddr# :: Addr# -> Int# -> a
  indexByteOffAddr# addr# i# = indexOffAddr# (addr# `plusAddr#` i#) 0#
  {-# INLINE indexByteOffAddr# #-}

  readByteOffMutableByteArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
  default readByteOffMutableByteArray# ::
    Unbox (UnboxIso a) => MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
  readByteOffMutableByteArray# mba# i# s = case readByteOffMutableByteArray# mba# i# s of
                                             (# s', pa :: UnboxIso a #) -> (# s', fromUnboxIso pa #)
  {-# INLINE readByteOffMutableByteArray# #-}

  readMutableByteArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
  default readMutableByteArray# ::
    Unbox (UnboxIso a) => MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
  readMutableByteArray# mba# i# s = case readMutableByteArray# mba# i# s of
                                      (# s', pa :: UnboxIso a #) -> (# s', fromUnboxIso pa #)
  {-# INLINE readMutableByteArray# #-}

  readOffAddr# :: Addr# -> Int# -> State# s -> (# State# s, a #)
  default readOffAddr# ::
    Unbox (UnboxIso a) => Addr# -> Int# -> State# s -> (# State# s, a #)
  readOffAddr# addr# i# s = case readOffAddr# addr# i# s of
                              (# s', pa :: UnboxIso a #) -> (# s', fromUnboxIso pa #)
  {-# INLINE readOffAddr# #-}

  readByteOffAddr# :: Addr# -> Int# -> State# s -> (# State# s, a #)
  readByteOffAddr# addr# i# = readOffAddr# (addr# `plusAddr#` i#) 0#
  {-# INLINE readByteOffAddr# #-}

  writeByteOffMutableByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> State# s
  default writeByteOffMutableByteArray# ::
    Unbox (UnboxIso a) => MutableByteArray# s -> Int# -> a -> State# s -> State# s
  writeByteOffMutableByteArray# mba# i# a =
    writeByteOffMutableByteArray# mba# i# (toUnboxIso a)
  {-# INLINE writeByteOffMutableByteArray# #-}

  writeMutableByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> State# s
  default writeMutableByteArray# ::
    Unbox (UnboxIso a) => MutableByteArray# s -> Int# -> a -> State# s -> State# s
  writeMutableByteArray# mba# i# a = writeMutableByteArray# mba# i# (toUnboxIso a)
  {-# INLINE writeMutableByteArray# #-}

  writeOffAddr# :: Addr# -> Int# -> a -> State# s -> State# s
  default writeOffAddr# :: Unbox (UnboxIso a) => Addr# -> Int# -> a -> State# s -> State# s
  writeOffAddr# addr# i# a = writeOffAddr# addr# i# (toUnboxIso a)
  {-# INLINE writeOffAddr# #-}

  writeByteOffAddr# :: Addr# -> Int# -> a -> State# s -> State# s
  writeByteOffAddr# addr# i# = writeOffAddr# (addr# `plusAddr#` i#) 0#
  {-# INLINE writeByteOffAddr# #-}

  -- | Set the region of MutableByteArray to the same value. Offset is in number of bytes.
  setByteOffMutableByteArray# :: MutableByteArray# s -> Int# -> Int# -> a -> State# s -> State# s
  default setByteOffMutableByteArray# ::
    Unbox (UnboxIso a) => MutableByteArray# s -> Int# -> Int# -> a -> State# s -> State# s
  setByteOffMutableByteArray# mba# o# n# a = setByteOffMutableByteArray# mba# o# n# (toUnboxIso a)
  {-# INLINE setByteOffMutableByteArray# #-}

  -- | Set specified number of elements to the same value.
  setAddr# :: Addr# -> Int# -> a -> State# s -> State# s
  default setAddr# :: Unbox (UnboxIso a) => Addr# -> Int# -> a -> State# s -> State# s
  setAddr# addr# n# a = setAddr# addr# n# (toUnboxIso a)
  {-# INLINE setAddr# #-}


-- | Set the region of MutableByteArray to the same value. Offset is in number of elements
setMutableByteArray# ::
     forall a s. Unbox a
  => MutableByteArray# s
  -> Int#
  -> Int#
  -> a
  -> State# s
  -> State# s
setMutableByteArray# mba# o# = setByteOffMutableByteArray# mba# (o# *# sizeOf# (proxy# :: Proxy# a))
{-# INLINE setMutableByteArray# #-}


-- | Set the region of memory to the same value. Offset is in number of elements
setOffAddr# :: forall a s. Unbox a => Addr# -> Int# -> Int# -> a -> State# s -> State# s
setOffAddr# addr# i# = setAddr# (addr# `plusAddr#` (i# *# sizeOf# (proxy# :: Proxy# a)))
{-# INLINE setOffAddr# #-}

-- | Set the region of memory to the same value. Offset is in number of bytes
setByteOffAddr# :: forall a s. Unbox a => Addr# -> Int# -> Int# -> a -> State# s -> State# s
setByteOffAddr# addr# i# = setAddr# (addr# `plusAddr#` i#)
{-# INLINE setByteOffAddr# #-}


-- | A loop that uses `writeByteOffMutableByteArray#` to set the values in the region. It is a
-- suboptimal way to fill the memory with a single value that is why it is only provided
-- here for convenience
setByteOffMutableByteArrayLoop# ::
     forall a s. Unbox a
  => MutableByteArray# s
  -> Int#
  -> Int#
  -> a
  -> State# s
  -> State# s
setByteOffMutableByteArrayLoop# mba# o# n# a = go o#
  where
    sz# = sizeOf# (proxy# :: Proxy# a)
    k# = o# +# n# *# sz#
    go i# s
      | isTrue# (i# <# k#) = go (i# +# sz#) (writeByteOffMutableByteArray# mba# i# a s)
      | otherwise = s
{-# INLINE setByteOffMutableByteArrayLoop# #-}


-- setAddrLoop# :: forall a s. Unbox a => Addr# -> Int# -> a -> State# s -> State# s
-- setAddrLoop# addr0# n# a = go addr0# n#
--   where
--     k# = sizeOf# (Proxy# :: Proxy# a)
--     go addr# i# s
--       | isTrue# (i# >=# 0#) = go (addr# `plusAddr#` k#) (i# -# 1#) (writeOffAddr# addr# 0# a s)
--       | otherwise = s
-- {-# INLINE setAddrLoop# #-}


setAddrLoop# :: Unbox a => Addr# -> Int# -> a -> State# s -> State# s
setAddrLoop# addr# n# a = go 0#
  where
    go i# s
      | isTrue# (i# <# n#) = go (i# +# 1#) (writeOffAddr# addr# i# a s)
      | otherwise = s
{-# INLINE setAddrLoop# #-}


errorImpossible :: String -> String -> a
errorImpossible fname msg =
  Exception.errorWithoutStackTrace $ "Impossible <" ++ fname ++ ">:" ++ msg
{-# NOINLINE errorImpossible #-}
