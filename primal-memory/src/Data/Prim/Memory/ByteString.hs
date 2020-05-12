{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
-- |
-- Module      : Data.Prim.Memory.ByteString
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Memory.ByteString
  (
  -- * Conversion
  -- ** ByteString
    ByteString(..)
  , toByteStringAddr
  , toByteStringBytes
  , fromByteStringAddr
  , fromByteStringMAddr
  -- ** ShortByteString
  , ShortByteString(..)
  , toShortByteStringBytes
  , fromShortByteStringBytes
  ) where

import Data.ByteString.Internal
import Data.ByteString.Short.Internal
import Data.Prim
import {-# SOURCE #-} Data.Prim.Memory.Bytes
  ( Bytes(..)
  , Pinned(..)
  , byteCountBytes
  , toForeignPtrBytes
  )
import Data.Prim.Memory.Addr

-- | /O(1)/ - Cast an immutable `Addr` to an immutable `ByteString`
--
-- @since 0.1.0
toByteStringAddr :: Addr Word8 -> ByteString
toByteStringAddr addr = PS (toForeignPtrAddr addr) 0 (unCount (countAddr addr))

-- | /O(1)/ - Cast an immutable `Bytes` to an immutable `ByteString`
--
-- @since 0.1.0
toByteStringBytes :: Bytes 'Pin -> ByteString
toByteStringBytes b = PS (toForeignPtrBytes b) 0 (coerce (byteCountBytes b))

-- | /O(1)/ - Cast an immutable `ByteString` to `Addr`. Also returns the original length of
-- ByteString, which will be less or equal to `countOfAddr` in the produced `Addr`.
--
-- @since 0.1.0
fromByteStringAddr :: ByteString -> (Addr Word8, Count Word8)
fromByteStringAddr (PS fptr i n) =
  case fromForeignPtrAddr fptr of
    Just addr -> (addr `plusOffAddr` Off i, Count n)
    Nothing -> byteStringConvertError

-- | /O(1)/ - Cast an immutable `ByteString` to a mutable `MAddr`. Also returns the
-- original length of ByteString, which will be less or equal to `getCountOfMAddr` in the
-- produced `MAddr`.
--
-- __Unsafe__ - Further modification of `MAddr` will affect the source `ByteString`
--
-- @since 0.1.0
fromByteStringMAddr :: ByteString -> (MAddr Word8 s, Count Word8)
fromByteStringMAddr (PS fptr i n) =
  case fromForeignPtrMAddr fptr of
    Just maddr -> (maddr `plusOffMAddr` Off i, Count n)
    Nothing -> byteStringConvertError

-- | /O(1)/ - Cast an immutable `Bytes` to an immutable `ShortByteString`
--
-- @since 0.1.0
toShortByteStringBytes :: Bytes p -> ShortByteString
toShortByteStringBytes (Bytes ba#) = SBS ba#

-- | /O(1)/ - Cast an immutable  `ShortByteString` to an immutable `Bytes`
--
-- @since 0.1.0
fromShortByteStringBytes :: ShortByteString -> Bytes 'Inc
fromShortByteStringBytes (SBS ba#) = Bytes ba#
{-# INLINE fromShortByteStringBytes #-}

byteStringConvertError :: a
byteStringConvertError =
  error
    "'ByteString' was not created natively in 'bytestring' package cannot convert"
{-# NOINLINE byteStringConvertError #-}
