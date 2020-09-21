{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
-- |
-- Module      : Data.Prim.Memory.Text
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Memory.Text
  ( Text(..)
  , MText(..)
  , Array(..)
  , MArray(..)
  , toBytesArray
  , fromBytesArray
  , toMBytesMArray
  , fromMBytesMArray
  ) where

import Data.Text.Array
import Data.Text.Internal
import Data.Prim.Memory.Bytes.Internal
  ( Bytes(..)
  , MBytes(..)
  , Pinned(..)
  )

-- | Mutable version of a `Text`
data MText s =
  MText
    {-# UNPACK #-}!(MArray s) -- payload (Word16 elements)
    {-# UNPACK #-}!Int        -- offset (units of Word16, not Char)
    {-# UNPACK #-}!Int        -- length (units of Word16, not Char)

toBytesArray :: Array -> Bytes 'Inc
toBytesArray (Array ba#) = Bytes ba#
{-# INLINE toBytesArray #-}

fromBytesArray :: Bytes p -> Array
fromBytesArray (Bytes ba#) = Array ba#
{-# INLINE fromBytesArray #-}

toMBytesMArray :: MArray s -> MBytes 'Inc s
toMBytesMArray (MArray mba#) = MBytes mba#
{-# INLINE toMBytesMArray #-}

fromMBytesMArray :: MBytes p s -> MArray s
fromMBytesMArray (MBytes ba#) = MArray ba#
{-# INLINE fromMBytesMArray #-}


