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
  , fromArrayBytes
  , toArrayBytes
  , fromMArrayMBytes
  , toMArrayMBytes
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

-- | /O(1)/ - Cast an immutable `Data.Text.Array.Array` from @text@ package to immutable `Bytes`
--
-- @since 0.3.0
fromArrayBytes :: Array -> Bytes 'Inc
fromArrayBytes (Array ba#) = Bytes ba#
{-# INLINE fromArrayBytes #-}

-- | /O(1)/ - Cast immutable `Bytes` to an immutable `Data.Text.Array.Array` from @text@ package
--
-- @since 0.3.0
toArrayBytes :: Bytes p -> Array
toArrayBytes (Bytes ba#) = Array ba#
{-# INLINE toArrayBytes #-}

-- | /O(1)/ - Cast a mutable `Data.Text.Array.MArray` from @text@ package to mutable `MBytes`
--
-- @since 0.3.0
fromMArrayMBytes :: MArray s -> MBytes 'Inc s
fromMArrayMBytes (MArray mba#) = MBytes mba#
{-# INLINE fromMArrayMBytes #-}

-- | /O(1)/ - Cast mutable `MBytes` to a mutable `Data.Text.Array.MArray` from @text@ package
--
-- @since 0.3.0
toMArrayMBytes :: MBytes p s -> MArray s
toMArrayMBytes (MBytes ba#) = MArray ba#
{-# INLINE toMArrayMBytes #-}


