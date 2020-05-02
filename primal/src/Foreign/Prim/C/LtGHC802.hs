{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
-- |
-- Module      : Foreign.Prim.C.LtGHC802
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Foreign.Prim.C.LtGHC802
  (
  -- ** GHC-8.2
    CBool(..)
  , isByteArrayPinned#
  , isMutableByteArrayPinned#
  -- ** GHC-8.0
  , compareByteArrays#
  , getSizeofMutableByteArray#
  ) where

import GHC.Exts

#if __GLASGOW_HASKELL__ < 802

import Data.Bits (Bits, FiniteBits)
import Data.Word
import Foreign.Storable (Storable)
import Control.DeepSeq

#include "MachDeps.h"
#include "HsBaseConfig.h"
#include "primal_compat.h"


-- | This type was added in base-4.10 and is provided from here for backwards compatibility
newtype {-# CTYPE "bool" #-} CBool = CBool HTYPE_BOOL
  deriving (Eq, Ord, Num, Enum, Storable, Real, Bounded, Integral, Bits, FiniteBits, NFData, Read, Show)

-- | Determine whether a `ByteArray#` is guaranteed not to move during GC.
foreign import ccall unsafe "primal_compat.c primal_is_byte_array_pinned"
  isByteArrayPinned# :: ByteArray# -> Int#

-- | Determine whether a `MutableByteArray#` is guaranteed not to move during GC.
foreign import ccall unsafe "primal_compat.c primal_is_byte_array_pinned"
  isMutableByteArrayPinned# :: MutableByteArray# s -> Int#

-- | This is equivalent to @memcmp@ in C
foreign import ccall unsafe "primal.c primal_memcmp"
  compareByteArrays# :: ByteArray# -> Int# -> ByteArray# -> Int# -> Int# -> Int#

#if __GLASGOW_HASKELL__ < 800
-- | Compatibility function for ghc-7.10 version
getSizeofMutableByteArray# :: MutableByteArray# s -> State# s -> (# State# s, Int# #)
getSizeofMutableByteArray# mba# s = (# s, sizeofMutableByteArray# mba# #)
{-# INLINE getSizeofMutableByteArray# #-}

#endif /* __GLASGOW_HASKELL__ < 800 */

#else

import Foreign.C.Types (CBool(..))

#endif /* __GLASGOW_HASKELL__ < 802 */
