{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
-- |
-- Module      : Foreign.Prim.Ptr
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Foreign.Prim.Ptr
  ( module GHC.Ptr
  , module X
  , WordPtr(..)
  , ptrToWordPtr
  , wordPtrToPtr
  , IntPtr(..)
  , ptrToIntPtr
  , intPtrToPtr
  ) where

import GHC.Ptr
import Foreign.Ptr as X hiding
  ( IntPtr
  , WordPtr
  , intPtrToPtr
  , ptrToIntPtr
  , ptrToWordPtr
  , wordPtrToPtr
  )

-- Deal with the fact that IntPtr and WordPtr constructors are hidden
#if __GLASGOW_HASKELL__ < 802
import Data.Bits (Bits, FiniteBits)
import Foreign.Storable (Storable)
import GHC.Int
import GHC.Word
import GHC.Exts

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

#else

import Foreign.Ptr

#endif
