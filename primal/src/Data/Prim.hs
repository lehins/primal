{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.Prim
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim
  ( Prim
  , Atomic
  , AtomicCount
  , AtomicBits
  , MonadPrim
  , RW
  , RealWorld
  -- * Prim type size
  , byteCount
  , byteCountType
  , byteCountProxy
  -- * Prim type alignment
  , alignment
  , alignmentType
  , alignmentProxy
  , Size(..)
  , Count(..)
  , fromCount
  , toByteCount
  , fromCount#
  , fromByteCount
  , fromByteCountRem
  , countAsProxy
  , Off(..)
  , toByteOff
  , fromOff#
  , offAsProxy
  -- * Prefetch
  , prefetchValue0
  , prefetchValue1
  , prefetchValue2
  , prefetchValue3
  -- * Re-export
  , module Data.Word
  , module Data.Int
  , Ptr
  , ForeignPtr
  , Typeable
  , module Data.Monoid
  , module Data.Coerce
  ) where

import Control.DeepSeq
import Control.Prim.Monad
import Data.Prim.Atomic
import Data.Prim.Class
import GHC.Base (quotInt,  quotRemInt)
import GHC.Exts
import Data.Word
import Data.Int
import Foreign.ForeignPtr (ForeignPtr)
import Data.Monoid
import Data.Coerce
import Data.Typeable

newtype Size = Size { unSize :: Int }
  deriving (Show, Eq, Ord, Num, Real, Integral, Bounded, Enum)

-- | Get the size of the data type in bytes. Argument is not evaluated.
byteCount :: forall a . Prim a => a -> Count Word8
byteCount _ = coerce (I# (sizeOf# (proxy# :: Proxy# a)))
{-# INLINE byteCount #-}

-- | Same as `sizeOf`, except that the type can be supplied as a type level argument
--
-- >>> :set -XTypeApplications
-- >>> import Data.Prim
-- >>> byteCountType @Int64
-- Count {unCount = 8}
--
byteCountType :: forall a . Prim a => Count Word8
byteCountType = coerce (I# (sizeOf# (proxy# :: Proxy# a)))
{-# INLINE byteCountType #-}

-- | Same as `sizeOf`, but argument is a `Proxy` of @a@, instead of the type itself.
--
-- >>> import Data.Prim
-- >>> import Data.Proxy
-- >>> byteCountProxy (Proxy :: Proxy Int64)
-- Count {unCount = 8}
--
byteCountProxy :: forall proxy a . Prim a => proxy a -> Count Word8
byteCountProxy _ = coerce (I# (sizeOf# (proxy# :: Proxy# a)))
{-# INLINE byteCountProxy #-}



-- | Get the size of the dat type in bytes. Argument is not evaluated.
alignment :: forall a . Prim a => a -> Count Word8
alignment _ = coerce (I# (alignment# (proxy# :: Proxy# a)))
{-# INLINE alignment #-}

-- | Same as `alignment`, except that the type can be supplied at the type level
--
-- >>> :set -XTypeApplications
-- >>> import Data.Prim
-- >>> alignmentType @Int64
-- Count {unCount = 8}
--
alignmentType :: forall a . Prim a => Count Word8
alignmentType = coerce (I# (alignment# (proxy# :: Proxy# a)))
{-# INLINE alignmentType #-}

-- | Same as `alignment`, but argument is a `Proxy` of @a@, instead of the type itself.
--
-- >>> import Data.Proxy
-- >>> alignmentProxy (Proxy :: Proxy Int64)
-- Count {unCount = 8}
--
alignmentProxy :: forall proxy a . Prim a => proxy a -> Count Word8
alignmentProxy _ = coerce (I# (alignment# (proxy# :: Proxy# a)))
{-# INLINE alignmentProxy #-}



-- | Number of elements
newtype Count a = Count
  { unCount :: Int
  } deriving (Eq, Show, Ord, Enum, Bounded, Num, Integral, Real, NFData)

instance Prim (Count a) where
  type PrimBase (Count a) = Int

fromCountWord8# :: Count Word8 -> Int#
fromCountWord8# (Count (I# n#)) = n#
{-# INLINE fromCountWord8# #-}
fromCountInt8# :: Count Int8 -> Int#
fromCountInt8# (Count (I# n#)) = n#
{-# INLINE fromCountInt8# #-}

fromCount# :: Prim a => Count a -> Int#
fromCount# c@(Count (I# n#)) =
  case coerce (byteCountProxy c) of
    I# sz# -> sz# *# n#
{-# INLINE[0] fromCount# #-}
{-# RULES
"fromCountWord8#" fromCount# = fromCountWord8#
"fromCountInt8#" fromCount# = fromCountInt8#
  #-}

fromCount :: Prim a => Count a -> Int
fromCount c = I# (fromCount# c)
{-# INLINE fromCount #-}

toByteCount :: Prim a => Count a -> Count Word8
toByteCount = Count . fromCount
{-# INLINE toByteCount #-}

-- | Offset in number of elements
newtype Off a = Off
  { unOff :: Int
  } deriving (Eq, Show, Ord, Enum, Bounded, Num, Integral, Real, NFData)

instance Prim (Off a) where
  type PrimBase (Off a) = Int

fromOffWord8# :: Off Word8 -> Int#
fromOffWord8# (Off (I# o#)) = o#
{-# INLINE fromOffWord8# #-}
fromOffInt8# :: Off Int8 -> Int#
fromOffInt8# (Off (I# o#)) = o#
{-# INLINE fromOffInt8# #-}

-- | Convert offset of some type into number of bytes
fromOff# :: Prim a => Off a -> Int#
fromOff# o@(Off (I# o#)) =
  case coerce (byteCountProxy o) of
    I# sz# -> sz# *# o#
{-# INLINE[0] fromOff# #-}
{-# RULES
"fromOffWord8#" fromOff# = fromOffWord8#
"fromOffInt8#" fromOff# = fromOffInt8#
  #-}

-- | Compute byte offset from an offset of `Prim` type
--
-- >>> toByteOff (10 :: Off Word64)
-- Off {unOff = 80}
--
-- @since 0.1.0
toByteOff :: Prim e => Off e -> Off Word8
toByteOff off = Off (I# (fromOff# off))
{-# INLINE toByteOff #-}

-- | Helper noop function that restricts `Off`set to the type of proxy
--
-- @since 0.1.0
offAsProxy :: proxy a -> Off a -> Off a
offAsProxy _ = id

-- | Helper noop function that restricts `Count` to the type of proxy
--
-- @since 0.1.0
countAsProxy :: proxy a -> Count a -> Count a
countAsProxy _ = id

fromByteCountInt8 :: Count Word8 -> Count Int8
fromByteCountInt8 = coerce
{-# INLINE fromByteCountInt8 #-}

fromByteCount :: forall a . Prim a => Count Word8 -> Count a
fromByteCount sz = coerce (quotSizeOfWith (proxy# :: Proxy# a) (coerce sz) 0 quotInt)
{-# INLINE[0] fromByteCount #-}
{-# RULES
"fromByteCount" fromByteCount = id
"fromByteCount" fromByteCount = fromByteCountInt8
  #-}

fromByteCountRemWord8 :: Count Word8 -> (Count Word8, Int)
fromByteCountRemWord8 i = (coerce i, 0)
{-# INLINE fromByteCountRemWord8 #-}

fromByteCountRemInt8 :: Count Word8 -> (Count Int8, Int)
fromByteCountRemInt8 i = (coerce i, 0)
{-# INLINE fromByteCountRemInt8 #-}


fromByteCountRem :: forall a . Prim a => Count Word8 -> (Count a, Int)
fromByteCountRem sz = coerce (quotSizeOfWith (proxy# :: Proxy# a) (coerce sz) (0, 0) quotRemInt)
{-# INLINE[0] fromByteCountRem #-}
{-# RULES
"fromByteCountRemWord8" fromByteCountRem = fromByteCountRemWord8
"fromByteCountRemInt8"  fromByteCountRem = fromByteCountRemInt8
  #-}

quotSizeOfWith :: forall a b. Prim a => Proxy# a -> Int -> b -> (Int -> Int -> b) -> b
quotSizeOfWith px# sz onZero quotWith
  | tySize <= 0 = onZero
  | otherwise = sz `quotWith` tySize
  where
    tySize = I# (sizeOf# px#)
{-# INLINE quotSizeOfWith #-}



prefetchValue0 :: MonadPrim s m => a -> m ()
prefetchValue0 a = prim_ (prefetchValue0# a)
{-# INLINE prefetchValue0 #-}

prefetchValue1 :: MonadPrim s m => a -> m ()
prefetchValue1 a = prim_ (prefetchValue1# a)
{-# INLINE prefetchValue1 #-}

prefetchValue2 :: MonadPrim s m => a -> m ()
prefetchValue2 a = prim_ (prefetchValue2# a)
{-# INLINE prefetchValue2 #-}

prefetchValue3 :: MonadPrim s m => a -> m ()
prefetchValue3 a = prim_ (prefetchValue3# a)
{-# INLINE prefetchValue3 #-}
