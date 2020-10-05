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
  , Atom(..)
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
  -- * Size
  , Size(..)
  -- * Count
  , Count(..)
  , unCountBytes
  , toByteCount
  , unCountBytes#
  , fromByteCount
  , fromByteCountRem
  , countToOff
  , countToByteOff
  , countForType
  , countForProxyTypeOf
  -- * Offset
  , Off(..)
  , unOffBytes
  , toByteOff
  , unOffBytes#
  , fromByteOff
  , fromByteOffRem
  , offToCount
  , offToByteCount
  , offForType
  , offForProxyTypeOf
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
  , Proxy(..)
  , module Data.Semigroup
  , module Data.Monoid
  , module Data.Coerce
  ) where

import Control.DeepSeq
import Control.Prim.Monad
import Data.Coerce
import Data.Int
import Data.Monoid hiding (First(..), Last(..), (<>))
import Data.Prim.Atom
import Data.Prim.Atomic
import Data.Prim.Class
import Data.Semigroup
import Data.Typeable
import Data.Word
import Foreign.ForeignPtr (ForeignPtr)
import GHC.Base (quotInt, quotRemInt)
import GHC.Exts

newtype Size = Size { unSize :: Int }
  deriving (Show, Eq, Ord, Num, Real, Integral, Bounded, Enum)

-- | Get the size of the data type in bytes. Argument is not evaluated.
--
-- >>> import Data.Prim
-- >>> byteCount (Just 'a')
-- Count {unCount = 5}
--
-- @since 0.1.0
byteCount :: forall e . Prim e => e -> Count Word8
byteCount _ = coerce (I# (sizeOf# (proxy# :: Proxy# e)))
{-# INLINE byteCount #-}

-- | Same as `sizeOf`, except that the type can be supplied as a type level argument
--
-- >>> :set -XTypeApplications
-- >>> import Data.Prim
-- >>> byteCountType @Int64
-- Count {unCount = 8}
--
-- @since 0.1.0
byteCountType :: forall e . Prim e => Count Word8
byteCountType = coerce (I# (sizeOf# (proxy# :: Proxy# e)))
{-# INLINE byteCountType #-}

-- | Same as `sizeOf`, but argument is a `Proxy` of @a@, instead of the type itself.
--
-- >>> import Data.Prim
-- >>> import Data.Proxy
-- >>> byteCountProxy (Proxy :: Proxy Int64)
-- Count {unCount = 8}
--
-- @since 0.1.0
byteCountProxy :: forall proxy e . Prim e => proxy e -> Count Word8
byteCountProxy _ = coerce (I# (sizeOf# (proxy# :: Proxy# e)))
{-# INLINE byteCountProxy #-}



-- | Get the alignemnt of the type in bytes. Argument is not evaluated.
--
-- @since 0.1.0
alignment :: forall e . Prim e => e -> Int
alignment _ = I# (alignment# (proxy# :: Proxy# e))
{-# INLINE alignment #-}

-- | Same as `alignment`, except that the type can be supplied with @TypeApplications@
--
-- >>> :set -XTypeApplications
-- >>> import Data.Prim
-- >>> alignmentType @Int32
-- 4
--
-- @since 0.1.0
alignmentType :: forall e . Prim e => Int
alignmentType = I# (alignment# (proxy# :: Proxy# e))
{-# INLINE alignmentType #-}

-- | Same as `alignment`, but argument is a `Proxy` of @a@, instead of the type itself.
--
-- >>> import Data.Proxy
-- >>> alignmentProxy (Proxy :: Proxy Int64)
-- 8
--
-- @since 0.1.0
alignmentProxy :: forall proxy e . Prim e => proxy e -> Int
alignmentProxy _ = I# (alignment# (proxy# :: Proxy# e))
{-# INLINE alignmentProxy #-}



-- | Number of elements
newtype Count e = Count
  { unCount :: Int
  } deriving (Eq, Show, Ord, Enum, Bounded, Num, Integral, Real, NFData)

instance Prim (Count e) where
  type PrimBase (Count e) = Int

unCountWord8# :: Count Word8 -> Int#
unCountWord8# (Count (I# n#)) = n#
{-# INLINE unCountWord8# #-}
unCountInt8# :: Count Int8 -> Int#
unCountInt8# (Count (I# n#)) = n#
{-# INLINE unCountInt8# #-}

unCountBytes# :: Prim e => Count e -> Int#
unCountBytes# c@(Count (I# n#)) =
  case coerce (byteCountProxy c) of
    I# sz# -> sz# *# n#
{-# INLINE[0] unCountBytes# #-}
{-# RULES
"unCountWord8#" unCountBytes# = unCountWord8#
"unCountInt8#" unCountBytes# = unCountInt8#
  #-}

-- | Covert an element count to number of bytes it coresponds to as an `Int`. See
-- `toByteCount` for preserving the `Count` wrapper.
--
-- @since 0.1.0
unCountBytes :: Prim e => Count e -> Int
unCountBytes c = I# (unCountBytes# c)
{-# INLINE unCountBytes #-}


-- | Covert to the `Count` of bytes
--
-- @since 0.1.0
toByteCount :: Prim e => Count e -> Count Word8
toByteCount = Count . unCountBytes
{-# INLINE toByteCount #-}

-- | Cast a count to an offset of the same type
--
-- @since 0.2.0
countToOff :: Count e -> Off e
countToOff = coerce

countToByteOff :: Prim e => Count e -> Off Word8
countToByteOff = countToOff . toByteCount
{-# INLINE countToByteOff #-}

-- | Helper noop function that restricts `Count` to the type of proxy
--
-- @since 0.2.0
countForProxyTypeOf :: Count e -> proxy e -> Count e
countForProxyTypeOf count _ = count

-- | Restrict type argument of `Count` to the same type as the second argument, which
-- itself is not evaluated
--
-- @since 0.2.0
countForType :: Count e -> e -> Count e
countForType count _ = count

fromByteCountInt8 :: Count Word8 -> Count Int8
fromByteCountInt8 = coerce
{-# INLINE fromByteCountInt8 #-}


-- | Compute how many elements of type @e@ can fit in the supplied number of bytes.
--
-- @since 0.1.0
fromByteCount :: forall e . Prim e => Count Word8 -> Count e
fromByteCount sz = coerce (quotSizeOfWith (proxy# :: Proxy# e) (coerce sz) 0 quotInt)
{-# INLINE[0] fromByteCount #-}
{-# RULES
"fromByteCount" fromByteCount = id
"fromByteCount" fromByteCount = fromByteCountInt8
  #-}

fromByteCountRemWord8 :: Count Word8 -> (Count Word8, Count Word8)
fromByteCountRemWord8 i = (coerce i, 0)
{-# INLINE fromByteCountRemWord8 #-}

fromByteCountRemInt8 :: Count Word8 -> (Count Int8, Count Word8)
fromByteCountRemInt8 i = (coerce i, 0)
{-# INLINE fromByteCountRemInt8 #-}


fromByteCountRem :: forall e . Prim e => Count Word8 -> (Count e, Count Word8)
fromByteCountRem sz = coerce (quotSizeOfWith (proxy# :: Proxy# e) (coerce sz) (0, 0) quotRemInt)
{-# INLINE[0] fromByteCountRem #-}
{-# RULES
"fromByteCountRemWord8" fromByteCountRem = fromByteCountRemWord8
"fromByteCountRemInt8"  fromByteCountRem = fromByteCountRemInt8
  #-}

quotSizeOfWith :: forall e b. Prim e => Proxy# e -> Int -> b -> (Int -> Int -> b) -> b
quotSizeOfWith px# sz onZero quotWith
  | tySize <= 0 = onZero
  | otherwise = sz `quotWith` tySize
  where
    tySize = I# (sizeOf# px#)
{-# INLINE quotSizeOfWith #-}


-- | Offset in number of elements
newtype Off e = Off
  { unOff :: Int
  } deriving (Eq, Show, Ord, Enum, Bounded, Num, Integral, Real, NFData)

instance Prim (Off e) where
  type PrimBase (Off e) = Int


-- | Helper noop function that restricts `Off`set to the type of proxy
--
-- @since 0.2.0
offForProxyTypeOf :: Off e -> proxy e -> Off e
offForProxyTypeOf off _ = off

-- | Restrict type argument of `Off` to the same type as the second argument, which itself
-- is not evaluated
--
-- @since 0.2.0
offForType :: Off e -> e -> Off e
offForType c _ = c

-- | Cast an offset to count. Useful for dealing with regions.
--
-- >>> import Data.Prim
-- >>> let totalCount = Count 10 :: Count Word
-- >>> let startOffset = Off 4 :: Off Word
-- >>> totalCount - offToCount startOffset
-- Count {unCount = 6}
--
-- @since 0.2.0
offToCount :: Off e -> Count e
offToCount = coerce

-- | Convert an offset in elements to count in bytres.
--
-- @since 0.2.0
offToByteCount :: Prim e => Off e -> Count Word8
offToByteCount = offToCount . toByteOff
{-# INLINE offToByteCount #-}

-- | Compute byte offset from an offset of `Prim` type
--
-- >>> toByteOff (10 :: Off Word64)
-- Off {unOff = 80}
--
-- @since 0.1.0
toByteOff :: Prim e => Off e -> Off Word8
toByteOff off = Off (I# (unOffBytes# off))
{-# INLINE toByteOff #-}


-- | Convert an offset for some type @e@ with `Prim` instance to the number of bytes as an
-- `Int`.
--
-- >>> unOffBytes (10 :: Off Word64)
-- 80
--
-- @since 0.2.0
unOffBytes :: Prim e => Off e -> Int
unOffBytes off = I# (unOffBytes# off)
{-# INLINE unOffBytes #-}



unOffWord8# :: Off Word8 -> Int#
unOffWord8# (Off (I# o#)) = o#
{-# INLINE unOffWord8# #-}
unOffInt8# :: Off Int8 -> Int#
unOffInt8# (Off (I# o#)) = o#
{-# INLINE unOffInt8# #-}

-- | Convert offset of some type into number of bytes
unOffBytes# :: Prim e => Off e -> Int#
unOffBytes# o@(Off (I# o#)) =
  case coerce (byteCountProxy o) of
    I# sz# -> sz# *# o#
{-# INLINE[0] unOffBytes# #-}
{-# RULES
"unOffWord8#" unOffBytes# = unOffWord8#
"unOffInt8#" unOffBytes# = unOffInt8#
  #-}



fromByteOffInt8 :: Off Word8 -> Off Int8
fromByteOffInt8 = coerce
{-# INLINE fromByteOffInt8 #-}

fromByteOff :: forall e . Prim e => Off Word8 -> Off e
fromByteOff sz = coerce (quotSizeOfWith (proxy# :: Proxy# e) (coerce sz) 0 quotInt)
{-# INLINE[0] fromByteOff #-}
{-# RULES
"fromByteOff" fromByteOff = id
"fromByteOff" fromByteOff = fromByteOffInt8
  #-}

fromByteOffRemWord8 :: Off Word8 -> (Off Word8, Off Word8)
fromByteOffRemWord8 i = (coerce i, 0)
{-# INLINE fromByteOffRemWord8 #-}

fromByteOffRemInt8 :: Off Word8 -> (Off Int8, Off Word8)
fromByteOffRemInt8 i = (coerce i, 0)
{-# INLINE fromByteOffRemInt8 #-}


fromByteOffRem :: forall e . Prim e => Off Word8 -> (Off e, Off Word8)
fromByteOffRem sz = coerce (quotSizeOfWith (proxy# :: Proxy# e) (coerce sz) (0, 0) quotRemInt)
{-# INLINE[0] fromByteOffRem #-}
{-# RULES
"fromByteOffRemWord8" fromByteOffRem = fromByteOffRemWord8
"fromByteOffRemInt8"  fromByteOffRem = fromByteOffRemInt8
  #-}



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
