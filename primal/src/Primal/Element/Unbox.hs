{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Primal.Element.Unbox
-- Copyright   : (c) Alexey Kuleshevich 2020-2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Element.Unbox
  ( module Primal.Element.Unbox.Class
  , module Primal.Element.Unbox.Instances
  , Atom(..)
  , Atomic
  , AtomicCount
  , AtomicBits
  , showsType
  -- * Unboxed element size
  , byteCount
  , byteCountType
  , byteCountProxy
  -- * Unboxed element alignment
  , alignment
  , alignmentType
  , alignmentProxy
  -- * Unboxed element count
  , Count(..)
  , unCountBytes
  , toByteCount
  , unCountBytes#
  , fromByteCount
  , fromByteCountRem
  , offPlusCount
  , offMinusCount
  , countPlusOff
  , countMinusOff
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
  , module Data.Coerce
  , (#.)
  , (.#)
  ) where

import Control.DeepSeq
import Data.Coerce
import Data.Int
import Data.Typeable
import Data.Word
import Foreign.ForeignPtr (ForeignPtr)
import GHC.Base (quotInt, quotRemInt)
import GHC.Exts
import Primal.Element.Unbox.Atom
import Primal.Element.Unbox.Atomic
import Primal.Element.Unbox.Class
import Primal.Element.Unbox.Instances
import Primal.Monad


-- | Helper function that converts a type into a string
--
-- @since 0.3.0
showsType :: Typeable t => proxy t -> ShowS
showsType = showsTypeRep . typeRep

-- | Get the size of the data type in bytes. Argument is not evaluated.
--
-- >>> import Primal.Unbox
-- >>> byteCount (Just 'a')
-- Count {unCount = 5}
--
-- @since 0.1.0
byteCount :: forall e . Unbox e => e -> Count Word8
byteCount _ = coerce (I# (sizeOf# (proxy# :: Proxy# e)))
{-# INLINE byteCount #-}

-- | Same as `sizeOf`, except that the type can be supplied as a type level argument
--
-- >>> :set -XTypeApplications
-- >>> import Primal.Unbox
-- >>> byteCountType @Int64
-- Count {unCount = 8}
--
-- @since 0.1.0
byteCountType :: forall e . Unbox e => Count Word8
byteCountType = coerce (I# (sizeOf# (proxy# :: Proxy# e)))
{-# INLINE byteCountType #-}

-- | Same as `byteCount`, but argument is a `Proxy` of @e@, instead of the type itself.
--
-- >>> import Primal.Unbox
-- >>> import Data.Proxy
-- >>> byteCountProxy (Proxy :: Proxy Int64)
-- Count {unCount = 8}
--
-- @since 0.1.0
byteCountProxy :: forall proxy e . Unbox e => proxy e -> Count Word8
byteCountProxy _ = coerce (I# (sizeOf# (proxy# :: Proxy# e)))
{-# INLINE byteCountProxy #-}



-- | Get the alignemnt of the type in bytes. Argument is not evaluated.
--
-- @since 0.1.0
alignment :: forall e . Unbox e => e -> Int
alignment _ = I# (alignment# (proxy# :: Proxy# e))
{-# INLINE alignment #-}

-- | Same as `alignment`, except that the type can be supplied with @TypeApplications@
--
-- >>> :set -XTypeApplications
-- >>> import Primal.Unbox
-- >>> alignmentType @Int32
-- 4
--
-- @since 0.1.0
alignmentType :: forall e . Unbox e => Int
alignmentType = I# (alignment# (proxy# :: Proxy# e))
{-# INLINE alignmentType #-}

-- | Same as `alignment`, but argument is a `Proxy` of @a@, instead of the type itself.
--
-- >>> import Data.Proxy
-- >>> alignmentProxy (Proxy :: Proxy Int64)
-- 8
--
-- @since 0.1.0
alignmentProxy :: forall proxy e . Unbox e => proxy e -> Int
alignmentProxy _ = I# (alignment# (proxy# :: Proxy# e))
{-# INLINE alignmentProxy #-}



-- | Number of elements
newtype Count e = Count
  { unCount :: Int
  } deriving (Eq, Show, Ord, Enum, Bounded, Num, Integral, Real, NFData)

instance Unbox (Count e) where
  type UnboxIso (Count e) = Int

unCountWord8# :: Count Word8 -> Int#
unCountWord8# (Count (I# n#)) = n#
{-# INLINE unCountWord8# #-}
unCountInt8# :: Count Int8 -> Int#
unCountInt8# (Count (I# n#)) = n#
{-# INLINE unCountInt8# #-}

unCountBytes# :: Unbox e => Count e -> Int#
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
unCountBytes :: Unbox e => Count e -> Int
unCountBytes c = I# (unCountBytes# c)
{-# INLINE unCountBytes #-}


-- | Covert to the `Count` of bytes
--
-- @since 0.1.0
toByteCount :: Unbox e => Count e -> Count Word8
toByteCount = Count . unCountBytes
{-# INLINE toByteCount #-}

-- | Cast a count to an offset of the same type
--
-- @since 0.2.0
countToOff :: Count e -> Off e
countToOff = coerce

countToByteOff :: Unbox e => Count e -> Off Word8
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

-- | Compute how many elements of type @e@ can fit in the supplied number of bytes.
--
-- @since 0.1.0
fromByteCount :: forall e . Unbox e => Count Word8 -> Count e
fromByteCount sz = coerce (quotSizeOfWith (proxy# :: Proxy# e) (coerce sz) 0 quotInt)
{-# INLINE[0] fromByteCount #-}
{-# RULES
"fromByteCount" fromByteCount = id
"fromByteCount" fromByteCount = coerce :: Count Word8 -> Count Int8
  #-}


fromByteCountRem :: forall e . Unbox e => Count Word8 -> (Count e, Count Word8)
fromByteCountRem sz = coerce (quotSizeOfWith (proxy# :: Proxy# e) (coerce sz) (0, 0) quotRemInt)
{-# INLINE[0] fromByteCountRem #-}
{-# RULES
"fromByteCountRemWord8"
  fromByteCountRem = (, 0) :: Count Word8 -> (Count Word8, Count Word8)
"fromByteCountRemInt8"
  fromByteCountRem = (, 0) . coerce :: Count Word8 -> (Count Int8, Count Word8)
  #-}

quotSizeOfWith :: forall e b. Unbox e => Proxy# e -> Int -> b -> (Int -> Int -> b) -> b
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

instance Unbox (Off e) where
  type UnboxIso (Off e) = Int


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
-- >>> import Primal.Unbox
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
offToByteCount :: Unbox e => Off e -> Count Word8
offToByteCount = offToCount . toByteOff
{-# INLINE offToByteCount #-}

-- | Compute byte offset from an offset of `Unbox` type
--
-- >>> toByteOff (10 :: Off Word64)
-- Off {unOff = 80}
--
-- @since 0.1.0
toByteOff :: Unbox e => Off e -> Off Word8
toByteOff off = Off (I# (unOffBytes# off))
{-# INLINE toByteOff #-}


-- | Convert an offset for some type @e@ with `Unbox` instance to the number of bytes as an
-- `Int`.
--
-- >>> unOffBytes (10 :: Off Word64)
-- 80
--
-- @since 0.2.0
unOffBytes :: Unbox e => Off e -> Int
unOffBytes off = I# (unOffBytes# off)
{-# INLINE unOffBytes #-}



unOffWord8# :: Off Word8 -> Int#
unOffWord8# (Off (I# o#)) = o#
{-# INLINE unOffWord8# #-}
unOffInt8# :: Off Int8 -> Int#
unOffInt8# (Off (I# o#)) = o#
{-# INLINE unOffInt8# #-}

-- | Convert offset of some type into number of bytes
unOffBytes# :: Unbox e => Off e -> Int#
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

fromByteOff :: forall e . Unbox e => Off Word8 -> Off e
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


fromByteOffRem :: forall e . Unbox e => Off Word8 -> (Off e, Off Word8)
fromByteOffRem sz = coerce (quotSizeOfWith (proxy# :: Proxy# e) (coerce sz) (0, 0) quotRemInt)
{-# INLINE[0] fromByteOffRem #-}
{-# RULES
"fromByteOffRemWord8" fromByteOffRem = fromByteOffRemWord8
"fromByteOffRemInt8"  fromByteOffRem = fromByteOffRemInt8
  #-}




offPlusCount :: Off e -> Count e -> Off e
offPlusCount o c = Off (coerce o + coerce c)
{-# INLINE offPlusCount #-}

offMinusCount :: Off e -> Count e -> Off e
offMinusCount o c = Off (coerce o - coerce c)
{-# INLINE offMinusCount #-}



countPlusOff :: Count e -> Off e -> Count e
countPlusOff c o = Count (coerce c + coerce o)
{-# INLINE countPlusOff #-}

countMinusOff :: Count e -> Off e -> Count e
countMinusOff c o = Count (coerce c - coerce o)
{-# INLINE countMinusOff #-}


prefetchValue0 :: Primal s m => a -> m ()
prefetchValue0 a = primal_ (prefetchValue0# a)
{-# INLINE prefetchValue0 #-}

prefetchValue1 :: Primal s m => a -> m ()
prefetchValue1 a = primal_ (prefetchValue1# a)
{-# INLINE prefetchValue1 #-}

prefetchValue2 :: Primal s m => a -> m ()
prefetchValue2 a = primal_ (prefetchValue2# a)
{-# INLINE prefetchValue2 #-}

prefetchValue3 :: Primal s m => a -> m ()
prefetchValue3 a = primal_ (prefetchValue3# a)
{-# INLINE prefetchValue3 #-}


-- | Coerce result of a function (it is also a hidden function in Data.Functor.Utils)
--
-- @since 0.3.0
(#.) :: forall a b c proxy. Coercible b c => proxy b c -> (a -> b) -> (a -> c)
(#.) _px = coerce
{-# INLINE (#.) #-}

-- | Coerce result of a function. Flipped version of `(#.)`
--
-- @since 0.3.0
(.#) :: forall a b c proxy. Coercible b c => (a -> b) -> proxy b c -> (a -> c)
(.#) f _px = coerce f
{-# INLINE (.#) #-}
