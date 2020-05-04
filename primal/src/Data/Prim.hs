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
  , sizeOf
  , sizeOfType
  , sizeOfProxy
  -- * Prim type alignment
  , alignment
  , alignmentType
  , alignmentProxy
  , Count(..)
  , fromCount
  , countWord8
  , fromCount#
  , countSize
  , countRemSize
  , countAsProxy
  , Off(..)
  , fromOff
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
  , module Data.Monoid
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


-- | Get the size of the data type in bytes. Argument is not evaluated.
sizeOf :: forall a . Prim a => a -> Int
sizeOf _ = sizeOf# (proxy# :: Proxy# a)
{-# INLINE sizeOf #-}

-- | Same as `sizeOf`, except that the type can be supplied as a type level argument
--
-- >>> :set -XTypeApplications
-- >>> import Data.Prim
-- >>> sizeOfType @Int64
-- 8
--
sizeOfType :: forall a . Prim a => Int
sizeOfType = sizeOf# (proxy# :: Proxy# a)
{-# INLINE sizeOfType #-}

-- | Same as `sizeOf`, but argument is a `Proxy` of @a@, instead of the type itself.
--
-- >>> import Data.Prim
-- >>> import Data.Proxy
-- >>> sizeOfProxy (Proxy :: Proxy Int64)
-- 8
--
sizeOfProxy :: forall proxy a . Prim a => proxy a -> Int
sizeOfProxy _ = sizeOf# (proxy# :: Proxy# a)
{-# INLINE sizeOfProxy #-}



-- | Get the size of the dat type in bytes. Argument is not evaluated.
alignment :: forall a . Prim a => a -> Int
alignment _ = alignment# (proxy# :: Proxy# a)
{-# INLINE alignment #-}

-- | Same as `alignment`, except that the type can be supplied at the type level
--
-- >>> :set -XTypeApplications
-- >>> import Data.Prim
-- >>> alignmentType @Int64
-- 8
--
alignmentType :: forall a . Prim a => Int
alignmentType = alignment# (proxy# :: Proxy# a)
{-# INLINE alignmentType #-}

-- | Same as `alignment`, but argument is a `Proxy` of @a@, instead of the type itself.
--
-- >>> import Data.Proxy
-- >>> alignmentProxy (Proxy :: Proxy Int64)
-- 8
--
alignmentProxy :: forall proxy a . Prim a => proxy a -> Int
alignmentProxy _ = alignment# (proxy# :: Proxy# a)
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
  case sizeOfProxy c of
    (I# sz#) -> sz# *# n#
{-# INLINE[0] fromCount# #-}
{-# RULES
"fromCountWord8#" fromCount# = fromCountWord8#
"fromCountInt8#" fromCount# = fromCountInt8#
  #-}

fromCount :: Prim a => Count a -> Int
fromCount c = I# (fromCount# c)
{-# INLINE fromCount #-}

countWord8 :: Prim a => Count a -> Count Word8
countWord8 = Count . fromCount
{-# INLINE countWord8 #-}

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

fromOff# :: Prim a => Off a -> Int#
fromOff# o@(Off (I# o#)) =
  case sizeOfProxy o of
    (I# sz#) -> sz# *# o#
{-# INLINE[0] fromOff# #-}
{-# RULES
"fromOffWord8#" fromOff# = fromOffWord8#
"fromOffInt8#" fromOff# = fromOffInt8#
  #-}


fromOff :: Prim a => Off a -> Int
fromOff c = I# (fromOff# c)
{-# INLINE fromOff #-}

offAsProxy :: proxy a -> Off a -> Off a
offAsProxy _ = id

countAsProxy :: proxy a -> Count a -> Count a
countAsProxy _ = id

countSizeWord8 :: Int -> Count Word8
countSizeWord8 = coerce
{-# INLINE countSizeWord8 #-}

countSizeInt8 :: Int -> Count Int8
countSizeInt8 = coerce
{-# INLINE countSizeInt8 #-}

countSize :: forall a . Prim a => Int -> Count a
countSize sz =  coerce (quotSizeOfWith (proxy# :: Proxy# a) sz 0 quotInt)
{-# INLINE[0] countSize #-}
{-# RULES
"countSize" countSize = countSizeWord8
"countSize" countSize = countSizeInt8
  #-}

countRemSizeWord8 :: Int -> (Count Word8, Int)
countRemSizeWord8 i = (coerce i, 0)
{-# INLINE countRemSizeWord8 #-}

countRemSizeInt8 :: Int -> (Count Int8, Int)
countRemSizeInt8 i = (coerce i, 0)
{-# INLINE countRemSizeInt8 #-}


countRemSize :: forall a . Prim a => Int -> (Count a, Int)
countRemSize sz = coerce (quotSizeOfWith (proxy# :: Proxy# a) sz (0, 0) quotRemInt)
{-# INLINE[0] countRemSize #-}
{-# RULES
"countRemSize" countRemSize = countRemSizeWord8
"countRemSize" countRemSize = countRemSizeInt8
  #-}

quotSizeOfWith :: forall a b. Prim a => Proxy# a -> Int -> b -> (Int -> Int -> b) -> b
quotSizeOfWith px# sz onZero quotWith
  | tySize <= 0 = onZero
  | otherwise = sz `quotWith` tySize
  where
    tySize = sizeOf# px#
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
