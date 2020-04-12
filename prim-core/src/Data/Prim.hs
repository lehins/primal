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
  , MonadPrim
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
  , Off(..)
  , fromOff
  , fromOff#
  , module Data.Word
  , module Data.Int
  ) where

import Control.DeepSeq
import Control.Monad.Prim
import Data.Prim.Class
import GHC.Base (quotInt,  quotRemInt)
import GHC.Exts
import Data.Word
import Data.Int


-- | Get the size of the data type in bytes. Argument is not evaluated.
sizeOf :: forall a . Prim a => a -> Int
sizeOf _ = sizeOf# (proxy# :: Proxy# a)
{-# INLINE sizeOf #-}

-- | Same as `sizeOf`, except that the type can be supplied as a type level argument
--
-- >>> :set -XTypeApplications
-- >>> sizeOfType @Int64
-- 8
--
sizeOfType :: forall a . Prim a => Int
sizeOfType = sizeOf# (proxy# :: Proxy# a)
{-# INLINE sizeOfType #-}

-- | Same as `sizeOf`, but argument is a `Proxy` of @a@, instead of the type itself.
--
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
{-# RULES
"fromCountWord8#" fromCount# = fromCountWord8#
"fromCountInt8#" fromCount# = fromCountInt8#
  #-}

fromCount# :: Prim a => Count a -> Int#
fromCount# c@(Count (I# n#)) =
  case sizeOfProxy c of
    (I# sz#) -> sz# *# n#
{-# INLINE[0] fromCount# #-}

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
{-# RULES
"fromOffWord8#" fromOff# = fromOffWord8#
"fromOffInt8#" fromOff# = fromOffInt8#
  #-}

fromOff# :: Prim a => Off a -> Int#
fromOff# o@(Off (I# o#)) =
  case sizeOfProxy o of
    (I# sz#) -> sz# *# o#
{-# INLINE[0] fromOff# #-}


fromOff :: Prim a => Off a -> Int
fromOff c = I# (fromOff# c)
{-# INLINE fromOff #-}


countSize :: forall a . Prim a => Int -> Count a
countSize sz =  coerce (quotSizeOfWith (proxy# :: Proxy# a) sz 0 quotInt)
{-# INLINE countSize #-}

countRemSize :: forall a . Prim a => Int -> (Count a, Int)
countRemSize sz =  coerce (quotSizeOfWith (proxy# :: Proxy# a) sz (0, 0) quotRemInt)
{-# INLINE countRemSize #-}

quotSizeOfWith :: forall a b. Prim a => Proxy# a -> Int -> b -> (Int -> Int -> b) -> b
quotSizeOfWith px# sz onZero quotWith
  | tySize <= 0 = onZero
  | otherwise = sz `quotWith` tySize
  where
    tySize = sizeOf# px#
{-# INLINE quotSizeOfWith #-}
