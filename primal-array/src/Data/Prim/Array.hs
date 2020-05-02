-- |
-- Module      : Data.Prim.Ptr
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}

module Data.Prim.Array where

import GHC.Exts
import GHC.TypeNats

data Array a = Array (Array# a)

data MArray a s = MArray (MutableArray# s a)

data StorableArray = Addr#


data PArray a = PArray ByteArray#

data MPArray a s = MPArray (MutableByteArray# s)


data SArray a = SArray (SmallArray# a)

data MSArray a s = MSArray (SmallMutableArray# s a)


data AArray (n :: Nat) a = AArray ArrayArray#

data MAArray (n :: Nat) a s = MAArray (MutableArrayArray# s)


data B
data N
data BS
data NS

-- indexAArray :: AArray 1 a -> Int -> PArray a

-- indexAArray :: AArray 2 a -> Int -> AArray 1 a
