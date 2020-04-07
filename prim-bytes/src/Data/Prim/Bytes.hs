{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Data.Prim.Bytes
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Bytes
    ( Pinned(..)
    , Bytes(..)
    , MBytes(..)
    , allocMBytes
    , allocPinnedMBytes
    , readMBytes
    , writeMBytes
    , setMBytes
    , getBytesPtr
    , getMBytesPtr
    , module Data.Prim
    ) where

import GHC.Int
import GHC.Word
import Control.Monad (when)
import GHC.Exts
import Data.Prim
import Data.Prim.Class
import Control.Monad.Prim
import Control.DeepSeq

data Pinned = Pinned

data Bytes (p :: Pinned) = Bytes ByteArray#

data MBytes (p :: Pinned) s = MBytes (MutableByteArray# s)

instance NFData (Bytes p) where
  rnf (Bytes _) = ()

instance NFData (MBytes p s) where
  rnf (MBytes _) = ()


newtype Es a = Es {unEs :: Int}
  deriving (Eq, Show, Ord, Enum, Bounded, Num, Integral, Real, NFData)

instance Prim (Es a) where
  type PrimBase (Es a) = Int

allocMBytes :: MonadPrim s m => Int -> m (MBytes p s)
allocMBytes (I# i#) =
  prim $ \s# ->
    case newByteArray# i# s# of
      (# s'#, ba# #) -> (# s'#, MBytes ba# #)
{-# INLINE allocMBytes #-}


allocPinnedMBytes :: MonadPrim s m => Int -> m (MBytes 'Pinned s)
allocPinnedMBytes (I# i#) =
  prim $ \s# ->
    case newPinnedByteArray# i# s# of
      (# s'#, ba# #) -> (# s'#, MBytes ba# #)
{-# INLINE allocPinnedMBytes #-}

readMBytes :: (MonadPrim s m, Prim a) => MBytes p s -> Int -> m a
readMBytes (MBytes mba#) (I# i#) = prim (readMutableByteArray# mba# i#)
{-# INLINE readMBytes #-}

writeMBytes :: (MonadPrim s m, Prim a) => MBytes p s -> Int -> a -> m ()
writeMBytes (MBytes mba#) (I# i#) a = prim_ (writeMutableByteArray# mba# i# a)
{-# INLINE writeMBytes #-}


getBytesPtr :: Bytes 'Pinned -> Ptr a
getBytesPtr (Bytes ba#) = Ptr (byteArrayContents# ba#)
{-# INLINE getBytesPtr #-}

getMBytesPtr :: MBytes 'Pinned s -> Ptr a
getMBytesPtr (MBytes mba#) = Ptr (byteArrayContents# (unsafeCoerce# mba#))
{-# INLINE getMBytesPtr #-}

setMBytes :: (MonadPrim s m, Prim a) => MBytes p s
  -> Int -- ^ Offset in number of elements
  -> Int -- ^ Number of cells to fill
  -> a -- ^ A value to fill cells with
  -> m ()
setMBytes (MBytes mba#) (I# o#) (I# n#) a = prim_ (setMutableByteArray# mba# o# n# a)

-- setMBytes (MBytes mba#) (I# o#) (I# n#) a = prim_ (go 0#)
--   where
--     go i# s#
--       | isTrue# (i# <# n#) = go (i# +# 1#) (writeMutableByteArray# mba# (i# +# o#) a s#)
--       | otherwise = s#
-- setMBytes mba o n a = go o
--   where
--     k = o + n
--     go i =
--       when (i < k) $ do
--         writeMBytes mba i a
--         go (i + 1)
-- setMBytes mba o n a = go n (getMBytesPtr mba `plusPtr` (o * s))
--   where
--     s = S.sizeOf a
--     go i ptr =
--       when (i > 0) $ do
--         S.poke ptr a
--         go (i - 1) (ptr `plusPtr` s)
{-# INLINE setMBytes #-}
