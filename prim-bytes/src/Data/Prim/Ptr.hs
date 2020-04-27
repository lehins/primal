{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Prim.Ptr
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Ptr
  ( Ptr(..)
  , nullPtr
  , plusPtrOff
  , castPtr
  , readPtr
  , readOffPtr
  , readByteOffPtr
  , writePtr
  , writeOffPtr
  , writeByteOffPtr
  , setOffPtr

  , copyPtrToPtr
  , movePtrToPtr
  , copyPtrToMBytes
  , movePtrToMBytes
  , copyBytesToPtr
  , copyMBytesToPtr
  , moveMBytesToPtr
  , module Data.Prim
  ) where


import Control.Prim.Monad
import Control.Prim.Monad.Unsafe
import Data.Prim
import Data.Prim.Bytes
import Data.Prim.Class
import Foreign.Marshal.Utils
import Foreign.Prim
import Foreign.Prim.Ptr



setOffPtr ::
     (MonadPrim s m, Prim a)
  => Ptr a -- ^ Chunk of memory to fill
  -> Off a -- ^ Offset in number of elements
  -> Count a -- ^ Number of cells to fill
  -> a -- ^ A value to fill the cells with
  -> m ()
setOffPtr (Ptr addr#) (Off (I# o#)) (Count (I# n#)) a = prim_ (setOffAddr# addr# o# n# a)
{-# INLINE setOffPtr #-}


readOffPtr :: (MonadPrim s m, Prim a) => Ptr a -> Off a -> m a
readOffPtr (Ptr addr#) (Off (I# i#)) = prim (readOffAddr# addr# i#)
{-# INLINE readOffPtr #-}


readByteOffPtr :: (MonadPrim s m, Prim a) => Ptr a -> Off Word8 -> m a
readByteOffPtr ptr (Off i) =
  case ptr `plusPtr` i of
    Ptr addr# -> prim (readOffAddr# addr# 0#)
{-# INLINE readByteOffPtr #-}

writeOffPtr :: (MonadPrim s m, Prim a) => Ptr a -> Off a -> a -> m ()
writeOffPtr (Ptr addr#) (Off (I# i#)) a = prim_ (writeOffAddr# addr# i# a)
{-# INLINE writeOffPtr #-}

writeByteOffPtr :: (MonadPrim s m, Prim a) => Ptr a -> Off Word8 -> a -> m ()
writeByteOffPtr ptr (Off i) a =
  case ptr `plusPtr` i of
    Ptr addr# -> prim_ (writeOffAddr# addr# 0# a)
{-# INLINE writeByteOffPtr #-}

readPtr :: (MonadPrim s m, Prim a) => Ptr a -> m a
readPtr (Ptr addr#) = prim (readOffAddr# addr# 0#)
{-# INLINE readPtr #-}

writePtr :: (MonadPrim s m, Prim a) => Ptr a -> a -> m ()
writePtr (Ptr addr#) a = prim_ (writeOffAddr# addr# 0# a)
{-# INLINE writePtr #-}

plusPtrOff :: Prim a => Ptr a -> Off a -> Ptr a
plusPtrOff (Ptr addr#) off = Ptr (addr# `plusAddr#` fromOff# off)
{-# INLINE plusPtrOff #-}

copyPtrToPtr :: (MonadPrim s m, Prim a) => Ptr a -> Off a -> Ptr a -> Off a -> Count a -> m ()
copyPtrToPtr srcPtr srcOff dstPtr dstOff c =
  unsafeIOToPrim $
  copyBytes
    (dstPtr `plusPtrOff` dstOff)
    (srcPtr `plusPtrOff` srcOff)
    (fromCount c)
{-# INLINE copyPtrToPtr #-}

movePtrToPtr :: (MonadPrim s m, Prim a) => Ptr a -> Off a -> Ptr a -> Off a -> Count a -> m ()
movePtrToPtr (Ptr srcAddr#) srcOff (Ptr dstAddr#) dstOff c =
  unsafeIOToPrim $
  memmoveAddr#
    srcAddr#
    (fromOff# srcOff)
    dstAddr#
    (fromOff# dstOff)
    (fromCount# c)
{-# INLINE movePtrToPtr #-}

copyPtrToMBytes ::
     (MonadPrim s m, Prim a) => Ptr a -> Off a -> MBytes pd s -> Off a -> Count a -> m ()
copyPtrToMBytes srcPtr srcOff (MBytes dst#) dstOff c =
  let !(Ptr addr#) = srcPtr `plusPtrOff` srcOff
   in prim_ $ copyAddrToByteArray# addr# dst# (fromOff# dstOff) (fromCount# c)
{-# INLINE copyPtrToMBytes #-}

copyBytesToPtr :: (MonadPrim s m, Prim a) => Bytes p -> Off a -> Ptr a -> Off a -> Count a -> m ()
copyBytesToPtr (Bytes src#) srcOff dstPtr dstOff c =
  let !(Ptr addr#) = dstPtr `plusPtrOff` dstOff
  in prim_ (copyByteArrayToAddr# src# (fromOff# srcOff) addr# (fromCount# c))
{-# INLINE copyBytesToPtr #-}

copyMBytesToPtr :: (MonadPrim s m, Prim a) => MBytes p s -> Off a -> Ptr a -> Off a -> Count a -> m ()
copyMBytesToPtr (MBytes src#) srcOff dstPtr dstOff c =
  let !(Ptr addr#) = dstPtr `plusPtrOff` dstOff
   in prim_ $
      copyMutableByteArrayToAddr# src# (fromOff# srcOff) addr# (fromCount# c)
{-# INLINE copyMBytesToPtr #-}

movePtrToMBytes :: (MonadPrim s m, Prim a) => Ptr a -> Off a -> MBytes p s -> Off a -> Count a -> m ()
movePtrToMBytes (Ptr srcAddr#) srcOff (MBytes dst#) dstOff c =
  unsafeIOToPrim $
  memmoveMutableByteArrayFromAddr#
    srcAddr#
    (fromOff# srcOff)
    dst#
    (fromOff# dstOff)
    (fromCount# c)
{-# INLINE movePtrToMBytes #-}

moveMBytesToPtr :: (MonadPrim s m, Prim a) => MBytes p s -> Off a -> Ptr a -> Off a -> Count a -> m ()
moveMBytesToPtr (MBytes src#) srcOff (Ptr dstAddr#) dstOff c =
  unsafeIOToPrim $
  memmoveMutableByteArrayToAddr#
    src#
    (fromOff# srcOff)
    dstAddr#
    (fromOff# dstOff)
    (fromCount# c)
{-# INLINE moveMBytesToPtr #-}
