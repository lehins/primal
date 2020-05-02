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
-- Module      : Data.Prim.Bytes.Ptr
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Bytes.Ptr
  ( module Foreign.Prim.Ptr

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
import Foreign.Prim
import Foreign.Prim.Ptr



copyPtrToMBytes ::
     (MonadPrim s m, Prim a) => Ptr a -> Off a -> MBytes pd s -> Off a -> Count a -> m ()
copyPtrToMBytes srcPtr srcOff (MBytes dst#) dstOff c =
  let !(Ptr addr#) = srcPtr `plusOffPtr` srcOff
   in prim_ $ copyAddrToByteArray# addr# dst# (fromOff# dstOff) (fromCount# c)
{-# INLINE copyPtrToMBytes #-}

copyBytesToPtr :: (MonadPrim s m, Prim a) => Bytes p -> Off a -> Ptr a -> Off a -> Count a -> m ()
copyBytesToPtr (Bytes src#) srcOff dstPtr dstOff c =
  let !(Ptr addr#) = dstPtr `plusOffPtr` dstOff
  in prim_ (copyByteArrayToAddr# src# (fromOff# srcOff) addr# (fromCount# c))
{-# INLINE copyBytesToPtr #-}

copyMBytesToPtr :: (MonadPrim s m, Prim a) => MBytes p s -> Off a -> Ptr a -> Off a -> Count a -> m ()
copyMBytesToPtr (MBytes src#) srcOff dstPtr dstOff c =
  let !(Ptr addr#) = dstPtr `plusOffPtr` dstOff
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
