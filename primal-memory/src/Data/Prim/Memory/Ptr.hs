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
-- Module      : Data.Prim.Memory.Ptr
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Memory.Ptr
  ( module Foreign.Prim.Ptr

  , copyPtrToMBytes
  , movePtrToMBytes
  , copyBytesToPtr
  , copyMBytesToPtr
  , moveMBytesToPtr
  , copyByteOffPtrToMBytes
  , moveByteOffPtrToMBytes
  , copyByteOffBytesToPtr
  , copyByteOffMBytesToPtr
  , moveByteOffMBytesToPtr
  , compareByteOffBytesToPtr
  , compareByteOffPtrToBytes
  , module Data.Prim
  ) where


import Control.Prim.Monad
import Control.Prim.Monad.Unsafe
import Data.Prim
import Data.Prim.Memory.Bytes.Internal (Bytes(..), MBytes(..))
import Data.Prim.Class
import Foreign.Prim
import Foreign.Prim.Ptr



copyPtrToMBytes ::
     (MonadPrim s m, Prim e) => Ptr e -> Off e -> MBytes p s -> Off e -> Count e -> m ()
copyPtrToMBytes src srcOff dst dstOff =
  copyByteOffPtrToMBytes src (toByteOff srcOff) dst (toByteOff dstOff)
{-# INLINE copyPtrToMBytes #-}



copyByteOffPtrToMBytes ::
     (MonadPrim s m, Prim e) => Ptr e -> Off Word8 -> MBytes p s -> Off Word8 -> Count e -> m ()
copyByteOffPtrToMBytes (Ptr srcAddr#) (Off (I# srcOff#)) (MBytes dst#) (Off (I# dstOff#)) c =
  prim_ $ copyAddrToByteArray# (srcAddr# `plusAddr#` srcOff#) dst# dstOff# (unCountBytes# c)
{-# INLINE copyByteOffPtrToMBytes #-}


copyBytesToPtr :: (MonadPrim s m, Prim e) => Bytes p -> Off e -> Ptr e -> Off e -> Count e -> m ()
copyBytesToPtr src srcOff dst dstOff =
  copyByteOffBytesToPtr src (toByteOff srcOff) dst (toByteOff dstOff)
{-# INLINE copyBytesToPtr #-}


copyByteOffBytesToPtr ::
     (MonadPrim s m, Prim e)
  => Bytes p
  -> Off Word8
  -> Ptr e
  -> Off Word8
  -> Count e
  -> m ()
copyByteOffBytesToPtr (Bytes src#) (Off (I# srcOff#)) (Ptr dstAddr#) (Off (I# dstOff#)) c =
  prim_ $
  copyByteArrayToAddr#
    src#
    srcOff#
    (dstAddr# `plusAddr#` dstOff#)
    (unCountBytes# c)
{-# INLINE copyByteOffBytesToPtr #-}


copyMBytesToPtr :: (MonadPrim s m, Prim e) => MBytes p s -> Off e -> Ptr e -> Off e -> Count e -> m ()
copyMBytesToPtr src srcOff dst dstOff =
  copyByteOffMBytesToPtr src (toByteOff srcOff) dst (toByteOff dstOff)
{-# INLINE copyMBytesToPtr #-}


copyByteOffMBytesToPtr ::
     (MonadPrim s m, Prim e)
  => MBytes p s
  -> Off Word8
  -> Ptr e
  -> Off Word8
  -> Count e
  -> m ()
copyByteOffMBytesToPtr (MBytes src#) (Off (I# srcOff#)) (Ptr dstAddr#) (Off (I# dstOff#)) c =
  prim_ $
  copyMutableByteArrayToAddr#
    src#
    srcOff#
    (dstAddr# `plusAddr#` dstOff#)
    (unCountBytes# c)
{-# INLINE copyByteOffMBytesToPtr #-}


movePtrToMBytes :: (MonadPrim s m, Prim e) => Ptr e -> Off e -> MBytes p s -> Off e -> Count e -> m ()
movePtrToMBytes src srcOff dst dstOff =
  moveByteOffPtrToMBytes src (toByteOff srcOff) dst (toByteOff dstOff)
{-# INLINE movePtrToMBytes #-}

moveByteOffPtrToMBytes ::
     (MonadPrim s m, Prim e)
  => Ptr e
  -> Off Word8
  -> MBytes p s
  -> Off Word8
  -> Count e
  -> m ()
moveByteOffPtrToMBytes (Ptr srcAddr#) (Off (I# srcOff#)) (MBytes dst#) (Off (I# dstOff#)) c =
  unsafeIOToPrim $
  memmoveMutableByteArrayFromAddr# srcAddr# srcOff# dst# dstOff# (unCountBytes# c)
{-# INLINE moveByteOffPtrToMBytes #-}

moveMBytesToPtr :: (MonadPrim s m, Prim e) => MBytes p s -> Off e -> Ptr e -> Off e -> Count e -> m ()
moveMBytesToPtr src srcOff dst dstOff =
  moveByteOffMBytesToPtr src (toByteOff srcOff) dst (toByteOff dstOff)
{-# INLINE moveMBytesToPtr #-}


moveByteOffMBytesToPtr ::
  (MonadPrim s m, Prim e) => MBytes p s -> Off Word8 -> Ptr e -> Off Word8 -> Count e -> m ()
moveByteOffMBytesToPtr (MBytes src#) (Off (I# srcOff#)) (Ptr dstAddr#) (Off (I# dstOff#)) c =
  unsafeIOToPrim $
  memmoveMutableByteArrayToAddr# src# srcOff# dstAddr# dstOff# (unCountBytes# c)
{-# INLINE moveByteOffMBytesToPtr #-}


compareByteOffBytesToPtr ::
     Prim e => Bytes p -> Off Word8 -> Ptr e -> Off Word8 -> Count e -> Ordering
compareByteOffBytesToPtr (Bytes b#) (Off (I# off1#)) (Ptr addr#) (Off (I# off2#)) c =
  toOrdering# (memcmpByteArrayAddr# b# off1# addr# off2# (unCountBytes# c))
{-# INLINE compareByteOffBytesToPtr #-}

compareByteOffPtrToBytes ::
     Prim e => Ptr e -> Off Word8 -> Bytes p -> Off Word8 -> Count e -> Ordering
compareByteOffPtrToBytes (Ptr addr#) (Off (I# off1#)) (Bytes b#) (Off (I# off2#)) c =
  toOrdering# (memcmpAddrByteArray# addr# off1# b# off2# (unCountBytes# c))
{-# INLINE compareByteOffPtrToBytes #-}
