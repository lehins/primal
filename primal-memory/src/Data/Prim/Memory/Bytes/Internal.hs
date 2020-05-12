{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Data.Prim.Memory.Bytes.Internal
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Memory.Bytes.Internal
  ( Bytes(..)
  , MBytes(..)
  , Pinned(..)
  , allocMBytes
  , allocUnpinnedMBytes
  , allocPinnedMBytes
  , allocAlignedMBytes
  , callocAlignedMBytes
  , freezeMBytes
  , thawBytes
  , indexOffBytes
  , indexByteOffBytes
  , compareByteOffBytes
  , byteCountBytes
  , countBytes
  , getCountMBytes
  , getByteCountMBytes
  , setMBytes
  , moveMBytesToMBytes
  , copyBytesToMBytes
  , readOffMBytes
  , readByteOffMBytes
  , writeOffMBytes
  , writeByteOffMBytes
  , toPtrBytes
  , toPtrMBytes
  , withPtrBytes
  , withPtrMBytes
  , withNoHaltPtrBytes
  , withNoHaltPtrMBytes
  , toForeignPtrBytes
  , toForeignPtrMBytes
  ) where

import Control.DeepSeq
import Control.Prim.Monad
import Data.Prim
import Data.Prim.Class
import Data.Proxy
import GHC.ForeignPtr
import Data.Typeable
import Foreign.Prim


-- | Memory can either be `Pin`ned or `Inc`onclusive. Use eith `toPinnedBytes` or
-- `toPinnedMBytes` to get a conclusive answer for the latter case.
data Pinned = Pin | Inc

data Bytes (p :: Pinned) = Bytes ByteArray#
type role Bytes phantom

data MBytes (p :: Pinned) s = MBytes (MutableByteArray# s)
type role MBytes phantom nominal


instance NFData (Bytes p) where
  rnf (Bytes _) = ()

instance NFData (MBytes p s) where
  rnf (MBytes _) = ()



---- Pure

compareByteOffBytes :: Prim a => Bytes p1 -> Off Word8 -> Bytes p2 -> Off Word8 -> Count a -> Ordering
compareByteOffBytes (Bytes b1#) (Off (I# off1#)) (Bytes b2#) (Off (I# off2#)) c =
  toOrdering# (compareByteArrays# b1# off1# b2# off2# (fromCount# c))
{-# INLINE compareByteOffBytes #-}

indexOffBytes :: Prim a => Bytes p -> Off a -> a
indexOffBytes (Bytes ba#) (Off (I# i#)) = indexByteArray# ba# i#
{-# INLINE indexOffBytes #-}

indexByteOffBytes :: Prim a => Bytes p -> Off Word8 -> a
indexByteOffBytes (Bytes ba#) (Off (I# i#)) = indexByteOffByteArray# ba# i#
{-# INLINE indexByteOffBytes #-}


---- Mutable


allocMBytes ::
     forall p a s m. (Typeable p, Prim a, MonadPrim s m)
  => Count a
  -> m (MBytes p s)
allocMBytes c =
  case eqT :: Maybe (p :~: 'Pin) of
    Just Refl -> allocPinnedMBytes c
    _ ->
      case eqT :: Maybe (p :~: 'Inc) of
        Just Refl -> allocUnpinnedMBytes c
        Nothing ->
          errorImpossible
            "allocMBytes"
            $ "Unexpected 'Pinned' kind: '" ++ showsType (Proxy :: Proxy (Bytes p)) "'."
{-# INLINE[0] allocMBytes #-}
{-# RULES
"allocUnpinnedMBytes" allocMBytes = allocUnpinnedMBytes
"allocPinnedMBytes" allocMBytes = allocPinnedMBytes
  #-}

allocUnpinnedMBytes :: (MonadPrim s m, Prim a) => Count a -> m (MBytes 'Inc s)
allocUnpinnedMBytes c =
  prim $ \s ->
    case newByteArray# (fromCount# c) s of
      (# s', ba# #) -> (# s', MBytes ba# #)
{-# INLINE allocUnpinnedMBytes #-}


allocPinnedMBytes :: (MonadPrim s m, Prim a) => Count a -> m (MBytes 'Pin s)
allocPinnedMBytes c =
  prim $ \s ->
    case newPinnedByteArray# (fromCount# c) s of
      (# s', ba# #) -> (# s', MBytes ba# #)
{-# INLINE allocPinnedMBytes #-}

allocAlignedMBytes ::
     (MonadPrim s m, Prim a)
  => Count a -- ^ Size in number of bytes
  -> m (MBytes 'Pin s)
allocAlignedMBytes c =
  prim $ \s ->
    case alignmentProxy c of
      Count (I# a#) ->
        case newAlignedPinnedByteArray# (fromCount# c) a# s of
          (# s', ba# #) -> (# s', MBytes ba# #)
{-# INLINE allocAlignedMBytes #-}

callocAlignedMBytes ::
     (MonadPrim s m, Prim a)
  => Count a -- ^ Size in number of bytes
  -> m (MBytes 'Pin s)
callocAlignedMBytes n = allocAlignedMBytes n >>= \mb -> mb <$ setMBytes mb 0 (toByteCount n) 0
{-# INLINE callocAlignedMBytes #-}


getByteCountMBytes :: MonadPrim s m => MBytes p s -> m (Count Word8)
getByteCountMBytes (MBytes mba#) =
  prim $ \s ->
    case getSizeofMutableByteArray# mba# s of
      (# s', n# #) -> (# s', Count (I# n#) #)
{-# INLINE getByteCountMBytes #-}

freezeMBytes :: MonadPrim s m => MBytes p s -> m (Bytes p)
freezeMBytes (MBytes mba#) =
  prim $ \s ->
    case unsafeFreezeByteArray# mba# s of
      (# s', ba# #) -> (# s', Bytes ba# #)
{-# INLINE freezeMBytes #-}

thawBytes :: MonadPrim s m => Bytes p -> m (MBytes p s)
thawBytes (Bytes ba#) =
  prim $ \s ->
    case unsafeThawByteArray# ba# s of
      (# s', mba# #) -> (# s', MBytes mba# #)
{-# INLINE thawBytes #-}


copyBytesToMBytes ::
     (MonadPrim s m, Prim a) => Bytes ps -> Off a -> MBytes pd s -> Off a -> Count a -> m ()
copyBytesToMBytes (Bytes src#) srcOff (MBytes dst#) dstOff c =
  prim_ $
  copyByteArray# src# (fromOff# srcOff) dst# (fromOff# dstOff) (fromCount# c)
{-# INLINE copyBytesToMBytes #-}

moveMBytesToMBytes ::
     (MonadPrim s m, Prim a) => MBytes ps s-> Off a -> MBytes pd s -> Off a -> Count a -> m ()
moveMBytesToMBytes (MBytes src#) srcOff (MBytes dst#) dstOff c =
  prim_ (copyMutableByteArray# src# (fromOff# srcOff) dst# (fromOff# dstOff) (fromCount# c))
{-# INLINE moveMBytesToMBytes #-}


byteCountBytes :: Bytes p -> Count Word8
byteCountBytes (Bytes ba#) = coerce (I# (sizeofByteArray# ba#))
{-# INLINE byteCountBytes #-}

-- | How many elements of type @a@ fits into bytes completely. In order to get a possible
-- count of leftover bytes use `countRemBytes`
countBytes :: Prim a => Bytes p -> Count a
countBytes = fromByteCount . byteCountBytes
{-# INLINE countBytes #-}

-- | How many elements of type @a@ fits into bytes completely. In order to get any number
-- of leftover bytes use `countRemBytes`
getCountMBytes :: (MonadPrim s m, Prim a) => MBytes p s -> m (Count a)
getCountMBytes b = fromByteCount <$> getByteCountMBytes b
{-# INLINE getCountMBytes #-}

readOffMBytes :: (MonadPrim s m, Prim a) => MBytes p s -> Off a -> m a
readOffMBytes (MBytes mba#) (Off (I# i#)) = prim (readMutableByteArray# mba# i#)
{-# INLINE readOffMBytes #-}

readByteOffMBytes :: (MonadPrim s m, Prim a) => MBytes p s -> Off Word8 -> m a
readByteOffMBytes (MBytes mba#) (Off (I# i#)) = prim (readByteOffMutableByteArray# mba# i#)
{-# INLINE readByteOffMBytes #-}

writeOffMBytes :: (MonadPrim s m, Prim a) => MBytes p s -> Off a -> a -> m ()
writeOffMBytes (MBytes mba#) (Off (I# i#)) a = prim_ (writeMutableByteArray# mba# i# a)
{-# INLINE writeOffMBytes #-}

writeByteOffMBytes :: (MonadPrim s m, Prim a) => MBytes p s -> Off Word8 -> a -> m ()
writeByteOffMBytes (MBytes mba#) (Off (I# i#)) a = prim_ (writeByteOffMutableByteArray# mba# i# a)
{-# INLINE writeByteOffMBytes #-}

isPinnedBytes :: Bytes p -> Bool
isPinnedBytes (Bytes b#) = isTrue# (isByteArrayPinned# b#)
{-# INLINE[0] isPinnedBytes #-}

isPinnedMBytes :: MBytes p d -> Bool
isPinnedMBytes (MBytes mb#) = isTrue# (isMutableByteArrayPinned# mb#)
{-# INLINE[0] isPinnedMBytes #-}

{-# RULES
"isPinnedBytes" forall (x :: Bytes 'Pin) . isPinnedBytes x = True
"isPinnedMBytes" forall (x :: MBytes 'Pin s) . isPinnedMBytes x = True
  #-}



setMBytes ::
     (MonadPrim s m, Prim a)
  => MBytes p s -- ^ Chunk of memory to fill
  -> Off a -- ^ Offset in number of elements
  -> Count a -- ^ Number of cells to fill
  -> a -- ^ A value to fill the cells with
  -> m ()
setMBytes (MBytes mba#) (Off (I# o#)) (Count (I# n#)) a = prim_ (setMutableByteArray# mba# o# n# a)
{-# INLINE setMBytes #-}


toPtrBytes :: Bytes 'Pin -> Ptr a
toPtrBytes (Bytes ba#) = Ptr (byteArrayContents# ba#)
{-# INLINE toPtrBytes #-}

toPtrMBytes :: MBytes 'Pin s -> Ptr a
toPtrMBytes (MBytes mba#) = Ptr (byteArrayContents# (unsafeCoerce# mba#))
{-# INLINE toPtrMBytes #-}

-- | Pointer access to immutable `Bytes` should be for read only purposes, but it is
-- not enforced. Any mutation will break referential transparency
withPtrBytes :: MonadPrim s m => Bytes 'Pin -> (Ptr a -> m b) -> m b
withPtrBytes b f = do
  res <- f (toPtrBytes b)
  res <$ touch b
{-# INLINE withPtrBytes #-}

-- | Same as `withPtrBytes`, but is suitable for actions that don't terminate
withNoHaltPtrBytes :: MonadUnliftPrim s m => Bytes 'Pin -> (Ptr a -> m b) -> m b
withNoHaltPtrBytes b f = withUnliftPrim b $ f (toPtrBytes b)
{-# INLINE withNoHaltPtrBytes #-}

withPtrMBytes :: MonadPrim s m => MBytes 'Pin s -> (Ptr a -> m b) -> m b
withPtrMBytes mb f = do
  res <- f (toPtrMBytes mb)
  res <$ touch mb
{-# INLINE withPtrMBytes #-}

withNoHaltPtrMBytes :: (MonadUnliftPrim s m) => MBytes 'Pin s -> (Ptr a -> m b) -> m b
withNoHaltPtrMBytes mb f = withUnliftPrim mb $ f (toPtrMBytes mb)
{-# INLINE withNoHaltPtrMBytes #-}

toForeignPtrBytes :: Bytes 'Pin -> ForeignPtr a
toForeignPtrBytes (Bytes ba#) =
  ForeignPtr (byteArrayContents# ba#) (PlainPtr (unsafeCoerce# ba#))
{-# INLINE toForeignPtrBytes #-}


toForeignPtrMBytes :: MBytes 'Pin s -> ForeignPtr a
toForeignPtrMBytes (MBytes mba#) =
  ForeignPtr (byteArrayContents# (unsafeCoerce# mba#)) (PlainPtr (unsafeCoerce# mba#))
{-# INLINE toForeignPtrMBytes #-}


