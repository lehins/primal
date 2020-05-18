{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
  , isSameBytes
  , isSamePinnedBytes
  , isPinnedBytes
  , isPinnedMBytes
  , allocMBytes
  , allocPinnedMBytes
  , allocAlignedMBytes
  , allocUnpinnedMBytes
  , callocAlignedMBytes
  , reallocMBytes
  , freezeMBytes
  , thawBytes
  , shrinkMBytes
  , resizeMBytes
  , indexOffBytes
  , indexByteOffBytes
  , compareByteOffBytes
  , byteCountBytes
  , countBytes
  , getCountMBytes
  , getByteCountMBytes
  , setMBytes
  , copyByteOffBytesToMBytes
  , moveByteOffMBytesToMBytes
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
  , fromForeignPtrBytes
  , byteStringConvertError
  ) where

import Control.DeepSeq
import Control.Prim.Monad
import Control.Prim.Monad.Unsafe
import Data.Prim
import Data.Prim.Class
import GHC.ForeignPtr
import Data.Typeable
import Foreign.Prim


-- | In Haskell there is a distinction between pinned or unpinned memory.
--
-- Pinned memory is such, when allocated, it is guaranteed not to move throughout the
-- lifetime of a program. In other words the address pointer that refers to allocated
-- bytes will not change until it gets garbage collected because it is no longer
-- referenced by anything. Unpinned memory on the other hand can be moved around during
-- GC, which helps to reduce memory fragmentation.
--
-- Pinned/unpinnned choice during allocation is a bit of a lie, because when attempt is
-- made to allocate memory as unpinned, but requested size is a bit more than a certain
-- threashold (somewhere around 3KiB) it might still be allocated as pinned. Because of
-- that fact through out the "primal" universe there is a distinction between memory that
-- is either @`Pin`ned@ or @`Inc`onclusive@.
--
-- It is possible to use one of `Data.Prim.Memory.Bytes.toPinnedBytes` or
-- `Data.Prim.Memory.Bytes.toPinnedMBytes` to get a conclusive type.
--
-- @since 0.1.0
data Pinned = Pin | Inc

-- | An immutable region of memory which was allocated either as pinned or unpinned.
--
-- Constructor is not exported for safety. Violating type level `Pinned` kind is very
-- dangerous. Type safe constructor `Data.Prim.Memory.Bytes.fromByteArray#` and unwrapper
-- `Data.Prim.Memory.Bytes.toByteArray#` should be used instead. As a backdoor, of course,
-- the actual constructor is available in "Data.Prim.Memory.Internal" module.
data Bytes (p :: Pinned) = Bytes ByteArray#
type role Bytes phantom

-- | Mutable region of memory which was allocated either as pinned or unpinned.
--
-- Constructor is not exported for safety. Violating type level `Pinned` kind is very
-- dangerous. Type safe constructor `Data.Prim.Memory.Bytes.fromMutableByteArray#` and unwrapper
-- `Data.Prim.Memory.Bytes.toMutableByteArray#` should be used instead. As a backdoor, of course,
-- the actual constructor is available in "Data.Prim.Memory.Internal" module.
data MBytes (p :: Pinned) s = MBytes (MutableByteArray# s)
type role MBytes phantom nominal


instance NFData (Bytes p) where
  rnf (Bytes _) = ()

instance NFData (MBytes p s) where
  rnf (MBytes _) = ()



---- Pure

compareByteOffBytes :: Prim e => Bytes p1 -> Off Word8 -> Bytes p2 -> Off Word8 -> Count e -> Ordering
compareByteOffBytes (Bytes b1#) (Off (I# off1#)) (Bytes b2#) (Off (I# off2#)) c =
  toOrdering# (compareByteArrays# b1# off1# b2# off2# (fromCount# c))
{-# INLINE compareByteOffBytes #-}

indexOffBytes :: Prim e => Bytes p -> Off e -> e
indexOffBytes (Bytes ba#) (Off (I# i#)) = indexByteArray# ba# i#
{-# INLINE indexOffBytes #-}

indexByteOffBytes :: Prim e => Bytes p -> Off Word8 -> e
indexByteOffBytes (Bytes ba#) (Off (I# i#)) = indexByteOffByteArray# ba# i#
{-# INLINE indexByteOffBytes #-}


---- Mutable


allocMBytes ::
     forall p e s m. (Typeable p, Prim e, MonadPrim s m)
  => Count e
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

allocUnpinnedMBytes :: (MonadPrim s m, Prim e) => Count e -> m (MBytes 'Inc s)
allocUnpinnedMBytes c =
  prim $ \s ->
    case newByteArray# (fromCount# c) s of
      (# s', ba# #) -> (# s', MBytes ba# #)
{-# INLINE allocUnpinnedMBytes #-}


allocPinnedMBytes :: (MonadPrim s m, Prim e) => Count e -> m (MBytes 'Pin s)
allocPinnedMBytes c =
  prim $ \s ->
    case newPinnedByteArray# (fromCount# c) s of
      (# s', ba# #) -> (# s', MBytes ba# #)
{-# INLINE allocPinnedMBytes #-}

allocAlignedMBytes ::
     forall e m s. (MonadPrim s m, Prim e)
  => Count e -- ^ Size in number of bytes
  -> m (MBytes 'Pin s)
allocAlignedMBytes c =
  prim $ \s ->
    case newAlignedPinnedByteArray#
           (fromCount# c)
           (alignment# (proxy# :: Proxy# e))
           s of
      (# s', ba# #) -> (# s', MBytes ba# #)
{-# INLINE allocAlignedMBytes #-}

callocAlignedMBytes ::
     (MonadPrim s m, Prim e)
  => Count e -- ^ Size in number of bytes
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

copyByteOffBytesToMBytes ::
     (MonadPrim s m, Prim e) => Bytes ps -> Off Word8 -> MBytes pd s -> Off Word8 -> Count e -> m ()
copyByteOffBytesToMBytes (Bytes src#) (Off (I# srcOff#)) (MBytes dst#) (Off (I# dstOff#)) c =
  prim_ $ copyByteArray# src# srcOff# dst# dstOff# (fromCount# c)
{-# INLINE copyByteOffBytesToMBytes #-}

moveByteOffMBytesToMBytes ::
     (MonadPrim s m, Prim e) => MBytes ps s-> Off Word8 -> MBytes pd s -> Off Word8 -> Count e -> m ()
moveByteOffMBytesToMBytes (MBytes src#) (Off (I# srcOff#)) (MBytes dst#) (Off (I# dstOff#)) c =
  prim_ (copyMutableByteArray# src# srcOff# dst# dstOff# (fromCount# c))
{-# INLINE moveByteOffMBytesToMBytes #-}


byteCountBytes :: Bytes p -> Count Word8
byteCountBytes (Bytes ba#) = coerce (I# (sizeofByteArray# ba#))
{-# INLINE byteCountBytes #-}


-- | Shrink mutable bytes to new specified count of elements. The new count must be less
-- than or equal to the current count as reported by `getCountMBytes`.
shrinkMBytes :: (MonadPrim s m) => MBytes p s -> Count Word8 -> m ()
shrinkMBytes (MBytes mb#) (Count (I# c#)) = prim_ (shrinkMutableByteArray# mb# c#)
{-# INLINE shrinkMBytes #-}


-- | Attempt to resize mutable bytes in place.
--
-- * New bytes might be allocated, with the copy of an old one.
-- * Old references should not be kept around to allow GC to claim it
-- * Old references should not be used to avoid undefined behavior
resizeMBytes ::
     (MonadPrim s m, Prim e) => MBytes p s -> Count e -> m (MBytes 'Inc s)
resizeMBytes (MBytes mb#) c =
  prim $ \s ->
    case resizeMutableByteArray# mb# (fromCount# c) s of
      (# s', mb'# #) -> (# s', MBytes mb'# #)
{-# INLINE resizeMBytes #-}

reallocMBytes ::
     forall e p m s. (MonadPrim s m, Typeable p,  Prim e)
  => MBytes p s
  -> Count e
  -> m (MBytes p s)
reallocMBytes mb c = do
  oldByteCount <- getByteCountMBytes mb
  let newByteCount = toByteCount c
  if newByteCount <= oldByteCount
    then mb <$ when (newByteCount < oldByteCount) (shrinkMBytes mb newByteCount)
    else case eqT :: Maybe (p :~: 'Pin) of
           Just Refl -> do
             b <- freezeMBytes mb
             mb' <- allocPinnedMBytes newByteCount
             mb' <$ copyByteOffBytesToMBytes b 0 mb' 0 oldByteCount
           Nothing -> coerce <$> resizeMBytes mb newByteCount
{-# INLINABLE reallocMBytes #-}


-- | How many elements of type @a@ fits into bytes completely. In order to get a possible
-- count of leftover bytes use `countRemBytes`
countBytes :: Prim e => Bytes p -> Count e
countBytes = fromByteCount . byteCountBytes
{-# INLINE countBytes #-}

-- | How many elements of type @a@ fits into bytes completely. In order to get any number
-- of leftover bytes use `countRemBytes`
getCountMBytes :: (MonadPrim s m, Prim e) => MBytes p s -> m (Count e)
getCountMBytes b = fromByteCount <$> getByteCountMBytes b
{-# INLINE getCountMBytes #-}

readOffMBytes :: (MonadPrim s m, Prim e) => MBytes p s -> Off e -> m e
readOffMBytes (MBytes mba#) (Off (I# i#)) = prim (readMutableByteArray# mba# i#)
{-# INLINE readOffMBytes #-}

readByteOffMBytes :: (MonadPrim s m, Prim e) => MBytes p s -> Off Word8 -> m e
readByteOffMBytes (MBytes mba#) (Off (I# i#)) = prim (readByteOffMutableByteArray# mba# i#)
{-# INLINE readByteOffMBytes #-}

writeOffMBytes :: (MonadPrim s m, Prim e) => MBytes p s -> Off e -> e -> m ()
writeOffMBytes (MBytes mba#) (Off (I# i#)) a = prim_ (writeMutableByteArray# mba# i# a)
{-# INLINE writeOffMBytes #-}

writeByteOffMBytes :: (MonadPrim s m, Prim e) => MBytes p s -> Off Word8 -> e -> m ()
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
     (MonadPrim s m, Prim e)
  => MBytes p s -- ^ Chunk of memory to fill
  -> Off e -- ^ Offset in number of elements
  -> Count e -- ^ Number of cells to fill
  -> e -- ^ A value to fill the cells with
  -> m ()
setMBytes (MBytes mba#) (Off (I# o#)) (Count (I# n#)) a = prim_ (setMutableByteArray# mba# o# n# a)
{-# INLINE setMBytes #-}


toPtrBytes :: Bytes 'Pin -> Ptr e
toPtrBytes (Bytes ba#) = Ptr (byteArrayContents# ba#)
{-# INLINE toPtrBytes #-}

toPtrMBytes :: MBytes 'Pin s -> Ptr e
toPtrMBytes (MBytes mba#) = Ptr (mutableByteArrayContents# mba#)
{-# INLINE toPtrMBytes #-}

-- | Pointer access to immutable `Bytes` should be for read only purposes, but it is
-- not enforced. Any mutation will break referential transparency
withPtrBytes :: MonadPrim s m => Bytes 'Pin -> (Ptr e -> m b) -> m b
withPtrBytes b f = do
  res <- f (toPtrBytes b)
  res <$ touch b
{-# INLINE withPtrBytes #-}

-- | Same as `withPtrBytes`, but is suitable for actions that don't terminate
withNoHaltPtrBytes :: MonadUnliftPrim s m => Bytes 'Pin -> (Ptr e -> m b) -> m b
withNoHaltPtrBytes b f = withUnliftPrim b $ f (toPtrBytes b)
{-# INLINE withNoHaltPtrBytes #-}

withPtrMBytes :: MonadPrim s m => MBytes 'Pin s -> (Ptr e -> m b) -> m b
withPtrMBytes mb f = do
  res <- f (toPtrMBytes mb)
  res <$ touch mb
{-# INLINE withPtrMBytes #-}

withNoHaltPtrMBytes :: MonadUnliftPrim s m => MBytes 'Pin s -> (Ptr e -> m b) -> m b
withNoHaltPtrMBytes mb f = withUnliftPrim mb $ f (toPtrMBytes mb)
{-# INLINE withNoHaltPtrMBytes #-}

toForeignPtrBytes :: Bytes 'Pin -> ForeignPtr e
toForeignPtrBytes (Bytes ba#) =
  ForeignPtr (byteArrayContents# ba#) (PlainPtr (unsafeCoerce# ba#))
{-# INLINE toForeignPtrBytes #-}


toForeignPtrMBytes :: MBytes 'Pin s -> ForeignPtr e
toForeignPtrMBytes (MBytes mba#) =
  ForeignPtr (byteArrayContents# (unsafeCoerce# mba#)) (PlainPtr (unsafeCoerce# mba#))
{-# INLINE toForeignPtrMBytes #-}


-- | Discarding the `ForeignPtr` will trigger all if there are any associated
-- Haskell finalizers.
fromForeignPtrBytes :: ForeignPtr e -> Either String (Bytes 'Pin)
fromForeignPtrBytes (ForeignPtr addr# content) =
  case content of
    PlainPtr mbaRW# -> checkConvert mbaRW#
    MallocPtr mbaRW# _ -> checkConvert mbaRW#
    _ -> Left "Cannot convert a C allocated pointer"
  where
    checkConvert mba# =
      let !b@(Bytes ba#) = unsafePerformIO (freezeMBytes (MBytes mba#))
       in if isTrue# (byteArrayContents# ba# `eqAddr#` addr#)
            then Right b
            else Left
                   "ForeignPtr does not point to the beginning of the associated MutableByteArray#"
{-# INLINE fromForeignPtrBytes #-}


-- | Check if two byte arrays refer to pinned memory and compare their pointers.
isSameBytes :: Bytes p1 -> Bytes p2 -> Bool
isSameBytes (Bytes b1#) (Bytes b2#) = isTrue# (isSameByteArray# b1# b2#)
{-# INLINE[0] isSameBytes #-}
{-# RULES
"isSamePinnedBytes" isSameBytes = isSamePinnedBytes
  #-}

-- | Perform pointer equality on pinned `Bytes`.
isSamePinnedBytes :: Bytes 'Pin -> Bytes 'Pin -> Bool
isSamePinnedBytes pb1 pb2 = toPtrBytes pb1 == toPtrBytes pb2
{-# INLINE isSamePinnedBytes #-}



byteStringConvertError :: String -> a
byteStringConvertError msg = error $ "Cannot convert 'ByteString'. " ++ msg
{-# NOINLINE byteStringConvertError #-}

