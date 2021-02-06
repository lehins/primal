{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Primal.Memory.Bytes.Internal
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Memory.Bytes.Internal
  ( Bytes(..)
  , MBytes(..)
  , Pinned(..)
  , toByteArray#
  , fromByteArray#
  , toMutableByteArray#
  , fromMutableByteArray#
  , isSameBytes
  , isSamePinnedBytes
  , isSameMBytes
  , isPinnedBytes
  , isPinnedMBytes
  , castStateMBytes
  , castPinnedBytes
  , castPinnedMBytes
  , relaxPinnedBytes
  , relaxPinnedMBytes
  , toInconclusiveBytes
  , toInconclusiveMBytes
  , allocMBytes
  , allocPinnedMBytes
  , allocAlignedMBytes
  , allocUnpinnedMBytes
  , allocZeroPinnedMBytes
  , allocZeroAlignedMBytes
  , reallocMBytes
  , freezeMBytes
  , thawBytes
  , shrinkMBytes
  , resizeMBytes
  , indexOffBytes
  , indexByteOffBytes
  , compareByteOffBytes
  , compareByteOffMBytes
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
  , toUArrayBytes
  , fromUArrayBytes
  , toUMArrayMBytes
  , fromUMArrayMBytes
  , toPtrBytes
  , toPtrMBytes
  , withPtrBytes
  , withPtrMBytes
  , withNoHaltPtrBytes
  , withNoHaltPtrMBytes
  , toForeignPtrBytes
  , toForeignPtrMBytes
  , castForeignPtrToBytes
  , onForeignPtrContents
  , byteStringConvertError
  ) where

import Control.DeepSeq
import Data.Typeable
import GHC.ForeignPtr
import Primal.Array.Unboxed
import Primal.Eval
import Primal.Foreign
import Primal.Monad
import Primal.Mutable.Eq
import Primal.Mutable.Ord
import Primal.Mutable.Freeze
import Primal.Monad.Unsafe
import Primal.Unbox
import Primal.Unbox.Class
import Unsafe.Coerce
#if MIN_VERSION_base(4,14,0)
import Data.IORef
#endif

-- | In GHC there is a distinction between pinned and unpinned memory.
--
-- Pinned memory is such that when allocated, it is guaranteed not to move throughout the
-- lifetime of a program. In other words the address pointer that refers to allocated
-- bytes will not change until the associated `ByteArray#` or `MutableByteArray#` is no
-- longer referenced anywhere in the program at which point it gets garbage collected. On
-- the other hand unpinned memory can be moved around during GC, which helps to reduce
-- memory fragmentation.
--
-- Pinned/unpinnned choice during allocation is a bit of a lie, because when attempt is
-- made to allocate memory as unpinned, but requested size is a bit more than a certain
-- threshold (somewhere around 3KiB) it might still be allocated as pinned. Because of
-- that fact through out the "primal" universe there is a distinction between memory that
-- is either @`Pin`ned@ or @`Inc`onclusive@.
--
-- It is possible to use one of `Primal.Memory.Bytes.toPinnedBytes` or
-- `Primal.Memory.Bytes.toPinnedMBytes` to get a conclusive type.
--
-- @since 0.1.0
data Pinned
  = Pin -- ^ Pinned, which indicates that allocated memory will not move
  | Inc -- ^ Inconclusive, thus memory could be pinned or unpinned

-- | An immutable region of memory which was allocated either as pinned or unpinned.
--
-- Constructor is not exported for safety. Violating type level `Pinned` kind is very
-- dangerous. Type safe constructor `Primal.Memory.Bytes.fromByteArray#` and unwrapper
-- `Primal.Memory.Bytes.toByteArray#` should be used instead. As a backdoor, of course,
-- the actual constructor is available from @Primal.Memory.Internal@
data Bytes (p :: Pinned) = Bytes ByteArray#
type role Bytes nominal

-- | Mutable region of memory which was allocated either as pinned or unpinned.
--
-- Constructor is not exported for safety. Violating type level `Pinned` kind is very
-- dangerous. Type safe constructor `Primal.Memory.Bytes.fromMutableByteArray#` and
-- unwrapper `Primal.Memory.Bytes.toMutableByteArray#` should be used instead. As a
-- backdoor, of course, the actual constructor is available in "Primal.Memory.Internal"
-- module and specially unsafe function `castPinnedMBytes` was crafted.
data MBytes (p :: Pinned) s = MBytes (MutableByteArray# s)
type role MBytes nominal nominal


instance NFData (Bytes p) where
  rnf (Bytes _) = ()

instance NFData (MBytes p s) where
  rnf (MBytes _) = ()

type instance Frozen (MBytes p) = Bytes p

instance Typeable p => MutFreeze (MBytes p) where
  thaw = thawBytes
  {-# INLINE thaw #-}
  thawClone bs = do
    let bytes = byteCountBytes bs
    mbs <- allocMBytes bytes
    mbs <$ copyByteOffBytesToMBytes bs 0 mbs 0 bytes
  {-# INLINE thawClone #-}
  freezeMut = freezeMBytes
  {-# INLINE freezeMut #-}

instance MutEq (MBytes p) where
  eqMut mbs1 mbs2 = (EQ ==) <$> compareMut mbs1 mbs2
  {-# INLINE eqMut #-}

instance MutOrd (MBytes p) where
  compareMut mbs1 mbs2
    | isSameMBytes mbs1 mbs2 = pure EQ
    | otherwise = do
      sz1 <- getByteCountMBytes mbs1
      sz2 <- getByteCountMBytes mbs2
      case compare sz1 sz2 of
        EQ -> compareByteOffMBytes mbs1 0 mbs2 0 sz1
        cmp -> pure cmp
  {-# INLINE compareMut #-}


-- | Unwrap `Bytes` to get the underlying `ByteArray#`.
--
-- @since 0.1.0
toByteArray# :: Bytes p -> ByteArray#
toByteArray# (Bytes b#) = b#

-- | Wrap `ByteArray#` into `Bytes`
--
-- @since 0.1.0
fromByteArray# :: ByteArray# -> Bytes 'Inc
fromByteArray# = Bytes

-- | Unwrap `MBytes` to get the underlying `MutableByteArray#`.
--
-- @since 0.1.0
toMutableByteArray# :: MBytes p s -> MutableByteArray# s
toMutableByteArray# (MBytes mb#) = mb#

-- | Wrap `MutableByteArray#` into `MBytes`
--
-- @since 0.1.0
fromMutableByteArray# :: MutableByteArray# s -> MBytes 'Inc s
fromMutableByteArray# = MBytes




---- Pure

compareByteOffBytes :: Unbox e => Bytes p1 -> Off Word8 -> Bytes p2 -> Off Word8 -> Count e -> Ordering
compareByteOffBytes (Bytes b1#) (Off (I# off1#)) (Bytes b2#) (Off (I# off2#)) c =
  toOrdering# (compareByteArrays# b1# off1# b2# off2# (unCountBytes# c))
{-# INLINE compareByteOffBytes #-}

compareByteOffMBytes ::
     (Unbox e, MonadPrim s m)
  => MBytes p1 s
  -> Off Word8
  -> MBytes p2 s
  -> Off Word8
  -> Count e
  -> m Ordering
compareByteOffMBytes (MBytes mb1#) (Off (I# off1#)) (MBytes mb2#) (Off (I# off2#)) c =
  toOrdering <$> unsafeIOToPrim (memcmpMutableByteArray# mb1# off1# mb2# off2# (unCountBytes# c))
{-# INLINE compareByteOffMBytes #-}

indexOffBytes :: Unbox e => Bytes p -> Off e -> e
indexOffBytes (Bytes ba#) (Off (I# i#)) = indexByteArray# ba# i#
{-# INLINE indexOffBytes #-}

indexByteOffBytes :: Unbox e => Bytes p -> Off Word8 -> e
indexByteOffBytes (Bytes ba#) (Off (I# i#)) = indexByteOffByteArray# ba# i#
{-# INLINE indexByteOffBytes #-}


---- Mutable


allocMBytes ::
     forall p e s m. (Typeable p, Unbox e, MonadPrim s m)
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

allocUnpinnedMBytes :: (MonadPrim s m, Unbox e) => Count e -> m (MBytes 'Inc s)
allocUnpinnedMBytes c =
  prim $ \s ->
    case newByteArray# (unCountBytes# c) s of
      (# s', ba# #) -> (# s', MBytes ba# #)
{-# INLINE allocUnpinnedMBytes #-}


allocPinnedMBytes :: (MonadPrim s m, Unbox e) => Count e -> m (MBytes 'Pin s)
allocPinnedMBytes c =
  prim $ \s ->
    case newPinnedByteArray# (unCountBytes# c) s of
      (# s', ba# #) -> (# s', MBytes ba# #)
{-# INLINE allocPinnedMBytes #-}

allocAlignedMBytes ::
     forall e m s. (MonadPrim s m, Unbox e)
  => Count e -- ^ Size in number of bytes
  -> m (MBytes 'Pin s)
allocAlignedMBytes c =
  prim $ \s ->
    case newAlignedPinnedByteArray#
           (unCountBytes# c)
           (alignment# (proxy# :: Proxy# e))
           s of
      (# s', ba# #) -> (# s', MBytes ba# #)
{-# INLINE allocAlignedMBytes #-}


-- @since 0.3.0
allocZeroPinnedMBytes ::
     (MonadPrim s m, Unbox e)
  => Count e -- ^ Size in number of bytes
  -> m (MBytes 'Pin s)
allocZeroPinnedMBytes n = allocPinnedMBytes n >>= \mb -> mb <$ setMBytes mb 0 (toByteCount n) 0
{-# INLINE allocZeroPinnedMBytes #-}

-- @since 0.3.0
allocZeroAlignedMBytes ::
     (MonadPrim s m, Unbox e)
  => Count e -- ^ Size in number of bytes
  -> m (MBytes 'Pin s)
allocZeroAlignedMBytes n = allocAlignedMBytes n >>= \mb -> mb <$ setMBytes mb 0 (toByteCount n) 0
{-# INLINE allocZeroAlignedMBytes #-}


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
     (MonadPrim s m, Unbox e) => Bytes ps -> Off Word8 -> MBytes pd s -> Off Word8 -> Count e -> m ()
copyByteOffBytesToMBytes (Bytes src#) (Off (I# srcOff#)) (MBytes dst#) (Off (I# dstOff#)) c =
  prim_ $ copyByteArray# src# srcOff# dst# dstOff# (unCountBytes# c)
{-# INLINE copyByteOffBytesToMBytes #-}

moveByteOffMBytesToMBytes ::
     (MonadPrim s m, Unbox e) => MBytes ps s-> Off Word8 -> MBytes pd s -> Off Word8 -> Count e -> m ()
moveByteOffMBytesToMBytes (MBytes src#) (Off (I# srcOff#)) (MBytes dst#) (Off (I# dstOff#)) c =
  prim_ (copyMutableByteArray# src# srcOff# dst# dstOff# (unCountBytes# c))
{-# INLINE moveByteOffMBytesToMBytes #-}


byteCountBytes :: Bytes p -> Count Word8
byteCountBytes (Bytes ba#) = coerce (I# (sizeofByteArray# ba#))
{-# INLINE byteCountBytes #-}


-- | Shrink mutable bytes to new specified count of elements. The new count must be less
-- than or equal to the current count as reported by `getCountMBytes`.
shrinkMBytes :: (MonadPrim s m, Unbox e) => MBytes p s -> Count e -> m ()
shrinkMBytes (MBytes mb#) c = prim_ (shrinkMutableByteArray# mb# (unCountBytes# c))
{-# INLINE shrinkMBytes #-}


-- | Attempt to resize mutable bytes in place.
--
-- * New bytes might be allocated, with the copy of an old one.
-- * Old references should not be kept around to allow GC to claim it
-- * Old references should not be used to avoid undefined behavior
resizeMBytes ::
     (MonadPrim s m, Unbox e) => MBytes p s -> Count e -> m (MBytes 'Inc s)
resizeMBytes (MBytes mb#) c =
  prim $ \s ->
    case resizeMutableByteArray# mb# (unCountBytes# c) s of
      (# s', mb'# #) -> (# s', MBytes mb'# #)
{-# INLINE resizeMBytes #-}

reallocMBytes ::
     forall e p m s. (MonadPrim s m, Typeable p,  Unbox e)
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
           Nothing -> castPinnedMBytes <$> resizeMBytes mb newByteCount
{-# INLINABLE reallocMBytes #-}

castStateMBytes :: MBytes p s' -> MBytes p s
castStateMBytes = unsafeCoerce

castPinnedBytes :: Bytes p' -> Bytes p
castPinnedBytes (Bytes b#) = Bytes b#

castPinnedMBytes :: MBytes p' s -> MBytes p s
castPinnedMBytes (MBytes b#) = MBytes b#


relaxPinnedBytes :: Bytes 'Pin -> Bytes p
relaxPinnedBytes = castPinnedBytes

relaxPinnedMBytes :: MBytes 'Pin e -> MBytes p e
relaxPinnedMBytes = castPinnedMBytes

toInconclusiveBytes :: Bytes p -> Bytes 'Inc
toInconclusiveBytes = castPinnedBytes

toInconclusiveMBytes :: MBytes p e -> MBytes 'Inc e
toInconclusiveMBytes = castPinnedMBytes


-- | How many elements of type @a@ fits into bytes completely. In order to get a possible
-- count of leftover bytes use `countRemBytes`
countBytes :: Unbox e => Bytes p -> Count e
countBytes = fromByteCount . byteCountBytes
{-# INLINE countBytes #-}

-- | How many elements of type @a@ fits into bytes completely. In order to get any number
-- of leftover bytes use `countRemBytes`
getCountMBytes :: (MonadPrim s m, Unbox e) => MBytes p s -> m (Count e)
getCountMBytes b = fromByteCount <$> getByteCountMBytes b
{-# INLINE getCountMBytes #-}

readOffMBytes :: (MonadPrim s m, Unbox e) => MBytes p s -> Off e -> m e
readOffMBytes (MBytes mba#) (Off (I# i#)) = prim (readMutableByteArray# mba# i#)
{-# INLINE readOffMBytes #-}

readByteOffMBytes :: (MonadPrim s m, Unbox e) => MBytes p s -> Off Word8 -> m e
readByteOffMBytes (MBytes mba#) (Off (I# i#)) = prim (readByteOffMutableByteArray# mba# i#)
{-# INLINE readByteOffMBytes #-}

writeOffMBytes :: (MonadPrim s m, Unbox e) => MBytes p s -> Off e -> e -> m ()
writeOffMBytes (MBytes mba#) (Off (I# i#)) a = prim_ (writeMutableByteArray# mba# i# a)
{-# INLINE writeOffMBytes #-}

writeByteOffMBytes :: (MonadPrim s m, Unbox e) => MBytes p s -> Off Word8 -> e -> m ()
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
     (MonadPrim s m, Unbox e)
  => MBytes p s -- ^ Chunk of memory to fill
  -> Off e -- ^ Offset in number of elements
  -> Count e -- ^ Number of cells to fill
  -> e -- ^ A value to fill the cells with
  -> m ()
setMBytes (MBytes mba#) (Off (I# o#)) (Count (I# n#)) a = prim_ (setMutableByteArray# mba# o# n# a)
{-# INLINE setMBytes #-}


-- | /O(1)/ - Cast an unboxed array into `Bytes`
--
-- @since 0.3.0
fromUArrayBytes :: UArray e -> Bytes 'Inc
fromUArrayBytes (UArray ba#) = fromByteArray# ba#
{-# INLINE fromUArrayBytes #-}

-- | /O(1)/ - Cast `Bytes` into an unboxed array
--
-- @since 0.3.0
toUArrayBytes :: Bytes p -> UArray e
toUArrayBytes b = UArray (toByteArray# b)
{-# INLINE toUArrayBytes #-}

-- | /O(1)/ - Cast a mutable unboxed array into `MBytes`
--
-- @since 0.3.0
fromUMArrayMBytes :: UMArray e s -> MBytes 'Inc s
fromUMArrayMBytes (UMArray a#) = fromMutableByteArray# a#
{-# INLINE fromUMArrayMBytes #-}

-- | /O(1)/ - Cast `MBytes` into a mutable unboxed array
--
-- @since 0.3.0
toUMArrayMBytes :: MBytes p s -> UMArray e s
toUMArrayMBytes mb = UMArray (toMutableByteArray# mb)
{-# INLINE toUMArrayMBytes #-}



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
withNoHaltPtrBytes b f = keepAlive b $ f (toPtrBytes b)
{-# INLINE withNoHaltPtrBytes #-}

withPtrMBytes :: MonadPrim s m => MBytes 'Pin s -> (Ptr e -> m b) -> m b
withPtrMBytes mb f = do
  res <- f (toPtrMBytes mb)
  res <$ touch mb
{-# INLINE withPtrMBytes #-}

withNoHaltPtrMBytes :: MonadUnliftPrim s m => MBytes 'Pin s -> (Ptr e -> m b) -> m b
withNoHaltPtrMBytes mb f = keepAlive mb $ f (toPtrMBytes mb)
{-# INLINE withNoHaltPtrMBytes #-}

toForeignPtrBytes :: Bytes 'Pin -> ForeignPtr e
toForeignPtrBytes (Bytes ba#) =
  ForeignPtr (byteArrayContents# ba#) (PlainPtr (unsafeCoerce# ba#))
{-# INLINE toForeignPtrBytes #-}


toForeignPtrMBytes :: MBytes 'Pin s -> ForeignPtr e
toForeignPtrMBytes (MBytes mba#) =
  ForeignPtr (mutableByteArrayContents# mba#) (PlainPtr (unsafeCoerce# mba#))
{-# INLINE toForeignPtrMBytes #-}


-- | This function will only cast a pointer that was allocated on Haskell heap and it is
-- cerain that the ForeignPtr has no finalizers associated with it.
castForeignPtrToBytes :: ForeignPtr e -> Either String (Bytes 'Pin)
castForeignPtrToBytes fp =
  unsafePerformIO $
  onForeignPtrContents fp checkConvert $ \_ ->
    pure (Left "Cannot convert a C allocated pointer")
  where
    checkConvert addr# mba# checkFinalizers = do
      ba@(Bytes ba#) <- freezeMBytes (MBytes mba#)
      if isTrue# (byteArrayContents# ba# `eqAddr#` addr#)
        then do
          hasFinilizers <- checkFinalizers
          pure $
            if hasFinilizers
              then Left "MallocPtr has associated finalizers"
              else Right ba
        else pure $
             Left
               "ForeignPtr does not point to the beginning of the associated MutableByteArray#"
{-# INLINE castForeignPtrToBytes #-}


onForeignPtrContents ::
     MonadPrim RW m
  => ForeignPtr e
  -> (Addr# -> MutableByteArray# RW -> m Bool -> m a)
  -> (Addr# -> m a)
  -> m a
onForeignPtrContents (ForeignPtr addr# contents) onHaskellPtr onCPtr =
  case contents of
    PlainPtr mbaRW# -> onHaskellPtr addr# mbaRW# (pure False)
#if MIN_VERSION_base(4,14,0)
    MallocPtr mbaRW# fref -> onHaskellPtr addr# mbaRW# $ do
      finilizers <- liftPrimBase $ readIORef fref
      pure $! case finilizers of
        NoFinalizers         -> False
        HaskellFinalizers fs -> not $! null fs
        CFinalizers _        -> True -- impossible case, but nevertheless
#else
    MallocPtr mbaRW# _ -> onHaskellPtr addr# mbaRW# (pure True)
#endif
    PlainForeignPtr _ -> onCPtr addr#
{-# INLINE onForeignPtrContents #-}


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


-- | Check if two mutable bytes pointers refer to the same memory
--
-- @since 0.1.0
isSameMBytes :: MBytes p1 s -> MBytes p2 s -> Bool
isSameMBytes (MBytes mb1#) (MBytes mb2#) = isTrue# (sameMutableByteArray# mb1# mb2#)
{-# INLINE isSameMBytes #-}


byteStringConvertError :: String -> a
byteStringConvertError msg = error $ "Cannot convert 'ByteString'. " ++ msg
{-# NOINLINE byteStringConvertError #-}

