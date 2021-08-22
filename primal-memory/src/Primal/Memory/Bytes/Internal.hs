{-# LANGUAGE BangPatterns #-}
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
-- Copyright   : (c) Alexey Kuleshevich 2020-2021
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
  , toIncBytes
  , toIncMBytes
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
  , setByteOffMBytes
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
  , castForeignPtrToBytes
  , onForeignPtrContents
  , byteStringConvertError
  -- * Ptr
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
  -- * MemRead
  , MemRead(..)
  , MemWrite(..)
  , MemAlloc(..)
  -- * Helpers
  , defaultReallocMutMem
  -- ** text
  , fromTextArrayBytes
  , toTextArrayBytes
  , fromTextMArrayMBytes
  , toTextMArrayMBytes
  -- ** bytestring
  , withPtrByteString
  , fromShortByteStringBytes
  , toShortByteStringBytes
  ) where

import qualified Data.ByteString as BS
import Data.ByteString.Internal
import Data.ByteString.Short.Internal
import qualified Data.Text.Array as T
import qualified Data.Text.Internal as T
import Data.Typeable
import GHC.ForeignPtr
import GHC.Ptr
import Primal.Array.Unboxed
import Primal.Eval
import Primal.Foreign
import Primal.Memory.Ptr
import Primal.Monad
import Primal.Monad.Unsafe
import Primal.Mutable.Eq
import Primal.Mutable.Freeze
import Primal.Mutable.Ord
import Primal.Unbox.Class
import Primal.Unlift
import Unsafe.Coerce
#if MIN_VERSION_base(4,14,0)
import Data.IORef
#endif

-- | When memory is allocated there is a distinction between pinned and unpinned.
--
-- Pinned memory is such that it is guaranteed not to move throughout the lifetime of a
-- program. In other words the address pointer that refers to allocated bytes will not
-- change until the associated `ByteArray#` or `MutableByteArray#` is no longer referenced
-- anywhere in the program at which point it gets garbage collected. On the other hand
-- unpinned memory can be moved around during GC, which helps to reduce memory
-- fragmentation.
--
-- Pinned/unpinnned choice during allocation is a bit of a lie, because when attempt is
-- made to allocate memory as unpinned and requested size is a bit more than a certain
-- threshold (somewhere around 3KiB) then it will still be allocated as pinned. Because of
-- that fact throughout the "primal" universe there is a distinction between memory that
-- is either @`Pin`ned@ or @`Inc`onclusive@.
--
-- It is possible to use one of `Primal.Memory.Bytes.toPinnedBytes` or
-- `Primal.Memory.Bytes.toPinnedMBytes` to get a conclusive type.
--
-- @since 0.1.0
data Pinned
  = Pin -- ^ Pinned, which indicates that allocated memory will not move
  | Inc -- ^ Inconclusive, thus memory could be pinned or unpinned

-- | An immutable region of memory which is allocated either as pinned or unpinned.
--
-- Constructor is not exported for safety. Violating type level `Pinned` kind is very
-- dangerous. Type safe constructor `Primal.Memory.Bytes.fromByteArray#` and unwrapper
-- `Primal.Memory.Bytes.toByteArray#` should be used instead. As a backdoor, of course,
-- the actual constructor is available from @Primal.Memory.Internal@
data Bytes (p :: Pinned) = Bytes ByteArray#
type role Bytes nominal


instance NFData (Bytes p) where
  rnf (Bytes _) = ()

instance Unlift (Bytes p) where
  type UnliftIso (Bytes p) = Bytes p
  indexArrayArray# aa# i# = Bytes (indexByteArrayArray# aa# i#)
  {-# INLINE indexArrayArray# #-}
  readMutableArrayArray# maa# i# s = case readByteArrayArray# maa# i# s of
                                      (# s', ba# #) -> (# s', Bytes ba# #)
  {-# INLINE readMutableArrayArray# #-}
  writeMutableArrayArray# maa# i# (Bytes ba#) = writeByteArrayArray# maa# i# ba#
  {-# INLINE writeMutableArrayArray# #-}


-- | Mutable region of memory which is allocated either as pinned or unpinned.
--
-- Constructor is not exported for safety. Violating type level `Pinned` kind is very
-- dangerous. Type safe constructor `Primal.Memory.Bytes.fromMutableByteArray#` and
-- unwrapper `Primal.Memory.Bytes.toMutableByteArray#` should be used instead. As a
-- backdoor, of course, the actual constructor is available from "Primal.Memory.Internal"
-- module and especially unsafe function `castPinnedMBytes` was crafted.
data MBytes (p :: Pinned) s = MBytes (MutableByteArray# s)
type role MBytes nominal nominal


instance NFData (MBytes p s) where
  rnf (MBytes _) = ()

instance MutNFData (MBytes p) where
  rnfMutST (MBytes _) = pure ()

instance MutUnlift (MBytes p) where
  type MutUnliftIso (MBytes p) = MBytes p
  readMutMutableArrayArray# maa# i# s = case readMutableByteArrayArray# maa# i# s of
                                          (# s', ba# #) -> (# s', MBytes ba# #)
  {-# INLINE readMutMutableArrayArray# #-}
  writeMutMutableArrayArray# maa# i# (MBytes ba#) = writeMutableByteArrayArray# maa# i# ba#
  {-# INLINE writeMutMutableArrayArray# #-}


type instance Frozen (MBytes p) = Bytes p

instance Typeable p => MutFreeze (MBytes p) where
  thawST = thawBytes
  {-# INLINE thawST #-}
  thawCloneST bs = do
    let bytes = byteCountBytes bs
    mbs <- allocMBytes bytes
    mbs <$ copyByteOffBytesToMBytes bs 0 mbs 0 bytes
  {-# INLINE thawCloneST #-}
  freezeMutST = freezeMBytes
  {-# INLINE freezeMutST #-}

instance MutEq (MBytes p) where
  eqMutST mbs1 mbs2 = (EQ ==) <$> compareMut mbs1 mbs2
  {-# INLINE eqMutST #-}

instance MutOrd (MBytes p) where
  compareMutST mbs1 mbs2
    | isSameMBytes mbs1 mbs2 = pure EQ
    | otherwise = do
      sz1 <- getByteCountMBytes mbs1
      sz2 <- getByteCountMBytes mbs2
      case compare sz1 sz2 of
        EQ  -> compareByteOffMBytes mbs1 0 mbs2 0 sz1
        cmp -> pure cmp
  {-# INLINE compareMutST #-}


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

compareByteOffBytes ::
     Unbox e
  => Bytes p1
  -> Off Word8
  -> Bytes p2
  -> Off Word8
  -> Count e
  -> Ordering
compareByteOffBytes (Bytes b1#) (Off (I# off1#)) (Bytes b2#) (Off (I# off2#)) c =
  toOrdering# (compareByteArrays# b1# off1# b2# off2# (unCountBytes# c))
{-# INLINE compareByteOffBytes #-}

compareByteOffMBytes ::
     (Unbox e, Primal s m)
  => MBytes p1 s
  -> Off Word8
  -> MBytes p2 s
  -> Off Word8
  -> Count e
  -> m Ordering
compareByteOffMBytes (MBytes mb1#) (Off (I# off1#)) (MBytes mb2#) (Off (I# off2#)) c =
  toOrdering <$> unsafeIOToPrimal (memcmpMutableByteArray# mb1# off1# mb2# off2# (unCountBytes# c))
{-# INLINE compareByteOffMBytes #-}

indexOffBytes :: Unbox e => Bytes p -> Off e -> e
indexOffBytes (Bytes ba#) (Off (I# i#)) = indexByteArray# ba# i#
{-# INLINE indexOffBytes #-}

indexByteOffBytes :: Unbox e => Bytes p -> Off Word8 -> e
indexByteOffBytes (Bytes ba#) (Off (I# i#)) = indexByteOffByteArray# ba# i#
{-# INLINE indexByteOffBytes #-}


---- Mutable


allocMBytes ::
     forall p e s m. (Typeable p, Unbox e, Primal s m)
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

allocUnpinnedMBytes :: (Primal s m, Unbox e) => Count e -> m (MBytes 'Inc s)
allocUnpinnedMBytes c =
  primal $ \s ->
    case newByteArray# (unCountBytes# c) s of
      (# s', ba# #) -> (# s', MBytes ba# #)
{-# INLINE allocUnpinnedMBytes #-}


allocPinnedMBytes :: (Primal s m, Unbox e) => Count e -> m (MBytes 'Pin s)
allocPinnedMBytes c =
  primal $ \s ->
    case newPinnedByteArray# (unCountBytes# c) s of
      (# s', ba# #) -> (# s', MBytes ba# #)
{-# INLINE allocPinnedMBytes #-}

allocAlignedMBytes ::
     forall e m s. (Primal s m, Unbox e)
  => Count e -- ^ Size in number of bytes
  -> m (MBytes 'Pin s)
allocAlignedMBytes c =
  primal $ \s ->
    case newAlignedPinnedByteArray#
           (unCountBytes# c)
           (alignment# (proxy# :: Proxy# e))
           s of
      (# s', ba# #) -> (# s', MBytes ba# #)
{-# INLINE allocAlignedMBytes #-}


-- @since 0.3.0
allocZeroPinnedMBytes ::
     (Primal s m, Unbox e)
  => Count e -- ^ Size in number of bytes
  -> m (MBytes 'Pin s)
allocZeroPinnedMBytes n = allocPinnedMBytes n >>= \mb -> mb <$ setMBytes mb 0 (toByteCount n) 0
{-# INLINE allocZeroPinnedMBytes #-}

-- @since 0.3.0
allocZeroAlignedMBytes ::
     (Primal s m, Unbox e)
  => Count e -- ^ Size in number of bytes
  -> m (MBytes 'Pin s)
allocZeroAlignedMBytes n = allocAlignedMBytes n >>= \mb -> mb <$ setMBytes mb 0 (toByteCount n) 0
{-# INLINE allocZeroAlignedMBytes #-}


getByteCountMBytes :: Primal s m => MBytes p s -> m (Count Word8)
getByteCountMBytes (MBytes mba#) =
  primal $ \s ->
    case getSizeofMutableByteArray# mba# s of
      (# s', n# #) -> (# s', Count (I# n#) #)
{-# INLINE getByteCountMBytes #-}

freezeMBytes :: Primal s m => MBytes p s -> m (Bytes p)
freezeMBytes (MBytes mba#) =
  primal $ \s ->
    case unsafeFreezeByteArray# mba# s of
      (# s', ba# #) -> (# s', Bytes ba# #)
{-# INLINE freezeMBytes #-}

thawBytes :: Primal s m => Bytes p -> m (MBytes p s)
thawBytes (Bytes ba#) =
  primal $ \s ->
    case unsafeThawByteArray# ba# s of
      (# s', mba# #) -> (# s', MBytes mba# #)
{-# INLINE thawBytes #-}

copyByteOffBytesToMBytes ::
     (Primal s m, Unbox e) => Bytes ps -> Off Word8 -> MBytes pd s -> Off Word8 -> Count e -> m ()
copyByteOffBytesToMBytes (Bytes src#) (Off (I# srcOff#)) (MBytes dst#) (Off (I# dstOff#)) c =
  primal_ $ copyByteArray# src# srcOff# dst# dstOff# (unCountBytes# c)
{-# INLINE copyByteOffBytesToMBytes #-}

moveByteOffMBytesToMBytes ::
     (Primal s m, Unbox e) => MBytes ps s-> Off Word8 -> MBytes pd s -> Off Word8 -> Count e -> m ()
moveByteOffMBytesToMBytes (MBytes src#) (Off (I# srcOff#)) (MBytes dst#) (Off (I# dstOff#)) c =
  primal_ (copyMutableByteArray# src# srcOff# dst# dstOff# (unCountBytes# c))
{-# INLINE moveByteOffMBytesToMBytes #-}


byteCountBytes :: Bytes p -> Count Word8
byteCountBytes (Bytes ba#) = coerce (I# (sizeofByteArray# ba#))
{-# INLINE byteCountBytes #-}


-- | Shrink mutable bytes to new specified count of elements. The new count must be less
-- than or equal to the current count as reported by `getCountMBytes`.
shrinkMBytes :: (Primal s m, Unbox e) => MBytes p s -> Count e -> m ()
shrinkMBytes (MBytes mb#) c = primal_ (shrinkMutableByteArray# mb# (unCountBytes# c))
{-# INLINE shrinkMBytes #-}


-- | Attempt to resize mutable bytes in place.
--
-- * New bytes might be allocated, with the copy of an old one.
-- * Old references should not be kept around to allow GC to claim it
-- * Old references should not be used to avoid undefined behavior
resizeMBytes ::
     (Primal s m, Unbox e) => MBytes p s -> Count e -> m (MBytes 'Inc s)
resizeMBytes (MBytes mb#) c =
  primal $ \s ->
    case resizeMutableByteArray# mb# (unCountBytes# c) s of
      (# s', mb'# #) -> (# s', MBytes mb'# #)
{-# INLINE resizeMBytes #-}

reallocMBytes ::
     forall e p m s. (Primal s m, Typeable p,  Unbox e)
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

toIncBytes :: Bytes p -> Bytes 'Inc
toIncBytes = castPinnedBytes

toIncMBytes :: MBytes p e -> MBytes 'Inc e
toIncMBytes = castPinnedMBytes


-- | How many elements of type @a@ fits into bytes completely. In order to get a possible
-- count of leftover bytes use `countRemBytes`
countBytes :: Unbox e => Bytes p -> Count e
countBytes = fromByteCount . byteCountBytes
{-# INLINE countBytes #-}

-- | How many elements of type @a@ fits into bytes completely. In order to get any number
-- of leftover bytes use `countRemBytes`
getCountMBytes :: (Primal s m, Unbox e) => MBytes p s -> m (Count e)
getCountMBytes b = fromByteCount <$> getByteCountMBytes b
{-# INLINE getCountMBytes #-}

readOffMBytes :: (Primal s m, Unbox e) => MBytes p s -> Off e -> m e
readOffMBytes (MBytes mba#) (Off (I# i#)) = primal (readMutableByteArray# mba# i#)
{-# INLINE readOffMBytes #-}

readByteOffMBytes :: (Primal s m, Unbox e) => MBytes p s -> Off Word8 -> m e
readByteOffMBytes (MBytes mba#) (Off (I# i#)) = primal (readByteOffMutableByteArray# mba# i#)
{-# INLINE readByteOffMBytes #-}

writeOffMBytes :: (Primal s m, Unbox e) => MBytes p s -> Off e -> e -> m ()
writeOffMBytes (MBytes mba#) (Off (I# i#)) a = primal_ (writeMutableByteArray# mba# i# a)
{-# INLINE writeOffMBytes #-}

writeByteOffMBytes :: (Primal s m, Unbox e) => MBytes p s -> Off Word8 -> e -> m ()
writeByteOffMBytes (MBytes mba#) (Off (I# i#)) a =
  primal_ (writeByteOffMutableByteArray# mba# i# a)
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



-- | Set a region of memory to the same value starting at an offset in number of elements.
setMBytes ::
     (Primal s m, Unbox e)
  => MBytes p s -- ^ Chunk of memory to fill
  -> Off e -- ^ Offset in number of elements
  -> Count e -- ^ Number of cells to fill
  -> e -- ^ A value to fill the cells with
  -> m ()
setMBytes (MBytes mba#) (Off (I# o#)) (Count (I# n#)) a = primal_ (setMutableByteArray# mba# o# n# a)
{-# INLINE setMBytes #-}


-- | Set a region of memory to the same value starting at an offset in bytes.
--
-- @since 1.0.0
setByteOffMBytes ::
     (Primal s m, Unbox e)
  => MBytes p s -- ^ Chunk of memory to fill
  -> Off Word8 -- ^ Offset in number of bytes
  -> Count e -- ^ Number of cells to fill
  -> e -- ^ A value to fill the cells with
  -> m ()
setByteOffMBytes (MBytes mba#) (Off (I# o#)) (Count (I# n#)) a =
  primal_ (setByteOffMutableByteArray# mba# o# n# a)
{-# INLINE setByteOffMBytes #-}


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
withPtrBytes :: Primal s m => Bytes 'Pin -> (Ptr e -> m b) -> m b
withPtrBytes b f = do
  res <- f (toPtrBytes b)
  res <$ touch b
{-# INLINE withPtrBytes #-}

-- | Same as `withPtrBytes`, but is suitable for actions that don't terminate
withNoHaltPtrBytes :: UnliftPrimal s m => Bytes 'Pin -> (Ptr e -> m b) -> m b
withNoHaltPtrBytes b f = keepAlive b $ f (toPtrBytes b)
{-# INLINE withNoHaltPtrBytes #-}

withPtrMBytes :: Primal s m => MBytes 'Pin s -> (Ptr e -> m b) -> m b
withPtrMBytes mb f = do
  res <- f (toPtrMBytes mb)
  res <$ touch mb
{-# INLINE withPtrMBytes #-}

withNoHaltPtrMBytes :: UnliftPrimal s m => MBytes 'Pin s -> (Ptr e -> m b) -> m b
withNoHaltPtrMBytes mb f = keepAlive mb $ f (toPtrMBytes mb)
{-# INLINE withNoHaltPtrMBytes #-}


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
     Primal RW m
  => ForeignPtr e
  -> (Addr# -> MutableByteArray# RW -> m Bool -> m a)
  -> (Addr# -> m a)
  -> m a
onForeignPtrContents (ForeignPtr addr# contents) onHaskellPtr onCPtr =
  case contents of
    PlainPtr mbaRW# -> onHaskellPtr addr# mbaRW# (pure False)
#if MIN_VERSION_base(4,14,0)
    MallocPtr mbaRW# fref -> onHaskellPtr addr# mbaRW# $ do
      finilizers <- liftPrimalState $ readIORef fref
      pure $! case finilizers of
        NoFinalizers         -> False
        HaskellFinalizers fs -> not $ Prelude.null fs
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


-- | Check if two `MBytes` pointers refer to the same memory
--
-- @since 0.1.0
isSameMBytes :: MBytes p1 s -> MBytes p2 s -> Bool
isSameMBytes (MBytes mb1#) (MBytes mb2#) = isTrue# (sameMutableByteArray# mb1# mb2#)
{-# INLINE isSameMBytes #-}


byteStringConvertError :: String -> a
byteStringConvertError msg = error $ "Cannot convert 'ByteString'. " ++ msg
{-# NOINLINE byteStringConvertError #-}


-- Ptr + Bytes interoperability

copyByteOffBytesToPtr ::
     (Primal s m, Unbox e)
  => Bytes p
  -> Off Word8
  -> Ptr e
  -> Off Word8
  -> Count e
  -> m ()
copyByteOffBytesToPtr (Bytes src#) (Off (I# srcOff#)) (Ptr dstAddr#) (Off (I# dstOff#)) c =
  primal_ $
  copyByteArrayToAddr#
    src#
    srcOff#
    (dstAddr# `plusAddr#` dstOff#)
    (unCountBytes# c)
{-# INLINE copyByteOffBytesToPtr #-}

compareByteOffBytesToPtr ::
     Unbox e => Bytes p -> Off Word8 -> Ptr e -> Off Word8 -> Count e -> Ordering
compareByteOffBytesToPtr (Bytes b#) (Off (I# off1#)) (Ptr addr#) (Off (I# off2#)) c =
  toOrdering# (memcmpByteArrayAddr# b# off1# addr# off2# (unCountBytes# c))
{-# INLINE compareByteOffBytesToPtr #-}


moveByteOffMBytesToPtr ::
  (Primal s m, Unbox e) => MBytes p s -> Off Word8 -> Ptr e -> Off Word8 -> Count e -> m ()
moveByteOffMBytesToPtr (MBytes src#) (Off (I# srcOff#)) (Ptr dstAddr#) (Off (I# dstOff#)) c =
  unsafeIOToPrimal $
  memmoveMutableByteArrayToAddr# src# srcOff# dstAddr# dstOff# (unCountBytes# c)
{-# INLINE moveByteOffMBytesToPtr #-}

copyPtrToMBytes ::
     (Primal s m, Unbox e) => Ptr e -> Off e -> MBytes p s -> Off e -> Count e -> m ()
copyPtrToMBytes src srcOff dst dstOff =
  copyByteOffPtrToMBytes src (toByteOff srcOff) dst (toByteOff dstOff)
{-# INLINE copyPtrToMBytes #-}



copyByteOffPtrToMBytes ::
     (Primal s m, Unbox e) => Ptr e -> Off Word8 -> MBytes p s -> Off Word8 -> Count e -> m ()
copyByteOffPtrToMBytes (Ptr srcAddr#) (Off (I# srcOff#)) (MBytes dst#) (Off (I# dstOff#)) c =
  primal_ $ copyAddrToByteArray# (srcAddr# `plusAddr#` srcOff#) dst# dstOff# (unCountBytes# c)
{-# INLINE copyByteOffPtrToMBytes #-}


copyBytesToPtr :: (Primal s m, Unbox e) => Bytes p -> Off e -> Ptr e -> Off e -> Count e -> m ()
copyBytesToPtr src srcOff dst dstOff =
  copyByteOffBytesToPtr src (toByteOff srcOff) dst (toByteOff dstOff)
{-# INLINE copyBytesToPtr #-}




copyMBytesToPtr :: (Primal s m, Unbox e) => MBytes p s -> Off e -> Ptr e -> Off e -> Count e -> m ()
copyMBytesToPtr src srcOff dst dstOff =
  copyByteOffMBytesToPtr src (toByteOff srcOff) dst (toByteOff dstOff)
{-# INLINE copyMBytesToPtr #-}


copyByteOffMBytesToPtr ::
     (Primal s m, Unbox e)
  => MBytes p s
  -> Off Word8
  -> Ptr e
  -> Off Word8
  -> Count e
  -> m ()
copyByteOffMBytesToPtr (MBytes src#) (Off (I# srcOff#)) (Ptr dstAddr#) (Off (I# dstOff#)) c =
  primal_ $
  copyMutableByteArrayToAddr#
    src#
    srcOff#
    (dstAddr# `plusAddr#` dstOff#)
    (unCountBytes# c)
{-# INLINE copyByteOffMBytesToPtr #-}


movePtrToMBytes :: (Primal s m, Unbox e) => Ptr e -> Off e -> MBytes p s -> Off e -> Count e -> m ()
movePtrToMBytes src srcOff dst dstOff =
  moveByteOffPtrToMBytes src (toByteOff srcOff) dst (toByteOff dstOff)
{-# INLINE movePtrToMBytes #-}

moveByteOffPtrToMBytes ::
     (Primal s m, Unbox e)
  => Ptr e
  -> Off Word8
  -> MBytes p s
  -> Off Word8
  -> Count e
  -> m ()
moveByteOffPtrToMBytes (Ptr srcAddr#) (Off (I# srcOff#)) (MBytes dst#) (Off (I# dstOff#)) c =
  unsafeIOToPrimal $
  memmoveMutableByteArrayFromAddr# srcAddr# srcOff# dst# dstOff# (unCountBytes# c)
{-# INLINE moveByteOffPtrToMBytes #-}

moveMBytesToPtr :: (Primal s m, Unbox e) => MBytes p s -> Off e -> Ptr e -> Off e -> Count e -> m ()
moveMBytesToPtr src srcOff dst dstOff =
  moveByteOffMBytesToPtr src (toByteOff srcOff) dst (toByteOff dstOff)
{-# INLINE moveMBytesToPtr #-}



compareByteOffPtrToBytes ::
     Unbox e => Ptr e -> Off Word8 -> Bytes p -> Off Word8 -> Count e -> Ordering
compareByteOffPtrToBytes (Ptr addr#) (Off (I# off1#)) (Bytes b#) (Off (I# off2#)) c =
  toOrdering# (memcmpAddrByteArray# addr# off1# b# off2# (unCountBytes# c))
{-# INLINE compareByteOffPtrToBytes #-}


-- | An action that can be used as a default implementation for `reallocMutMem`. Whenever
-- current memory region byte count matches the supplied new size exactly then such memory
-- region is simply returned back and this function is a noop. Otherwise a new memory
-- region is allocated and all the data that can fit into the new region will be copied
-- over.
--
-- [Unsafe] Same unsafety notice as in `reallocMutMem`
--
-- @since 0.3.0
defaultReallocMutMem ::
     forall e ma m s. (Unbox e, MemAlloc ma, Primal s m)
  => ma s
  -> Count e
  -> m (ma s)
defaultReallocMutMem mem c = liftST $ do
  let newByteCount = toByteCount c
  oldByteCount <- getByteCountMutMemST mem
  if oldByteCount == newByteCount
    then pure mem
    else do
      newMem <- allocMutMemST newByteCount
      oldMem <- freezeMut mem
      newMem <$ copyByteOffMutMemST oldMem 0 newMem 0 (min oldByteCount newByteCount)
{-# INLINE defaultReallocMutMem #-}



-- | Type class that can be implemented for an immutable data type that provides direct
-- read-only access to its underlying memory
class MemRead mr where

  accessMem :: mr -> (ByteArray# -> Off Word8 -> a) -> (Addr# -> Off Word8 -> a) -> Off Word8 -> a

  -- | Check if two read only regions refer to the exact same region of memory
  --
  -- @since 0.3.0
  isSameMem :: mr -> mr -> Bool

  -- | Number of bytes allocated by the data type available for reading.
  --
  -- ====__Example__
  --
  -- >>> :set -XDataKinds
  -- >>> import Primal.Memory
  -- >>> byteCountMem (fromByteListMem [1,2,3] :: Bytes 'Inc)
  -- Count {unCount = 3}
  --
  -- @since 0.1.0
  byteCountMem :: mr -> Count Word8

  -- | Read an element with an offset in number of elements, rather than bytes as is the
  -- case with `indexByteOffMem`.
  --
  -- [Unsafe] Bounds are not checked. When precondition for @off@ argument is violated the
  -- result is either unpredictable output or failure with a segfault.
  --
  -- @since 0.1.0
  indexOffMem ::
       Unbox e
    => mr -- ^ /memRead/ - Memory to read an element from
    -> Off e
    -- ^ /off/ - Offset in number of elements from the beginning of @memRead@
    --
    -- /__Preconditions:__/
    --
    -- > 0 <= off
    --
    -- > unOffBytes off <= unCount (byteCountMem memRead - byteCountType @e)
    --
    -> e
  indexOffMem mr off = indexByteOffMem mr (toByteOff off)
  {-# INLINE indexOffMem #-}

  -- | Read an element with an offset in number of bytes. Bounds are not checked.
  --
  -- [Unsafe] When precondition for @off@ argument is violated the result is either
  -- unpredictable output or failure with a segfault.
  --
  -- @since 0.1.0
  indexByteOffMem ::
       Unbox e
    => mr -- ^ /memRead/ - Memory to read an element from
    -> Off Word8
    -- ^ /off/ - Offset in number of elements from the beginning of @memRead@
    --
    -- /__Preconditions:__/
    --
    -- > 0 <= unOff off
    --
    -- > unOff off <= unCount (byteCountMem memRead - byteCountType @e)
    --
    -> e

  -- | Same as `compareByteOffMem`, but compare the read-only memory region to `Bytes`.
  --
  -- [Unsafe] When any precondition for either of the offsets @memOff1@, @memOff2@ or the
  -- element count @memCount@ is violated the result is either unpredictable output or
  -- failure with a segfault.
  --
  -- @since 0.1.0
  compareByteOffToBytesMem ::
       Unbox e
    => mr -- ^ /memRead1/ - First memory region
    -> Off Word8
    -- ^ /memOff1/ - Offset for @memRead1@ in number of bytes
    --
    -- /__Preconditions:__/
    --
    -- > 0 <= memOff1
    --
    -- > unOff memOff1 <= unCount (byteCountMem memRead1 - byteCountType @e)
    -> Bytes p -- ^ /memRead2/- Second memory region that is backed by `Bytes`
    -> Off Word8
    -- ^ /memOff2/ - Offset for @memRead2@ in number of bytes
    --
    -- /__Preconditions:__/
    --
    -- > 0 <= memOff2
    --
    -- > unOff memOff2 <= unCount (byteCountMem memRead2 - byteCountType @e)
    -> Count e
    -- ^ /memCount/ - Number of elements of type __@e@__ to compare as binary
    --
    -- /__Preconditions:__/
    --
    -- > 0 <= memCount
    --
    -- > unCountBytes memCount + unOff memOff1 <= unCount (byteCountMem memRead1 - byteCountType @e)
    --
    -- > unCountBytes memCount + unOff memOff2 <= unCount (byteCountMem memRead2 - byteCountType @e)
    -> Ordering

  -- | See `Primal.Memory.compareByteOffToPtrMem` for details.
  --
  -- @since 1.0.0
  compareByteOffToPtrMemST ::
       Unbox e
    => mr
    -> Off Word8
    -> Ptr e
    -> Off Word8
    -> Count e
    -> ST s Ordering

  -- | Compare two read-only regions of memory byte-by-byte. The very first mismatched
  -- byte will cause this function to produce `LT` if the byte in @memRead1@ is smaller
  -- than the one in @memRead2@ and `GT` if it is bigger. It is not a requirement to
  -- short-circuit on the first mismatch, but it is a good optimization to have for
  -- non-sensitive data. Memory regions that store security critical data may choose to
  -- implement this function to work in constant time.
  --
  -- This function is usually implemented by either one of `compareByteOffToPtrMem` or
  -- `compareByteOffToBytesMem`, depending on the nature of @mr@ type. However it differs
  -- from the aforementioned functions with a fact that it is pure non-monadic
  -- computation.
  --
  -- [Unsafe] When any precondition for either of the offsets @memOff1@, @memOff2@ or the
  -- element count @memCount@ is violated the result is either unpredictable output or
  -- failure with a segfault.
  --
  -- @since 0.1.0
  compareByteOffMem ::
       (MemRead mr', Unbox e)
    => mr' -- ^ /memRead1/ - First memory region
    -> Off Word8
    -- ^ /memOff1/ - Offset for @memRead1@ in number of bytes
    --
    -- /__Preconditions:__/
    --
    -- > 0 <= memOff1
    --
    -- > unOff memOff1 <= unCount (byteCountMem memRead1 - byteCountType @e)
    -> mr -- ^ /memRead2/ - Second memory region
    -> Off Word8
    -- ^ /memOff2/ - Offset for @memRead2@ in number of bytes
    --
    -- /__Preconditions:__/
    --
    -- > 0 <= memOff2
    --
    -- > unOff memOff2 <= unCount (byteCountMem memRead2 - byteCountType @e)
    -> Count e
    -- ^ /memCount/ - Number of elements of type __@e@__ to compare as binary
    --
    -- /__Preconditions:__/
    --
    -- > 0 <= memCount
    --
    -- > unCountBytes memCount + unOff memOff1 <= unCount (byteCountMem memRead1 - byteCountType @e)
    --
    -- > unCountBytes memCount + unOff memOff2 <= unCount (byteCountMem memRead2 - byteCountType @e)
    -> Ordering

  -- | See `Primal.Memory.copyByteOffToMBytesMem` for details.
  --
  -- @since 1.0.0
  copyByteOffToMBytesMemST ::
       Unbox e
    => mr
    -> Off Word8
    -> MBytes p s
    -> Off Word8
    -> Count e
    -> ST s ()

  -- | See `Primal.Memory.copyByteOffToPtrMem` for details.
  --
  -- @since 1.0.0
  copyByteOffToPtrMemST ::
       Unbox e => mr -> Off Word8 -> Ptr e -> Off Word8 -> Count e -> ST s ()


-- | Type class that can be implemented for a mutable data type that provides direct read
-- and write access to memory
class MemWrite mw where

  accessMutMemST :: mw s
                 -> (MutableByteArray# s -> Off Word8 -> ST s a)
                 -> (Addr# -> Off Word8 -> ST s a)
                 -> Off Word8
                 -> ST s a

  -- | Check if two mutable regions refer to the exact same region of memory
  --
  -- @since 0.3.0
  isSameMutMem :: mw s -> mw s -> Bool

  -- | See `Primal.Memory.readOffMutMem` for details.
  --
  -- @since 1.0.0
  readOffMutMemST :: Unbox e => mw s -> Off e -> ST s e
  readOffMutMemST mw off = readByteOffMutMemST mw (toByteOff off)
  {-# INLINE readOffMutMemST #-}

  -- | See `Primal.Memory.readByteOffMutMem` for details.
  --
  -- @since 1.0.0
  readByteOffMutMemST :: Unbox e => mw s -> Off Word8 -> ST s e

  -- | See `Primal.Memory.writeOffMutMem` for details.
  --
  -- @since 1.0.0
  writeOffMutMemST :: Unbox e => mw s -> Off e -> e -> ST s ()
  writeOffMutMemST mw off = writeByteOffMutMemST mw (toByteOff off)
  {-# INLINE writeOffMutMemST #-}

  -- | See `Primal.Memory.writeByteOffMutMem` for details.
  --
  -- @since 1.0.0
  writeByteOffMutMemST ::
       Unbox e
    => mw s -- ^ /memWrite/ - Memory region to write an element into
    -> Off Word8
    -> e
    -> ST s ()

  -- | See `Primal.Memory.moveByteOffToMBytesMutMem` for details.
  --
  -- @since 1.0.0
  moveByteOffToMBytesMutMemST ::
       Unbox e
    => mw s -- ^ /memSource/ - Source memory from where to copy
    -> Off Word8
    -> MBytes p s -- ^ /memTarget/ - Target memory into where to copy
    -> Off Word8
    -> Count e
    -> ST s ()

  -- | See `Primal.Memory.moveByteOffToPtrMutMem` for details.
  --
  -- @since 1.0.0
  moveByteOffToPtrMutMemST ::
       Unbox e
    => mw s
    -> Off Word8
    -> Ptr e
    -> Off Word8
    -> Count e
    -> ST s ()

  -- | See `Primal.Memory.copyByteOffMutMem` for details.
  --
  -- @since 1.0.0
  copyByteOffMutMemST ::
       (MemRead mr, Unbox e)
    => mr
    -> Off Word8
    -> mw s
    -> Off Word8
    -> Count e
    -> ST s ()

  -- | See `Primal.Memory.moveByteOffMutMem` for details.
  --
  -- @since 1.0.0
  moveByteOffMutMemST ::
       (MemWrite mw', Unbox e)
    => mw' s
    -> Off Word8
    -> mw s
    -> Off Word8
    -> Count e
    -> ST s ()

  -- | See `Primal.Memory.setByteOffMutMem` for details.
  --
  -- @since 1.0.0
  setByteOffMutMemST :: Unbox e => mw s -> Off Word8 -> Count e -> e -> ST s ()

  -- | See `Primal.Memory.setMutMem` for details.
  --
  -- @since 1.0.0
  setMutMemST :: Unbox e => mw s -> Off e -> Count e -> e -> ST s ()
  setMutMemST mv off = setByteOffMutMemST mv (toByteOff off)
  {-# INLINE setMutMemST #-}


-- | Generalized memory allocation and pure/mutable state conversion.
class (MemRead (Frozen ma), MemWrite ma, MutFreeze ma) => MemAlloc ma where

  -- | See `Primal.Memory.getByteCountMutMem` for details.
  --
  -- @since 1.0.0
  getByteCountMutMemST :: ma s -> ST s (Count Word8)

  -- | See `Primal.Memory.allocMutMemST` for details.
  --
  -- @since 1.0.0
  allocMutMemST :: Unbox e => Count e -> ST s (ma s)

  -- | Either grow or shrink currently allocated mutable region of memory. For some
  -- implementations it might be possible to change the size of the allocated region
  -- in-place, i.e. without copy. However in all implementations there is a good chance
  -- that the memory region has to be allocated anew, in which case all of the contents
  -- up to the minimum of new and old sizes will get copied over. After the resize
  -- operation is complete the supplied @memSource@ region must not be used
  -- anymore. Moreover, no reference to the old one should be kept in order to allow
  -- garbage collection of the original in case a new one had to be allocated.
  --
  -- Default implementation is `defaultReallocMutMem`
  --
  -- [Unsafe] Undefined behavior when @memSource@ is used afterwards. The same unsafety
  -- notice from `allocMutMem` with regards to @memCount@ is applicable here as well.
  --
  -- @since 0.3.0
  reallocMutMemST :: Unbox e
    => ma s
    -> Count e
    -> ST s (ma s)
  reallocMutMemST = defaultReallocMutMem
  {-# INLINE reallocMutMemST #-}


instance MemRead (Bytes p) where
  accessMem (Bytes ba#) f _ = f ba#
  {-# INLINE accessMem #-}
  isSameMem = isSameBytes
  {-# INLINE isSameMem #-}
  byteCountMem = byteCountBytes
  {-# INLINE byteCountMem #-}
  indexOffMem = indexOffBytes
  {-# INLINE indexOffMem #-}
  indexByteOffMem = indexByteOffBytes
  {-# INLINE indexByteOffMem #-}
  compareByteOffToBytesMem bytes1 off1 bytes2 off2 c =
    compareByteOffBytes bytes1 off1 bytes2 off2 c
  {-# INLINE compareByteOffToBytesMem #-}
  compareByteOffMem mem1 off1 bs off2 c =
    compareByteOffToBytesMem mem1 off1 bs off2 c
  {-# INLINE compareByteOffMem #-}
  compareByteOffToPtrMemST bytes1 off1 ptr2 off2 c =
    pure $! compareByteOffBytesToPtr bytes1 off1 ptr2 off2 c
  {-# INLINE compareByteOffToPtrMemST #-}
  copyByteOffToMBytesMemST = copyByteOffBytesToMBytes
  {-# INLINE copyByteOffToMBytesMemST #-}
  copyByteOffToPtrMemST = copyByteOffBytesToPtr
  {-# INLINE copyByteOffToPtrMemST #-}


instance MemWrite (MBytes p) where
  accessMutMemST (MBytes mba#) f _ = f mba#
  {-# INLINE accessMutMemST #-}
  isSameMutMem = isSameMBytes
  {-# INLINE isSameMutMem #-}
  readOffMutMemST = readOffMBytes
  {-# INLINE readOffMutMemST #-}
  readByteOffMutMemST = readByteOffMBytes
  {-# INLINE readByteOffMutMemST #-}
  writeOffMutMemST = writeOffMBytes
  {-# INLINE writeOffMutMemST #-}
  writeByteOffMutMemST = writeByteOffMBytes
  {-# INLINE writeByteOffMutMemST #-}
  moveByteOffToPtrMutMemST = moveByteOffMBytesToPtr
  {-# INLINE moveByteOffToPtrMutMemST #-}
  moveByteOffToMBytesMutMemST = moveByteOffMBytesToMBytes
  {-# INLINE moveByteOffToMBytesMutMemST #-}
  moveByteOffMutMemST = moveByteOffToMBytesMutMemST
  {-# INLINE moveByteOffMutMemST #-}
  copyByteOffMutMemST = copyByteOffToMBytesMemST
  {-# INLINE copyByteOffMutMemST #-}
  setByteOffMutMemST = setByteOffMBytes
  {-# INLINE setByteOffMutMemST #-}
  setMutMemST = setMBytes
  {-# INLINE setMutMemST #-}


instance Typeable p => MemAlloc (MBytes p) where
  getByteCountMutMemST = getByteCountMBytes
  {-# INLINE getByteCountMutMemST #-}
  allocMutMemST = allocMBytes
  {-# INLINE allocMutMemST #-}
  reallocMutMemST = reallocMBytes
  {-# INLINE reallocMutMemST #-}



instance MemRead (UArray e) where
  accessMem (UArray ba#) f _ = f ba#
  {-# INLINE accessMem #-}
  isSameMem = isSameUArray
  {-# INLINE isSameMem #-}
  byteCountMem = byteCountMem . fromUArrayBytes
  {-# INLINE byteCountMem #-}
  indexOffMem a = indexOffMem (fromUArrayBytes a)
  {-# INLINE indexOffMem #-}
  indexByteOffMem a = indexByteOffMem (fromUArrayBytes a)
  {-# INLINE indexByteOffMem #-}
  compareByteOffToBytesMem a = compareByteOffToBytesMem (fromUArrayBytes a)
  {-# INLINE compareByteOffToBytesMem #-}
  compareByteOffMem mem1 off1 a = compareByteOffMem mem1 off1 (fromUArrayBytes a)
  {-# INLINE compareByteOffMem #-}
  compareByteOffToPtrMemST a = compareByteOffToPtrMemST (fromUArrayBytes a)
  {-# INLINE compareByteOffToPtrMemST #-}
  copyByteOffToMBytesMemST a = copyByteOffToMBytesMemST (fromUArrayBytes a)
  {-# INLINE copyByteOffToMBytesMemST #-}
  copyByteOffToPtrMemST a = copyByteOffToPtrMemST (fromUArrayBytes a)
  {-# INLINE copyByteOffToPtrMemST #-}

instance MemWrite (UMArray e) where
  accessMutMemST (UMArray mba#) f _ = f mba#
  {-# INLINE accessMutMemST #-}
  isSameMutMem = isSameUMArray
  {-# INLINE isSameMutMem #-}
  readOffMutMemST = readOffMutMemST . fromUMArrayMBytes
  {-# INLINE readOffMutMemST #-}
  readByteOffMutMemST = readByteOffMutMemST . fromUMArrayMBytes
  {-# INLINE readByteOffMutMemST #-}
  writeOffMutMemST = writeOffMutMemST . fromUMArrayMBytes
  {-# INLINE writeOffMutMemST #-}
  writeByteOffMutMemST = writeByteOffMutMemST . fromUMArrayMBytes
  {-# INLINE writeByteOffMutMemST #-}
  moveByteOffToPtrMutMemST = moveByteOffToPtrMutMemST . fromUMArrayMBytes
  {-# INLINE moveByteOffToPtrMutMemST #-}
  moveByteOffToMBytesMutMemST = moveByteOffToMBytesMutMemST . fromUMArrayMBytes
  {-# INLINE moveByteOffToMBytesMutMemST #-}
  copyByteOffMutMemST src srcOff = copyByteOffMutMemST src srcOff . fromUMArrayMBytes
  {-# INLINE copyByteOffMutMemST #-}
  moveByteOffMutMemST src srcOff = moveByteOffMutMemST src srcOff . fromUMArrayMBytes
  {-# INLINE moveByteOffMutMemST #-}
  setByteOffMutMemST = setByteOffMutMemST . fromUMArrayMBytes
  {-# INLINE setByteOffMutMemST #-}
  setMutMemST = setMutMemST . fromUMArrayMBytes
  {-# INLINE setMutMemST #-}


instance MemAlloc (UMArray e) where
  getByteCountMutMemST = getByteCountMutMemST . fromUMArrayMBytes
  {-# INLINE getByteCountMutMemST #-}
  allocMutMemST = fmap toUMArrayMBytes . allocUnpinnedMBytes
  {-# INLINE allocMutMemST #-}
  reallocMutMemST ma = fmap toUMArrayMBytes . reallocMBytes (fromUMArrayMBytes ma)
  {-# INLINE reallocMutMemST #-}




-- | /O(1)/ - Cast an immutable `Data.Text.Array.Array` from @text@ package to immutable `Bytes`
--
-- @since 1.0.0
fromTextArrayBytes :: T.Array -> Bytes 'Inc
fromTextArrayBytes (T.Array ba#) = Bytes ba#
{-# INLINE fromTextArrayBytes #-}

-- | /O(1)/ - Cast immutable `Bytes` to an immutable `Data.Text.Array.Array` from @text@ package
--
-- @since 1.0.0
toTextArrayBytes :: Bytes p -> T.Array
toTextArrayBytes (Bytes ba#) = T.Array ba#
{-# INLINE toTextArrayBytes #-}

-- | /O(1)/ - Cast a mutable `Data.Text.Array.MArray` from @text@ package to mutable `MBytes`
--
-- @since 1.0.0
fromTextMArrayMBytes :: T.MArray s -> MBytes 'Inc s
fromTextMArrayMBytes (T.MArray mba#) = MBytes mba#
{-# INLINE fromTextMArrayMBytes #-}

-- | /O(1)/ - Cast mutable `MBytes` to a mutable `Data.Text.Array.MArray` from @text@ package
--
-- @since 1.0.0
toTextMArrayMBytes :: MBytes p s -> T.MArray s
toTextMArrayMBytes (MBytes ba#) = T.MArray ba#
{-# INLINE toTextMArrayMBytes #-}



instance MemRead T.Array where
  accessMem (T.Array ba#) f _ = f ba#
  {-# INLINE accessMem #-}
  isSameMem a1 a2 = isSameMem (fromTextArrayBytes a1) (fromTextArrayBytes a2)
  {-# INLINE isSameMem #-}
  byteCountMem = byteCountMem . fromTextArrayBytes
  {-# INLINE byteCountMem #-}
  indexOffMem = indexOffMem . fromTextArrayBytes
  {-# INLINE indexOffMem #-}
  indexByteOffMem = indexByteOffMem . fromTextArrayBytes
  {-# INLINE indexByteOffMem #-}
  copyByteOffToMBytesMemST = copyByteOffToMBytesMemST . fromTextArrayBytes
  {-# INLINE copyByteOffToMBytesMemST #-}
  copyByteOffToPtrMemST = copyByteOffToPtrMemST . fromTextArrayBytes
  {-# INLINE copyByteOffToPtrMemST #-}
  compareByteOffToPtrMemST = compareByteOffToPtrMemST . fromTextArrayBytes
  {-# INLINE compareByteOffToPtrMemST #-}
  compareByteOffToBytesMem = compareByteOffToBytesMem . fromTextArrayBytes
  {-# INLINE compareByteOffToBytesMem #-}
  compareByteOffMem mem off = compareByteOffMem mem off . fromTextArrayBytes
  {-# INLINE compareByteOffMem #-}


instance MemWrite T.MArray where
  accessMutMemST (T.MArray mba#) f _ = f mba#
  {-# INLINE accessMutMemST #-}
  isSameMutMem ma1 ma2 = isSameMutMem (fromTextMArrayMBytes ma1) (fromTextMArrayMBytes ma2)
  {-# INLINE isSameMutMem #-}
  readOffMutMemST = readOffMBytes . fromTextMArrayMBytes
  {-# INLINE readOffMutMemST #-}
  readByteOffMutMemST = readByteOffMBytes . fromTextMArrayMBytes
  {-# INLINE readByteOffMutMemST #-}
  writeOffMutMemST = writeOffMBytes . fromTextMArrayMBytes
  {-# INLINE writeOffMutMemST #-}
  writeByteOffMutMemST = writeByteOffMBytes . fromTextMArrayMBytes
  {-# INLINE writeByteOffMutMemST #-}
  moveByteOffToPtrMutMemST = moveByteOffMBytesToPtr . fromTextMArrayMBytes
  {-# INLINE moveByteOffToPtrMutMemST #-}
  moveByteOffToMBytesMutMemST = moveByteOffMBytesToMBytes . fromTextMArrayMBytes
  {-# INLINE moveByteOffToMBytesMutMemST #-}
  moveByteOffMutMemST src srcOff = moveByteOffToMBytesMutMemST src srcOff . fromTextMArrayMBytes
  {-# INLINE moveByteOffMutMemST #-}
  copyByteOffMutMemST src srcOff = copyByteOffToMBytesMemST src srcOff . fromTextMArrayMBytes
  {-# INLINE copyByteOffMutMemST #-}
  setByteOffMutMemST = setByteOffMBytes . fromTextMArrayMBytes
  {-# INLINE setByteOffMutMemST #-}
  setMutMemST = setMBytes . fromTextMArrayMBytes
  {-# INLINE setMutMemST #-}


instance MemAlloc T.MArray where
  getByteCountMutMemST = getByteCountMBytes . fromTextMArrayMBytes
  {-# INLINE getByteCountMutMemST #-}
  allocMutMemST = fmap toTextMArrayMBytes . allocUnpinnedMBytes
  {-# INLINE allocMutMemST #-}
  reallocMutMemST m = fmap toTextMArrayMBytes . reallocMBytes (fromTextMArrayMBytes m)
  {-# INLINE reallocMutMemST #-}


instance MemRead T.Text where
  accessMem (T.Text a o _) f g i = accessMem a f g (toByteOff (Off o :: Off Word16) + i)
  {-# INLINE accessMem #-}
  isSameMem (T.Text a1 o1 n1) (T.Text a2 o2 n2) = isSameMem a1 a2 && o1 == o2 && n1 == n2
  {-# INLINE isSameMem #-}
  byteCountMem (T.Text _ _ n) = toByteCount (Count n :: Count Word16)
  {-# INLINE byteCountMem #-}
  indexByteOffMem (T.Text a o _) i = indexByteOffMem a (toByteOff (Off o :: Off Word16) + i)
  {-# INLINE indexByteOffMem #-}
  copyByteOffToMBytesMemST (T.Text a o _) i =
    copyByteOffToMBytesMemST a (toByteOff (Off o :: Off Word16) + i)
  {-# INLINE copyByteOffToMBytesMemST #-}
  copyByteOffToPtrMemST (T.Text a o _) i =
    copyByteOffToPtrMemST a (toByteOff (Off o :: Off Word16) + i)
  {-# INLINE copyByteOffToPtrMemST #-}
  compareByteOffToPtrMemST (T.Text a o _) off =
    compareByteOffToPtrMemST a (toByteOff (Off o :: Off Word16) + off)
  {-# INLINE compareByteOffToPtrMemST #-}
  compareByteOffToBytesMem (T.Text a o _) off =
    compareByteOffToBytesMem a (toByteOff (Off o :: Off Word16) + off)
  {-# INLINE compareByteOffToBytesMem #-}
  compareByteOffMem mem off1 (T.Text a o _) off2 =
    compareByteOffMem mem off1 a (toByteOff (Off o :: Off Word16) + off2)
  {-# INLINE compareByteOffMem #-}



-- bytestring


withPtrByteString :: Primal s m => ByteString -> (Ptr a -> m b) -> m b
#if MIN_VERSION_bytestring(0,11,0)
withPtrByteString (BS (ForeignPtr addr# ptrContents) _) f = do
#else
withPtrByteString (PS (ForeignPtr addr'# ptrContents) (I# o#) _) f = do
  let !addr# = addr'# `plusAddr#` o#
#endif
  r <- f (Ptr addr#)
  r <$ touch ptrContents
{-# INLINE withPtrByteString #-}


instance MemRead BS.ByteString where
  accessMem bs _ g o = unsafeInlineST $ withPtrByteString bs (\(Ptr addr#) -> pure $! g addr# o)
  {-# INLINE accessMem #-}
  isSameMem bs1 bs2 =
    unsafeInlineIO $
    withPtrByteString bs1 $ \ptr1 ->
      withPtrByteString bs2 $ \ptr2 -> -- Can refer to the same memory but sliced differently:
        pure (ptr1 == (ptr2 :: Ptr Word8) && BS.length bs1 == BS.length bs2)
  {-# INLINE isSameMem #-}
  byteCountMem = Count . BS.length
  {-# INLINE byteCountMem #-}
  indexOffMem bs i =
    unsafeInlineST $ withPtrByteString bs (\ptr -> readOffPtr ptr i)
  {-# INLINE indexOffMem #-}
  indexByteOffMem bs i =
    unsafeInlineST $ withPtrByteString bs (\ptr -> readByteOffPtr ptr i)
  {-# INLINE indexByteOffMem #-}
  copyByteOffToMBytesMemST bs srcOff mb dstOff c =
    withPtrByteString bs $ \srcPtr ->
      copyByteOffPtrToMBytes srcPtr srcOff mb dstOff c
  {-# INLINE copyByteOffToMBytesMemST #-}
  copyByteOffToPtrMemST bs srcOff dstPtr dstOff c =
    withPtrByteString bs $ \srcPtr ->
      copyByteOffPtrToPtr srcPtr srcOff dstPtr dstOff c
  {-# INLINE copyByteOffToPtrMemST #-}
  compareByteOffToPtrMemST bs off1 ptr2 off2 c =
    withPtrByteString bs $ \ptr1 ->
      pure $! compareByteOffPtrToPtr ptr1 off1 ptr2 off2 c
  {-# INLINE compareByteOffToPtrMemST #-}
  compareByteOffToBytesMem bs off1 bytes off2 c =
    unsafeInlineST $
    withPtrByteString bs $ \ptr1 ->
      pure $! compareByteOffPtrToBytes ptr1 off1 bytes off2 c
  {-# INLINE compareByteOffToBytesMem #-}
  compareByteOffMem mem1 off1 bs off2 c =
    unsafeInlineST $
    withPtrByteString bs $ \ptr2 -> compareByteOffToPtrMemST mem1 off1 ptr2 off2 c
  {-# INLINE compareByteOffMem #-}



-- | /O(1)/ - Cast an immutable `Bytes` to an immutable `ShortByteString`
--
-- @since 0.1.0
toShortByteStringBytes :: Bytes p -> ShortByteString
toShortByteStringBytes (Bytes ba#) = SBS ba#
{-# INLINE toShortByteStringBytes #-}

-- | /O(1)/ - Cast an immutable  `ShortByteString` to an immutable `Bytes`
--
-- @since 0.1.0
fromShortByteStringBytes :: ShortByteString -> Bytes 'Inc
fromShortByteStringBytes (SBS ba#) = Bytes ba#
{-# INLINE fromShortByteStringBytes #-}

instance MemRead ShortByteString where
  accessMem (SBS ba#) f _ = f ba#
  {-# INLINE accessMem #-}
  isSameMem sbs1 sbs2 = isSameMem (fromShortByteStringBytes sbs1) (fromShortByteStringBytes sbs2)
  {-# INLINE isSameMem #-}
  byteCountMem = byteCountMem . fromShortByteStringBytes
  {-# INLINE byteCountMem #-}
  indexOffMem = indexOffMem . fromShortByteStringBytes
  {-# INLINE indexOffMem #-}
  indexByteOffMem = indexByteOffMem . fromShortByteStringBytes
  {-# INLINE indexByteOffMem #-}
  copyByteOffToMBytesMemST = copyByteOffToMBytesMemST . fromShortByteStringBytes
  {-# INLINE copyByteOffToMBytesMemST #-}
  copyByteOffToPtrMemST = copyByteOffToPtrMemST . fromShortByteStringBytes
  {-# INLINE copyByteOffToPtrMemST #-}
  compareByteOffToPtrMemST = compareByteOffToPtrMemST . fromShortByteStringBytes
  {-# INLINE compareByteOffToPtrMemST #-}
  compareByteOffToBytesMem = compareByteOffToBytesMem . fromShortByteStringBytes
  {-# INLINE compareByteOffToBytesMem #-}
  compareByteOffMem mem off1 = compareByteOffMem mem off1 . fromShortByteStringBytes
  {-# INLINE compareByteOffMem #-}
