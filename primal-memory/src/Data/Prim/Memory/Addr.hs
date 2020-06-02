{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Data.Prim.Memory.Addr
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Memory.Addr
  ( -- * Immutable Addr
    Addr(..)
  , castAddr
  , fromBytesAddr
  , curOffAddr
  , byteCountAddr
  , countAddr
  , plusOffAddr
  , indexAddr
  , indexOffAddr
  , indexByteOffAddr
  , readAddr
  , readOffAddr
  , readByteOffAddr
  , thawAddr
  , freezeMAddr
  , withPtrAddr
  , withAddrAddr#
  , withNoHaltPtrAddr

   -- * Mutable MAddr
  , MAddr(..)
  , castMAddr
  , allocMAddr
  , callocMAddr
  , reallocMAddr
  , shrinkMAddr
  , shrinkByteCountMAddr
  , setMAddr
  , curOffMAddr
  , getByteCountMAddr
  , getCountMAddr
  , plusOffMAddr
  , readMAddr
  , readOffMAddr
  , readByteOffMAddr
  , writeMAddr
  , writeOffMAddr
  , writeByteOffMAddr
  , copyAddrToMAddr
  , moveMAddrToMAddr

  , withPtrMAddr
  , withAddrMAddr#
  , withNoHaltPtrMAddr
  , toForeignPtrAddr
  , toForeignPtrMAddr
  , fromForeignPtrAddr
  , fromForeignPtrMAddr
  -- * Conversion
  -- ** ByteString
  , toByteStringAddr
  , toShortByteStringAddr
  , fromShortByteStringAddr
  , fromByteStringAddr
  , fromByteStringMAddr

  -- * Atomic
  , casOffMAddr
  , casBoolOffMAddr
  , casBoolFetchOffMAddr
  , atomicReadOffMAddr
  , atomicWriteOffMAddr
  , atomicModifyOffMAddr
  , atomicModifyOffMAddr_
  , atomicModifyFetchOldOffMAddr
  , atomicModifyFetchNewOffMAddr
  -- ** Numeric
  , atomicAddFetchOldOffMAddr
  , atomicAddFetchNewOffMAddr
  , atomicSubFetchOldOffMAddr
  , atomicSubFetchNewOffMAddr
  -- ** Binary
  , atomicAndFetchOldOffMAddr
  , atomicAndFetchNewOffMAddr
  , atomicNandFetchOldOffMAddr
  , atomicNandFetchNewOffMAddr
  , atomicOrFetchOldOffMAddr
  , atomicOrFetchNewOffMAddr
  , atomicXorFetchOldOffMAddr
  , atomicXorFetchNewOffMAddr
  , atomicNotFetchOldOffMAddr
  , atomicNotFetchNewOffMAddr
  -- * Prefetch
  -- ** Directly
  , prefetchAddr0
  , prefetchMAddr0
  , prefetchAddr1
  , prefetchMAddr1
  , prefetchAddr2
  , prefetchMAddr2
  , prefetchAddr3
  , prefetchMAddr3
  -- ** With offset
  , prefetchOffAddr0
  , prefetchOffMAddr0
  , prefetchOffAddr1
  , prefetchOffMAddr1
  , prefetchOffAddr2
  , prefetchOffMAddr2
  , prefetchOffAddr3
  , prefetchOffMAddr3
  -- * Re-export
  , module Data.Prim
  ) where

import Control.Arrow (first)
import Control.DeepSeq
import Control.Prim.Monad
import Control.Prim.Monad.Unsafe
import Data.ByteString.Internal
import Data.ByteString.Short.Internal
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Monoid as Monoid
import Data.Prim
import Data.Prim.Atomic
import Data.Prim.Class
import Data.Prim.Memory.Bytes
import Data.Prim.Memory.ByteString
import Data.Prim.Memory.ForeignPtr
import Data.Prim.Memory.Internal
import Data.Prim.Memory.Ptr
import qualified Data.Semigroup as Semigroup
import Foreign.Prim


data Addr e = Addr
  { addrAddr# :: Addr#
  , addrBytes :: {-# UNPACK #-}!(Bytes 'Pin)
  }
type role Addr representational


data MAddr e s = MAddr
  { mAddrAddr#  :: Addr#
  , mAddrMBytes :: {-# UNPACK #-}!(MBytes 'Pin s)
  }
type role MAddr representational nominal



instance Eq (Addr e) where
  a1 == a2 = isSameAddr a1 a2 || eqMem a1 a2

instance (Show e, Prim e) => Show (Addr e) where
  show a = show (toListMem a :: [e])

instance IsString (Addr Char) where
  fromString = fromListMem

instance Prim e => IsList (Addr e) where
  type Item (Addr e) = e
  fromList = fromListMem
  fromListN n = fromListMemN_ (Count n)
  toList = toListMem

instance Semigroup.Semigroup (Addr e) where
  (<>) = appendMem
  sconcat (x :| xs) = concatMem (x:xs)
  stimes i = cycleMemN (fromIntegral i)

instance Monoid.Monoid (Addr e) where
  mappend = appendMem
  mconcat = concatMem
  mempty = emptyMem


castAddr :: Addr e -> Addr b
castAddr (Addr a b) = Addr a b

castMAddr :: MAddr e s -> MAddr b s
castMAddr (MAddr a mb) = MAddr a mb

isSameAddr :: Addr e -> Addr e -> Bool
isSameAddr (Addr a1# _) (Addr a2# _) = isTrue# (a1# `eqAddr#` a2#)

instance NFData (Addr e) where
  rnf (Addr _ _) = ()

instance NFData (MAddr e s) where
  rnf (MAddr _ _) = ()

toBytesAddr :: Addr e -> (Bytes 'Pin, Off Word8)
toBytesAddr addr@(Addr _ b) = (b, curByteOffAddr addr)

fromBytesAddr :: Bytes 'Pin -> Addr e
fromBytesAddr b@(Bytes b#) = Addr (byteArrayContents# b#) b

fromMBytesMAddr :: MBytes 'Pin s -> MAddr e s
fromMBytesMAddr mb =
  case toPtrMBytes mb of
    Ptr addr# -> MAddr addr# mb

allocMAddr :: (MonadPrim s m, Prim e) => Count e -> m (MAddr e s)
allocMAddr c = fromMBytesMAddr <$> allocAlignedMBytes c

callocMAddr :: (MonadPrim s m, Prim e) => Count e -> m (MAddr e s)
callocMAddr c = fromMBytesMAddr <$> callocAlignedMBytes c


-- | Shrink mutable address to new specified size in number of elements. The new count
-- must be less than or equal to the current as reported by `getCountMAddr`.
shrinkMAddr :: (MonadPrim s m, Prim e) => MAddr e s -> Count e -> m ()
shrinkMAddr maddr@(MAddr _ mb) c = shrinkMBytes mb (toByteCount c + coerce (curByteOffMAddr maddr))
{-# INLINE shrinkMAddr #-}

-- | Shrink mutable address to new specified size in bytes. The new count must be less
-- than or equal to the current as reported by `getByteCountMAddr`.
shrinkByteCountMAddr :: MonadPrim s m => MAddr e s -> Count Word8 -> m ()
shrinkByteCountMAddr maddr@(MAddr _ mb) c = shrinkMBytes mb (c + coerce (curByteOffMAddr maddr))
{-# INLINE shrinkByteCountMAddr #-}


reallocMAddr :: (MonadPrim s m, Prim e) => MAddr e s -> Count e -> m (MAddr e s)
reallocMAddr maddr c = do
  oldByteCount <- getByteCountMAddr maddr
  let newByteCount = toByteCount c
  if newByteCount <= oldByteCount
    then maddr <$
         when (newByteCount < oldByteCount) (shrinkByteCountMAddr maddr newByteCount)
    else do
      addr <- freezeMAddr maddr
      maddr' <- allocMAddr newByteCount
      castMAddr maddr' <$
        copyAddrToMAddr (castAddr addr) 0 maddr' 0 oldByteCount
{-# INLINABLE reallocMAddr #-}


plusOffAddr :: Prim e => Addr e -> Off e -> Addr e
plusOffAddr (Addr addr# b) off = Addr (addr# `plusAddr#` fromOff# off) b

plusOffMAddr :: Prim e => MAddr e s -> Off e -> MAddr e s
plusOffMAddr (MAddr addr# mb) off = MAddr (addr# `plusAddr#` fromOff# off) mb

curOffAddr :: Prim e => Addr e -> Off e
curOffAddr a@(Addr addr# b) = offAsProxy a (Ptr addr# `minusOffPtr` toPtrBytes b)

curByteOffAddr :: Addr e -> Off Word8
curByteOffAddr (Addr addr# b) = Ptr addr# `minusByteOffPtr` toPtrBytes b

countAddr ::
     forall e. Prim e
  => Addr e
  -> Count e
countAddr addr@(Addr _ b) = countBytes b - coerce (curOffAddr addr)

byteCountAddr :: Addr e -> Count Word8
byteCountAddr = countAddr . castAddr

getCountMAddr :: (MonadPrim s m, Prim e) => MAddr e s -> m (Count e)
getCountMAddr maddr@(MAddr _ mb) =
  subtract (coerce (curOffMAddr maddr)) <$> getCountMBytes mb

getByteCountMAddr :: MonadPrim s m => MAddr e s -> m (Count Word8)
getByteCountMAddr = getCountMAddr . castMAddr

indexAddr :: Prim e => Addr e -> e
indexAddr addr = indexOffAddr addr 0

indexOffAddr :: Prim e => Addr e -> Off e -> e
indexOffAddr addr off = unsafeInlineIO $ readOffAddr addr off

indexByteOffAddr :: Prim e => Addr e -> Off Word8 -> e
indexByteOffAddr addr off = unsafeInlineIO $ readByteOffAddr addr off

withPtrAddr :: MonadPrim s m => Addr e -> (Ptr e -> m b) -> m b
withPtrAddr addr f = withAddrAddr# addr $ \addr# -> f (Ptr addr#)
{-# INLINE withPtrAddr #-}

withAddrAddr# :: MonadPrim s m => Addr e -> (Addr# -> m b) -> m b
withAddrAddr# (Addr addr# b) f = do
  a <- f addr#
  a <$ touch b
{-# INLINE withAddrAddr# #-}

withNoHaltPtrAddr :: MonadUnliftPrim s m => Addr e -> (Ptr e -> m b) -> m b
withNoHaltPtrAddr (Addr addr# b) f = withUnliftPrim b $ f (Ptr addr#)
{-# INLINE withNoHaltPtrAddr #-}

curOffMAddr :: forall e s . Prim e => MAddr e s -> Off e
curOffMAddr (MAddr addr# mb) = (Ptr addr# :: Ptr e) `minusOffPtr` toPtrMBytes mb

curByteOffMAddr :: forall e s . MAddr e s -> Off Word8
curByteOffMAddr (MAddr addr# mb) = (Ptr addr# :: Ptr e) `minusByteOffPtr` toPtrMBytes mb

withPtrMAddr :: MonadPrim s m => MAddr e s -> (Ptr e -> m b) -> m b
withPtrMAddr maddr f = withAddrMAddr# maddr $ \addr# -> f (Ptr addr#)
{-# INLINE withPtrMAddr #-}



toForeignPtrAddr :: Addr e -> ForeignPtr e
toForeignPtrAddr (Addr addr# (Bytes ba#)) = ForeignPtr addr# (PlainPtr (unsafeCoerce# ba#))


toForeignPtrMAddr :: MAddr e s -> ForeignPtr e
toForeignPtrMAddr (MAddr addr# (MBytes mba#)) = ForeignPtr addr# (PlainPtr (unsafeCoerce# mba#))

-- | Discarding the original `ForeignPtr` will trigger finalizers that were attached to
-- it, because `Addr` does not retain any finalizers. This is a unsafe cast therefore
-- modification of `ForeignPtr` will be reflected in resulting immutable `Addr`. Pointer
-- created with @malloc@ cannot be converted to `Addr` and will result in `Nothing`
--
-- @since 0.1.0
fromForeignPtrAddr :: ForeignPtr e -> Maybe (Addr e)
fromForeignPtrAddr (ForeignPtr addr# c) =
  case c of
    PlainPtr mba#    -> Just (Addr addr# (unsafePerformIO (freezeMBytes (MBytes mba#))))
    MallocPtr mba# _ -> Just (Addr addr# (unsafePerformIO (freezeMBytes (MBytes mba#))))
    _                -> Nothing

-- | Discarding the original ForeignPtr will trigger finalizers that were attached to it,
-- because `MAddr` does not retain any finalizers. Pointer created with @malloc@ cannot be
-- converted to `MAddr` and will result in `Nothing`
--
-- @since 0.1.0
fromForeignPtrMAddr :: ForeignPtr e -> Maybe (MAddr e s)
fromForeignPtrMAddr (ForeignPtr addr# c) =
  case c of
    PlainPtr mba#    -> Just (MAddr addr# (MBytes (unsafeCoerce# mba#)))
    MallocPtr mba# _ -> Just (MAddr addr# (MBytes (unsafeCoerce# mba#)))
    _                -> Nothing



withAddrMAddr# :: MonadPrim s m => MAddr e s -> (Addr# -> m b) -> m b
withAddrMAddr# (MAddr addr# mb) f = do
  a <- f addr#
  a <$ touch mb
{-# INLINE withAddrMAddr# #-}

withNoHaltPtrMAddr :: MonadUnliftPrim s m => MAddr e s -> (Ptr e -> m b) -> m b
withNoHaltPtrMAddr (MAddr addr# mb) f = withUnliftPrim mb $ f (Ptr addr#)
{-# INLINE withNoHaltPtrMAddr #-}



-- | Read-only access, but it is not enforced.
instance PtrAccess s (Addr e) where
  toForeignPtr = pure . toForeignPtrAddr . castAddr
  {-# INLINE toForeignPtr #-}
  withPtrAccess addr = withPtrAddr (castAddr addr)
  {-# INLINE withPtrAccess #-}
  withNoHaltPtrAccess addr = withNoHaltPtrAddr (castAddr addr)
  {-# INLINE withNoHaltPtrAccess #-}

instance PtrAccess s (MAddr e s) where
  toForeignPtr = pure . toForeignPtrMAddr . castMAddr
  {-# INLINE toForeignPtr #-}
  withPtrAccess maddr = withPtrMAddr (castMAddr maddr)
  {-# INLINE withPtrAccess #-}
  withNoHaltPtrAccess maddr = withNoHaltPtrMAddr (castMAddr maddr)
  {-# INLINE withNoHaltPtrAccess #-}



instance MemAlloc (MAddr e) where
  type FrozenMem (MAddr e) = Addr e

  getByteCountMem = getByteCountMAddr
  {-# INLINE getByteCountMem #-}
  allocByteCountMem = fmap castMAddr . allocMAddr
  {-# INLINE allocByteCountMem #-}
  thawMem = thawAddr
  {-# INLINE thawMem #-}
  freezeMem = freezeMAddr
  {-# INLINE freezeMem #-}
  resizeMem maddr = fmap castMAddr . reallocMAddr (castMAddr maddr)
  {-# INLINE resizeMem #-}


instance MemRead (Addr e) where
  byteCountMem = byteCountAddr
  {-# INLINE byteCountMem #-}
  indexOffMem a i = unsafeInlineIO $ withAddrAddr# a $ \addr# -> readOffPtr (Ptr addr#) i
  {-# INLINE indexOffMem #-}
  indexByteOffMem a i = unsafeInlineIO $ withAddrAddr# a $ \addr# -> readByteOffPtr (Ptr addr#) i
  {-# INLINE indexByteOffMem #-}
  copyByteOffToMBytesMem a si mb di c =
    withPtrAddr a $ \ptr -> copyByteOffPtrToMBytes (castPtr ptr) si mb di c
  {-# INLINE copyByteOffToMBytesMem #-}
  copyByteOffToPtrMem a si mb di c =
    withPtrAddr a $ \ptr -> copyByteOffPtrToPtr (castPtr ptr) si mb di c
  {-# INLINE copyByteOffToPtrMem #-}
  compareByteOffToPtrMem addr off1 ptr2 off2 c =
    withPtrAccess addr $ \ptr1 -> pure $ compareByteOffPtrToPtr ptr1 off1 ptr2 off2 c
  {-# INLINE compareByteOffToPtrMem #-}
  compareByteOffToBytesMem addr off1 bytes off2 c =
    withPtrAccess addr $ \ptr1 -> pure $ compareByteOffPtrToBytes ptr1 off1 bytes off2 c
  {-# INLINE compareByteOffToBytesMem #-}
  compareByteOffMem mem1 off1 addr off2 c =
    unsafeInlineIO $ withPtrAccess addr $ \ptr2 -> compareByteOffToPtrMem mem1 off1 ptr2 off2 c
  {-# INLINE compareByteOffMem #-}

instance MemWrite (MAddr e) where
  readOffMem a = readOffMAddr (castMAddr a)
  {-# INLINE readOffMem #-}
  readByteOffMem a = readByteOffMAddr (castMAddr a)
  {-# INLINE readByteOffMem #-}
  writeOffMem a = writeOffMAddr (castMAddr a)
  {-# INLINE writeOffMem #-}
  writeByteOffMem a = writeByteOffMAddr (castMAddr a)
  {-# INLINE writeByteOffMem #-}
  moveByteOffToPtrMem src srcOff dstPtr dstOff c =
    withAddrMAddr# src $ \ srcAddr# ->
      moveByteOffPtrToPtr (Ptr srcAddr#) srcOff dstPtr dstOff c
  {-# INLINE moveByteOffToPtrMem #-}
  moveByteOffToMBytesMem src srcOff dst dstOff c =
    withAddrMAddr# src $ \ srcAddr# ->
      moveByteOffPtrToMBytes (Ptr srcAddr#) srcOff dst dstOff c
  {-# INLINE moveByteOffToMBytesMem #-}
  copyByteOffMem src srcOff dst dstOff c =
    withAddrMAddr# dst $ \ dstAddr# ->
      copyByteOffToPtrMem src srcOff (Ptr dstAddr#) dstOff c
  {-# INLINE copyByteOffMem #-}
  moveByteOffMem src srcOff dst dstOff c =
    withAddrMAddr# dst $ \ dstAddr# ->
      moveByteOffToPtrMem src srcOff (Ptr dstAddr#) dstOff c
  {-# INLINE moveByteOffMem #-}
  setMem maddr = setMAddr (castMAddr maddr)
  {-# INLINE setMem #-}



thawAddr :: MonadPrim s m => Addr e -> m (MAddr e s)
thawAddr (Addr addr# b) = MAddr addr# <$> thawBytes b
{-# INLINE thawAddr #-}

freezeMAddr :: MonadPrim s m => MAddr e s -> m (Addr e)
freezeMAddr (MAddr addr# mb) = Addr addr# <$> freezeMBytes mb
{-# INLINE freezeMAddr #-}


readAddr :: (MonadPrim s m, Prim e) => Addr e -> m e
readAddr addr = readOffAddr addr 0
{-# INLINE readAddr #-}

readOffAddr :: (MonadPrim s m, Prim e) => Addr e -> Off e -> m e
readOffAddr (Addr addr# b) (Off (I# off#)) = do
  -- TODO: benchmark and see if `readOffAddr` is faster here
  a <- prim (seq# (indexOffAddr# addr# off#))
  a <$ touch b
{-# INLINE readOffAddr #-}

readByteOffAddr :: (MonadPrim s m, Prim e) => Addr e -> Off Word8 -> m e
readByteOffAddr (Addr addr# b) (Off (I# off#)) = do
  a <- prim (seq# (indexOffAddr# (addr# `plusAddr#` off#) 0#))
  a <$ touch b
{-# INLINE readByteOffAddr #-}


readMAddr :: (MonadPrim s m, Prim e) => MAddr e s -> m e
readMAddr maddr = readOffMAddr maddr 0
{-# INLINE readMAddr #-}

readOffMAddr :: (MonadPrim s m, Prim e) => MAddr e s -> Off e -> m e
readOffMAddr (MAddr addr# mb) (Off (I# off#)) = do
  a <- prim (readOffAddr# addr# off#)
  a <$ touch mb
{-# INLINE readOffMAddr #-}

readByteOffMAddr :: (MonadPrim s m, Prim e) => MAddr e s -> Off Word8 -> m e
readByteOffMAddr (MAddr addr# mb) (Off (I# off#)) = do
  a <- prim (readOffAddr# (addr# `plusAddr#` off#) 0#)
  a <$ touch mb
{-# INLINE readByteOffMAddr #-}

writeMAddr :: (MonadPrim s m, Prim e) => MAddr e s -> e -> m ()
writeMAddr maddr = writeOffMAddr maddr 0
{-# INLINE writeMAddr #-}

writeOffMAddr :: (MonadPrim s m, Prim e) => MAddr e s -> Off e -> e -> m ()
writeOffMAddr (MAddr addr# mb) (Off (I# off#)) a =
  prim_ (writeOffAddr# addr# off# a) >> touch mb
{-# INLINE writeOffMAddr #-}

writeByteOffMAddr :: (MonadPrim s m, Prim e) => MAddr e s -> Off Word8 -> e -> m ()
writeByteOffMAddr (MAddr addr# mb) (Off (I# off#)) a =
  prim_ (writeOffAddr# (addr# `plusAddr#` off#) 0# a) >> touch mb
{-# INLINE writeByteOffMAddr #-}


copyAddrToMAddr ::
     (MonadPrim s m, Prim e) => Addr e -> Off e -> MAddr e s -> Off e -> Count e -> m ()
copyAddrToMAddr src srcOff dst dstOff c =
  withPtrAddr src $ \ srcPtr ->
    withPtrMAddr dst $ \ dstPtr ->
      copyPtrToPtr srcPtr srcOff dstPtr dstOff c
{-# INLINE copyAddrToMAddr #-}

moveMAddrToMAddr ::
     (MonadPrim s m, Prim e) => MAddr e s -> Off e -> MAddr e s -> Off e -> Count e -> m ()
moveMAddrToMAddr src srcOff dst dstOff c =
  withPtrMAddr src $ \ srcPtr ->
    withPtrMAddr dst $ \ dstPtr ->
      movePtrToPtr srcPtr srcOff dstPtr dstOff c
{-# INLINE moveMAddrToMAddr #-}

setMAddr :: (MonadPrim s m, Prim e) => MAddr e s -> Off e -> Count e -> e -> m ()
setMAddr (MAddr addr# mb) (Off (I# off#)) (Count (I# n#)) a =
  prim_ (setOffAddr# addr# off# n# a) >> touch mb
{-# INLINE setMAddr #-}



-- | /O(1)/ - Cast an immutable `Addr` to an immutable `ByteString`
--
-- @since 0.1.0
toByteStringAddr :: Addr Word8 -> ByteString
toByteStringAddr addr = PS (toForeignPtrAddr addr) 0 (unCount (countAddr addr))

-- | /O(1)/ - Cast an immutable `Addr` to an immutable `ShortByteString`
--
-- @since 0.1.0
toShortByteStringAddr :: Addr Word8 -> (ShortByteString, Off Word8)
toShortByteStringAddr = first toShortByteStringBytes . toBytesAddr

-- | /O(1)/ - Cast an immutable `ShortByteString` to an immutable `Addr`. In a most common
-- case when `ShortByteString` is not backed by pinned memory, this function will return
-- `Nothing`.
--
-- @since 0.1.0
fromShortByteStringAddr :: ShortByteString -> Addr Word8
fromShortByteStringAddr = fromBytesAddr . ensurePinnedBytes . fromShortByteStringBytes

-- | /O(1)/ - Cast an immutable `ByteString` to `Addr`. Also returns the original length of
-- ByteString, which will be less or equal to `countOfAddr` in the produced `Addr`.
--
-- @since 0.1.0
fromByteStringAddr :: ByteString -> (Addr Word8, Count Word8)
fromByteStringAddr (PS fptr i n) =
  case fromForeignPtrAddr fptr of
    Just addr -> (addr `plusOffAddr` Off i, Count n)
    Nothing -> byteStringConvertError "It was allocated outside of 'bytestring' package"

-- | /O(1)/ - Cast an immutable `ByteString` to a mutable `MAddr`. Also returns the
-- original length of ByteString, which will be less or equal to `getCountOfMAddr` in the
-- produced `MAddr`.
--
-- __Unsafe__ - Further modification of `MAddr` will affect the source `ByteString`
--
-- @since 0.1.0
fromByteStringMAddr :: ByteString -> (MAddr Word8 s, Count Word8)
fromByteStringMAddr (PS fptr i n) =
  case fromForeignPtrMAddr fptr of
    Just maddr -> (maddr `plusOffMAddr` Off i, Count n)
    Nothing -> byteStringConvertError "It was allocated outside of 'bytestring' package"



-- | Perform atomic modification of an element in the `MAddr` at the supplied
-- index. Returns the artifact of computation @__b__@.  Offset is in number of elements,
-- rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
casOffMAddr ::
     (MonadPrim s m, Atomic e)
  => MAddr e s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__e__@, rather than bytes.
  -> e -- ^ Expected old value
  -> e -- ^ New value
  -> m e
casOffMAddr maddr (Off (I# i#)) old new =
  withAddrMAddr# maddr $ \ addr# -> prim $ casOffAddr# addr# i# old new
{-# INLINE casOffMAddr #-}


-- | Perform atomic modification of an element in the `MAddr` at the supplied
-- index. Returns `True` if swap was successfull and false otherwise.  Offset is in number
-- of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
casBoolOffMAddr ::
     (MonadPrim s m, Atomic e)
  => MAddr e s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__e__@, rather than bytes.
  -> e -- ^ Expected old value
  -> e -- ^ New value
  -> m Bool
casBoolOffMAddr maddr (Off (I# i#)) old new =
  withAddrMAddr# maddr $ \ addr# -> prim $ casBoolOffAddr# addr# i# old new
{-# INLINE casBoolOffMAddr #-}

-- | Just like `casBoolOffMAddr`, but also returns the actual value, which will match the
-- supplied expected value if the returned flag is `True`
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
casBoolFetchOffMAddr ::
     (MonadPrim s m, Atomic e)
  => MAddr e s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__e__@, rather than bytes.
  -> e -- ^ Expected old value
  -> e -- ^ New value
  -> m (Bool, e)
casBoolFetchOffMAddr maddr (Off (I# i#)) expected new = do
  withAddrMAddr# maddr $ \addr# ->
    prim $ \s ->
      case casBoolOffAddr# addr# i# expected new s of
        (# s', isCasSucc #)
          | isCasSucc -> (# s', (True, new) #)
          | otherwise ->
            case readOffAddr# addr# i# s' of
              (# s'', actual #) -> (# s'', (False, actual) #)
{-# INLINE casBoolFetchOffMAddr #-}


-- | Perform atomic read of an element in the `MAddr` at the supplied offset. Offset is in
-- number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicReadOffMAddr ::
     (MonadPrim s m, Atomic e)
  => MAddr e s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__e__@, rather than bytes.
  -> m e
atomicReadOffMAddr maddr (Off (I# i#)) =
  withAddrMAddr# maddr $ \ addr# -> prim $ atomicReadOffAddr# addr# i#
{-# INLINE atomicReadOffMAddr #-}

-- | Perform atomic write of an element in the `MAddr` at the supplied offset. Offset is in
-- number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicWriteOffMAddr ::
     (MonadPrim s m, Atomic e)
  => MAddr e s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__e__@, rather than bytes.
  -> e
  -> m ()
atomicWriteOffMAddr maddr (Off (I# i#)) e =
  withAddrMAddr# maddr $ \ addr# -> prim_ $ atomicWriteOffAddr# addr# i# e
{-# INLINE atomicWriteOffMAddr #-}


-- | Perform atomic modification of an element in the `MAddr` at the supplied
-- index. Returns the artifact of computation @__b__@.  Offset is in number of elements,
-- rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicModifyOffMAddr ::
     (MonadPrim s m, Atomic e)
  => MAddr e s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__e__@, rather than bytes.
  -> (e -> (e, b)) -- ^ Function that is applied to the old value and returns new value
                   -- and some artifact of computation @__b__@
  -> m b
atomicModifyOffMAddr maddr (Off (I# i#)) f =
  withAddrMAddr# maddr $ \ addr# -> prim $
  atomicModifyOffAddr# addr# i# $ \a ->
    case f a of
      (a', b) -> (# a', b #)
{-# INLINE atomicModifyOffMAddr #-}

-- | Perform atomic modification of an element in the `MAddr` at the supplied
-- index.  Offset is in number of elements, rather than bytes. Implies a full memory
-- barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicModifyOffMAddr_ ::
     (MonadPrim s m, Atomic e)
  => MAddr e s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__e__@, rather than bytes.
  -> (e -> e) -- ^ Function that is applied to the current value
  -> m ()
atomicModifyOffMAddr_ maddr (Off (I# i#)) f =
  withAddrMAddr# maddr $ \ addr# -> prim_ $ atomicModifyOffAddr_# addr# i# f
{-# INLINE atomicModifyOffMAddr_ #-}


-- | Perform atomic modification of an element in the `MAddr` at the supplied
-- index. Returns the previous value.  Offset is in number of elements, rather than
-- bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicModifyFetchOldOffMAddr ::
     (MonadPrim s m, Atomic e)
  => MAddr e s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__e__@, rather than bytes.
  -> (e -> e) -- ^ Function that is applied to the old value
  -> m e -- ^ Returns the old value
atomicModifyFetchOldOffMAddr maddr (Off (I# i#)) f =
  withAddrMAddr# maddr $ \ addr# -> prim $ atomicModifyFetchOldOffAddr# addr# i# f
{-# INLINE atomicModifyFetchOldOffMAddr #-}


-- | Perform atomic modification of an element in the `MAddr` at the supplied
-- index.  Offset is in number of elements, rather than bytes. Implies a full memory
-- barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicModifyFetchNewOffMAddr ::
     (MonadPrim s m, Atomic e)
  => MAddr e s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__e__@, rather than bytes
  -> (e -> e) -- ^ Function that is applied to the old value
  -> m e -- ^ Returns the new value
atomicModifyFetchNewOffMAddr maddr (Off (I# i#)) f =
  withAddrMAddr# maddr $ \ addr# -> prim $ atomicModifyFetchNewOffAddr# addr# i# f
{-# INLINE atomicModifyFetchNewOffMAddr #-}



-- | Add a numeric value to an element of a `MAddr`, corresponds to @(`+`)@ done
-- atomically. Returns the previous value.  Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicAddFetchOldOffMAddr ::
     (MonadPrim s m, AtomicCount e)
  => MAddr e s
  -> Off e
  -> e
  -> m e
atomicAddFetchOldOffMAddr maddr (Off (I# i#)) a =
  withAddrMAddr# maddr $ \ addr# -> prim $ atomicAddFetchOldOffAddr# addr# i# a
{-# INLINE atomicAddFetchOldOffMAddr #-}

-- | Add a numeric value to an element of a `MAddr`, corresponds to @(`+`)@ done
-- atomically. Returns the new value.  Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicAddFetchNewOffMAddr ::
     (MonadPrim s m, AtomicCount e)
  => MAddr e s
  -> Off e
  -> e
  -> m e
atomicAddFetchNewOffMAddr maddr (Off (I# i#)) a =
  withAddrMAddr# maddr $ \ addr# -> prim $ atomicAddFetchNewOffAddr# addr# i# a
{-# INLINE atomicAddFetchNewOffMAddr #-}



-- | Subtract a numeric value from an element of a `MAddr`, corresponds to
-- @(`-`)@ done atomically. Returns the previous value.  Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicSubFetchOldOffMAddr ::
     (MonadPrim s m, AtomicCount e)
  => MAddr e s
  -> Off e
  -> e
  -> m e
atomicSubFetchOldOffMAddr maddr (Off (I# i#)) a =
  withAddrMAddr# maddr $ \ addr# -> prim $ atomicSubFetchOldOffAddr# addr# i# a
{-# INLINE atomicSubFetchOldOffMAddr #-}

-- | Subtract a numeric value from an element of a `MAddr`, corresponds to
-- @(`-`)@ done atomically. Returns the new value. Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicSubFetchNewOffMAddr ::
     (MonadPrim s m, AtomicCount e)
  => MAddr e s
  -> Off e
  -> e
  -> m e
atomicSubFetchNewOffMAddr maddr (Off (I# i#)) a =
  withAddrMAddr# maddr $ \ addr# -> prim $ atomicSubFetchNewOffAddr# addr# i# a
{-# INLINE atomicSubFetchNewOffMAddr #-}



-- | Binary conjunction (AND) of an element of a `MAddr` with the supplied value,
-- corresponds to @(`Data.Bits..&.`)@ done atomically. Returns the previous value. Offset
-- is in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicAndFetchOldOffMAddr ::
     (MonadPrim s m, AtomicBits e)
  => MAddr e s
  -> Off e
  -> e
  -> m e
atomicAndFetchOldOffMAddr maddr (Off (I# i#)) a =
  withAddrMAddr# maddr $ \ addr# -> prim $ atomicAndFetchOldOffAddr# addr# i# a
{-# INLINE atomicAndFetchOldOffMAddr #-}

-- | Binary conjunction (AND) of an element of a `MAddr` with the supplied value,
-- corresponds to @(`Data.Bits..&.`)@ done atomically. Returns the new value. Offset is
-- in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicAndFetchNewOffMAddr ::
     (MonadPrim s m, AtomicBits e)
  => MAddr e s
  -> Off e
  -> e
  -> m e
atomicAndFetchNewOffMAddr maddr (Off (I# i#)) a =
  withAddrMAddr# maddr $ \ addr# -> prim $ atomicAndFetchNewOffAddr# addr# i# a
{-# INLINE atomicAndFetchNewOffMAddr #-}



-- | Negation of binary conjunction (NAND) of an element of a `MAddr` with the
-- supplied value, corresponds to @\\x y -> `Data.Bits.complement` (x `Data.Bits..&.` y)@
-- done atomically. Returns the previous value. Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicNandFetchOldOffMAddr ::
     (MonadPrim s m, AtomicBits e)
  => MAddr e s
  -> Off e
  -> e
  -> m e
atomicNandFetchOldOffMAddr maddr (Off (I# i#)) a =
  withAddrMAddr# maddr $ \ addr# -> prim $ atomicNandFetchOldOffAddr# addr# i# a
{-# INLINE atomicNandFetchOldOffMAddr #-}

-- | Negation of binary conjunction (NAND)  of an element of a `MAddr` with the supplied
-- value, corresponds to @\\x y -> `Data.Bits.complement` (x `Data.Bits..&.` y)@ done
-- atomically. Returns the new value. Offset is in number of elements, rather than
-- bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicNandFetchNewOffMAddr ::
     (MonadPrim s m, AtomicBits e)
  => MAddr e s
  -> Off e
  -> e
  -> m e
atomicNandFetchNewOffMAddr maddr (Off (I# i#)) a =
  withAddrMAddr# maddr $ \ addr# -> prim $ atomicNandFetchNewOffAddr# addr# i# a
{-# INLINE atomicNandFetchNewOffMAddr #-}




-- | Binary disjunction (OR) of an element of a `MAddr` with the supplied value,
-- corresponds to @(`Data.Bits..|.`)@ done atomically. Returns the previous value. Offset
-- is in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicOrFetchOldOffMAddr ::
     (MonadPrim s m, AtomicBits e)
  => MAddr e s
  -> Off e
  -> e
  -> m e
atomicOrFetchOldOffMAddr maddr (Off (I# i#)) a =
  withAddrMAddr# maddr $ \ addr# -> prim $ atomicOrFetchOldOffAddr# addr# i# a
{-# INLINE atomicOrFetchOldOffMAddr #-}

-- | Binary disjunction (OR) of an element of a `MAddr` with the supplied value,
-- corresponds to @(`Data.Bits..|.`)@ done atomically. Returns the new value. Offset is
-- in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicOrFetchNewOffMAddr ::
     (MonadPrim s m, AtomicBits e)
  => MAddr e s
  -> Off e
  -> e
  -> m e
atomicOrFetchNewOffMAddr maddr (Off (I# i#)) a =
  withAddrMAddr# maddr $ \ addr# -> prim $ atomicOrFetchNewOffAddr# addr# i# a
{-# INLINE atomicOrFetchNewOffMAddr #-}



-- | Binary exclusive disjunction (XOR) of an element of a `MAddr` with the supplied value,
-- corresponds to @`Data.Bits.xor`@ done atomically. Returns the previous value. Offset
-- is in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicXorFetchOldOffMAddr ::
     (MonadPrim s m, AtomicBits e)
  => MAddr e s
  -> Off e
  -> e
  -> m e
atomicXorFetchOldOffMAddr maddr (Off (I# i#)) a =
  withAddrMAddr# maddr $ \ addr# -> prim $ atomicXorFetchOldOffAddr# addr# i# a
{-# INLINE atomicXorFetchOldOffMAddr #-}

-- | Binary exclusive disjunction (XOR) of an element of a `MAddr` with the supplied value,
-- corresponds to @`Data.Bits.xor`@ done atomically. Returns the new value. Offset is
-- in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicXorFetchNewOffMAddr ::
     (MonadPrim s m, AtomicBits e)
  => MAddr e s
  -> Off e
  -> e
  -> m e
atomicXorFetchNewOffMAddr maddr (Off (I# i#)) a =
  withAddrMAddr# maddr $ \ addr# -> prim $ atomicXorFetchNewOffAddr# addr# i# a
{-# INLINE atomicXorFetchNewOffMAddr #-}





-- | Binary negation (NOT) of an element of a `MAddr`, corresponds to
-- @(`Data.Bits.complement`)@ done atomically. Returns the previous value. Offset is in
-- number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicNotFetchOldOffMAddr ::
     (MonadPrim s m, AtomicBits e)
  => MAddr e s
  -> Off e
  -> m e
atomicNotFetchOldOffMAddr maddr (Off (I# i#)) =
  withAddrMAddr# maddr $ \ addr# -> prim $ atomicNotFetchOldOffAddr# addr# i#
{-# INLINE atomicNotFetchOldOffMAddr #-}

-- | Binary negation (NOT) of an element of a `MAddr`, corresponds to
-- @(`Data.Bits.complement`)@ done atomically. Returns the new value. Offset is in number
-- of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicNotFetchNewOffMAddr ::
     (MonadPrim s m, AtomicBits e)
  => MAddr e s
  -> Off e
  -> m e
atomicNotFetchNewOffMAddr maddr (Off (I# i#)) =
  withAddrMAddr# maddr $ \ addr# -> prim $ atomicNotFetchNewOffAddr# addr# i#
{-# INLINE atomicNotFetchNewOffMAddr #-}




prefetchAddr0 :: MonadPrim s m => Addr e -> m ()
prefetchAddr0 (Addr addr# _) = prim_ (prefetchAddr0# addr# 0#)
{-# INLINE prefetchAddr0 #-}

prefetchMAddr0 :: MonadPrim s m => MAddr e s -> m ()
prefetchMAddr0 (MAddr maddr# _) = prim_ (prefetchAddr0# maddr# 0#)
{-# INLINE prefetchMAddr0 #-}

prefetchAddr1 :: MonadPrim s m => Addr e -> m ()
prefetchAddr1 (Addr addr# _) = prim_ (prefetchAddr1# addr# 0#)
{-# INLINE prefetchAddr1 #-}

prefetchMAddr1 :: MonadPrim s m => MAddr e s -> m ()
prefetchMAddr1 (MAddr maddr# _) = prim_ (prefetchAddr1# maddr# 0#)
{-# INLINE prefetchMAddr1 #-}

prefetchAddr2 :: MonadPrim s m => Addr e -> m ()
prefetchAddr2 (Addr addr# _) = prim_ (prefetchAddr2# addr# 0#)
{-# INLINE prefetchAddr2 #-}

prefetchMAddr2 :: MonadPrim s m => MAddr e s -> m ()
prefetchMAddr2 (MAddr maddr# _) = prim_ (prefetchAddr2# maddr# 0#)
{-# INLINE prefetchMAddr2 #-}

prefetchAddr3 :: MonadPrim s m => Addr e -> m ()
prefetchAddr3 (Addr addr# _) = prim_ (prefetchAddr3# addr# 0#)
{-# INLINE prefetchAddr3 #-}

prefetchMAddr3 :: MonadPrim s m => MAddr e s -> m ()
prefetchMAddr3 (MAddr maddr# _) = prim_ (prefetchAddr3# maddr# 0#)
{-# INLINE prefetchMAddr3 #-}


prefetchOffAddr0 :: (MonadPrim s m, Prim e) => Addr e -> Off e -> m ()
prefetchOffAddr0 (Addr addr# _) off = prim_ (prefetchAddr0# addr# (fromOff# off))
{-# INLINE prefetchOffAddr0 #-}

prefetchOffMAddr0 :: (MonadPrim s m, Prim e) => MAddr e s -> Off e -> m ()
prefetchOffMAddr0 (MAddr maddr# _) off = prim_ (prefetchAddr0# maddr# (fromOff# off))
{-# INLINE prefetchOffMAddr0 #-}

prefetchOffAddr1 :: (MonadPrim s m, Prim e) => Addr e -> Off e -> m ()
prefetchOffAddr1 (Addr addr# _) off = prim_ (prefetchAddr1# addr# (fromOff# off))
{-# INLINE prefetchOffAddr1 #-}

prefetchOffMAddr1 :: (MonadPrim s m, Prim e) => MAddr e s -> Off e -> m ()
prefetchOffMAddr1 (MAddr maddr# _) off = prim_ (prefetchAddr1# maddr# (fromOff# off))
{-# INLINE prefetchOffMAddr1 #-}

prefetchOffAddr2 :: (MonadPrim s m, Prim e) => Addr e -> Off e -> m ()
prefetchOffAddr2 (Addr addr# _) off = prim_ (prefetchAddr2# addr# (fromOff# off))
{-# INLINE prefetchOffAddr2 #-}

prefetchOffMAddr2 :: (MonadPrim s m, Prim e) => MAddr e s -> Off e -> m ()
prefetchOffMAddr2 (MAddr maddr# _) off = prim_ (prefetchAddr2# maddr# (fromOff# off))
{-# INLINE prefetchOffMAddr2 #-}

prefetchOffAddr3 :: (MonadPrim s m, Prim e) => Addr e -> Off e -> m ()
prefetchOffAddr3 (Addr addr# _) off = prim_ (prefetchAddr3# addr# (fromOff# off))
{-# INLINE prefetchOffAddr3 #-}

prefetchOffMAddr3 :: (MonadPrim s m, Prim e) => MAddr e s -> Off e -> m ()
prefetchOffMAddr3 (MAddr maddr# _) off = prim_ (prefetchAddr3# maddr# (fromOff# off))
{-# INLINE prefetchOffMAddr3 #-}
