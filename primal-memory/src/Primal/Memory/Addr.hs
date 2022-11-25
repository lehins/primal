{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Primal.Memory.Addr
-- Copyright   : (c) Alexey Kuleshevich 2020-2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Memory.Addr
  ( -- * Immutable Addr
    Addr(..)
  , emptyAddr
  , isSameAddr
  , castAddr
  , fromBytesAddr
  , curOffAddr
  , byteCountAddr
  , countAddr
  , plusOffAddr
  , plusByteOffAddr
  , minusCountAddr
  , minusByteCountAddr
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
  , isSameMAddr
  , castMAddr
  , singletonMAddr
  , allocMAddr
  , allocAlignedMAddr
  , allocZeroMAddr
  , allocZeroAlignedMAddr
  , reallocMAddr
  , shrinkMAddr
  , shrinkByteCountMAddr
  , setMAddr
  , setOffMAddr
  , setByteOffMAddr
  , curOffMAddr
  , getByteCountMAddr
  , getCountMAddr
  , plusOffMAddr
  , plusByteOffMAddr
  -- TODO:
  -- , minusCountMAddr
  -- , minusByteCountMAddr
  , readMAddr
  , readOffMAddr
  , readByteOffMAddr
  , writeMAddr
  , writeOffMAddr
  , writeByteOffMAddr
  , copyAddrToMAddr
  , moveMAddrToMAddr


  , modifyMAddr
  , modifyMAddr_
  , modifyFetchOldMAddr
  , modifyFetchNewMAddr
  , modifyMAddrM
  , modifyMAddrM_
  , modifyFetchOldMAddrM
  , modifyFetchNewMAddrM
  , swapMAddrs_
  , swapMAddrs

  , withPtrMAddr
  , withAddrMAddr#
  , withNoHaltPtrMAddr
  , toForeignPtrAddr
  , toMForeignPtrMAddr
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
  -- * Re-export
  , module Primal.Element.Unbox
  ) where

import Control.Arrow (first)
import Data.ByteString.Internal
import Data.ByteString.Short.Internal
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Monoid as Monoid
import qualified Data.Semigroup as Semigroup
import Primal.Array
import Primal.Eval
import Primal.Foreign
import Primal.Memory.ByteString
import Primal.Memory.Bytes
import Primal.Memory.Bytes.Internal
import Primal.Memory.Fold
import Primal.Memory.ForeignPtr
import Primal.Memory.Internal
import Primal.Memory.Ptr
import Primal.Monad
import Primal.Monad.Unsafe
import Primal.Mutable.Eq
import Primal.Mutable.Freeze
import Primal.Mutable.Ord
import Primal.Element.Unbox
import Primal.Element.Unbox.Atomic
import Unsafe.Coerce


-- | Immutable read-only address
data Addr e = Addr
  { aAddr# :: Addr#
  , aBytes :: Bytes 'Pin
  }
type role Addr nominal

-- | Mutable address
data MAddr e s = MAddr
  { maAddr#  :: Addr#
  , maMBytes :: MBytes 'Pin s
  }
type role MAddr nominal nominal


instance (Eq e, Unbox e) => Eq (Addr e) where
  (==) = eqMem @e
  {-# INLINE (==) #-}

instance (Unbox e, Eq e) => MutEq (MAddr e) where
  eqMutST m1 m2 = eqWithST isSameMAddr getSizeOfMAddr (\m -> readOffMAddr m . coerce) m1 m2
  {-# INLINE eqMutST #-}

instance (Unbox e, Ord e) => Ord (Addr e) where
  compare = compareMem @e
  {-# INLINE compare #-}

instance (Unbox e, Ord e) => MutOrd (MAddr e) where
  compareMutST m1 m2 = compareWithST isSameMAddr getSizeOfMAddr (\m -> readOffMAddr m . coerce) m1 m2
  {-# INLINE compareMutST #-}

instance (Show e, Unbox e) => Show (Addr e) where
  show a = show (toListMem a :: [e])

instance IsString (Addr Char) where
  fromString = fromListMem
  {-# INLINE fromString #-}

instance Unbox e => IsList (Addr e) where
  type Item (Addr e) = e
  fromList = fromListMem
  {-# INLINE fromList #-}
  fromListN n = fromListZeroMemN_ (Count n)
  {-# INLINE fromListN #-}
  toList = toListMem
  {-# INLINE toList #-}

instance Unbox e => Semigroup.Semigroup (Addr e) where
  (<>) = appendMem
  {-# INLINE (<>) #-}
  sconcat (x :| xs) = concatMem (x:xs)
  {-# INLINE sconcat #-}
  stimes i = cycleMemN (fromIntegral i)
  {-# INLINE stimes #-}

instance Unbox e => Monoid.Monoid (Addr e) where
  mappend = appendMem
  {-# INLINE mappend #-}
  mconcat = concatMem
  {-# INLINE mconcat #-}
  mempty = emptyAddr
  {-# INLINE mempty #-}

type instance Frozen (MAddr e) = Addr e

instance MutFreeze (MAddr e) where
  thawST = thawAddr
  {-# INLINE thawST #-}
  thawCloneST addr = do
    let bc = byteCountAddr addr
    maddr <- allocMAddr bc
    castMAddr maddr <$ copyAddrToMAddr (castAddr addr) 0 maddr 0 bc
  {-# INLINE thawCloneST #-}
  freezeMutST = freezeMAddr
  {-# INLINE freezeMutST #-}

emptyAddr :: Addr e
emptyAddr = fromBytesAddr emptyBytes
{-# INLINE emptyAddr #-}

castAddr :: Addr e -> Addr b
castAddr (Addr a b) = Addr a b
{-# INLINE castAddr #-}

castMAddr :: MAddr e s -> MAddr b s
castMAddr (MAddr a mb) = MAddr a mb
{-# INLINE castMAddr #-}

castStateMAddr :: MAddr e s' -> MAddr b s
castStateMAddr = unsafeCoerce

isSameAddr :: Addr e -> Addr e -> Bool
isSameAddr (Addr a1# _) (Addr a2# _) =
  isTrue# (a1# `eqAddr#` a2#)
{-# INLINE isSameAddr #-}

isSameMAddr :: MAddr e s -> MAddr e s -> Bool
isSameMAddr (MAddr a1# _) (MAddr a2# _) = isTrue# (a1# `eqAddr#` a2#)
{-# INLINE isSameMAddr #-}

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
{-# INLINE fromMBytesMAddr #-}

singletonMAddr :: forall e m s. (Primal s m, Unbox e) => e -> m (MAddr e s)
singletonMAddr e = do
  maddr <- fromMBytesMAddr <$> allocPinnedMBytes (1 :: Count e)
  maddr <$ writeMAddr maddr e
{-# INLINE singletonMAddr #-}

allocMAddr :: forall e m s. (Primal s m, Unbox e) => Count e -> m (MAddr e s)
allocMAddr c = fromMBytesMAddr <$> allocPinnedMBytes c

allocZeroMAddr :: forall e m s. (Primal s m, Unbox e) => Count e -> m (MAddr e s)
allocZeroMAddr c = fromMBytesMAddr <$> allocZeroPinnedMBytes c


allocAlignedMAddr :: forall e m s. (Primal s m, Unbox e) => Count e -> m (MAddr e s)
allocAlignedMAddr c = fromMBytesMAddr <$> allocAlignedPinnedMBytes c

allocZeroAlignedMAddr :: forall e m s. (Primal s m, Unbox e) => Count e -> m (MAddr e s)
allocZeroAlignedMAddr c = fromMBytesMAddr <$> allocZeroAlignedPinnedMBytes c


-- | Shrink mutable address to new specified size in number of elements. The new count
-- must be less than or equal to the current as reported by `getCountMAddr`.
shrinkMAddr :: (Primal s m, Unbox e) => MAddr e s -> Count e -> m ()
shrinkMAddr maddr@(MAddr _ mb) c = shrinkMBytes mb (toByteCount c + coerce (curByteOffMAddr maddr))
{-# INLINE shrinkMAddr #-}

-- | Shrink mutable address to new specified size in bytes. The new count must be less
-- than or equal to the current as reported by `getByteCountMAddr`.
shrinkByteCountMAddr :: Primal s m => MAddr e s -> Count Word8 -> m ()
shrinkByteCountMAddr maddr@(MAddr _ mb) c = shrinkMBytes mb (c + coerce (curByteOffMAddr maddr))
{-# INLINE shrinkByteCountMAddr #-}


reallocMAddr :: (Primal s m, Unbox e) => MAddr e s -> Count e -> m (MAddr e s)
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
{-# INLINE reallocMAddr #-}


plusOffAddr :: Unbox e => Addr e -> Off e -> Addr e
plusOffAddr (Addr addr# b) off = Addr (addr# `plusAddr#` unOffBytes# off) b
{-# INLINE plusOffAddr #-}

plusByteOffAddr :: Addr e -> Off Word8 -> Addr e
plusByteOffAddr (Addr addr# b) off = Addr (addr# `plusAddr#` unOffBytes# off) b
{-# INLINE plusByteOffAddr #-}

plusOffMAddr :: Unbox e => MAddr e s -> Off e -> MAddr e s
plusOffMAddr (MAddr addr# mb) off = MAddr (addr# `plusAddr#` unOffBytes# off) mb
{-# INLINE plusOffMAddr #-}

plusByteOffMAddr :: MAddr e s -> Off Word8 -> MAddr e s
plusByteOffMAddr (MAddr addr# mb) off = MAddr (addr# `plusAddr#` unOffBytes# off) mb
{-# INLINE plusByteOffMAddr #-}

minusCountAddr :: Unbox e => Addr e -> Addr e -> Count e
minusCountAddr a1 a2 = fromByteCount $ minusByteCountAddr a1 a2
{-# INLINE minusCountAddr #-}

minusByteCountAddr :: Addr e1 -> Addr e2 -> Count Word8
minusByteCountAddr (Addr addr1# _) (Addr addr2# _) = Count (I# (addr1# `minusAddr#` addr2#))
{-# INLINE minusByteCountAddr #-}


curOffAddr :: Unbox e => Addr e -> Off e
curOffAddr a@(Addr addr# b) = (Ptr addr# `minusOffPtr` toPtrBytes b) `offForProxyTypeOf` a
{-# INLINE curOffAddr #-}

curByteOffAddr :: Addr e -> Off Word8
curByteOffAddr (Addr addr# b) = Ptr addr# `minusByteOffPtr` toPtrBytes b
{-# INLINE curByteOffAddr #-}

countAddr ::
     forall e. Unbox e
  => Addr e
  -> Count e
countAddr addr@(Addr _ b) = countBytes b - coerce (curOffAddr addr)
{-# INLINE countAddr #-}

byteCountAddr :: Addr e -> Count Word8
byteCountAddr = countAddr . castAddr
{-# INLINE byteCountAddr #-}

getCountMAddr :: (Primal s m, Unbox e) => MAddr e s -> m (Count e)
getCountMAddr maddr@(MAddr _ mb) =
  subtract (coerce (curOffMAddr maddr)) <$> getCountMBytes mb
{-# INLINE getCountMAddr #-}

getSizeOfMAddr :: (Primal s m, Unbox e) => MAddr e s -> m Size
getSizeOfMAddr maddr = coerce <$> getCountMAddr maddr
{-# INLINE getSizeOfMAddr #-}

getByteCountMAddr :: Primal s m => MAddr e s -> m (Count Word8)
getByteCountMAddr = getCountMAddr . castMAddr
{-# INLINE getByteCountMAddr #-}

indexAddr :: Unbox e => Addr e -> e
indexAddr addr = indexOffAddr addr 0
{-# INLINE indexAddr #-}

indexOffAddr :: Unbox e => Addr e -> Off e -> e
indexOffAddr addr (Off (I# off#)) =
  unsafeInlineIO $ withAddrAddr# addr $ \addr# -> pure $! indexOffAddr# addr# off#
{-# INLINE indexOffAddr #-}

indexByteOffAddr :: Unbox e => Addr e -> Off Word8 -> e
indexByteOffAddr addr off = unsafeInlineIO $ readByteOffAddr addr off
{-# INLINE indexByteOffAddr #-}

withPtrAddr :: Primal s m => Addr e -> (Ptr e -> m b) -> m b
withPtrAddr addr f = withAddrAddr# addr $ \addr# -> f (Ptr addr#)
{-# INLINE withPtrAddr #-}

withAddrAddr# :: Primal s m => Addr e -> (Addr# -> m b) -> m b
withAddrAddr# (Addr addr# b) f = do
  a <- f addr#
  a <$ touch b
{-# INLINE withAddrAddr# #-}

withNoHaltPtrAddr :: UnliftPrimal s m => Addr e -> (Ptr e -> m b) -> m b
withNoHaltPtrAddr (Addr addr# b) f = keepAlive b $ f (Ptr addr#)
{-# INLINE withNoHaltPtrAddr #-}

curOffMAddr :: forall e s . Unbox e => MAddr e s -> Off e
curOffMAddr (MAddr addr# mb) = (Ptr addr# :: Ptr e) `minusOffPtr` toPtrMBytes mb
{-# INLINE curOffMAddr #-}

curByteOffMAddr :: forall e s . MAddr e s -> Off Word8
curByteOffMAddr (MAddr addr# mb) = (Ptr addr# :: Ptr e) `minusByteOffPtr` toPtrMBytes mb
{-# INLINE curByteOffMAddr #-}

withPtrMAddr :: Primal s m => MAddr e s -> (Ptr e -> m b) -> m b
withPtrMAddr maddr f = withAddrMAddr# maddr $ \addr# -> f (Ptr addr#)
{-# INLINE withPtrMAddr #-}



toForeignPtrAddr :: Addr e -> ForeignPtr e
toForeignPtrAddr (Addr addr# (Bytes ba#)) = ForeignPtr addr# (PlainPtr (unsafeCoerce# ba#))


toMForeignPtrMAddr :: MAddr e s -> MForeignPtr e s
toMForeignPtrMAddr (MAddr addr# (MBytes mba#)) =
  MForeignPtr (ForeignPtr addr# (PlainPtr (unsafeCoerce# mba#)))

-- | This is a unsafe cast therefore modification of `ForeignPtr` will be reflected in
-- resulting immutable `Addr`. Pointer created with @malloc@ cannot be converted to `Addr`
-- and will result in `Nothing`
--
-- @since 0.1.0
fromForeignPtrAddr :: ForeignPtr e -> Maybe (Addr e)
fromForeignPtrAddr fptr =
  unsafePerformIO $ fromForeignPtrIO fptr >>= traverse freezeMAddr


-- | Discarding the original ForeignPtr will trigger finalizers that were attached to it,
-- because `MAddr` does not retain any finalizers. Pointer created with @malloc@ cannot be
-- converted to `MAddr` and will result in `Nothing`
--
-- @since 0.1.0
fromForeignPtrMAddr :: ForeignPtr e -> Maybe (MAddr e s)
fromForeignPtrMAddr fptr =
  unsafePerformIO (fmap castStateMAddr <$> fromForeignPtrIO fptr)
  -- case c of
  --   PlainPtr mba#    -> Just (MAddr addr# (MBytes (unsafeCoerce# mba#)))
  --   MallocPtr mba# _ -> Just (MAddr addr# (MBytes (unsafeCoerce# mba#)))
  --   _                -> Nothing


fromForeignPtrIO :: ForeignPtr e -> IO (Maybe (MAddr e RW))
fromForeignPtrIO fptr =
  onForeignPtrContents fptr checkConvert $ \_ -> pure Nothing
  where
    checkConvert addr# mba# checkFinalizers = do
      hasFinalizers <- checkFinalizers
      pure $
        if hasFinalizers
          then Nothing
          else Just (MAddr addr# (MBytes mba#))

withAddrMAddr# :: Primal s m => MAddr e s -> (Addr# -> m b) -> m b
withAddrMAddr# (MAddr addr# mb) f = do
  a <- f addr#
  a <$ touch mb
{-# INLINE withAddrMAddr# #-}

withNoHaltPtrMAddr :: UnliftPrimal s m => MAddr e s -> (Ptr e -> m b) -> m b
withNoHaltPtrMAddr (MAddr addr# mb) f = keepAlive mb $ f (Ptr addr#)
{-# INLINE withNoHaltPtrMAddr #-}


instance MemForeignPtr (MAddr e) where
  toMForeignPtrMem = toMForeignPtrMAddr . castMAddr
  {-# INLINE toMForeignPtrMem #-}

instance MemPtr (MAddr e) where
  withPtrMutMemST maddr = withPtrMAddr (castMAddr maddr)
  {-# INLINE withPtrMutMemST #-}
  withNoHaltPtrMutMemST maddr = withNoHaltPtrMAddr (castMAddr maddr)
  {-# INLINE withNoHaltPtrMutMemST #-}



instance MemAlloc (MAddr e) where
  allocMutMemST = fmap castMAddr . allocMAddr
  {-# INLINE allocMutMemST #-}
  allocPinnedMutMemST = fmap castMAddr . allocMAddr
  {-# INLINE allocPinnedMutMemST #-}
  allocAlignedPinnedMutMemST = fmap castMAddr . allocAlignedMAddr
  {-# INLINE allocAlignedPinnedMutMemST #-}

instance MemFreeze (MAddr e) where
  getByteCountMutMemST = getByteCountMAddr
  {-# INLINE getByteCountMutMemST #-}
  reallocMutMemST maddr = fmap castMAddr . reallocMAddr (castMAddr maddr)
  {-# INLINE reallocMutMemST #-}


instance MemRead (Addr e) where
  accessMem addr _ g o =
    unsafeInlineST $ withAddrAddr# addr $ \addr# -> pure $! g addr# o
  {-# INLINE accessMem #-}
  isSameMem = isSameAddr
  {-# INLINE isSameMem #-}
  byteCountMem = byteCountAddr
  {-# INLINE byteCountMem #-}
  indexOffMem a = indexOffAddr (castAddr a)
  {-# INLINE indexOffMem #-}
  indexByteOffMem a = indexByteOffAddr (castAddr a)
  {-# INLINE indexByteOffMem #-}
  copyByteOffToMBytesMemST a si mb di c =
    withPtrAddr a $ \ptr -> copyByteOffPtrToMBytes (castPtr ptr) si mb di c
  {-# INLINE copyByteOffToMBytesMemST #-}
  copyByteOffToPtrMemST a si mb di c =
    withPtrAddr a $ \ptr -> copyByteOffPtrToPtr (castPtr ptr) si mb di c
  {-# INLINE copyByteOffToPtrMemST #-}
  compareByteOffToPtrMemST addr off1 ptr2 off2 c =
    withPtrAddr addr $ \ptr1 -> pure $ compareByteOffPtrToPtr (castPtr ptr1) off1 ptr2 off2 c
  {-# INLINE compareByteOffToPtrMemST #-}
  compareByteOffToBytesMem addr off1 bytes off2 c =
    unsafeInlineIO $ withPtrAddr addr $ \ptr1 ->
      pure $! compareByteOffPtrToBytes (castPtr ptr1) off1 bytes off2 c
  {-# INLINE compareByteOffToBytesMem #-}
  compareByteOffMem mem1 off1 addr off2 c =
    unsafeInlineIO $ withPtrAddr addr $ \ptr2 ->
      compareByteOffToPtrMem mem1 off1 (castPtr ptr2) off2 c
  {-# INLINE compareByteOffMem #-}

instance MemWrite (MAddr e) where
  accessMutMemST maddr _ g o = withAddrMAddr# maddr $ \addr# -> g addr# o
  {-# INLINE accessMutMemST #-}
  isSameMutMem = isSameMAddr
  {-# INLINE isSameMutMem #-}
  readOffMutMemST a = readOffMAddr (castMAddr a)
  {-# INLINE readOffMutMemST #-}
  readByteOffMutMemST a = readByteOffMAddr (castMAddr a)
  {-# INLINE readByteOffMutMemST #-}
  writeOffMutMemST a = writeOffMAddr (castMAddr a)
  {-# INLINE writeOffMutMemST #-}
  writeByteOffMutMemST a = writeByteOffMAddr (castMAddr a)
  {-# INLINE writeByteOffMutMemST #-}
  moveByteOffToPtrMutMemST src srcOff dstPtr dstOff c =
    withAddrMAddr# src $ \ srcAddr# ->
      moveByteOffPtrToPtr (Ptr srcAddr#) srcOff dstPtr dstOff c
  {-# INLINE moveByteOffToPtrMutMemST #-}
  moveByteOffToMBytesMutMemST src srcOff dst dstOff c =
    withAddrMAddr# src $ \ srcAddr# ->
      moveByteOffPtrToMBytes (Ptr srcAddr#) srcOff dst dstOff c
  {-# INLINE moveByteOffToMBytesMutMemST #-}
  copyByteOffMutMemST src srcOff dst dstOff c =
    withAddrMAddr# dst $ \ dstAddr# ->
      copyByteOffToPtrMem src srcOff (Ptr dstAddr#) dstOff c
  {-# INLINE copyByteOffMutMemST #-}
  moveByteOffMutMemST src srcOff dst dstOff c =
    withAddrMAddr# dst $ \ dstAddr# ->
      moveByteOffToPtrMutMemST src srcOff (Ptr dstAddr#) dstOff c
  {-# INLINE moveByteOffMutMemST #-}
  setByteOffMutMemST maddr = setByteOffMAddr (castMAddr maddr)
  {-# INLINE setByteOffMutMemST #-}
  setMutMemST maddr = setOffMAddr (castMAddr maddr)
  {-# INLINE setMutMemST #-}



thawAddr :: Primal s m => Addr e -> m (MAddr e s)
thawAddr (Addr addr# b) = MAddr addr# <$> thawBytes b
{-# INLINE thawAddr #-}

freezeMAddr :: Primal s m => MAddr e s -> m (Addr e)
freezeMAddr (MAddr addr# mb) = Addr addr# <$> freezeMBytes mb
{-# INLINE freezeMAddr #-}


readAddr :: (Primal s m, Unbox e) => Addr e -> m e
readAddr (Addr addr# b) = do
  a <- primal (readOffAddr# addr# 0#)
  a <$ touch b
{-# INLINE readAddr #-}

readOffAddr :: (Primal s m, Unbox e) => Addr e -> Off e -> m e
readOffAddr (Addr addr# b) (Off (I# off#)) = do
  a <- primal (readOffAddr# addr# off#)
  a <$ touch b
{-# INLINE readOffAddr #-}

readByteOffAddr :: (Primal s m, Unbox e) => Addr e -> Off Word8 -> m e
readByteOffAddr (Addr addr# b) (Off (I# off#)) = do
  a <- primal (readOffAddr# (addr# `plusAddr#` off#) 0#)
  a <$ touch b
{-# INLINE readByteOffAddr #-}

readMAddr :: (Primal s m, Unbox e) => MAddr e s -> m e
readMAddr (MAddr addr# mb) = do
  a <- primal (readOffAddr# addr# 0#)
  a <$ touch mb
{-# INLINE readMAddr #-}

readOffMAddr :: (Primal s m, Unbox e) => MAddr e s -> Off e -> m e
readOffMAddr (MAddr addr# mb) (Off (I# off#)) = do
  a <- primal (readOffAddr# addr# off#)
  a <$ touch mb
{-# INLINE readOffMAddr #-}

readByteOffMAddr :: (Primal s m, Unbox e) => MAddr e s -> Off Word8 -> m e
readByteOffMAddr (MAddr addr# mb) (Off (I# off#)) = do
  a <- primal (readByteOffAddr# addr# off#)
  a <$ touch mb
{-# INLINE readByteOffMAddr #-}

writeMAddr :: (Primal s m, Unbox e) => MAddr e s -> e -> m ()
writeMAddr (MAddr addr# mb) e =
  primal_ $ \s -> touch# mb (writeOffAddr# addr# 0# e s)
{-# INLINE writeMAddr #-}

writeOffMAddr :: (Primal s m, Unbox e) => MAddr e s -> Off e -> e -> m ()
writeOffMAddr (MAddr addr# mb) (Off (I# off#)) e =
  primal_ $ \s -> touch# mb (writeOffAddr# addr# off# e s)
{-# INLINE writeOffMAddr #-}

writeByteOffMAddr :: (Primal s m, Unbox e) => MAddr e s -> Off Word8 -> e -> m ()
writeByteOffMAddr (MAddr addr# mb) (Off (I# off#)) a =
  primal_ $ \s -> touch# mb (writeByteOffAddr# addr# off# a s)
{-# INLINE writeByteOffMAddr #-}


copyAddrToMAddr ::
     (Primal s m, Unbox e) => Addr e -> Off e -> MAddr e s -> Off e -> Count e -> m ()
copyAddrToMAddr src srcOff dst dstOff c =
  withPtrAddr src $ \ srcPtr ->
    withPtrMAddr dst $ \ dstPtr ->
      copyPtrToPtr srcPtr srcOff dstPtr dstOff c
{-# INLINE copyAddrToMAddr #-}

moveMAddrToMAddr ::
     (Primal s m, Unbox e) => MAddr e s -> Off e -> MAddr e s -> Off e -> Count e -> m ()
moveMAddrToMAddr src srcOff dst dstOff c =
  withPtrMAddr src $ \ srcPtr ->
    withPtrMAddr dst $ \ dstPtr ->
      movePtrToPtr srcPtr srcOff dstPtr dstOff c
{-# INLINE moveMAddrToMAddr #-}

setMAddr :: (Primal s m, Unbox e) => MAddr e s -> Count e -> e -> m ()
setMAddr (MAddr addr# mb) (Count (I# n#)) a = primal_ (setAddr# addr# n# a) >> touch mb
{-# INLINE setMAddr #-}

setOffMAddr :: (Primal s m, Unbox e) => MAddr e s -> Off e -> Count e -> e -> m ()
setOffMAddr (MAddr addr# mb) (Off (I# off#)) (Count (I# n#)) a =
  primal_ (setOffAddr# addr# off# n# a) >> touch mb
{-# INLINE setOffMAddr #-}

setByteOffMAddr :: (Primal s m, Unbox e) => MAddr e s -> Off Word8 -> Count e -> e -> m ()
setByteOffMAddr (MAddr addr# mb) (Off (I# off#)) (Count (I# n#)) a =
  primal_ (setByteOffAddr# addr# off# n# a) >> touch mb
{-# INLINE setByteOffMAddr #-}



-- | Apply a pure function to the contents of a mutable variable. Returns the artifact of
-- computation.
--
-- @since 0.2.0
modifyMAddr :: (Primal s m, Unbox a) => MAddr a s -> (a -> (a, b)) -> m b
modifyMAddr maddr f = modifyMAddrM maddr (return . f)
{-# INLINE modifyMAddr #-}

-- | Apply a pure function to the contents of a mutable variable.
--
-- @since 0.1.0
modifyMAddr_ :: (Primal s m, Unbox a) => MAddr a s -> (a -> a) -> m ()
modifyMAddr_ maddr f = modifyMAddrM_ maddr (return . f)
{-# INLINE modifyMAddr_ #-}


-- | Apply a pure function to the contents of a mutable variable. Returns the old value.
--
-- @since 2.0.0
modifyFetchOldMAddr :: (Primal s m, Unbox a) => MAddr a s -> (a -> a) -> m a
modifyFetchOldMAddr maddr f = modifyFetchOldMAddrM maddr (return . f)
{-# INLINE modifyFetchOldMAddr #-}

-- | Apply a pure function to the contents of a mutable variable. Returns the new value.
--
-- @since 2.0.0
modifyFetchNewMAddr :: (Primal s m, Unbox a) => MAddr a s -> (a -> a) -> m a
modifyFetchNewMAddr maddr f = modifyFetchNewMAddrM maddr (return . f)
{-# INLINE modifyFetchNewMAddr #-}


-- | Apply a monadic action to the contents of a mutable variable. Returns the artifact of
-- computation.
--
-- @since 0.2.0
modifyMAddrM :: (Primal s m, Unbox a) => MAddr a s -> (a -> m (a, b)) -> m b
modifyMAddrM maddr f = do
  a <- readMAddr maddr
  (a', b) <- f a
  b <$ writeMAddr maddr a'
{-# INLINE modifyMAddrM #-}

-- | Apply a monadic action to the contents of a mutable variable. Returns the old value.
--
-- @since 2.0.0
modifyFetchOldMAddrM :: (Primal s m, Unbox a) => MAddr a s -> (a -> m a) -> m a
modifyFetchOldMAddrM maddr f = do
  a <- readMAddr maddr
  a <$ (writeMAddr maddr =<< f a)
{-# INLINE modifyFetchOldMAddrM #-}


-- | Apply a monadic action to the contents of a mutable variable. Returns the new value.
--
-- @since 2.0.0
modifyFetchNewMAddrM :: (Primal s m, Unbox a) => MAddr a s -> (a -> m a) -> m a
modifyFetchNewMAddrM maddr f = do
  a <- readMAddr maddr
  a' <- f a
  a' <$ writeMAddr maddr a'
{-# INLINE modifyFetchNewMAddrM #-}


-- | Apply a monadic action to the contents of a mutable variable.
--
-- @since 0.1.0
modifyMAddrM_ :: (Primal s m, Unbox a) => MAddr a s -> (a -> m a) -> m ()
modifyMAddrM_ maddr f = readMAddr maddr >>= f >>= writeMAddr maddr
{-# INLINE modifyMAddrM_ #-}

-- | Swap contents of two mutable variables. Returns their old values.
--
-- @since 0.1.0
swapMAddrs :: (Primal s m, Unbox a) => MAddr a s -> MAddr a s -> m (a, a)
swapMAddrs maddr1 maddr2 = do
  a1 <- readMAddr maddr1
  a2 <- modifyFetchOldMAddr maddr2 (const a1)
  (a1, a2) <$ writeMAddr maddr1 a2
{-# INLINE swapMAddrs #-}

-- | Swap contents of two mutable variables.
--
-- @since 0.1.0
swapMAddrs_ :: (Primal s m, Unbox a) => MAddr a s -> MAddr a s -> m ()
swapMAddrs_ maddr1 maddr2 = void $ swapMAddrs maddr1 maddr2
{-# INLINE swapMAddrs_ #-}



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

-- | /O(n)/ - Convert an immutable `ShortByteString` to an immutable `Addr`. In a most common
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
    Nothing -> byteStringConvertError "ByteString was allocated outside of 'bytestring' package"

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
     (Primal s m, Atomic e)
  => MAddr e s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__e__@, rather than bytes.
  -> e -- ^ Expected old value
  -> e -- ^ New value
  -> m e
casOffMAddr maddr (Off (I# i#)) old new =
  withAddrMAddr# maddr $ \ addr# -> primal $ casOffAddr# addr# i# old new
{-# INLINE casOffMAddr #-}


-- | Perform atomic modification of an element in the `MAddr` at the supplied
-- index. Returns `True` if swap was successfull and false otherwise.  Offset is in number
-- of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
casBoolOffMAddr ::
     (Primal s m, Atomic e)
  => MAddr e s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__e__@, rather than bytes.
  -> e -- ^ Expected old value
  -> e -- ^ New value
  -> m Bool
casBoolOffMAddr maddr (Off (I# i#)) old new =
  withAddrMAddr# maddr $ \ addr# -> primal $ casBoolOffAddr# addr# i# old new
{-# INLINE casBoolOffMAddr #-}

-- | Just like `casBoolOffMAddr`, but also returns the actual value, which will match the
-- supplied expected value if the returned flag is `True`
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
casBoolFetchOffMAddr ::
     (Primal s m, Atomic e)
  => MAddr e s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__e__@, rather than bytes.
  -> e -- ^ Expected old value
  -> e -- ^ New value
  -> m (Bool, e)
casBoolFetchOffMAddr maddr (Off (I# i#)) expected new = do
  withAddrMAddr# maddr $ \addr# ->
    primal $ \s ->
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
     (Primal s m, Atomic e)
  => MAddr e s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__e__@, rather than bytes.
  -> m e
atomicReadOffMAddr maddr (Off (I# i#)) =
  withAddrMAddr# maddr $ \ addr# -> primal $ atomicReadOffAddr# addr# i#
{-# INLINE atomicReadOffMAddr #-}

-- | Perform atomic write of an element in the `MAddr` at the supplied offset. Offset is in
-- number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicWriteOffMAddr ::
     (Primal s m, Atomic e)
  => MAddr e s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__e__@, rather than bytes.
  -> e
  -> m ()
atomicWriteOffMAddr maddr (Off (I# i#)) e =
  withAddrMAddr# maddr $ \ addr# -> primal_ $ atomicWriteOffAddr# addr# i# e
{-# INLINE atomicWriteOffMAddr #-}


-- | Perform atomic modification of an element in the `MAddr` at the supplied
-- index. Returns the artifact of computation @__b__@.  Offset is in number of elements,
-- rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicModifyOffMAddr ::
     (Primal s m, Atomic e)
  => MAddr e s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__e__@, rather than bytes.
  -> (e -> (e, b)) -- ^ Function that is applied to the old value and returns new value
                   -- and some artifact of computation @__b__@
  -> m b
atomicModifyOffMAddr maddr (Off (I# i#)) f =
  withAddrMAddr# maddr $ \ addr# -> primal $
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
     (Primal s m, Atomic e)
  => MAddr e s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__e__@, rather than bytes.
  -> (e -> e) -- ^ Function that is applied to the current value
  -> m ()
atomicModifyOffMAddr_ maddr (Off (I# i#)) f =
  withAddrMAddr# maddr $ \ addr# -> primal_ $ atomicModifyOffAddr_# addr# i# f
{-# INLINE atomicModifyOffMAddr_ #-}


-- | Perform atomic modification of an element in the `MAddr` at the supplied
-- index. Returns the previous value.  Offset is in number of elements, rather than
-- bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicModifyFetchOldOffMAddr ::
     (Primal s m, Atomic e)
  => MAddr e s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__e__@, rather than bytes.
  -> (e -> e) -- ^ Function that is applied to the old value
  -> m e -- ^ Returns the old value
atomicModifyFetchOldOffMAddr maddr (Off (I# i#)) f =
  withAddrMAddr# maddr $ \ addr# -> primal $ atomicModifyFetchOldOffAddr# addr# i# f
{-# INLINE atomicModifyFetchOldOffMAddr #-}


-- | Perform atomic modification of an element in the `MAddr` at the supplied
-- index.  Offset is in number of elements, rather than bytes. Implies a full memory
-- barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicModifyFetchNewOffMAddr ::
     (Primal s m, Atomic e)
  => MAddr e s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__e__@, rather than bytes
  -> (e -> e) -- ^ Function that is applied to the old value
  -> m e -- ^ Returns the new value
atomicModifyFetchNewOffMAddr maddr (Off (I# i#)) f =
  withAddrMAddr# maddr $ \ addr# -> primal $ atomicModifyFetchNewOffAddr# addr# i# f
{-# INLINE atomicModifyFetchNewOffMAddr #-}



-- | Add a numeric value to an element of a `MAddr`, corresponds to @(`+`)@ done
-- atomically. Returns the previous value.  Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicAddFetchOldOffMAddr ::
     (Primal s m, AtomicCount e)
  => MAddr e s
  -> Off e
  -> e
  -> m e
atomicAddFetchOldOffMAddr maddr (Off (I# i#)) a =
  withAddrMAddr# maddr $ \ addr# -> primal $ atomicAddFetchOldOffAddr# addr# i# a
{-# INLINE atomicAddFetchOldOffMAddr #-}

-- | Add a numeric value to an element of a `MAddr`, corresponds to @(`+`)@ done
-- atomically. Returns the new value.  Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicAddFetchNewOffMAddr ::
     (Primal s m, AtomicCount e)
  => MAddr e s
  -> Off e
  -> e
  -> m e
atomicAddFetchNewOffMAddr maddr (Off (I# i#)) a =
  withAddrMAddr# maddr $ \ addr# -> primal $ atomicAddFetchNewOffAddr# addr# i# a
{-# INLINE atomicAddFetchNewOffMAddr #-}



-- | Subtract a numeric value from an element of a `MAddr`, corresponds to
-- @(`-`)@ done atomically. Returns the previous value.  Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicSubFetchOldOffMAddr ::
     (Primal s m, AtomicCount e)
  => MAddr e s
  -> Off e
  -> e
  -> m e
atomicSubFetchOldOffMAddr maddr (Off (I# i#)) a =
  withAddrMAddr# maddr $ \ addr# -> primal $ atomicSubFetchOldOffAddr# addr# i# a
{-# INLINE atomicSubFetchOldOffMAddr #-}

-- | Subtract a numeric value from an element of a `MAddr`, corresponds to
-- @(`-`)@ done atomically. Returns the new value. Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicSubFetchNewOffMAddr ::
     (Primal s m, AtomicCount e)
  => MAddr e s
  -> Off e
  -> e
  -> m e
atomicSubFetchNewOffMAddr maddr (Off (I# i#)) a =
  withAddrMAddr# maddr $ \ addr# -> primal $ atomicSubFetchNewOffAddr# addr# i# a
{-# INLINE atomicSubFetchNewOffMAddr #-}



-- | Binary conjunction (AND) of an element of a `MAddr` with the supplied value,
-- corresponds to @(`Data.Bits..&.`)@ done atomically. Returns the previous value. Offset
-- is in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicAndFetchOldOffMAddr ::
     (Primal s m, AtomicBits e)
  => MAddr e s
  -> Off e
  -> e
  -> m e
atomicAndFetchOldOffMAddr maddr (Off (I# i#)) a =
  withAddrMAddr# maddr $ \ addr# -> primal $ atomicAndFetchOldOffAddr# addr# i# a
{-# INLINE atomicAndFetchOldOffMAddr #-}

-- | Binary conjunction (AND) of an element of a `MAddr` with the supplied value,
-- corresponds to @(`Data.Bits..&.`)@ done atomically. Returns the new value. Offset is
-- in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicAndFetchNewOffMAddr ::
     (Primal s m, AtomicBits e)
  => MAddr e s
  -> Off e
  -> e
  -> m e
atomicAndFetchNewOffMAddr maddr (Off (I# i#)) a =
  withAddrMAddr# maddr $ \ addr# -> primal $ atomicAndFetchNewOffAddr# addr# i# a
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
     (Primal s m, AtomicBits e)
  => MAddr e s
  -> Off e
  -> e
  -> m e
atomicNandFetchOldOffMAddr maddr (Off (I# i#)) a =
  withAddrMAddr# maddr $ \ addr# -> primal $ atomicNandFetchOldOffAddr# addr# i# a
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
     (Primal s m, AtomicBits e)
  => MAddr e s
  -> Off e
  -> e
  -> m e
atomicNandFetchNewOffMAddr maddr (Off (I# i#)) a =
  withAddrMAddr# maddr $ \ addr# -> primal $ atomicNandFetchNewOffAddr# addr# i# a
{-# INLINE atomicNandFetchNewOffMAddr #-}




-- | Binary disjunction (OR) of an element of a `MAddr` with the supplied value,
-- corresponds to @(`Data.Bits..|.`)@ done atomically. Returns the previous value. Offset
-- is in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicOrFetchOldOffMAddr ::
     (Primal s m, AtomicBits e)
  => MAddr e s
  -> Off e
  -> e
  -> m e
atomicOrFetchOldOffMAddr maddr (Off (I# i#)) a =
  withAddrMAddr# maddr $ \ addr# -> primal $ atomicOrFetchOldOffAddr# addr# i# a
{-# INLINE atomicOrFetchOldOffMAddr #-}

-- | Binary disjunction (OR) of an element of a `MAddr` with the supplied value,
-- corresponds to @(`Data.Bits..|.`)@ done atomically. Returns the new value. Offset is
-- in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicOrFetchNewOffMAddr ::
     (Primal s m, AtomicBits e)
  => MAddr e s
  -> Off e
  -> e
  -> m e
atomicOrFetchNewOffMAddr maddr (Off (I# i#)) a =
  withAddrMAddr# maddr $ \ addr# -> primal $ atomicOrFetchNewOffAddr# addr# i# a
{-# INLINE atomicOrFetchNewOffMAddr #-}



-- | Binary exclusive disjunction (XOR) of an element of a `MAddr` with the supplied value,
-- corresponds to @`Data.Bits.xor`@ done atomically. Returns the previous value. Offset
-- is in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicXorFetchOldOffMAddr ::
     (Primal s m, AtomicBits e)
  => MAddr e s
  -> Off e
  -> e
  -> m e
atomicXorFetchOldOffMAddr maddr (Off (I# i#)) a =
  withAddrMAddr# maddr $ \ addr# -> primal $ atomicXorFetchOldOffAddr# addr# i# a
{-# INLINE atomicXorFetchOldOffMAddr #-}

-- | Binary exclusive disjunction (XOR) of an element of a `MAddr` with the supplied value,
-- corresponds to @`Data.Bits.xor`@ done atomically. Returns the new value. Offset is
-- in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicXorFetchNewOffMAddr ::
     (Primal s m, AtomicBits e)
  => MAddr e s
  -> Off e
  -> e
  -> m e
atomicXorFetchNewOffMAddr maddr (Off (I# i#)) a =
  withAddrMAddr# maddr $ \ addr# -> primal $ atomicXorFetchNewOffAddr# addr# i# a
{-# INLINE atomicXorFetchNewOffMAddr #-}





-- | Binary negation (NOT) of an element of a `MAddr`, corresponds to
-- @(`Data.Bits.complement`)@ done atomically. Returns the previous value. Offset is in
-- number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicNotFetchOldOffMAddr ::
     (Primal s m, AtomicBits e)
  => MAddr e s
  -> Off e
  -> m e
atomicNotFetchOldOffMAddr maddr (Off (I# i#)) =
  withAddrMAddr# maddr $ \ addr# -> primal $ atomicNotFetchOldOffAddr# addr# i#
{-# INLINE atomicNotFetchOldOffMAddr #-}

-- | Binary negation (NOT) of an element of a `MAddr`, corresponds to
-- @(`Data.Bits.complement`)@ done atomically. Returns the new value. Offset is in number
-- of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicNotFetchNewOffMAddr ::
     (Primal s m, AtomicBits e)
  => MAddr e s
  -> Off e
  -> m e
atomicNotFetchNewOffMAddr maddr (Off (I# i#)) =
  withAddrMAddr# maddr $ \ addr# -> primal $ atomicNotFetchNewOffAddr# addr# i#
{-# INLINE atomicNotFetchNewOffMAddr #-}
