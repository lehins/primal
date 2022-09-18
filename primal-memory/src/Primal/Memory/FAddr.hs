{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Primal.Memory.FAddr
-- Copyright   : (c) Alexey Kuleshevich 2020-2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Memory.FAddr
  ( -- * Immutable Foreign Address
    FAddr(..)
  , emptyFAddr
  , isSameFAddr
  -- , castFAddr
  -- , fromBytesFAddr
  -- , curOffFAddr
  , byteCountFAddr
  , countFAddr
  , plusOffFAddr
  , plusByteOffFAddr
  , minusCountFAddr
  , minusByteCountFAddr
  , indexFAddr
  , indexOffFAddr
  , indexByteOffFAddr
  , readFAddr
  , readOffFAddr
  , readByteOffFAddr
  -- , thawFAddr
  , freezeFMAddr
  , withPtrFAddr
  , withAddrFAddr#
  , withNoHaltPtrFAddr
   -- * Mutable Foreign Address
  , FMAddr(..)
  , Finalizer(..)
  , isSameFMAddr
  , castFMAddr
  , allocFMAddr
  , allocZeroFMAddr
  , reallocFMAddr
  , reallocPtrFMAddr
  , allocWithFinalizerPtrFMAddr
  , allocWithFinalizerEnvPtrFMAddr
  , singletonFMAddr
  , setFMAddr
  , setOffFMAddr
  , setByteOffFMAddr
  -- , curOffFMAddr
  , getByteCountFMAddr
  , getCountFMAddr
  , getSizeOfFMAddr
  , plusOffFMAddr
  , plusByteOffFMAddr
  -- -- TODO:
  -- -- , minusCountFMAddr
  -- -- , minusByteCountFMAddr
  , readFMAddr
  , readOffFMAddr
  , readByteOffFMAddr
  , writeFMAddr
  , writeOffFMAddr
  , writeByteOffFMAddr
  , copyFAddrToFMAddr
  , moveFMAddrToFMAddr
  , modifyFMAddr
  , modifyFMAddr_
  , modifyFetchOldFMAddr
  , modifyFetchNewFMAddr
  , modifyFMAddrM
  , modifyFMAddrM_
  , modifyFetchOldFMAddrM
  , modifyFetchNewFMAddrM
  , swapFMAddrs_
  , swapFMAddrs
  , withPtrFMAddr
  , withAddrFMAddr#
  , withNoHaltPtrFMAddr
  -- * Atomic
  , casOffFMAddr
  , casBoolOffFMAddr
  , casBoolFetchOffFMAddr
  , atomicReadOffFMAddr
  , atomicWriteOffFMAddr
  , atomicModifyOffFMAddr
  , atomicModifyOffFMAddr_
  , atomicModifyFetchOldOffFMAddr
  , atomicModifyFetchNewOffFMAddr
  -- ** Numeric
  , atomicAddFetchOldOffFMAddr
  , atomicAddFetchNewOffFMAddr
  , atomicSubFetchOldOffFMAddr
  , atomicSubFetchNewOffFMAddr
  -- ** Binary
  , atomicAndFetchOldOffFMAddr
  , atomicAndFetchNewOffFMAddr
  , atomicNandFetchOldOffFMAddr
  , atomicNandFetchNewOffFMAddr
  , atomicOrFetchOldOffFMAddr
  , atomicOrFetchNewOffFMAddr
  , atomicXorFetchOldOffFMAddr
  , atomicXorFetchNewOffFMAddr
  , atomicNotFetchOldOffFMAddr
  , atomicNotFetchNewOffFMAddr
  -- * Re-export
  , module Primal.Element.Unbox
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Monoid as Monoid
import qualified Data.Semigroup as Semigroup
import Primal.Array
import Primal.Exception
import Primal.Exception.Interruptible as Interruptible
import Primal.Eval
import Primal.Foreign
import Primal.Memory.Bytes
import Primal.Memory.Bytes.Internal
import Primal.Memory.Fold
import Primal.Memory.ForeignPtr
import Primal.Memory.Internal
import Primal.Memory.Ptr
import Primal.Monad
import Primal.Monad.Unsafe
import Primal.Mutable.Eq
import Primal.Memory.Weak
import Primal.Mutable.Freeze
import Primal.Mutable.Ord
import Primal.Ref.Boxed
import Primal.Element.Unbox
import Primal.Element.Unbox.Atomic

data Finalizer
  = NoFinalizer
  | Finalizer {-# UNPACK #-} !(BRef () RW)
  | ReallocFinalizer {-# UNPACK #-} !(MBytes 'Pin RW)


instance NFData Finalizer where
  rnf = \case
    NoFinalizer -> ()
    Finalizer _ -> ()
    ReallocFinalizer _ -> ()

-- | Immutable read-only foreign address
data FAddr e = FAddr
  { faAddr# :: Addr#
  , faCount :: {-# UNPACK #-} !(Count e)
  , faFinalizer :: !Finalizer
  }
type role FAddr nominal

-- | Mutable foreign address
data FMAddr e s = FMAddr
  { fmaAddr#  :: Addr#
  , fmaCount :: {-# UNPACK #-} !(Count e)
  , fmaFinalizer :: !Finalizer
  }
type role FMAddr nominal nominal


allocFMAddr :: forall e m s. (Primal s m, Unbox e) => Count e -> m (FMAddr e s)
allocFMAddr c =
  unsafeIOToPrimal (castStateFMAddr <$> allocWithFinalizerPtrFMAddr c mallocPtr freeFinalizerPtr)
{-# INLINE allocFMAddr #-}

allocZeroFMAddr :: forall e m s. (Primal s m, Unbox e) => Count e -> m (FMAddr e s)
allocZeroFMAddr c =
  unsafeIOToPrimal (castStateFMAddr <$> allocWithFinalizerPtrFMAddr c callocPtr freeFinalizerPtr)
{-# INLINE allocZeroFMAddr #-}

guardImpossibleFinalizer :: String -> IO Bool -> IO ()
guardImpossibleFinalizer name m =
  unlessM m $
  raiseString $
    "Impossible: " ++ name ++ " was just created, thus adding finalizer should always succeed"

-- | It is important that the supplied pointer is never freed afterwards
reallocPtrFMAddr :: forall e m s. (Primal s m, Unbox e) => Ptr e -> Count e -> m (FMAddr e s)
reallocPtrFMAddr ptr c =
  unsafeIOToPrimal $ do
    -- We need to keep around the freeing flag as well as the pointer to the beginning
    mb <- allocZeroPinnedMBytes (toByteCount (1 :: Count CBool) + toByteCount (1 :: Count (Ptr e)))
    weakPtr <- mkWeakNoFinalizerMBytes mb mb
    -- Make sure not to leak memory by masking exceptions until finalizer is properly registered
    Interruptible.mask_ $ do
      newPtr@(Ptr addr#) <- reallocPtr ptr c
      writeByteOffMBytes mb (toByteOff (1 :: Off CBool)) newPtr
      guardImpossibleFinalizer "MBytes" $
        addCFinalizerEnv guardedFreeFinalizerEnvPtr (toPtrMBytes mb) newPtr weakPtr
      pure $!
        FMAddr {fmaAddr# = addr#, fmaCount = c, fmaFinalizer = ReallocFinalizer mb}
{-# INLINE reallocPtrFMAddr #-}

reallocFMAddr :: forall e m s. (Primal s m, Unbox e) => FMAddr e s -> Count e -> m (FMAddr e s)
reallocFMAddr fma c
  | c == fmaCount fma = pure fma
  | otherwise =
    case fmaFinalizer fma of
      ReallocFinalizer mb -> do
        ptr <- unsafeIOToPrimal $ readByteOffMBytes mb (toByteOff (1 :: Off CBool))
        let curPtr = Ptr (fmaAddr# fma)
            offPtr = minusByteOffPtr curPtr ptr
        when (offPtr > 0) $
          moveByteOffPtrToPtr ptr offPtr ptr 0 (min (fmaCount fma) c)
        fma' <- reallocPtrFMAddr ptr c
        -- Mark that the pointer has potentially been freed. This way the old
        -- ReallocFinalizer does not double free and if it wasn't freed the new finalizer
        -- will take care of it when it is no longer referenced.
        fma' <$ unsafeIOToPrimal (writeOffMBytes mb 0 (1 :: CBool))
      _ -> do
        fa <- freezeFMAddr fma
        fma' <- reallocPtrFMAddr nullPtr c
        fma' <$ copyFAddrToFMAddr fa 0 fma' 0 (min c (faCount fa))
{-# INLINE reallocFMAddr #-}

-- allocWithFinalizerFMAddr ::
--      Count e
--   -- ^ Number of elements to allocate memory for
--   -> (Count e -> IO (Ptr e))
--   -- ^ Function to be used for allocating memory
--   -> Ptr e -> IO ()
--   -- ^ Finalizer to be used for freeing memory
--   -> IO (FMAddr e s)
-- allocWithFinalizerFMAddr c alloc freeFinalizer =
--   allocAndFinalizeFMAddr c alloc (addCFinalizer freeFinalizer)
-- {-# INLINE allocWithFinalizerFMAddr #-}


allocWithFinalizerPtrFMAddr ::
     Count e
  -- ^ Number of elements to allocate memory for
  -> (Count e -> IO (Ptr e))
  -- ^ Function to be used for allocating memory
  -> FinalizerPtr e
  -- ^ Finalizer to be used for freeing memory
  -> IO (FMAddr e RW)
allocWithFinalizerPtrFMAddr c alloc freeFinalizer =
  allocAndFinalizeFMAddr c alloc (addCFinalizer freeFinalizer)
{-# INLINE allocWithFinalizerPtrFMAddr #-}

allocWithFinalizerEnvPtrFMAddr ::
     Count e
  -- ^ Number of elements to allocate memory for
  -> (Count e -> IO (Ptr e))
  -- ^ Function to be used for allocating memory
  -> FinalizerEnvPtr env e
  -- ^ Finalizer to be used for freeing memory
  -> Ptr env
  -- ^ Pointer with enviroment to be passed to the finalizer
  -> IO (FMAddr e RW)
allocWithFinalizerEnvPtrFMAddr c alloc freeFinalizer envPtr =
  allocAndFinalizeFMAddr c alloc (addCFinalizerEnv freeFinalizer envPtr)
{-# INLINE allocWithFinalizerEnvPtrFMAddr #-}

allocAndFinalizeFMAddr ::
     Count e
  -> (Count e -> IO (Ptr a))
  -> (Ptr a -> Weak () -> IO Bool)
  -> IO (FMAddr e s)
allocAndFinalizeFMAddr c alloc finalize = do
  ref <- newBRef ()
  weakPtr <- mkWeakNoFinalizerBRef ref ()
  Interruptible.mask_ $ do
    ptr@(Ptr addr#) <- alloc c
    guardImpossibleFinalizer "BRef" (finalize ptr weakPtr)
    pure $!
      FMAddr {fmaAddr# = addr#, fmaCount = c, fmaFinalizer = Finalizer ref}
{-# INLINE allocAndFinalizeFMAddr #-}


singletonFMAddr :: forall e m s. (Primal s m, Unbox e) => e -> m (FMAddr e s)
singletonFMAddr e = do
  fma <- allocFMAddr (1 :: Count e)
  fma <$ writeFMAddr fma e
{-# INLINE singletonFMAddr #-}


instance (Eq e, Unbox e) => Eq (FAddr e) where
  (==) = eqMem @e
  {-# INLINE (==) #-}

instance (Unbox e, Eq e) => MutEq (FMAddr e) where
  eqMutST m1 m2 = eqWithST isSameFMAddr getSizeOfFMAddr (\m -> readOffFMAddr m . coerce) m1 m2
  {-# INLINE eqMutST #-}

instance (Unbox e, Ord e) => Ord (FAddr e) where
  compare = compareMem @e
  {-# INLINE compare #-}

instance (Unbox e, Ord e) => MutOrd (FMAddr e) where
  compareMutST m1 m2 =
    compareWithST isSameFMAddr getSizeOfFMAddr (\m -> readOffFMAddr m . coerce) m1 m2
  {-# INLINE compareMutST #-}

instance (Show e, Unbox e) => Show (FAddr e) where
  show a = show (toListMem a :: [e])

instance IsString (FAddr Char) where
  fromString = fromListMem
  {-# INLINE fromString #-}

instance Unbox e => IsList (FAddr e) where
  type Item (FAddr e) = e
  fromList = fromListMem
  {-# INLINE fromList #-}
  fromListN n = fromListZeroMemN_ (Count n)
  {-# INLINE fromListN #-}
  toList = toListMem
  {-# INLINE toList #-}

instance Unbox e => Semigroup.Semigroup (FAddr e) where
  (<>) = appendMem
  {-# INLINE (<>) #-}
  sconcat (x :| xs) = concatMem (x:xs)
  {-# INLINE sconcat #-}
  stimes i = cycleMemN (fromIntegral i)
  {-# INLINE stimes #-}

instance Unbox e => Monoid.Monoid (FAddr e) where
  mappend = appendMem
  {-# INLINE mappend #-}
  mconcat = concatMem
  {-# INLINE mconcat #-}
  mempty = emptyFAddr
  {-# INLINE mempty #-}

type instance Frozen (FMAddr e) = FAddr e

instance Unbox e => MutFreeze (FMAddr e) where
  thawST = thawFAddr
  {-# INLINE thawST #-}
  thawCloneST addr = do
    let c = countFAddr addr
    maddr <- allocFMAddr c
    maddr <$ copyFAddrToFMAddr addr 0 maddr 0 c
  {-# INLINE thawCloneST #-}
  clone = cloneMem
  {-# INLINE clone #-}
  freezeMutST = freezeFMAddr
  {-# INLINE freezeMutST #-}

emptyFAddr :: FAddr e
emptyFAddr = FAddr nullAddr# 0 NoFinalizer
{-# INLINE emptyFAddr #-}


-- | Cast a foreign address. Watchout the count, since mismatch on the element size can
-- lead to unused slack at the end of the buffer
castFAddr :: (Unbox e, Unbox a) => FAddr e -> FAddr a
castFAddr (FAddr addr# c fi) = FAddr addr# (fromByteCount (toByteCount c)) fi
{-# INLINE castFAddr #-}

-- | Cast a foreign address. Watchout the count, since mismatch on the element size can
-- lead to unused slack at the end of the buffer
castFMAddr :: (Unbox e, Unbox a) => FMAddr e s -> FMAddr a s
castFMAddr (FMAddr addr# c fin) = FMAddr addr# (fromByteCount (toByteCount c)) fin
{-# INLINE castFMAddr #-}

-- | Casr the state token type.
castStateFMAddr :: FMAddr e s -> FMAddr e s'
castStateFMAddr (FMAddr addr# c fin) = FMAddr addr# c fin
{-# INLINE castStateFMAddr #-}

isSameFAddr :: FAddr e -> FAddr e -> Bool
isSameFAddr (FAddr a1# c1 _) (FAddr a2# c2 _) = isTrue# (a1# `eqAddr#` a2#) && c1 == c2
{-# INLINE isSameFAddr #-}

isSameFMAddr :: FMAddr e s -> FMAddr e s -> Bool
isSameFMAddr (FMAddr a1# c1 _) (FMAddr a2# c2 _) = isTrue# (a1# `eqAddr#` a2#) && c1 == c2
{-# INLINE isSameFMAddr #-}

instance NFData (FAddr e) where
  rnf (FAddr _ _ fin) = rnf fin

instance NFData (FMAddr e s) where
  rnf (FMAddr _ _ fin) = rnf fin


plusOffFAddr :: Unbox e => FAddr e -> Off e -> FAddr e
plusOffFAddr (FAddr addr# c fin) off =
  FAddr (addr# `plusAddr#` unOffBytes# off) (c `countMinusOff` off) fin
{-# INLINE plusOffFAddr #-}

plusByteOffFAddr :: FAddr e -> Off Word8 -> FAddr e
plusByteOffFAddr (FAddr addr# c fin) off =
  FAddr (addr# `plusAddr#` unOffBytes# off) c fin
{-# INLINE plusByteOffFAddr #-}

plusOffFMAddr :: Unbox e => FMAddr e s -> Off e -> FMAddr e s
plusOffFMAddr fma off = fma { fmaAddr# = fmaAddr# fma `plusAddr#` unOffBytes# off }
{-# INLINE plusOffFMAddr #-}

plusByteOffFMAddr :: FMAddr e s -> Off Word8 -> FMAddr e s
plusByteOffFMAddr (FMAddr addr# c fin) off = FMAddr (addr# `plusAddr#` unOffBytes# off) c fin
{-# INLINE plusByteOffFMAddr #-}

minusCountFAddr :: Unbox e => FAddr e -> FAddr e -> Count e
minusCountFAddr a1 a2 = fromByteCount $ minusByteCountFAddr a1 a2
{-# INLINE minusCountFAddr #-}

minusByteCountFAddr :: FAddr e1 -> FAddr e2 -> Count Word8
minusByteCountFAddr (FAddr addr1# _ _) (FAddr addr2# _ _) = Count (I# (addr1# `minusAddr#` addr2#))
{-# INLINE minusByteCountFAddr #-}


-- curOffFAddr :: Unbox e => FAddr e -> Off e
-- curOffFAddr a@(FAddr addr# _ b) = (Ptr addr# `minusOffPtr` toPtrBytes b) `offForProxyTypeOf` a
-- {-# INLINE curOffFAddr #-}

-- curByteOffAddr :: Addr e -> Off Word8
-- curByteOffAddr (Addr addr# b) = Ptr addr# `minusByteOffPtr` toPtrBytes b
-- {-# INLINE curByteOffAddr #-}

countFAddr :: FAddr e -> Count e
countFAddr = faCount
{-# INLINE countFAddr #-}

byteCountFAddr :: Unbox e => FAddr e -> Count Word8
byteCountFAddr = toByteCount . faCount
{-# INLINE byteCountFAddr #-}

getCountFMAddr :: Monad m => FMAddr e s -> m (Count e)
getCountFMAddr (FMAddr _ c _) = pure c
{-# INLINE getCountFMAddr #-}

getSizeOfFMAddr :: (Monad m, Unbox e) => FMAddr e s -> m Size
getSizeOfFMAddr maddr = coerce <$> getCountFMAddr maddr
{-# INLINE getSizeOfFMAddr #-}

getByteCountFMAddr :: (Unbox e, Monad m) => FMAddr e s -> m (Count Word8)
getByteCountFMAddr = getCountFMAddr . castFMAddr
{-# INLINE getByteCountFMAddr #-}

indexFAddr :: Unbox e => FAddr e -> e
indexFAddr addr = indexOffFAddr addr 0
{-# INLINE indexFAddr #-}

indexOffFAddr :: Unbox e => FAddr e -> Off e -> e
indexOffFAddr addr (Off (I# off#)) =
  unsafeInlineIO $ withAddrFAddr# addr $ \addr# -> pure $! indexOffAddr# addr# off#
{-# INLINE indexOffFAddr #-}

indexByteOffFAddr :: Unbox e => FAddr e -> Off Word8 -> e
indexByteOffFAddr addr off = unsafeInlineIO $ readByteOffFAddr addr off
{-# INLINE indexByteOffFAddr #-}

withPtrFAddr :: Primal s m => FAddr e -> (Ptr e -> m b) -> m b
withPtrFAddr addr f = withAddrFAddr# addr $ \addr# -> f (Ptr addr#)
{-# INLINE withPtrFAddr #-}

withAddrFAddr# :: Primal s m => FAddr e -> (Addr# -> m b) -> m b
withAddrFAddr# (FAddr addr# _ fin) f = do
  a <- f addr#
  a <$ touch fin
{-# INLINE withAddrFAddr# #-}

-- | Access the pointer for read-only purposes. Make sure the pointer is not modified.
withNoHaltPtrFAddr :: UnliftPrimal s m => FAddr e -> (Ptr e -> m b) -> m b
withNoHaltPtrFAddr (FAddr addr# _ b) f = keepAlive b $ f (Ptr addr#)
{-# INLINE withNoHaltPtrFAddr #-}

-- curOffMAddr :: forall e s . Unbox e => MAddr e s -> Off e
-- curOffMAddr (MAddr addr# mb) = (Ptr addr# :: Ptr e) `minusOffPtr` toPtrMBytes mb
-- {-# INLINE curOffMAddr #-}

-- curByteOffMAddr :: forall e s . MAddr e s -> Off Word8
-- curByteOffMAddr (MAddr addr# mb) = (Ptr addr# :: Ptr e) `minusByteOffPtr` toPtrMBytes mb
-- {-# INLINE curByteOffMAddr #-}

withPtrFMAddr :: Primal s m => FMAddr e s -> (Ptr e -> m b) -> m b
withPtrFMAddr maddr f = withAddrFMAddr# maddr $ \addr# -> f (Ptr addr#)
{-# INLINE withPtrFMAddr #-}


withAddrFMAddr# :: Primal s m => FMAddr e s -> (Addr# -> m b) -> m b
withAddrFMAddr# (FMAddr addr# _ fin) f = do
  a <- f addr#
  a <$ touch fin
{-# INLINE withAddrFMAddr# #-}

withNoHaltPtrFMAddr :: UnliftPrimal s m => FMAddr e s -> (Ptr e -> m b) -> m b
withNoHaltPtrFMAddr (FMAddr addr# _ fin) f = keepAlive fin $ f (Ptr addr#)
{-# INLINE withNoHaltPtrFMAddr #-}


instance Unbox e => MemPtr (FMAddr e) where
  withPtrMemST maddr = withPtrFMAddr (castFMAddr maddr)
  {-# INLINE withPtrMemST #-}
  withNoHaltPtrMemST maddr = withNoHaltPtrFMAddr (castFMAddr maddr)
  {-# INLINE withNoHaltPtrMemST #-}



instance Unbox e => MemAlloc (FMAddr e) where
  getByteCountMutMemST = getByteCountFMAddr
  {-# INLINE getByteCountMutMemST #-}
  allocMutMemST = fmap castFMAddr . allocFMAddr
  {-# INLINE allocMutMemST #-}
  allocPinnedMutMemST = fmap castFMAddr . allocFMAddr
  {-# INLINE allocPinnedMutMemST #-}
  -- Use posix_memalign
  -- allocAlignedPinnedMutMemST = fmap castFMAddr . allocFMAddr
  -- {-# INLINE allocAlignedPinnedMutMemST #-}
  reallocMutMemST maddr = fmap castFMAddr . reallocFMAddr (castFMAddr maddr)
  {-# INLINE reallocMutMemST #-}


instance Unbox e => MemRead (FAddr e) where
  accessMem addr _ g o =
    unsafeInlineST $ withAddrFAddr# addr $ \addr# -> pure $! g addr# o
  {-# INLINE accessMem #-}
  isSameMem = isSameFAddr
  {-# INLINE isSameMem #-}
  byteCountMem = byteCountFAddr
  {-# INLINE byteCountMem #-}
  indexOffMem a = indexOffFAddr (castFAddr a)
  {-# INLINE indexOffMem #-}
  indexByteOffMem a = indexByteOffFAddr (castFAddr a)
  {-# INLINE indexByteOffMem #-}
  copyByteOffToMBytesMemST a si mb di c =
    withPtrFAddr a $ \ptr -> copyByteOffPtrToMBytes (castPtr ptr) si mb di c
  {-# INLINE copyByteOffToMBytesMemST #-}
  copyByteOffToPtrMemST a si mb di c =
    withPtrFAddr a $ \ptr -> copyByteOffPtrToPtr (castPtr ptr) si mb di c
  {-# INLINE copyByteOffToPtrMemST #-}
  compareByteOffToPtrMemST addr off1 ptr2 off2 c =
    withPtrFAddr addr $ \ptr1 -> pure $ compareByteOffPtrToPtr (castPtr ptr1) off1 ptr2 off2 c
  {-# INLINE compareByteOffToPtrMemST #-}
  compareByteOffToBytesMem addr off1 bytes off2 c =
    unsafeInlineIO $ withPtrFAddr addr $ \ptr1 ->
      pure $! compareByteOffPtrToBytes (castPtr ptr1) off1 bytes off2 c
  {-# INLINE compareByteOffToBytesMem #-}
  compareByteOffMem mem1 off1 addr off2 c =
    unsafeInlineIO $ withPtrFAddr addr $ \ptr2 ->
      compareByteOffToPtrMem mem1 off1 (castPtr ptr2) off2 c
  {-# INLINE compareByteOffMem #-}

instance Unbox e => MemWrite (FMAddr e) where
  accessMutMemST maddr _ g o = withAddrFMAddr# maddr $ \addr# -> g addr# o
  {-# INLINE accessMutMemST #-}
  isSameMutMem = isSameFMAddr
  {-# INLINE isSameMutMem #-}
  readOffMutMemST a = readOffFMAddr (castFMAddr a)
  {-# INLINE readOffMutMemST #-}
  readByteOffMutMemST a = readByteOffFMAddr (castFMAddr a)
  {-# INLINE readByteOffMutMemST #-}
  writeOffMutMemST a = writeOffFMAddr (castFMAddr a)
  {-# INLINE writeOffMutMemST #-}
  writeByteOffMutMemST a = writeByteOffFMAddr (castFMAddr a)
  {-# INLINE writeByteOffMutMemST #-}
  moveByteOffToPtrMutMemST src srcOff dstPtr dstOff c =
    withAddrFMAddr# src $ \ srcAddr# ->
      moveByteOffPtrToPtr (Ptr srcAddr#) srcOff dstPtr dstOff c
  {-# INLINE moveByteOffToPtrMutMemST #-}
  moveByteOffToMBytesMutMemST src srcOff dst dstOff c =
    withAddrFMAddr# src $ \ srcAddr# ->
      moveByteOffPtrToMBytes (Ptr srcAddr#) srcOff dst dstOff c
  {-# INLINE moveByteOffToMBytesMutMemST #-}
  copyByteOffMutMemST src srcOff dst dstOff c =
    withAddrFMAddr# dst $ \ dstAddr# ->
      copyByteOffToPtrMem src srcOff (Ptr dstAddr#) dstOff c
  {-# INLINE copyByteOffMutMemST #-}
  moveByteOffMutMemST src srcOff dst dstOff c =
    withAddrFMAddr# dst $ \ dstAddr# ->
      moveByteOffToPtrMutMemST src srcOff (Ptr dstAddr#) dstOff c
  {-# INLINE moveByteOffMutMemST #-}
  setByteOffMutMemST maddr = setByteOffFMAddr (castFMAddr maddr)
  {-# INLINE setByteOffMutMemST #-}
  setMutMemST maddr = setOffFMAddr (castFMAddr maddr)
  {-# INLINE setMutMemST #-}



thawFAddr :: Monad m => FAddr e -> m (FMAddr e s)
thawFAddr (FAddr addr# c fin) = pure $ FMAddr addr# c fin
{-# INLINE thawFAddr #-}

freezeFMAddr :: Primal s m => FMAddr e s -> m (FAddr e)
freezeFMAddr (FMAddr addr# c fin) = pure $ FAddr addr# c fin
{-# INLINE freezeFMAddr #-}


readFAddr :: (Primal s m, Unbox e) => FAddr e -> m e
readFAddr (FAddr addr# _ b) = do
  a <- primal (readOffAddr# addr# 0#)
  a <$ touch b
{-# INLINE readFAddr #-}

readOffFAddr :: (Primal s m, Unbox e) => FAddr e -> Off e -> m e
readOffFAddr (FAddr addr# _ b) (Off (I# off#)) = do
  a <- primal (readOffAddr# addr# off#)
  a <$ touch b
{-# INLINE readOffFAddr #-}

readByteOffFAddr :: (Primal s m, Unbox e) => FAddr e -> Off Word8 -> m e
readByteOffFAddr (FAddr addr# _ b) (Off (I# off#)) = do
  a <- primal (readOffAddr# (addr# `plusAddr#` off#) 0#)
  a <$ touch b
{-# INLINE readByteOffFAddr #-}

readFMAddr :: (Primal s m, Unbox e) => FMAddr e s -> m e
readFMAddr (FMAddr addr# _ fin) = do
  a <- primal (readOffAddr# addr# 0#)
  a <$ touch fin
{-# INLINE readFMAddr #-}

readOffFMAddr :: (Primal s m, Unbox e) => FMAddr e s -> Off e -> m e
readOffFMAddr (FMAddr addr# _ fin) (Off (I# off#)) = do
  a <- primal (readOffAddr# addr# off#)
  a <$ touch fin
{-# INLINE readOffFMAddr #-}

readByteOffFMAddr :: (Primal s m, Unbox e) => FMAddr e s -> Off Word8 -> m e
readByteOffFMAddr (FMAddr addr# _ fin) (Off (I# off#)) = do
  a <- primal (readByteOffAddr# addr# off#)
  a <$ touch fin
{-# INLINE readByteOffFMAddr #-}

writeFMAddr :: (Primal s m, Unbox e) => FMAddr e s -> e -> m ()
writeFMAddr (FMAddr addr# _ fin) e =
  primal_ $ \s -> touch# fin (writeOffAddr# addr# 0# e s)
{-# INLINE writeFMAddr #-}

writeOffFMAddr :: (Primal s m, Unbox e) => FMAddr e s -> Off e -> e -> m ()
writeOffFMAddr (FMAddr addr# _ fin) (Off (I# off#)) e =
  primal_ $ \s -> touch# fin (writeOffAddr# addr# off# e s)
{-# INLINE writeOffFMAddr #-}

writeByteOffFMAddr :: (Primal s m, Unbox e) => FMAddr e s -> Off Word8 -> e -> m ()
writeByteOffFMAddr (FMAddr addr# _ fin) (Off (I# off#)) a =
  primal_ $ \s -> touch# fin (writeByteOffAddr# addr# off# a s)
{-# INLINE writeByteOffFMAddr #-}

copyFAddrToFMAddr ::
     (Primal s m, Unbox e) => FAddr e -> Off e -> FMAddr e s -> Off e -> Count e -> m ()
copyFAddrToFMAddr src srcOff dst dstOff c =
  withPtrFAddr src $ \ srcPtr ->
    withPtrFMAddr dst $ \ dstPtr ->
      copyPtrToPtr srcPtr srcOff dstPtr dstOff c
{-# INLINE copyFAddrToFMAddr #-}

moveFMAddrToFMAddr ::
     (Primal s m, Unbox e) => FMAddr e s -> Off e -> FMAddr e s -> Off e -> Count e -> m ()
moveFMAddrToFMAddr src srcOff dst dstOff c =
  withPtrFMAddr src $ \ srcPtr ->
    withPtrFMAddr dst $ \ dstPtr ->
      movePtrToPtr srcPtr srcOff dstPtr dstOff c
{-# INLINE moveFMAddrToFMAddr #-}

setFMAddr :: (Primal s m, Unbox e) => FMAddr e s -> Count e -> e -> m ()
setFMAddr (FMAddr addr# _ mb) (Count (I# n#)) a = primal_ (setAddr# addr# n# a) >> touch mb
{-# INLINE setFMAddr #-}

setOffFMAddr :: (Primal s m, Unbox e) => FMAddr e s -> Off e -> Count e -> e -> m ()
setOffFMAddr (FMAddr addr# _ fin) (Off (I# off#)) (Count (I# n#)) a =
  primal_ (setOffAddr# addr# off# n# a) >> touch fin
{-# INLINE setOffFMAddr #-}

setByteOffFMAddr :: (Primal s m, Unbox e) => FMAddr e s -> Off Word8 -> Count e -> e -> m ()
setByteOffFMAddr (FMAddr addr# _ fin) (Off (I# off#)) (Count (I# n#)) a =
  primal_ (setByteOffAddr# addr# off# n# a) >> touch fin
{-# INLINE setByteOffFMAddr #-}



-- | Apply a pure function to the contents of a mutable variable. Returns the artifact of
-- computation.
--
-- @since 0.2.0
modifyFMAddr :: (Primal s m, Unbox a) => FMAddr a s -> (a -> (a, b)) -> m b
modifyFMAddr maddr f = modifyFMAddrM maddr (return . f)
{-# INLINE modifyFMAddr #-}

-- | Apply a pure function to the contents of a mutable variable.
--
-- @since 0.1.0
modifyFMAddr_ :: (Primal s m, Unbox a) => FMAddr a s -> (a -> a) -> m ()
modifyFMAddr_ maddr f = modifyFMAddrM_ maddr (return . f)
{-# INLINE modifyFMAddr_ #-}


-- | Apply a pure function to the contents of a mutable variable. Returns the old value.
--
-- @since 2.0.0
modifyFetchOldFMAddr :: (Primal s m, Unbox a) => FMAddr a s -> (a -> a) -> m a
modifyFetchOldFMAddr maddr f = modifyFetchOldFMAddrM maddr (return . f)
{-# INLINE modifyFetchOldFMAddr #-}

-- | Apply a pure function to the contents of a mutable variable. Returns the new value.
--
-- @since 2.0.0
modifyFetchNewFMAddr :: (Primal s m, Unbox a) => FMAddr a s -> (a -> a) -> m a
modifyFetchNewFMAddr maddr f = modifyFetchNewFMAddrM maddr (return . f)
{-# INLINE modifyFetchNewFMAddr #-}


-- | Apply a monadic action to the contents of a mutable variable. Returns the artifact of
-- computation.
--
-- @since 0.2.0
modifyFMAddrM :: (Primal s m, Unbox a) => FMAddr a s -> (a -> m (a, b)) -> m b
modifyFMAddrM maddr f = do
  a <- readFMAddr maddr
  (a', b) <- f a
  b <$ writeFMAddr maddr a'
{-# INLINE modifyFMAddrM #-}

-- | Apply a monadic action to the contents of a mutable variable. Returns the old value.
--
-- @since 2.0.0
modifyFetchOldFMAddrM :: (Primal s m, Unbox a) => FMAddr a s -> (a -> m a) -> m a
modifyFetchOldFMAddrM maddr f = do
  a <- readFMAddr maddr
  a <$ (writeFMAddr maddr =<< f a)
{-# INLINE modifyFetchOldFMAddrM #-}


-- | Apply a monadic action to the contents of a mutable variable. Returns the new value.
--
-- @since 2.0.0
modifyFetchNewFMAddrM :: (Primal s m, Unbox a) => FMAddr a s -> (a -> m a) -> m a
modifyFetchNewFMAddrM maddr f = do
  a <- readFMAddr maddr
  a' <- f a
  a' <$ writeFMAddr maddr a'
{-# INLINE modifyFetchNewFMAddrM #-}


-- | Apply a monadic action to the contents of a mutable variable.
--
-- @since 0.1.0
modifyFMAddrM_ :: (Primal s m, Unbox a) => FMAddr a s -> (a -> m a) -> m ()
modifyFMAddrM_ maddr f = readFMAddr maddr >>= f >>= writeFMAddr maddr
{-# INLINE modifyFMAddrM_ #-}

-- | Swap contents of two mutable variables. Returns their old values.
--
-- @since 0.1.0
swapFMAddrs :: (Primal s m, Unbox a) => FMAddr a s -> FMAddr a s -> m (a, a)
swapFMAddrs maddr1 maddr2 = do
  a1 <- readFMAddr maddr1
  a2 <- modifyFetchOldFMAddr maddr2 (const a1)
  (a1, a2) <$ writeFMAddr maddr1 a2
{-# INLINE swapFMAddrs #-}

-- | Swap contents of two mutable variables.
--
-- @since 0.1.0
swapFMAddrs_ :: (Primal s m, Unbox a) => FMAddr a s -> FMAddr a s -> m ()
swapFMAddrs_ maddr1 maddr2 = void $ swapFMAddrs maddr1 maddr2
{-# INLINE swapFMAddrs_ #-}


-- | Perform atomic modification of an element in the `FMAddr` at the supplied
-- index. Returns the artifact of computation @__b__@.  Offset is in number of elements,
-- rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
casOffFMAddr ::
     (Primal s m, Atomic e)
  => FMAddr e s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__e__@, rather than bytes.
  -> e -- ^ Expected old value
  -> e -- ^ New value
  -> m e
casOffFMAddr maddr (Off (I# i#)) old new =
  withAddrFMAddr# maddr $ \ addr# -> primal $ casOffAddr# addr# i# old new
{-# INLINE casOffFMAddr #-}


-- | Perform atomic modification of an element in the `FMAddr` at the supplied
-- index. Returns `True` if swap was successfull and false otherwise.  Offset is in number
-- of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
casBoolOffFMAddr ::
     (Primal s m, Atomic e)
  => FMAddr e s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__e__@, rather than bytes.
  -> e -- ^ Expected old value
  -> e -- ^ New value
  -> m Bool
casBoolOffFMAddr maddr (Off (I# i#)) old new =
  withAddrFMAddr# maddr $ \ addr# -> primal $ casBoolOffAddr# addr# i# old new
{-# INLINE casBoolOffFMAddr #-}

-- | Just like `casBoolOffFMAddr`, but also returns the actual value, which will match the
-- supplied expected value if the returned flag is `True`
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
casBoolFetchOffFMAddr ::
     (Primal s m, Atomic e)
  => FMAddr e s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__e__@, rather than bytes.
  -> e -- ^ Expected old value
  -> e -- ^ New value
  -> m (Bool, e)
casBoolFetchOffFMAddr maddr (Off (I# i#)) expected new = do
  withAddrFMAddr# maddr $ \addr# ->
    primal $ \s ->
      case casBoolOffAddr# addr# i# expected new s of
        (# s', isCasSucc #)
          | isCasSucc -> (# s', (True, new) #)
          | otherwise ->
            case readOffAddr# addr# i# s' of
              (# s'', actual #) -> (# s'', (False, actual) #)
{-# INLINE casBoolFetchOffFMAddr #-}


-- | Perform atomic read of an element in the `FMAddr` at the supplied offset. Offset is in
-- number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicReadOffFMAddr ::
     (Primal s m, Atomic e)
  => FMAddr e s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__e__@, rather than bytes.
  -> m e
atomicReadOffFMAddr maddr (Off (I# i#)) =
  withAddrFMAddr# maddr $ \ addr# -> primal $ atomicReadOffAddr# addr# i#
{-# INLINE atomicReadOffFMAddr #-}

-- | Perform atomic write of an element in the `FMAddr` at the supplied offset. Offset is in
-- number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicWriteOffFMAddr ::
     (Primal s m, Atomic e)
  => FMAddr e s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__e__@, rather than bytes.
  -> e
  -> m ()
atomicWriteOffFMAddr maddr (Off (I# i#)) e =
  withAddrFMAddr# maddr $ \ addr# -> primal_ $ atomicWriteOffAddr# addr# i# e
{-# INLINE atomicWriteOffFMAddr #-}


-- | Perform atomic modification of an element in the `FMAddr` at the supplied
-- index. Returns the artifact of computation @__b__@.  Offset is in number of elements,
-- rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicModifyOffFMAddr ::
     (Primal s m, Atomic e)
  => FMAddr e s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__e__@, rather than bytes.
  -> (e -> (e, b)) -- ^ Function that is applied to the old value and returns new value
                   -- and some artifact of computation @__b__@
  -> m b
atomicModifyOffFMAddr maddr (Off (I# i#)) f =
  withAddrFMAddr# maddr $ \ addr# -> primal $
  atomicModifyOffAddr# addr# i# $ \a ->
    case f a of
      (a', b) -> (# a', b #)
{-# INLINE atomicModifyOffFMAddr #-}

-- | Perform atomic modification of an element in the `FMAddr` at the supplied
-- index.  Offset is in number of elements, rather than bytes. Implies a full memory
-- barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicModifyOffFMAddr_ ::
     (Primal s m, Atomic e)
  => FMAddr e s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__e__@, rather than bytes.
  -> (e -> e) -- ^ Function that is applied to the current value
  -> m ()
atomicModifyOffFMAddr_ maddr (Off (I# i#)) f =
  withAddrFMAddr# maddr $ \ addr# -> primal_ $ atomicModifyOffAddr_# addr# i# f
{-# INLINE atomicModifyOffFMAddr_ #-}


-- | Perform atomic modification of an element in the `FMAddr` at the supplied
-- index. Returns the previous value.  Offset is in number of elements, rather than
-- bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicModifyFetchOldOffFMAddr ::
     (Primal s m, Atomic e)
  => FMAddr e s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__e__@, rather than bytes.
  -> (e -> e) -- ^ Function that is applied to the old value
  -> m e -- ^ Returns the old value
atomicModifyFetchOldOffFMAddr maddr (Off (I# i#)) f =
  withAddrFMAddr# maddr $ \ addr# -> primal $ atomicModifyFetchOldOffAddr# addr# i# f
{-# INLINE atomicModifyFetchOldOffFMAddr #-}


-- | Perform atomic modification of an element in the `FMAddr` at the supplied
-- index.  Offset is in number of elements, rather than bytes. Implies a full memory
-- barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicModifyFetchNewOffFMAddr ::
     (Primal s m, Atomic e)
  => FMAddr e s -- ^ Array to be mutated
  -> Off e -- ^ Index is in elements of @__e__@, rather than bytes
  -> (e -> e) -- ^ Function that is applied to the old value
  -> m e -- ^ Returns the new value
atomicModifyFetchNewOffFMAddr maddr (Off (I# i#)) f =
  withAddrFMAddr# maddr $ \ addr# -> primal $ atomicModifyFetchNewOffAddr# addr# i# f
{-# INLINE atomicModifyFetchNewOffFMAddr #-}



-- | Add a numeric value to an element of a `FMAddr`, corresponds to @(`+`)@ done
-- atomically. Returns the previous value.  Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicAddFetchOldOffFMAddr ::
     (Primal s m, AtomicCount e)
  => FMAddr e s
  -> Off e
  -> e
  -> m e
atomicAddFetchOldOffFMAddr maddr (Off (I# i#)) a =
  withAddrFMAddr# maddr $ \ addr# -> primal $ atomicAddFetchOldOffAddr# addr# i# a
{-# INLINE atomicAddFetchOldOffFMAddr #-}

-- | Add a numeric value to an element of a `FMAddr`, corresponds to @(`+`)@ done
-- atomically. Returns the new value.  Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicAddFetchNewOffFMAddr ::
     (Primal s m, AtomicCount e)
  => FMAddr e s
  -> Off e
  -> e
  -> m e
atomicAddFetchNewOffFMAddr maddr (Off (I# i#)) a =
  withAddrFMAddr# maddr $ \ addr# -> primal $ atomicAddFetchNewOffAddr# addr# i# a
{-# INLINE atomicAddFetchNewOffFMAddr #-}



-- | Subtract a numeric value from an element of a `FMAddr`, corresponds to
-- @(`-`)@ done atomically. Returns the previous value.  Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicSubFetchOldOffFMAddr ::
     (Primal s m, AtomicCount e)
  => FMAddr e s
  -> Off e
  -> e
  -> m e
atomicSubFetchOldOffFMAddr maddr (Off (I# i#)) a =
  withAddrFMAddr# maddr $ \ addr# -> primal $ atomicSubFetchOldOffAddr# addr# i# a
{-# INLINE atomicSubFetchOldOffFMAddr #-}

-- | Subtract a numeric value from an element of a `FMAddr`, corresponds to
-- @(`-`)@ done atomically. Returns the new value. Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicSubFetchNewOffFMAddr ::
     (Primal s m, AtomicCount e)
  => FMAddr e s
  -> Off e
  -> e
  -> m e
atomicSubFetchNewOffFMAddr maddr (Off (I# i#)) a =
  withAddrFMAddr# maddr $ \ addr# -> primal $ atomicSubFetchNewOffAddr# addr# i# a
{-# INLINE atomicSubFetchNewOffFMAddr #-}



-- | Binary conjunction (AND) of an element of a `FMAddr` with the supplied value,
-- corresponds to @(`Data.Bits..&.`)@ done atomically. Returns the previous value. Offset
-- is in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicAndFetchOldOffFMAddr ::
     (Primal s m, AtomicBits e)
  => FMAddr e s
  -> Off e
  -> e
  -> m e
atomicAndFetchOldOffFMAddr maddr (Off (I# i#)) a =
  withAddrFMAddr# maddr $ \ addr# -> primal $ atomicAndFetchOldOffAddr# addr# i# a
{-# INLINE atomicAndFetchOldOffFMAddr #-}

-- | Binary conjunction (AND) of an element of a `FMAddr` with the supplied value,
-- corresponds to @(`Data.Bits..&.`)@ done atomically. Returns the new value. Offset is
-- in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicAndFetchNewOffFMAddr ::
     (Primal s m, AtomicBits e)
  => FMAddr e s
  -> Off e
  -> e
  -> m e
atomicAndFetchNewOffFMAddr maddr (Off (I# i#)) a =
  withAddrFMAddr# maddr $ \ addr# -> primal $ atomicAndFetchNewOffAddr# addr# i# a
{-# INLINE atomicAndFetchNewOffFMAddr #-}



-- | Negation of binary conjunction (NAND) of an element of a `FMAddr` with the
-- supplied value, corresponds to @\\x y -> `Data.Bits.complement` (x `Data.Bits..&.` y)@
-- done atomically. Returns the previous value. Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicNandFetchOldOffFMAddr ::
     (Primal s m, AtomicBits e)
  => FMAddr e s
  -> Off e
  -> e
  -> m e
atomicNandFetchOldOffFMAddr maddr (Off (I# i#)) a =
  withAddrFMAddr# maddr $ \ addr# -> primal $ atomicNandFetchOldOffAddr# addr# i# a
{-# INLINE atomicNandFetchOldOffFMAddr #-}

-- | Negation of binary conjunction (NAND)  of an element of a `FMAddr` with the supplied
-- value, corresponds to @\\x y -> `Data.Bits.complement` (x `Data.Bits..&.` y)@ done
-- atomically. Returns the new value. Offset is in number of elements, rather than
-- bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicNandFetchNewOffFMAddr ::
     (Primal s m, AtomicBits e)
  => FMAddr e s
  -> Off e
  -> e
  -> m e
atomicNandFetchNewOffFMAddr maddr (Off (I# i#)) a =
  withAddrFMAddr# maddr $ \ addr# -> primal $ atomicNandFetchNewOffAddr# addr# i# a
{-# INLINE atomicNandFetchNewOffFMAddr #-}




-- | Binary disjunction (OR) of an element of a `FMAddr` with the supplied value,
-- corresponds to @(`Data.Bits..|.`)@ done atomically. Returns the previous value. Offset
-- is in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicOrFetchOldOffFMAddr ::
     (Primal s m, AtomicBits e)
  => FMAddr e s
  -> Off e
  -> e
  -> m e
atomicOrFetchOldOffFMAddr maddr (Off (I# i#)) a =
  withAddrFMAddr# maddr $ \ addr# -> primal $ atomicOrFetchOldOffAddr# addr# i# a
{-# INLINE atomicOrFetchOldOffFMAddr #-}

-- | Binary disjunction (OR) of an element of a `FMAddr` with the supplied value,
-- corresponds to @(`Data.Bits..|.`)@ done atomically. Returns the new value. Offset is
-- in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicOrFetchNewOffFMAddr ::
     (Primal s m, AtomicBits e)
  => FMAddr e s
  -> Off e
  -> e
  -> m e
atomicOrFetchNewOffFMAddr maddr (Off (I# i#)) a =
  withAddrFMAddr# maddr $ \ addr# -> primal $ atomicOrFetchNewOffAddr# addr# i# a
{-# INLINE atomicOrFetchNewOffFMAddr #-}



-- | Binary exclusive disjunction (XOR) of an element of a `FMAddr` with the supplied value,
-- corresponds to @`Data.Bits.xor`@ done atomically. Returns the previous value. Offset
-- is in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicXorFetchOldOffFMAddr ::
     (Primal s m, AtomicBits e)
  => FMAddr e s
  -> Off e
  -> e
  -> m e
atomicXorFetchOldOffFMAddr maddr (Off (I# i#)) a =
  withAddrFMAddr# maddr $ \ addr# -> primal $ atomicXorFetchOldOffAddr# addr# i# a
{-# INLINE atomicXorFetchOldOffFMAddr #-}

-- | Binary exclusive disjunction (XOR) of an element of a `FMAddr` with the supplied value,
-- corresponds to @`Data.Bits.xor`@ done atomically. Returns the new value. Offset is
-- in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicXorFetchNewOffFMAddr ::
     (Primal s m, AtomicBits e)
  => FMAddr e s
  -> Off e
  -> e
  -> m e
atomicXorFetchNewOffFMAddr maddr (Off (I# i#)) a =
  withAddrFMAddr# maddr $ \ addr# -> primal $ atomicXorFetchNewOffAddr# addr# i# a
{-# INLINE atomicXorFetchNewOffFMAddr #-}





-- | Binary negation (NOT) of an element of a `FMAddr`, corresponds to
-- @(`Data.Bits.complement`)@ done atomically. Returns the previous value. Offset is in
-- number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicNotFetchOldOffFMAddr ::
     (Primal s m, AtomicBits e)
  => FMAddr e s
  -> Off e
  -> m e
atomicNotFetchOldOffFMAddr maddr (Off (I# i#)) =
  withAddrFMAddr# maddr $ \ addr# -> primal $ atomicNotFetchOldOffAddr# addr# i#
{-# INLINE atomicNotFetchOldOffFMAddr #-}

-- | Binary negation (NOT) of an element of a `FMAddr`, corresponds to
-- @(`Data.Bits.complement`)@ done atomically. Returns the new value. Offset is in number
-- of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicNotFetchNewOffFMAddr ::
     (Primal s m, AtomicBits e)
  => FMAddr e s
  -> Off e
  -> m e
atomicNotFetchNewOffFMAddr maddr (Off (I# i#)) =
  withAddrFMAddr# maddr $ \ addr# -> primal $ atomicNotFetchNewOffAddr# addr# i#
{-# INLINE atomicNotFetchNewOffFMAddr #-}



