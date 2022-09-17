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
  -- , emptyFAddr
  , isSameFAddr
  -- , castFAddr
  -- , fromBytesFAddr
  -- , curOffFAddr
  -- , byteCountFAddr
  -- , countFAddr
  , plusOffFAddr
  , plusByteOffFAddr
  -- , minusCountFAddr
  -- , minusByteCountFAddr
  -- , indexFAddr
  -- , indexOffFAddr
  -- , indexByteOffFAddr
  -- , readFAddr
  -- , readOffFAddr
  -- , readByteOffFAddr
  -- , thawFAddr
  , freezeFMAddr
  , withPtrFAddr
  , withAddrFAddr#
  -- , withNoHaltPtrFAddr
   -- * Mutable Foreign Address
  , FMAddr(..)
  , Finalizer(..)
  -- , isSameFMAddr
  , castFMAddr
  , allocFMAddr
  , allocZeroFMAddr
  , reallocFMAddr
  , reallocPtrFMAddr
  , allocWithFinalizerPtrFMAddr
  , allocWithFinalizerEnvPtrFMAddr
  -- , singletonFMAddr
  -- , setFMAddr
  -- , setOffFMAddr
  -- , setByteOffFMAddr
  -- , curOffFMAddr
  -- , getByteCountFMAddr
  -- , getCountFMAddr
  , plusOffFMAddr
  -- , plusByteOffFMAddr
  -- -- TODO:
  -- -- , minusCountFMAddr
  -- -- , minusByteCountFMAddr
  , readFMAddr
  , readOffFMAddr
  -- , readByteOffFMAddr
  -- , writeFMAddr
  , writeOffFMAddr
  -- , writeByteOffFMAddr
  , copyFAddrToFMAddr
  -- , moveFMAddrToFMAddr
  -- , modifyFMAddr
  -- , modifyFMAddr_
  -- , modifyFetchOldFMAddr
  -- , modifyFetchNewFMAddr
  -- , modifyFMAddrM
  -- , modifyFMAddrM_
  -- , modifyFetchOldFMAddrM
  -- , modifyFetchNewFMAddrM
  -- , swapFMAddrs_
  -- , swapFMAddrs
  , withPtrFMAddr
  , withAddrFMAddr#
  -- , withNoHaltPtrFMAddr
  -- , toForeignPtrAddr
  -- , toMForeignPtrFMAddr
  -- , fromForeignPtrAddr
  -- , fromForeignPtrFMAddr
  -- -- * Conversion
  -- -- ** ByteString
  -- , toByteStringAddr
  -- , toShortByteStringAddr
  -- , fromShortByteStringAddr
  -- , fromByteStringAddr
  -- , fromByteStringFMAddr
  -- -- * Atomic
  -- , casOffFMAddr
  -- , casBoolOffFMAddr
  -- , casBoolFetchOffFMAddr
  -- , atomicReadOffFMAddr
  -- , atomicWriteOffFMAddr
  -- , atomicModifyOffFMAddr
  -- , atomicModifyOffFMAddr_
  -- , atomicModifyFetchOldOffFMAddr
  -- , atomicModifyFetchNewOffFMAddr
  -- -- ** Numeric
  -- , atomicAddFetchOldOffFMAddr
  -- , atomicAddFetchNewOffFMAddr
  -- , atomicSubFetchOldOffFMAddr
  -- , atomicSubFetchNewOffFMAddr
  -- -- ** Binary
  -- , atomicAndFetchOldOffFMAddr
  -- , atomicAndFetchNewOffFMAddr
  -- , atomicNandFetchOldOffFMAddr
  -- , atomicNandFetchNewOffFMAddr
  -- , atomicOrFetchOldOffFMAddr
  -- , atomicOrFetchNewOffFMAddr
  -- , atomicXorFetchOldOffFMAddr
  -- , atomicXorFetchNewOffFMAddr
  -- , atomicNotFetchOldOffFMAddr
  -- , atomicNotFetchNewOffFMAddr
  -- -- * Prefetch
  -- -- ** Directly
  -- , prefetchAddr0
  -- , prefetchFMAddr0
  -- , prefetchAddr1
  -- , prefetchFMAddr1
  -- , prefetchAddr2
  -- , prefetchFMAddr2
  -- , prefetchAddr3
  -- , prefetchFMAddr3
  -- -- ** With offset
  -- , prefetchOffAddr0
  -- , prefetchOffFMAddr0
  -- , prefetchOffAddr1
  -- , prefetchOffFMAddr1
  -- , prefetchOffAddr2
  -- , prefetchOffFMAddr2
  -- , prefetchOffAddr3
  -- , prefetchOffFMAddr3
  -- -- * Re-export
  -- , module Primal.Element.Unbox
  ) where

import Control.Arrow (first)
import Data.ByteString.Internal
import Data.ByteString.Short.Internal
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Monoid as Monoid
import qualified Data.Semigroup as Semigroup
import Primal.Array
import Primal.Exception
import Primal.Exception.Interruptible as Interruptible
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
import Primal.Memory.Weak
import Primal.Mutable.Freeze
import Primal.Mutable.Ord
import Primal.Ref.Boxed
import Primal.Element.Unbox
import Primal.Element.Unbox.Atomic
import Unsafe.Coerce

data Finalizer
  = NoFinalizer
  | Finalizer {-# UNPACK #-} !(BRef () RW)
  | ReallocFinalizer {-# UNPACK #-} !(MBytes 'Pin RW)

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

-- instance (Eq e, Unbox e) => Eq (Addr e) where
--   (==) = eqMem @e
--   {-# INLINE (==) #-}

-- instance (Unbox e, Eq e) => MutEq (MAddr e) where
--   eqMutST m1 m2 = eqWithST isSameMAddr getSizeOfMAddr (\m -> readOffMAddr m . coerce) m1 m2
--   {-# INLINE eqMutST #-}

-- instance (Unbox e, Ord e) => Ord (Addr e) where
--   compare = compareMem @e
--   {-# INLINE compare #-}

-- instance (Unbox e, Ord e) => MutOrd (MAddr e) where
--   compareMutST m1 m2 = compareWithST isSameMAddr getSizeOfMAddr (\m -> readOffMAddr m . coerce) m1 m2
--   {-# INLINE compareMutST #-}

-- instance (Show e, Unbox e) => Show (Addr e) where
--   show a = show (toListMem a :: [e])

-- instance IsString (Addr Char) where
--   fromString = fromListMem
--   {-# INLINE fromString #-}

-- instance Unbox e => IsList (Addr e) where
--   type Item (Addr e) = e
--   fromList = fromListMem
--   {-# INLINE fromList #-}
--   fromListN n = fromListZeroMemN_ (Count n)
--   {-# INLINE fromListN #-}
--   toList = toListMem
--   {-# INLINE toList #-}

-- instance Unbox e => Semigroup.Semigroup (Addr e) where
--   (<>) = appendMem
--   {-# INLINE (<>) #-}
--   sconcat (x :| xs) = concatMem (x:xs)
--   {-# INLINE sconcat #-}
--   stimes i = cycleMemN (fromIntegral i)
--   {-# INLINE stimes #-}

-- instance Unbox e => Monoid.Monoid (Addr e) where
--   mappend = appendMem
--   {-# INLINE mappend #-}
--   mconcat = concatMem
--   {-# INLINE mconcat #-}
--   mempty = emptyAddr
--   {-# INLINE mempty #-}

-- type instance Frozen (MAddr e) = Addr e

-- instance Unbox e => MutFreeze (MAddr e) where
--   thawST = thawAddr
--   {-# INLINE thawST #-}
--   thawCloneST addr = do
--     let c = countAddr addr
--     maddr <- allocMAddr c
--     maddr <$ copyAddrToMAddr addr 0 maddr 0 c
--   {-# INLINE thawCloneST #-}
--   clone = cloneMem
--   {-# INLINE clone #-}
--   freezeMutST = freezeMAddr
--   {-# INLINE freezeMutST #-}

-- emptyAddr :: Addr e
-- emptyAddr = fromBytesAddr emptyBytes
-- {-# INLINE emptyAddr #-}

-- castAddr :: Addr e -> Addr b
-- castAddr (Addr a b) = Addr a b
-- {-# INLINE castAddr #-}

-- | Changes the element type, note that it can affect the byte size if byte count of
-- elements is different.
castFMAddr :: FMAddr e s -> FMAddr e' s
castFMAddr (FMAddr addr# (Count c) fin) = FMAddr addr# (Count c) fin
{-# INLINE castFMAddr #-}

-- | Casr the state token type.
castStateFMAddr :: FMAddr e s -> FMAddr e s'
castStateFMAddr (FMAddr addr# c fin) = FMAddr addr# c fin
{-# INLINE castStateFMAddr #-}

isSameFAddr :: FAddr e -> FAddr e -> Bool
isSameFAddr (FAddr a1# c1 _) (FAddr a2# c2 _) = isTrue# (a1# `eqAddr#` a2#) && c1 == c2
{-# INLINE isSameFAddr #-}

-- isSameMAddr :: MAddr e s -> MAddr e s -> Bool
-- isSameMAddr (MAddr a1# _) (MAddr a2# _) = isTrue# (a1# `eqAddr#` a2#)
-- {-# INLINE isSameMAddr #-}

-- instance NFData (Addr e) where
--   rnf (Addr _ _) = ()

-- instance NFData (MAddr e s) where
--   rnf (MAddr _ _) = ()

-- toBytesAddr :: Addr e -> (Bytes 'Pin, Off Word8)
-- toBytesAddr addr@(Addr _ b) = (b, curByteOffAddr addr)

-- fromBytesAddr :: Bytes 'Pin -> Addr e
-- fromBytesAddr b@(Bytes b#) = Addr (byteArrayContents# b#) b

-- fromMBytesMAddr :: MBytes 'Pin s -> MAddr e s
-- fromMBytesMAddr mb =
--   case toPtrMBytes mb of
--     Ptr addr# -> MAddr addr# mb
-- {-# INLINE fromMBytesMAddr #-}

-- singletonMAddr :: forall e m s. (Primal s m, Unbox e) => e -> m (MAddr e s)
-- singletonMAddr e = do
--   maddr <- fromMBytesMAddr <$> allocPinnedMBytes (1 :: Count e)
--   maddr <$ writeMAddr maddr e
-- {-# INLINE singletonMAddr #-}

-- allocMAddr :: forall e m s. (Primal s m, Unbox e) => Count e -> m (MAddr e s)
-- allocMAddr c = fromMBytesMAddr <$> allocPinnedMBytes c

-- allocZeroMAddr :: forall e m s. (Primal s m, Unbox e) => Count e -> m (MAddr e s)
-- allocZeroMAddr c = fromMBytesMAddr <$> allocZeroPinnedMBytes c


-- allocAlignedMAddr :: forall e m s. (Primal s m, Unbox e) => Count e -> m (MAddr e s)
-- allocAlignedMAddr c = fromMBytesMAddr <$> allocAlignedPinnedMBytes c

-- allocZeroAlignedMAddr :: forall e m s. (Primal s m, Unbox e) => Count e -> m (MAddr e s)
-- allocZeroAlignedMAddr c = fromMBytesMAddr <$> allocZeroAlignedPinnedMBytes c


-- -- | Shrink mutable address to new specified size in number of elements. The new count
-- -- must be less than or equal to the current as reported by `getCountMAddr`.
-- shrinkMAddr :: (Primal s m, Unbox e) => MAddr e s -> Count e -> m ()
-- shrinkMAddr maddr@(MAddr _ mb) c = shrinkMBytes mb (toByteCount c + coerce (curByteOffMAddr maddr))
-- {-# INLINE shrinkMAddr #-}

-- -- | Shrink mutable address to new specified size in bytes. The new count must be less
-- -- than or equal to the current as reported by `getByteCountMAddr`.
-- shrinkByteCountMAddr :: Primal s m => MAddr e s -> Count Word8 -> m ()
-- shrinkByteCountMAddr maddr@(MAddr _ mb) c = shrinkMBytes mb (c + coerce (curByteOffMAddr maddr))
-- {-# INLINE shrinkByteCountMAddr #-}


-- reallocMAddr :: (Primal s m, Unbox e) => MAddr e s -> Count e -> m (MAddr e s)
-- reallocMAddr maddr c = do
--   oldByteCount <- getByteCountMAddr maddr
--   let newByteCount = toByteCount c
--   if newByteCount <= oldByteCount
--     then maddr <$
--          when (newByteCount < oldByteCount) (shrinkByteCountMAddr maddr newByteCount)
--     else do
--       addr <- freezeMAddr maddr
--       maddr' <- allocMAddr newByteCount
--       castMAddr maddr' <$
--         copyAddrToMAddr (castAddr addr) 0 maddr' 0 oldByteCount
-- {-# INLINE reallocMAddr #-}


plusOffFAddr :: Unbox e => FAddr e -> Off e -> FAddr e
plusOffFAddr (FAddr addr# c b) off = FAddr (addr# `plusAddr#` unOffBytes# off) c b
{-# INLINE plusOffFAddr #-}

plusByteOffFAddr :: FAddr e -> Off Word8 -> FAddr e
plusByteOffFAddr (FAddr addr# c b) off = FAddr (addr# `plusAddr#` unOffBytes# off) c b
{-# INLINE plusByteOffFAddr #-}

plusOffFMAddr :: Unbox e => FMAddr e s -> Off e -> FMAddr e s
plusOffFMAddr fma off = fma { fmaAddr# = fmaAddr# fma `plusAddr#` unOffBytes# off }
{-# INLINE plusOffFMAddr #-}

-- plusByteOffMAddr :: MAddr e s -> Off Word8 -> MAddr e s
-- plusByteOffMAddr (MAddr addr# mb) off = MAddr (addr# `plusAddr#` unOffBytes# off) mb
-- {-# INLINE plusByteOffMAddr #-}

-- minusCountAddr :: Unbox e => Addr e -> Addr e -> Count e
-- minusCountAddr a1 a2 = fromByteCount $ minusByteCountAddr a1 a2
-- {-# INLINE minusCountAddr #-}

-- minusByteCountAddr :: Addr e1 -> Addr e2 -> Count Word8
-- minusByteCountAddr (Addr addr1# _) (Addr addr2# _) = Count (I# (addr1# `minusAddr#` addr2#))
-- {-# INLINE minusByteCountAddr #-}


-- curOffAddr :: Unbox e => Addr e -> Off e
-- curOffAddr a@(Addr addr# b) = (Ptr addr# `minusOffPtr` toPtrBytes b) `offForProxyTypeOf` a
-- {-# INLINE curOffAddr #-}

-- curByteOffAddr :: Addr e -> Off Word8
-- curByteOffAddr (Addr addr# b) = Ptr addr# `minusByteOffPtr` toPtrBytes b
-- {-# INLINE curByteOffAddr #-}

-- countAddr ::
--      forall e. Unbox e
--   => Addr e
--   -> Count e
-- countAddr addr@(Addr _ b) = countBytes b - coerce (curOffAddr addr)
-- {-# INLINE countAddr #-}

-- byteCountAddr :: Addr e -> Count Word8
-- byteCountAddr = countAddr . castAddr
-- {-# INLINE byteCountAddr #-}

-- getCountMAddr :: (Primal s m, Unbox e) => MAddr e s -> m (Count e)
-- getCountMAddr maddr@(MAddr _ mb) =
--   subtract (coerce (curOffMAddr maddr)) <$> getCountMBytes mb
-- {-# INLINE getCountMAddr #-}

-- getSizeOfMAddr :: (Primal s m, Unbox e) => MAddr e s -> m Size
-- getSizeOfMAddr maddr = coerce <$> getCountMAddr maddr
-- {-# INLINE getSizeOfMAddr #-}

-- getByteCountMAddr :: Primal s m => MAddr e s -> m (Count Word8)
-- getByteCountMAddr = getCountMAddr . castMAddr
-- {-# INLINE getByteCountMAddr #-}

-- indexAddr :: Unbox e => Addr e -> e
-- indexAddr addr = indexOffAddr addr 0
-- {-# INLINE indexAddr #-}

-- indexOffAddr :: Unbox e => Addr e -> Off e -> e
-- indexOffAddr addr (Off (I# off#)) =
--   unsafeInlineIO $ withAddrAddr# addr $ \addr# -> pure $! indexOffAddr# addr# off#
-- {-# INLINE indexOffAddr #-}

-- indexByteOffAddr :: Unbox e => Addr e -> Off Word8 -> e
-- indexByteOffAddr addr off = unsafeInlineIO $ readByteOffAddr addr off
-- {-# INLINE indexByteOffAddr #-}

withPtrFAddr :: Primal s m => FAddr e -> (Ptr e -> m b) -> m b
withPtrFAddr addr f = withAddrFAddr# addr $ \addr# -> f (Ptr addr#)
{-# INLINE withPtrFAddr #-}

withAddrFAddr# :: Primal s m => FAddr e -> (Addr# -> m b) -> m b
withAddrFAddr# (FAddr addr# _ fin) f = do
  a <- f addr#
  a <$ touch fin
{-# INLINE withAddrFAddr# #-}

-- withNoHaltPtrAddr :: UnliftPrimal s m => Addr e -> (Ptr e -> m b) -> m b
-- withNoHaltPtrAddr (Addr addr# b) f = keepAlive b $ f (Ptr addr#)
-- {-# INLINE withNoHaltPtrAddr #-}

-- curOffMAddr :: forall e s . Unbox e => MAddr e s -> Off e
-- curOffMAddr (MAddr addr# mb) = (Ptr addr# :: Ptr e) `minusOffPtr` toPtrMBytes mb
-- {-# INLINE curOffMAddr #-}

-- curByteOffMAddr :: forall e s . MAddr e s -> Off Word8
-- curByteOffMAddr (MAddr addr# mb) = (Ptr addr# :: Ptr e) `minusByteOffPtr` toPtrMBytes mb
-- {-# INLINE curByteOffMAddr #-}

withPtrFMAddr :: Primal s m => FMAddr e s -> (Ptr e -> m b) -> m b
withPtrFMAddr maddr f = withAddrFMAddr# maddr $ \addr# -> f (Ptr addr#)
{-# INLINE withPtrFMAddr #-}



-- toForeignPtrAddr :: Addr e -> ForeignPtr e
-- toForeignPtrAddr (Addr addr# (Bytes ba#)) = ForeignPtr addr# (PlainPtr (unsafeCoerce# ba#))


-- toMForeignPtrMAddr :: MAddr e s -> MForeignPtr e s
-- toMForeignPtrMAddr (MAddr addr# (MBytes mba#)) =
--   MForeignPtr (ForeignPtr addr# (PlainPtr (unsafeCoerce# mba#)))

-- -- | This is a unsafe cast therefore modification of `ForeignPtr` will be reflected in
-- -- resulting immutable `Addr`. Pointer created with @malloc@ cannot be converted to `Addr`
-- -- and will result in `Nothing`
-- --
-- -- @since 0.1.0
-- fromForeignPtrAddr :: ForeignPtr e -> Maybe (Addr e)
-- fromForeignPtrAddr fptr =
--   unsafePerformIO $ fromForeignPtrIO fptr >>= traverse freezeMAddr


-- -- | Discarding the original ForeignPtr will trigger finalizers that were attached to it,
-- -- because `MAddr` does not retain any finalizers. Pointer created with @malloc@ cannot be
-- -- converted to `MAddr` and will result in `Nothing`
-- --
-- -- @since 0.1.0
-- fromForeignPtrMAddr :: ForeignPtr e -> Maybe (MAddr e s)
-- fromForeignPtrMAddr fptr =
--   unsafePerformIO (fmap castStateMAddr <$> fromForeignPtrIO fptr)
--   -- case c of
--   --   PlainPtr mba#    -> Just (MAddr addr# (MBytes (unsafeCoerce# mba#)))
--   --   MallocPtr mba# _ -> Just (MAddr addr# (MBytes (unsafeCoerce# mba#)))
--   --   _                -> Nothing


-- fromForeignPtrIO :: ForeignPtr e -> IO (Maybe (MAddr e RW))
-- fromForeignPtrIO fptr =
--   onForeignPtrContents fptr checkConvert $ \_ -> pure Nothing
--   where
--     checkConvert addr# mba# checkFinalizers = do
--       hasFinalizers <- checkFinalizers
--       pure $
--         if hasFinalizers
--           then Nothing
--           else Just (MAddr addr# (MBytes mba#))

withAddrFMAddr# :: Primal s m => FMAddr e s -> (Addr# -> m b) -> m b
withAddrFMAddr# (FMAddr addr# _ fin) f = do
  a <- f addr#
  a <$ touch fin
{-# INLINE withAddrFMAddr# #-}

withNoHaltPtrFMAddr :: UnliftPrimal s m => FMAddr e s -> (Ptr e -> m b) -> m b
withNoHaltPtrFMAddr (FMAddr addr# mb) f = keepAlive mb $ f (Ptr addr#)
{-# INLINE withNoHaltPtrFMAddr #-}


-- instance MemPtr (MAddr e) where
--   toMForeignPtrMem = toMForeignPtrMAddr . castMAddr
--   {-# INLINE toMForeignPtrMem #-}
--   withPtrMemST maddr = withPtrMAddr (castMAddr maddr)
--   {-# INLINE withPtrMemST #-}
--   withNoHaltPtrMemST maddr = withNoHaltPtrMAddr (castMAddr maddr)
--   {-# INLINE withNoHaltPtrMemST #-}



-- instance Unbox e => MemAlloc (MAddr e) where
--   getByteCountMutMemST = getByteCountMAddr
--   {-# INLINE getByteCountMutMemST #-}
--   allocMutMemST = fmap castMAddr . allocMAddr
--   {-# INLINE allocMutMemST #-}
--   allocPinnedMutMemST = fmap castMAddr . allocMAddr
--   {-# INLINE allocPinnedMutMemST #-}
--   allocAlignedPinnedMutMemST = fmap castMAddr . allocAlignedMAddr
--   {-# INLINE allocAlignedPinnedMutMemST #-}
--   reallocMutMemST maddr = fmap castMAddr . reallocMAddr (castMAddr maddr)
--   {-# INLINE reallocMutMemST #-}


-- instance MemRead (Addr e) where
--   accessMem addr _ g o =
--     unsafeInlineST $ withAddrAddr# addr $ \addr# -> pure $! g addr# o
--   {-# INLINE accessMem #-}
--   isSameMem = isSameAddr
--   {-# INLINE isSameMem #-}
--   byteCountMem = byteCountAddr
--   {-# INLINE byteCountMem #-}
--   indexOffMem a = indexOffAddr (castAddr a)
--   {-# INLINE indexOffMem #-}
--   indexByteOffMem a = indexByteOffAddr (castAddr a)
--   {-# INLINE indexByteOffMem #-}
--   copyByteOffToMBytesMemST a si mb di c =
--     withPtrAddr a $ \ptr -> copyByteOffPtrToMBytes (castPtr ptr) si mb di c
--   {-# INLINE copyByteOffToMBytesMemST #-}
--   copyByteOffToPtrMemST a si mb di c =
--     withPtrAddr a $ \ptr -> copyByteOffPtrToPtr (castPtr ptr) si mb di c
--   {-# INLINE copyByteOffToPtrMemST #-}
--   compareByteOffToPtrMemST addr off1 ptr2 off2 c =
--     withPtrAddr addr $ \ptr1 -> pure $ compareByteOffPtrToPtr (castPtr ptr1) off1 ptr2 off2 c
--   {-# INLINE compareByteOffToPtrMemST #-}
--   compareByteOffToBytesMem addr off1 bytes off2 c =
--     unsafeInlineIO $ withPtrAddr addr $ \ptr1 ->
--       pure $! compareByteOffPtrToBytes (castPtr ptr1) off1 bytes off2 c
--   {-# INLINE compareByteOffToBytesMem #-}
--   compareByteOffMem mem1 off1 addr off2 c =
--     unsafeInlineIO $ withPtrAddr addr $ \ptr2 ->
--       compareByteOffToPtrMem mem1 off1 (castPtr ptr2) off2 c
--   {-# INLINE compareByteOffMem #-}

-- instance MemWrite (MAddr e) where
--   accessMutMemST maddr _ g o = withAddrMAddr# maddr $ \addr# -> g addr# o
--   {-# INLINE accessMutMemST #-}
--   isSameMutMem = isSameMAddr
--   {-# INLINE isSameMutMem #-}
--   readOffMutMemST a = readOffMAddr (castMAddr a)
--   {-# INLINE readOffMutMemST #-}
--   readByteOffMutMemST a = readByteOffMAddr (castMAddr a)
--   {-# INLINE readByteOffMutMemST #-}
--   writeOffMutMemST a = writeOffMAddr (castMAddr a)
--   {-# INLINE writeOffMutMemST #-}
--   writeByteOffMutMemST a = writeByteOffMAddr (castMAddr a)
--   {-# INLINE writeByteOffMutMemST #-}
--   moveByteOffToPtrMutMemST src srcOff dstPtr dstOff c =
--     withAddrMAddr# src $ \ srcAddr# ->
--       moveByteOffPtrToPtr (Ptr srcAddr#) srcOff dstPtr dstOff c
--   {-# INLINE moveByteOffToPtrMutMemST #-}
--   moveByteOffToMBytesMutMemST src srcOff dst dstOff c =
--     withAddrMAddr# src $ \ srcAddr# ->
--       moveByteOffPtrToMBytes (Ptr srcAddr#) srcOff dst dstOff c
--   {-# INLINE moveByteOffToMBytesMutMemST #-}
--   copyByteOffMutMemST src srcOff dst dstOff c =
--     withAddrMAddr# dst $ \ dstAddr# ->
--       copyByteOffToPtrMem src srcOff (Ptr dstAddr#) dstOff c
--   {-# INLINE copyByteOffMutMemST #-}
--   moveByteOffMutMemST src srcOff dst dstOff c =
--     withAddrMAddr# dst $ \ dstAddr# ->
--       moveByteOffToPtrMutMemST src srcOff (Ptr dstAddr#) dstOff c
--   {-# INLINE moveByteOffMutMemST #-}
--   setByteOffMutMemST maddr = setByteOffMAddr (castMAddr maddr)
--   {-# INLINE setByteOffMutMemST #-}
--   setMutMemST maddr = setOffMAddr (castMAddr maddr)
--   {-# INLINE setMutMemST #-}



-- thawAddr :: Primal s m => Addr e -> m (MAddr e s)
-- thawAddr (Addr addr# b) = MAddr addr# <$> thawBytes b
-- {-# INLINE thawAddr #-}

freezeFMAddr :: Primal s m => FMAddr e s -> m (FAddr e)
freezeFMAddr (FMAddr addr# c fin) = pure $ FAddr addr# c fin
{-# INLINE freezeFMAddr #-}


-- readAddr :: (Primal s m, Unbox e) => Addr e -> m e
-- readAddr (Addr addr# b) = do
--   a <- primal (readOffAddr# addr# 0#)
--   a <$ touch b
-- {-# INLINE readAddr #-}

-- readOffAddr :: (Primal s m, Unbox e) => Addr e -> Off e -> m e
-- readOffAddr (Addr addr# b) (Off (I# off#)) = do
--   a <- primal (readOffAddr# addr# off#)
--   a <$ touch b
-- {-# INLINE readOffAddr #-}

-- readByteOffAddr :: (Primal s m, Unbox e) => Addr e -> Off Word8 -> m e
-- readByteOffAddr (Addr addr# b) (Off (I# off#)) = do
--   a <- primal (readOffAddr# (addr# `plusAddr#` off#) 0#)
--   a <$ touch b
-- {-# INLINE readByteOffAddr #-}

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

-- readByteOffMAddr :: (Primal s m, Unbox e) => MAddr e s -> Off Word8 -> m e
-- readByteOffMAddr (MAddr addr# mb) (Off (I# off#)) = do
--   a <- primal (readByteOffAddr# addr# off#)
--   a <$ touch mb
-- {-# INLINE readByteOffMAddr #-}

-- writeMAddr :: (Primal s m, Unbox e) => MAddr e s -> e -> m ()
-- writeMAddr (MAddr addr# mb) e =
--   primal_ $ \s -> touch# mb (writeOffAddr# addr# 0# e s)
-- {-# INLINE writeMAddr #-}

writeOffFMAddr :: (Primal s m, Unbox e) => FMAddr e s -> Off e -> e -> m ()
writeOffFMAddr (FMAddr addr# _ fin) (Off (I# off#)) e =
  primal_ $ \s -> touch# fin (writeOffAddr# addr# off# e s)
{-# INLINE writeOffFMAddr #-}

-- writeByteOffMAddr :: (Primal s m, Unbox e) => MAddr e s -> Off Word8 -> e -> m ()
-- writeByteOffMAddr (MAddr addr# mb) (Off (I# off#)) a =
--   primal_ $ \s -> touch# mb (writeByteOffAddr# addr# off# a s)
-- {-# INLINE writeByteOffMAddr #-}


copyFAddrToFMAddr ::
     (Primal s m, Unbox e) => FAddr e -> Off e -> FMAddr e s -> Off e -> Count e -> m ()
copyFAddrToFMAddr src srcOff dst dstOff c =
  withPtrFAddr src $ \ srcPtr ->
    withPtrFMAddr dst $ \ dstPtr ->
      copyPtrToPtr srcPtr srcOff dstPtr dstOff c
{-# INLINE copyFAddrToFMAddr #-}

-- moveMAddrToMAddr ::
--      (Primal s m, Unbox e) => MAddr e s -> Off e -> MAddr e s -> Off e -> Count e -> m ()
-- moveMAddrToMAddr src srcOff dst dstOff c =
--   withPtrMAddr src $ \ srcPtr ->
--     withPtrMAddr dst $ \ dstPtr ->
--       movePtrToPtr srcPtr srcOff dstPtr dstOff c
-- {-# INLINE moveMAddrToMAddr #-}

-- setMAddr :: (Primal s m, Unbox e) => MAddr e s -> Count e -> e -> m ()
-- setMAddr (MAddr addr# mb) (Count (I# n#)) a = primal_ (setAddr# addr# n# a) >> touch mb
-- {-# INLINE setMAddr #-}

-- setOffMAddr :: (Primal s m, Unbox e) => MAddr e s -> Off e -> Count e -> e -> m ()
-- setOffMAddr (MAddr addr# mb) (Off (I# off#)) (Count (I# n#)) a =
--   primal_ (setOffAddr# addr# off# n# a) >> touch mb
-- {-# INLINE setOffMAddr #-}

-- setByteOffMAddr :: (Primal s m, Unbox e) => MAddr e s -> Off Word8 -> Count e -> e -> m ()
-- setByteOffMAddr (MAddr addr# mb) (Off (I# off#)) (Count (I# n#)) a =
--   primal_ (setByteOffAddr# addr# off# n# a) >> touch mb
-- {-# INLINE setByteOffMAddr #-}



-- -- | Apply a pure function to the contents of a mutable variable. Returns the artifact of
-- -- computation.
-- --
-- -- @since 0.2.0
-- modifyMAddr :: (Primal s m, Unbox a) => MAddr a s -> (a -> (a, b)) -> m b
-- modifyMAddr maddr f = modifyMAddrM maddr (return . f)
-- {-# INLINE modifyMAddr #-}

-- -- | Apply a pure function to the contents of a mutable variable.
-- --
-- -- @since 0.1.0
-- modifyMAddr_ :: (Primal s m, Unbox a) => MAddr a s -> (a -> a) -> m ()
-- modifyMAddr_ maddr f = modifyMAddrM_ maddr (return . f)
-- {-# INLINE modifyMAddr_ #-}


-- -- | Apply a pure function to the contents of a mutable variable. Returns the old value.
-- --
-- -- @since 2.0.0
-- modifyFetchOldMAddr :: (Primal s m, Unbox a) => MAddr a s -> (a -> a) -> m a
-- modifyFetchOldMAddr maddr f = modifyFetchOldMAddrM maddr (return . f)
-- {-# INLINE modifyFetchOldMAddr #-}

-- -- | Apply a pure function to the contents of a mutable variable. Returns the new value.
-- --
-- -- @since 2.0.0
-- modifyFetchNewMAddr :: (Primal s m, Unbox a) => MAddr a s -> (a -> a) -> m a
-- modifyFetchNewMAddr maddr f = modifyFetchNewMAddrM maddr (return . f)
-- {-# INLINE modifyFetchNewMAddr #-}


-- -- | Apply a monadic action to the contents of a mutable variable. Returns the artifact of
-- -- computation.
-- --
-- -- @since 0.2.0
-- modifyMAddrM :: (Primal s m, Unbox a) => MAddr a s -> (a -> m (a, b)) -> m b
-- modifyMAddrM maddr f = do
--   a <- readMAddr maddr
--   (a', b) <- f a
--   b <$ writeMAddr maddr a'
-- {-# INLINE modifyMAddrM #-}

-- -- | Apply a monadic action to the contents of a mutable variable. Returns the old value.
-- --
-- -- @since 2.0.0
-- modifyFetchOldMAddrM :: (Primal s m, Unbox a) => MAddr a s -> (a -> m a) -> m a
-- modifyFetchOldMAddrM maddr f = do
--   a <- readMAddr maddr
--   a <$ (writeMAddr maddr =<< f a)
-- {-# INLINE modifyFetchOldMAddrM #-}


-- -- | Apply a monadic action to the contents of a mutable variable. Returns the new value.
-- --
-- -- @since 2.0.0
-- modifyFetchNewMAddrM :: (Primal s m, Unbox a) => MAddr a s -> (a -> m a) -> m a
-- modifyFetchNewMAddrM maddr f = do
--   a <- readMAddr maddr
--   a' <- f a
--   a' <$ writeMAddr maddr a'
-- {-# INLINE modifyFetchNewMAddrM #-}


-- -- | Apply a monadic action to the contents of a mutable variable.
-- --
-- -- @since 0.1.0
-- modifyMAddrM_ :: (Primal s m, Unbox a) => MAddr a s -> (a -> m a) -> m ()
-- modifyMAddrM_ maddr f = readMAddr maddr >>= f >>= writeMAddr maddr
-- {-# INLINE modifyMAddrM_ #-}

-- -- | Swap contents of two mutable variables. Returns their old values.
-- --
-- -- @since 0.1.0
-- swapMAddrs :: (Primal s m, Unbox a) => MAddr a s -> MAddr a s -> m (a, a)
-- swapMAddrs maddr1 maddr2 = do
--   a1 <- readMAddr maddr1
--   a2 <- modifyFetchOldMAddr maddr2 (const a1)
--   (a1, a2) <$ writeMAddr maddr1 a2
-- {-# INLINE swapMAddrs #-}

-- -- | Swap contents of two mutable variables.
-- --
-- -- @since 0.1.0
-- swapMAddrs_ :: (Primal s m, Unbox a) => MAddr a s -> MAddr a s -> m ()
-- swapMAddrs_ maddr1 maddr2 = void $ swapMAddrs maddr1 maddr2
-- {-# INLINE swapMAddrs_ #-}



-- -- | /O(1)/ - Cast an immutable `Addr` to an immutable `ByteString`
-- --
-- -- @since 0.1.0
-- toByteStringAddr :: Addr Word8 -> ByteString
-- toByteStringAddr addr = PS (toForeignPtrAddr addr) 0 (unCount (countAddr addr))

-- -- | /O(1)/ - Cast an immutable `Addr` to an immutable `ShortByteString`
-- --
-- -- @since 0.1.0
-- toShortByteStringAddr :: Addr Word8 -> (ShortByteString, Off Word8)
-- toShortByteStringAddr = first toShortByteStringBytes . toBytesAddr

-- -- | /O(n)/ - Convert an immutable `ShortByteString` to an immutable `Addr`. In a most common
-- -- case when `ShortByteString` is not backed by pinned memory, this function will return
-- -- `Nothing`.
-- --
-- -- @since 0.1.0
-- fromShortByteStringAddr :: ShortByteString -> Addr Word8
-- fromShortByteStringAddr = fromBytesAddr . ensurePinnedBytes . fromShortByteStringBytes

-- -- | /O(1)/ - Cast an immutable `ByteString` to `Addr`. Also returns the original length of
-- -- ByteString, which will be less or equal to `countOfAddr` in the produced `Addr`.
-- --
-- -- @since 0.1.0
-- fromByteStringAddr :: ByteString -> (Addr Word8, Count Word8)
-- fromByteStringAddr (PS fptr i n) =
--   case fromForeignPtrAddr fptr of
--     Just addr -> (addr `plusOffAddr` Off i, Count n)
--     Nothing -> byteStringConvertError "ByteString was allocated outside of 'bytestring' package"

-- -- | /O(1)/ - Cast an immutable `ByteString` to a mutable `MAddr`. Also returns the
-- -- original length of ByteString, which will be less or equal to `getCountOfMAddr` in the
-- -- produced `MAddr`.
-- --
-- -- __Unsafe__ - Further modification of `MAddr` will affect the source `ByteString`
-- --
-- -- @since 0.1.0
-- fromByteStringMAddr :: ByteString -> (MAddr Word8 s, Count Word8)
-- fromByteStringMAddr (PS fptr i n) =
--   case fromForeignPtrMAddr fptr of
--     Just maddr -> (maddr `plusOffMAddr` Off i, Count n)
--     Nothing -> byteStringConvertError "It was allocated outside of 'bytestring' package"



-- -- | Perform atomic modification of an element in the `MAddr` at the supplied
-- -- index. Returns the artifact of computation @__b__@.  Offset is in number of elements,
-- -- rather than bytes. Implies a full memory barrier.
-- --
-- -- /Note/ - Bounds are not checked, therefore this function is unsafe.
-- --
-- -- @since 0.1.0
-- casOffMAddr ::
--      (Primal s m, Atomic e)
--   => MAddr e s -- ^ Array to be mutated
--   -> Off e -- ^ Index is in elements of @__e__@, rather than bytes.
--   -> e -- ^ Expected old value
--   -> e -- ^ New value
--   -> m e
-- casOffMAddr maddr (Off (I# i#)) old new =
--   withAddrMAddr# maddr $ \ addr# -> primal $ casOffAddr# addr# i# old new
-- {-# INLINE casOffMAddr #-}


-- -- | Perform atomic modification of an element in the `MAddr` at the supplied
-- -- index. Returns `True` if swap was successfull and false otherwise.  Offset is in number
-- -- of elements, rather than bytes. Implies a full memory barrier.
-- --
-- -- /Note/ - Bounds are not checked, therefore this function is unsafe.
-- --
-- -- @since 0.1.0
-- casBoolOffMAddr ::
--      (Primal s m, Atomic e)
--   => MAddr e s -- ^ Array to be mutated
--   -> Off e -- ^ Index is in elements of @__e__@, rather than bytes.
--   -> e -- ^ Expected old value
--   -> e -- ^ New value
--   -> m Bool
-- casBoolOffMAddr maddr (Off (I# i#)) old new =
--   withAddrMAddr# maddr $ \ addr# -> primal $ casBoolOffAddr# addr# i# old new
-- {-# INLINE casBoolOffMAddr #-}

-- -- | Just like `casBoolOffMAddr`, but also returns the actual value, which will match the
-- -- supplied expected value if the returned flag is `True`
-- --
-- -- /Note/ - Bounds are not checked, therefore this function is unsafe.
-- --
-- -- @since 0.1.0
-- casBoolFetchOffMAddr ::
--      (Primal s m, Atomic e)
--   => MAddr e s -- ^ Array to be mutated
--   -> Off e -- ^ Index is in elements of @__e__@, rather than bytes.
--   -> e -- ^ Expected old value
--   -> e -- ^ New value
--   -> m (Bool, e)
-- casBoolFetchOffMAddr maddr (Off (I# i#)) expected new = do
--   withAddrMAddr# maddr $ \addr# ->
--     primal $ \s ->
--       case casBoolOffAddr# addr# i# expected new s of
--         (# s', isCasSucc #)
--           | isCasSucc -> (# s', (True, new) #)
--           | otherwise ->
--             case readOffAddr# addr# i# s' of
--               (# s'', actual #) -> (# s'', (False, actual) #)
-- {-# INLINE casBoolFetchOffMAddr #-}


-- -- | Perform atomic read of an element in the `MAddr` at the supplied offset. Offset is in
-- -- number of elements, rather than bytes. Implies a full memory barrier.
-- --
-- -- /Note/ - Bounds are not checked, therefore this function is unsafe.
-- --
-- -- @since 0.1.0
-- atomicReadOffMAddr ::
--      (Primal s m, Atomic e)
--   => MAddr e s -- ^ Array to be mutated
--   -> Off e -- ^ Index is in elements of @__e__@, rather than bytes.
--   -> m e
-- atomicReadOffMAddr maddr (Off (I# i#)) =
--   withAddrMAddr# maddr $ \ addr# -> primal $ atomicReadOffAddr# addr# i#
-- {-# INLINE atomicReadOffMAddr #-}

-- -- | Perform atomic write of an element in the `MAddr` at the supplied offset. Offset is in
-- -- number of elements, rather than bytes. Implies a full memory barrier.
-- --
-- -- /Note/ - Bounds are not checked, therefore this function is unsafe.
-- --
-- -- @since 0.1.0
-- atomicWriteOffMAddr ::
--      (Primal s m, Atomic e)
--   => MAddr e s -- ^ Array to be mutated
--   -> Off e -- ^ Index is in elements of @__e__@, rather than bytes.
--   -> e
--   -> m ()
-- atomicWriteOffMAddr maddr (Off (I# i#)) e =
--   withAddrMAddr# maddr $ \ addr# -> primal_ $ atomicWriteOffAddr# addr# i# e
-- {-# INLINE atomicWriteOffMAddr #-}


-- -- | Perform atomic modification of an element in the `MAddr` at the supplied
-- -- index. Returns the artifact of computation @__b__@.  Offset is in number of elements,
-- -- rather than bytes. Implies a full memory barrier.
-- --
-- -- /Note/ - Bounds are not checked, therefore this function is unsafe.
-- --
-- -- @since 0.1.0
-- atomicModifyOffMAddr ::
--      (Primal s m, Atomic e)
--   => MAddr e s -- ^ Array to be mutated
--   -> Off e -- ^ Index is in elements of @__e__@, rather than bytes.
--   -> (e -> (e, b)) -- ^ Function that is applied to the old value and returns new value
--                    -- and some artifact of computation @__b__@
--   -> m b
-- atomicModifyOffMAddr maddr (Off (I# i#)) f =
--   withAddrMAddr# maddr $ \ addr# -> primal $
--   atomicModifyOffAddr# addr# i# $ \a ->
--     case f a of
--       (a', b) -> (# a', b #)
-- {-# INLINE atomicModifyOffMAddr #-}

-- -- | Perform atomic modification of an element in the `MAddr` at the supplied
-- -- index.  Offset is in number of elements, rather than bytes. Implies a full memory
-- -- barrier.
-- --
-- -- /Note/ - Bounds are not checked, therefore this function is unsafe.
-- --
-- -- @since 0.1.0
-- atomicModifyOffMAddr_ ::
--      (Primal s m, Atomic e)
--   => MAddr e s -- ^ Array to be mutated
--   -> Off e -- ^ Index is in elements of @__e__@, rather than bytes.
--   -> (e -> e) -- ^ Function that is applied to the current value
--   -> m ()
-- atomicModifyOffMAddr_ maddr (Off (I# i#)) f =
--   withAddrMAddr# maddr $ \ addr# -> primal_ $ atomicModifyOffAddr_# addr# i# f
-- {-# INLINE atomicModifyOffMAddr_ #-}


-- -- | Perform atomic modification of an element in the `MAddr` at the supplied
-- -- index. Returns the previous value.  Offset is in number of elements, rather than
-- -- bytes. Implies a full memory barrier.
-- --
-- -- /Note/ - Bounds are not checked, therefore this function is unsafe.
-- --
-- -- @since 0.1.0
-- atomicModifyFetchOldOffMAddr ::
--      (Primal s m, Atomic e)
--   => MAddr e s -- ^ Array to be mutated
--   -> Off e -- ^ Index is in elements of @__e__@, rather than bytes.
--   -> (e -> e) -- ^ Function that is applied to the old value
--   -> m e -- ^ Returns the old value
-- atomicModifyFetchOldOffMAddr maddr (Off (I# i#)) f =
--   withAddrMAddr# maddr $ \ addr# -> primal $ atomicModifyFetchOldOffAddr# addr# i# f
-- {-# INLINE atomicModifyFetchOldOffMAddr #-}


-- -- | Perform atomic modification of an element in the `MAddr` at the supplied
-- -- index.  Offset is in number of elements, rather than bytes. Implies a full memory
-- -- barrier.
-- --
-- -- /Note/ - Bounds are not checked, therefore this function is unsafe.
-- --
-- -- @since 0.1.0
-- atomicModifyFetchNewOffMAddr ::
--      (Primal s m, Atomic e)
--   => MAddr e s -- ^ Array to be mutated
--   -> Off e -- ^ Index is in elements of @__e__@, rather than bytes
--   -> (e -> e) -- ^ Function that is applied to the old value
--   -> m e -- ^ Returns the new value
-- atomicModifyFetchNewOffMAddr maddr (Off (I# i#)) f =
--   withAddrMAddr# maddr $ \ addr# -> primal $ atomicModifyFetchNewOffAddr# addr# i# f
-- {-# INLINE atomicModifyFetchNewOffMAddr #-}



-- -- | Add a numeric value to an element of a `MAddr`, corresponds to @(`+`)@ done
-- -- atomically. Returns the previous value.  Offset is in number of elements, rather
-- -- than bytes. Implies a full memory barrier.
-- --
-- -- /Note/ - Bounds are not checked, therefore this function is unsafe.
-- --
-- -- @since 0.1.0
-- atomicAddFetchOldOffMAddr ::
--      (Primal s m, AtomicCount e)
--   => MAddr e s
--   -> Off e
--   -> e
--   -> m e
-- atomicAddFetchOldOffMAddr maddr (Off (I# i#)) a =
--   withAddrMAddr# maddr $ \ addr# -> primal $ atomicAddFetchOldOffAddr# addr# i# a
-- {-# INLINE atomicAddFetchOldOffMAddr #-}

-- -- | Add a numeric value to an element of a `MAddr`, corresponds to @(`+`)@ done
-- -- atomically. Returns the new value.  Offset is in number of elements, rather
-- -- than bytes. Implies a full memory barrier.
-- --
-- -- /Note/ - Bounds are not checked, therefore this function is unsafe.
-- --
-- -- @since 0.1.0
-- atomicAddFetchNewOffMAddr ::
--      (Primal s m, AtomicCount e)
--   => MAddr e s
--   -> Off e
--   -> e
--   -> m e
-- atomicAddFetchNewOffMAddr maddr (Off (I# i#)) a =
--   withAddrMAddr# maddr $ \ addr# -> primal $ atomicAddFetchNewOffAddr# addr# i# a
-- {-# INLINE atomicAddFetchNewOffMAddr #-}



-- -- | Subtract a numeric value from an element of a `MAddr`, corresponds to
-- -- @(`-`)@ done atomically. Returns the previous value.  Offset is in number of elements, rather
-- -- than bytes. Implies a full memory barrier.
-- --
-- -- /Note/ - Bounds are not checked, therefore this function is unsafe.
-- --
-- -- @since 0.1.0
-- atomicSubFetchOldOffMAddr ::
--      (Primal s m, AtomicCount e)
--   => MAddr e s
--   -> Off e
--   -> e
--   -> m e
-- atomicSubFetchOldOffMAddr maddr (Off (I# i#)) a =
--   withAddrMAddr# maddr $ \ addr# -> primal $ atomicSubFetchOldOffAddr# addr# i# a
-- {-# INLINE atomicSubFetchOldOffMAddr #-}

-- -- | Subtract a numeric value from an element of a `MAddr`, corresponds to
-- -- @(`-`)@ done atomically. Returns the new value. Offset is in number of elements, rather
-- -- than bytes. Implies a full memory barrier.
-- --
-- -- /Note/ - Bounds are not checked, therefore this function is unsafe.
-- --
-- -- @since 0.1.0
-- atomicSubFetchNewOffMAddr ::
--      (Primal s m, AtomicCount e)
--   => MAddr e s
--   -> Off e
--   -> e
--   -> m e
-- atomicSubFetchNewOffMAddr maddr (Off (I# i#)) a =
--   withAddrMAddr# maddr $ \ addr# -> primal $ atomicSubFetchNewOffAddr# addr# i# a
-- {-# INLINE atomicSubFetchNewOffMAddr #-}



-- -- | Binary conjunction (AND) of an element of a `MAddr` with the supplied value,
-- -- corresponds to @(`Data.Bits..&.`)@ done atomically. Returns the previous value. Offset
-- -- is in number of elements, rather than bytes. Implies a full memory barrier.
-- --
-- -- /Note/ - Bounds are not checked, therefore this function is unsafe.
-- --
-- -- @since 0.1.0
-- atomicAndFetchOldOffMAddr ::
--      (Primal s m, AtomicBits e)
--   => MAddr e s
--   -> Off e
--   -> e
--   -> m e
-- atomicAndFetchOldOffMAddr maddr (Off (I# i#)) a =
--   withAddrMAddr# maddr $ \ addr# -> primal $ atomicAndFetchOldOffAddr# addr# i# a
-- {-# INLINE atomicAndFetchOldOffMAddr #-}

-- -- | Binary conjunction (AND) of an element of a `MAddr` with the supplied value,
-- -- corresponds to @(`Data.Bits..&.`)@ done atomically. Returns the new value. Offset is
-- -- in number of elements, rather than bytes. Implies a full memory barrier.
-- --
-- -- /Note/ - Bounds are not checked, therefore this function is unsafe.
-- --
-- -- @since 0.1.0
-- atomicAndFetchNewOffMAddr ::
--      (Primal s m, AtomicBits e)
--   => MAddr e s
--   -> Off e
--   -> e
--   -> m e
-- atomicAndFetchNewOffMAddr maddr (Off (I# i#)) a =
--   withAddrMAddr# maddr $ \ addr# -> primal $ atomicAndFetchNewOffAddr# addr# i# a
-- {-# INLINE atomicAndFetchNewOffMAddr #-}



-- -- | Negation of binary conjunction (NAND) of an element of a `MAddr` with the
-- -- supplied value, corresponds to @\\x y -> `Data.Bits.complement` (x `Data.Bits..&.` y)@
-- -- done atomically. Returns the previous value. Offset is in number of elements, rather
-- -- than bytes. Implies a full memory barrier.
-- --
-- -- /Note/ - Bounds are not checked, therefore this function is unsafe.
-- --
-- -- @since 0.1.0
-- atomicNandFetchOldOffMAddr ::
--      (Primal s m, AtomicBits e)
--   => MAddr e s
--   -> Off e
--   -> e
--   -> m e
-- atomicNandFetchOldOffMAddr maddr (Off (I# i#)) a =
--   withAddrMAddr# maddr $ \ addr# -> primal $ atomicNandFetchOldOffAddr# addr# i# a
-- {-# INLINE atomicNandFetchOldOffMAddr #-}

-- -- | Negation of binary conjunction (NAND)  of an element of a `MAddr` with the supplied
-- -- value, corresponds to @\\x y -> `Data.Bits.complement` (x `Data.Bits..&.` y)@ done
-- -- atomically. Returns the new value. Offset is in number of elements, rather than
-- -- bytes. Implies a full memory barrier.
-- --
-- -- /Note/ - Bounds are not checked, therefore this function is unsafe.
-- --
-- -- @since 0.1.0
-- atomicNandFetchNewOffMAddr ::
--      (Primal s m, AtomicBits e)
--   => MAddr e s
--   -> Off e
--   -> e
--   -> m e
-- atomicNandFetchNewOffMAddr maddr (Off (I# i#)) a =
--   withAddrMAddr# maddr $ \ addr# -> primal $ atomicNandFetchNewOffAddr# addr# i# a
-- {-# INLINE atomicNandFetchNewOffMAddr #-}




-- -- | Binary disjunction (OR) of an element of a `MAddr` with the supplied value,
-- -- corresponds to @(`Data.Bits..|.`)@ done atomically. Returns the previous value. Offset
-- -- is in number of elements, rather than bytes. Implies a full memory barrier.
-- --
-- -- /Note/ - Bounds are not checked, therefore this function is unsafe.
-- --
-- -- @since 0.1.0
-- atomicOrFetchOldOffMAddr ::
--      (Primal s m, AtomicBits e)
--   => MAddr e s
--   -> Off e
--   -> e
--   -> m e
-- atomicOrFetchOldOffMAddr maddr (Off (I# i#)) a =
--   withAddrMAddr# maddr $ \ addr# -> primal $ atomicOrFetchOldOffAddr# addr# i# a
-- {-# INLINE atomicOrFetchOldOffMAddr #-}

-- -- | Binary disjunction (OR) of an element of a `MAddr` with the supplied value,
-- -- corresponds to @(`Data.Bits..|.`)@ done atomically. Returns the new value. Offset is
-- -- in number of elements, rather than bytes. Implies a full memory barrier.
-- --
-- -- /Note/ - Bounds are not checked, therefore this function is unsafe.
-- --
-- -- @since 0.1.0
-- atomicOrFetchNewOffMAddr ::
--      (Primal s m, AtomicBits e)
--   => MAddr e s
--   -> Off e
--   -> e
--   -> m e
-- atomicOrFetchNewOffMAddr maddr (Off (I# i#)) a =
--   withAddrMAddr# maddr $ \ addr# -> primal $ atomicOrFetchNewOffAddr# addr# i# a
-- {-# INLINE atomicOrFetchNewOffMAddr #-}



-- -- | Binary exclusive disjunction (XOR) of an element of a `MAddr` with the supplied value,
-- -- corresponds to @`Data.Bits.xor`@ done atomically. Returns the previous value. Offset
-- -- is in number of elements, rather than bytes. Implies a full memory barrier.
-- --
-- -- /Note/ - Bounds are not checked, therefore this function is unsafe.
-- --
-- -- @since 0.1.0
-- atomicXorFetchOldOffMAddr ::
--      (Primal s m, AtomicBits e)
--   => MAddr e s
--   -> Off e
--   -> e
--   -> m e
-- atomicXorFetchOldOffMAddr maddr (Off (I# i#)) a =
--   withAddrMAddr# maddr $ \ addr# -> primal $ atomicXorFetchOldOffAddr# addr# i# a
-- {-# INLINE atomicXorFetchOldOffMAddr #-}

-- -- | Binary exclusive disjunction (XOR) of an element of a `MAddr` with the supplied value,
-- -- corresponds to @`Data.Bits.xor`@ done atomically. Returns the new value. Offset is
-- -- in number of elements, rather than bytes. Implies a full memory barrier.
-- --
-- -- /Note/ - Bounds are not checked, therefore this function is unsafe.
-- --
-- -- @since 0.1.0
-- atomicXorFetchNewOffMAddr ::
--      (Primal s m, AtomicBits e)
--   => MAddr e s
--   -> Off e
--   -> e
--   -> m e
-- atomicXorFetchNewOffMAddr maddr (Off (I# i#)) a =
--   withAddrMAddr# maddr $ \ addr# -> primal $ atomicXorFetchNewOffAddr# addr# i# a
-- {-# INLINE atomicXorFetchNewOffMAddr #-}





-- -- | Binary negation (NOT) of an element of a `MAddr`, corresponds to
-- -- @(`Data.Bits.complement`)@ done atomically. Returns the previous value. Offset is in
-- -- number of elements, rather than bytes. Implies a full memory barrier.
-- --
-- -- /Note/ - Bounds are not checked, therefore this function is unsafe.
-- --
-- -- @since 0.1.0
-- atomicNotFetchOldOffMAddr ::
--      (Primal s m, AtomicBits e)
--   => MAddr e s
--   -> Off e
--   -> m e
-- atomicNotFetchOldOffMAddr maddr (Off (I# i#)) =
--   withAddrMAddr# maddr $ \ addr# -> primal $ atomicNotFetchOldOffAddr# addr# i#
-- {-# INLINE atomicNotFetchOldOffMAddr #-}

-- -- | Binary negation (NOT) of an element of a `MAddr`, corresponds to
-- -- @(`Data.Bits.complement`)@ done atomically. Returns the new value. Offset is in number
-- -- of elements, rather than bytes. Implies a full memory barrier.
-- --
-- -- /Note/ - Bounds are not checked, therefore this function is unsafe.
-- --
-- -- @since 0.1.0
-- atomicNotFetchNewOffMAddr ::
--      (Primal s m, AtomicBits e)
--   => MAddr e s
--   -> Off e
--   -> m e
-- atomicNotFetchNewOffMAddr maddr (Off (I# i#)) =
--   withAddrMAddr# maddr $ \ addr# -> primal $ atomicNotFetchNewOffAddr# addr# i#
-- {-# INLINE atomicNotFetchNewOffMAddr #-}




-- prefetchAddr0 :: Primal s m => Addr e -> m ()
-- prefetchAddr0 (Addr addr# _) = primal_ (prefetchAddr0# addr# 0#)
-- {-# INLINE prefetchAddr0 #-}

-- prefetchMAddr0 :: Primal s m => MAddr e s -> m ()
-- prefetchMAddr0 (MAddr maddr# _) = primal_ (prefetchAddr0# maddr# 0#)
-- {-# INLINE prefetchMAddr0 #-}

-- prefetchAddr1 :: Primal s m => Addr e -> m ()
-- prefetchAddr1 (Addr addr# _) = primal_ (prefetchAddr1# addr# 0#)
-- {-# INLINE prefetchAddr1 #-}

-- prefetchMAddr1 :: Primal s m => MAddr e s -> m ()
-- prefetchMAddr1 (MAddr maddr# _) = primal_ (prefetchAddr1# maddr# 0#)
-- {-# INLINE prefetchMAddr1 #-}

-- prefetchAddr2 :: Primal s m => Addr e -> m ()
-- prefetchAddr2 (Addr addr# _) = primal_ (prefetchAddr2# addr# 0#)
-- {-# INLINE prefetchAddr2 #-}

-- prefetchMAddr2 :: Primal s m => MAddr e s -> m ()
-- prefetchMAddr2 (MAddr maddr# _) = primal_ (prefetchAddr2# maddr# 0#)
-- {-# INLINE prefetchMAddr2 #-}

-- prefetchAddr3 :: Primal s m => Addr e -> m ()
-- prefetchAddr3 (Addr addr# _) = primal_ (prefetchAddr3# addr# 0#)
-- {-# INLINE prefetchAddr3 #-}

-- prefetchMAddr3 :: Primal s m => MAddr e s -> m ()
-- prefetchMAddr3 (MAddr maddr# _) = primal_ (prefetchAddr3# maddr# 0#)
-- {-# INLINE prefetchMAddr3 #-}


-- prefetchOffAddr0 :: (Primal s m, Unbox e) => Addr e -> Off e -> m ()
-- prefetchOffAddr0 (Addr addr# _) off = primal_ (prefetchAddr0# addr# (unOffBytes# off))
-- {-# INLINE prefetchOffAddr0 #-}

-- prefetchOffMAddr0 :: (Primal s m, Unbox e) => MAddr e s -> Off e -> m ()
-- prefetchOffMAddr0 (MAddr maddr# _) off = primal_ (prefetchAddr0# maddr# (unOffBytes# off))
-- {-# INLINE prefetchOffMAddr0 #-}

-- prefetchOffAddr1 :: (Primal s m, Unbox e) => Addr e -> Off e -> m ()
-- prefetchOffAddr1 (Addr addr# _) off = primal_ (prefetchAddr1# addr# (unOffBytes# off))
-- {-# INLINE prefetchOffAddr1 #-}

-- prefetchOffMAddr1 :: (Primal s m, Unbox e) => MAddr e s -> Off e -> m ()
-- prefetchOffMAddr1 (MAddr maddr# _) off = primal_ (prefetchAddr1# maddr# (unOffBytes# off))
-- {-# INLINE prefetchOffMAddr1 #-}

-- prefetchOffAddr2 :: (Primal s m, Unbox e) => Addr e -> Off e -> m ()
-- prefetchOffAddr2 (Addr addr# _) off = primal_ (prefetchAddr2# addr# (unOffBytes# off))
-- {-# INLINE prefetchOffAddr2 #-}

-- prefetchOffMAddr2 :: (Primal s m, Unbox e) => MAddr e s -> Off e -> m ()
-- prefetchOffMAddr2 (MAddr maddr# _) off = primal_ (prefetchAddr2# maddr# (unOffBytes# off))
-- {-# INLINE prefetchOffMAddr2 #-}

-- prefetchOffAddr3 :: (Primal s m, Unbox e) => Addr e -> Off e -> m ()
-- prefetchOffAddr3 (Addr addr# _) off = primal_ (prefetchAddr3# addr# (unOffBytes# off))
-- {-# INLINE prefetchOffAddr3 #-}

-- prefetchOffMAddr3 :: (Primal s m, Unbox e) => MAddr e s -> Off e -> m ()
-- prefetchOffMAddr3 (MAddr maddr# _) off = primal_ (prefetchAddr3# maddr# (unOffBytes# off))
-- {-# INLINE prefetchOffMAddr3 #-}
