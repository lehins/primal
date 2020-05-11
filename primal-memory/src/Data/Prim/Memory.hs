{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Prim.Memory
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Memory where


import Data.ByteString.Internal hiding (toForeignPtr)
import Control.Prim.Monad
import Control.Prim.Monad.Unsafe
import Data.Prim
import Data.Prim.Memory.Bytes
import Data.Prim.Memory.Addr
import Data.Prim.Memory.Ptr
import Data.Prim.Memory.ForeignPtr
import Foreign.ForeignPtr
import Foreign.Ptr
import GHC.Exts hiding (getSizeofMutableByteArray#, isByteArrayPinned#,
                 isMutableByteArrayPinned#)
import GHC.ForeignPtr
import Control.Monad.ST
import Data.Typeable
import Data.Foldable as Foldable
import Data.List as List
import Data.Kind

-- | Access to a pointer is only safe in Haskell with pinned memory, which also means that
-- it can always be converted to a `ForeignPtr`.
class PtrAccess m p where
  withPtrAccess :: MonadPrim s m => p -> (Ptr a -> m b) -> m b
  withPtrAccess p action = toForeignPtr p >>= (`withForeignPtrPrim` action)

  toForeignPtr :: MonadPrim s m => p -> m (ForeignPtr a)

withNoHaltPtrAccess ::
     (MonadUnliftPrim s m, PtrAccess m p) => p -> (Ptr a -> m b) -> m b
withNoHaltPtrAccess p f = do
  ForeignPtr addr# ptrContents <- toForeignPtr p
  withUnliftPrim ptrContents $ f (Ptr addr#)

withForeignPtrPrim :: MonadPrim s m => ForeignPtr a -> (Ptr a -> m b) -> m b
withForeignPtrPrim (ForeignPtr addr# ptrContents) f = do
  r <- f (Ptr addr#)
  r <$ touch ptrContents

instance PtrAccess m (ForeignPtr a) where
  withPtrAccess p f = withForeignPtrPrim p (f . castPtr)
  toForeignPtr = pure . coerce

instance PtrAccess m (Bytes 'Pin) where
  withPtrAccess b f = do
    !res <- f $ toPtrBytes b
    res <$ touch b
  toForeignPtr = pure . toForeignPtrBytes

instance MonadPrim s m => PtrAccess m (MBytes 'Pin s) where
  withPtrAccess = withPtrMBytes
  toForeignPtr = pure . toForeignPtrMBytes

instance MonadPrim s m => PtrAccess m ByteString where
  withPtrAccess (PS ps s _) f = withForeignPtrPrim ps $ \ptr -> f (ptr `plusPtr` s)
  toForeignPtr (PS ps s _) = pure (coerce ps `plusForeignPtr` s)

class MemRead r where
  memIndexOff :: Prim a => r -> Off a -> a

  memIndexByteOff :: Prim a => r -> Off Word8 -> a

  -- | Source and target can't refer to the same memory chunks
  memCopyToMBytes :: (MonadPrim s m, Prim a) => r -> Off a -> MBytes p s -> Off a -> Count a -> m ()

  -- | Source and target can't refer to the same memory chunks
  memCopyToPtr :: (MonadPrim s m, Prim a) => r -> Off a -> Ptr a -> Off a -> Count a -> m ()


class MemWrite r where
  memReadOff :: (MonadPrim s m, Prim a) => r s -> Off a -> m a

  memReadByteOff :: (MonadPrim s m, Prim a) => r s -> Off Word8 -> m a

  memWriteOff :: (MonadPrim s m, Prim a) => r s -> Off a -> a -> m ()

  memWriteByteOff :: (MonadPrim s m, Prim a) => r s -> Off Word8 -> a -> m ()

  -- | Source and target can be overlapping memory chunks
  memMoveToMBytes :: (MonadPrim s m, Prim a) => r s -> Off a -> MBytes p s -> Off a -> Count a -> m ()

  -- | Source and target can be overlapping memory chunks
  memMoveToPtr :: (MonadPrim s m, Prim a) => r s -> Off a -> Ptr a -> Off a -> Count a -> m ()

  memCopy :: (MonadPrim s m, Prim a) => r s -> Off a -> r s -> Off a -> Count a -> m ()

  memMove :: (MonadPrim s m, Prim a) => r s -> Off a -> r s -> Off a -> Count a -> m ()

  memSet :: (MonadPrim s m, Prim a) => r s -> Off a -> Count a -> a -> m ()

instance MemRead ByteString where
  memIndexOff bs i = unsafeInlineIO $ withPtrAccess bs (`readOffPtr` i)
  {-# INLINE memIndexOff #-}
  memIndexByteOff bs i = unsafeInlineIO $ withPtrAccess bs (`readByteOffPtr` i)
  {-# INLINE memIndexByteOff #-}
  memCopyToMBytes bs srcOff mb dstOff c =
    withPtrAccess bs $ \(Ptr p#) -> copyPtrToMBytes (Ptr p#) srcOff mb dstOff c
  {-# INLINE memCopyToMBytes #-}
  memCopyToPtr bs srcOff dstPtr dstOff c =
    withPtrAccess bs $ \(Ptr p#) -> copyPtrToPtr (Ptr p#) srcOff dstPtr dstOff c
  {-# INLINE memCopyToPtr #-}

instance MemRead (ForeignPtr a) where
  memIndexOff fptr i = unsafeInlineIO $ withForeignPtrPrim fptr ((`readOffPtr` i) . castPtr)
  {-# INLINE memIndexOff #-}
  memIndexByteOff fptr i = unsafeInlineIO $ withForeignPtrPrim fptr ((`readByteOffPtr` i) . castPtr)
  {-# INLINE memIndexByteOff #-}
  memCopyToMBytes fptr srcOff mb dstOff c =
    withForeignPtrPrim fptr $ \(Ptr p#) -> copyPtrToMBytes (Ptr p#) srcOff mb dstOff c
  {-# INLINE memCopyToMBytes #-}
  memCopyToPtr fptr srcOff dstPtr dstOff c =
    withForeignPtrPrim fptr $ \(Ptr p#) -> copyPtrToPtr (Ptr p#) srcOff dstPtr dstOff c
  {-# INLINE memCopyToPtr #-}

newtype MemState a s = MemState { unMemState :: a }

instance MemWrite (MemState (ForeignPtr a)) where
  memReadOff (MemState fptr) i = withForeignPtrPrim fptr $ \ptr -> readOffPtr (castPtr ptr) i
  {-# INLINE memReadOff #-}
  memReadByteOff (MemState fptr) i =
    withForeignPtrPrim fptr $ \ptr -> readByteOffPtr (castPtr ptr) i
  {-# INLINE memReadByteOff #-}
  memWriteOff (MemState fptr) i a = withForeignPtrPrim fptr $ \ptr -> writeOffPtr (castPtr ptr) i a
  {-# INLINE memWriteOff #-}
  memWriteByteOff (MemState fptr) i a =
    withForeignPtrPrim fptr $ \ptr -> writeByteOffPtr (castPtr ptr) i a
  {-# INLINE memWriteByteOff #-}
  memMoveToPtr (MemState fsrc) srcOff dstPtr dstOff c =
    withForeignPtrPrim fsrc $ \srcPtr -> movePtrToPtr (castPtr srcPtr) srcOff dstPtr dstOff c
  {-# INLINE memMoveToPtr #-}
  memMoveToMBytes (MemState fsrc) srcOff dst dstOff c =
    withForeignPtrPrim fsrc $ \srcPtr -> movePtrToMBytes (castPtr srcPtr) srcOff dst dstOff c
  {-# INLINE memMoveToMBytes #-}
  memCopy (MemState fsrc) srcOff (MemState fdst) dstOff c =
    withForeignPtrPrim fsrc $ \srcPtr ->
      withForeignPtrPrim fdst $ \dstPtr ->
         copyPtrToPtr (castPtr srcPtr) srcOff (castPtr dstPtr) dstOff c
  {-# INLINE memCopy #-}
  memMove (MemState fsrc) srcOff (MemState fdst) dstOff c =
    withForeignPtrPrim fsrc $ \srcPtr ->
      withForeignPtrPrim fdst $ \dstPtr ->
         movePtrToPtr (castPtr srcPtr) srcOff (castPtr dstPtr) dstOff c
  {-# INLINE memMove #-}
  memSet (MemState fptr) off c a = withForeignPtrPrim fptr $ \ptr -> setOffPtr (castPtr ptr) off c a
  {-# INLINE memSet #-}


instance MemRead (Bytes p) where
  memIndexOff = indexBytes
  {-# INLINE memIndexOff #-}
  memIndexByteOff = indexByteOffBytes
  {-# INLINE memIndexByteOff #-}
  memCopyToMBytes = copyBytesToMBytes
  {-# INLINE memCopyToMBytes #-}
  memCopyToPtr = copyBytesToPtr
  {-# INLINE memCopyToPtr #-}


instance MemWrite (MBytes p) where
  memReadOff = readOffMBytes
  {-# INLINE memReadOff #-}
  memReadByteOff = readByteOffMBytes
  {-# INLINE memReadByteOff #-}
  memWriteOff = writeOffMBytes
  {-# INLINE memWriteOff #-}
  memWriteByteOff = writeByteOffMBytes
  {-# INLINE memWriteByteOff #-}
  memMoveToPtr = moveMBytesToPtr
  {-# INLINE memMoveToPtr #-}
  memMoveToMBytes = moveMBytesToMBytes
  {-# INLINE memMoveToMBytes #-}
  memCopy = moveMBytesToMBytes
  {-# INLINE memCopy #-}
  memMove = moveMBytesToMBytes
  {-# INLINE memMove #-}
  memSet = setMBytes
  {-# INLINE memSet #-}


instance MemRead (Addr a) where
  memIndexOff a i = unsafeInlineIO $ withAddrAddr# a $ \addr# -> readOffPtr (Ptr addr#) i
  {-# INLINE memIndexOff #-}
  memIndexByteOff a i = unsafeInlineIO $ withAddrAddr# a $ \addr# -> readByteOffPtr (Ptr addr#) i
  {-# INLINE memIndexByteOff #-}
  memCopyToMBytes a si mb di c =
    withPtrAddr a $ \ptr -> copyPtrToMBytes (castPtr ptr) si mb di c
  {-# INLINE memCopyToMBytes #-}
  memCopyToPtr a si mb di c =
    withPtrAddr a $ \ptr -> copyPtrToPtr (castPtr ptr) si mb di c
  {-# INLINE memCopyToPtr #-}

instance MemWrite (MAddr a) where
  memReadOff a = readOffMAddr (castMAddr a)
  {-# INLINE memReadOff #-}
  memReadByteOff a = readByteOffMAddr (castMAddr a)
  {-# INLINE memReadByteOff #-}
  memWriteOff a = writeOffMAddr (castMAddr a)
  {-# INLINE memWriteOff #-}
  memWriteByteOff a = writeByteOffMAddr (castMAddr a)
  {-# INLINE memWriteByteOff #-}
  memMoveToPtr src srcOff dstPtr dstOff c =
    withAddrMAddr# src $ \ srcAddr# ->
      movePtrToPtr (Ptr srcAddr#) srcOff dstPtr dstOff c
  {-# INLINE memMoveToPtr #-}
  memMoveToMBytes src srcOff dst dstOff c =
    withAddrMAddr# src $ \ srcAddr# ->
      movePtrToMBytes (Ptr srcAddr#) srcOff dst dstOff c
  {-# INLINE memMoveToMBytes #-}
  memCopy src srcOff dst = moveMAddrToMAddr (castMAddr src) srcOff (castMAddr dst)
  {-# INLINE memCopy #-}
  memMove src srcOff dst = moveMAddrToMAddr (castMAddr src) srcOff (castMAddr dst)
  {-# INLINE memMove #-}
  memSet maddr = setMAddr (castMAddr maddr)
  {-# INLINE memSet #-}


class MemWrite r => MemAlloc r where
  type MemFrozen r :: *
  memSizeOf :: MonadPrim s m => r s -> m Size

  memRawAlloc :: MonadPrim s m => Size -> m (r s)

  memThaw :: MonadPrim s m => MemFrozen r -> m (r s)

  memFreeze :: MonadPrim s m => r s -> m (MemFrozen r)


instance MemAlloc (MAddr a) where
  type MemFrozen (MAddr a) = Addr a

  memSizeOf = getSizeOfMAddr

  memRawAlloc = fmap castMAddr . allocMAddr . (coerce :: Size -> Count Word8)


-- | Allocate enough memory for number of elements. Memory is not initialized and may
-- contain garbage. Use `memAllocZero` if clean memory is needed.
--
-- [Unsafe Count] Negative element count will result in unpredictable behavior
--
-- @since 0.1.0
memAlloc :: (MemAlloc r, MonadPrim s m, Prim a) => Count a -> m (r s)
memAlloc n = memRawAlloc (coerce (countWord8 n))


-- | Same as `memAlloc`, but also use @memset@ to initialize all the new memory to zeros.
--
-- [Unsafe Count] Negative element count will result in unpredictable behavior
--
-- @since 0.1.0
memAllocZero ::
     (MemAlloc r, MonadPrim s m, Prim a) => Count a -> m (r s)
memAllocZero n = do
  m <- memAlloc n
  m <$ memSet m 0 (countWord8 n) (0 :: Word8)


newST :: (MemAlloc r, Prim a) => Count a -> (forall s . r s -> ST s b) -> (b, MemFrozen r)
newST n f = runST $ do
  m <- memAllocZero n
  res <- f m
  i <- memFreeze m
  pure (res, i)

newST_ :: (MemAlloc r, Prim a) => Count a -> (forall s . r s -> ST s b) -> MemFrozen r
newST_ n f = runST (memAllocZero n >>= \m -> f m >> memFreeze m)


-- instance Typeable p => MemAlloc (Mem (Bytes p)) where
--   type MemFrozen (Mem (Bytes p)) = Bytes p
--   memSizeOf = pure . countOfBytes . coerce
--   memAlloc = fmap Mem . freezeMBytes <=< memAlloc
--   thaw = pure . Mem
--   freeze = pure . unMem

instance Typeable p => MemAlloc (MBytes p) where
  type MemFrozen (MBytes p) = Bytes p
  memSizeOf = getSizeOfMBytes
  memRawAlloc = allocMBytes . (coerce :: Size -> Count Word8)
  memThaw = thawBytes
  memFreeze = freezeMBytes



-- loadListInternal :: (MonadPrim s m, Prim a) => Count a -> Int -> [a] -> r s -> m Ordering
-- loadListInternal (Count n) slack ys mb = do
--   let go [] !i = pure (compare i n <> compare 0 slack)
--       go (x:xs) !i
--         | i < n = writeOffMBytes mb (Off i) x >> go xs (i + 1)
--         | otherwise = pure GT
--   go ys 0
-- {-# INLINE loadListInternal #-}

-- -- | Returns `EQ` if the full list did fit into the supplied memory chunk exactly.
-- -- Otherwise it will return either `LT` if the list was smaller than allocated memory or
-- -- `GT` if the list was bigger than the available memory and did not fit into `MBytes`.
-- memLoadList :: forall a p s m . (MonadPrim s m, Prim a) => [a] -> MBytes p s -> m Ordering
-- memLoadList ys mb = do
--   (c :: Count a, slack) <- getCountRemOfMBytes mb
--   loadListInternal c slack ys mb
-- {-# INLINE loadListMBytes #-}

-- fromListBytesN ::
--   forall a p . (Prim a, Typeable p)
--   => Int
--   -> [a]
--   -> (Ordering, Bytes p)
-- fromListBytesN n xs = runST $ do
--   mb <- alloc (Count n :: Count a)
--   res <- loadListMBytes xs mb
--   (,) res <$> freezeMBytes mb
-- {-# INLINE fromListBytesN #-}



-- memcopy :: (MemRead s m r, PtrAccess m p, Prim a) => r -> Off a -> p -> Off a -> Count a -> m ()
-- memcopy src srcOff dst dstOff c =
--   withPtrAccess dst $ \dstPtr -> memCopyToPtr src srcOff dstPtr dstOff c

-- memmove :: (WriteAccess s m r, PtrAccess m p, Prim a) => r -> Off a -> p -> Off a -> Count a -> m ()
-- memmove src srcOff dst dstOff c =
--   withPtrAccess dst $ \dstPtr -> memMoveToPtr src srcOff dstPtr dstOff c


-- cloneBytes :: (MonadPrim s m, MemAlloc m (MBytes p s)) => Bytes p' -> m (Bytes p)
-- cloneBytes b = withCopyMBytes_ b pure
-- {-# INLINE cloneBytes #-}


memCountOf :: (MemAlloc r, MonadPrim s m) => r s -> m (Count Word8)
memCountOf = fmap (countSize . coerce) . memSizeOf
{-# INLINE memCountOf #-}


memCountOfRem :: (MemAlloc r, MonadPrim s m, Prim a) => r s -> m (Count a, Int)
memCountOfRem = fmap (countRemSize . coerce) . memSizeOf
{-# INLINE memCountOfRem #-}


memByteCountOf :: (MemAlloc r, MonadPrim s m) => r s -> m (Count Word8)
memByteCountOf = fmap coerce . memSizeOf
{-# INLINE memByteCountOf #-}

clone :: (MemAlloc r, MonadPrim s m) => r s -> m (r s)
clone mb = do
  n <- memByteCountOf mb
  mb' <- memAlloc n
  mb' <$ memCopy mb 0 mb' 0 n
{-# INLINE clone #-}



-- withCopyMBytes ::
--      (MonadPrim s m, MemAlloc m (MBytes p s))
--   => Bytes p'
--   -> (MBytes p s -> m a)
--   -> m (a, Bytes p)
-- withCopyMBytes b f = do
--   let n = memSizeOf b
--   mb <- memAlloc n
--   copyBytesToMBytes8 b 0 mb 0 (Count n)
--   applyFreezeMBytes f mb
-- {-# INLINE withCopyMBytes #-}


-- withCopyMBytes_ ::
--      (MonadPrim s m, MemAlloc m (MBytes p s)) => Bytes p' -> (MBytes p s -> m a) -> m (Bytes p)
-- withCopyMBytes_ b f = snd <$> withCopyMBytes b f
-- {-# INLINE withCopyMBytes_ #-}



-- import Data.ByteString (ByteString)
-- import Data.ByteString.Builder.Prim (word32LE, word64LE)
-- import Data.ByteString.Builder.Prim.Internal (runF)
-- import Data.ByteString.Unsafe
-- import Data.ByteString.Internal
-- import Data.Proxy

-- -- | Writing 8 bytes at a time in a Little-endian order gives us platform portability
-- fillPinnedMBytesWord64LE :: MonadPrim s m => g -> (g -> (Word64, g)) -> MBytes 'Pin s -> m g
-- fillPinnedMBytesWord64LE = fillPinnedMBytesWith (\a -> unsafeIOToPrim . runF word64LE a)
-- {-# INLINE fillPinnedMBytesWord64LE #-}

-- -- | Writing 4 bytes at a time in a Little-endian order gives us platform portability
-- fillPinnedMBytesWord32LE :: MonadPrim s m => g -> (g -> (Word32, g)) -> MBytes 'Pin s -> m g
-- fillPinnedMBytesWord32LE = fillPinnedMBytesWith (\a -> unsafeIOToPrim . runF word32LE a)
-- {-# INLINE fillPinnedMBytesWord32LE #-}

-- fillPinnedMBytesWith ::
--      forall a g s m. (MonadPrim s m, Prim a)
--   => (a -> Ptr Word8 -> m ())
--   -> g
--   -> (g -> (a, g))
--   -> MBytes 'Pin s
--   -> m g
-- fillPinnedMBytesWith f g0 gen64 mb =
--   withMBytesPtr mb $ \ptr0 -> do
--     (c@(Count n64 :: Count a), nrem64) <- getCountRemOfMBytes mb
--     let sz = sizeOfProxy c
--     let go g i ptr
--           | i < n64 = do
--             let (w64, g') = gen64 g
--             f w64 ptr
--             go g' (i + 1) (ptr `plusPtr` sz)
--           | otherwise = return (g, ptr)
--     (g, ptr') <- go g0 0 ptr0
--     if nrem64 == 0
--       then pure g
--       else do
--         let (w64, g') = gen64 g
--         -- In order to not mess up the byte order we write generated Word64 into a temporary
--         -- pointer and then copy only the missing bytes over to the array. It is tempting to
--         -- simply generate as many bytes as we still need using smaller type (eg. Word16),
--         -- but that would result in an inconsistent tail when total the length is slightly
--         -- varied.
--         w64mb <- newPinnedMBytes (Count 1 :: Count a)
--         writeOffMBytes w64mb 0 w64
--         withMBytesPtr w64mb (f w64)
--         copyMBytesToPtr8 w64mb 0 ptr' 0 (Count nrem64)
--         pure g'
-- {-# INLINE fillPinnedMBytesWith #-}


-- class NoConstraint a
-- instance NoConstraint a

-- class Mut f where
--   type Elt f :: * -> Constraint
--   type Elt f = NoConstraint
--   newMut :: (Elt f a, MonadPrim s m) => m (f a s)

--   readMut :: (Elt f a, MonadPrim s m) => f a s -> m a

--   writeMut :: (Elt f a, MonadPrim s m) => f a s -> a -> m ()

--   makeMut :: (Elt f a, MonadPrim s m) => a -> m (f a s)
--   makeMut a = do
--     mut <- newMut
--     mut <$ writeMut mut a


-- instance Mut MAddr where
--   type Elt MAddr = Prim
--   newMut = allocMAddr (Count 1)
--   readMut = readMAddr
--   writeMut = writeMAddr

-- class Mut f => MFunctor f where
--   mmap :: (Elt f a, Elt f b, MonadPrim s m) => (a -> b) -> f a s -> m (f b s)

-- class Mut f => MTraverse f where
--   mmapM :: (Elt f a, Elt f b, MonadPrim s m) => (a -> m b) -> f a s -> m (f b s)

-- class MFunctor f => MApplicative f where
--   pureMut :: (Elt f a, MonadPrim s m) => a -> m (f a s)
--   liftMut ::
--     (Elt f a, Elt f b, Elt f c, MonadPrim s m) => (a -> b -> m c) -> f a s -> f b s -> m (f c s)

-- class MApplicative f => MMonad f where
--   bindMut ::
--     (Elt f a, Elt f b, MonadPrim s m) => f a s -> (a -> m b) -> f b s -> m (f c s)

-- instance MFunctor MAddr where
--   mmap f maddr = do
--     Count n <- getCountOfMAddr maddr
--     maddr' <- allocMAddr (Count n)
--     let go i =
--           when (i < n) $ do
--             writeOffMAddr maddr' (Off i) . f =<< readOffMAddr maddr (Off i)
--             go (i + 1)
--     maddr' <$ go 0

-- instance MTraverse MAddr where
--   mmapM f maddr = do
--     Count n <- getCountOfMAddr maddr
--     maddr' <- allocMAddr (Count n)
--     let go i =
--           when (i < n) $ do
--             writeOffMAddr maddr' (Off i) =<< f =<< readOffMAddr maddr (Off i)
--             go (i + 1)
--     maddr' <$ go 0
