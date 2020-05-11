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
  memCopyToMBytes fptr srcOff mb dstOff c =
    withForeignPtrPrim fptr $ \(Ptr p#) -> copyPtrToMBytes (Ptr p#) srcOff mb dstOff c
  {-# INLINE memCopyToMBytes #-}
  memCopyToPtr fptr srcOff dstPtr dstOff c =
    withForeignPtrPrim fptr $ \(Ptr p#) -> copyPtrToPtr (Ptr p#) srcOff dstPtr dstOff c
  {-# INLINE memCopyToPtr #-}

newtype MemState a s = MemState { unMemState :: a }

instance MemWrite (MemState (ForeignPtr a)) where
  memReadOff (MemState fptr) i = withForeignPtrPrim fptr $ \ptr -> readOffPtr (castPtr ptr) i
  memWriteOff (MemState fptr) i a = withForeignPtrPrim fptr $ \ptr -> writeOffPtr (castPtr ptr) i a
  memMoveToPtr (MemState fsrc) srcOff dstPtr dstOff c =
    withForeignPtrPrim fsrc $ \srcPtr -> movePtrToPtr (castPtr srcPtr) srcOff dstPtr dstOff c
  memMoveToMBytes (MemState fsrc) srcOff dst dstOff c =
    withForeignPtrPrim fsrc $ \srcPtr -> movePtrToMBytes (castPtr srcPtr) srcOff dst dstOff c
  memCopy (MemState fsrc) srcOff (MemState fdst) dstOff c =
    withForeignPtrPrim fsrc $ \srcPtr ->
      withForeignPtrPrim fdst $ \dstPtr ->
         copyPtrToPtr (castPtr srcPtr) srcOff (castPtr dstPtr) dstOff c
  memMove (MemState fsrc) srcOff (MemState fdst) dstOff c =
    withForeignPtrPrim fsrc $ \srcPtr ->
      withForeignPtrPrim fdst $ \dstPtr ->
         movePtrToPtr (castPtr srcPtr) srcOff (castPtr dstPtr) dstOff c
  memSet (MemState fptr) off c a = withForeignPtrPrim fptr $ \ptr -> setOffPtr (castPtr ptr) off c a


instance MemRead (Bytes p) where
  memIndexOff = indexBytes
  memIndexByteOff = indexByteOffBytes
  memCopyToMBytes = copyBytesToMBytes
  memCopyToPtr = copyBytesToPtr


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
  memMoveToMBytes = moveMBytesToMBytes
  memCopy = moveMBytesToMBytes
  memMove = moveMBytesToMBytes
  memSet = setMBytes

class MemWrite r => MemAlloc r where
  type Frozen r :: *
  memSizeOf :: MonadPrim s m => r s -> m (Count Word8)

  memAlloc :: MonadPrim s m => Count Word8 -> m (r s)

  thaw :: MonadPrim s m => Frozen r -> m (r s)

  freeze :: MonadPrim s m => r s -> m (Frozen r)

instance MemRead (Addr a) where
  memIndexOff a i = unsafeInlineIO $ withAddrAddr# a $ \addr# -> readOffPtr (Ptr addr#) i
  memIndexByteOff a i = unsafeInlineIO $ withAddrAddr# a $ \addr# -> readByteOffPtr (Ptr addr#) i
  memCopyToMBytes a si mb di c =
    withPtrAddr a $ \ptr -> copyPtrToMBytes (castPtr ptr) si mb di c
  memCopyToPtr a si mb di c =
    withPtrAddr a $ \ptr -> copyPtrToPtr (castPtr ptr) si mb di c

instance MemWrite (MAddr a) where
  memReadOff a = readOffMAddr (castMAddr a)
  memReadByteOff a = readByteOffMAddr (castMAddr a)
  memWriteOff a = writeOffMAddr (castMAddr a)
  {-# INLINE memWriteOff #-}
  memWriteByteOff a = writeByteOffMAddr (castMAddr a)
  {-# INLINE memWriteByteOff #-}
  memMoveToPtr src srcOff dstPtr dstOff c =
    withPtrMAddr src $ \ srcPtr ->
      movePtrToPtr (castPtr srcPtr) srcOff dstPtr dstOff c
  {-# INLINE memMoveToPtr #-}
  memMoveToMBytes src srcOff dst dstOff c =
    withPtrMAddr src $ \ srcPtr ->
      movePtrToMBytes (castPtr srcPtr) srcOff dst dstOff c
  {-# INLINE memMoveToMBytes #-}
  memCopy src srcOff dst = moveMAddrToMAddr (castMAddr src) srcOff (castMAddr dst)
  memMove src srcOff dst = moveMAddrToMAddr (castMAddr src) srcOff (castMAddr dst)
  memSet maddr = setMAddr (castMAddr maddr)


-- instance MemAlloc (MAddr a) where
--   type Frozen (MAddr a) = Addr a



alloc :: (MemAlloc r, MonadPrim s m, Prim p) => Count p -> m (r s)
alloc = memAlloc . countWord8

calloc ::
     (MemAlloc r, MonadPrim s m, Prim a) => Count a -> m (r s)
calloc n = do
  m <- alloc n
  m <$ memSet m 0 (countWord8 n) (0 :: Word8)


newST :: (MemAlloc r, Prim a) => Count a -> (forall s . r s -> ST s b) -> (b, Frozen r)
newST n f = runST $ do
  m <- calloc n
  res <- f m
  i <- freeze m
  pure (res, i)

newST_ :: (MemAlloc r, Prim a) => Count a -> (forall s . r s -> ST s b) -> Frozen r
newST_ n f = runST (calloc n >>= \m -> f m >> freeze m)


-- instance Typeable p => MemAlloc (Mem (Bytes p)) where
--   type Frozen (Mem (Bytes p)) = Bytes p
--   memSizeOf = pure . countOfBytes . coerce
--   memAlloc = fmap Mem . freezeMBytes <=< memAlloc
--   thaw = pure . Mem
--   freeze = pure . unMem

instance Typeable p => MemAlloc (MBytes p) where
  type Frozen (MBytes p) = Bytes p
  memSizeOf = getCountOfMBytes
  memAlloc = allocMBytes
  thaw = thawBytes
  freeze = freezeMBytes

-- -- instance MonadPrim s m => MemAlloc m (MBytes 'Pin s) where
-- --   memAlloc = allocPinnedMBytes

-- instance MemAlloc IO (ForeignPtr a) where
--   memAlloc = mallocForeignPtrBytes . coerce

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

clone :: (MemAlloc r, MemWrite r, MonadPrim s m) => r s -> m (r s)
clone mb = do
  n <- memSizeOf mb
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
