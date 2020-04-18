{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Prim.Access
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Access where


import Data.ByteString.Internal hiding (toForeignPtr)
import Control.Monad
import Control.Monad.Prim
import Data.Prim
import Data.Prim.Bytes
import Data.Prim.Ptr
import Foreign.ForeignPtr
import Foreign.Ptr
import GHC.Exts hiding (getSizeofMutableByteArray#, isByteArrayPinned#,
                 isMutableByteArrayPinned#)
import GHC.ForeignPtr
import Control.Monad.ST
import Data.Typeable

-- | Access to a pointer is only safe in Haskell with pinned memory, which also means that
-- it can always be converted to a `ForeignPtr`.
class PtrAccess m p where
  withPtrAccess :: MonadPrim s m => p -> (Ptr a -> m b) -> m b
  withPtrAccess p action = toForeignPtr p >>= (`withForeignPtrPrim` action)

  toForeignPtr :: MonadPrim s m => p -> m (ForeignPtr a)

withNoHaltPtrAccess ::
     (MonadPrimBase s n, MonadPrim s m, PtrAccess m p) => p -> (Ptr a -> n b) -> m b
withNoHaltPtrAccess p f = do
  ForeignPtr addr# ptrContents <- toForeignPtr p
  withPrimBase ptrContents $ f (Ptr addr#)

withForeignPtrPrim :: MonadPrim s m => ForeignPtr a1 -> (Ptr a2 -> m b) -> m b
withForeignPtrPrim (ForeignPtr addr# ptrContents) f = do
  r <- f (Ptr addr#)
  touch ptrContents
  return r

instance MonadPrim RealWorld m => PtrAccess m (ForeignPtr a) where
  withPtrAccess = withForeignPtrPrim
  toForeignPtr = pure . coerce

instance PtrAccess m (Bytes 'Pin) where
  withPtrAccess b f = do
    !res <- f $ getPtrBytes b
    res <$ touch b
  toForeignPtr = pure . getBytesForeignPtr

instance MonadPrim s m => PtrAccess m (MBytes 'Pin s) where
  withPtrAccess = withPtrMBytes
  toForeignPtr = pure . getMBytesForeignPtr

instance MonadPrim s m => PtrAccess m ByteString where
  withPtrAccess (PS ps s _) f = withForeignPtrPrim ps $ \ptr -> f (ptr `plusPtr` s)
  toForeignPtr (PS ps s _) = pure (coerce ps `plusForeignPtr` s)


class MonadPrim s m => ReadAccess s m r where
  countPrim :: Prim a => r -> m (Count a)

  readPrim :: Prim a => r -> Off a -> m a

  -- | Source and target can't be the same memory chunks
  copyToMBytes :: Prim a => r -> Off a -> MBytes p s -> Off a -> Count a -> m ()

  -- | Source and target can't be the same memory chunks
  copyToPtr :: Prim a => r -> Off a -> Ptr a -> Off a -> Count a -> m ()

class ReadAccess s m r => WriteAccess s m r where
  writePrim :: Prim a => r -> Off a -> a -> m ()

  -- | Source and target can be overlapping memory chunks
  moveToMBytes :: Prim a => r -> Off a -> MBytes p s -> Off a -> Count a -> m ()

  -- | Source and target can be overlapping memory chunks
  moveToPtr :: Prim a => r -> Off a -> Ptr a -> Off a -> Count a -> m ()

  copy :: Prim a => r -> Off a -> r -> Off a -> Count a -> m ()
  default copy :: (PtrAccess m r, Prim a) => r -> Off a -> r -> Off a -> Count a -> m ()
  copy src srcOff dst dstOff n =
    withPtrAccess dst $ \dstPtr -> copyToPtr src srcOff dstPtr dstOff n

  move :: Prim a => r -> Off a -> r -> Off a -> Count a -> m ()
  default move :: (PtrAccess m r, Prim a) => r -> Off a -> r -> Off a -> Count a -> m ()
  move src srcOff dst dstOff n =
    withPtrAccess dst $ \dstPtr -> copyToPtr src srcOff dstPtr dstOff n

  set :: Prim a => r -> Off a -> Count a -> a -> m ()

instance MonadPrim s m => ReadAccess s m ByteString where
  countPrim (PS _ _ c) = pure $ countSize c
  readPrim bs i = withPtrAccess bs (`readOffPtr` i)
  copyToMBytes bs srcOff mb dstOff c =
    withPtrAccess bs $ \(Ptr p#) -> copyPtrToMBytes (Ptr p#) srcOff mb dstOff c
  copyToPtr bs srcOff dstPtr dstOff c =
    withPtrAccess bs $ \(Ptr p#) -> copyPtrToPtr (Ptr p#) srcOff dstPtr dstOff c

-- instance MonadPrim s m => ReadAccess s m (ForeignPtr a) where
--   countPrim (PS _ _ c) = pure $ countSize c
--   readPrim bs i = withPtrAccess bs (`readOffPtr` i)
--   copyToMBytes bs srcOff mb dstOff c =
--     withPtrAccess bs $ \(Ptr p#) -> copyPtrToMBytes (Ptr p#) srcOff mb dstOff c
--   copyToPtr bs srcOff dstPtr dstOff c =
--     withPtrAccess bs $ \(Ptr p#) -> copyPtrToPtr (Ptr p#) srcOff dstPtr dstOff c



instance MonadPrim s m => ReadAccess s m (Bytes p) where
  countPrim = pure . countOfBytes
  readPrim b = pure . indexBytes b
  copyToMBytes = copyBytesToMBytes
  copyToPtr = copyBytesToPtr

-- newtype MVec s a = MVec (MBytes 'Pin s)

-- instance (MonadPrim s m) => ReadAccess s m (MVec s a) where
--   readAccess (MVec mb) = readMBytes mb
--   --readWithCount mb f = getCountOfMBytes mb >>= f >>= readMBytes mb


instance (MonadPrim s m) => ReadAccess s m (MBytes p s) where
  countPrim = getCountOfMBytes
  readPrim = readMBytes
  copyToMBytes = copyMBytesToMBytes
  copyToPtr = copyMBytesToPtr
instance (MonadPrim s m) => WriteAccess s m (MBytes p s) where
  writePrim = writeMBytes
  moveToPtr = moveMBytesToPtr
  moveToMBytes = moveMBytesToMBytes
  copy = copyMBytesToMBytes
  move = moveMBytesToMBytes
  set = setMBytes

class Alloc m a where
  malloc :: MonadPrim s m => Count Word8 -> m a

-- class AllocAligned m a where
--   mallocBytesAligned :: MonadPrim s m => Int -> Int -> m a


instance Typeable p => Alloc m (Bytes p) where
  malloc = malloc >=> freezeMBytes

instance (Typeable p, MonadPrim s m) => Alloc m (MBytes p s) where
  malloc = allocMBytes

-- instance MonadPrim s m => Alloc m (MBytes 'Pin s) where
--   malloc = allocPinnedMBytes

instance Alloc IO (ForeignPtr a) where
  malloc = mallocForeignPtrBytes . coerce

fromListBytesN ::
  forall a p . (Prim a, Typeable p)
  => Int
  -> [a]
  -> (Ordering, Bytes p)
fromListBytesN n xs = runST $ do
  mb <- alloc (Count n :: Count a)
  res <- loadListMBytes xs mb
  (,) res <$> freezeMBytes mb
{-# INLINE fromListBytesN #-}

alloc :: (Alloc m a, MonadPrim s m, Prim p) => Count p -> m a
alloc = malloc . countWord8

-- calloc n = do
--   m <- malloc n


-- memcopy :: (ReadAccess s m r, PtrAccess m p, Prim a) => r -> Off a -> p -> Off a -> Count a -> m ()
-- memcopy src srcOff dst dstOff c =
--   withPtrAccess dst $ \dstPtr -> copyToPtr src srcOff dstPtr dstOff c

-- memmove :: (WriteAccess s m r, PtrAccess m p, Prim a) => r -> Off a -> p -> Off a -> Count a -> m ()
-- memmove src srcOff dst dstOff c =
--   withPtrAccess dst $ \dstPtr -> moveToPtr src srcOff dstPtr dstOff c


-- cloneBytes :: (MonadPrim s m, Alloc m (MBytes p s)) => Bytes p' -> m (Bytes p)
-- cloneBytes b = withCopyMBytes_ b pure
-- {-# INLINE cloneBytes #-}

clone :: (Alloc m r, WriteAccess s m r) => r -> m r
clone mb = do
  n <- countPrim mb
  mb' <- malloc n
  mb' <$ copy mb 0 mb' 0 n
{-# INLINE clone #-}



-- withCopyMBytes ::
--      (MonadPrim s m, Alloc m (MBytes p s))
--   => Bytes p'
--   -> (MBytes p s -> m a)
--   -> m (a, Bytes p)
-- withCopyMBytes b f = do
--   let n = sizeOfBytes b
--   mb <- malloc n
--   copyBytesToMBytes8 b 0 mb 0 (Count n)
--   applyFreezeMBytes f mb
-- {-# INLINE withCopyMBytes #-}


-- withCopyMBytes_ ::
--      (MonadPrim s m, Alloc m (MBytes p s)) => Bytes p' -> (MBytes p s -> m a) -> m (Bytes p)
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
--         writeMBytes w64mb 0 w64
--         withMBytesPtr w64mb (f w64)
--         copyMBytesToPtr8 w64mb 0 ptr' 0 (Count nrem64)
--         pure g'
-- {-# INLINE fillPinnedMBytesWith #-}

