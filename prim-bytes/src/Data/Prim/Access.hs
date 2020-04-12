{-# LANGUAGE BangPatterns #-}
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

class PtrAccess m p where
  withPtrAccess :: MonadPrim s m => p -> (Ptr a -> m b) -> m b
  withPtrAccess p action = toForeignPtr p >>= (`withPtrAccess` action)

  toForeignPtr :: MonadPrim s m => p -> m (ForeignPtr a)

instance MonadPrim s m => PtrAccess m (ForeignPtr a) where
  withPtrAccess (ForeignPtr addr# ptrContents) f = do
    r <- f (Ptr addr#)
    touch ptrContents
    return r
  toForeignPtr = pure . coerce

instance PtrAccess m (Bytes 'Pin) where
  withPtrAccess b f = do
    let ptr = getBytesPtr b
    res <- f ptr
    touch b
    pure res
  toForeignPtr = pure . getBytesForeignPtr

instance MonadPrim s m => PtrAccess m (MBytes 'Pin s) where
  withPtrAccess = withMBytesPtr
  toForeignPtr = pure . getMBytesForeignPtr

instance PtrAccess m ByteString where
  withPtrAccess (PS ps s _) f = withPtrAccess ps $ \ptr -> f (ptr `plusPtr` s)
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

  

instance MonadPrim s m => ReadAccess s m ByteString where
  countPrim (PS _ _ c) = pure $ countSize c
  readPrim bs i = withPtrAccess bs (`readOffPtr` i)
  copyToMBytes bs srcOff mb dstOff c =
    withPtrAccess bs $ \(Ptr p#) -> copyPtrToMBytes (Ptr p#) srcOff mb dstOff c
  copyToPtr bs srcOff dstPtr dstOff c =
    withPtrAccess bs $ \(Ptr p#) -> copyPtrToPtr (Ptr p#) srcOff dstPtr dstOff c



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

class Alloc m a where
  malloc :: MonadPrim s m => Count Word8 -> m a

-- class AllocAligned m a where
--   mallocBytesAligned :: MonadPrim s m => Int -> Int -> m a


instance Alloc m (Bytes 'Inc) where
  malloc = malloc >=> freezeMBytes

instance Alloc m (Bytes 'Pin) where
  malloc = malloc >=> freezeMBytes

instance MonadPrim s m => Alloc m (MBytes 'Inc s) where
  malloc = allocMBytes

instance MonadPrim s m => Alloc m (MBytes 'Pin s) where
  malloc = allocPinnedMBytes

instance Alloc IO (ForeignPtr a) where
  malloc = mallocForeignPtrBytes . coerce

-- instance Alloc IO (Ptr a) where
--   malloc = new . coerce

alloc :: (Alloc m a, MonadPrim s m, Prim p) => Count p -> m a
alloc = malloc . countWord8

-- calloc n = do
--   m <- malloc n
  

memcopy :: (ReadAccess s m r, PtrAccess m p, Prim a) => r -> Off a -> p -> Off a -> Count a -> m ()
memcopy src srcOff dst dstOff c =
  withPtrAccess dst $ \dstPtr -> copyToPtr src srcOff dstPtr dstOff c

memmove :: (WriteAccess s m r, PtrAccess m p, Prim a) => r -> Off a -> p -> Off a -> Count a -> m ()
memmove src srcOff dst dstOff c =
  withPtrAccess dst $ \dstPtr -> moveToPtr src srcOff dstPtr dstOff c


-- cloneBytes :: (MonadPrim s m, Alloc m (MBytes p s)) => Bytes p' -> m (Bytes p)
-- cloneBytes b = withCopyMBytes_ b pure
-- {-# INLINE cloneBytes #-}

-- clone ::
--      (MonadPrim s m, Alloc m (MBytes pd s)) => MBytes ps s -> m (MBytes pd s)
-- clone mb = do
--   n <- countPrim mb
--   mb' <- malloc n
--   mb' <$ copyMBytesToMBytes8 mb 0 mb' 0 (Count n)
-- {-# INLINE cloneMBytes #-}



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
