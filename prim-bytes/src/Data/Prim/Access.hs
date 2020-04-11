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


import Data.ByteString (ByteString)
import Data.ByteString.Builder.Prim (word32LE, word64LE)
import Data.ByteString.Builder.Prim.Internal (runF)
import Data.ByteString.Unsafe
import Data.ByteString.Internal
import Data.Proxy
--import Data.ByteString.Internal (ByteString(PS))
import Control.Arrow
import Control.DeepSeq
import Control.Monad
import Control.Monad.Prim
import Control.Monad.Prim.Unsafe
import Control.Monad.ST
import Data.Foldable as Foldable
import Data.Function
import Data.List as List
import Data.Maybe
import Data.Prim
import Data.Prim.Bytes
import Data.Prim.Class
import Data.Prim.Foreign (getSizeofMutableByteArray#, isByteArrayPinned#,
                          isMutableByteArrayPinned#, memmoveAddr#,
                          memmoveMutableByteArray#,
                          memmoveMutableByteArrayFromAddr#,
                          memmoveMutableByteArrayToAddr#)
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Utils
import GHC.Exts hiding (getSizeofMutableByteArray#, isByteArrayPinned#,
                 isMutableByteArrayPinned#)
import GHC.Word
import GHC.ForeignPtr
import Numeric (showHex)

class PtrAccess m p where
  withPtrAccess :: MonadPrim s m => p -> (Ptr a -> m b) -> m b

instance PtrAccess m (Bytes 'Pin) where
  withPtrAccess b f = do
    let ptr = getBytesPtr b
    res <- f ptr
    touch b
    pure res

instance MonadPrim s m => PtrAccess m (MBytes 'Pin s) where
  withPtrAccess = withMBytesPtr


instance MonadPrim s m => PtrAccess m (ForeignPtr a) where
  withPtrAccess fptr f = unsafeIOToPrim $ withForeignPtr fptr $ \ ptr -> liftPrimBase (f (castPtr ptr))

-- instance MonadPrim s m => PtrAccess m (ForeignPtr a) where
--   withPtrAccess (ForeignPtr addr# ptrContents) f = do
--     r <- f (Ptr addr#)
--     touch ptrContents
--     return r

instance MonadPrim s m => PtrAccess m (Ptr a) where
  withPtrAccess ptr f = f (castPtr ptr)

instance PtrAccess m ByteString where
  withPtrAccess (PS ps s _) f = withPtrAccess ps $ \ptr -> f (ptr `plusPtr` s)

class MonadPrim s m => ReadAccess s m r where
  count :: Prim a => r -> m (Count a)

  readPrim :: Prim a => r -> Int -> m a

  readPrimWithCount :: Prim a => r -> (Count a -> m Int) -> m a

  -- | Source and target can't be the same memory chunks
  copyToMBytes :: Prim a => r -> Int -> MBytes p s -> Int -> Count a -> m ()

  -- | Source and target can't be the same memory chunks
  copyToPtr :: Prim a => r -> Int -> Ptr a -> Int -> Count a -> m ()

class ReadAccess s m r => WriteAccess s m r where
  writeAccess :: r -> Int -> a -> m a

  -- | Source and target can be overlapping memory chunks
  moveToMBytes :: Prim a => r -> Int -> MBytes p s -> Int -> Count a -> m ()

  -- | Source and target can be overlapping memory chunks
  moveToPtr :: Prim a => r -> Int -> Ptr a -> Int -> Count a -> m ()

instance ReadAccess RealWorld IO ByteString where
  readPrim bs i = unsafeUseAsCString bs $ \(Ptr p#) -> readOffPtr (Ptr p#) i
  readPrimWithCount bs f =
    unsafeUseAsCStringLen bs $ \(Ptr p#, c) -> f (Count c) >>= readOffPtr (Ptr p#)
  copyToMBytes bs srdOff mb dstOff c =
    unsafeUseAsCString bs $ \(Ptr p#) -> copyPtrToMBytes (Ptr p#) srdOff mb dstOff c
  copyToPtr bs srdOff dstPtr dstOff c =
    unsafeUseAsCString bs $ \(Ptr p#) -> copyPtrToPtr (Ptr p#) srdOff dstPtr dstOff c
instance WriteAccess RealWorld IO ByteString where
  moveToPtr bs srdOff dstPtr dstOff c =
    unsafeUseAsCString bs $ \(Ptr p#) -> movePtrToPtr (Ptr p#) srdOff dstPtr dstOff c
  moveToMBytes bs srdOff dst dstOff c =
    unsafeUseAsCString bs $ \(Ptr p#) -> movePtrToMBytes (Ptr p#) srdOff dst dstOff c

instance (MonadPrim s m) => ReadAccess s m (Bytes p) where
  readPrim b = pure . indexBytes b
  readPrimWithCount b f = indexBytes b <$> f (countOfBytes b)
  copyToMBytes = copyBytesToMBytes
  copyToPtr = copyBytesToPtr

-- newtype MVec s a = MVec (MBytes 'Pin s)

-- instance (MonadPrim s m) => ReadAccess s m (MVec s a) where
--   readAccess (MVec mb) = readMBytes mb
--   --readWithCount mb f = getCountOfMBytes mb >>= f >>= readMBytes mb

instance (MonadPrim s m) => ReadAccess s m (MBytes p s) where
  readPrim = readMBytes
  readPrimWithCount mb f = getCountOfMBytes mb >>= f >>= readMBytes mb
  copyToMBytes = copyMBytesToMBytes
  copyToPtr = copyMBytesToPtr
instance (MonadPrim s m) => WriteAccess s m (MBytes p s) where
  moveToPtr = moveMBytesToPtr
  moveToMBytes = moveMBytesToMBytes

class Alloc m a where
  mallocBytes :: MonadPrim s m => Int -> m a

-- class AllocAligned m a where
--   mallocBytesAligned :: MonadPrim s m => Int -> Int -> m a


instance Alloc m (Bytes 'Inc) where
  mallocBytes = mallocBytes >=> freezeMBytes

instance Alloc m (Bytes 'Pin) where
  mallocBytes = mallocBytes >=> freezeMBytes

instance MonadPrim s m => Alloc m (MBytes 'Inc s) where
  mallocBytes = allocMBytes

instance MonadPrim s m => Alloc m (MBytes 'Pin s) where
  mallocBytes = allocPinnedMBytes

instance Alloc IO (ForeignPtr a) where
  mallocBytes = mallocForeignPtrBytes

malloc :: (Alloc m a, MonadPrim s m, Prim p) => Count p -> m a
malloc c@(Count n) = mallocBytes (n * sizeOfProxy c)

memcpy :: (ReadAccess s m r, PtrAccess m p, Prim a) => r -> Int -> p -> Int -> Count a -> m ()
memcpy src srcOff dst dstOff c =
  withPtrAccess dst $ \dstPtr -> copyToPtr src srcOff dstPtr dstOff c

memmove :: (ReadAccess s m r, PtrAccess m p, Prim a) => r -> Int -> p -> Int -> Count a -> m ()
memmove src srcOff dst dstOff c =
  withPtrAccess dst $ \dstPtr -> moveToPtr src srcOff dstPtr dstOff c


-- cloneBytes :: (MonadPrim s m, Alloc m (MBytes p s)) => Bytes p' -> m (Bytes p)
-- cloneBytes b = withCopyMBytes_ b pure
-- {-# INLINE cloneBytes #-}

-- cloneMBytes ::
--      (MonadPrim s m, Alloc m (MBytes pd s)) => MBytes ps s -> m (MBytes pd s)
-- cloneMBytes mb = do
--   n <- getSizeOfMBytes mb
--   mb' <- mallocBytes n
--   mb' <$ copyMBytesToMBytes8 mb 0 mb' 0 (Count n)
-- {-# INLINE cloneMBytes #-}



-- withCopyMBytes ::
--      (MonadPrim s m, Alloc m (MBytes p s))
--   => Bytes p'
--   -> (MBytes p s -> m a)
--   -> m (a, Bytes p)
-- withCopyMBytes b f = do
--   let n = sizeOfBytes b
--   mb <- mallocBytes n
--   copyBytesToMBytes8 b 0 mb 0 (Count n)
--   applyFreezeMBytes f mb
-- {-# INLINE withCopyMBytes #-}


-- withCopyMBytes_ ::
--      (MonadPrim s m, Alloc m (MBytes p s)) => Bytes p' -> (MBytes p s -> m a) -> m (Bytes p)
-- withCopyMBytes_ b f = snd <$> withCopyMBytes b f
-- {-# INLINE withCopyMBytes_ #-}
