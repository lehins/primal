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
-- Module      : Data.Prim.Bytes
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Bytes
  ( Bytes(..)
  , MBytes(..)
  , Pinned(..)
  , Count(..)
  , Ptr(..)
  --, ForeignPtr
  , isPinnedBytes
  , isPinnedMBytes
  , toPinnedBytes
  , toPinnedMBytes
  , ensurePinnedBytes
  , ensurePinnedMBytes
  , cloneBytes
  , cloneMBytes
  , indexBytes
  , countOfBytes
  , countRemOfBytes
  , fromListBytes
  , fromListBytesN
  , toListBytes
  , sameBytes
  -- * Mutable
  -- ** To/From immutable
  , thawBytes
  , freezeMBytes
  -- ** Construction
  , allocMBytes
  , allocPinnedMBytes
  , allocPinnedAlignedMBytes
  , callocMBytes
  , callocPinnedMBytes
  , callocPinnedAlignedMBytes
  , newMBytes
  , newPinnedMBytes
  , newPinnedAlignedMBytes
  , cnewMBytes
  , cnewPinnedMBytes
  , cnewPinnedAlignedMBytes
  , loadListMBytes
  , showsBytesHex
  , zerosMBytes
  , coerceStateMBytes
  -- ** Modifying data
  , withMBytes
  , withMBytes_
  , withCopyMBytes
  , withCopyMBytes_
  , copyBytesToPtr
  , copyMBytesToPtr
  , applyFreezeMBytes
  -- ** Moving data
  , copyBytesToMBytes
  , copyMBytesToMBytes
  -- * Size
  , getSizeOfMBytes
  , getCountOfMBytes
  , getCountRemOfMBytes
  , readMBytes
  , writeMBytes
  , setMBytes
  , withMBytesPtr
  , getBytesPtr
  , getMBytesPtr
  , module Data.Prim
  -- * Experimental
  , fillPinnedMBytesWord64LE
  , fillPinnedMBytesWith
  , Alloc(..)
  , malloc
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Builder.Prim (word32LE, word64LE)
import Data.ByteString.Builder.Prim.Internal (runF)
import Data.ByteString.Unsafe
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
import Numeric (showHex)

class PtrAccess m p a | p -> a where
  withPtrAccess :: MonadPrim s m => p -> (Ptr a -> m b) -> m b

instance PtrAccess m (Bytes 'Pin) Word8 where
  withPtrAccess b f = do
    let ptr = getBytesPtr b
    res <- f ptr
    touch b
    pure res

instance MonadPrim s m => PtrAccess m (MBytes 'Pin s) Word8 where
  withPtrAccess = withMBytesPtr

-- TODO: MonadUnliftIO
instance PtrAccess IO (ForeignPtr a) a where
  withPtrAccess = withForeignPtr

instance PtrAccess IO (Ptr a) a where
  withPtrAccess = (&)

instance PtrAccess IO ByteString CChar where
  withPtrAccess = unsafeUseAsCString

-- class (MonadPrim s m, Prim a) => WriteAccess s m r a where
--   writeAccess :: r -> Int -> a -> m a

--   writeWithCount :: r -> (Count a -> m (Int, a)) -> m a

class (MonadPrim s m, Prim a) => ReadAccess s m r a where
  readAccess :: r -> Int -> m a

  readWithCount :: r -> (Count a -> m Int) -> m a

  -- | Source and target can't be the same memory chunks
  copyToMBytes :: r -> Int -> MBytes p s -> Int -> Count a -> m ()

  -- | Source and target can't be the same memory chunks
  copyToPtr :: r -> Int -> Ptr a -> Int -> Count a -> m ()

  -- | Source and target can be overlapping memory chunks
  moveToMBytes :: r -> Int -> MBytes p s -> Int -> Count a -> m ()

  -- | Source and target can be overlapping memory chunks
  moveToPtr :: r -> Int -> Ptr a -> Int -> Count a -> m ()

instance ReadAccess RealWorld IO ByteString Word8 where
  readAccess bs i = unsafeUseAsCString bs $ \(Ptr p#) -> peekElemOff (Ptr p#) i
  readWithCount bs f =
    unsafeUseAsCStringLen bs $ \(Ptr p#, c) -> f (Count c) >>= peekElemOff (Ptr p#)
  copyToMBytes bs srdOff mb dstOff c =
    unsafeUseAsCString bs $ \(Ptr p#) -> copyPtrToMBytes8 (Ptr p#) srdOff mb dstOff c
  copyToPtr bs srdOff dstPtr dstOff c =
    unsafeUseAsCString bs $ \(Ptr p#) -> copyPtrToPtr8 (Ptr p#) srdOff dstPtr dstOff c
  moveToPtr bs srdOff dstPtr dstOff c =
    unsafeUseAsCString bs $ \(Ptr p#) -> movePtrToPtr8 (Ptr p#) srdOff dstPtr dstOff c
  moveToMBytes bs srdOff dst dstOff c =
    unsafeUseAsCString bs $ \(Ptr p#) -> movePtrToMBytes8 (Ptr p#) srdOff dst dstOff c

instance ReadAccess RealWorld IO ByteString CChar where
  readAccess bs i = unsafeUseAsCString bs (`peekElemOff` i)
  readWithCount bs f =
    unsafeUseAsCStringLen bs $ \(ptr, c) -> f (Count c) >>= peekElemOff ptr
  copyToMBytes bs srdOff mb dstOff c =
    unsafeUseAsCString bs $ \ptr -> copyPtrToMBytes ptr srdOff mb dstOff c
  copyToPtr bs srdOff dstPtr dstOff c =
    unsafeUseAsCString bs $ \ptr -> copyPtrToPtr ptr srdOff dstPtr dstOff c
  moveToPtr bs srdOff dstPtr dstOff c =
    unsafeUseAsCString bs $ \ptr -> movePtrToPtr ptr srdOff dstPtr dstOff c
  moveToMBytes bs srdOff dst dstOff c =
    unsafeUseAsCString bs $ \ptr -> movePtrToMBytes ptr srdOff dst dstOff c

instance (MonadPrim s m, Prim a) => ReadAccess s m (Bytes p) a where
  readAccess b = pure . indexBytes b
  readWithCount b f = indexBytes b <$> f (countOfBytes b)
  copyToMBytes = copyBytesToMBytes
  copyToPtr = copyBytesToPtr
  moveToPtr = moveBytesToPtr
  moveToMBytes = moveBytesToMBytes

instance (MonadPrim s m, Prim a) => ReadAccess s m (MBytes p s) a where
  readAccess = readMBytes
  readWithCount mb f = getCountOfMBytes mb >>= f >>= readMBytes mb
  copyToMBytes = copyMBytesToMBytes
  copyToPtr = copyMBytesToPtr
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

memcpy :: (ReadAccess s m r a, PtrAccess m p a) => r -> Int -> p -> Int -> Count a -> m ()
memcpy src srcOff dst dstOff c =
  withPtrAccess dst $ \dstPtr -> copyToPtr src srcOff dstPtr dstOff c

memmove :: (ReadAccess s m r a, PtrAccess m p a) => r -> Int -> p -> Int -> Count a -> m ()
memmove src srcOff dst dstOff c =
  withPtrAccess dst $ \dstPtr -> moveToPtr src srcOff dstPtr dstOff c

-- | Memory can either be Pinned or Inconclusive. Use eith `toPinnedBytes` or
-- `toPinnedMBytes` to get a conclusive answer for the latter case.
data Pinned = Pin | Inc

data Bytes (p :: Pinned) = Bytes ByteArray#

instance Show (Bytes p) where
  show b =
    Foldable.foldr' ($) "]" $
    ('[' :) : List.intersperse (',' :) (map (("0x" ++) .) (showsBytesHex b))

-- | A list of `ShowS` that covert bytes to base16 encoded strings. Each element of the list
-- is a function that will convert one byte.
--
-- >>> mb <- newPinnedMBytes (Count 5 :: Count Int)
-- >>> mapM_ (\i -> writeMBytes mb (pred i) i) [1 .. 5]
-- >>> foldr ($) "" . showsBytesHex <$> freezeMBytes mb
-- "01000000000000000200000000000000030000000000000004000000000000000500000000000000"
--
showsBytesHex :: Bytes p -> [ShowS]
showsBytesHex b = map toHex (toListBytes b :: [Word8])
  where
    toHex b8 =
      (if b8 <= 0x0f
         then ('0' :)
         else id) .
      showHex b8

instance Eq (Bytes p) where
  (==) = eqBytes

-- | Check if two byte arrays refer to pinned memory and compare their pointers.
sameBytes :: Bytes p1 -> Bytes p2 -> Bool
sameBytes b1 b2 =
  fromMaybe False (samePinnedBytes <$> toPinnedBytes b1 <*> toPinnedBytes b2)
{-# INLINE sameBytes #-}

-- | Perform pointer equality on pinned allocations of `Bytes`.
samePinnedBytes :: Bytes 'Pin -> Bytes 'Pin -> Bool
samePinnedBytes pb1 pb2 = getBytesPtr pb1 == getBytesPtr pb2
{-# INLINE samePinnedBytes #-}

-- | Check if two mutable bytes allocations refer to the same memory
sameMBytes :: MBytes p1 s -> MBytes p2 s -> Bool
sameMBytes (MBytes mb1#) (MBytes mb2#) =
  isTrue# (sameMutableByteArray# mb1# mb2#)

eqBytes :: Bytes p1 -> Bytes p2 -> Bool
eqBytes b1 b2 = mPtrEq || eqBytesN lenEq 0
  where
    n = sizeOfBytes b1
    lenEq = n == sizeOfBytes b2
    eqBytesN acc i
      | i < n = acc && eqBytesN ((indexBytes b1 i :: Word8) == indexBytes b2 i) (i + 1)
      | otherwise = acc
    mPtrEq =
      fromMaybe False $ do
        pb1 <- toPinnedBytes b1
        pb2 <- toPinnedBytes b2
        Just (getBytesPtr pb1 == getBytesPtr pb2)

data MBytes (p :: Pinned) s = MBytes (MutableByteArray# s)

instance NFData (Bytes p) where
  rnf (Bytes _) = ()

instance NFData (MBytes p s) where
  rnf (MBytes _) = ()

-- | Number of elements
newtype Count a = Count {unCount :: Int}
  deriving (Eq, Show, Ord, Enum, Bounded, Num, Integral, Real, NFData)

instance Prim (Count a) where
  type PrimBase (Count a) = Int


---- Pure

indexBytes :: Prim a => Bytes p -> Int -> a
indexBytes (Bytes ba#) (I# i#) = indexByteArray# ba# i#
{-# INLINE indexBytes #-}

-- | This function allows the change of state token. Use with care, because it can allow
-- mutation to escape the `ST` monad.
coerceStateMBytes :: MBytes p s' -> MBytes p s
coerceStateMBytes = unsafeCoerce#

---- Mutable

newMBytes :: (MonadPrim s m, Prim a) => Count a -> m (MBytes 'Inc s)
newMBytes es@(Count n) = allocMBytes (n * sizeOfProxy es)
{-# INLINE newMBytes #-}

newPinnedMBytes :: (MonadPrim s m, Prim a) => Count a -> m (MBytes 'Pin s)
newPinnedMBytes es@(Count n) = allocPinnedMBytes (n * sizeOfProxy es)
{-# INLINE newPinnedMBytes #-}

newPinnedAlignedMBytes :: (MonadPrim s m, Prim a) => Count a -> m (MBytes 'Pin s)
newPinnedAlignedMBytes es@(Count n) =
  allocPinnedAlignedMBytes (n * sizeOfProxy es) (alignmentProxy es)
{-# INLINE newPinnedAlignedMBytes #-}


cnewMBytes :: (MonadPrim s m, Prim a) => Count a -> m (MBytes 'Inc s)
cnewMBytes c = do
  mb <- newMBytes c
  n <- getSizeOfMBytes mb
  mb <$ setMBytes mb 0 (Count n) (0 :: Word8)
{-# INLINE cnewMBytes #-}

cnewPinnedMBytes :: (MonadPrim s m, Prim a) => Count a -> m (MBytes 'Pin s)
cnewPinnedMBytes c = do
  mb <- newPinnedMBytes c
  n <- getSizeOfMBytes mb
  mb <$ setMBytes mb 0 (Count n) (0 :: Word8)
{-# INLINE cnewPinnedMBytes #-}

cnewPinnedAlignedMBytes ::
     (MonadPrim s m, Prim a)
  => Count a -- ^ Size in number of bytes
  -> m (MBytes 'Pin s)
cnewPinnedAlignedMBytes c = do
  mb <- newPinnedAlignedMBytes c
  n <- getSizeOfMBytes mb
  mb <$ setMBytes mb 0 (Count n) (0 :: Word8)
{-# INLINE cnewPinnedAlignedMBytes #-}


allocMBytes :: MonadPrim s m => Int -> m (MBytes 'Inc s)
allocMBytes (I# i#) =
  prim $ \s# ->
    case newByteArray# i# s# of
      (# s'#, ba# #) -> (# s'#, MBytes ba# #)
{-# INLINE allocMBytes #-}

allocPinnedMBytes :: MonadPrim s m => Int -> m (MBytes 'Pin s)
allocPinnedMBytes (I# i#) =
  prim $ \s# ->
    case newPinnedByteArray# i# s# of
      (# s'#, ba# #) -> (# s'#, MBytes ba# #)
{-# INLINE allocPinnedMBytes #-}

allocPinnedAlignedMBytes ::
     MonadPrim s m
  => Int -- ^ Size in number of bytes
  -> Int -- ^ Alignment in number of bytes
  -> m (MBytes 'Pin s)
allocPinnedAlignedMBytes (I# i#) (I# a#) =
  prim $ \s# ->
    case newAlignedPinnedByteArray# i# a# s# of
      (# s'#, ba# #) -> (# s'#, MBytes ba# #)
{-# INLINE allocPinnedAlignedMBytes #-}

callocMBytes :: MonadPrim s m => Int -> m (MBytes 'Inc s)
callocMBytes n = do
  mb <- allocMBytes n
  mb <$ setMBytes mb 0 (Count n) (0 :: Word8)
{-# INLINE callocMBytes #-}

callocPinnedMBytes :: MonadPrim s m => Int -> m (MBytes 'Pin s)
callocPinnedMBytes n = do
  mb <- allocPinnedMBytes n
  mb <$ setMBytes mb 0 (Count n) (0 :: Word8)
{-# INLINE callocPinnedMBytes #-}

callocPinnedAlignedMBytes ::
     MonadPrim s m
  => Int -- ^ Size in number of bytes
  -> Int -- ^ Alignment in number of bytes
  -> m (MBytes 'Pin s)
callocPinnedAlignedMBytes n a = do
  mb <- allocPinnedAlignedMBytes n a
  mb <$ setMBytes mb 0 (Count n) (0 :: Word8)
{-# INLINE callocPinnedAlignedMBytes #-}


getSizeOfMBytes :: MonadPrim s m => MBytes p s -> m Int
getSizeOfMBytes (MBytes ba#) =
  prim $ \s# ->
    case getSizeofMutableByteArray# ba# s# of
      (# s'#, n# #) -> (# s'#, I# n# #)
{-# INLINE getSizeOfMBytes #-}

-- | Fill the mutable array with zeros efficiently.
zerosMBytes :: MonadPrim s m => MBytes p s -> m ()
zerosMBytes mba@(MBytes mba#) = do
  I# n# <- getSizeOfMBytes mba
  prim_ (setByteArray# mba# 0# n# 0#)
{-# INLINE zerosMBytes #-}

freezeMBytes :: MonadPrim s m => MBytes p s -> m (Bytes p)
freezeMBytes (MBytes mba#) =
  prim $ \s# ->
    case unsafeFreezeByteArray# mba# s# of
      (# s'#, ba# #) -> (# s'#, Bytes ba# #)
{-# INLINE freezeMBytes #-}

thawBytes :: MonadPrim s m => Bytes p -> m (MBytes p s)
thawBytes (Bytes ba#) =
  prim $ \s# ->
    case thawByteArray# ba# s# of
      (# s'#, mba# #) -> (# s'#, MBytes mba# #)
{-# INLINE thawBytes #-}

withCopyMBytes ::
     (MonadPrim s m, Alloc m (MBytes p s))
  => Bytes p'
  -> (MBytes p s -> m a)
  -> m (a, Bytes p)
withCopyMBytes b f = do
  let n = sizeOfBytes b
  mb <- mallocBytes n
  copyBytesToMBytes8 b 0 mb 0 (Count n)
  applyFreezeMBytes f mb
{-# INLINE withCopyMBytes #-}


withCopyMBytes_ ::
     (MonadPrim s m, Alloc m (MBytes p s)) => Bytes p' -> (MBytes p s -> m a) -> m (Bytes p)
withCopyMBytes_ b f = snd <$> withCopyMBytes b f
{-# INLINE withCopyMBytes_ #-}


applyFreezeMBytes ::
     MonadPrim s m => (MBytes p s -> m a) -> MBytes p s -> m (a, Bytes p)
applyFreezeMBytes f mb = do
  res <- f mb
  b' <- freezeMBytes mb
  pure (res, b')
{-# INLINE applyFreezeMBytes #-}

withMBytes ::
     MonadPrim s m => Bytes p -> (MBytes p s -> m a) -> m (a, Bytes p)
withMBytes b f = thawBytes b >>= applyFreezeMBytes f
{-# INLINE withMBytes #-}

withMBytes_ ::
     MonadPrim s m => Bytes p -> (MBytes p s -> m a) -> m (Bytes p)
withMBytes_ b f = snd <$> withMBytes b f
{-# INLINE withMBytes_ #-}



-- | Offsets are in bytes
copyPtrToMBytes8 ::
     MonadPrim s m => Ptr Word8 -> Int -> MBytes pd s -> Int -> Count Word8 -> m ()
copyPtrToMBytes8 (Ptr srcAddr#) (I# srcOff#) (MBytes dst#) (I# dstOff#) (Count (I# n#)) =
  prim_ (copyAddrToByteArray# (srcAddr# `plusAddr#` srcOff#) dst# dstOff# n#)
{-# INLINE copyPtrToMBytes8 #-}

copyBytesToMBytes8 ::
     MonadPrim s m => Bytes p -> Int -> MBytes pd s -> Int -> Count Word8 -> m ()
copyBytesToMBytes8 (Bytes src#) (I# srcOff#) (MBytes dst#) (I# dstOff#) (Count (I# n#)) =
  prim_ (copyByteArray# src# srcOff# dst# dstOff# n#)
{-# INLINE copyBytesToMBytes8 #-}

copyMBytesToMBytes8 ::
     MonadPrim s m => MBytes ps s -> Int -> MBytes pd s -> Int -> Count Word8 -> m ()
copyMBytesToMBytes8 (MBytes src#) (I# srcOff#) (MBytes dst#) (I# dstOff#) (Count (I# n#)) =
  prim_ (copyMutableByteArray# src# srcOff# dst# dstOff# n#)
{-# INLINE copyMBytesToMBytes8 #-}

copyPtrToMBytes ::
     (MonadPrim s m, Prim a) => Ptr a -> Int -> MBytes pd s -> Int -> Count a -> m ()
copyPtrToMBytes (Ptr srcAddr#) srcOff (MBytes dst#) dstOff c =
  let src# = srcAddr# `plusAddr#` fromOff# c srcOff
   in prim_ $ copyAddrToByteArray# src# dst# (fromOff# c dstOff) (fromCount# c)
{-# INLINE[0] copyPtrToMBytes #-}

copyBytesToMBytes ::
     (MonadPrim s m, Prim a) => Bytes p -> Int -> MBytes pd s -> Int -> Count a -> m ()
copyBytesToMBytes (Bytes src#) srcOff (MBytes dst#) dstOff c =
  prim_ $
  copyByteArray# src# (fromOff# c srcOff) dst# (fromOff# c dstOff) (fromCount# c)
{-# INLINE[0] copyBytesToMBytes #-}

copyMBytesToMBytes ::
     (MonadPrim s m, Prim a) => MBytes ps s-> Int -> MBytes pd s -> Int -> Count a -> m ()
copyMBytesToMBytes (MBytes mbSrc#) (I# mbSrcOff#) (MBytes mbDst#) (I# mbDstOff#) c =
  prim_ (copyMutableByteArray# mbSrc# mbSrcOff# mbDst# mbDstOff# (fromCount# c))
{-# INLINE[0] copyMBytesToMBytes #-}

copyMBytesToPtr8 :: MonadPrim s m => MBytes p s -> Int -> Ptr Word8 -> Int -> Count Word8 -> m ()
copyMBytesToPtr8 (MBytes src#) (I# srcOff#) (Ptr dstAddr#) (I# dstOff#) (Count (I# n#)) =
  let addr# = dstAddr# `plusAddr#` dstOff#
  in prim_ (copyMutableByteArrayToAddr# src# srcOff# addr# n#)
{-# INLINE copyMBytesToPtr8 #-}


copyBytesToPtr8 :: MonadPrim s m => Bytes p -> Int -> Ptr Word8 -> Int -> Count Word8 -> m ()
copyBytesToPtr8 (Bytes src#) (I# srcOff#) (Ptr dstAddr#) (I# dstOff#) (Count (I# n#)) =
  let addr# = dstAddr# `plusAddr#` dstOff#
  in prim_ (copyByteArrayToAddr# src# srcOff# addr# n#)
{-# INLINE copyBytesToPtr8 #-}

copyMBytesToPtr :: (MonadPrim s m, Prim a) => MBytes p s -> Int -> Ptr a -> Int -> Count a -> m ()
copyMBytesToPtr (MBytes src#) srcOff (Ptr dstAddr#) dstOff c =
  let addr# = dstAddr# `plusAddr#` fromOff# c dstOff
   in prim_ $
      copyMutableByteArrayToAddr# src# (fromOff# c srcOff) addr# (fromCount# c)
{-# INLINE[0] copyMBytesToPtr #-}


copyBytesToPtr :: (MonadPrim s m, Prim a) => Bytes p -> Int -> Ptr a -> Int -> Count a -> m ()
copyBytesToPtr (Bytes src#) srcOff (Ptr dstAddr#) dstOff c =
  let addr# = dstAddr# `plusAddr#` fromOff# c dstOff
  in prim_ (copyByteArrayToAddr# src# (fromOff# c srcOff) addr# (fromCount# c))
{-# INLINE[0] copyBytesToPtr #-}

copyPtrToPtr8 :: MonadPrim s m => Ptr Word8 -> Int -> Ptr Word8 -> Int -> Count Word8 -> m ()
copyPtrToPtr8 srcPtr srcOff dstPtr dstOff (Count n) =
  unsafeIOToPrim $
  copyBytes (dstPtr `plusPtr` dstOff) (srcPtr `plusPtr` srcOff) n
{-# INLINE copyPtrToPtr8 #-}

copyPtrToPtr :: (MonadPrim s m, Prim a) => Ptr a -> Int -> Ptr a -> Int -> Count a -> m ()
copyPtrToPtr (Ptr srcAddr#) srcOff (Ptr dstAddr#) dstOff c =
  unsafeIOToPrim $
  copyBytes
    (Ptr (dstAddr# `plusAddr#` fromOff# c dstOff))
    (Ptr (srcAddr# `plusAddr#` fromOff# c srcOff))
    (I# (fromCount# c))
{-# INLINE[0] copyPtrToPtr #-}


-- moveMBytesToPtr8 :: MonadPrim s m => MBytes p s -> Int -> Ptr Word8 -> Int -> Count Word8 -> m ()
-- moveMBytesToPtr8 (MBytes src#) (I# srcOff#) (Ptr dstAddr#) (I# dstOff#) (Count (I# n#)) =
--   let addr# = dstAddr# `plusAddr#` dstOff#
--   in prim_ (moveMutableByteArrayToAddr# src# srcOff# addr# n#)
-- {-# INLINE moveMBytesToPtr8 #-}

moveMBytesToMBytes8 ::
  MonadPrim s m => MBytes ps s -> Int -> MBytes pd s -> Int -> Count Word8 -> m ()
moveMBytesToMBytes8 (MBytes src#) (I# srcOff#) (MBytes dst#) (I# dstOff#) (Count (I# n#)) =
  unsafeIOToPrim $
  memmoveMutableByteArray# src# srcOff# dst# dstOff# n#
{-# INLINE moveMBytesToMBytes8 #-}

moveMBytesToMBytes ::
  (MonadPrim s m, Prim a) => MBytes ps s -> Int -> MBytes pd s -> Int -> Count a -> m ()
moveMBytesToMBytes (MBytes src#) srcOff (MBytes dst#) dstOff c =
  unsafeIOToPrim $
  memmoveMutableByteArray#
    src#
    (fromOff# c srcOff)
    dst#
    (fromOff# c dstOff)
    (fromCount# c)
{-# INLINE[0] moveMBytesToMBytes #-}

moveMBytesToPtr8 :: MonadPrim s m => MBytes p s -> Int -> Ptr Word8 -> Int -> Count Word8 -> m ()
moveMBytesToPtr8 (MBytes src#) (I# srcOff#) (Ptr dstAddr#) (I# dstOff#) (Count (I# n#)) =
  unsafeIOToPrim $
  memmoveMutableByteArrayToAddr# src# srcOff# dstAddr# dstOff# n#
{-# INLINE moveMBytesToPtr8 #-}

moveMBytesToPtr :: (MonadPrim s m, Prim a) => MBytes p s -> Int -> Ptr a -> Int -> Count a -> m ()
moveMBytesToPtr (MBytes src#) srcOff (Ptr dstAddr#) dstOff c =
  unsafeIOToPrim $
  memmoveMutableByteArrayToAddr#
    src#
    (fromOff# c srcOff)
    dstAddr#
    (fromOff# c dstOff)
    (fromCount# c)
{-# INLINE[0] moveMBytesToPtr #-}

movePtrToMBytes8 :: MonadPrim s m => Ptr Word8 -> Int -> MBytes p s -> Int -> Count Word8 -> m ()
movePtrToMBytes8 (Ptr srcAddr#) (I# srcOff#) (MBytes dst#) (I# dstOff#) (Count (I# n#)) =
  unsafeIOToPrim $
  memmoveMutableByteArrayFromAddr# srcAddr# srcOff# dst# dstOff# n#
{-# INLINE movePtrToMBytes8 #-}

movePtrToMBytes :: (MonadPrim s m, Prim a) => Ptr a -> Int -> MBytes p s -> Int -> Count a -> m ()
movePtrToMBytes (Ptr srcAddr#) srcOff (MBytes dst#) dstOff c =
  unsafeIOToPrim $
  memmoveMutableByteArrayFromAddr#
    srcAddr#
    (fromOff# c srcOff)
    dst#
    (fromOff# c dstOff)
    (fromCount# c)
{-# INLINE[0] movePtrToMBytes #-}


movePtrToPtr8 :: MonadPrim s m => Ptr Word8 -> Int -> Ptr Word8 -> Int -> Count Word8 -> m ()
movePtrToPtr8 (Ptr srcAddr#) (I# srcOff#) (Ptr dstAddr#) (I# dstOff#) (Count (I# n#)) =
  unsafeIOToPrim $ memmoveAddr# srcAddr# srcOff# dstAddr# dstOff# n#
{-# INLINE movePtrToPtr8 #-}

movePtrToPtr :: (MonadPrim s m, Prim a) => Ptr a -> Int -> Ptr a -> Int -> Count a -> m ()
movePtrToPtr (Ptr srcAddr#) srcOff (Ptr dstAddr#) dstOff c =
  unsafeIOToPrim $
  memmoveAddr#
    srcAddr#
    (fromOff# c srcOff)
    dstAddr#
    (fromOff# c dstOff)
    (fromCount# c)
{-# INLINE[0] movePtrToPtr #-}

{-# RULES
"copyPtrToMBytes8" copyPtrToMBytes = copyPtrToMBytes8
"copyBytesToMBytes8" copyBytesToMBytes = copyBytesToMBytes8
"copyMBytesToMBytes8" copyMBytesToMBytes = copyMBytesToMBytes8
"copyBytesToPtr8" copyBytesToPtr = copyBytesToPtr8
"copyMBytesToPtr8" copyMBytesToPtr = copyMBytesToPtr8
"copyPtrToPtr8" copyPtrToPtr = copyPtrToPtr8
"moveMBytesToMBytes8" moveMBytesToMBytes = moveMBytesToMBytes8
"moveMBytesToPtr8" moveMBytesToPtr = moveMBytesToPtr8
"movePtrToMBytes8" movePtrToMBytes = movePtrToMBytes8
"movePtrToPtr8" movePtrToPtr = movePtrToPtr8
  #-}

moveBytesToMBytes :: (MonadPrim s m, Prim a) =>
  Bytes ps -> Int -> MBytes pd s -> Int -> Count a -> m ()
moveBytesToMBytes src srcOff dst dstOff c = do
  msrc <- thawBytes src
  moveMBytesToMBytes msrc srcOff dst dstOff c
{-# INLINE moveBytesToMBytes #-}


moveBytesToPtr :: (MonadPrim s m, Prim a) => Bytes p -> Int -> Ptr a -> Int -> Count a -> m ()
moveBytesToPtr src srcOff dst dstOff c = do
  msrc <- thawBytes src
  moveMBytesToPtr msrc srcOff dst dstOff c
{-# INLINE moveBytesToPtr #-}


fromCount# :: Prim a => Count a -> Int#
fromCount# c@(Count (I# n#)) =
  case sizeOfProxy c of
    (I# sz#) -> sz# *# n#
{-# INLINE fromCount# #-}

fromOff# :: Prim a => proxy a -> Int -> Int#
fromOff# px (I# o#) =
  case sizeOfProxy px of
    (I# sz#) -> sz# *# o#
{-# INLINE fromOff# #-}

cloneBytes :: (MonadPrim s m, Alloc m (MBytes p s)) => Bytes p' -> m (Bytes p)
cloneBytes b = withCopyMBytes_ b pure
{-# INLINE cloneBytes #-}

cloneMBytes ::
     (MonadPrim s m, Alloc m (MBytes pd s)) => MBytes ps s -> m (MBytes pd s)
cloneMBytes mb = do
  n <- getSizeOfMBytes mb
  mb' <- mallocBytes n
  mb' <$ copyMBytesToMBytes8 mb 0 mb' 0 (Count n)
{-# INLINE cloneMBytes #-}

sizeOfBytes :: Bytes p -> Int
sizeOfBytes (Bytes ba#) = I# (sizeofByteArray# ba#)
{-# INLINE sizeOfBytes #-}

-- | How many elements of type @a@ fits into bytes completely. In order to get any number
-- of leftover bytes use `countRemOfBytes`
countOfBytes :: forall a p. Prim a => Bytes p -> Count a
countOfBytes b = Count (sizeOfBytes b `quot` sizeOfType @a)
{-# INLINE countOfBytes #-}

-- | Get the number of elements of type @a@ that can fit into bytes as well as the slack
-- number of bytes that would be leftover in case when total number of bytes available is
-- not exactly divisable by the size of the element that will be stored in the memory
-- chunk.
countRemOfBytes :: forall a p. Prim a => Bytes p -> (Count a, Int)
countRemOfBytes b = first Count (sizeOfBytes b `quotRem` sizeOfType @a)
{-# INLINE countRemOfBytes #-}

-- | How many elements of type @a@ fits into bytes completely. In order to get any number
-- of leftover bytes use `countRemOfBytes`
getCountOfMBytes :: forall a p s m. (MonadPrim s m, Prim a) => MBytes p s -> m (Count a)
getCountOfMBytes b = do
  sz <- getSizeOfMBytes b
  pure $ Count (sz `quot` sizeOfType @a)
{-# INLINE getCountOfMBytes #-}

-- | Get the number of elements of type @a@ that can fit into bytes as well as the slack
-- number of bytes that would be leftover in case when total number of bytes available is
-- not exactly divisable by the size of the element that will be stored in the memory
-- chunk.
getCountRemOfMBytes :: forall a p s m. (MonadPrim s m, Prim a) => MBytes p s -> m (Count a, Int)
getCountRemOfMBytes b = do
  sz <- getSizeOfMBytes b
  pure $ first Count (sz `quotRem` sizeOfType @a)
{-# INLINE getCountRemOfMBytes #-}

-- | It is only guaranteed to convert the full chunk of memory to a list of elements of
-- size 8bit (eg. `Word8`)
toListBytes :: Prim a => Bytes p -> [a]
toListBytes ba = build (\ c n -> foldrBytesBuild c n ba)
{-# INLINE toListBytes #-}

foldrBytesBuild :: forall a b p . Prim a => (a -> b -> b) -> b -> Bytes p -> b
foldrBytesBuild c n bs = go 0
  where
    Count k = countOfBytes bs :: Count a
    go i
      | i == k = n
      | otherwise =
        let !v = indexBytes bs i
         in v `c` go (i + 1)
{-# INLINE [0] foldrBytesBuild #-}


-- | Returns `EQ` if the full list did fit into the supplied memory chunk exactly.
-- Otherwise it will return `LT` if the list was too small and `GT` if the list was bigger
-- than the available allocated memory.
loadListMBytes :: forall a p s m . (MonadPrim s m, Prim a) => [a] -> MBytes p s -> m Ordering
loadListMBytes ys mb = do
  (Count n :: Count a, slack) <- getCountRemOfMBytes mb
  let go [] i = pure (compare i n <> compare 0 slack)
      go (x:xs) i
        | i < n = writeMBytes mb i x >> go xs (i + 1)
        | otherwise = pure GT
  go ys 0
{-# INLINE loadListMBytes #-}

fromListBytesN ::
     forall a. Prim a
  => Int
  -> [a]
  -> (Ordering, Bytes 'Inc)
fromListBytesN n xs = runST $ do
  mb <- newMBytes (Count n :: Count a)
  res <- loadListMBytes xs mb
  (,) res <$> freezeMBytes mb
{-# INLINE fromListBytesN #-}

fromListBytes ::
     forall a. Prim a
  => [a]
  -> Bytes 'Inc
fromListBytes xs =
  case fromListBytesN (length xs) xs of
    (EQ, ba) -> ba
    (_, _) ->
      impossibleError "fromListBytes" "Number of elements in the list was expected"
{-# INLINE fromListBytes #-}


readMBytes :: (MonadPrim s m, Prim a) => MBytes p s -> Int -> m a
readMBytes (MBytes mba#) (I# i#) = prim (readMutableByteArray# mba# i#)
{-# INLINE readMBytes #-}

writeMBytes :: (MonadPrim s m, Prim a) => MBytes p s -> Int -> a -> m ()
writeMBytes (MBytes mba#) (I# i#) a = prim_ (writeMutableByteArray# mba# i# a)
{-# INLINE writeMBytes #-}


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

ensurePinnedBytes :: MonadPrim s m => Bytes p -> m (Bytes 'Pin)
ensurePinnedBytes b =
  case toPinnedBytes b of
    Just pb -> pure pb
    Nothing -> cloneBytes b
{-# INLINE ensurePinnedBytes #-}

ensurePinnedMBytes :: MonadPrim s m => MBytes p s -> m (MBytes 'Pin s)
ensurePinnedMBytes mb =
  case toPinnedMBytes mb of
    Just pmb -> pure pmb
    Nothing  -> cloneMBytes mb
{-# INLINE ensurePinnedMBytes #-}

toPinnedBytes :: Bytes p -> Maybe (Bytes 'Pin)
toPinnedBytes (Bytes b#)
  | isTrue# (isByteArrayPinned# b#) = Just (Bytes b#)
  | otherwise = Nothing
{-# INLINE toPinnedBytes #-}

toPinnedMBytes :: MBytes p s -> Maybe (MBytes 'Pin s)
toPinnedMBytes (MBytes mb#)
  | isTrue# (isMutableByteArrayPinned# mb#) = Just (MBytes mb#)
  | otherwise = Nothing
{-# INLINE toPinnedMBytes #-}


getBytesPtr :: Bytes 'Pin -> Ptr a
getBytesPtr (Bytes ba#) = Ptr (byteArrayContents# ba#)
{-# INLINE getBytesPtr #-}

getMBytesPtr :: MBytes 'Pin s -> Ptr a
getMBytesPtr (MBytes mba#) = Ptr (byteArrayContents# (unsafeCoerce# mba#))
{-# INLINE getMBytesPtr #-}

setMBytes ::
     (MonadPrim s m, Prim a)
  => MBytes p s -- ^ Chunk of memory to fill
  -> Int -- ^ Offset in number of elements
  -> Count a -- ^ Number of cells to fill
  -> a -- ^ A value to fill the cells with
  -> m ()
setMBytes (MBytes mba#) (I# o#) (Count (I# n#)) a = prim_ (setMutableByteArray# mba# o# n# a)
{-# INLINE setMBytes #-}



-- | Writing 8 bytes at a time in a Little-endian order gives us platform portability
fillPinnedMBytesWord64LE :: MonadPrim s m => g -> (g -> (Word64, g)) -> MBytes 'Pin s -> m g
fillPinnedMBytesWord64LE = fillPinnedMBytesWith (\a -> unsafeIOToPrim . runF word64LE a)
{-# INLINE fillPinnedMBytesWord64LE #-}

-- | Writing 4 bytes at a time in a Little-endian order gives us platform portability
fillPinnedMBytesWord32LE :: MonadPrim s m => g -> (g -> (Word32, g)) -> MBytes 'Pin s -> m g
fillPinnedMBytesWord32LE = fillPinnedMBytesWith (\a -> unsafeIOToPrim . runF word32LE a)
{-# INLINE fillPinnedMBytesWord32LE #-}

fillPinnedMBytesWith ::
     forall a g s m. (MonadPrim s m, Prim a)
  => (a -> Ptr Word8 -> m ())
  -> g
  -> (g -> (a, g))
  -> MBytes 'Pin s
  -> m g
fillPinnedMBytesWith f g0 gen64 mb =
  withMBytesPtr mb $ \ptr0 -> do
    (c@(Count n64 :: Count a), nrem64) <- getCountRemOfMBytes mb
    let sz = sizeOfProxy c
    let go g i ptr
          | i < n64 = do
            let (w64, g') = gen64 g
            f w64 ptr
            go g' (i + 1) (ptr `plusPtr` sz)
          | otherwise = return (g, ptr)
    (g, ptr') <- go g0 0 ptr0
    if nrem64 == 0
      then pure g
      else do
        let (w64, g') = gen64 g
        -- In order to not mess up the byte order we write generated Word64 into a temporary
        -- pointer and then copy only the missing bytes over to the array. It is tempting to
        -- simply generate as many bytes as we still need using smaller type (eg. Word16),
        -- but that would result in an inconsistent tail when total the length is slightly
        -- varied.
        w64mb <- newPinnedMBytes (Count 1 :: Count a)
        writeMBytes w64mb 0 w64
        withMBytesPtr w64mb (f w64)
        copyMBytesToPtr8 w64mb 0 ptr' 0 (Count nrem64)
        pure g'
{-# INLINE fillPinnedMBytesWith #-}


withMBytesPtr :: MonadPrim s m => MBytes 'Pin s -> (Ptr a -> m b) -> m b
withMBytesPtr mb f = do
  let ptr = getMBytesPtr mb
  res <- f ptr
  touch mb
  pure res
{-# INLINE withMBytesPtr #-}
