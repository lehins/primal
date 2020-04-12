{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
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
  , isPinnedBytes
  , isPinnedMBytes
  , toPinnedBytes
  , toPinnedMBytes
  , ensurePinnedBytes
  , ensurePinnedMBytes
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
  , copyBytesToMBytes
  , copyMBytesToMBytes
  , moveMBytesToMBytes
  -- ** Moving data
  -- * Size
  , getSizeOfMBytes
  , getCountOfMBytes
  , getCountRemOfMBytes
  -- * Access
  , readMBytes
  , writeMBytes
  , setMBytes
  -- ** With Ptr
  , withMBytesPtr
  , getBytesPtr
  , getMBytesPtr
  , module Data.Prim
  -- * Helpers
  , applyFreezeMBytes
  -- * Experimental
  -- , fillPinnedMBytesWord64LE
  -- , fillPinnedMBytesWith
  ) where

import Control.Arrow
import Control.DeepSeq
import Control.Monad.Prim
import Control.Monad.Prim.Unsafe
import Control.Monad.ST
import Data.Foldable as Foldable
import Data.List as List
import Data.Maybe
import Data.Prim
import Data.Prim.Class
import Data.Prim.Foreign (getSizeofMutableByteArray#, isByteArrayPinned#,
                          isMutableByteArrayPinned#,
                          memmoveMutableByteArray#)
import GHC.Base
import GHC.Exts hiding (getSizeofMutableByteArray#, isByteArrayPinned#,
                 isMutableByteArrayPinned#)
import GHC.Word
import Numeric (showHex)

-- class PtrAccess m p a | p -> a where
--   withPtrAccess :: MonadPrim s m => p -> (Ptr a -> m b) -> m b

-- instance PtrAccess m (Bytes 'Pin) Word8 where
--   withPtrAccess b f = do
--     let ptr = getBytesPtr b
--     res <- f ptr
--     touch b
--     pure res

-- instance MonadPrim s m => PtrAccess m (MBytes 'Pin s) Word8 where
--   withPtrAccess = withMBytesPtr

-- -- TODO: MonadUnliftIO
-- instance PtrAccess IO (ForeignPtr a) a where
--   withPtrAccess = withForeignPtr

-- instance PtrAccess IO (Ptr a) a where
--   withPtrAccess = (&)

-- instance PtrAccess IO ByteString CChar where
--   withPtrAccess = unsafeUseAsCString

-- class MonadPrim s m => ReadAccess s m r where
--   count :: Prim a => r -> m (Count a)

--   readAccess :: Prim a => r -> Int -> m a

--   readWithCount :: Prim a => r -> (Count a -> m Int) -> m a

--   -- | Source and target can't be the same memory chunks
--   copyToMBytes :: Prim a => r -> Int -> MBytes p s -> Int -> Count a -> m ()

--   -- | Source and target can't be the same memory chunks
--   copyToPtr :: Prim a => r -> Int -> Ptr a -> Int -> Count a -> m ()

--   -- | Source and target can be overlapping memory chunks
--   moveToMBytes :: Prim a => r -> Int -> MBytes p s -> Int -> Count a -> m ()

--   -- | Source and target can be overlapping memory chunks
--   moveToPtr :: Prim a => r -> Int -> Ptr a -> Int -> Count a -> m ()

-- class ReadAccess s m r => WriteAccess s m r where
--   writeAccess :: r -> Int -> a -> m a


-- instance ReadAccess RealWorld IO ByteString where
--   readAccess bs i = unsafeUseAsCString bs $ \(Ptr p#) -> readOffPtr (Ptr p#) i
--   readWithCount bs f =
--     unsafeUseAsCStringLen bs $ \(Ptr p#, c) -> f (Count c) >>= readOffPtr (Ptr p#)
--   copyToMBytes bs srdOff mb dstOff c =
--     unsafeUseAsCString bs $ \(Ptr p#) -> copyPtrToMBytes (Ptr p#) srdOff mb dstOff c
--   copyToPtr bs srdOff dstPtr dstOff c =
--     unsafeUseAsCString bs $ \(Ptr p#) -> copyPtrToPtr (Ptr p#) srdOff dstPtr dstOff c
--   moveToPtr bs srdOff dstPtr dstOff c =
--     unsafeUseAsCString bs $ \(Ptr p#) -> movePtrToPtr (Ptr p#) srdOff dstPtr dstOff c
--   moveToMBytes bs srdOff dst dstOff c =
--     unsafeUseAsCString bs $ \(Ptr p#) -> movePtrToMBytes (Ptr p#) srdOff dst dstOff c

-- instance (MonadPrim s m) => ReadAccess s m (Bytes p) where
--   readAccess b = pure . indexBytes b
--   readWithCount b f = indexBytes b <$> f (countOfBytes b)
--   copyToMBytes = copyBytesToMBytes
--   copyToPtr = copyBytesToPtr
--   moveToPtr = moveBytesToPtr
--   moveToMBytes = moveBytesToMBytes

-- newtype MVec s a = MVec (MBytes 'Pin s)

-- instance (MonadPrim s m) => ReadAccess s m (MVec s a) where
--   readAccess (MVec mb) = readMBytes mb
--   --readWithCount mb f = getCountOfMBytes mb >>= f >>= readMBytes mb

-- rv :: (ReadAccess s m (MVec s b), Prim a) => MVec s b -> Int -> m a
-- rv = readAccess

-- instance (MonadPrim s m) => ReadAccess s m (MBytes p s) where
--   readAccess = readMBytes
--   readWithCount mb f = getCountOfMBytes mb >>= f >>= readMBytes mb
--   copyToMBytes = copyMBytesToMBytes
--   copyToPtr = copyMBytesToPtr
--   moveToPtr = moveMBytesToPtr
--   moveToMBytes = moveMBytesToMBytes

-- class Alloc m a where
--   mallocBytes :: MonadPrim s m => Int -> m a

-- -- class AllocAligned m a where
-- --   mallocBytesAligned :: MonadPrim s m => Int -> Int -> m a


-- instance Alloc m (Bytes 'Inc) where
--   mallocBytes = mallocBytes >=> freezeMBytes

-- instance Alloc m (Bytes 'Pin) where
--   mallocBytes = mallocBytes >=> freezeMBytes

-- instance MonadPrim s m => Alloc m (MBytes 'Inc s) where
--   mallocBytes = allocMBytes

-- instance MonadPrim s m => Alloc m (MBytes 'Pin s) where
--   mallocBytes = allocPinnedMBytes

-- instance Alloc IO (ForeignPtr a) where
--   mallocBytes = mallocForeignPtrBytes

-- malloc :: (Alloc m a, MonadPrim s m, Prim p) => Count p -> m a
-- malloc c@(Count n) = mallocBytes (n * sizeOfProxy c)

-- memcpy :: (ReadAccess s m r, PtrAccess m p a, Prim a) => r -> Int -> p -> Int -> Count a -> m ()
-- memcpy src srcOff dst dstOff c =
--   withPtrAccess dst $ \dstPtr -> copyToPtr src srcOff dstPtr dstOff c

-- memmove :: (ReadAccess s m r, PtrAccess m p a, Prim a) => r -> Int -> p -> Int -> Count a -> m ()
-- memmove src srcOff dst dstOff c =
--   withPtrAccess dst $ \dstPtr -> moveToPtr src srcOff dstPtr dstOff c

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
eqBytes b1 b2 = mPtrEq || eqBytesN lenEq (0 :: Off Word8)
  where
    n1 = sizeOfBytes b1
    lenEq = n1 == sizeOfBytes b2
    eqBytesN acc i8@(Off i)
      | i < n1 = acc && eqBytesN (indexBytes b1 i8 == indexBytes b2 i8) (i8 + 1)
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

---- Pure

indexBytes :: Prim a => Bytes p -> Off a -> a
indexBytes (Bytes ba#) (Off (I# i#)) = indexByteArray# ba# i#
{-# INLINE indexBytes #-}

-- | This function allows the change of state token. Use with care, because it can allow
-- mutation to escape the `ST` monad.
coerceStateMBytes :: MBytes p s' -> MBytes p s
coerceStateMBytes = unsafeCoerce#

---- Mutable

newMBytes :: (MonadPrim s m, Prim a) => Count a -> m (MBytes 'Inc s)
newMBytes = allocMBytes . countWord8
{-# INLINE newMBytes #-}

newPinnedMBytes :: (MonadPrim s m, Prim a) => Count a -> m (MBytes 'Pin s)
newPinnedMBytes = allocPinnedMBytes . countWord8
{-# INLINE newPinnedMBytes #-}

newPinnedAlignedMBytes :: (MonadPrim s m, Prim a) => Count a -> m (MBytes 'Pin s)
newPinnedAlignedMBytes c =
  allocPinnedAlignedMBytes (countWord8 c) (alignmentProxy c)
{-# INLINE newPinnedAlignedMBytes #-}


cnewMBytes :: (MonadPrim s m, Prim a) => Count a -> m (MBytes 'Inc s)
cnewMBytes c = do
  mb <- newMBytes c
  n8 <- getSizeOfMBytes mb
  mb <$ setMBytes mb 0 (Count n8) (0 :: Word8)
{-# INLINE cnewMBytes #-}

cnewPinnedMBytes :: (MonadPrim s m, Prim a) => Count a -> m (MBytes 'Pin s)
cnewPinnedMBytes c = do
  mb <- newPinnedMBytes c
  n8 <- getSizeOfMBytes mb
  mb <$ setMBytes mb 0 (Count n8) (0 :: Word8)
{-# INLINE cnewPinnedMBytes #-}

cnewPinnedAlignedMBytes ::
     (MonadPrim s m, Prim a)
  => Count a -- ^ Size in number of bytes
  -> m (MBytes 'Pin s)
cnewPinnedAlignedMBytes c = do
  mb <- newPinnedAlignedMBytes c
  n8 <- getSizeOfMBytes mb
  mb <$ setMBytes mb 0 (Count n8) (0 :: Word8)
{-# INLINE cnewPinnedAlignedMBytes #-}


allocMBytes :: MonadPrim s m => Count Word8 -> m (MBytes 'Inc s)
allocMBytes (Count (I# i#)) =
  prim $ \s# ->
    case newByteArray# i# s# of
      (# s'#, ba# #) -> (# s'#, MBytes ba# #)
{-# INLINE allocMBytes #-}


allocPinnedMBytes :: MonadPrim s m => Count Word8 -> m (MBytes 'Pin s)
allocPinnedMBytes (Count (I# i#)) =
  prim $ \s# ->
    case newPinnedByteArray# i# s# of
      (# s'#, ba# #) -> (# s'#, MBytes ba# #)
{-# INLINE allocPinnedMBytes #-}

allocPinnedAlignedMBytes ::
     MonadPrim s m
  => Count Word8 -- ^ Size in number of bytes
  -> Int -- ^ Alignment in number of bytes
  -> m (MBytes 'Pin s)
allocPinnedAlignedMBytes (Count (I# i#)) (I# a#) =
  prim $ \s# ->
    case newAlignedPinnedByteArray# i# a# s# of
      (# s'#, ba# #) -> (# s'#, MBytes ba# #)
{-# INLINE allocPinnedAlignedMBytes #-}

callocMBytes :: MonadPrim s m => Count Word8 -> m (MBytes 'Inc s)
callocMBytes n = do
  mb <- allocMBytes n
  mb <$ setMBytes mb 0 n (0 :: Word8)
{-# INLINE callocMBytes #-}

callocPinnedMBytes :: MonadPrim s m => Count Word8 -> m (MBytes 'Pin s)
callocPinnedMBytes n = do
  mb <- allocPinnedMBytes n
  mb <$ setMBytes mb 0 n (0 :: Word8)
{-# INLINE callocPinnedMBytes #-}

callocPinnedAlignedMBytes ::
     MonadPrim s m
  => Count Word8 -- ^ Size in number of bytes
  -> Int -- ^ Alignment in number of bytes
  -> m (MBytes 'Pin s)
callocPinnedAlignedMBytes n a = do
  mb <- allocPinnedAlignedMBytes n a
  mb <$ setMBytes mb 0 n (0 :: Word8)
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



-- copyBytesToMBytes8 ::
--      MonadPrim s m => Bytes p -> Off Word8 -> MBytes pd s -> Off Word8 -> Count Word8 -> m ()
-- copyBytesToMBytes8 (Bytes src#) (Off (I# srcOff#)) (MBytes dst#) (Off (I# dstOff#)) (Count (I# n#)) =
--   prim_ (copyByteArray# src# srcOff# dst# dstOff# n#)
-- {-# INLINE copyBytesToMBytes8 #-}

-- copyMBytesToMBytes8 ::
--      MonadPrim s m => MBytes ps s -> Off Word8 -> MBytes pd s -> Off Word8 -> Count Word8 -> m ()
-- copyMBytesToMBytes8 (MBytes src#) (Off (I# srcOff#)) (MBytes dst#) (Off (I# dstOff#)) (Count (I# n#)) =
--   prim_ (copyMutableByteArray# src# srcOff# dst# dstOff# n#)
-- {-# INLINE copyMBytesToMBytes8 #-}


copyBytesToMBytes ::
     (MonadPrim s m, Prim a) => Bytes p -> Off a -> MBytes pd s -> Off a -> Count a -> m ()
copyBytesToMBytes (Bytes src#) srcOff (MBytes dst#) dstOff c =
  prim_ $
  copyByteArray# src# (fromOff# srcOff) dst# (fromOff# dstOff) (fromCount# c)
{-# INLINE[0] copyBytesToMBytes #-}

copyMBytesToMBytes ::
     (MonadPrim s m, Prim a) => MBytes ps s-> Off a -> MBytes pd s -> Off a -> Count a -> m ()
copyMBytesToMBytes (MBytes src#) srcOff (MBytes dst#) dstOff c =
  prim_ (copyMutableByteArray# src# (fromOff# srcOff) dst# (fromOff# dstOff) (fromCount# c))
{-# INLINE[0] copyMBytesToMBytes #-}





-- moveMBytesToPtr8 :: MonadPrim s m => MBytes p s -> Int -> Ptr Word8 -> Int -> Count Word8 -> m ()
-- moveMBytesToPtr8 (MBytes src#) (I# srcOff#) (Ptr dstAddr#) (I# dstOff#) (Count (I# n#)) =
--   let addr# = dstAddr# `plusAddr#` dstOff#
--   in prim_ (moveMutableByteArrayToAddr# src# srcOff# addr# n#)
-- {-# INLINE moveMBytesToPtr8 #-}

-- moveMBytesToMBytes8 ::
--   MonadPrim s m => MBytes ps s -> Off Word8 -> MBytes pd s -> Off Word8 -> Count Word8 -> m ()
-- moveMBytesToMBytes8 (MBytes src#) (Off (I# srcOff#)) (MBytes dst#) (Off (I# dstOff#)) (Count (I# n#)) =
--   unsafeIOToPrim $
--   memmoveMutableByteArray# src# srcOff# dst# dstOff# n#
-- {-# INLINE moveMBytesToMBytes8 #-}

-- {-# RULES
-- "copyBytesToMBytes8" copyBytesToMBytes = copyBytesToMBytes8
-- "copyMBytesToMBytes8" copyMBytesToMBytes = copyMBytesToMBytes8
-- "moveMBytesToMBytes8" moveMBytesToMBytes = moveMBytesToMBytes8
--   #-}

moveMBytesToMBytes ::
  (MonadPrim s m, Prim a) => MBytes ps s -> Off a -> MBytes pd s -> Off a -> Count a -> m ()
moveMBytesToMBytes (MBytes src#) srcOff (MBytes dst#) dstOff c =
  unsafeIOToPrim $
  memmoveMutableByteArray#
    src#
    (fromOff# srcOff)
    dst#
    (fromOff# dstOff)
    (fromCount# c)
{-# INLINE[0] moveMBytesToMBytes #-}


sizeOfBytes :: Bytes p -> Int
sizeOfBytes (Bytes ba#) = I# (sizeofByteArray# ba#)
{-# INLINE sizeOfBytes #-}

-- countOfBytes8 :: Bytes p -> Count Word8
-- countOfBytes8 = Count . sizeOfBytes
-- {-# INLINE countOfBytes8 #-}
-- {-# RULES "countOfBytes8" countOfBytes = countOfBytes8 #-}

-- | How many elements of type @a@ fits into bytes completely. In order to get a possible
-- count of leftover bytes use `countRemOfBytes`
countOfBytes :: forall a p. Prim a => Bytes p -> Count a
countOfBytes b =
  coerce (quotSizeOfWith (proxy# :: Proxy# a) (sizeOfBytes b) 0 quotInt)
{-# INLINE[0] countOfBytes #-}

-- | Get the count of elements of type @a@ that can fit into bytes as well as the slack
-- number of bytes that would be leftover in case when total number of bytes available is
-- not exactly divisable by the size of the element that will be stored in the memory
-- chunk.
countRemOfBytes :: forall a p. Prim a => Bytes p -> (Count a, Int)
countRemOfBytes b =
  coerce (quotSizeOfWith (proxy# :: Proxy# a) (sizeOfBytes b) (0, 0) quotRemInt)
{-# INLINE countRemOfBytes #-}

quotSizeOfWith :: forall a b. Prim a => Proxy# a -> Int -> b -> (Int -> Int -> b) -> b
quotSizeOfWith px# sz onZero quotWith
  | tySize <= 0 = onZero
  | otherwise = sz `quotWith` tySize
  where
    tySize = sizeOf# px#
{-# INLINE quotSizeOfWith #-}

-- getCountOfMBytes8 :: MonadPrim s m => MBytes p s -> m (Count Word8)
-- getCountOfMBytes8 = fmap coerce . getSizeOfMBytes
-- {-# INLINE getCountOfMBytes8 #-}
-- {-# RULES "getCountOfMBytes8" getCountOfMBytes = getCountOfMBytes8 #-}

-- | How many elements of type @a@ fits into bytes completely. In order to get any number
-- of leftover bytes use `countRemOfBytes`
getCountOfMBytes :: forall a p s m. (MonadPrim s m, Prim a) => MBytes p s -> m (Count a)
getCountOfMBytes b =
  (\sz -> coerce (quotSizeOfWith (proxy# :: Proxy# a) sz 0 quotInt)) <$>
  getSizeOfMBytes b
{-# INLINE[0] getCountOfMBytes #-}

-- | Get the number of elements of type @a@ that can fit into bytes as well as the slack
-- number of bytes that would be leftover in case when total number of bytes available is
-- not exactly divisable by the size of the element that will be stored in the memory
-- chunk.
getCountRemOfMBytes :: forall a p s m. (MonadPrim s m, Prim a) => MBytes p s -> m (Count a, Int)
getCountRemOfMBytes b =
  (\sz -> coerce (quotSizeOfWith (proxy# :: Proxy# a) sz (0, 0) quotRemInt)) <$>
  getSizeOfMBytes b
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
        let !v = indexBytes bs (Off i)
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
        | i < n = writeMBytes mb (Off i) x >> go xs (i + 1)
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


readMBytes :: (MonadPrim s m, Prim a) => MBytes p s -> Off a -> m a
readMBytes (MBytes mba#) (Off (I# i#)) = prim (readMutableByteArray# mba# i#)
{-# INLINE readMBytes #-}

writeMBytes :: (MonadPrim s m, Prim a) => MBytes p s -> Off a -> a -> m ()
writeMBytes (MBytes mba#) (Off (I# i#)) a = prim_ (writeMutableByteArray# mba# i# a)
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
    Nothing  -> do
      let n8 = Count (sizeOfBytes b) :: Count Word8
      pmb <- allocPinnedMBytes n8
      copyBytesToMBytes b 0 pmb 0 n8
      freezeMBytes pmb
{-# INLINE ensurePinnedBytes #-}

ensurePinnedMBytes :: MonadPrim s m => MBytes p s -> m (MBytes 'Pin s)
ensurePinnedMBytes mb =
  case toPinnedMBytes mb of
    Just pmb -> pure pmb
    Nothing  -> do
      n8 <- Count <$> getSizeOfMBytes mb
      pmb <- allocPinnedMBytes n8
      pmb <$ copyMBytesToMBytes mb 0 pmb 0 n8
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



setMBytes ::
     (MonadPrim s m, Prim a)
  => MBytes p s -- ^ Chunk of memory to fill
  -> Off a -- ^ Offset in number of elements
  -> Count a -- ^ Number of cells to fill
  -> a -- ^ A value to fill the cells with
  -> m ()
setMBytes (MBytes mba#) (Off (I# o#)) (Count (I# n#)) a = prim_ (setMutableByteArray# mba# o# n# a)
{-# INLINE setMBytes #-}


getBytesPtr :: Bytes 'Pin -> Ptr a
getBytesPtr (Bytes ba#) = Ptr (byteArrayContents# ba#)
{-# INLINE getBytesPtr #-}

getMBytesPtr :: MBytes 'Pin s -> Ptr a
getMBytesPtr (MBytes mba#) = Ptr (byteArrayContents# (unsafeCoerce# mba#))
{-# INLINE getMBytesPtr #-}

withMBytesPtr :: MonadPrim s m => MBytes 'Pin s -> (Ptr a -> m b) -> m b
withMBytesPtr mb f = do
  let ptr = getMBytesPtr mb
  res <- f ptr
  res <$ touch mb
{-# INLINE withMBytesPtr #-}


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


