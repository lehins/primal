{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
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
  , indexBytes
  , countOfBytes
  , countRemOfBytes
  , fromListBytes
  , fromListBytesN
  , toListBytes
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
    -- * Size
  , getSizeOfMBytes
  , countOfMBytes
  , countRemOfMBytes
  , readMBytes
  , writeMBytes
  , setMBytes
  , getBytesPtr
  , getMBytesPtr
  , module Data.Prim
  ) where

import Control.Arrow
import Control.DeepSeq
import Control.Monad
import Control.Monad.Prim
import Control.Monad.ST
import Data.List as List
import Data.Foldable as Foldable
import Data.Prim
import Data.Prim.Class
import Data.Prim.Foreign (isByteArrayPinned#, isMutableByteArrayPinned#, getSizeofMutableByteArray#)
import GHC.Exts hiding (isByteArrayPinned#, isMutableByteArrayPinned#, getSizeofMutableByteArray#)
import Numeric (showHex)
import GHC.Word

data Pinned = Pinned

data Bytes (p :: Pinned) = Bytes ByteArray#

instance Show (Bytes p) where
  show b =
    Foldable.foldr' ($) "]" $
    (('[' :) : List.intersperse (',' :) (map (("0x" ++) .) (showsBytesHex b)))

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

newMBytes :: (MonadPrim s m, Prim a) => Count a -> m (MBytes p s)
newMBytes es@(Count n) = allocMBytes (n * sizeOfProxy es)
{-# INLINE newMBytes #-}


newPinnedMBytes :: (MonadPrim s m, Prim a) => Count a -> m (MBytes 'Pinned s)
newPinnedMBytes es@(Count n) = allocPinnedMBytes (n * sizeOfProxy es)
{-# INLINE newPinnedMBytes #-}

newPinnedAlignedMBytes :: (MonadPrim s m, Prim a) => Count a -> m (MBytes 'Pinned s)
newPinnedAlignedMBytes es@(Count n) =
  allocPinnedAlignedMBytes (n * sizeOfProxy es) (alignmentProxy es)
{-# INLINE newPinnedAlignedMBytes #-}


cnewMBytes :: (MonadPrim s m, Prim a) => Count a -> m (MBytes p s)
cnewMBytes c = do
  mb <- newMBytes c
  n <- getSizeOfMBytes mb
  mb <$ setMBytes mb 0 (Count n) (0 :: Word8)
{-# INLINE cnewMBytes #-}

cnewPinnedMBytes :: (MonadPrim s m, Prim a) => Count a -> m (MBytes 'Pinned s)
cnewPinnedMBytes c = do
  mb <- newPinnedMBytes c
  n <- getSizeOfMBytes mb
  mb <$ setMBytes mb 0 (Count n) (0 :: Word8)
{-# INLINE cnewPinnedMBytes #-}

cnewPinnedAlignedMBytes ::
     (MonadPrim s m, Prim a)
  => Count a -- ^ Size in number of bytes
  -> m (MBytes 'Pinned s)
cnewPinnedAlignedMBytes c = do
  mb <- newPinnedAlignedMBytes c
  n <- getSizeOfMBytes mb
  mb <$ setMBytes mb 0 (Count n) (0 :: Word8)
{-# INLINE cnewPinnedAlignedMBytes #-}


allocMBytes :: MonadPrim s m => Int -> m (MBytes p s)
allocMBytes (I# i#) =
  prim $ \s# ->
    case newByteArray# i# s# of
      (# s'#, ba# #) -> (# s'#, MBytes ba# #)
{-# INLINE allocMBytes #-}

allocPinnedMBytes :: MonadPrim s m => Int -> m (MBytes 'Pinned s)
allocPinnedMBytes (I# i#) =
  prim $ \s# ->
    case newPinnedByteArray# i# s# of
      (# s'#, ba# #) -> (# s'#, MBytes ba# #)
{-# INLINE allocPinnedMBytes #-}

allocPinnedAlignedMBytes ::
     MonadPrim s m
  => Int -- ^ Size in number of bytes
  -> Int -- ^ Alignment in number of bytes
  -> m (MBytes 'Pinned s)
allocPinnedAlignedMBytes (I# i#) (I# a#) =
  prim $ \s# ->
    case newAlignedPinnedByteArray# i# a# s# of
      (# s'#, ba# #) -> (# s'#, MBytes ba# #)
{-# INLINE allocPinnedAlignedMBytes #-}

callocMBytes :: MonadPrim s m => Int -> m (MBytes p s)
callocMBytes n = do
  mb <- allocMBytes n
  mb <$ setMBytes mb 0 (Count n) (0 :: Word8)
{-# INLINE callocMBytes #-}

callocPinnedMBytes :: MonadPrim s m => Int -> m (MBytes 'Pinned s)
callocPinnedMBytes n = do
  mb <- allocPinnedMBytes n
  mb <$ setMBytes mb 0 (Count n) (0 :: Word8)
{-# INLINE callocPinnedMBytes #-}

callocPinnedAlignedMBytes ::
     MonadPrim s m
  => Int -- ^ Size in number of bytes
  -> Int -- ^ Alignment in number of bytes
  -> m (MBytes 'Pinned s)
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


withMBytes ::
     MonadPrim s m => Bytes p -> (MBytes pm s -> m a) -> m (a, Bytes pm)
withMBytes b f = do
  let n = sizeOfBytes b
  mb <- allocMBytes n
  copyBytesToMBytes b 0 mb 0 n
  res <- f mb
  b <- freezeMBytes mb
  pure (res, b)


copyBytesToMBytes ::
     MonadPrim s m => Bytes p -> Int -> MBytes pm s -> Int -> Int -> m ()
copyBytesToMBytes (Bytes bSrc#) (I# bSrcOff#) (MBytes mbDst#) (I# mbDstOff#) (I# n#) =
  prim_ (copyByteArray# bSrc# bSrcOff# mbDst# mbDstOff# n#)

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
countOfMBytes :: forall a p s m. (MonadPrim s m, Prim a) => MBytes p s -> m (Count a)
countOfMBytes b = do
  sz <- getSizeOfMBytes b
  pure $ Count (sz `quot` sizeOfType @a)
{-# INLINE countOfMBytes #-}

-- | Get the number of elements of type @a@ that can fit into bytes as well as the slack
-- number of bytes that would be leftover in case when total number of bytes available is
-- not exactly divisable by the size of the element that will be stored in the memory
-- chunk.
countRemOfMBytes :: forall a p s m. (MonadPrim s m, Prim a) => MBytes p s -> m (Count a, Int)
countRemOfMBytes b = do
  sz <- getSizeOfMBytes b
  pure $ first Count (sz `quotRem` sizeOfType @a)
{-# INLINE countRemOfMBytes #-}

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
  (Count n :: Count a, slack) <- countRemOfMBytes mb
  let go [] i = pure (compare i n <> compare 0 slack)
      go (x:xs) i
        | i < n = writeMBytes mb i x >> go xs (i + 1)
        | otherwise = pure GT
  go ys 0
{-# INLINE loadListMBytes #-}

fromListBytesN ::
     forall a p. Prim a
  => Int
  -> [a]
  -> (Ordering, Bytes p)
fromListBytesN n xs = runST $ do
  mb <- newMBytes (Count n :: Count a)
  res <- loadListMBytes xs mb
  (,) res <$> freezeMBytes mb
{-# INLINE fromListBytesN #-}

fromListBytes ::
     forall a p. Prim a
  => [a]
  -> Bytes p
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
{-# INLINE isPinnedBytes #-}

isPinnedMBytes :: MBytes p d -> Bool
isPinnedMBytes (MBytes mb#) = isTrue# (isMutableByteArrayPinned# mb#)
{-# INLINE isPinnedMBytes #-}

toPinnedBytes :: Bytes p -> Maybe (Bytes 'Pinned)
toPinnedBytes (Bytes b#)
  | isTrue# (isByteArrayPinned# b#) = Just (Bytes b#)
  | otherwise = Nothing
{-# INLINE toPinnedBytes #-}

toPinnedMBytes :: MBytes p s -> Maybe (MBytes 'Pinned s)
toPinnedMBytes (MBytes mb#)
  | isTrue# (isMutableByteArrayPinned# mb#) = Just (MBytes mb#)
  | otherwise = Nothing
{-# INLINE toPinnedMBytes #-}




getBytesPtr :: Bytes 'Pinned -> Ptr a
getBytesPtr (Bytes ba#) = Ptr (byteArrayContents# ba#)
{-# INLINE getBytesPtr #-}

getMBytesPtr :: MBytes 'Pinned s -> Ptr a
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
