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
  , Ptr(..)
  --, ForeignPtr
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
  , withCopyPinnedMBytes
  , withCopyPinnedMBytes_
  , copyBytesToPtr
  , copyMBytesToPtr
  , applyFreezeMBytes
  -- ** Moving data
  , copyBytesToMBytes
  , copyMBytesToMBytes
  -- * Size
  , getSizeOfMBytes
  , countOfMBytes
  , countRemOfMBytes
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
  ) where

import Data.Proxy
import Data.ByteString.Builder.Prim (word64LE, word32LE)
import Data.ByteString.Builder.Prim.Internal (runF)
--import Data.ByteString.Internal (ByteString(PS))
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
                          isMutableByteArrayPinned#)
import GHC.Exts hiding (getSizeofMutableByteArray#, isByteArrayPinned#,
                 isMutableByteArrayPinned#)
import GHC.Word
import Numeric (showHex)
import Foreign.Ptr

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

withCopyAllocMBytes ::
     MonadPrim s m
  => (Int -> m (MBytes pm s))
  -> Bytes p
  -> (MBytes pm s -> m a)
  -> m (a, Bytes pm)
withCopyAllocMBytes alloc b f = do
  let n = sizeOfBytes b
  mb <- alloc n
  copyBytesToMBytes b 0 mb 0 n
  applyFreezeMBytes f mb
{-# INLINE withCopyAllocMBytes #-}

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

withCopyMBytes ::
     MonadPrim s m => Bytes p -> (MBytes 'Inc s -> m a) -> m (a, Bytes 'Inc)
withCopyMBytes = withCopyAllocMBytes allocMBytes
{-# INLINE withCopyMBytes #-}

withCopyMBytes_ ::
     MonadPrim s m => Bytes p -> (MBytes 'Inc s -> m a) -> m (Bytes 'Inc)
withCopyMBytes_ b f = snd <$> withCopyMBytes b f
{-# INLINE withCopyMBytes_ #-}

withCopyPinnedMBytes ::
     MonadPrim s m => Bytes p -> (MBytes 'Pin s -> m a) -> m (a, Bytes 'Pin)
withCopyPinnedMBytes = withCopyAllocMBytes allocPinnedMBytes
{-# INLINE withCopyPinnedMBytes #-}

withCopyPinnedMBytes_ ::
     MonadPrim s m => Bytes p -> (MBytes 'Pin s -> m a) -> m (Bytes 'Pin)
withCopyPinnedMBytes_ b f = snd <$> withCopyPinnedMBytes b f
{-# INLINE withCopyPinnedMBytes_ #-}

copyBytesToMBytes ::
     MonadPrim s m => Bytes p -> Int -> MBytes pd s -> Int -> Int -> m ()
copyBytesToMBytes (Bytes bSrc#) (I# bSrcOff#) (MBytes mbDst#) (I# mbDstOff#) (I# n#) =
  prim_ (copyByteArray# bSrc# bSrcOff# mbDst# mbDstOff# n#)
{-# INLINE copyBytesToMBytes #-}

copyMBytesToMBytes ::
     MonadPrim s m => MBytes ps s-> Int -> MBytes pd s -> Int -> Int -> m ()
copyMBytesToMBytes (MBytes mbSrc#) (I# mbSrcOff#) (MBytes mbDst#) (I# mbDstOff#) (I# n#) =
  prim_ (copyMutableByteArray# mbSrc# mbSrcOff# mbDst# mbDstOff# n#)
{-# INLINE copyMBytesToMBytes #-}

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
    n <- getSizeOfMBytes mb
    let sz = sizeOfProxy (Proxy :: Proxy a)
        (n64, nrem64) = n `quotRem` sz
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
        copyMBytesToPtr w64mb 0 ptr' 0
        pure g'
{-# INLINE fillPinnedMBytesWith #-}

copyMBytesToPtr :: MonadPrim s m => MBytes p s -> Int -> Ptr a -> Int -> m ()
copyMBytesToPtr (MBytes src#) (I# srcOff#) (Ptr dstAddr#) (I# dstOff#) =
  prim_ (copyMutableByteArrayToAddr# src# srcOff# dstAddr# dstOff#)
{-# INLINE copyMBytesToPtr #-}

--
copyBytesToPtr :: MonadPrim s m => Bytes p -> Int -> Ptr a -> Int -> m ()
copyBytesToPtr (Bytes src#) (I# srcOff#) (Ptr dstAddr#) (I# dstOff#) =
  prim_ (copyByteArrayToAddr# src# srcOff# dstAddr# dstOff#)
{-# INLINE copyBytesToPtr #-}



withMBytesPtr :: MonadPrim s m => MBytes 'Pin s -> (Ptr a -> m b) -> m b
withMBytesPtr mb f = do
  let ptr = getMBytesPtr mb
  res <- f ptr
  touch mb
  pure res
{-# INLINE withMBytesPtr #-}
