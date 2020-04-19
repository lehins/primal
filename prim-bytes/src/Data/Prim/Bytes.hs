{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
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
  , cloneBytes
  , emptyBytes
  , singletonBytes
  , isEmptyBytes
  , createBytes
  , createBytes_
  , createBytesST
  , createBytesST_
  , MBytes(..)
  , Pinned(..)
  , Count(..)
  , isSameBytes
  , isSamePinnedBytes
  , isSameMBytes
  , relaxPinned
  , isPinnedBytes
  , isPinnedMBytes
  , toPinnedBytes
  , toPinnedMBytes
  , ensurePinnedBytes
  , ensurePinnedMBytes
  , indexBytes
  , sizeOfBytes
  , countOfBytes
  , countRemOfBytes
  , fromListBytes
  , fromListBytesN
  , fromListBytesN_
  , toListBytes
  , concatBytes
  , foldrBytes
  -- * Mutable
  -- ** To/From immutable
  , thawBytes
  , freezeMBytes
  -- ** Construction
  , allocMBytes
  , allocPinnedMBytes
  , allocAlignedMBytes
  , callocMBytes
  , callocAlignedMBytes
  , resizeMBytes
  , showsBytesHex
  , coerceStateMBytes
  -- ** Modifying data
  , cloneMBytes
  , withCloneMBytes
  , withCloneMBytes_
  , withCloneMBytesST
  , withCloneMBytesST_
  , loadListMBytes
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
  , zeroMBytes
  -- ** Ptr
  , withPtrMBytes
  , withNoHaltPtrMBytes
  , getPtrBytes
  , getPtrMBytes
  -- ** ForeignPtr
  , getBytesForeignPtr
  , getMBytesForeignPtr
  , module Data.Prim
  -- * Helpers
  -- * Experimental
  -- , fillPinnedMBytesWord64LE
  -- , fillPinnedMBytesWith
  ) where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Prim
import Control.Monad.Prim.Unsafe
import Control.Monad.ST
import Data.Foldable as Foldable
import Data.List as List
import Data.Prim
import Data.Prim.Class
import Data.Prim.Foreign (getSizeofMutableByteArray#, isByteArrayPinned#,
                          isMutableByteArrayPinned#, isSameByteArray#,
                          memmoveMutableByteArray#, memcmpByteArray#)
import Data.Proxy
import Data.Typeable
import GHC.Exts hiding (getSizeofMutableByteArray#, isByteArrayPinned#,
                 isMutableByteArrayPinned#)
import GHC.ForeignPtr
import Numeric (showHex)


-- | Memory can either be Pinned or Inconclusive. Use eith `toPinnedBytes` or
-- `toPinnedMBytes` to get a conclusive answer for the latter case.
data Pinned = Pin | Inc

data Bytes (p :: Pinned) = Bytes ByteArray#

instance NFData (Bytes p) where
  rnf (Bytes _) = ()

instance Show (Bytes p) where
  show b =
    Foldable.foldr' ($) "]" $
    ('[' :) : List.intersperse (',' :) (map (("0x" ++) .) (showsBytesHex b))

instance Typeable p => IsList (Bytes p) where
  type Item (Bytes p) = Word8
  fromList = fromListBytes
  fromListN n = fromListBytesN_ (Count n)
  toList = toListBytes

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

relaxPinned :: Bytes p -> Bytes 'Inc
relaxPinned = coerce

-- | Check if two byte arrays refer to pinned memory and compare their pointers.
isSameBytes :: Bytes p1 -> Bytes p2 -> Bool
isSameBytes (Bytes b1#) (Bytes b2#) = isTrue# (isSameByteArray# b1# b2#)
{-# INLINE[0] isSameBytes #-}
{-# RULES
"isSamePinnedBytes" isSameBytes = isSamePinnedBytes
  #-}

-- | Perform pointer equality on pinned `Bytes`.
isSamePinnedBytes :: Bytes 'Pin -> Bytes 'Pin -> Bool
isSamePinnedBytes pb1 pb2 = getPtrBytes pb1 == getPtrBytes pb2
{-# INLINE isSamePinnedBytes #-}

-- | Check if two mutable bytes pointers refer to the same memory
isSameMBytes :: MBytes p1 s -> MBytes p2 s -> Bool
isSameMBytes (MBytes mb1#) (MBytes mb2#) =
  isTrue# (sameMutableByteArray# mb1# mb2#)
{-# INLINE isSameMBytes #-}

eqBytes :: Bytes p1 -> Bytes p2 -> Bool
eqBytes b1@(Bytes ba1#) b2@(Bytes ba2#) =
  isSameBytes b1 b2 ||
  (lenEq && isTrue# (memcmpByteArray# ba1# 0# ba2# 0# len# ==# 0# ))
  where
    !n1@(I# len#) = sizeOfBytes b1
    lenEq = n1 == sizeOfBytes b2
{-# INLINE eqBytes #-}

data MBytes (p :: Pinned) s = MBytes (MutableByteArray# s)

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

castBytes :: Bytes p1 -> Bytes p2
castBytes = coerce

emptyBytes :: Bytes p
emptyBytes = castBytes $ runST $ allocUnpinnedMBytes (0 :: Count Word8) >>= freezeMBytes
{-# INLINE emptyBytes #-}

isEmptyBytes :: Bytes p -> Bool
isEmptyBytes b = sizeOfBytes b == 0
{-# INLINE isEmptyBytes #-}

singletonBytes :: forall a p. (Prim a, Typeable p) => a -> Bytes p
singletonBytes a = runST $ do
  mb <- allocMBytes (1 :: Count a)
  writeMBytes mb 0 a
  freezeMBytes mb
{-# INLINE singletonBytes #-}

---- Mutable


allocMBytes ::
     forall p a s m. (Typeable p, Prim a, MonadPrim s m)
  => Count a
  -> m (MBytes p s)
allocMBytes c =
  case eqT :: Maybe (p :~: 'Pin) of
    Just Refl -> allocPinnedMBytes c
    _ ->
      case eqT :: Maybe (p :~: 'Inc) of
        Just Refl -> allocUnpinnedMBytes c
        Nothing ->
          errorImpossible
            "allocMBytes"
            $ "Unexpected 'Pinned' kind: '" ++ showsType (Proxy :: Proxy (Bytes p)) "'."
{-# INLINE[0] allocMBytes #-}
{-# RULES
"allocUnpinnedMBytes" allocMBytes = allocUnpinnedMBytes
"allocPinnedMBytes" allocMBytes = allocPinnedMBytes
  #-}

-- | Attempt to resize mutable bytes in place.
--
-- * New bytes might be allocated, with the copy of an old one.
-- * Old references should not be kept around to allow GC to claim it
-- * Old references should not be used to avoid undefined behavior
resizeMBytes ::
     (MonadPrim s m, Prim a) => MBytes p1 s -> Count a -> m (MBytes 'Inc s)
resizeMBytes (MBytes mb#) c =
  prim $ \s# ->
    case resizeMutableByteArray# mb# (fromCount# c) s# of
      (# s'#, mb'# #) -> (# s'#, MBytes mb'# #)
{-# INLINE resizeMBytes #-}

cloneBytes :: Typeable p => Bytes p -> Bytes p
cloneBytes b = runST $ thawBytes b >>= cloneMBytes >>= freezeMBytes
{-# INLINE cloneBytes #-}

cloneMBytes :: (MonadPrim s m, Typeable p) => MBytes p s -> m (MBytes p s)
cloneMBytes mb = do
  n <- getCountOfMBytes mb
  mb' <- allocMBytes (n :: Count Word8)
  mb' <$ copyMBytesToMBytes mb 0 mb' 0 n
{-# INLINE cloneMBytes #-}


-- | Allocated memory is not cleared, so make sure to fill it in properly, otherwise you
-- might find some garbage there.
createBytes ::
     forall p a b s m. (Prim a, Typeable p, MonadPrim s m)
  => Count a
  -> (MBytes p s -> m b)
  -> m (b, Bytes p)
createBytes n f = do
  mb <- allocMBytes n
  !res <- f mb
  (,) res <$> freezeMBytes mb
{-# INLINE createBytes #-}

createBytes_ ::
     forall p a b s m. (Prim a, Typeable p, MonadPrim s m)
  => Count a
  -> (MBytes p s -> m b)
  -> m (Bytes p)
createBytes_ n f = allocMBytes n >>= \mb -> f mb >> freezeMBytes mb
{-# INLINE createBytes_ #-}

createBytesST ::
     forall p a b. (Prim a, Typeable p)
  => Count a
  -> (forall s . MBytes p s -> ST s b)
  -> (b, Bytes p)
createBytesST n f = runST $ createBytes n f
{-# INLINE createBytesST #-}

createBytesST_ ::
     forall p a b. (Prim a, Typeable p)
  => Count a
  -> (forall s. MBytes p s -> ST s b)
  -> Bytes p
createBytesST_ n f =  runST $ createBytes_ n f
{-# INLINE createBytesST_ #-}

allocUnpinnedMBytes :: (MonadPrim s m, Prim a) => Count a -> m (MBytes 'Inc s)
allocUnpinnedMBytes c =
  prim $ \s# ->
    case newByteArray# (fromCount# c) s# of
      (# s'#, ba# #) -> (# s'#, MBytes ba# #)
{-# INLINE allocUnpinnedMBytes #-}


allocPinnedMBytes :: (MonadPrim s m, Prim a) => Count a -> m (MBytes 'Pin s)
allocPinnedMBytes c =
  prim $ \s# ->
    case newPinnedByteArray# (fromCount# c) s# of
      (# s'#, ba# #) -> (# s'#, MBytes ba# #)
{-# INLINE allocPinnedMBytes #-}

allocAlignedMBytes ::
     (MonadPrim s m, Prim a)
  => Count a -- ^ Size in number of bytes
  -> m (MBytes 'Pin s)
allocAlignedMBytes c =
  prim $ \s# ->
    case alignmentProxy c of
      I# a# ->
        case newAlignedPinnedByteArray# (fromCount# c) a# s# of
          (# s'#, ba# #) -> (# s'#, MBytes ba# #)
{-# INLINE allocAlignedMBytes #-}

callocMBytes :: (MonadPrim s m, Prim a, Typeable p) => Count a -> m (MBytes p s)
callocMBytes n = do
  mb <- allocMBytes n
  mb <$ setMBytes mb 0 (countWord8 n) (0 :: Word8)
{-# INLINE callocMBytes #-}

callocAlignedMBytes ::
     (MonadPrim s m, Prim a)
  => Count a -- ^ Size in number of bytes
  -> m (MBytes 'Pin s)
callocAlignedMBytes n = do
  mb <- allocAlignedMBytes n
  mb <$ setMBytes mb 0 (countWord8 n) (0 :: Word8)
{-# INLINE callocAlignedMBytes #-}


getSizeOfMBytes :: MonadPrim s m => MBytes p s -> m Int
getSizeOfMBytes (MBytes ba#) =
  prim $ \s# ->
    case getSizeofMutableByteArray# ba# s# of
      (# s'#, n# #) -> (# s'#, I# n# #)
{-# INLINE getSizeOfMBytes #-}


-- | Fill the mutable array with zeros efficiently.
zeroMBytes :: MonadPrim s m => MBytes p s -> m ()
zeroMBytes mba@(MBytes mba#) = do
  I# n# <- getSizeOfMBytes mba
  prim_ (setByteArray# mba# 0# n# 0#)
{-# INLINE zeroMBytes #-}

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


withCloneMBytes ::
     (MonadPrim s m, Typeable p)
  => Bytes p
  -> (MBytes p s -> m a)
  -> m (a, Bytes p)
withCloneMBytes b f = do
  mb <- cloneMBytes =<< thawBytes b
  !res <- f mb
  b' <- freezeMBytes mb
  pure (res, b')
{-# INLINE withCloneMBytes #-}

withCloneMBytes_ ::
  (MonadPrim s m, Typeable p)
  => Bytes p
  -> (MBytes p s -> m a)
  -> m (Bytes p)
withCloneMBytes_ b f = thawBytes b >>= cloneMBytes >>= \mb -> f mb >> freezeMBytes mb
{-# INLINE withCloneMBytes_ #-}

withCloneMBytesST ::
  Typeable p => Bytes p -> (forall s. MBytes p s -> ST s a) -> (a, Bytes p)
withCloneMBytesST b f = runST $ withCloneMBytes b f
{-# INLINE withCloneMBytesST #-}

withCloneMBytesST_ ::
  Typeable p => Bytes p -> (forall s. MBytes p s -> ST s a) -> Bytes p
withCloneMBytesST_ b f = runST $ withCloneMBytes_ b f
{-# INLINE withCloneMBytesST_ #-}



copyBytesToMBytes ::
     (MonadPrim s m, Prim a) => Bytes p -> Off a -> MBytes pd s -> Off a -> Count a -> m ()
copyBytesToMBytes (Bytes src#) srcOff (MBytes dst#) dstOff c =
  prim_ $
  copyByteArray# src# (fromOff# srcOff) dst# (fromOff# dstOff) (fromCount# c)
{-# INLINE copyBytesToMBytes #-}

copyMBytesToMBytes ::
     (MonadPrim s m, Prim a) => MBytes ps s-> Off a -> MBytes pd s -> Off a -> Count a -> m ()
copyMBytesToMBytes (MBytes src#) srcOff (MBytes dst#) dstOff c =
  prim_ (copyMutableByteArray# src# (fromOff# srcOff) dst# (fromOff# dstOff) (fromCount# c))
{-# INLINE copyMBytesToMBytes #-}


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
{-# INLINE moveMBytesToMBytes #-}


sizeOfBytes :: Bytes p -> Int
sizeOfBytes (Bytes ba#) = I# (sizeofByteArray# ba#)
{-# INLINE sizeOfBytes #-}

-- | How many elements of type @a@ fits into bytes completely. In order to get a possible
-- count of leftover bytes use `countRemOfBytes`
countOfBytes :: forall a p. Prim a => Bytes p -> Count a
countOfBytes b = countSize (sizeOfBytes b)
{-# INLINE countOfBytes #-}

-- | Get the count of elements of type @a@ that can fit into bytes as well as the slack
-- number of bytes that would be leftover in case when total number of bytes available is
-- not exactly divisable by the size of the element that will be stored in the memory
-- chunk.
countRemOfBytes :: forall a p. Prim a => Bytes p -> (Count a, Int)
countRemOfBytes b = countRemSize (sizeOfBytes b)
{-# INLINE countRemOfBytes #-}



-- | How many elements of type @a@ fits into bytes completely. In order to get any number
-- of leftover bytes use `countRemOfBytes`
getCountOfMBytes :: forall a p s m. (MonadPrim s m, Prim a) => MBytes p s -> m (Count a)
getCountOfMBytes b = countSize <$> getSizeOfMBytes b
{-# INLINE getCountOfMBytes #-}

-- | Get the number of elements of type @a@ that can fit into bytes as well as the slack
-- number of bytes that would be leftover in case when total number of bytes available is
-- not exactly divisable by the size of the element that will be stored in the memory
-- chunk.
getCountRemOfMBytes :: forall a p s m. (MonadPrim s m, Prim a) => MBytes p s -> m (Count a, Int)
getCountRemOfMBytes b = countRemSize <$> getSizeOfMBytes b
{-# INLINE getCountRemOfMBytes #-}

-- | It is only guaranteed to convert the whole memory to a list whenever the size of
-- allocated memory is exactly divisible by the size of the element, otherwise there will
-- be some slack left unaccounted for.
toListBytes :: Prim a => Bytes p -> [a]
toListBytes ba = build (\ c n -> foldrBytes c n ba)
{-# INLINE toListBytes #-}

foldrBytes :: forall a b p . Prim a => (a -> b -> b) -> b -> Bytes p -> b
foldrBytes c nil bs = go 0
  where
    Count k = countOfBytes bs :: Count a
    go i
      | i == k = nil
      | otherwise =
        let !v = indexBytes bs (Off i)
         in v `c` go (i + 1)
{-# INLINE[0] foldrBytes #-}


loadListInternal :: (MonadPrim s m, Prim a) => Count a -> Int -> [a] -> MBytes p s -> m Ordering
loadListInternal (Count n) slack ys mb = do
  let go [] !i = pure (compare i n <> compare 0 slack)
      go (x:xs) !i
        | i < n = writeMBytes mb (Off i) x >> go xs (i + 1)
        | otherwise = pure GT
  go ys 0
{-# INLINE loadListInternal #-}

-- | Returns `EQ` if the full list did fit into the supplied memory chunk exactly.
-- Otherwise it will return either `LT` if the list was smaller than allocated memory or
-- `GT` if the list was bigger than the available memory and did not fit into `MBytes`.
loadListMBytes :: forall a p s m . (MonadPrim s m, Prim a) => [a] -> MBytes p s -> m Ordering
loadListMBytes ys mb = do
  (c :: Count a, slack) <- getCountRemOfMBytes mb
  loadListInternal c slack ys mb
{-# INLINE loadListMBytes #-}

loadListMBytes_ :: forall a p s m . (MonadPrim s m, Prim a) => [a] -> MBytes p s -> m ()
loadListMBytes_ ys mb = do
  c :: Count a <- getCountOfMBytes mb
  void $ loadListInternal c 0 ys mb
{-# INLINE loadListMBytes_ #-}

fromListBytesN_ ::
     forall a p. (Prim a, Typeable p)
  => Count a
  -> [a]
  -> Bytes p
fromListBytesN_ n xs = createBytesST_ n (loadListMBytes_ xs)
{-# INLINE fromListBytesN_ #-}

-- | If the list is bigger than the supplied @`Count` a@ then `GT` ordering will be
-- returned, along with the `Bytes` fully filled with the prefix of the list. On the other
-- hand if the list is smaller than the supplied `Count`, `LT` with partially filled
-- `Bytes` will returned. In the latter case expect some garbage at the end of the
-- allocated memory, since no attempt is made to zero it out. Exact match obviously
-- results in an `EQ`.
fromListBytesN ::
     forall a p. (Prim a, Typeable p)
  => Count a
  -> [a]
  -> (Ordering, Bytes p)
fromListBytesN n xs = createBytesST n (loadListMBytes xs)
{-# INLINE fromListBytesN #-}

fromListBytes ::
     forall a p. (Prim a, Typeable p)
  => [a]
  -> Bytes p
fromListBytes xs = fromListBytesN_ (Count (length xs)) xs
{-# INLINE fromListBytes #-}


concatBytes ::
     forall p p'. Typeable p
  => [Bytes p']
  -> Bytes p
concatBytes xs = do
  let c = Foldable.foldl' (\acc b -> acc + countOfBytes b) 0 xs
  createBytesST_ (c :: Count Word8) $ \mb -> do
    let load i b = do
          let cb@(Count n) = countOfBytes b :: Count Word8
          (i + Off n) <$ copyBytesToMBytes b 0 mb i cb
    foldM_ load 0 xs
{-# INLINE concatBytes #-}

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


ensurePinnedBytes :: Bytes p -> Bytes 'Pin
ensurePinnedBytes b =
  case toPinnedBytes b of
    Just pb -> pb
    Nothing ->
      runST $ do
        let n8 = countOfBytes b :: Count Word8
        pmb <- allocPinnedMBytes n8
        copyBytesToMBytes b 0 pmb 0 n8
        freezeMBytes pmb
{-# INLINE ensurePinnedBytes #-}

ensurePinnedMBytes :: MonadPrim s m => MBytes p s -> m (MBytes 'Pin s)
ensurePinnedMBytes mb =
  case toPinnedMBytes mb of
    Just pmb -> pure pmb
    Nothing  -> do
      n8 :: Count Word8 <- getCountOfMBytes mb
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


getPtrBytes :: Bytes 'Pin -> Ptr a
getPtrBytes (Bytes ba#) = Ptr (byteArrayContents# ba#)
{-# INLINE getPtrBytes #-}

getBytesForeignPtr :: Bytes 'Pin -> ForeignPtr a
getBytesForeignPtr (Bytes ba#) =
  ForeignPtr (byteArrayContents# ba#) $ PlainPtr (unsafeCoerce# ba#)
{-# INLINE getBytesForeignPtr #-}

getPtrMBytes :: MBytes 'Pin s -> Ptr a
getPtrMBytes (MBytes mba#) = Ptr (byteArrayContents# (unsafeCoerce# mba#))
{-# INLINE getPtrMBytes #-}

getMBytesForeignPtr :: MBytes 'Pin s -> ForeignPtr a
getMBytesForeignPtr (MBytes mba#) =
  ForeignPtr (byteArrayContents# (unsafeCoerce# mba#)) (PlainPtr (unsafeCoerce# mba#))
{-# INLINE getMBytesForeignPtr #-}

withPtrMBytes :: MonadPrim s m => MBytes 'Pin s -> (Ptr a -> m b) -> m b
withPtrMBytes mb f = do
  res <- f (getPtrMBytes mb)
  res <$ touch mb
{-# INLINE withPtrMBytes #-}

withNoHaltPtrMBytes ::
     (MonadPrimBase s n, MonadPrim s m)
  => MBytes 'Pin s
  -> (Ptr a -> n b)
  -> m b
withNoHaltPtrMBytes mb f = withPrimBase mb $ f (getPtrMBytes mb)
{-# INLINE withNoHaltPtrMBytes #-}


compareBytes (Bytes b1#) off1 (Bytes b2#) off2 c =
  case compareByteArrays# b1# (fromOff# off1) b2# (fromOff# off2) (fromCount# c) of
    0# -> EQ
    --order# | gt
