{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Data.Prim.Memory.Bytes
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Memory.Bytes
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
  , indexOffBytes
  , indexByteOffBytes
  , byteCountBytes
  , countBytes
  , countRemBytes
  , memcmpBytes
  , compareBytes
  , fromListBytes
  , fromListBytesN
  , fromListBytesN_
  , toListBytes
  , concatBytes
  -- * Mutable
  -- ** To/From immutable
  , thawBytes
  , freezeMBytes
  -- ** Construction
  , allocMBytes
  , allocPinnedMBytes
  , allocAlignedMBytes
  , allocUnpinnedMBytes
  , callocMBytes
  , callocAlignedMBytes
  , shrinkMBytes
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
  , moveMBytesToMBytes
  -- ** Moving data
  -- * Size
  , getByteCountMBytes
  , getCountMBytes
  , getCountRemOfMBytes
  -- * Access
  , readOffMBytes
  , readByteOffMBytes
  , writeOffMBytes
  , writeByteOffMBytes
  , setMBytes
  , zeroMBytes
  -- ** Ptr
  , withPtrBytes
  , withNoHaltPtrBytes
  , withPtrMBytes
  , withNoHaltPtrMBytes
  , toPtrBytes
  , toPtrMBytes
  , toForeignPtrBytes
  , toForeignPtrMBytes
  -- * Atomic
  , casMBytes
  , atomicModifyMBytes
  , atomicModifyMBytes_
  , atomicFetchModifyMBytes
  , atomicModifyFetchMBytes
  -- ** Numberic
  , atomicFetchAddMBytes
  , atomicAddFetchMBytes
  , atomicFetchSubMBytes
  , atomicSubFetchMBytes
  -- ** Binary
  , atomicFetchAndMBytes
  , atomicAndFetchMBytes
  , atomicFetchNandMBytes
  , atomicNandFetchMBytes
  , atomicFetchOrMBytes
  , atomicOrFetchMBytes
  , atomicFetchXorMBytes
  , atomicXorFetchMBytes
  , atomicFetchNotMBytes
  , atomicNotFetchMBytes
  -- * Prefetch
  , prefetchBytes0
  , prefetchMBytes0
  , prefetchBytes1
  , prefetchMBytes1
  , prefetchBytes2
  , prefetchMBytes2
  , prefetchBytes3
  , prefetchMBytes3
  , module Data.Prim
  , module Data.Typeable
  -- * Helpers
  -- * Experimental
  -- , fillPinnedMBytesWord64LE
  -- , fillPinnedMBytesWith
  ) where

import Control.DeepSeq
import Control.Monad
import Control.Monad.ST
import Control.Prim.Monad
import Data.Foldable as Foldable
import Data.List as List
import Data.Prim
import Data.Prim.Atomic
import Data.Prim.Class
import Data.Prim.Memory
import Data.Proxy
import GHC.ForeignPtr
import Data.Typeable
import Foreign.Prim
import Numeric (showHex)


-- | Memory can either be Pinned or Inconclusive. Use eith `toPinnedBytes` or
-- `toPinnedMBytes` to get a conclusive answer for the latter case.
data Pinned = Pin | Inc

data Bytes (p :: Pinned) = Bytes ByteArray#
type role Bytes phantom

data MBytes (p :: Pinned) s = MBytes (MutableByteArray# s)
type role MBytes phantom nominal



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

instance Eq (Bytes p) where
  (==) = eqBytes

instance NFData (MBytes p s) where
  rnf (MBytes _) = ()



-- | A list of `ShowS` that covert bytes to base16 encoded strings. Each element of the list
-- is a function that will convert one byte.
--
-- >>> mb <- newPinnedMBytes (Count 5 :: Count Int)
-- >>> mapM_ (\i -> writeOffMBytes mb (pred i) i) [1 .. 5]
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
isSamePinnedBytes pb1 pb2 = toPtrBytes pb1 == toPtrBytes pb2
{-# INLINE isSamePinnedBytes #-}

-- | Check if two mutable bytes pointers refer to the same memory
isSameMBytes :: MBytes p1 s -> MBytes p2 s -> Bool
isSameMBytes (MBytes mb1#) (MBytes mb2#) =
  isTrue# (sameMutableByteArray# mb1# mb2#)
{-# INLINE isSameMBytes #-}

eqBytes :: Bytes p1 -> Bytes p2 -> Bool
eqBytes b1 b2 =
  isSameBytes b1 b2 ||
  (lenEq && memcmpBytes b1 0 b2 0 (coerce n1 :: Count Word8) == EQ)
   --(lenEq && isTrue# (memcmpByteArray# ba1# 0# ba2# 0# len# ==# 0# ))
  where
    n1 = byteCountBytes b1
    lenEq = n1 == byteCountBytes b2
{-# INLINE eqBytes #-}

---- Pure


memcmpBytes :: Prim a => Bytes p1 -> Off a -> Bytes p2 -> Off a -> Count a -> Ordering
memcmpBytes (Bytes ba1#) off1 (Bytes ba2#) off2 c =
  toOrdering# (memcmpByteArray# ba1# (fromOff# off1) ba2# (fromOff# off2) (fromCount# c))
{-# INLINE memcmpBytes #-}

compareBytes :: Prim a => Bytes p1 -> Off a -> Bytes p2 -> Off a -> Count a -> Ordering
compareBytes (Bytes b1#) off1 (Bytes b2#) off2 c =
  toOrdering# (compareByteArrays# b1# (fromOff# off1) b2# (fromOff# off2) (fromCount# c))
{-# INLINE compareBytes #-}

indexOffBytes :: Prim a => Bytes p -> Off a -> a
indexOffBytes (Bytes ba#) (Off (I# i#)) = indexByteArray# ba# i#
{-# INLINE indexOffBytes #-}

indexByteOffBytes :: Prim a => Bytes p -> Off Word8 -> a
indexByteOffBytes (Bytes ba#) (Off (I# i#)) = indexByteOffByteArray# ba# i#
{-# INLINE indexByteOffBytes #-}


-- | This function allows the change of state token. Use with care, because it can allow
-- mutation to escape the `ST` monad.
coerceStateMBytes :: MBytes p s' -> MBytes p s
coerceStateMBytes = unsafeCoerce#


-- Internal function for coercing pinnes
castBytes :: Bytes p1 -> Bytes p2
castBytes = coerce

emptyBytes :: Bytes p
emptyBytes = castBytes $ runST $ allocUnpinnedMBytes (0 :: Count Word8) >>= freezeMBytes
{-# INLINE emptyBytes #-}

isEmptyBytes :: Bytes p -> Bool
isEmptyBytes b = byteCountBytes b == 0
{-# INLINE isEmptyBytes #-}

singletonBytes :: forall a p. (Prim a, Typeable p) => a -> Bytes p
singletonBytes a = runST $ do
  mb <- allocMBytes (1 :: Count a)
  writeOffMBytes mb 0 a
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

shrinkMBytes ::
     (MonadPrim s m, Prim a) => MBytes p s -> Count a -> m ()
shrinkMBytes (MBytes mb#) c = prim_ (shrinkMutableByteArray# mb# (fromCount# c))
{-# INLINE shrinkMBytes #-}


-- | Attempt to resize mutable bytes in place.
--
-- * New bytes might be allocated, with the copy of an old one.
-- * Old references should not be kept around to allow GC to claim it
-- * Old references should not be used to avoid undefined behavior
resizeMBytes ::
     (MonadPrim s m, Prim a) => MBytes p s -> Count a -> m (MBytes 'Inc s)
resizeMBytes (MBytes mb#) c =
  prim $ \s ->
    case resizeMutableByteArray# mb# (fromCount# c) s of
      (# s', mb'# #) -> (# s', MBytes mb'# #)
{-# INLINE resizeMBytes #-}

cloneBytes :: Typeable p => Bytes p -> Bytes p
cloneBytes b = runST $ thawBytes b >>= cloneMBytes >>= freezeMBytes
{-# INLINE cloneBytes #-}

cloneMBytes :: (MonadPrim s m, Typeable p) => MBytes p s -> m (MBytes p s)
cloneMBytes mb = do
  n <- getCountMBytes mb
  mb' <- allocMBytes (n :: Count Word8)
  mb' <$ moveMBytesToMBytes mb 0 mb' 0 n
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
  prim $ \s ->
    case newByteArray# (fromCount# c) s of
      (# s', ba# #) -> (# s', MBytes ba# #)
{-# INLINE allocUnpinnedMBytes #-}


allocPinnedMBytes :: (MonadPrim s m, Prim a) => Count a -> m (MBytes 'Pin s)
allocPinnedMBytes c =
  prim $ \s ->
    case newPinnedByteArray# (fromCount# c) s of
      (# s', ba# #) -> (# s', MBytes ba# #)
{-# INLINE allocPinnedMBytes #-}

allocAlignedMBytes ::
     (MonadPrim s m, Prim a)
  => Count a -- ^ Size in number of bytes
  -> m (MBytes 'Pin s)
allocAlignedMBytes c =
  prim $ \s ->
    case alignmentProxy c of
      Count (I# a#) ->
        case newAlignedPinnedByteArray# (fromCount# c) a# s of
          (# s', ba# #) -> (# s', MBytes ba# #)
{-# INLINE allocAlignedMBytes #-}

callocMBytes :: (MonadPrim s m, Prim a, Typeable p) => Count a -> m (MBytes p s)
callocMBytes n = allocMBytes n >>= \mb -> mb <$ setMBytes mb 0 (toByteCount n) 0
{-# INLINE callocMBytes #-}

callocAlignedMBytes ::
     (MonadPrim s m, Prim a)
  => Count a -- ^ Size in number of bytes
  -> m (MBytes 'Pin s)
callocAlignedMBytes n = allocAlignedMBytes n >>= \mb -> mb <$ setMBytes mb 0 (toByteCount n) 0
{-# INLINE callocAlignedMBytes #-}


getByteCountMBytes :: MonadPrim s m => MBytes p s -> m (Count Word8)
getByteCountMBytes (MBytes mba#) =
  prim $ \s ->
    case getSizeofMutableByteArray# mba# s of
      (# s', n# #) -> (# s', Count (I# n#) #)
{-# INLINE getByteCountMBytes #-}


-- | Fill the mutable array with zeros efficiently.
zeroMBytes :: MonadPrim s m => MBytes p s -> m ()
zeroMBytes mba@(MBytes mba#) = do
  Count (I# n#) <- getByteCountMBytes mba
  prim_ (setByteArray# mba# 0# n# 0#)
{-# INLINE zeroMBytes #-}

freezeMBytes :: MonadPrim s m => MBytes p s -> m (Bytes p)
freezeMBytes (MBytes mba#) =
  prim $ \s ->
    case unsafeFreezeByteArray# mba# s of
      (# s', ba# #) -> (# s', Bytes ba# #)
{-# INLINE freezeMBytes #-}

thawBytes :: MonadPrim s m => Bytes p -> m (MBytes p s)
thawBytes (Bytes ba#) =
  prim $ \s ->
    case unsafeThawByteArray# ba# s of
      (# s', mba# #) -> (# s', MBytes mba# #)
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
     (MonadPrim s m, Prim a) => Bytes ps -> Off a -> MBytes pd s -> Off a -> Count a -> m ()
copyBytesToMBytes (Bytes src#) srcOff (MBytes dst#) dstOff c =
  prim_ $
  copyByteArray# src# (fromOff# srcOff) dst# (fromOff# dstOff) (fromCount# c)
{-# INLINE copyBytesToMBytes #-}

moveMBytesToMBytes ::
     (MonadPrim s m, Prim a) => MBytes ps s-> Off a -> MBytes pd s -> Off a -> Count a -> m ()
moveMBytesToMBytes (MBytes src#) srcOff (MBytes dst#) dstOff c =
  prim_ (copyMutableByteArray# src# (fromOff# srcOff) dst# (fromOff# dstOff) (fromCount# c))
{-# INLINE moveMBytesToMBytes #-}


-- moveMBytesToMBytes ::
--   (MonadPrim s m, Prim a) => MBytes ps s -> Off a -> MBytes pd s -> Off a -> Count a -> m ()
-- moveMBytesToMBytes (MBytes src#) srcOff (MBytes dst#) dstOff c =
--   unsafeIOToPrim $
--   memmoveMutableByteArray#
--     src#
--     (fromOff# srcOff)
--     dst#
--     (fromOff# dstOff)
--     (fromCount# c)
-- {-# INLINE moveMBytesToMBytes #-}


byteCountBytes :: Bytes p -> Count Word8
byteCountBytes (Bytes ba#) = coerce (I# (sizeofByteArray# ba#))
{-# INLINE byteCountBytes #-}

-- | How many elements of type @a@ fits into bytes completely. In order to get a possible
-- count of leftover bytes use `countRemBytes`
countBytes :: forall a p. Prim a => Bytes p -> Count a
countBytes = fromByteCount . byteCountBytes
{-# INLINE countBytes #-}

-- | Get the count of elements of type @a@ that can fit into bytes as well as the slack
-- number of bytes that would be leftover in case when total number of bytes available is
-- not exactly divisable by the size of the element that will be stored in the memory
-- chunk.
countRemBytes :: forall a p. Prim a => Bytes p -> (Count a, Int)
countRemBytes = fromByteCountRem . byteCountBytes
{-# INLINE countRemBytes #-}



-- | How many elements of type @a@ fits into bytes completely. In order to get any number
-- of leftover bytes use `countRemBytes`
getCountMBytes :: forall a p s m. (MonadPrim s m, Prim a) => MBytes p s -> m (Count a)
getCountMBytes b = fromByteCount <$> getByteCountMBytes b
{-# INLINE getCountMBytes #-}

-- | Get the number of elements of type @a@ that can fit into bytes as well as the slack
-- number of bytes that would be leftover in case when total number of bytes available is
-- not exactly divisable by the size of the element that will be stored in the memory
-- chunk.
getCountRemOfMBytes :: forall a p s m. (MonadPrim s m, Prim a) => MBytes p s -> m (Count a, Int)
getCountRemOfMBytes b = fromByteCountRem <$> getByteCountMBytes b
{-# INLINE getCountRemOfMBytes #-}

-- | It is only guaranteed to convert the whole memory to a list whenever the size of
-- allocated memory is exactly divisible by the size of the element, otherwise there will
-- be some slack left unaccounted for.
toListBytes :: Prim a => Bytes p -> [a]
toListBytes = toListMem
{-# INLINE toListBytes #-}

toListSlackBytes :: Prim a => Bytes p -> ([a], Maybe (Bytes 'Inc))
toListSlackBytes = toListSlackMem
{-# INLINE toListSlackBytes #-}

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
  c :: Count a <- getCountMBytes mb
  void $ loadListInternal c 0 ys mb
{-# INLINE loadListMBytes_ #-}

fromListBytesN_ :: (Prim a, Typeable p) => Count a -> [a] -> Bytes p
fromListBytesN_ = fromListMemN_
{-# INLINE fromListBytesN_ #-}

-- | If the list is bigger than the supplied @`Count` a@ then `GT` ordering will be
-- returned, along with the `Bytes` fully filled with the prefix of the list. On the other
-- hand if the list is smaller than the supplied `Count`, `LT` with partially filled
-- `Bytes` will returned. In the latter case expect some garbage at the end of the
-- allocated memory, since no attempt is made to zero it out. Exact match obviously
-- results in an `EQ`.
fromListBytesN ::
     (Prim a, Typeable p)
  => Count a
  -> [a]
  -> (Ordering, Bytes p)
fromListBytesN = fromListMemN
{-# INLINE fromListBytesN #-}

fromListBytes ::
     forall a p. (Prim a, Typeable p)
  => [a]
  -> Bytes p
fromListBytes xs = fromListBytesN_ (Count (length xs)) xs
{-# INLINE fromListBytes #-}

concatBytes :: Typeable p => [Bytes p'] -> Bytes p
concatBytes = concatMem
{-# INLINE concatBytes #-}

readOffMBytes :: (MonadPrim s m, Prim a) => MBytes p s -> Off a -> m a
readOffMBytes (MBytes mba#) (Off (I# i#)) = prim (readMutableByteArray# mba# i#)
{-# INLINE readOffMBytes #-}

readByteOffMBytes :: (MonadPrim s m, Prim a) => MBytes p s -> Off Word8 -> m a
readByteOffMBytes (MBytes mba#) (Off (I# i#)) = prim (readByteOffMutableByteArray# mba# i#)
{-# INLINE readByteOffMBytes #-}

writeOffMBytes :: (MonadPrim s m, Prim a) => MBytes p s -> Off a -> a -> m ()
writeOffMBytes (MBytes mba#) (Off (I# i#)) a = prim_ (writeMutableByteArray# mba# i# a)
{-# INLINE writeOffMBytes #-}

writeByteOffMBytes :: (MonadPrim s m, Prim a) => MBytes p s -> Off Word8 -> a -> m ()
writeByteOffMBytes (MBytes mba#) (Off (I# i#)) a = prim_ (writeByteOffMutableByteArray# mba# i# a)
{-# INLINE writeByteOffMBytes #-}

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
        let n8 = byteCountBytes b
        pmb <- allocPinnedMBytes n8
        copyBytesToMBytes b 0 pmb 0 n8
        freezeMBytes pmb
{-# INLINE ensurePinnedBytes #-}

ensurePinnedMBytes :: MonadPrim s m => MBytes p s -> m (MBytes 'Pin s)
ensurePinnedMBytes mb =
  case toPinnedMBytes mb of
    Just pmb -> pure pmb
    Nothing  -> do
      n8 :: Count Word8 <- getCountMBytes mb
      pmb <- allocPinnedMBytes n8
      pmb <$ moveMBytesToMBytes mb 0 pmb 0 n8
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


toPtrBytes :: Bytes 'Pin -> Ptr a
toPtrBytes (Bytes ba#) = Ptr (byteArrayContents# ba#)
{-# INLINE toPtrBytes #-}

toPtrMBytes :: MBytes 'Pin s -> Ptr a
toPtrMBytes (MBytes mba#) = Ptr (byteArrayContents# (unsafeCoerce# mba#))
{-# INLINE toPtrMBytes #-}

-- | Pointer access to immutable `Bytes` should be for read only purposes, but it is
-- not enforced. Any mutation will break referential transparency
withPtrBytes :: MonadPrim s m => Bytes 'Pin -> (Ptr a -> m b) -> m b
withPtrBytes b f = do
  res <- f (toPtrBytes b)
  res <$ touch b
{-# INLINE withPtrBytes #-}

-- | Same as `withPtrBytes`, but is suitable for actions that don't terminate
withNoHaltPtrBytes :: MonadUnliftPrim s m => Bytes 'Pin -> (Ptr a -> m b) -> m b
withNoHaltPtrBytes b f = withUnliftPrim b $ f (toPtrBytes b)
{-# INLINE withNoHaltPtrBytes #-}

withPtrMBytes :: MonadPrim s m => MBytes 'Pin s -> (Ptr a -> m b) -> m b
withPtrMBytes mb f = do
  res <- f (toPtrMBytes mb)
  res <$ touch mb
{-# INLINE withPtrMBytes #-}

withNoHaltPtrMBytes :: (MonadUnliftPrim s m) => MBytes 'Pin s -> (Ptr a -> m b) -> m b
withNoHaltPtrMBytes mb f = withUnliftPrim mb $ f (toPtrMBytes mb)
{-# INLINE withNoHaltPtrMBytes #-}

toForeignPtrBytes :: Bytes 'Pin -> ForeignPtr a
toForeignPtrBytes (Bytes ba#) =
  ForeignPtr (byteArrayContents# ba#) (PlainPtr (unsafeCoerce# ba#))
{-# INLINE toForeignPtrBytes #-}


toForeignPtrMBytes :: MBytes 'Pin s -> ForeignPtr a
toForeignPtrMBytes (MBytes mba#) =
  ForeignPtr (byteArrayContents# (unsafeCoerce# mba#)) (PlainPtr (unsafeCoerce# mba#))
{-# INLINE toForeignPtrMBytes #-}



-- | Perform atomic modification of an element in the `MBytes` at the supplied
-- index. Returns the artifact of computation @__b__@.  Offset is in number of elements,
-- rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
casMBytes ::
     (MonadPrim s m, Atomic a)
  => MBytes p s -- ^ Array to be mutated
  -> Off a -- ^ Index is in elements of @__a__@, rather than bytes.
  -> a -- ^ Expected old value
  -> a -- ^ New value
  -> m a
casMBytes (MBytes mba#) (Off (I# i#)) old new = prim $ casMutableByteArray# mba# i# old new
{-# INLINE casMBytes #-}

-- | Perform atomic modification of an element in the `MBytes` at the supplied
-- index. Returns the artifact of computation @__b__@.  Offset is in number of elements,
-- rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicModifyMBytes ::
     (MonadPrim s m, Atomic a)
  => MBytes p s -- ^ Array to be mutated
  -> Off a -- ^ Index is in elements of @__a__@, rather than bytes.
  -> (a -> (a, b)) -- ^ Function that is applied to the old value and returns new value
                   -- and some artifact of computation @__b__@
  -> m b
atomicModifyMBytes (MBytes mba#) (Off (I# i#)) f =
  prim $
  atomicModifyMutableByteArray# mba# i# $ \a ->
    case f a of
      (a', b) -> (# a', b #)
{-# INLINE atomicModifyMBytes #-}

-- | Perform atomic modification of an element in the `MBytes` at the supplied
-- index.  Offset is in number of elements, rather than bytes. Implies a full memory
-- barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicModifyMBytes_ ::
     (MonadPrim s m, Atomic a)
  => MBytes p s -- ^ Array to be mutated
  -> Off a -- ^ Index is in elements of @__a__@, rather than bytes.
  -> (a -> a) -- ^ Function that is applied to the old value and returns new value.
  -> m ()
atomicModifyMBytes_ (MBytes mba#) (Off (I# i#)) f =
  prim_ $ atomicModifyMutableByteArray_# mba# i# f
{-# INLINE atomicModifyMBytes_ #-}


-- | Perform atomic modification of an element in the `MBytes` at the supplied
-- index. Returns the previous value.  Offset is in number of elements, rather than
-- bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicFetchModifyMBytes ::
     (MonadPrim s m, Atomic a)
  => MBytes p s -- ^ Array to be mutated
  -> Off a -- ^ Index is in elements of @__a__@, rather than bytes.
  -> (a -> a) -- ^ Function that is applied to the old value and returns the new value
  -> m a
atomicFetchModifyMBytes (MBytes mba#) (Off (I# i#)) f =
  prim $ atomicFetchModifyMutableByteArray# mba# i# f
{-# INLINE atomicFetchModifyMBytes #-}


-- | Perform atomic modification of an element in the `MBytes` at the supplied
-- index.  Offset is in number of elements, rather than bytes. Implies a full memory
-- barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicModifyFetchMBytes ::
     (MonadPrim s m, Atomic a)
  => MBytes p s -- ^ Array to be mutated
  -> Off a -- ^ Index is in elements of @__a__@, rather than bytes.
  -> (a -> a) -- ^ Function that is applied to the old value and returns the new value
  -> m a
atomicModifyFetchMBytes (MBytes mba#) (Off (I# i#)) f =
  prim $ atomicModifyFetchMutableByteArray# mba# i# f
{-# INLINE atomicModifyFetchMBytes #-}






-- | Add a numeric value to an element of a `MBytes`, corresponds to @(`+`)@ done
-- atomically. Returns the previous value.  Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicFetchAddMBytes ::
     (MonadPrim s m, AtomicCount a)
  => MBytes p s
  -> Off a
  -> a
  -> m a
atomicFetchAddMBytes (MBytes mba#) (Off (I# i#)) a =
  prim (atomicFetchAddMutableByteArray# mba# i# a)
{-# INLINE atomicFetchAddMBytes #-}

-- | Add a numeric value to an element of a `MBytes`, corresponds to @(`+`)@ done
-- atomically. Returns the new value.  Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicAddFetchMBytes ::
     (MonadPrim s m, AtomicCount a)
  => MBytes p s
  -> Off a
  -> a
  -> m a
atomicAddFetchMBytes (MBytes mba#) (Off (I# i#)) a =
  prim (atomicAddFetchMutableByteArray# mba# i# a)
{-# INLINE atomicAddFetchMBytes #-}



-- | Subtract a numeric value from an element of a `MBytes`, corresponds to
-- @(`-`)@ done atomically. Returns the previous value.  Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicFetchSubMBytes ::
     (MonadPrim s m, AtomicCount a)
  => MBytes p s
  -> Off a
  -> a
  -> m a
atomicFetchSubMBytes (MBytes mba#) (Off (I# i#)) a =
  prim (atomicFetchSubMutableByteArray# mba# i# a)
{-# INLINE atomicFetchSubMBytes #-}

-- | Subtract a numeric value from an element of a `MBytes`, corresponds to
-- @(`-`)@ done atomically. Returns the new value. Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicSubFetchMBytes ::
     (MonadPrim s m, AtomicCount a)
  => MBytes p s
  -> Off a
  -> a
  -> m a
atomicSubFetchMBytes (MBytes mba#) (Off (I# i#)) a =
  prim (atomicSubFetchMutableByteArray# mba# i# a)
{-# INLINE atomicSubFetchMBytes #-}



-- | Binary conjunction (AND) of an element of a `MBytes` with the supplied value,
-- corresponds to @(`Data.Bits..&.`)@ done atomically. Returns the previous value. Offset
-- is in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicFetchAndMBytes ::
     (MonadPrim s m, AtomicBits a)
  => MBytes p s
  -> Off a
  -> a
  -> m a
atomicFetchAndMBytes (MBytes mba#) (Off (I# i#)) a =
  prim (atomicFetchAndMutableByteArray# mba# i# a)
{-# INLINE atomicFetchAndMBytes #-}

-- | Binary conjunction (AND) of an element of a `MBytes` with the supplied value,
-- corresponds to @(`Data.Bits..&.`)@ done atomically. Returns the new value. Offset is
-- in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicAndFetchMBytes ::
     (MonadPrim s m, AtomicBits a)
  => MBytes p s
  -> Off a
  -> a
  -> m a
atomicAndFetchMBytes (MBytes mba#) (Off (I# i#)) a =
  prim (atomicAndFetchMutableByteArray# mba# i# a)
{-# INLINE atomicAndFetchMBytes #-}



-- | Negation of binary conjunction (NAND) of an element of a `MBytes` with the
-- supplied value, corresponds to @\\x y -> `Data.Bits.complement` (x `Data.Bits..&.` y)@
-- done atomically. Returns the previous value. Offset is in number of elements, rather
-- than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicFetchNandMBytes ::
     (MonadPrim s m, AtomicBits a)
  => MBytes p s
  -> Off a
  -> a
  -> m a
atomicFetchNandMBytes (MBytes mba#) (Off (I# i#)) a =
  prim (atomicFetchNandMutableByteArray# mba# i# a)
{-# INLINE atomicFetchNandMBytes #-}

-- | Negation of binary conjunction (NAND)  of an element of a `MBytes` with the supplied
-- value, corresponds to @\\x y -> `Data.Bits.complement` (x `Data.Bits..&.` y)@ done
-- atomically. Returns the new value. Offset is in number of elements, rather than
-- bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicNandFetchMBytes ::
     (MonadPrim s m, AtomicBits a)
  => MBytes p s
  -> Off a
  -> a
  -> m a
atomicNandFetchMBytes (MBytes mba#) (Off (I# i#)) a =
  prim (atomicNandFetchMutableByteArray# mba# i# a)
{-# INLINE atomicNandFetchMBytes #-}




-- | Binary disjunction (OR) of an element of a `MBytes` with the supplied value,
-- corresponds to @(`Data.Bits..|.`)@ done atomically. Returns the previous value. Offset
-- is in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicFetchOrMBytes ::
     (MonadPrim s m, AtomicBits a)
  => MBytes p s
  -> Off a
  -> a
  -> m a
atomicFetchOrMBytes (MBytes mba#) (Off (I# i#)) a =
  prim (atomicFetchOrMutableByteArray# mba# i# a)
{-# INLINE atomicFetchOrMBytes #-}

-- | Binary disjunction (OR) of an element of a `MBytes` with the supplied value,
-- corresponds to @(`Data.Bits..|.`)@ done atomically. Returns the new value. Offset is
-- in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicOrFetchMBytes ::
     (MonadPrim s m, AtomicBits a)
  => MBytes p s
  -> Off a
  -> a
  -> m a
atomicOrFetchMBytes (MBytes mba#) (Off (I# i#)) a =
  prim (atomicOrFetchMutableByteArray# mba# i# a)
{-# INLINE atomicOrFetchMBytes #-}



-- | Binary exclusive disjunction (XOR) of an element of a `MBytes` with the supplied value,
-- corresponds to @`Data.Bits.xor`@ done atomically. Returns the previous value. Offset
-- is in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicFetchXorMBytes ::
     (MonadPrim s m, AtomicBits a)
  => MBytes p s
  -> Off a
  -> a
  -> m a
atomicFetchXorMBytes (MBytes mba#) (Off (I# i#)) a =
  prim (atomicFetchXorMutableByteArray# mba# i# a)
{-# INLINE atomicFetchXorMBytes #-}

-- | Binary exclusive disjunction (XOR) of an element of a `MBytes` with the supplied value,
-- corresponds to @`Data.Bits.xor`@ done atomically. Returns the new value. Offset is
-- in number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicXorFetchMBytes ::
     (MonadPrim s m, AtomicBits a)
  => MBytes p s
  -> Off a
  -> a
  -> m a
atomicXorFetchMBytes (MBytes mba#) (Off (I# i#)) a =
  prim (atomicXorFetchMutableByteArray# mba# i# a)
{-# INLINE atomicXorFetchMBytes #-}





-- | Binary negation (NOT) of an element of a `MBytes`, corresponds to
-- @(`Data.Bits.complement`)@ done atomically. Returns the previous value. Offset is in
-- number of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicFetchNotMBytes ::
     (MonadPrim s m, AtomicBits a)
  => MBytes p s
  -> Off a
  -> m a
atomicFetchNotMBytes (MBytes mba#) (Off (I# i#)) =
  prim (atomicFetchNotMutableByteArray# mba# i#)
{-# INLINE atomicFetchNotMBytes #-}

-- | Binary negation (NOT) of an element of a `MBytes`, corresponds to
-- @(`Data.Bits.complement`)@ done atomically. Returns the new value. Offset is in number
-- of elements, rather than bytes. Implies a full memory barrier.
--
-- /Note/ - Bounds are not checked, therefore this function is unsafe.
--
-- @since 0.1.0
atomicNotFetchMBytes ::
     (MonadPrim s m, AtomicBits a)
  => MBytes p s
  -> Off a
  -> m a
atomicNotFetchMBytes (MBytes mba#) (Off (I# i#)) =
  prim (atomicNotFetchMutableByteArray# mba# i#)
{-# INLINE atomicNotFetchMBytes #-}




prefetchBytes0 :: (MonadPrim s m, Prim a) => Bytes p -> Off a -> m ()
prefetchBytes0 (Bytes b#) off = prim_ (prefetchByteArray0# b# (fromOff# off))
{-# INLINE prefetchBytes0 #-}

prefetchMBytes0 :: (MonadPrim s m, Prim a) => MBytes p s -> Off a -> m ()
prefetchMBytes0 (MBytes mb#) off = prim_ (prefetchMutableByteArray0# mb# (fromOff# off))
{-# INLINE prefetchMBytes0 #-}

prefetchBytes1 :: (MonadPrim s m, Prim a) => Bytes p -> Off a -> m ()
prefetchBytes1 (Bytes b#) off = prim_ (prefetchByteArray1# b# (fromOff# off))
{-# INLINE prefetchBytes1 #-}

prefetchMBytes1 :: (MonadPrim s m, Prim a) => MBytes p s -> Off a -> m ()
prefetchMBytes1 (MBytes mb#) off = prim_ (prefetchMutableByteArray1# mb# (fromOff# off))
{-# INLINE prefetchMBytes1 #-}

prefetchBytes2 :: (MonadPrim s m, Prim a) => Bytes p -> Off a -> m ()
prefetchBytes2 (Bytes b#) off = prim_ (prefetchByteArray2# b# (fromOff# off))
{-# INLINE prefetchBytes2 #-}

prefetchMBytes2 :: (MonadPrim s m, Prim a) => MBytes p s -> Off a -> m ()
prefetchMBytes2 (MBytes mb#) off = prim_ (prefetchMutableByteArray2# mb# (fromOff# off))
{-# INLINE prefetchMBytes2 #-}

prefetchBytes3 :: (MonadPrim s m, Prim a) => Bytes p -> Off a -> m ()
prefetchBytes3 (Bytes b#) off = prim_ (prefetchByteArray3# b# (fromOff# off))
{-# INLINE prefetchBytes3 #-}

prefetchMBytes3 :: (MonadPrim s m, Prim a) => MBytes p s -> Off a -> m ()
prefetchMBytes3 (MBytes mb#) off = prim_ (prefetchMutableByteArray3# mb# (fromOff# off))
{-# INLINE prefetchMBytes3 #-}

