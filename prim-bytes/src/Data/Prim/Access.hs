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
-- Module      : Data.Prim.Access
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Access where


import Data.ByteString.Internal hiding (toForeignPtr)
import Control.Prim.Monad
import Control.Prim.Monad.Unsafe
import Data.Prim
import Data.Prim.Bytes
import Data.Prim.Bytes.Addr
import Data.Prim.Ptr
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
     (MonadPrimBase s n, MonadPrim s m, PtrAccess m p) => p -> (Ptr a -> n b) -> m b
withNoHaltPtrAccess p f = do
  ForeignPtr addr# ptrContents <- toForeignPtr p
  withPrimBase ptrContents $ f (Ptr addr#)

withForeignPtrPrim :: MonadPrim s m => ForeignPtr a1 -> (Ptr a2 -> m b) -> m b
withForeignPtrPrim (ForeignPtr addr# ptrContents) f = do
  r <- f (Ptr addr#)
  r <$ touch ptrContents

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

indexPrim :: (ReadAccess (Mem r), Prim a) => Mem r s -> Off a -> a
indexPrim mem off = unsafeInlineST (readPrim mem off)

class ReadAccess r where
  readPrim :: (MonadPrim s m, Prim a) => r s -> Off a -> m a

  -- | Source and target can't be the same memory chunks
  copyToMBytes :: (MonadPrim s m, Prim a) => r s -> Off a -> MBytes p s -> Off a -> Count a -> m ()

  -- | Source and target can't be the same memory chunks
  copyToPtr :: (MonadPrim s m, Prim a) => r s -> Off a -> Ptr a -> Off a -> Count a -> m ()

class ReadAccess r => WriteAccess r where
  writePrim :: (MonadPrim s m, Prim a) => r s -> Off a -> a -> m ()

  -- | Source and target can be overlapping memory chunks
  moveToMBytes :: (MonadPrim s m, Prim a) => r s -> Off a -> MBytes p s -> Off a -> Count a -> m ()

  -- | Source and target can be overlapping memory chunks
  moveToPtr :: (MonadPrim s m, Prim a) => r s -> Off a -> Ptr a -> Off a -> Count a -> m ()

  copy :: (MonadPrim s m, Prim a) => r s -> Off a -> r s -> Off a -> Count a -> m ()
  -- default copy :: (PtrAccess m r, Prim a) => r s -> Off a -> r -> Off a -> Count a -> m ()
  -- copy src srcOff dst dstOff n =
  --   withPtrAccess dst $ \dstPtr -> copyToPtr src srcOff dstPtr dstOff n

  move :: (MonadPrim s m, Prim a) => r s -> Off a -> r s -> Off a -> Count a -> m ()
  -- default move :: (PtrAccess m r, Prim a) => r -> Off a -> r -> Off a -> Count a -> m ()
  -- move src srcOff dst dstOff n =
  --   withPtrAccess dst $ \dstPtr -> copyToPtr src srcOff dstPtr dstOff n

  set :: (MonadPrim s m, Prim a) => r s -> Off a -> Count a -> a -> m ()

newtype Mem a s = Mem { unMem :: a }

instance ReadAccess (Mem ByteString) where
  readPrim (Mem bs) i = withPtrAccess bs (`readOffPtr` i)
  copyToMBytes (Mem bs) srcOff mb dstOff c =
    withPtrAccess bs $ \(Ptr p#) -> copyPtrToMBytes (Ptr p#) srcOff mb dstOff c
  copyToPtr (Mem bs) srcOff dstPtr dstOff c =
    withPtrAccess bs $ \(Ptr p#) -> copyPtrToPtr (Ptr p#) srcOff dstPtr dstOff c

instance ReadAccess (Mem (ForeignPtr a)) where
  readPrim (Mem fptr) i = withForeignPtrPrim fptr (`readOffPtr` i)
  copyToMBytes (Mem fptr) srcOff mb dstOff c =
    withForeignPtrPrim fptr $ \(Ptr p#) -> copyPtrToMBytes (Ptr p#) srcOff mb dstOff c
  copyToPtr (Mem fptr) srcOff dstPtr dstOff c =
    withForeignPtrPrim fptr $ \(Ptr p#) -> copyPtrToPtr (Ptr p#) srcOff dstPtr dstOff c

instance WriteAccess (Mem (ForeignPtr a)) where
  writePrim (Mem fptr) i a = withForeignPtrPrim fptr (\ptr -> writeOffPtr ptr i a)
  moveToPtr (Mem fsrc) srcOff dstPtr dstOff c =
    withForeignPtrPrim fsrc $ \srcPtr -> copyPtrToPtr srcPtr srcOff dstPtr dstOff c
  moveToMBytes (Mem fsrc) srcOff dst dstOff c =
    withForeignPtrPrim fsrc $ \srcPtr -> copyPtrToMBytes srcPtr srcOff dst dstOff c
  copy (Mem fsrc) srcOff (Mem fdst) dstOff c =
    withForeignPtrPrim fsrc $ \srcPtr ->
      withForeignPtrPrim fdst $ \dstPtr ->
         copyPtrToPtr srcPtr srcOff dstPtr dstOff c
  move (Mem fsrc) srcOff (Mem fdst) dstOff c =
    withForeignPtrPrim fsrc $ \srcPtr ->
      withForeignPtrPrim fdst $ \dstPtr ->
         movePtrToPtr srcPtr srcOff dstPtr dstOff c
  set (Mem fptr) off c a = withForeignPtrPrim fptr $ \ptr -> setOffPtr ptr off c a


instance ReadAccess (Mem (Bytes p)) where
  readPrim b = pure . indexBytes (coerce b)
  copyToMBytes b = copyBytesToMBytes (coerce b)
  copyToPtr b = copyBytesToPtr (coerce b)


instance ReadAccess (MBytes p) where
  readPrim = readMBytes
  copyToMBytes = copyMBytesToMBytes
  copyToPtr = copyMBytesToPtr

instance WriteAccess (MBytes p) where
  writePrim = writeMBytes
  moveToPtr = moveMBytesToPtr
  moveToMBytes = moveMBytesToMBytes
  copy = copyMBytesToMBytes
  move = moveMBytesToMBytes
  set = setMBytes

class WriteAccess r => Alloc r where
  type Frozen r :: *
  sizeOfAlloc :: MonadPrim s m => r s -> m (Count Word8)

  malloc :: MonadPrim s m => Count Word8 -> m (r s)

  thaw :: MonadPrim s m => Frozen r -> m (r s)

  freeze :: MonadPrim s m => r s -> m (Frozen r)

instance ReadAccess (MAddr a) where
  readPrim ma i = withPtrMAddr ma ((`readOffPtr` i) . castPtr)
  copyToMBytes ma si mb di c =
    withPtrMAddr ma $ \ptr -> copyPtrToMBytes (castPtr ptr) si mb di c
  copyToPtr ma si mb di c =
    withPtrMAddr ma $ \ptr -> copyPtrToPtr (castPtr ptr) si mb di c

-- instance Alloc (MAddr a) where
--   type Frozen (MAddr a) = Addr a



alloc :: (Alloc r, MonadPrim s m, Prim p) => Count p -> m (r s)
alloc = malloc . countWord8

calloc ::
     (Alloc r, MonadPrim s m, Prim a) => Count a -> m (r s)
calloc n = do
  m <- alloc n
  m <$ set m 0 (countWord8 n) (0 :: Word8)


newST :: (Alloc r, Prim a) => Count a -> (forall s . r s -> ST s b) -> (b, Frozen r)
newST n f = runST $ do
  m <- calloc n
  res <- f m
  i <- freeze m
  pure (res, i)

newST_ :: (Alloc r, Prim a) => Count a -> (forall s . r s -> ST s b) -> Frozen r
newST_ n f = runST (calloc n >>= \m -> f m >> freeze m)


-- instance Typeable p => Alloc (Mem (Bytes p)) where
--   type Frozen (Mem (Bytes p)) = Bytes p
--   sizeOfAlloc = pure . countOfBytes . coerce
--   malloc = fmap Mem . freezeMBytes <=< malloc
--   thaw = pure . Mem
--   freeze = pure . unMem

instance Typeable p => Alloc (MBytes p) where
  type Frozen (MBytes p) = Bytes p
  sizeOfAlloc = getCountOfMBytes
  malloc = allocMBytes
  thaw = thawBytes
  freeze = freezeMBytes

-- -- instance MonadPrim s m => Alloc m (MBytes 'Pin s) where
-- --   malloc = allocPinnedMBytes

-- instance Alloc IO (ForeignPtr a) where
--   malloc = mallocForeignPtrBytes . coerce

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



-- memcopy :: (ReadAccess s m r, PtrAccess m p, Prim a) => r -> Off a -> p -> Off a -> Count a -> m ()
-- memcopy src srcOff dst dstOff c =
--   withPtrAccess dst $ \dstPtr -> copyToPtr src srcOff dstPtr dstOff c

-- memmove :: (WriteAccess s m r, PtrAccess m p, Prim a) => r -> Off a -> p -> Off a -> Count a -> m ()
-- memmove src srcOff dst dstOff c =
--   withPtrAccess dst $ \dstPtr -> moveToPtr src srcOff dstPtr dstOff c


-- cloneBytes :: (MonadPrim s m, Alloc m (MBytes p s)) => Bytes p' -> m (Bytes p)
-- cloneBytes b = withCopyMBytes_ b pure
-- {-# INLINE cloneBytes #-}

clone :: (Alloc r, WriteAccess r, MonadPrim s m) => r s -> m (r s)
clone mb = do
  n <- sizeOfAlloc mb
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



class NoConstraint a
instance NoConstraint a

class Mut f where
  type Elt f :: * -> Constraint
  type Elt f = NoConstraint
  newMut :: (Elt f a, MonadPrim s m) => m (f a s)

  readMut :: (Elt f a, MonadPrim s m) => f a s -> m a

  writeMut :: (Elt f a, MonadPrim s m) => f a s -> a -> m ()

  makeMut :: (Elt f a, MonadPrim s m) => a -> m (f a s)
  makeMut a = do
    mut <- newMut
    mut <$ writeMut mut a

class Mut f => MutArray f where
  type Array f :: * -> Type

  getCountMutArray :: Elt f a => f a s -> m (Count a)

  thawArray :: (Elt f a, MonadPrim s m) => Array f a -> m (f a s)

  freezeMutArray :: (Elt f a, MonadPrim s m) => f a s -> m (Array f a)

  newMutArray :: (Elt f a, MonadPrim s m) => Count a -> m (f a s)

  readMutArray :: (Elt f a, MonadPrim s m) => f a s -> Off a -> m a

  writeMutArray :: (Elt f a, MonadPrim s m) => f a s -> Off a -> a -> m ()



makeMutArray :: (MutArray f, Elt f a, MonadPrim s m) => Count a -> (Off a -> m a) -> m (f a s)
makeMutArray c f = do
  ma <- newMutArray c
  ma <$ forM_ [0 .. Off (unCount c) - 1] (\i -> f i >>= writeMutArray ma i)


instance Mut MAddr where
  type Elt MAddr = Prim
  newMut = allocMAddr (Count 1)
  readMut = readMAddr
  writeMut = writeMAddr

instance MutArray MAddr where
  type Array MAddr = Addr
  newMutArray = allocMAddr
  thawArray = thawAddr
  freezeMutArray = freezeMAddr
  readMutArray = readOffMAddr
  writeMutArray = writeOffMAddr

class Mut f => MutFunctor f where
  mapMut :: (Elt f a, Elt f b, MonadPrim s m) => (a -> m b) -> f a s -> m (f b s)

instance MutFunctor MAddr where
  mapMut f maddr = do
    Count n <- getCountOfMAddr maddr
    maddr' <- allocMAddr (Count n)
    forM_ [0 .. n - 1] $ \i ->
      readOffMAddr maddr (Off i) >>= f >>= writeOffMAddr maddr' (Off i)
    pure maddr'
