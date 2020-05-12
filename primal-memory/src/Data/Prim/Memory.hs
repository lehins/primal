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
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RoleAnnotations #-}
-- |
-- Module      : Data.Prim.Memory
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Memory where


import Control.Prim.Monad
import Control.Prim.Monad.Unsafe
import Data.Prim
import {-# SOURCE #-} Data.Prim.Memory.Bytes
  ( Bytes(..)
  , MBytes(..)
  , Pinned(..)
  , allocMBytes
  , allocUnpinnedMBytes
  , freezeMBytes
  , thawBytes
  , indexOffBytes
  , indexByteOffBytes
  , byteCountBytes
  , getByteCountMBytes
  , setMBytes
  , moveMBytesToMBytes
  , copyBytesToMBytes
  , readOffMBytes
  , readByteOffMBytes
  , writeOffMBytes
  , writeByteOffMBytes
  , toPtrBytes
  , withPtrMBytes
  )
import Data.Prim.Memory.ByteString
import Data.Prim.Memory.Addr
import Data.Prim.Memory.Ptr
import Data.Prim.Memory.ForeignPtr
import Foreign.Ptr
import GHC.Exts hiding
  ( getSizeofMutableByteArray#
  , isByteArrayPinned#
  , isMutableByteArrayPinned#
  )
import Control.Monad.ST
import Data.Typeable
import Data.Foldable as Foldable
import Data.List as List
import Data.Kind

class MemRead r where
  byteCountMem :: r -> Count Word8

  indexOffMem :: Prim a => r -> Off a -> a

  indexByteOffMem :: Prim a => r -> Off Word8 -> a

  -- | Source and target can't refer to the same memory chunks
  copyToMBytesMem :: (MonadPrim s m, Prim a) => r -> Off a -> MBytes p s -> Off a -> Count a -> m ()

  -- | Source and target can't refer to the same memory chunks
  copyToPtrMem :: (MonadPrim s m, Prim a) => r -> Off a -> Ptr a -> Off a -> Count a -> m ()


class MemWrite r where
  readOffMem :: (MonadPrim s m, Prim a) => r s -> Off a -> m a

  readByteOffMem :: (MonadPrim s m, Prim a) => r s -> Off Word8 -> m a

  writeOffMem :: (MonadPrim s m, Prim a) => r s -> Off a -> a -> m ()

  writeByteOffMem :: (MonadPrim s m, Prim a) => r s -> Off Word8 -> a -> m ()

  -- | Source and target can be overlapping memory chunks
  moveToMBytesMem :: (MonadPrim s m, Prim a) => r s -> Off a -> MBytes p s -> Off a -> Count a -> m ()

  -- | Source and target can be overlapping memory chunks
  moveToPtrMem :: (MonadPrim s m, Prim a) => r s -> Off a -> Ptr a -> Off a -> Count a -> m ()

  copyMem :: (MonadPrim s m, MemRead r', Prim a) => r' -> Off a -> r s -> Off a -> Count a -> m ()

  moveMem :: (MonadPrim s m, MemWrite r', Prim a) => r' s -> Off a -> r s -> Off a -> Count a -> m ()

  setMem :: (MonadPrim s m, Prim a) => r s -> Off a -> Count a -> a -> m ()


instance MemRead ByteString where
  byteCountMem (PS _ _ c) = Count c
  {-# INLINE byteCountMem #-}
  indexOffMem bs i = unsafeInlineIO $ withPtrAccess bs (`readOffPtr` i)
  {-# INLINE indexOffMem #-}
  indexByteOffMem bs i = unsafeInlineIO $ withPtrAccess bs (`readByteOffPtr` i)
  {-# INLINE indexByteOffMem #-}
  copyToMBytesMem bs srcOff mb dstOff c =
    withPtrAccess bs $ \(Ptr p#) -> copyPtrToMBytes (Ptr p#) srcOff mb dstOff c
  {-# INLINE copyToMBytesMem #-}
  copyToPtrMem bs srcOff dstPtr dstOff c =
    withPtrAccess bs $ \(Ptr p#) -> copyPtrToPtr (Ptr p#) srcOff dstPtr dstOff c
  {-# INLINE copyToPtrMem #-}



instance MemRead ShortByteString where
  byteCountMem = byteCountMem . fromShortByteStringBytes
  {-# INLINE byteCountMem #-}
  indexOffMem sbs = indexOffMem (fromShortByteStringBytes sbs)
  {-# INLINE indexOffMem #-}
  indexByteOffMem sbs = indexByteOffMem (fromShortByteStringBytes sbs)
  {-# INLINE indexByteOffMem #-}
  copyToMBytesMem sbs = copyToMBytesMem (fromShortByteStringBytes sbs)
  {-# INLINE copyToMBytesMem #-}
  copyToPtrMem sbs = copyToPtrMem (fromShortByteStringBytes sbs)
  {-# INLINE copyToPtrMem #-}

-- | A wrapper that adds a phantom state token to type that either doesn't have one or it
-- designed to work in IO. For that reason using this wrapper doesn't make it safe to use
-- in `ST` for example, but it is sometimes desired, so `MemState` makes it possible.
newtype MemState a s = MemState { unMemState :: a }

instance MemWrite (MemState (ForeignPtr a)) where
  readOffMem (MemState fptr) i = withForeignPtr fptr $ \ptr -> readOffPtr (castPtr ptr) i
  {-# INLINE readOffMem #-}
  readByteOffMem (MemState fptr) i =
    withForeignPtr fptr $ \ptr -> readByteOffPtr (castPtr ptr) i
  {-# INLINE readByteOffMem #-}
  writeOffMem (MemState fptr) i a = withForeignPtr fptr $ \ptr -> writeOffPtr (castPtr ptr) i a
  {-# INLINE writeOffMem #-}
  writeByteOffMem (MemState fptr) i a =
    withForeignPtr fptr $ \ptr -> writeByteOffPtr (castPtr ptr) i a
  {-# INLINE writeByteOffMem #-}
  moveToPtrMem (MemState fsrc) srcOff dstPtr dstOff c =
    withForeignPtr fsrc $ \srcPtr -> movePtrToPtr (castPtr srcPtr) srcOff dstPtr dstOff c
  {-# INLINE moveToPtrMem #-}
  moveToMBytesMem (MemState fsrc) srcOff dst dstOff c =
    withForeignPtr fsrc $ \srcPtr -> movePtrToMBytes (castPtr srcPtr) srcOff dst dstOff c
  {-# INLINE moveToMBytesMem #-}
  copyMem src srcOff (MemState fdst) dstOff c =
    withForeignPtr fdst $ \dstPtr ->
       copyToPtrMem src srcOff (castPtr dstPtr) dstOff c
  {-# INLINE copyMem #-}
  moveMem src srcOff (MemState fdst) dstOff c =
    withForeignPtr fdst $ \dstPtr ->
       moveToPtrMem src srcOff (castPtr dstPtr) dstOff c
  {-# INLINE moveMem #-}
  setMem (MemState fptr) off c a = withForeignPtr fptr $ \ptr -> setOffPtr (castPtr ptr) off c a
  {-# INLINE setMem #-}


instance MemRead (Bytes p) where
  byteCountMem = byteCountBytes
  {-# INLINE byteCountMem #-}
  indexOffMem = indexOffBytes
  {-# INLINE indexOffMem #-}
  indexByteOffMem = indexByteOffBytes
  {-# INLINE indexByteOffMem #-}
  copyToMBytesMem = copyBytesToMBytes
  {-# INLINE copyToMBytesMem #-}
  copyToPtrMem = copyBytesToPtr
  {-# INLINE copyToPtrMem #-}


instance MemWrite (MBytes p) where
  readOffMem = readOffMBytes
  {-# INLINE readOffMem #-}
  readByteOffMem = readByteOffMBytes
  {-# INLINE readByteOffMem #-}
  writeOffMem = writeOffMBytes
  {-# INLINE writeOffMem #-}
  writeByteOffMem = writeByteOffMBytes
  {-# INLINE writeByteOffMem #-}
  moveToPtrMem = moveMBytesToPtr
  {-# INLINE moveToPtrMem #-}
  moveToMBytesMem = moveMBytesToMBytes
  {-# INLINE moveToMBytesMem #-}
  moveMem = moveToMBytesMem
  {-# INLINE moveMem #-}
  setMem = setMBytes
  {-# INLINE setMem #-}
  copyMem = copyToMBytesMem
  {-# INLINE copyMem #-}



instance MemAlloc (MAddr a) where
  type FrozenMem (MAddr a) = Addr a

  getByteCountMem = getByteCountMAddr
  {-# INLINE getByteCountMem #-}
  allocRawMem = fmap castMAddr . allocMAddr
  {-# INLINE allocRawMem #-}
  thawMem = thawAddr
  {-# INLINE thawMem #-}
  freezeMem = freezeMAddr
  {-# INLINE freezeMem #-}


instance MemRead (Addr a) where
  byteCountMem = byteCountAddr
  {-# INLINE byteCountMem #-}
  indexOffMem a i = unsafeInlineIO $ withAddrAddr# a $ \addr# -> readOffPtr (Ptr addr#) i
  {-# INLINE indexOffMem #-}
  indexByteOffMem a i = unsafeInlineIO $ withAddrAddr# a $ \addr# -> readByteOffPtr (Ptr addr#) i
  {-# INLINE indexByteOffMem #-}
  copyToMBytesMem a si mb di c =
    withPtrAddr a $ \ptr -> copyPtrToMBytes (castPtr ptr) si mb di c
  {-# INLINE copyToMBytesMem #-}
  copyToPtrMem a si mb di c =
    withPtrAddr a $ \ptr -> copyPtrToPtr (castPtr ptr) si mb di c
  {-# INLINE copyToPtrMem #-}

instance MemWrite (MAddr a) where
  readOffMem a = readOffMAddr (castMAddr a)
  {-# INLINE readOffMem #-}
  readByteOffMem a = readByteOffMAddr (castMAddr a)
  {-# INLINE readByteOffMem #-}
  writeOffMem a = writeOffMAddr (castMAddr a)
  {-# INLINE writeOffMem #-}
  writeByteOffMem a = writeByteOffMAddr (castMAddr a)
  {-# INLINE writeByteOffMem #-}
  moveToPtrMem src srcOff dstPtr dstOff c =
    withAddrMAddr# src $ \ srcAddr# ->
      movePtrToPtr (Ptr srcAddr#) srcOff dstPtr dstOff c
  {-# INLINE moveToPtrMem #-}
  moveToMBytesMem src srcOff dst dstOff c =
    withAddrMAddr# src $ \ srcAddr# ->
      movePtrToMBytes (Ptr srcAddr#) srcOff dst dstOff c
  {-# INLINE moveToMBytesMem #-}
  copyMem src srcOff dst dstOff c =
    withAddrMAddr# dst $ \ dstAddr# ->
      copyToPtrMem src srcOff (Ptr dstAddr#) dstOff c
  {-# INLINE copyMem #-}
  moveMem src srcOff dst dstOff c =
    withAddrMAddr# dst $ \ dstAddr# ->
      moveToPtrMem src srcOff (Ptr dstAddr#) dstOff c
  {-# INLINE moveMem #-}
  setMem maddr = setMAddr (castMAddr maddr)
  {-# INLINE setMem #-}


class (MemRead (FrozenMem r), MemWrite r) => MemAlloc r where
  type FrozenMem r = (f :: *) | f -> r

  getByteCountMem :: MonadPrim s m => r s -> m (Count Word8)

  allocRawMem :: MonadPrim s m => Count Word8 -> m (r s)

  thawMem :: MonadPrim s m => FrozenMem r -> m (r s)

  freezeMem :: MonadPrim s m => r s -> m (FrozenMem r)


-- | Allocate enough memory for number of elements. Memory is not initialized and may
-- contain garbage. Use `allocZeroMem` if clean memory is needed.
--
-- [Unsafe Count] Negative element count will result in unpredictable behavior
--
-- @since 0.1.0
allocMem :: (MemAlloc r, MonadPrim s m, Prim a) => Count a -> m (r s)
allocMem n = allocRawMem (toByteCount n)


-- | Same as `allocMem`, but also use @memset@ to initialize all the new memory to zeros.
--
-- [Unsafe Count] Negative element count will result in unpredictable behavior
--
-- @since 0.1.0
allocZeroMem ::
     (MemAlloc r, MonadPrim s m, Prim a) => Count a -> m (r s)
allocZeroMem n = do
  m <- allocMem n
  m <$ setMem m 0 (toByteCount n) (0 :: Word8)


createMemST :: (MemAlloc r, Prim a) => Count a -> (forall s . r s -> ST s b) -> (b, FrozenMem r)
createMemST n f = runST $ do
  m <- allocZeroMem n
  res <- f m
  i <- freezeMem m
  pure (res, i)

createMemST_ :: (MemAlloc r, Prim a) => Count a -> (forall s . r s -> ST s b) -> FrozenMem r
createMemST_ n f = runST (allocZeroMem n >>= \m -> f m >> freezeMem m)


instance Typeable p => MemAlloc (MBytes p) where
  type FrozenMem (MBytes p) = Bytes p
  getByteCountMem = getByteCountMBytes
  allocRawMem = allocMBytes
  thawMem = thawBytes
  freezeMem = freezeMBytes



-- | It is only guaranteed to convert the whole memory to a list whenever the size of
-- allocated memory is exactly divisible by the size of the element, otherwise there will
-- be some slack left unaccounted for.
toListMem :: (MemRead r, Prim a) => r -> [a]
toListMem ba = build (\ c n -> foldrCountMem (countOfMem ba) c n ba)
{-# INLINE toListMem #-}


-- | Same as `toListMem`, except if there is some slack at the end of the memory that
-- didn't fit in a list it will be copied over into the new small `Bytes` chunk
toListSlackMem :: (MemRead r, Prim a) => r -> ([a], Maybe (Bytes 'Inc))
toListSlackMem ba = (build (\c n -> foldrCountMem k c n ba), slack)
  where
    (k, r) = countOfRemMem ba
    slack
      | r < 0 = Nothing
      | otherwise =
        Just $
        runST $ do
          let rc = Count r :: Count Word8
          mb <- allocUnpinnedMBytes rc
          copyToMBytesMem ba (coerce (toByteCount k :: Count Word8) :: Off Word8) mb 0 rc
          freezeMBytes mb
{-# INLINE toListSlackMem #-}

foldrCountMem :: (MemRead r, Prim a) => Count a -> (a -> b -> b) -> b -> r -> b
foldrCountMem (Count k) c nil bs = go 0
  where
    go i
      | i == k = nil
      | otherwise =
        let !v = indexOffMem bs (Off i)
         in v `c` go (i + 1)
{-# INLINE[0] foldrCountMem #-}


loadListInternal :: (MemWrite r, MonadPrim s m, Prim a) => Count a -> Int -> [a] -> r s -> m Ordering
loadListInternal (Count n) slack ys mb = do
  let go [] !i = pure (compare i n <> compare 0 slack)
      go (x:xs) !i
        | i < n = writeOffMem mb (Off i) x >> go xs (i + 1)
        | otherwise = pure GT
  go ys 0
{-# INLINE loadListInternal #-}

-- | Returns `EQ` if the full list did fit into the supplied memory chunk exactly.
-- Otherwise it will return either `LT` if the list was smaller than allocated memory or
-- `GT` if the list was bigger than the available memory and did not fit into `MBytes`.
loadListMem :: (MonadPrim s m, MemAlloc r, Prim a) => [a] -> r s -> m Ordering
loadListMem ys mb = do
  (c, slack) <- getCountOfRemMem mb
  loadListInternal (countAsProxy ys c) slack ys mb
{-# INLINE loadListMem #-}

fromListMemN :: (MemAlloc r, Prim a) => Count a -> [a] -> (Ordering, FrozenMem r)
fromListMemN n xs = createMemST n $ \mem -> loadListMem xs mem

fromListMemN_ :: (MemAlloc r, Prim a) => Count a -> [a] -> FrozenMem r
fromListMemN_ n xs = createMemST_ n $ \mem -> loadListMem xs mem


fromListMem :: (MemAlloc r, Prim a) => [a] -> FrozenMem r
fromListMem xs = createMemST_ (countAsProxy xs (coerce (length xs))) $ \mem -> loadListMem xs mem



concatMem :: (MemRead r, MemAlloc a) => [r] -> FrozenMem a
concatMem xs = do
  let c = Foldable.foldl' (\ !acc b -> acc + byteCountMem b) 0 xs
  createMemST_ (coerce c :: Count Word8) $ \mb -> do
    let load i b = do
          let cb@(Count n) = coerce (byteCountMem b) :: Count Word8
          (i + Off n) <$ copyMem b 0 mb i cb
    foldM_ load 0 xs
{-# INLINE concatMem #-}



allocCopyMem :: (MemRead r, MemAlloc a, MonadPrim s m) => r -> m (a s)
allocCopyMem a = do
  let n = byteCountOfMem a
  mem <- allocMem n
  mem <$ copyMem a 0 mem 0 n
{-# INLINE allocCopyMem #-}

freezeCopyMem :: (MemAlloc a, MonadPrim s m) => a s -> m (FrozenMem a)
freezeCopyMem = freezeMem >=> allocCopyMem >=> freezeMem
{-# INLINE freezeCopyMem #-}

thawCopyMem :: (MemAlloc a, MonadPrim s m) => FrozenMem a -> m (a s)
thawCopyMem = allocCopyMem
{-# INLINE thawCopyMem #-}

convertMem :: (MemRead r, MemAlloc a) => r -> FrozenMem a
convertMem a = runST $ allocCopyMem a >>= freezeMem
{-# INLINE convertMem #-}



byteCountOfMem :: MemRead r => r -> Count Word8
byteCountOfMem = byteCountMem
{-# INLINE byteCountOfMem #-}

countOfMem :: (MemRead r, Prim a) => r -> Count a
countOfMem = fromByteCount . byteCountMem
{-# INLINE countOfMem #-}

countOfRemMem :: (MemRead r, Prim a) => r -> (Count a, Int)
countOfRemMem = fromByteCountRem . byteCountMem
{-# INLINE countOfRemMem #-}

getCountOfMem :: (MemAlloc r, MonadPrim s m) => r s -> m (Count Word8)
getCountOfMem = fmap (fromByteCount . coerce) . getByteCountMem
{-# INLINE getCountOfMem #-}


getCountOfRemMem :: (MemAlloc r, MonadPrim s m, Prim a) => r s -> m (Count a, Int)
getCountOfRemMem = fmap (fromByteCountRem . coerce) . getByteCountMem
{-# INLINE getCountOfRemMem #-}


getByteCountOfMem :: (MemAlloc r, MonadPrim s m) => r s -> m (Count Word8)
getByteCountOfMem = fmap coerce . getByteCountMem
{-# INLINE getByteCountOfMem #-}

clone :: (MemAlloc r, MonadPrim s m) => r s -> m (r s)
clone mb = do
  n <- getByteCountOfMem mb
  mb' <- allocMem n
  mb' <$ moveMem mb 0 mb' 0 n
{-# INLINE clone #-}



-- withCopyMBytes ::
--      (MonadPrim s m, MemAlloc m (MBytes p s))
--   => Bytes p'
--   -> (MBytes p s -> m a)
--   -> m (a, Bytes p)
-- withCopyMBytes b f = do
--   let n = getByteCountMem b
--   mb <- allocMem n
--   copyBytesToMBytes8 b 0 mb 0 (Count n)
--   applyFreezeMBytes f mb
-- {-# INLINE withCopyMBytes #-}


-- withCopyMBytes_ ::
--      (MonadPrim s m, MemAlloc m (MBytes p s)) => Bytes p' -> (MBytes p s -> m a) -> m (Bytes p)
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
--         writeOffMBytes w64mb 0 w64
--         withMBytesPtr w64mb (f w64)
--         copyMBytesToPtr8 w64mb 0 ptr' 0 (Count nrem64)
--         pure g'
-- {-# INLINE fillPinnedMBytesWith #-}


-- class NoConstraint a
-- instance NoConstraint a

-- class Mut f where
--   type Elt f :: * -> Constraint
--   type Elt f = NoConstraint
--   newMut :: (Elt f a, MonadPrim s m) => m (f a s)

--   readMut :: (Elt f a, MonadPrim s m) => f a s -> m a

--   writeMut :: (Elt f a, MonadPrim s m) => f a s -> a -> m ()

--   makeMut :: (Elt f a, MonadPrim s m) => a -> m (f a s)
--   makeMut a = do
--     mut <- newMut
--     mut <$ writeMut mut a


-- instance Mut MAddr where
--   type Elt MAddr = Prim
--   newMut = allocMAddr (Count 1)
--   readMut = readMAddr
--   writeMut = writeMAddr

-- class Mut f => MFunctor f where
--   mmap :: (Elt f a, Elt f b, MonadPrim s m) => (a -> b) -> f a s -> m (f b s)

-- class Mut f => MTraverse f where
--   mmapM :: (Elt f a, Elt f b, MonadPrim s m) => (a -> m b) -> f a s -> m (f b s)

-- class MFunctor f => MApplicative f where
--   pureMut :: (Elt f a, MonadPrim s m) => a -> m (f a s)
--   liftMut ::
--     (Elt f a, Elt f b, Elt f c, MonadPrim s m) => (a -> b -> m c) -> f a s -> f b s -> m (f c s)

-- class MApplicative f => MMonad f where
--   bindMut ::
--     (Elt f a, Elt f b, MonadPrim s m) => f a s -> (a -> m b) -> f b s -> m (f c s)

-- instance MFunctor MAddr where
--   mmap f maddr = do
--     Count n <- getCountOfMAddr maddr
--     maddr' <- allocMAddr (Count n)
--     let go i =
--           when (i < n) $ do
--             writeOffMAddr maddr' (Off i) . f =<< readOffMAddr maddr (Off i)
--             go (i + 1)
--     maddr' <$ go 0

-- instance MTraverse MAddr where
--   mmapM f maddr = do
--     Count n <- getCountOfMAddr maddr
--     maddr' <- allocMAddr (Count n)
--     let go i =
--           when (i < n) $ do
--             writeOffMAddr maddr' (Off i) =<< f =<< readOffMAddr maddr (Off i)
--             go (i + 1)
--     maddr' <$ go 0
