{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Prim.Memory
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Memory where


import Control.Monad.ST
import Control.Prim.Monad
import Control.Prim.Monad.Unsafe
import Data.Foldable as Foldable
import Data.Prim
import Data.Prim.Memory.Bytes.Internal
  ( Bytes(..)
  , MBytes(..)
  , Pinned(..)
  , allocMBytes
  , allocUnpinnedMBytes
  , byteCountBytes
  , compareByteOffBytes
  , copyBytesToMBytes
  , freezeMBytes
  , getByteCountMBytes
  , indexByteOffBytes
  , indexOffBytes
  , isSameBytes
  , moveMBytesToMBytes
  , readByteOffMBytes
  , readOffMBytes
  , setMBytes
  , thawBytes
  , writeByteOffMBytes
  , writeOffMBytes
  )
import Data.List as List
import Data.Prim.Memory.ByteString
import Data.Prim.Memory.ForeignPtr
import Data.Prim.Memory.Ptr
import Foreign.Prim
import Numeric (showHex)
import qualified Data.Semigroup as Semigroup



class MemRead r where
  byteCountMem :: r -> Count Word8

  indexOffMem :: Prim a => r -> Off a -> a

  indexByteOffMem :: Prim a => r -> Off Word8 -> a

  -- | Source and target can't refer to the same memory chunks
  copyToMBytesMem :: (MonadPrim s m, Prim a) => r -> Off a -> MBytes p s -> Off a -> Count a -> m ()

  -- | Source and target can't refer to the same memory chunks
  copyToPtrMem :: (MonadPrim s m, Prim a) => r -> Off a -> Ptr a -> Off a -> Count a -> m ()

  compareByteOffToPtrMem ::
    (MonadPrim s m, Prim a) => r -> Off Word8 -> Ptr a -> Off Word8 -> Count a -> m Ordering

  compareByteOffToBytesMem ::
    (MonadPrim s m, Prim a) => r -> Off Word8 -> Bytes p -> Off Word8 -> Count a -> m Ordering

  compareByteOffMem :: (MemRead r', Prim a) => r' -> Off Word8 -> r -> Off Word8 -> Count a -> Ordering

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
    withPtrAccess bs $ \srcPtr -> copyPtrToMBytes srcPtr srcOff mb dstOff c
  {-# INLINE copyToMBytesMem #-}
  copyToPtrMem bs srcOff dstPtr dstOff c =
    withPtrAccess bs $ \srcPtr -> copyPtrToPtr srcPtr srcOff dstPtr dstOff c
  {-# INLINE copyToPtrMem #-}
  compareByteOffToPtrMem bs off1 ptr2 off2 c =
    withPtrAccess bs $ \ptr1 -> pure $ compareByteOffPtrToPtr ptr1 off1 ptr2 off2 c
  {-# INLINE compareByteOffToPtrMem #-}
  compareByteOffToBytesMem bs off1 bytes off2 c =
    withPtrAccess bs $ \ptr1 -> pure $ compareByteOffPtrToBytes ptr1 off1 bytes off2 c
  {-# INLINE compareByteOffToBytesMem #-}
  compareByteOffMem mem1 off1 bs off2 c =
    unsafeInlineIO $ withPtrAccess bs $ \ptr2 -> compareByteOffToPtrMem mem1 off1 ptr2 off2 c
  {-# INLINE compareByteOffMem #-}



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
  compareByteOffToPtrMem sbs = compareByteOffToPtrMem (fromShortByteStringBytes sbs)
  {-# INLINE compareByteOffToPtrMem #-}
  compareByteOffToBytesMem sbs = compareByteOffToBytesMem (fromShortByteStringBytes sbs)
  {-# INLINE compareByteOffToBytesMem #-}
  compareByteOffMem mem off1 sbs = compareByteOffMem mem off1 (fromShortByteStringBytes sbs)
  {-# INLINE compareByteOffMem #-}

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
  compareByteOffToPtrMem bytes1 off1 ptr2 off2 c =
    pure $ compareByteOffBytesToPtr bytes1 off1 ptr2 off2 c
  {-# INLINE compareByteOffToPtrMem #-}
  compareByteOffToBytesMem bytes1 off1 bytes2 off2 c =
    pure $ compareByteOffBytes bytes1 off1 bytes2 off2 c
  {-# INLINE compareByteOffToBytesMem #-}
  compareByteOffMem mem1 off1 bs off2 c =
    unsafeInlineIO $ compareByteOffToBytesMem mem1 off1 bs off2 c
  {-# INLINE compareByteOffMem #-}


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
{-# INLINE allocMem #-}


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
{-# INLINE allocZeroMem #-}


createMemST :: (MemAlloc r, Prim a) => Count a -> (forall s . r s -> ST s b) -> (b, FrozenMem r)
createMemST n f = runST $ do
  m <- allocZeroMem n
  res <- f m
  i <- freezeMem m
  pure (res, i)
{-# INLINE createMemST #-}

createMemST_ :: (MemAlloc r, Prim a) => Count a -> (forall s . r s -> ST s b) -> FrozenMem r
createMemST_ n f = runST (allocZeroMem n >>= \m -> f m >> freezeMem m)
{-# INLINE createMemST_ #-}


instance Typeable p => MemAlloc (MBytes p) where
  type FrozenMem (MBytes p) = Bytes p
  getByteCountMem = getByteCountMBytes
  {-# INLINE getByteCountMem #-}
  allocRawMem = allocMBytes
  {-# INLINE allocRawMem #-}
  thawMem = thawBytes
  {-# INLINE thawMem #-}
  freezeMem = freezeMBytes
  {-# INLINE freezeMem #-}



-- | It is only guaranteed to convert the whole memory to a list whenever the size of
-- allocated memory is exactly divisible by the size of the element, otherwise there will
-- be some slack left unaccounted for.
toListMem :: (MemRead r, Prim a) => r -> [a]
toListMem ba = build (\ c n -> foldrCountMem (countMem ba) c n ba)
{-# INLINE toListMem #-}
{-# SPECIALIZE toListMem :: Prim a => Bytes p -> [a] #-}

-- | Same as `toListMem`, except if there is some slack at the end of the memory that
-- didn't fit in a list it will be copied over into the new small `Bytes` chunk
toListSlackMem :: (MemRead r, Prim a) => r -> ([a], Maybe (Bytes 'Inc))
toListSlackMem ba = (build (\c n -> foldrCountMem k c n ba), slack)
  where
    (k, r) = countRemMem ba
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

loadListInternal_ :: (MemWrite r, MonadPrim s m, Prim e) => Count e -> [e] -> r s -> m ()
loadListInternal_ (Count n) ys mb =
  let go [] _     = pure ()
      go (x:xs) i = when (i < n) $ writeOffMem mb (Off i) x >> go xs (i + 1)
   in go ys 0
{-# INLINE loadListInternal_ #-}

-- | Returns `EQ` if the full list did fit into the supplied memory chunk exactly.
-- Otherwise it will return either `LT` if the list was smaller than allocated memory or
-- `GT` if the list was bigger than the available memory and did not fit into `MBytes`.
loadListMem :: (MonadPrim s m, MemAlloc r, Prim e) => [e] -> r s -> m Ordering
loadListMem ys mb = do
  (c, slack) <- getCountRemMem mb
  loadListInternal (countAsProxy ys c) slack ys mb
{-# INLINE loadListMem #-}

loadListMem_ :: (MonadPrim s m, MemAlloc r, Prim e) => [e] -> r s -> m ()
loadListMem_ ys mb = do
  c <- getCountMem mb
  loadListInternal_ (countAsProxy ys c) ys mb
{-# INLINE loadListMem_ #-}


fromListMemN :: (MemAlloc r, Prim a) => Count a -> [a] -> (Ordering, FrozenMem r)
fromListMemN n xs = createMemST n (loadListMem xs)
{-# INLINE fromListMemN #-}

fromListMemN_ :: (MemAlloc r, Prim a) => Count a -> [a] -> FrozenMem r
fromListMemN_ n xs = createMemST_ n (loadListMem_ xs)
{-# INLINE fromListMemN_ #-}

fromListMem :: (MemAlloc r, Prim a) => [a] -> FrozenMem r
fromListMem xs = createMemST_ (countAsProxy xs (coerce (length xs))) (loadListMem_ xs)
{-# INLINE fromListMem #-}


appendMem :: (MemRead r1, MemRead r2, MemAlloc a) => r1 -> r2 -> FrozenMem a
appendMem r1 r2 =
  createMemST_ (n1 + n2) $ \mem -> do
    copyMem r1 0 mem 0 n1
    copyMem r2 (coerce n1) mem (coerce n1) n2
  where
    n1 = byteCountMem r1
    n2 = byteCountMem r2


concatMem :: (MemRead r, MemAlloc a) => [r] -> FrozenMem a
concatMem xs = do
  let c = Foldable.foldl' (\ !acc b -> acc + byteCountMem b) 0 xs
  createMemST_ (coerce c :: Count Word8) $ \mb -> do
    let load i b = do
          let cb@(Count n) = byteCountMem b :: Count Word8
          (i + Off n) <$ copyMem b 0 mb i cb
    foldM_ load 0 xs
{-# INLINE concatMem #-}



allocCopyMem :: (MemRead r, MemAlloc a, MonadPrim s m) => r -> m (a s)
allocCopyMem a = do
  let n = byteCountMem a
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

countMem :: (MemRead r, Prim e) => r -> Count e
countMem = fromByteCount . byteCountMem
{-# INLINE countMem #-}

countRemMem :: (MemRead r, Prim e) => r -> (Count e, Int)
countRemMem = fromByteCountRem . byteCountMem
{-# INLINE countRemMem #-}

getCountMem :: (MemAlloc r, MonadPrim s m, Prim e) => r s -> m (Count e)
getCountMem = fmap (fromByteCount . coerce) . getByteCountMem
{-# INLINE getCountMem #-}


getCountRemMem :: (MemAlloc r, MonadPrim s m, Prim e) => r s -> m (Count e, Int)
getCountRemMem = fmap (fromByteCountRem . coerce) . getByteCountMem
{-# INLINE getCountRemMem #-}


clone :: (MemAlloc r, MonadPrim s m) => r s -> m (r s)
clone mb = do
  n <- getByteCountMem mb
  mb' <- allocMem n
  mb' <$ moveMem mb 0 mb' 0 n
{-# INLINE clone #-}

eqMem :: (MemRead r1, MemRead r2) => r1 -> r2 -> Bool
eqMem b1 b2 = n == byteCountMem b2 && compareByteOffMem b1 0 b2 0 n == EQ
  where
    n = byteCountMem b1
{-# INLINE eqMem #-}


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
--     Count n <- getCountMAddr maddr
--     maddr' <- allocMAddr (Count n)
--     let go i =
--           when (i < n) $ do
--             writeOffMAddr maddr' (Off i) . f =<< readOffMAddr maddr (Off i)
--             go (i + 1)
--     maddr' <$ go 0

-- instance MTraverse MAddr where
--   mmapM f maddr = do
--     Count n <- getCountMAddr maddr
--     maddr' <- allocMAddr (Count n)
--     let go i =
--           when (i < n) $ do
--             writeOffMAddr maddr' (Off i) =<< f =<< readOffMAddr maddr (Off i)
--             go (i + 1)
--     maddr' <$ go 0


-------------------
-- Bytes orphans --
-------------------



instance Show (Bytes p) where
  show b =
    Foldable.foldr' ($) "]" $
    ('[' :) : List.intersperse (',' :) (map (("0x" ++) .) (showsHexMem b))

instance Typeable p => IsList (Bytes p) where
  type Item (Bytes p) = Word8
  fromList = fromListMem
  fromListN n = fromListMemN_ (Count n)
  toList = toListMem

instance Eq (Bytes p) where
  b1 == b2 = isSameBytes b1 b2 || eqMem b1 b2

instance Ord (Bytes p) where
  compare b1 b2 =
    compare n (byteCountBytes b2) <> compareByteOffBytes b1 0 b2 0 n
    where
      n = byteCountBytes b1

-- instance Typeable p => Semigroup.Semigroup (Bytes p) where



-- | A list of `ShowS` that covert bytes to base16 encoded strings. Each element of the list
-- is a function that will convert one byte.
--
-- >>> mb <- newPinnedMBytes (Count 5 :: Count Int)
-- >>> mapM_ (\i -> writeOffMBytes mb (pred i) i) [1 .. 5]
-- >>> foldr ($) "" . showsBytesHex <$> freezeMBytes mb
-- "01000000000000000200000000000000030000000000000004000000000000000500000000000000"
--
showsHexMem :: MemRead r => r -> [ShowS]
showsHexMem b = map toHex (toListMem b :: [Word8])
  where
    toHex b8 =
      (if b8 <= 0x0f
         then ('0' :)
         else id) .
      showHex b8
