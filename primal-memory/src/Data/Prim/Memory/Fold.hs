{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Prim.Memory.Fold
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Memory.Fold where

import Data.Prim
import Data.Prim.Memory.Internal


foldlMem ::
     forall e a mr. (Prim e, MemRead mr)
  => (a -> e -> a)
  -- ^ Folding function
  -> a
  -- ^ Initial accumulator
  -> mr
  -- ^ Memory region to iterate over
  -> a
foldlMem f = ifoldlMem (\a _ -> f a)
{-# INLINE foldlMem #-}


ifoldlMem ::
     forall e a mr. (Prim e, MemRead mr)
  => (a -> Off e -> e -> a)
  -- ^ Folding function
  -> a
  -- ^ Initial accumulator
  -> mr
  -- ^ Memory region to iterate over
  -> a
ifoldlMem f initAcc mem = ifoldlOffMem 0 (countMem mem :: Count e) f initAcc mem
{-# INLINE ifoldlMem #-}

ifoldlOffMem ::
     forall e a mr. (Prim e, MemRead mr)
  => Off e
  -- ^ Initial offset to start at
  -> Count e
  -- ^ Total number of elements to iterate through
  -> (a -> Off e -> e -> a)
  -- ^ Folding function
  -> a
  -- ^ Initial accumulator
  -> mr
  -- ^ Memory region to iterate over
  -> a
ifoldlOffMem off count f initAcc mem = loop initAcc off
  where
    k = countToOff count + off
    loop !acc i
      | i >= k = acc
      | otherwise = loop (f acc i (indexOffMem mem i)) (i + 1)
{-# INLINE ifoldlOffMem #-}




foldlLazyMem ::
     forall e a mr. (Prim e, MemRead mr)
  => (a -> e -> a)
  -- ^ Folding function
  -> a
  -- ^ Initial accumulator
  -> mr
  -- ^ Memory region to iterate over
  -> a
foldlLazyMem f = ifoldlLazyMem (\a _ -> f a)
{-# INLINE foldlLazyMem #-}

ifoldlLazyMem ::
     forall e a mr. (Prim e, MemRead mr)
  => (a -> Off e -> e -> a)
  -- ^ Folding function
  -> a
  -- ^ Initial accumulator
  -> mr
  -- ^ Memory region to iterate over
  -> a
ifoldlLazyMem f initAcc mem = ifoldlLazyOffMem 0 (countMem mem :: Count e) f initAcc mem
{-# INLINE ifoldlLazyMem #-}

ifoldlLazyOffMem ::
     forall e a mr. (Prim e, MemRead mr)
  => Off e
  -- ^ Initial offset to start at
  -> Count e
  -- ^ Total number of elements to iterate through
  -> (a -> Off e -> e -> a)
  -- ^ Folding function
  -> a
  -- ^ Initial accumulator
  -> mr
  -- ^ Memory region to iterate over
  -> a
ifoldlLazyOffMem off count f initAcc mem = loop initAcc off
  where
    k = countToOff count + off
    loop acc i
      | i >= k = acc
      | otherwise = loop (f acc i (indexOffMem mem i)) (i + 1)
{-# INLINE ifoldlLazyOffMem #-}




foldrMem ::
     forall e a mr. (Prim e, MemRead mr)
  => (e -> a -> a)
  -- ^ Folding function
  -> a
  -- ^ Initial accumulator
  -> mr
  -- ^ Memory region to iterate over
  -> a
foldrMem f = ifoldrMem (const f)
{-# INLINE foldrMem #-}

ifoldrMem ::
     forall e a mr. (Prim e, MemRead mr)
  => (Off e -> e -> a -> a)
  -- ^ Folding function
  -> a
  -- ^ Initial accumulator
  -> mr
  -- ^ Memory region to iterate over
  -> a
ifoldrMem f initAcc mem = ifoldrOffMem 0 (countMem mem :: Count e) f initAcc mem
{-# INLINE ifoldrMem #-}

ifoldrOffMem ::
     forall e a mr. (Prim e, MemRead mr)
  => Off e
  -- ^ Initial offset to start at
  -> Count e
  -- ^ Total number of elements to iterate through
  -> (Off e -> e -> a -> a)
  -- ^ Folding function
  -> a
  -- ^ Initial accumulator
  -> mr
  -- ^ Memory region to iterate over
  -> a
ifoldrOffMem off count f initAcc mem = loop initAcc off
  where
    k = countToOff count + off
    loop !acc i
      | i >= k = acc
      | otherwise = f i (indexOffMem mem i) (loop acc (i + 1))
{-# INLINE ifoldrOffMem #-}



foldrLazyMem ::
     forall e a mr. (Prim e, MemRead mr)
  => (e -> a -> a)
  -- ^ Folding function
  -> a
  -- ^ Initial accumulator
  -> mr
  -- ^ Memory region to iterate over
  -> a
foldrLazyMem f = ifoldrLazyMem (\_ -> f)
{-# INLINE foldrLazyMem #-}

ifoldrLazyMem ::
     forall e a mr. (Prim e, MemRead mr)
  => (Off e -> e -> a -> a)
  -- ^ Folding function
  -> a
  -- ^ Initial accumulator
  -> mr
  -- ^ Memory region to iterate over
  -> a
ifoldrLazyMem f initAcc mem =
  ifoldrLazyOffMem 0 (countMem mem :: Count e) f initAcc mem
{-# INLINE ifoldrLazyMem #-}


ifoldrLazyOffMem ::
     forall e a mr. (Prim e, MemRead mr)
  => Off e
  -- ^ Initial offset to start at
  -> Count e
  -- ^ Total number of elements to iterate through
  -> (Off e -> e -> a -> a)
  -- ^ Folding function
  -> a
  -- ^ Initial accumulator
  -> mr
  -- ^ Memory region to iterate over
  -> a
ifoldrLazyOffMem off count f initAcc mem = loop initAcc off
  where
    k = countToOff count + off
    loop acc i
      | i >= k = acc
      | otherwise = f i (indexOffMem mem i) (loop acc (i + 1))
{-# INLINE ifoldrLazyOffMem #-}




foldMapOffMem ::
     forall e m mr. (Prim e, MemRead mr, Monoid m)
  => Off e
  -> Count e
  -> (e -> m)
  -> mr
  -> m
foldMapOffMem off count f = ifoldrLazyOffMem off count (\_ e acc -> f e <> acc) mempty
{-# INLINE foldMapOffMem #-}

ifoldMapOffMem ::
     forall e m mr. (Prim e, MemRead mr, Monoid m)
  => Off e
  -> Count e
  -> (Off e -> e -> m)
  -> mr
  -> m
ifoldMapOffMem off count f =
  ifoldrLazyOffMem off count (\i e acc -> f i e <> acc) mempty
{-# INLINE ifoldMapOffMem #-}

allMem :: forall e mr . (Prim e, MemRead mr) => (e -> Bool) -> mr -> Bool
allMem p xs = getAll #. foldMapOffMem 0 (countMem xs :: Count e) (All #. p) $ xs
{-# INLINE allMem #-}

iallMem :: forall e mr . (Prim e, MemRead mr) => (Off e -> e -> Bool) -> mr -> Bool
iallMem p xs = getAll #. ifoldMapOffMem 0 (countMem xs :: Count e) (\i -> All #. p i) $ xs
{-# INLINE iallMem #-}



---------




-- Dangerous: ignores the slack
eqMem :: forall e mr . (Prim e, Eq e, MemRead mr) => mr -> mr -> Bool
eqMem m1 m2
  | isSameMem m1 m2 = True
  | otherwise = n == countMem m2 && eqOffMem m1 0 m2 0 n
   -- iallMem (\i e -> (e :: e) == indexOffMem m2 i) m1)
  where
    n = countMem m1 :: Count e
{-# INLINE eqMem #-}



-- | Check two regions of memory for equality using the `Eq` instance. It will return
-- `True` whenever both regions hold exactly the same elements and `False` as soon as the
-- first pair of mismatched elements is discovered in the two regions. It is safe for both
-- regions to refer to the same part of memory.
--
-- [Unsafe] When any precondition for either of the offsets @memOff1@, @memOff2@ or the
-- element count @memCount@ is violated the result is either unpredictable output or
-- failure with a segfault.
--
-- @since 0.3.0
eqOffMem ::
     (Prim e, Eq e, MemRead mr1, MemRead mr2)
  => mr1 -- ^ /memRead1/ - First region of memory
  -> Off e
  -- ^ /memOff1/ - Offset for @memRead1@ in number of elements
  --
  -- /__Precondition:__/
  --
  -- > 0 <= memOff1
  -> mr2 -- ^ /memRead2/ - Second region of memory
  -> Off e
  -- ^ /memOff2/ - Offset for @memRead1@ in number of elements
  --
  -- /__Precondition:__/
  --
  -- > 0 <= memOff2
  -> Count e
  -- ^ /memCount/ - Number of elements of type __@e@__ to compare
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memCount
  --
  -- > offToCount memOff1 + memCount < countMem memRead1
  --
  -- > offToCount memOff2 + memCount < countMem memRead2
  -> Bool
eqOffMem m1 off1 m2 off2 count = loop off1
  where
    doff = off2 - off1
    k = countToOff count + off1
    loop !i
      | i < k = indexOffMem m1 i == indexOffMem m2 (i + doff) && loop (i + 1)
      | otherwise = True
{-# INLINE[1] eqOffMem #-}

eqOffMemBinary ::
     forall e mr1 mr2. (Prim e, MemRead mr1, MemRead mr2)
  => mr1
  -> Off e
  -> mr2
  -> Off e
  -> Count e
  -> Bool
eqOffMemBinary m1 off1 m2 off2 count =
  eqByteOffMem m1 (toByteOff off1) m2 (toByteOff off2) (toByteCount count)
{-# INLINE eqOffMemBinary #-}

-- Verify this rule holds:
-- "eqOffMem/Char" forall mr1 (off1 :: Off Word) . eqOffMem mr1 off1 = eqOffMemBinary mr1 off1
{-# RULES
"eqOffMem/Word" forall mr1 (off1 :: Off Word) . eqOffMem mr1 off1 = eqOffMemBinary mr1 off1
"eqOffMem/Word8" eqOffMem = eqByteOffMem
"eqOffMem/Word16" forall mr1 (off1 :: Off Word16) . eqOffMem mr1 off1 = eqOffMemBinary mr1 off1
"eqOffMem/Word32" forall mr1 (off1 :: Off Word32) . eqOffMem mr1 off1 = eqOffMemBinary mr1 off1
"eqOffMem/Word64" forall mr1 (off1 :: Off Word64) . eqOffMem mr1 off1 = eqOffMemBinary mr1 off1
"eqOffMem/Int" forall mr1 (off1 :: Off Int) . eqOffMem mr1 off1 = eqOffMemBinary mr1 off1
"eqOffMem/Int8" forall mr1 (off1 :: Off Int8) . eqOffMem mr1 off1 = eqOffMemBinary mr1 off1
"eqOffMem/Int16" forall mr1 (off1 :: Off Int16) . eqOffMem mr1 off1 = eqOffMemBinary mr1 off1
"eqOffMem/Int32" forall mr1 (off1 :: Off Int32) . eqOffMem mr1 off1 = eqOffMemBinary mr1 off1
"eqOffMem/Int64" forall mr1 (off1 :: Off Int64) . eqOffMem mr1 off1 = eqOffMemBinary mr1 off1
#-}

eqOffMutMem ::
     forall e ma1 ma2 m s. (Prim e, Eq e, MonadPrim s m, MemWrite ma1, MemWrite ma2)
  => ma1 s
  -> Off e
  -> ma2 s
  -> Off e
  -> Count e
  -> m Bool
eqOffMutMem mm1 off1 mm2 off2 count = loop off1
  where
    doff = off2 - off1
    k = countToOff count + off1
    loop !i
      | i < k = do
        e1 <- readOffMutMem mm1 i
        e2 <- readOffMutMem mm2 (i + doff)
        if e1 == e2
          then loop (i + 1)
          else pure False
      | otherwise = pure True
{-# INLINE eqOffMutMem #-}


-- | Compare two mutable memory regions for element equality. Regions themselves are not
-- modified, as such it is semantically similar to `eqMem` which works on immutable
-- regions.
eqMutMem ::
     forall e ma m s. (Prim e, Eq e, MonadPrim s m, MemAlloc ma)
  => ma s
  -> ma s
  -> m Bool
eqMutMem mm1 mm2
  | isSameMutMem mm1 mm2 = pure True
  | otherwise = do
    n1 <- getCountMutMem mm1
    n2 <- getCountMutMem mm2
    if n1 /= n2
      then pure False
      else eqOffMutMem mm1 0 mm2 0 (n1 :: Count e)
{-# INLINE eqMutMem #-}



-- | Compare two regions using the `Ord` instance. It will return `EQ` whenever both
-- regions hold exactly the same elements and `LT` or `GT` as soon as the first discovered
-- element that is less than or greater than respectfully in the first region when
-- compared to the second one. It is safe for both regions to refer to the same part of
-- memory.
--
-- @since 0.3.0
compareMem ::
     forall e mr. (Prim e, Ord e, MemRead mr)
  => mr -- ^ /memRead1/ - First region of memory
  -> mr -- ^ /memRead2/ - Second region of memory
  -> Ordering
compareMem m1 m2
  | isSameMem m1 m2 = EQ
  | otherwise = compareOffMem m1 0 m2 0 (min n1 n2) <> compare n1 n2
  where
    n1 = countMem m1 :: Count e
    n2 = countMem m2 :: Count e
{-# INLINE compareMem #-}

-- | Compare two regions using the `Ord` instance. It will return `EQ` whenever both
-- regions hold exactly the same elements and `LT` or `GT` as soon as the first discovered
-- element that is less than or greater than respectfully in the first region when
-- compared to the second one. It is safe for both regions to refer to the same part of
-- memory.
--
-- [Unsafe] When any precondition for either of the offsets @memOff1@, @memOff2@ or the
-- element count @memCount@ is violated the result is either unpredictable output or
-- failure with a segfault.
--
-- @since 0.3.0
compareOffMem ::
     (Prim e, Ord e, MemRead mr1, MemRead mr2)
  => mr1 -- ^ /memRead1/ - First region of memory
  -> Off e
  -- ^ /memOff1/ - Offset for @memRead1@ in number of elements
  --
  -- /__Precondition:__/
  --
  -- > 0 <= memOff1
  -> mr2 -- ^ /memRead2/ - Second region of memory
  -> Off e
  -- ^ /memOff2/ - Offset for @memRead1@ in number of elements
  --
  -- /__Precondition:__/
  --
  -- > 0 <= memOff2
  -> Count e
  -- ^ /memCount/ - Number of elements of type __@e@__ to compare
  --
  -- /__Preconditions:__/
  --
  -- > 0 <= memCount
  --
  -- > offToCount memOff1 + memCount < countMem memRead1
  --
  -- > offToCount memOff2 + memCount < countMem memRead2
  -> Ordering
compareOffMem m1 off1 m2 off2 count = loop off1
  where
    doff = off2 - off1
    k = countToOff count + off1
    loop !i
      | i < k = compare (indexOffMem m1 i) (indexOffMem m2 (i + doff)) <> loop (i + 1)
      | otherwise = EQ
{-# INLINE [1] compareOffMem #-}

-- Some loop unrolling to get an extra kick.
{-# RULES
"compareOffMem/Word8" compareOffMem = compareByteOffMem
"compareOffMem/Word16"
  forall mr1 (off1 :: Off Word16) . compareOffMem mr1 off1 = compareOffMem4x mr1 off1
"compareOffMem/Word32"
  forall mr1 (off1 :: Off Word32) . compareOffMem mr1 off1 = compareOffMem4x mr1 off1
"compareOffMem/Int8"
  forall mr1 (off1 :: Off Int8) . compareOffMem mr1 off1 = compareOffMem4x mr1 off1
"compareOffMem/Int16"
  forall mr1 (off1 :: Off Int16) . compareOffMem mr1 off1 = compareOffMem4x mr1 off1
"compareOffMem/Int32"
  forall mr1 (off1 :: Off Int32) . compareOffMem mr1 off1 = compareOffMem4x mr1 off1
"compareOffMem/Char"
  forall mr1 (off1 :: Off Char) . compareOffMem mr1 off1 = compareOffMem4x mr1 off1
"compareOffMem/Float"
  forall mr1 (off1 :: Off Float) . compareOffMem mr1 off1 = compareOffMem4x mr1 off1
#-}

-- | A compare function that handles 4 elements from each region in a single iteration
compareOffMem4x ::
     (Prim e, Ord e, MemRead mr1, MemRead mr2)
  => mr1 -- ^ /memRead1/ - First region of memory
  -> Off e
  -> mr2 -- ^ /memRead2/ - Second region of memory
  -> Off e
  -> Count e
  -> Ordering
compareOffMem4x m1 off1 m2 off2 count = loop off1
  where
    doff = off2 - off1
    k = countToOff count + off1
    kRem = countToOff count `rem` 4
    k4 = k - kRem
    loop !i
      | i < k4 =
        let !i' = i + 1
            !i'' = i + 2
            !i''' = i + 3
         in compare (indexOffMem m1 i) (indexOffMem m2 (i + doff)) <>
            compare (indexOffMem m1 i') (indexOffMem m2 (i' + doff)) <>
            compare (indexOffMem m1 i'') (indexOffMem m2 (i'' + doff)) <>
            compare (indexOffMem m1 i''') (indexOffMem m2 (i''' + doff)) <>
            loop (i + 4)
      | i < k = compare (indexOffMem m1 i) (indexOffMem m2 (i + doff)) <> loop (i + 1)
      | otherwise = EQ
{-# INLINE compareOffMem4x #-}
