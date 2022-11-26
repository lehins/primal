{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Primal.Memory.Fold
-- Copyright   : (c) Alexey Kuleshevich 2020-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module Primal.Memory.Fold where

import Data.Semigroup
import Primal.Element.Unbox
import Primal.Memory.Internal
import Primal.Monad

foldlMem
  :: forall e a mr
   . (Unbox e, MemRead mr)
  => (a -> e -> a)
  -- ^ Folding function
  -> a
  -- ^ Initial accumulator
  -> mr
  -- ^ Memory region to iterate over
  -> a
foldlMem f = ifoldlMem (\a _ -> f a)
{-# INLINE foldlMem #-}

ifoldlMem
  :: forall e a mr
   . (Unbox e, MemRead mr)
  => (a -> Off e -> e -> a)
  -- ^ Folding function
  -> a
  -- ^ Initial accumulator
  -> mr
  -- ^ Memory region to iterate over
  -> a
ifoldlMem f initAcc mem = ifoldlOffMem 0 (countMem mem :: Count e) f initAcc mem
{-# INLINE ifoldlMem #-}

ifoldlOffMem
  :: forall e a mr
   . (Unbox e, MemRead mr)
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

foldlLazyMem
  :: forall e a mr
   . (Unbox e, MemRead mr)
  => (a -> e -> a)
  -- ^ Folding function
  -> a
  -- ^ Initial accumulator
  -> mr
  -- ^ Memory region to iterate over
  -> a
foldlLazyMem f = ifoldlLazyMem (\a _ -> f a)
{-# INLINE foldlLazyMem #-}

ifoldlLazyMem
  :: forall e a mr
   . (Unbox e, MemRead mr)
  => (a -> Off e -> e -> a)
  -- ^ Folding function
  -> a
  -- ^ Initial accumulator
  -> mr
  -- ^ Memory region to iterate over
  -> a
ifoldlLazyMem f initAcc mem = ifoldlLazyOffMem 0 (countMem mem :: Count e) f initAcc mem
{-# INLINE ifoldlLazyMem #-}

ifoldlLazyOffMem
  :: forall e a mr
   . (Unbox e, MemRead mr)
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

foldrMem
  :: forall e a mr
   . (Unbox e, MemRead mr)
  => (e -> a -> a)
  -- ^ Folding function
  -> a
  -- ^ Initial accumulator
  -> mr
  -- ^ Memory region to iterate over
  -> a
foldrMem f = ifoldrMem (const f)
{-# INLINE foldrMem #-}

ifoldrMem
  :: forall e a mr
   . (Unbox e, MemRead mr)
  => (Off e -> e -> a -> a)
  -- ^ Folding function
  -> a
  -- ^ Initial accumulator
  -> mr
  -- ^ Memory region to iterate over
  -> a
ifoldrMem f initAcc mem = ifoldrOffMem 0 (countMem mem :: Count e) f initAcc mem
{-# INLINE ifoldrMem #-}

ifoldrOffMem
  :: forall e a mr
   . (Unbox e, MemRead mr)
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

-- | Right fold with a lazy accumulator
--
-- @since 0.3.0
foldrLazyMem
  :: forall e a mr
   . (Unbox e, MemRead mr)
  => (e -> a -> a)
  -- ^ Folding function
  -> a
  -- ^ Initial accumulator
  -> mr
  -- ^ Memory region to iterate over
  -> a
foldrLazyMem f = ifoldrLazyMem (const f)
{-# INLINE foldrLazyMem #-}

-- | Right fold with a lazy accumulator using an offset aware function
--
-- @since 0.3.0
ifoldrLazyMem
  :: forall e a mr
   . (Unbox e, MemRead mr)
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

ifoldrLazyOffMem
  :: forall e a mr
   . (Unbox e, MemRead mr)
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

foldMapOffMem
  :: forall e m mr
   . (Unbox e, MemRead mr, Monoid m)
  => Off e
  -> Count e
  -> (e -> m)
  -> mr
  -> m
foldMapOffMem off count f = ifoldrLazyOffMem off count (\_ e acc -> f e `mappend` acc) mempty
{-# INLINE foldMapOffMem #-}

ifoldMapOffMem
  :: forall e m mr
   . (Unbox e, MemRead mr, Monoid m)
  => Off e
  -> Count e
  -> (Off e -> e -> m)
  -> mr
  -> m
ifoldMapOffMem off count f =
  ifoldrLazyOffMem off count (\i e acc -> f i e `mappend` acc) mempty
{-# INLINE ifoldMapOffMem #-}

anyOffMem
  :: forall e mr
   . (Unbox e, MemRead mr)
  => Off e
  -> Count e
  -> (e -> Bool)
  -> mr
  -> Bool
anyOffMem off count p = getAny #. foldMapOffMem off count (Any #. p)
{-# INLINE anyOffMem #-}

ianyOffMem
  :: forall e mr
   . (Unbox e, MemRead mr)
  => Off e
  -> Count e
  -> (Off e -> e -> Bool)
  -> mr
  -> Bool
ianyOffMem off count p = getAny #. ifoldMapOffMem off count (\i -> Any #. p i)
{-# INLINE ianyOffMem #-}

anyMem :: forall e mr. (Unbox e, MemRead mr) => (e -> Bool) -> mr -> Bool
anyMem p xs = anyOffMem 0 (countMem xs :: Count e) p xs
{-# INLINE anyMem #-}

ianyMem :: forall e mr. (Unbox e, MemRead mr) => (Off e -> e -> Bool) -> mr -> Bool
ianyMem p xs = ianyOffMem 0 (countMem xs :: Count e) p xs
{-# INLINE ianyMem #-}

allOffMem
  :: forall e mr
   . (Unbox e, MemRead mr)
  => Off e
  -> Count e
  -> (e -> Bool)
  -> mr
  -> Bool
allOffMem off count p = getAll #. foldMapOffMem off count (All #. p)
{-# INLINE allOffMem #-}

iallOffMem
  :: forall e mr
   . (Unbox e, MemRead mr)
  => Off e
  -> Count e
  -> (Off e -> e -> Bool)
  -> mr
  -> Bool
iallOffMem off count p = getAll #. ifoldMapOffMem off count (\i -> All #. p i)
{-# INLINE iallOffMem #-}

allMem :: forall e mr. (Unbox e, MemRead mr) => (e -> Bool) -> mr -> Bool
allMem p xs = allOffMem 0 (countMem xs :: Count e) p xs
{-# INLINE allMem #-}

iallMem :: forall e mr. (Unbox e, MemRead mr) => (Off e -> e -> Bool) -> mr -> Bool
iallMem p xs = iallOffMem 0 (countMem xs :: Count e) p xs
{-# INLINE iallMem #-}

---------

-- | Compare two read only buffers for equality element-by-element using the `Eq`
-- instance. It will short circuit if exact same memory buffer is supplied, which will
-- return `True`, or when there is a size mismatch, which will naturally result in a
-- `False` answer. The very first two unequal elements will also cause the loop to short
-- circuit.
--
-- Dangerous: ignores the slack
eqMem :: forall e mr. (Unbox e, Eq e, MemRead mr) => mr -> mr -> Bool
eqMem m1 m2
  | isSameMem m1 m2 = True
  | otherwise = n == countMem m2 && eqOffMem m1 0 m2 0 n
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
eqOffMem
  :: (Unbox e, Eq e, MemRead mr1, MemRead mr2)
  => mr1
  -- ^ /memRead1/ - First region of memory
  -> Off e
  -- ^ /memOff1/ - Offset for @memRead1@ in number of elements
  --
  -- /__Precondition:__/
  --
  -- > 0 <= memOff1
  -> mr2
  -- ^ /memRead2/ - Second region of memory
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
{-# INLINE [1] eqOffMem #-}

eqOffMemBinary
  :: forall e mr1 mr2
   . (Unbox e, MemRead mr1, MemRead mr2)
  => mr1
  -> Off e
  -> mr2
  -> Off e
  -> Count e
  -> Bool
eqOffMemBinary m1 off1 m2 off2 count =
  eqByteOffMem m1 (toByteOff off1) m2 (toByteOff off2) (toByteCount count)
{-# INLINE eqOffMemBinary #-}

{-# RULES
"eqOffMem/Char" forall mr (off :: Off Char). eqOffMem mr off = eqOffMemBinary mr off
"eqOffMem/Word" forall mr (off :: Off Word). eqOffMem mr off = eqOffMemBinary mr off
"eqOffMem/Word8" eqOffMem = eqByteOffMem
"eqOffMem/Word16" forall mr (off :: Off Word16). eqOffMem mr off = eqOffMemBinary mr off
"eqOffMem/Word32" forall mr (off :: Off Word32). eqOffMem mr off = eqOffMemBinary mr off
"eqOffMem/Word64" forall mr (off :: Off Word64). eqOffMem mr off = eqOffMemBinary mr off
"eqOffMem/Int" forall mr (off :: Off Int). eqOffMem mr off = eqOffMemBinary mr off
"eqOffMem/Int8" forall mr (off :: Off Int8). eqOffMem mr off = eqOffMemBinary mr off
"eqOffMem/Int16" forall mr (off :: Off Int16). eqOffMem mr off = eqOffMemBinary mr off
"eqOffMem/Int32" forall mr (off :: Off Int32). eqOffMem mr off = eqOffMemBinary mr off
"eqOffMem/Int64" forall mr (off :: Off Int64). eqOffMem mr off = eqOffMemBinary mr off
  #-}

eqOffMutMem
  :: forall e ma1 ma2 m s
   . (Unbox e, Eq e, Primal s m, MemWrite ma1, MemWrite ma2)
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
    loop i
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
eqMutMem
  :: forall e ma m s
   . (Unbox e, Eq e, Primal s m, MemFreeze ma)
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
compareMem
  :: forall e mr
   . (Unbox e, Ord e, MemRead mr)
  => mr
  -- ^ /memRead1/ - First region of memory
  -> mr
  -- ^ /memRead2/ - Second region of memory
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
compareOffMem
  :: (Unbox e, Ord e, MemRead mr1, MemRead mr2)
  => mr1
  -- ^ /memRead1/ - First region of memory
  -> Off e
  -- ^ /memOff1/ - Offset for @memRead1@ in number of elements
  --
  -- /__Precondition:__/
  --
  -- > 0 <= memOff1
  -> mr2
  -- ^ /memRead2/ - Second region of memory
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
    kRem = countToOff count `rem` 4
    k4 = k - kRem
    -- Some loop unrolling to get an extra 25% kick for smaller types
    loop !i
      | i < k4 =
          let !i' = i + 1
              !i'' = i + 2
              !i''' = i + 3
           in compare (indexOffMem m1 i) (indexOffMem m2 (i + doff))
                <> compare (indexOffMem m1 i') (indexOffMem m2 (i' + doff))
                <> compare (indexOffMem m1 i'') (indexOffMem m2 (i'' + doff))
                <> compare (indexOffMem m1 i''') (indexOffMem m2 (i''' + doff))
                <> loop (i + 4)
      | i < k = compare (indexOffMem m1 i) (indexOffMem m2 (i + doff)) <> loop (i + 1)
      | otherwise = EQ
{-# INLINE [1] compareOffMem #-}

{-# RULES
"compareOffMem/Word8" compareOffMem = compareByteOffMem
  #-}
