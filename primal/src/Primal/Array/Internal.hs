{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Primal.Array.Internal
-- Copyright   : (c) Alexey Kuleshevich 2020-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Array.Internal
  ( Size(..)
    -- * Helper functions
  , uninitialized
  , makeMutWith
  , fromListMutWith
  , foldrWithFB
  , eqWith
  , eqWithST
  , compareWith
  , compareWithST
  , appendWith
  , concatWith
  , cycleWith
  , liftEqWith
  , liftCompareWith
  , liftShowsPrecArray
  ) where

import Control.Monad
import Primal.Exception
import qualified Data.Foldable as F
import Primal.Unbox
import Primal.Unbox.Class


-- | Number of elements contained in the data structure
--
-- @since 0.3.0
newtype Size = Size { unSize :: Int }
  deriving (Show, Eq, Ord, Num, Real, Integral, Bounded, Enum)

instance Unbox Size where
  type UnboxIso Size = Int

-------------
-- Helpers --
-- ======= --

-- | Default "raw" element for boxed arrays.
uninitialized ::
     HasCallStack
  => String -- ^ Module name
  -> String -- ^ Function name
  -> a
uninitialized mname fname = raiseImprecise $ UndefinedElement $ mname ++ "." ++ fname
{-# NOINLINE uninitialized #-}

-- | Convert a list to a mutable array
fromListMutWith ::
     Monad m
  => (Size -> m b) -- ^ Function for array creation
  -> (b -> Int -> a -> m ()) -- ^ Function for writing elements
  -> Size -- ^ Size for the created array
  -> [a] -- ^ Function for generating elements from array index
  -> m b
fromListMutWith new write sz@(Size n) ls = do
  ma <- new sz
  let go i =
        \case
          x:xs
            | i < n -> write ma i x >> go (i + 1) xs
          _ -> pure ()
  ma <$ go 0 ls
{-# INLINE fromListMutWith #-}


-- | Helper for generating mutable arrays
--
-- @since 0.3.0
makeMutWith ::
     Monad m
  => (Size -> m b) -- ^ Function for array creation
  -> (b -> Int -> a -> m ()) -- ^ Function for writing elements
  -> Size -- ^ Size for the created array
  -> (Int -> m a) -- ^ Function for generating elements from array index
  -> m b
makeMutWith new write sz@(Size n) f = do
  ma <- new sz
  let go i = when (i < n) $ f i >>= write ma i >> go (i + 1)
  ma <$ go 0
{-# INLINE makeMutWith #-}


-- | Right fold that is strict on the element. The key feature of this function is that it
--  can be used to convert an array to a list by integrating with list fusion using `build`.
--
-- @since 0.3.0
foldrWithFB ::
     (a e -> Size) -- ^ Function that produces the size of an array
  -> (a e -> Int -> e) -- ^ Indexing function
  -> (e -> b -> b) -- ^ Folding functions
  -> b -- ^ Initial accumulator
  -> a e -- ^ Array to fold over
  -> b
foldrWithFB size index c nil a = go 0
  where
    k = coerce (size a)
    go i
      | i >= k = nil
      | otherwise =
        let v = index a i
         in v `seq` (v `c` go (i + 1))
{-# INLINE[0] foldrWithFB #-}

-- | Check for equality of two arrays
--
-- @since 0.3.0
eqWith ::
     Eq e
  => (a e -> a e -> Bool) -- ^ Pointer equality
  -> (a e -> Size) -- ^ Get the size of array
  -> (a e -> Int -> e) -- ^ Index an element of an array
  -> a e -- ^ First array
  -> a e -- ^ Second array
  -> Bool
eqWith isSame sizeOf index a1 a2 = isSame a1 a2 || (sz1 == sizeOf a2 && loop 0)
  where
    sz1@(Size n) = sizeOf a1
    loop i
      | i < n = index a1 i == index a2 i && loop (i + 1)
      | otherwise = True
{-# INLINE eqWith #-}

-- | Check for equality of two mutable arrays
--
-- @since 0.3.0
eqWithST ::
     Eq e
  => (a e s -> a e s -> Bool) -- ^ Pointer equality
  -> (a e s -> ST s Size) -- ^ Get the size of array
  -> (a e s -> Int -> ST s e) -- ^ Read an element from the mutable array
  -> a e s -- ^ First array
  -> a e s -- ^ Second array
  -> ST s Bool
eqWithST isSameMut getSizeOfMut readMut ma1 ma2
  | isSameMut ma1 ma2 = pure True
  | otherwise = do
    sz1@(Size n) <- getSizeOfMut ma1
    sz2 <- getSizeOfMut ma2
    let loop i
          | i < n = do
            eltEq <- (==) <$> readMut ma1 i <*> readMut ma2 i
            if eltEq
              then loop (i + 1)
              else pure False
          | otherwise = pure True
    if sz1 /= sz2
      then pure False
      else loop 0
{-# INLINE eqWithST #-}

-- | Compare two mutable arrays using supplied functions
--
-- @since 1.0.0
compareWithST ::
     Ord e
  => (a e s -> a e s -> Bool) -- ^ Pointer equality
  -> (a e s -> ST s Size) -- ^ Get the size of array
  -> (a e s -> Int -> ST s e) -- ^ Read an element from the mutable array
  -> a e s -- ^ First array
  -> a e s -- ^ Second array
  -> ST s Ordering
compareWithST isSameMut getSizeOfMut readMut ma1 ma2
  | isSameMut ma1 ma2 = pure EQ
  | otherwise = do
      sz1@(Size n) <- getSizeOfMut ma1
      sz2 <- getSizeOfMut ma2
      let loop i
            | i < n = do
              x1 <- readMut ma1 i
              x2 <- readMut ma2 i
              case compare x1 x2 of
                EQ -> loop (i + 1)
                c  -> pure c
            | otherwise = pure $ compare sz1 sz2
      loop 0
{-# INLINE compareWithST #-}

-- | Compare two arrays using supplied functions
--
-- @since 0.3.0
compareWith ::
     Ord e
  => (a e -> a e -> Bool) -- ^ Pointer equality
  -> (a e -> Size) -- ^ Get the size of array
  -> (a e -> Int -> e) -- ^ Index an element of an array
  -> a e -- ^ First array
  -> a e -- ^ Second array
  -> Ordering
compareWith isSame sizeOf index a1 a2
  | isSame a1 a2 = EQ
  | otherwise = loop 0
  where
    sz1 = sizeOf a1
    sz2 = sizeOf a2
    Size n = min sz1 sz2
    loop i
      | i < n = compare (index a1 i) (index a2 i) <> loop (i + 1)
      | otherwise = compare sz1 sz2
{-# INLINE compareWith #-}


-- | Compare two arrays using supplied functions
--
-- @since 1.0.0
liftCompareWith ::
     (forall e. a e -> Size) -- ^ Get the size of array
  -> (forall e. a e -> Int -> e) -- ^ Index an element of an array
  -> (b -> c -> Ordering)
  -> a b -- ^ First array
  -> a c -- ^ Second array
  -> Ordering
liftCompareWith sizeOf index comp a1 a2 = loop 0
  where
    Size n = min (sizeOf a1) (sizeOf a2)
    loop i
      | i < n = comp (index a1 i) (index a2 i) <> loop (i + 1)
      | otherwise = compare (sizeOf a1) (sizeOf a2)
{-# INLINE liftCompareWith #-}


-- | Check for equality of two arrays
--
-- @since 1.0.0
liftEqWith ::
     (forall e. a e -> Size) -- ^ Get the size of array
  -> (forall e. a e -> Int -> e) -- ^ Index an element of an array
  -> (b -> c -> Bool)
  -> a b -- ^ First array
  -> a c -- ^ Second array
  -> Bool
liftEqWith sizeOf index eq a1 a2 = sz1 == sizeOf a2 && loop 0
  where
    sz1@(Size n) = sizeOf a1
    loop i
      | i < n = (index a1 i `eq` index a2 i) && loop (i + 1)
      | otherwise = True
{-# INLINE liftEqWith #-}

-- | Show two arrays
--
-- @since 1.0.0
liftShowsPrecArray :: Foldable f => String -> ([e] -> ShowS) -> Int -> f e -> ShowS
liftShowsPrecArray tyName listShows n arr
  | n > 1 = ('(' :) . inner . (')' :)
  | otherwise = inner
  where
    inner = (tyName ++) . (' ' :) . listShows (F.toList arr)


-- | Append two arrays together using supplied functions
--
-- @since 0.3.0
appendWith ::
     (forall s. Size -> ST s (ma e s))
  -> (forall s. a e -> Int -> ma e s -> Int -> Size -> ST s ())
  -> (forall s. ma e s -> ST s (a e))
  -> (a e -> Size)
  -> a e
  -> a e
  -> a e
appendWith newRaw copy freeze sizeOf a1 a2 =
  runST $ do
    let n1 = sizeOf a1
        n2 = sizeOf a2
    ma <- newRaw (n1 + n2)
    copy a1 0 ma 0 n1
    copy a2 0 ma (coerce n1) n2
    freeze ma
{-# INLINE appendWith #-}


-- | Concat many arrays together using supplied functions
--
-- @since 0.3.0
concatWith ::
     (forall s. Size -> ST s (ma e s))
  -> (forall s. a e -> Int -> ma e s -> Int -> Size -> ST s ())
  -> (forall s. ma e s -> ST s (a e))
  -> (a e -> Size)
  -> [a e]
  -> a e
concatWith newRaw copy freeze sizeOf xs =
  runST $ do
    let as = [(sizeOf a, a) | a <- xs]
        !n = getSum $ foldMap (Sum . fst) as
    ma <- newRaw n
    let load i (sz, a) = (i + coerce sz) <$ copy a 0 ma i sz
    foldM_ load 0 as
    freeze ma
{-# INLINE concatWith #-}


-- | Repeat an array N times and concat them together using supplied functions
--
-- @since 1.0.0
cycleWith ::
     Monoid (a e)
  => (forall s. Size -> ST s (ma e s))
  -> (forall s. a e -> Int -> ma e s -> Int -> Size -> ST s ())
  -> (forall s. ma e s -> ST s (a e))
  -> (a e -> Size)
  -> Int
  -> a e
  -> a e
cycleWith newRaw copy freeze sizeOf k a
  | k <= 0 = mempty
  | otherwise =
    runST $ do
      let sz@(Size n) = sizeOf a
      ma <- newRaw (Size k * sz)
      let load i = when (i < k) $ copy a 0 ma (i * n) sz >> load (i + 1)
      load 0
      freeze ma
{-# INLINE cycleWith #-}
