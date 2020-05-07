{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Prim.Array.Unboxed.Ragged
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Array.Unboxed.Ragged
  ( Array
  , pattern Array
  , RaggedArray
  , MArray
  , pattern MArray
  , RaggedMArray
  -- , Size(..)
  -- -- * Immutable
  -- , makeArray
  -- , makeArrayM
  -- , sizeOfArray
  -- , indexArray
  -- -- * Mutable
  -- -- ** Create
  -- , newMArray
  -- , newRawMArray
  -- , newMArrayLazy
  -- , makeMArray
  -- , createArrayM
  -- , createArrayM_
  -- , sizeOfMArray
  -- -- ** Access
  -- , readMArray
  -- , writeMArray
  -- , writeMArrayLazy
  -- , writeMArrayDeep
  -- -- *** Atomic
  -- , casMArray
  -- , atomicModifyFetchMArray
  -- , atomicFetchModifyMArray
  -- , atomicModifyMArray
  -- , atomicModifyMArray_
  -- , atomicModifyMArray2
  -- -- *
  -- , thawArray
  -- , thawCopyArray
  -- , freezeMArray
  -- , freezeCopyMArray
  -- , copyArray
  -- , moveMArray
  -- , cloneArray
  -- , cloneMArray
  -- -- * List
  -- , fromListArray
  -- , fromListArrayN
  -- , toListArray
  -- -- * Helpers
  -- , foldrArray
  -- , traverseArray
  ) where

import Control.DeepSeq
import Control.Exception (ArrayException(UndefinedElement), throw)
import Control.Monad.ST
import Control.Prim.Monad
import Data.Prim.Array.Internal (Size(..))
import qualified Data.Prim.Array.Unboxed as U
import qualified Data.Prim.Array.Internal as I
import Foreign.Prim
import GHC.TypeLits

data RaggedMArray (n :: Nat) a s = MArray (MutableArrayArray# s)

type MArray n a s = RaggedMArray n a s


type Array n a = RaggedArray n a


-- -- | Check if both of the arrays refer to the exact same one. None of the elements are
-- -- evaluated.
-- instance Eq (RaggedMArray a s) where
--   MArray ma1# == MArray ma2# = isTrue# (sameMutableArray# ma1# ma2#)

data RArray (n :: Nat) a where
  UArray :: ArrayArray# -> RArray 0 (U.Array a)
  RArray :: ArrayArray# -> RArray n (RArray (n-1) a)


#if __GLASGOW_HASKELL__ >= 800
data RaggedArray (n :: Nat) a = Array ArrayArray#

-- instance Functor RaggedArray where
--   fmap f a = runST $ traverseArray (pure . f) a

instance I.MArray (RaggedMArray 0) (U.Array a) where
  type IArray (RaggedMArray 0) = RaggedArray 0
#else
type RaggedArray n a = I.IArray (RaggedMArray n) a

instance I.MArray (RaggedMArray o) (U.Array a) where
  data IArray (RaggedMArray 0) = Array ArrayArray#
#endif
  indexArray = indexUnboxedArray
  {-# INLINE indexArray #-}
  sizeOfArray = sizeOfArray
  {-# INLINE sizeOfArray #-}
  getSizeOfMArray = pure . sizeOfMArray
  {-# INLINE getSizeOfMArray #-}
  thawArray = thawArray
  {-# INLINE thawArray #-}
  freezeMArray = freezeMArray
  {-# INLINE freezeMArray #-}
  newRawMArray = newRawMArray
  {-# INLINE newRawMArray #-}
  readMArray = readUnboxedFrozenMArray
  {-# INLINE readMArray #-}
  writeMArray = writeUnboxedFrozenMArray
  {-# INLINE writeMArray #-}
  copyArray = copyArray
  {-# INLINE copyArray #-}
  moveMArray = moveMArray
  {-# INLINE moveMArray #-}



sizeOfArray :: Array n a -> Size
sizeOfArray (Array a#) = Size (I# (sizeofArrayArray# a#))
{-# INLINE sizeOfArray #-}

indexUnboxedArray :: Array 0 b -> Int -> U.Array a
indexUnboxedArray (Array a#) (I# i#) = U.Array (indexByteArrayArray# a# i#)
{-# INLINE indexUnboxedArray #-}

indexArray :: (KnownNat n, CmpNat n 0 ~ 'GT) => Array n a -> Int -> Array (n - 1) a
indexArray (Array a#) (I# i#) = Array (indexArrayArrayArray# a# i#)
{-# INLINE indexArray #-}

-- |
--
-- @since 0.1.0
newRawMArray :: MonadPrim s m => Size -> m (MArray n a s)
newRawMArray (Size (I# n#)) =
  prim $ \s ->
    case newArrayArray# n# s of
      (# s', ma# #) -> (# s', MArray ma# #)
{-# INLINE newRawMArray #-}


thawArray :: MonadPrim s m => Array n a -> m (MArray n a s)
thawArray (Array a#) =
  prim $ \s ->
    case unsafeThawArrayArray# a# s of
      (# s', ma# #) -> (# s', MArray ma# #)
{-# INLINE thawArray #-}

freezeMArray :: MonadPrim s m => MArray n a s -> m (Array n a)
freezeMArray (MArray ma#) =
  prim $ \s ->
    case unsafeFreezeArrayArray# ma# s of
      (# s', a# #) -> (# s', Array a# #)
{-# INLINE freezeMArray #-}


-- | Get the size of a mutable boxed array
--
-- >>> ma <- newMArray 1024 "Element of each cell"
-- >>> sizeOfMArray ma
-- Size 1024
--
-- @since 0.1.0
sizeOfMArray :: MArray n a s -> Size
sizeOfMArray (MArray ma#) = Size (I# (sizeofMutableArrayArray# ma#))
{-# INLINE sizeOfMArray #-}


-- | Read an element from mutable boxed array at a supplied index.
--
-- [Unsafe index] Negative or larger than array size can fail with unchecked exception
--
-- ==== __Examples__
--
-- >>> ma <- makeMArray 10 (pure . ("Element ix: " ++) . show)
-- >>> readMArray ma 5
-- "Element ix: 5"
--
-- @since 0.1.0
readMArray :: (KnownNat n, CmpNat n 0 ~ 'GT) => MonadPrim s m =>
  MArray n a s -> Int -> m (MArray (n - 1) a s)
readMArray (MArray ma#) (I# i#) =
  prim $ \s ->
    case readMutableArrayArrayArray# ma# i# s of
      (# s', ma'# #) -> (# s', MArray ma'# #)
{-# INLINE readMArray #-}

readFrozenMArray :: (KnownNat n, CmpNat n 0 ~ 'GT) => MonadPrim s m =>
  MArray n a s -> Int -> m (Array (n - 1) a)
readFrozenMArray (MArray ma#) (I# i#) =
  prim $ \s ->
    case readArrayArrayArray# ma# i# s of
      (# s', a# #) -> (# s', Array a# #)
{-# INLINE readFrozenMArray #-}

readUnboxedMArray :: MonadPrim s m => MArray 0 a s -> Int -> m (U.MArray a s)
readUnboxedMArray (MArray ma#) (I# i#) =
  prim $ \s ->
    case readMutableByteArrayArray# ma# i# s of
      (# s', mba# #) -> (# s', U.MArray mba# #)
{-# INLINE readUnboxedMArray #-}

readUnboxedFrozenMArray :: MonadPrim s m => MArray 0 b s -> Int -> m (U.Array a)
readUnboxedFrozenMArray (MArray ma#) (I# i#) =
  prim $ \s ->
    case readByteArrayArray# ma# i# s of
      (# s', ba# #) -> (# s', U.Array ba# #)
{-# INLINE readUnboxedFrozenMArray #-}

-- | Write an element into a mutable boxed array at a supplied index strictly. An
-- element will be evaluated to WHNF.
--
-- [Unsafe index] Negative or larger than array size can fail with unchecked exception
--
-- ==== __Examples__
--
-- >>> ma <- newMArray 4 (Nothing :: Maybe Int)
-- >>> writeMArray ma 2 (Just 2)
-- >>> freezeMArray ma
-- Array [Nothing,Nothing,Just 2,Nothing]
--
-- Important to note that an element is evaluated prior to being written into a cell, so
-- it will not overwrite a value with if it evaluates to an exception:
--
-- >>> import Control.Exception
-- >>> writeMArray ma 2 (throw DivideByZero)
-- *** Exception: divide by zero
-- >>> freezeMArray ma
-- Array [Nothing,Nothing,Just 2,Nothing]
--
-- But it is evaluated to Normal Form, so it is still possible to write something that
-- eventually evaluates to bottom.
--
-- >>> writeMArray ma 3 (Just (7 `div` 0 ))
-- >>> freezeMArray ma
-- Array [Nothing,Nothing,Just 2,Just *** Exception: divide by zero
--
-- Either `deepseq` or `writeMArrayDeep` can be used to alleviate that.
--
-- @since 0.1.0
writeMArray ::
     (KnownNat n, CmpNat n 0 ~ 'GT, MonadPrim s m)
  => MArray n a s
  -> Int
  -> MArray (n - 1) a s
  -> m ()
writeMArray (MArray ma#) (I# i#) (MArray e#) =
  prim_ (writeMutableArrayArrayArray# ma# i# e#)
{-# INLINE writeMArray #-}

writeFrozenMArray ::
     (KnownNat n, CmpNat n 0 ~ 'GT, MonadPrim s m)
  => MArray n a s
  -> Int
  -> Array (n - 1) a
  -> m ()
writeFrozenMArray (MArray ma#) (I# i#) (Array e#) =
  prim_ (writeArrayArrayArray# ma# i# e#)
{-# INLINE writeFrozenMArray #-}

writeUnboxedMArray ::
     MonadPrim s m => MArray n a s -> Int -> U.MArray a s -> m ()
writeUnboxedMArray (MArray ma#) (I# i#) (U.MArray mba#) =
  prim_ (writeMutableByteArrayArray# ma# i# mba#)
{-# INLINE writeUnboxedMArray #-}


writeUnboxedFrozenMArray ::
     MonadPrim s m => MArray 0 b s -> Int -> U.Array a -> m ()
writeUnboxedFrozenMArray (MArray ma#) (I# i#) (U.Array ba#) =
  prim_ (writeByteArrayArray# ma# i# ba#)
{-# INLINE writeUnboxedFrozenMArray #-}


-- | Copy a subsection of an immutable array into a subsection of another mutable array.
--
-- [Unsafe overlap] The two arrays must not be the same array in different states
--
-- [Unsafe offset] Each offset cannot be negative or larger than the size of a
-- corresponding array, otherwise it can result in an unchecked exception
--
-- [Unsafe new size] Number of elements to be copied cannot be larger than the size of an
-- each array minus their corersponding offsets.
--
-- @since 0.1.0
copyArray ::
     MonadPrim s m
  => Array n a -- ^ Source immutable array
  -> Int -- ^ Offset into the source immutable array
  -> MArray n a s -- ^ Destination mutable array
  -> Int -- ^ Offset into the destination mutable array
  -> Size -- ^ Number of elements to copy over
  -> m ()
copyArray (Array src#) (I# srcOff#) (MArray dst#) (I# dstOff#) (Size (I# n#)) =
  prim_ (copyArrayArray# src# srcOff# dst# dstOff# n#)
{-# INLINE copyArray #-}

-- | Copy a subsection of a mutable array into a subsection of another or the same
-- mutable array. Therefore, unlike `copyArray`, memory overlap is allowed.
--
-- [Unsafe offset] Each offset cannot be negative or larger than the size of a
-- corresponding array, otherwise it can result in an unchecked exception
--
-- [Unsafe new size] Number of elements to be copied cannot be larger than the size of an
-- each array minus their corersponding offsets.
--
-- @since 0.1.0
moveMArray ::
     MonadPrim s m
  => MArray n a s -- ^ Source mutable array
  -> Int -- ^ Offset into the source mutable array
  -> MArray n a s -- ^ Destination mutable array
  -> Int -- ^ Offset into the destination mutable array
  -> Size -- ^ Number of elements to copy over
  -> m ()
moveMArray (MArray src#) (I# srcOff#) (MArray dst#) (I# dstOff#) (Size (I# n#)) =
  prim_ (copyMutableArrayArray# src# srcOff# dst# dstOff# n#)
{-# INLINE moveMArray #-}


-- -- | Compare-and-swap operation that can be used as a concurrency primitive for
-- -- implementing atomic operations on the mutable array. Returns a boolean value, which
-- -- indicates `True` for success and `False` otherwise for the update, as well as the
-- -- current value at the supplied index. In case of success current value returned will
-- -- be the newly supplied one, otherwise it will still be the old one. Note that there is
-- -- no `Eq` constraint on the element, that is because compare operation is done on a
-- -- thunk reference reference, not the value itself, in other words the expected value
-- -- must be the exact same one.
-- --
-- -- [Unsafe index] Negative or larger than array size can fail with unchecked exception
-- --
-- -- ====__Examples__
-- --
-- -- >>> ma <- makeMArray 5 (pure . (*10))
-- -- >>> freezeMArray ma
-- -- Array [0,10,20,30,40]
-- --
-- -- A possible mistake is to try and pass the expected value, instead of an actual element:
-- --
-- -- >>> casMArray ma 2 20 1000
-- -- (False,20)
-- -- >>> freezeMArray ma
-- -- Array [0,10,20,30,40]
-- --
-- -- But this will get us nowhere, since what we really need is the actual reference to the
-- -- value currently in the array cell
-- --
-- -- >>> expected <- readMArray ma 2
-- -- >>> r@(_, currentValue) <- casMArray ma 2 expected 1000
-- -- >>> freezeMArray ma
-- -- Array [0,10,1000,30,40]
-- -- >>> r
-- -- (True,1000)
-- --
-- -- In a concurrent setting current value can potentially be modified by some other
-- -- thread, therefore returned value can be immedieately used as the expected one to the
-- -- next call, if we don want to retry the atomic modification:
-- --
-- -- >>> casMArray ma 2 currentValue 2000
-- -- (True,2000)
-- -- >>> freezeMArray ma
-- -- Array [0,10,2000,30,40]
-- --
-- -- @since 0.1.0
-- casMArray ::
--      MonadPrim s m
--   => MArray a s -- ^ Mutable array to mutate
--   -> Int -- ^ Index at which the cell should be set to the new value
--   -> a -- ^ Reference to the expected boxed value
--   -> a -- ^ New value to update the cell with
--   -> m (Bool, a)
-- casMArray (MArray ma#) (I# i#) expected new =
--   prim $ \s ->
--     case casArray# ma# i# expected new s of
--       (# s', failed#, actual #) -> (# s', (isTrue# (failed# ==# 0#), actual) #)
-- {-# INLINE casMArray #-}


-- atomicModifyMArray# :: MonadPrim s m => MArray a s -> Int -> (a -> (# a, b #)) -> m b
-- atomicModifyMArray# ma@(MArray ma#) i@(I# i#) f = do
--   current0 <- readMArray ma i
--   prim $
--     let go expected s =
--           case f expected of
--             (# new, artifact #) ->
--               case casArray# ma# i# expected new s of
--                 (# s', 0#, _ #)     -> (# s', artifact #)
--                 (# s', _, actual #) -> go actual s'
--      in go current0
-- {-# INLINE atomicModifyMArray# #-}


-- atomicModifyFetchMArray :: MonadPrim s m => MArray a s -> Int -> (a -> a) -> m a
-- atomicModifyFetchMArray ma i f =
--   atomicModifyMArray# ma i (\a -> let a' = f a in (# a', a' #))
-- {-# INLINE atomicModifyFetchMArray #-}

-- -- atomicModifyFetchMArray ma@(MArray ma#) i@(I# i#) f = do
-- --   current0 <- readMArray ma i
-- --   prim $ \s0 ->
-- --     let go expected s =
-- --           case casArray# ma# i# expected (f expected) s of
-- --             (# s', 0#, actual #) -> go actual s'
-- --             (# s', _, current #) -> (# s', current #)
-- --     in go current0 s0
--   -- let go e =
--   --       casMArray ma i e (f e) >>= \case
--   --         (True, new) -> pure new
--   --         (_, current) -> go current
--   --  in readMArray ma i >>= go

-- atomicFetchModifyMArray :: MonadPrim s m => MArray a s -> Int -> (a -> a) -> m a
-- atomicFetchModifyMArray ma i f =
--   atomicModifyMArray# ma i (\a -> (# f a, a #))
-- {-# INLINE atomicFetchModifyMArray #-}
--   -- let go e =
--   --       casMArray ma i e (f e) >>= \case
--   --         (True, _new) -> pure e
--   --         (_, current) -> go current
--   --  in readMArray ma i >>= go



-- atomicModifyMArray :: MonadPrim s m => MArray a s -> Int -> (a -> (a, b)) -> m b
-- atomicModifyMArray ma i f =
--   atomicModifyMArray# ma i (\a -> let (a', b) = f a in (# a', b #))
-- {-# INLINE atomicModifyMArray #-}
--   -- let go e =
--   --       let (new, artifact) = f e
--   --        in casMArray ma i e new >>= \case
--   --             (True, _new) -> pure artifact
--   --             (_, current) -> go current
--   --  in readMArray ma i >>= go


-- atomicModifyMArray_ :: MonadPrim s m => MArray a s -> Int -> (a -> a) -> m ()
-- atomicModifyMArray_ ma i f =
--   atomicModifyMArray# ma i (\a -> let a' = f a in (# a', () #))
-- {-# INLINE atomicModifyMArray_ #-}


-- atomicModifyMArray2 :: MonadPrim s m => MArray a s -> Int -> (a -> (a, b)) -> m (a, a, b)
-- atomicModifyMArray2 ma i f =
--   atomicModifyMArray# ma i (\a -> let (a', b) = f a in (# a', (a, a', b) #))
-- {-# INLINE atomicModifyMArray2 #-}


-- -- | Convert a list into an array strictly, i.e. each element is evaluated to WHNF prior
-- -- to being written into the newly created array. In order to allocate the array ahead
-- -- of time, the spine of a list will be evaluated first, in order to get the total
-- -- number of elements. Infinite lists will cause the program to halt. On the other hand
-- -- if the length of a list is known ahead of time, `fromListArrayN` can be used instead as
-- -- optimization.
-- --
-- -- @since 0.1.0
-- fromListArray :: [a] -> Array a
-- fromListArray xs = fromListArrayN (Size (length xs)) xs
-- {-# INLINE fromListArray #-}

-- -- | Same as `fromListArray`, except it will allocate an array exactly of @n@ size, as
-- -- such it will not convert any portion of the list that doesn't fit into the newly
-- -- created array.
-- --
-- -- [Unsafe size] if the length of supplied list is actually smaller then the expected
-- -- size, thunks with `UndefinedElement` will be left in the tail of the array.
-- --
-- -- ====__Examples__
-- --
-- -- >>> fromListArrayN 3 [1 :: Int, 2, 3]
-- -- Array [1,2,3]
-- -- >>> fromListArrayN 3 [1 :: Int ..]
-- -- Array [1,2,3]
-- -- >>> fromListArrayN 3 [1 :: Int, 2]
-- -- Array [1,2*** Exception: undefined array element: Data.Prim.Array.Unboxed.Ragged.uninitialized
-- --
-- -- @since 0.1.0
-- fromListArrayN ::
--      Size -- ^ Expected @n@ size of a list
--   -> [a]
--   -> Array a
-- fromListArrayN = I.fromListArrayN
-- {-# INLINE fromListArrayN #-}

-- -- | Convert a pure boxed array into a list. It should work fine with GHC built-in list
-- -- fusion.
-- --
-- -- @since 0.1.0
-- toListArray :: Array a -> [a]
-- toListArray = I.toListArray
-- {-# INLINE toListArray #-}

-- -- | Strict right fold
-- foldrArray :: (a -> b -> b) -> b -> Array a -> b
-- foldrArray = I.foldrArray
-- {-# INLINE foldrArray #-}

-- makeArray :: Size -> (Int -> a) -> Array a
-- makeArray = I.makeArray
-- {-# INLINE makeArray #-}

-- makeArrayM :: MonadPrim s m => Size -> (Int -> m a) -> m (Array a)
-- makeArrayM = I.makeArrayM
-- {-# INLINE makeArrayM #-}

-- createArrayM :: MonadPrim s m => Size -> (MArray a s -> m b) -> m (b, Array a)
-- createArrayM = I.createArrayM
-- {-# INLINE createArrayM #-}

-- createArrayM_ :: MonadPrim s m => Size -> (MArray a s -> m b) -> m (Array a)
-- createArrayM_ = I.createArrayM_
-- {-# INLINE createArrayM_ #-}


-- -- | Create a new mutable array of a supplied size by applying a monadic action to indices
-- -- of each one of the new elements.
-- --
-- -- [Unsafe size] Negative or too large of an array size can kill the current thread with
-- -- `HeapOverflow` asynchronous exception.
-- --
-- -- ====__Examples__
-- --
-- -- >>> import Control.Monad ((>=>))
-- -- >>> import Data.Prim.Ref
-- -- >>> ref <- newRef "Numbers: "
-- -- >>> ma <- makeMArray 5 $ \i -> modifyFetchRef ref (\cur -> cur ++ show i ++ ",")
-- -- >>> mapM_ (readMArray ma >=> putStrLn) [0 .. 4]
-- -- Numbers: 0,
-- -- Numbers: 0,1,
-- -- Numbers: 0,1,2,
-- -- Numbers: 0,1,2,3,
-- -- Numbers: 0,1,2,3,4,
-- --
-- -- @since 0.1.0
-- makeMArray :: MonadPrim s m => Size -> (Int -> m a) -> m (MArray a s)
-- makeMArray = I.makeMArray
-- {-# INLINE makeMArray #-}

-- -- | Traverse an array with a monadic action.
-- --
-- -- @since 0.1.0
-- traverseArray :: MonadPrim s m => (a -> m b) -> Array a -> m (Array b)
-- traverseArray = I.traverseArray
-- {-# INLINE traverseArray #-}
