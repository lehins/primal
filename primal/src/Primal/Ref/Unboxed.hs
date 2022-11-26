{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

-- |
-- Module      : Primal.Ref.Unboxed
-- Copyright   : (c) Alexey Kuleshevich 2020-2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module Primal.Ref.Unboxed (
  URef (..),

  -- * Create

  -- ** Initialized
  newURef,
  newPinnedURef,
  newAlignedPinnedURef,
  isSameURef,

  -- ** Uninitialized
  newRawURef,
  newRawPinnedURef,
  newRawAlignedPinnedURef,
  zeroURef,

  -- * Read/write
  readURef,
  writeURef,
  writeFetchOldURef,

  -- * Swap
  swapURefs,
  swapURefs_,

  -- * Modify

  -- ** With pure function
  modifyURef,
  modifyURef_,

  -- ** With monadic action
  modifyFetchOldURef,
  modifyFetchNewURef,
  modifyURefM,
  modifyURefM_,
  modifyFetchOldURefM,
  modifyFetchNewURefM,

  -- * Weak Pointer
  mkWeakURef,
) where

import Primal.Element.Unbox
import Primal.Eval
import Primal.Foreign
import Primal.Memory.Weak
import Primal.Monad

-- | Mutable variable that can hold an unboxed value.
--
-- @since 1.0.0
data URef e s = URef (MutableByteArray# s)

-- | Values are already written into `URef` in NF, this instance is trivial.
instance NFData (URef e s) where
  rnf (URef _) = ()

-- | Values are already written into `URef` in NF, this instance is trivial.
instance MutNFData (URef e) where
  rnfMutST (URef _) = pure ()
  {-# INLINE rnfMutST #-}

-- | Uses `isSameURef`
instance Eq (URef e s) where
  (==) = isSameURef
  {-# INLINE (==) #-}

-- | Check whether supplied `URef`s refer to the exact same one or not.
--
-- @since 1.0.0
isSameURef :: URef e s -> URef e s -> Bool
isSameURef (URef ref1#) (URef ref2#) = isTrue# (isSameMutableByteArray# ref1# ref2#)
{-# INLINE isSameURef #-}

-- | Create a mutable variable in unpinned memory (i.e. GC can move it) with an initial
-- value. This is a prefered way to create a mutable variable, since it will not
-- contribute to memory fragmentation. For pinned memory versions see `newPinnedURef` and
-- `newAlignedPinnedURef`
--
-- ==== __Examples__
--
-- >>> import Primal.Ref.Unboxed
-- >>> ref <- newURef (217 :: Int)
-- >>> modifyFetchOldURef ref succ
-- 217
-- >>> readURef ref
-- 218
--
-- @since 1.0.0
newURef :: (Primal s m, Unbox e) => e -> m (URef e s)
newURef v = do
  pvar <- newRawURef
  pvar <$ writeURef pvar v
{-# INLINE newURef #-}

-- | Create a mutable variable in unpinned and unititialized memory
--
-- @since 1.0.0
newRawURef
  :: forall e m s
   . (Primal s m, Unbox e)
  => m (URef e s)
newRawURef =
  primal $ \s# ->
    case newByteArray# (unCountBytes# (1 :: Count e)) s# of
      (# s'#, mba# #) -> (# s'#, URef mba# #)
{-# INLINE newRawURef #-}

-- | Create a mutable variable in pinned memory with an initial value.
--
-- @since 1.0.0
newPinnedURef :: (Primal s m, Unbox e) => e -> m (URef e s)
newPinnedURef e = do
  pvar <- newRawPinnedURef
  pvar <$ writeURef pvar e
{-# INLINE newPinnedURef #-}

-- | Create a mutable variable in pinned memory with uninitialized memory.
--
-- @since 2.0.0
newRawPinnedURef
  :: forall e m s
   . (Primal s m, Unbox e)
  => m (URef e s)
newRawPinnedURef =
  primal $ \s# ->
    case newPinnedByteArray# (unCountBytes# (1 :: Count e)) s# of
      (# s'#, mba# #) -> (# s'#, URef mba# #)
{-# INLINE newRawPinnedURef #-}

-- | Create a mutable variable in pinned memory with an initial value and aligned
-- according to its `alignment`
--
-- @since 1.0.0
newAlignedPinnedURef
  :: forall e m s
   . (Primal s m, Unbox e)
  => e
  -> m (URef e s)
newAlignedPinnedURef v = do
  pvar <- newRawAlignedPinnedURef
  writeURef pvar v
  return pvar
{-# INLINE newAlignedPinnedURef #-}

-- | Create a mutable variable in pinned uninitialized memory.
--
-- @since 1.0.0
newRawAlignedPinnedURef
  :: forall e m s
   . (Primal s m, Unbox e)
  => m (URef e s)
newRawAlignedPinnedURef =
  primal $ \s# ->
    let c# = unCountBytes# (1 :: Count e)
        a# = alignment# (proxy# :: Proxy# e)
     in case newAlignedPinnedByteArray# c# a# s# of
          (# s'#, mba# #) -> (# s'#, URef mba# #)
{-# INLINE newRawAlignedPinnedURef #-}

-- | Reset contents of a mutable variable to zero.
--
-- @since 1.0.0
zeroURef
  :: forall e m s
   . (Primal s m, Unbox e)
  => URef e s
  -> m ()
zeroURef (URef mba#) = primal_ (setByteArray# mba# 0# (unCountBytes# (1 :: Count e)) 0#)
{-# INLINE zeroURef #-}

----------------
-- Read/Write --
----------------

-- | Read contents of the mutable variable
--
-- ==== __Examples__
--
-- >>> import Primal.Ref
-- >>> ref <- newURef (Just 'a')
-- >>> readURef ref
-- Just 'a'
--
-- @since 1.0.0
readURef
  :: forall e m s
   . (Primal s m, Unbox e)
  => URef e s
  -> m e
readURef (URef mba#) = primal (readMutableByteArray# mba# 0#)
{-# INLINE readURef #-}

-- | Swap the contents of a mutable variable with a new value, while retrieving the old one.
--
-- ==== __Examples__
--
-- >>> ref <- newURef (Left 1 :: Either Int Double)
-- >>> writeFetchOldURef ref (Right pi)
-- Left 1
-- >>> readURef ref
-- Right 3.141592653589793
--
-- @since 1.0.0
writeFetchOldURef
  :: forall e m s
   . (Primal s m, Unbox e)
  => URef e s
  -- ^ Mutable variable to write a new value into
  -> e
  -- ^ New value to write into the variable
  -> m e
  -- ^ Returns the old value
writeFetchOldURef ref e = readURef ref <* writeURef ref e
{-# INLINE writeFetchOldURef #-}

-- | Write a value into a mutable unboxed variable.
--
-- ==== __Examples__
--
-- >>> ref <- newURef (0 :: Int)
-- >>> writeURef ref 10
-- >>> readURef ref
-- 10
--
-- @since 1.0.0
writeURef :: (Primal s m, Unbox e) => URef e s -> e -> m ()
writeURef (URef mba#) v = primal_ (writeMutableByteArray# mba# 0# v)
{-# INLINE writeURef #-}

-- | Swap contents of two mutable variables. Returns their old values.
--
-- @since 1.0.0
swapURefs :: forall e m s. (Primal s m, Unbox e) => URef e s -> URef e s -> m (e, e)
swapURefs ref1 ref2 = do
  a1 <- readURef ref1
  a2 <- readURef ref2
  writeURef ref1 a2
  writeURef ref2 a1
  pure (a1, a2)
{-# INLINE swapURefs #-}

-- | Swap contents of two mutable variables.
--
-- @since 1.0.0
swapURefs_ :: forall e m s. (Primal s m, Unbox e) => URef e s -> URef e s -> m ()
swapURefs_ ref1 ref2 = void $ swapURefs ref1 ref2
{-# INLINE swapURefs_ #-}

------------
-- Modify --
------------

-- | Apply a pure function to the contents of a mutable variable strictly. Returns the
-- artifact produced by the modifying function. Artifact is not forced, therfore it cannot
-- affect the outcome of modification.
--
-- @since 1.0.0
modifyURef
  :: forall e a m s
   . (Primal s m, Unbox e)
  => URef e s
  -> (e -> (e, a))
  -> m a
modifyURef ref f = modifyURefM ref (pure . f)
{-# INLINE modifyURef #-}

-- | Apply a pure function to the contents of a mutable variable strictly.
--
-- @since 1.0.0
modifyURef_
  :: forall e m s
   . (Primal s m, Unbox e)
  => URef e s
  -> (e -> e)
  -> m ()
modifyURef_ ref f = modifyURefM_ ref (pure . f)
{-# INLINE modifyURef_ #-}

-- | Apply a pure function to the contents of a mutable variable strictly. Returns the new value.
--
-- @since 1.0.0
modifyFetchNewURef
  :: forall e m s
   . (Primal s m, Unbox e)
  => URef e s
  -> (e -> e)
  -> m e
modifyFetchNewURef ref f = modifyFetchNewURefM ref (pure . f)
{-# INLINE modifyFetchNewURef #-}

-- | Apply a pure function to the contents of a mutable variable strictly. Returns the old value.
--
-- ==== __Examples__
--
-- >>> ref1 <- newURef (10 :: Int)
-- >>> ref2 <- newURef (201 :: Int)
-- >>> modifyURefM_ ref1 (\x -> modifyFetchOldURef ref2 (* x))
-- >>> readURef ref1
-- 201
-- >>> readURef ref2
-- 2010
--
-- @since 1.0.0
modifyFetchOldURef
  :: forall e m s
   . (Primal s m, Unbox e)
  => URef e s
  -> (e -> e)
  -> m e
modifyFetchOldURef ref f = modifyFetchOldURefM ref (pure . f)
{-# INLINE modifyFetchOldURef #-}

-- | Modify value of a mutable variable with a monadic action. It is not strict in a
-- return value of type @b@.
--
-- ==== __Examples__
--
--
-- @since 1.0.0
modifyURefM :: forall e a m s. (Primal s m, Unbox e) => URef e s -> (e -> m (e, a)) -> m a
modifyURefM ref f = do
  (a, b) <- f =<< readURef ref
  b <$ writeURef ref a
{-# INLINE modifyURefM #-}

-- | Modify value of a mutable variable with a monadic action.
--
-- ==== __Examples__
--
-- >>> ref <- newURef (Just True)
-- >>> modifyURefM_ ref $ \ mv -> Nothing <$ mapM_ print mv
-- True
-- >>> readURef ref
-- Nothing
--
-- @since 1.0.0
modifyURefM_
  :: forall e m s
   . (Primal s m, Unbox e)
  => URef e s
  -> (e -> m e)
  -> m ()
modifyURefM_ ref f = readURef ref >>= f >>= writeURef ref
{-# INLINE modifyURefM_ #-}

-- | Apply a monadic action to the contents of a mutable variable strictly. Returns the old value.
--
-- ==== __Examples__
--
-- >>> refTotal <- newURef (5500 :: Int)
-- >>> refMyScore <- newURef (100 :: Int)
-- >>> myOldScore <- modifyFetchOldURefM refMyScore $ \ myScore -> 0 <$ modifyURef_ refTotal (+ myScore)
-- >>> print myOldScore
-- 100
-- >>> print =<< readURef refMyScore
-- 0
-- >>> print =<< readURef refTotal
-- 5600
--
-- @since 1.0.0
modifyFetchOldURefM
  :: forall e m s
   . (Primal s m, Unbox e)
  => URef e s
  -> (e -> m e)
  -> m e
modifyFetchOldURefM ref f = do
  a <- readURef ref
  a <$ (writeURef ref =<< f a)
{-# INLINE modifyFetchOldURefM #-}

-- | Apply a monadic action to the contents of a mutable. Returns the new value.
--
-- @since 1.0.0
modifyFetchNewURefM
  :: forall e m s
   . (Primal s m, Unbox e)
  => URef e s
  -> (e -> m e)
  -> m e
modifyFetchNewURefM ref f = do
  a <- readURef ref
  a' <- f a
  a' <$ writeURef ref a'
{-# INLINE modifyFetchNewURefM #-}

-- | Create a `Weak` pointer associated with the supplied `URef`.
--
-- Same as `Data.IORef.mkWeakRef` from @base@, but for `URef` and works in any `UnliftPrimal`
-- with `RealWorld` state token.
--
-- @since 1.0.0
mkWeakURef
  :: forall a b m
   . UnliftPrimal RW m
  => URef a RW
  -> m b
  -- ^ An action that will get executed whenever `URef` gets garbage collected by
  -- the runtime.
  -> m (Weak (URef a RW))
mkWeakURef ref@(URef ref#) !finalizer =
  runInPrimalState finalizer $ \f# s ->
    case mkWeak# ref# ref f# s of
      (# s', weak# #) -> (# s', Weak weak# #)
{-# INLINE mkWeakURef #-}
