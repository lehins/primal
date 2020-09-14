{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Data.Prim.Bytes.ForeignPtr
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Memory.ForeignPtr
  ( PtrAccess(..)
    -- * ForeignPtr
  , ForeignPtr(..)
  , castForeignPtr
  , unsafeForeignPtrToPtr
  , ForeignPtrContents(..)
  -- * Pointer arithmetic
  , plusOffForeignPtr
  , plusByteOffForeignPtr
  , minusOffForeignPtr
  , minusOffRemForeignPtr
  , minusByteOffForeignPtr
  , withForeignPtr
  , withNoHaltForeignPtr
  -- ** PlainPtr
  , mallocPlainForeignPtr
  , mallocCountPlainForeignPtr
  , mallocCountPlainForeignPtrAligned
  , mallocByteCountPlainForeignPtr
  , mallocByteCountPlainForeignPtrAligned
  -- ** With Finalizers
  , finalizeForeignPtr
  -- *** Foreign finalizer
  , FinalizerPtr
  , newForeignPtr
  , newForeignPtr_
  , touchForeignPtr
  , mallocForeignPtr
  , mallocCountForeignPtr
  , mallocCountForeignPtrAligned
  , mallocByteCountForeignPtr
  , mallocByteCountForeignPtrAligned
  , addForeignPtrFinalizer
  -- *** With environment
  , FinalizerEnvPtr
  , newForeignPtrEnv
  , addForeignPtrFinalizerEnv
  -- *** Haskell finalizer
  , newConcForeignPtr
  , addForeignPtrConcFinalizer
  -- * Conversion
  -- ** Bytes
  , toForeignPtrBytes
  , toForeignPtrMBytes
  ) where

import Control.Prim.Monad
import Data.Prim
import Data.Prim.Class
import Data.Prim.Memory.Bytes.Internal (Bytes, MBytes(..), Pinned(..),
                                        toForeignPtrBytes, toForeignPtrMBytes,
                                        withNoHaltPtrBytes, withNoHaltPtrMBytes,
                                        withPtrBytes, withPtrMBytes)
import Data.Prim.Memory.ByteString
import qualified Foreign.ForeignPtr as GHC
import Foreign.Prim
import GHC.ForeignPtr (FinalizerEnvPtr, FinalizerPtr, ForeignPtr(..),
                       ForeignPtrContents(..), castForeignPtr,
                       unsafeForeignPtrToPtr)
import qualified GHC.ForeignPtr as GHC


-- | For memory allocated as pinned it is possible to operate on it with a `Ptr`. Any data
-- type that is backed by such memory can have a `PtrAccess` instance. The simplest way is
-- to convert it to a `ForeignPtr` and other functions will come for free.
class PtrAccess s p where
  -- | Convert to `ForeignPtr`.
  toForeignPtr :: MonadPrim s m => p -> m (ForeignPtr a)

  -- | Apply an action to the raw memory `Ptr` to which the data type point to. Type of data
  -- stored in memory is left ambiguous intentionaly, so that the user can choose how to
  -- treat the memory content.
  withPtrAccess :: MonadPrim s m => p -> (Ptr a -> m b) -> m b
  withPtrAccess p action = toForeignPtr p >>= (`withForeignPtr` action)
  {-# INLINE withPtrAccess #-}

  -- | See this GHC <https://gitlab.haskell.org/ghc/ghc/issues/17746 issue #17746> and
  -- related to it in order to get more insight why this is needed.
  withNoHaltPtrAccess :: (MonadUnliftPrim s m) => p -> (Ptr a -> m b) -> m b
  withNoHaltPtrAccess p f = do
    ForeignPtr addr# ptrContents <- toForeignPtr p
    withAliveUnliftPrim ptrContents $ f (Ptr addr#)
  {-# INLINE withNoHaltPtrAccess #-}

instance PtrAccess s (ForeignPtr a) where
  toForeignPtr = pure . coerce
  {-# INLINE toForeignPtr #-}

-- | Read-only access, but it is not enforced.
instance PtrAccess s ByteString where
  toForeignPtr (PS ps s _) = pure (coerce ps `plusByteOffForeignPtr` Off s)
  {-# INLINE toForeignPtr #-}
  withPtrAccess = withPtrByteString
  {-# INLINE withPtrAccess #-}
  withNoHaltPtrAccess = withNoHaltPtrByteString
  {-# INLINE withNoHaltPtrAccess #-}

instance PtrAccess s (MByteString s) where
  toForeignPtr mbs = toForeignPtr (coerce mbs :: ByteString)
  {-# INLINE toForeignPtr #-}
  withPtrAccess mbs = withPtrByteString (coerce mbs)
  {-# INLINE withPtrAccess #-}
  withNoHaltPtrAccess mbs = withNoHaltPtrByteString (coerce mbs)
  {-# INLINE withNoHaltPtrAccess #-}

-- | Read-only access, but it is not enforced.
instance PtrAccess s (Bytes 'Pin) where
  toForeignPtr = pure . toForeignPtrBytes
  {-# INLINE toForeignPtr #-}
  withPtrAccess = withPtrBytes
  {-# INLINE withPtrAccess #-}
  withNoHaltPtrAccess = withNoHaltPtrBytes
  {-# INLINE withNoHaltPtrAccess #-}

instance PtrAccess s (MBytes 'Pin s) where
  toForeignPtr = pure . toForeignPtrMBytes
  {-# INLINE toForeignPtr #-}
  withPtrAccess = withPtrMBytes
  {-# INLINE withPtrAccess #-}
  withNoHaltPtrAccess = withNoHaltPtrMBytes
  {-# INLINE withNoHaltPtrAccess #-}


-- | Apply an action to the raw pointer. It is unsafe to return the actual pointer back from
-- the action because memory itself might get garbage collected or cleaned up by
-- finalizers.
--
-- It is also important not to run non-terminating actions, because GHC can optimize away
-- the logic that runs after the action and GC will happen before the action get's a chance
-- to finish resulting in corrupt memory. Whenever you have an action that runs an infinite
-- loop or ends in an exception throwing, make sure to use `withNoHaltForeignPtr` instead.
withForeignPtr :: MonadPrim s m => ForeignPtr e -> (Ptr e -> m b) -> m b
withForeignPtr (ForeignPtr addr# ptrContents) f = do
  r <- f (Ptr addr#)
  r <$ touch ptrContents
{-# INLINE withForeignPtr #-}

-- | Same thing as `withForeignPtr` except it should be used for never ending actions. See
-- `withNoHaltPtrAccess` for more information on how this differes from `withForeignPtr`.
--
-- @since 0.1.0
withNoHaltForeignPtr ::
     MonadUnliftPrim s m => ForeignPtr e -> (Ptr e -> m b) -> m b
withNoHaltForeignPtr (ForeignPtr addr# ptrContents) f =
  withAliveUnliftPrim ptrContents $ f (Ptr addr#)
{-# INLINE withNoHaltForeignPtr #-}

-- | Lifted version of `GHC.touchForeignPtr`.
touchForeignPtr :: MonadPrim s m => ForeignPtr e -> m ()
touchForeignPtr (ForeignPtr _ contents) = touch contents

-- | Lifted version of `GHC.newForeignPtr`.
newForeignPtr :: MonadPrim RW m => FinalizerPtr e -> Ptr e -> m (ForeignPtr e)
newForeignPtr fin = liftPrimBase . GHC.newForeignPtr fin

-- | Lifted version of `GHC.newForeignPtrEnv`.
newForeignPtrEnv :: MonadPrim RW m => FinalizerEnvPtr env e -> Ptr env -> Ptr e -> m (ForeignPtr e)
newForeignPtrEnv finEnv envPtr = liftPrimBase . GHC.newForeignPtrEnv finEnv envPtr


-- | Lifted version of `GHC.newForeignPtr_`.
newForeignPtr_ :: MonadPrim RW m => Ptr e -> m (ForeignPtr e)
newForeignPtr_ = liftPrimBase . GHC.newForeignPtr_

-- | Simila to `GHC.mallocForeignPtr`, except it operates on `Prim`, instead of `Storable`.
mallocForeignPtr :: forall e m . (MonadPrim RW m, Prim e) => m (ForeignPtr e)
mallocForeignPtr = mallocCountForeignPtrAligned (1 :: Count e)


-- | Similar to `Foreign.ForeignPtr.mallocForeignPtrArray`, except instead of `Storable` we
-- use `Prim`.
mallocCountForeignPtr :: (MonadPrim RW m, Prim e) => Count e -> m (ForeignPtr e)
mallocCountForeignPtr = liftPrimBase . GHC.mallocForeignPtrBytes . unCountBytes

-- | Just like `mallocCountForeignPtr`, but memory is also aligned according to `Prim` instance
mallocCountForeignPtrAligned :: (MonadPrim RW m, Prim e) => Count e -> m (ForeignPtr e)
mallocCountForeignPtrAligned count =
  liftPrimBase $ GHC.mallocForeignPtrAlignedBytes (coerce count) (alignmentProxy count)

-- | Lifted version of `GHC.mallocForeignPtrBytes`.
mallocByteCountForeignPtr :: MonadPrim RW m => Count Word8 -> m (ForeignPtr e)
mallocByteCountForeignPtr = liftPrimBase . GHC.mallocForeignPtrBytes . coerce

-- | Lifted version of `GHC.mallocForeignPtrAlignedBytes`.
mallocByteCountForeignPtrAligned ::
     MonadPrim RW m
  => Count Word8 -- ^ Number of bytes to allocate
  -> Int -- ^ Alignment in bytes
  -> m (ForeignPtr e)
mallocByteCountForeignPtrAligned count =
  liftPrimBase . GHC.mallocForeignPtrAlignedBytes (coerce count)


-- | Lifted version of `GHC.addForeignPtrFinalizer`
addForeignPtrFinalizer :: MonadPrim RW m => FinalizerPtr e -> ForeignPtr e -> m ()
addForeignPtrFinalizer fin = liftPrimBase . GHC.addForeignPtrFinalizer fin


-- | Lifted version of `GHC.addForeignPtrFinalizerEnv`
addForeignPtrFinalizerEnv ::
     MonadPrim RW m => FinalizerEnvPtr env e -> Ptr env -> ForeignPtr e -> m ()
addForeignPtrFinalizerEnv fin envPtr = liftPrimBase . GHC.addForeignPtrFinalizerEnv fin envPtr


-- | Similar to `GHC.mallocPlainForeignPtr`, except instead of `Storable` we use `Prim` and
-- we are not restricted to `IO`, since finalizers are not possible with `PlaintPtr`
mallocPlainForeignPtr ::
     forall e m s. (MonadPrim s m, Prim e)
  => m (ForeignPtr e)
mallocPlainForeignPtr = mallocCountPlainForeignPtr (1 :: Count e)
{-# INLINE mallocPlainForeignPtr #-}

-- | Similar to `Foreign.ForeignPtr.mallocPlainForeignPtrArray`, except instead of `Storable` we
-- use `Prim`.
mallocCountPlainForeignPtr :: (MonadPrim s m, Prim e) => Count e -> m (ForeignPtr e)
mallocCountPlainForeignPtr = mallocByteCountPlainForeignPtr . toByteCount
{-# INLINE mallocCountPlainForeignPtr #-}

-- | Just like `mallocCountForeignPtr`, but memory is also aligned according to `Prim` instance
mallocCountPlainForeignPtrAligned ::
     forall e m s. (MonadPrim s m, Prim e)
  => Count e
  -> m (ForeignPtr e)
mallocCountPlainForeignPtrAligned c =
  prim $ \s ->
    let a# = alignment# (proxy# :: Proxy# e)
     in case newAlignedPinnedByteArray# (unCountBytes# c) a# s of
          (# s', mba# #) ->
            let addr# = mutableByteArrayContents# mba#
             in (# s', ForeignPtr addr# (PlainPtr (unsafeCoerce# mba#)) #)
{-# INLINE mallocCountPlainForeignPtrAligned #-}

-- | Lifted version of `GHC.mallocForeignPtrBytes`.
mallocByteCountPlainForeignPtr :: MonadPrim s m => Count Word8 -> m (ForeignPtr e)
mallocByteCountPlainForeignPtr (Count (I# c#)) =
  prim $ \s ->
    case newPinnedByteArray# c# s of
      (# s', mba# #) ->
        (# s', ForeignPtr (mutableByteArrayContents# mba#) (PlainPtr (unsafeCoerce# mba#)) #)
{-# INLINE mallocByteCountPlainForeignPtr #-}


-- | Lifted version of `GHC.mallocForeignPtrAlignedBytes`.
mallocByteCountPlainForeignPtrAligned ::
     MonadPrim s m
  => Count Word8 -- ^ Number of bytes to allocate
  -> Int -- ^ Alignment in bytes
  -> m (ForeignPtr e)
mallocByteCountPlainForeignPtrAligned (Count (I# c#)) (I# a#) =
  prim $ \s ->
    case newAlignedPinnedByteArray# c# a# s of
      (# s', mba# #) ->
        (# s', ForeignPtr (mutableByteArrayContents# mba#) (PlainPtr (unsafeCoerce# mba#)) #)
{-# INLINE mallocByteCountPlainForeignPtrAligned #-}



-- | Unlifted version of `GHC.newConcForeignPtr`
newConcForeignPtr :: MonadUnliftPrim RW m => Ptr e -> m () -> m (ForeignPtr e)
newConcForeignPtr ptr fin =
  withRunInPrimBase $ \run -> liftPrimBase (GHC.newConcForeignPtr ptr (run fin))


-- | Unlifted version of `GHC.addForeignPtrConcFinalizer`
addForeignPtrConcFinalizer :: MonadUnliftPrim RW m => ForeignPtr a -> m () -> m ()
addForeignPtrConcFinalizer fp fin =
  withRunInPrimBase $ \run -> liftPrimBase (GHC.addForeignPtrConcFinalizer fp (run fin))

-- | Lifted version of `GHC.finalizeForeignPtr`.
finalizeForeignPtr :: MonadPrim RW m => ForeignPtr e -> m ()
finalizeForeignPtr = liftPrimBase . GHC.finalizeForeignPtr

-- | Advances the given address by the given offset in number of elemeents. This operation
-- does not affect associated finalizers in any way.
--
-- @since 0.1.0
plusOffForeignPtr :: Prim e => ForeignPtr e -> Off e -> ForeignPtr e
plusOffForeignPtr (ForeignPtr addr# content) off =
  ForeignPtr (addr# `plusAddr#` unOffBytes# off) content
{-# INLINE plusOffForeignPtr #-}


-- | Advances the given address by the given offset in bytes. This operation does not
-- affect associated finalizers in any way.
--
-- @since 0.1.0
plusByteOffForeignPtr :: ForeignPtr e -> Off Word8 -> ForeignPtr e
plusByteOffForeignPtr (ForeignPtr addr# content) (Off (I# c#)) =
  ForeignPtr (addr# `plusAddr#` c#) content
{-# INLINE plusByteOffForeignPtr #-}

-- | Find the offset in bytes that is between the two pointers by subtracting one address
-- from another.
--
-- @since 0.1.0
minusByteOffForeignPtr :: ForeignPtr e -> ForeignPtr e -> Off Word8
minusByteOffForeignPtr (ForeignPtr xaddr# _) (ForeignPtr yaddr# _) =
  Off (I# (xaddr# `minusAddr#` yaddr#))
{-# INLINE minusByteOffForeignPtr #-}

-- | Find the offset in number of elements that is between the two pointers by subtracting
-- one address from another and dividing the result by the size of an element.
--
-- @since 0.1.0
minusOffForeignPtr :: Prim e => ForeignPtr e -> ForeignPtr e -> Off e
minusOffForeignPtr (ForeignPtr xaddr# _) (ForeignPtr yaddr# _) =
  fromByteOff (Off (I# (xaddr# `minusAddr#` yaddr#)))
{-# INLINE minusOffForeignPtr #-}

-- | Same as `minusOffForeignPtr`, but will also return the remainder in bytes that is
-- left over.
--
-- @since 0.1.0
minusOffRemForeignPtr :: Prim e => ForeignPtr e -> ForeignPtr e -> (Off e, Off Word8)
minusOffRemForeignPtr (ForeignPtr xaddr# _) (ForeignPtr yaddr# _) =
  fromByteOffRem (Off (I# (xaddr# `minusAddr#` yaddr#)))
{-# INLINE minusOffRemForeignPtr #-}
