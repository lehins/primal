{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Primal.Memory.ForeignPtr
-- Copyright   : (c) Alexey Kuleshevich 2020-2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Memory.ForeignPtr
  ( MemPtr(..)
  , withPtrMem
  , withNoHaltPtrMem
  , MemForeignPtr(..)
    -- * ForeignPtr
  , ForeignPtr(..)
  , MForeignPtr(..)
  , castForeignPtr
  , unsafeForeignPtrToPtr
  , withMForeignPtr
  , withNoHaltMForeignPtr
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
  , toMForeignPtrMBytes
  ) where

import qualified Foreign.ForeignPtr as GHC
import GHC.ForeignPtr (FinalizerEnvPtr, FinalizerPtr, ForeignPtr(..),
                       ForeignPtrContents(..), castForeignPtr,
                       unsafeForeignPtrToPtr)
import qualified GHC.ForeignPtr as GHC
import Primal.Eval
import Primal.Foreign
import Primal.Memory.Bytes.Internal
import Primal.Memory.Ptr
import Primal.Monad
import Primal.Element.Unbox


newtype MForeignPtr e s = MForeignPtr (ForeignPtr e)


instance MemWrite (MForeignPtr e) where
  accessMutMemST fptr _ g o = withMForeignPtr fptr $ \(Ptr addr#) -> g addr# o
  {-# INLINE accessMutMemST #-}
  isSameMutMem (MForeignPtr (ForeignPtr a1# _)) (MForeignPtr (ForeignPtr a2# _)) =
    isTrue# (a1# `eqAddr#` a2#)
  {-# INLINE isSameMutMem #-}
  readOffMutMemST fptr i =
    withMForeignPtr fptr $ \ptr -> readOffPtr (castPtr ptr) i
  {-# INLINE readOffMutMemST #-}
  readByteOffMutMemST fptr i =
    withMForeignPtr fptr $ \ptr -> readByteOffPtr (castPtr ptr) i
  {-# INLINE readByteOffMutMemST #-}
  writeOffMutMemST fptr i a =
    withMForeignPtr fptr $ \ptr -> writeOffPtr (castPtr ptr) i a
  {-# INLINE writeOffMutMemST #-}
  writeByteOffMutMemST fptr i a =
    withMForeignPtr fptr $ \ptr -> writeByteOffPtr (castPtr ptr) i a
  {-# INLINE writeByteOffMutMemST #-}
  moveByteOffToPtrMutMemST fsrc srcOff dstPtr dstOff c =
    withMForeignPtr fsrc $ \srcPtr ->
      moveByteOffPtrToPtr (castPtr srcPtr) srcOff dstPtr dstOff c
  {-# INLINE moveByteOffToPtrMutMemST #-}
  moveByteOffToMBytesMutMemST fsrc srcOff dst dstOff c =
    withMForeignPtr fsrc $ \srcPtr ->
      moveByteOffPtrToMBytes (castPtr srcPtr) srcOff dst dstOff c
  {-# INLINE moveByteOffToMBytesMutMemST #-}
  copyByteOffMutMemST src srcOff fdst dstOff c =
    withMForeignPtr fdst $ \dstPtr ->
      copyByteOffToPtrMemST src srcOff (castPtr dstPtr) dstOff c
  {-# INLINE copyByteOffMutMemST #-}
  moveByteOffMutMemST src srcOff fdst dstOff c =
    withMForeignPtr fdst $ \dstPtr ->
      moveByteOffToPtrMutMemST src srcOff (castPtr dstPtr) dstOff c
  {-# INLINE moveByteOffMutMemST #-}
  setByteOffMutMemST fptr off c a =
    withMForeignPtr fptr $ \ptr -> setByteOffPtr (castPtr ptr) off c a
  {-# INLINE setByteOffMutMemST #-}
  setMutMemST fptr off c a =
    withMForeignPtr fptr $ \ptr -> setOffPtr (castPtr ptr) off c a
  {-# INLINE setMutMemST #-}

-- | It is possible to operate on memory that was allocated as pinned with a regular
-- `Ptr`. Any data type that is backed by such memory can have a `MemPtr` instance. The
-- simplest way is to create an instance for `MemForeignPtr` and other functions will come
-- for free.
class MemWrite mp => MemPtr mp where

  -- | Apply an action to the raw memory `Ptr` to which the data type points to. Type of data
  -- stored in memory is left ambiguous intentionaly, so that the user can choose how to
  -- treat the memory content.
  withPtrMemST :: Unbox e => mp s -> (Ptr e -> ST s b) -> ST s b
  default withPtrMemST :: (Unbox e, MemForeignPtr mp) => mp s -> (Ptr e -> ST s b) -> ST s b
  withPtrMemST = withMForeignPtr . toMForeignPtrMem
  {-# INLINE withPtrMemST #-}

  -- | See this GHC <https://gitlab.haskell.org/ghc/ghc/issues/17746 issue #17746> and
  -- related to it in order to get more insight why this is needed.
  withNoHaltPtrMemST :: Unbox e => mp s -> (Ptr e -> ST s b) -> ST s b
  default withNoHaltPtrMemST :: (Unbox e, MemForeignPtr mp) => mp s -> (Ptr e -> ST s b) -> ST s b
  withNoHaltPtrMemST = withNoHaltMForeignPtr . toMForeignPtrMem
  {-# INLINE withNoHaltPtrMemST #-}


-- | Any pinned memory that can be converted to a `ForeignPtr` without copy
class MemPtr mp => MemForeignPtr mp where
  -- | Convert to `ForeignPtr`.
  toMForeignPtrMem :: mp s -> MForeignPtr e s

instance MemPtr (MForeignPtr e)

instance MemForeignPtr (MForeignPtr e) where
  toMForeignPtrMem = coerce
  {-# INLINE toMForeignPtrMem #-}

instance MemForeignPtr (MBytes 'Pin) where
  toMForeignPtrMem = toMForeignPtrMBytes
  {-# INLINE toMForeignPtrMem #-}

instance MemPtr (MBytes 'Pin) where
  withPtrMemST = withPtrMBytes
  {-# INLINE withPtrMemST #-}
  withNoHaltPtrMemST = withNoHaltPtrMBytes
  {-# INLINE withNoHaltPtrMemST #-}


toForeignPtrBytes :: Bytes 'Pin -> ForeignPtr e
toForeignPtrBytes (Bytes ba#) =
  ForeignPtr (byteArrayContents# ba#) (PlainPtr (unsafeCoerce# ba#))
{-# INLINE toForeignPtrBytes #-}


toMForeignPtrMBytes :: MBytes 'Pin s -> MForeignPtr e s
toMForeignPtrMBytes (MBytes mba#) =
  MForeignPtr (ForeignPtr (mutableByteArrayContents# mba#) (PlainPtr (unsafeCoerce# mba#)))
{-# INLINE toMForeignPtrMBytes #-}


-- | Apply an action to the raw memory `Ptr` to which the data type points to. Type of data
-- stored in memory is left ambiguous intentionaly, so that the user can choose how to
-- treat the memory content.
withPtrMem :: (Unbox e, MemPtr mp, UnliftPrimal s m) => mp s -> (Ptr e -> m b) -> m b
withPtrMem mem f =
  withRunInST (\unlift -> withPtrMemST mem $ \ptr -> unlift (f ptr))
{-# INLINE withPtrMem #-}

-- | See this GHC <https://gitlab.haskell.org/ghc/ghc/issues/17746 issue #17746> and
-- related to it in order to get more insight why this is needed.
withNoHaltPtrMem :: (Unbox e, MemPtr mp, UnliftPrimal s m) => mp s -> (Ptr e -> m b) -> m b
withNoHaltPtrMem mem f =
  withRunInST (\unlift -> withNoHaltPtrMemST mem $ \ptr -> unlift (f ptr))
{-# INLINE withNoHaltPtrMem #-}

-- | Apply an action to the raw pointer. It is unsafe to return the actual pointer back from
-- the action because memory itself might get garbage collected or cleaned up by
-- finalizers.
--
-- It is also important not to run non-terminating actions, because GHC can optimize away
-- the logic that runs after the action and GC will happen before the action get's a chance
-- to finish resulting in corrupt memory. Whenever you have an action that runs an infinite
-- loop or ends in an exception throwing, make sure to use `withNoHaltForeignPtr` instead.
withForeignPtr :: Primal RW m => ForeignPtr e -> (Ptr e -> m b) -> m b
withForeignPtr (ForeignPtr addr# ptrContents) f = do
  r <- f (Ptr addr#)
  r <$ touch ptrContents
{-# INLINE withForeignPtr #-}

-- | Same thing as `withForeignPtr` except it should be used for never ending actions. See
-- `withNoHaltPtrAccess` for more information on how this differes from `withForeignPtr`.
--
-- @since 0.1.0
withNoHaltForeignPtr ::
     UnliftPrimal RW m => ForeignPtr e -> (Ptr e -> m b) -> m b
withNoHaltForeignPtr (ForeignPtr addr# ptrContents) f =
  keepAlive ptrContents $ f (Ptr addr#)
{-# INLINE withNoHaltForeignPtr #-}


-- | Apply an action to the raw pointer. It is unsafe to return the actual pointer back from
-- the action because memory itself might get garbage collected or cleaned up by
-- finalizers.
--
-- It is also important not to run non-terminating actions, because GHC can optimize away
-- the logic that runs after the action and GC will happen before the action get's a chance
-- to finish resulting in corrupt memory. Whenever you have an action that runs an infinite
-- loop or ends in an exception throwing, make sure to use `withNoHaltForeignPtr` instead.
withMForeignPtr :: Primal s m => MForeignPtr e s -> (Ptr e -> m b) -> m b
withMForeignPtr (MForeignPtr (ForeignPtr addr# ptrContents)) f = do
  r <- f (Ptr addr#)
  r <$ touch ptrContents
{-# INLINE withMForeignPtr #-}

-- | Same thing as `withMForeignPtr` except it should be used for never ending actions. See
-- `withNoHaltPtrAccess` for more information on how this differes from `withMForeignPtr`.
--
-- @since 1.0.0
withNoHaltMForeignPtr ::
     UnliftPrimal s m => MForeignPtr e s -> (Ptr e -> m b) -> m b
withNoHaltMForeignPtr (MForeignPtr (ForeignPtr addr# ptrContents)) f =
  keepAlive ptrContents $ f (Ptr addr#)
{-# INLINE withNoHaltMForeignPtr #-}



-- | Lifted version of `GHC.touchForeignPtr`.
touchForeignPtr :: Primal s m => ForeignPtr e -> m ()
touchForeignPtr (ForeignPtr _ contents) = touch contents

-- | Lifted version of `GHC.newForeignPtr`.
newForeignPtr :: Primal RW m => FinalizerPtr e -> Ptr e -> m (ForeignPtr e)
newForeignPtr fin = liftP . GHC.newForeignPtr fin

-- | Lifted version of `GHC.newForeignPtrEnv`.
newForeignPtrEnv :: Primal RW m => FinalizerEnvPtr env e -> Ptr env -> Ptr e -> m (ForeignPtr e)
newForeignPtrEnv finEnv envPtr = liftP . GHC.newForeignPtrEnv finEnv envPtr


-- | Lifted version of `GHC.newForeignPtr_`.
newForeignPtr_ :: Primal RW m => Ptr e -> m (ForeignPtr e)
newForeignPtr_ = liftP . GHC.newForeignPtr_

-- | Similar to `GHC.mallocForeignPtr`, except it operates on `Unbox`, instead of `Storable`.
mallocForeignPtr :: forall e m . (Primal RW m, Unbox e) => m (ForeignPtr e)
mallocForeignPtr = mallocCountForeignPtrAligned (1 :: Count e)


-- | Similar to `Foreign.ForeignPtr.mallocForeignPtrArray`, except instead of `Storable` we
-- use `Unbox`.
mallocCountForeignPtr :: (Primal RW m, Unbox e) => Count e -> m (ForeignPtr e)
mallocCountForeignPtr = liftP . GHC.mallocForeignPtrBytes . unCountBytes

-- | Just like `mallocCountForeignPtr`, but memory is also aligned according to `Unbox` instance
mallocCountForeignPtrAligned :: (Primal RW m, Unbox e) => Count e -> m (ForeignPtr e)
mallocCountForeignPtrAligned count =
  liftP $ GHC.mallocForeignPtrAlignedBytes (unCountBytes count) (alignmentProxy count)

-- | Lifted version of `GHC.mallocForeignPtrBytes`.
mallocByteCountForeignPtr :: Primal RW m => Count Word8 -> m (ForeignPtr e)
mallocByteCountForeignPtr = liftP . GHC.mallocForeignPtrBytes . coerce

-- | Lifted version of `GHC.mallocForeignPtrAlignedBytes`.
mallocByteCountForeignPtrAligned ::
     Primal RW m
  => Count Word8 -- ^ Number of bytes to allocate
  -> Int -- ^ Alignment in bytes
  -> m (ForeignPtr e)
mallocByteCountForeignPtrAligned count =
  liftP . GHC.mallocForeignPtrAlignedBytes (coerce count)


-- | Lifted version of `GHC.addForeignPtrFinalizer`
addForeignPtrFinalizer :: Primal RW m => FinalizerPtr e -> ForeignPtr e -> m ()
addForeignPtrFinalizer fin = liftP . GHC.addForeignPtrFinalizer fin


-- | Lifted version of `GHC.addForeignPtrFinalizerEnv`
addForeignPtrFinalizerEnv ::
     Primal RW m => FinalizerEnvPtr env e -> Ptr env -> ForeignPtr e -> m ()
addForeignPtrFinalizerEnv fin envPtr = liftP . GHC.addForeignPtrFinalizerEnv fin envPtr


-- | Similar to `GHC.mallocPlainForeignPtr`, except instead of `Storable` we use `Unbox` and
-- we are not restricted to `IO`, since finalizers are not possible with `PlaintPtr`
mallocPlainForeignPtr ::
     forall e m s. (Primal s m, Unbox e)
  => m (ForeignPtr e)
mallocPlainForeignPtr = mallocCountPlainForeignPtr (1 :: Count e)
{-# INLINE mallocPlainForeignPtr #-}

-- | Similar to `Foreign.ForeignPtr.mallocPlainForeignPtrArray`, except instead of `Storable` we
-- use `Unbox`.
mallocCountPlainForeignPtr :: (Primal s m, Unbox e) => Count e -> m (ForeignPtr e)
mallocCountPlainForeignPtr = mallocByteCountPlainForeignPtr . toByteCount
{-# INLINE mallocCountPlainForeignPtr #-}

-- | Just like `mallocCountForeignPtr`, but memory is also aligned according to `Unbox` instance
mallocCountPlainForeignPtrAligned ::
     forall e m s. (Primal s m, Unbox e)
  => Count e
  -> m (ForeignPtr e)
mallocCountPlainForeignPtrAligned c =
  primal $ \s ->
    let a# = alignment# (proxy# :: Proxy# e)
     in case newAlignedPinnedByteArray# (unCountBytes# c) a# s of
          (# s', mba# #) ->
            let addr# = mutableByteArrayContents# mba#
             in (# s', ForeignPtr addr# (PlainPtr (unsafeCoerce# mba#)) #)
{-# INLINE mallocCountPlainForeignPtrAligned #-}

-- | Lifted version of `GHC.mallocForeignPtrBytes`.
mallocByteCountPlainForeignPtr :: Primal s m => Count Word8 -> m (ForeignPtr e)
mallocByteCountPlainForeignPtr (Count (I# c#)) =
  primal $ \s ->
    case newPinnedByteArray# c# s of
      (# s', mba# #) ->
        (# s', ForeignPtr (mutableByteArrayContents# mba#) (PlainPtr (unsafeCoerce# mba#)) #)
{-# INLINE mallocByteCountPlainForeignPtr #-}


-- | Lifted version of `GHC.mallocForeignPtrAlignedBytes`.
mallocByteCountPlainForeignPtrAligned ::
     forall e m s. (Primal s m, Unbox e)
  => Count Word8 -- ^ Number of bytes to allocate
  -> m (ForeignPtr e)
mallocByteCountPlainForeignPtrAligned (Count (I# c#)) =
  primal $ \s ->
     let a# = alignment# (proxy# :: Proxy# e)
     in case newAlignedPinnedByteArray# c# a# s of
      (# s', mba# #) ->
        (# s', ForeignPtr (mutableByteArrayContents# mba#) (PlainPtr (unsafeCoerce# mba#)) #)
{-# INLINE mallocByteCountPlainForeignPtrAligned #-}



-- | Unlifted version of `GHC.newConcForeignPtr`
newConcForeignPtr :: UnliftPrimal RW m => Ptr e -> m () -> m (ForeignPtr e)
newConcForeignPtr ptr fin =
  withRunInIO $ \run -> liftP (GHC.newConcForeignPtr ptr (run fin))


-- | Unlifted version of `GHC.addForeignPtrConcFinalizer`
addForeignPtrConcFinalizer :: UnliftPrimal RW m => ForeignPtr a -> m () -> m ()
addForeignPtrConcFinalizer fp fin =
  withRunInIO $ \run -> liftP (GHC.addForeignPtrConcFinalizer fp (run fin))

-- | Lifted version of `GHC.finalizeForeignPtr`.
finalizeForeignPtr :: Primal RW m => ForeignPtr e -> m ()
finalizeForeignPtr = liftP . GHC.finalizeForeignPtr

-- | Advances the given address by the given offset in number of elemeents. This operation
-- does not affect associated finalizers in any way.
--
-- @since 0.1.0
plusOffForeignPtr :: Unbox e => ForeignPtr e -> Off e -> ForeignPtr e
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
minusOffForeignPtr :: Unbox e => ForeignPtr e -> ForeignPtr e -> Off e
minusOffForeignPtr (ForeignPtr xaddr# _) (ForeignPtr yaddr# _) =
  fromByteOff (Off (I# (xaddr# `minusAddr#` yaddr#)))
{-# INLINE minusOffForeignPtr #-}

-- | Same as `minusOffForeignPtr`, but will also return the remainder in bytes that is
-- left over.
--
-- @since 0.1.0
minusOffRemForeignPtr :: Unbox e => ForeignPtr e -> ForeignPtr e -> (Off e, Off Word8)
minusOffRemForeignPtr (ForeignPtr xaddr# _) (ForeignPtr yaddr# _) =
  fromByteOffRem (Off (I# (xaddr# `minusAddr#` yaddr#)))
{-# INLINE minusOffRemForeignPtr #-}
