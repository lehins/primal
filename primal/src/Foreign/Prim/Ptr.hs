{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
-- |
-- Module      : Foreign.Prim.Ptr
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Foreign.Prim.Ptr
  ( module GHC.Ptr
  , plusOffPtr
  , readPtr
  , readOffPtr
  , readByteOffPtr
  , writePtr
  , writeOffPtr
  , writeByteOffPtr
  , setOffPtr
  , copyPtrToPtr
  , movePtrToPtr
  , freeHaskellFunPtr
  , module X
  , WordPtr(..)
  , ptrToWordPtr
  , wordPtrToPtr
  , IntPtr(..)
  , ptrToIntPtr
  , intPtrToPtr
  -- * Prefetch
  , prefetchPtr0
  , prefetchPtr1
  , prefetchPtr2
  , prefetchPtr3
  , prefetchOffPtr0
  , prefetchOffPtr1
  , prefetchOffPtr2
  , prefetchOffPtr3
  ) where

import Control.Prim.Monad
import Control.Prim.Monad.Unsafe
import Data.Prim
import Data.Prim.Class
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Prim
import qualified Foreign.Ptr as GHC (freeHaskellFunPtr)
import Foreign.Ptr as X hiding (IntPtr, WordPtr, freeHaskellFunPtr, intPtrToPtr,
                         ptrToIntPtr, ptrToWordPtr, wordPtrToPtr)
import GHC.Ptr

setOffPtr ::
     (MonadPrim s m, Prim a)
  => Ptr a -- ^ Chunk of memory to fill
  -> Off a -- ^ Offset in number of elements
  -> Count a -- ^ Number of cells to fill
  -> a -- ^ A value to fill the cells with
  -> m ()
setOffPtr (Ptr addr#) (Off (I# o#)) (Count (I# n#)) a = prim_ (setOffAddr# addr# o# n# a)
{-# INLINE setOffPtr #-}


readOffPtr :: (MonadPrim s m, Prim a) => Ptr a -> Off a -> m a
readOffPtr (Ptr addr#) (Off (I# i#)) = prim (readOffAddr# addr# i#)
{-# INLINE readOffPtr #-}


readByteOffPtr :: (MonadPrim s m, Prim a) => Ptr a -> Off Word8 -> m a
readByteOffPtr ptr (Off i) =
  case ptr `plusPtr` i of
    Ptr addr# -> prim (readOffAddr# addr# 0#)
{-# INLINE readByteOffPtr #-}

writeOffPtr :: (MonadPrim s m, Prim a) => Ptr a -> Off a -> a -> m ()
writeOffPtr (Ptr addr#) (Off (I# i#)) a = prim_ (writeOffAddr# addr# i# a)
{-# INLINE writeOffPtr #-}

writeByteOffPtr :: (MonadPrim s m, Prim a) => Ptr a -> Off Word8 -> a -> m ()
writeByteOffPtr ptr (Off i) a =
  case ptr `plusPtr` i of
    Ptr addr# -> prim_ (writeOffAddr# addr# 0# a)
{-# INLINE writeByteOffPtr #-}

readPtr :: (MonadPrim s m, Prim a) => Ptr a -> m a
readPtr (Ptr addr#) = prim (readOffAddr# addr# 0#)
{-# INLINE readPtr #-}

writePtr :: (MonadPrim s m, Prim a) => Ptr a -> a -> m ()
writePtr (Ptr addr#) a = prim_ (writeOffAddr# addr# 0# a)
{-# INLINE writePtr #-}

plusOffPtr :: Prim a => Ptr a -> Off a -> Ptr a
plusOffPtr (Ptr addr#) off = Ptr (addr# `plusAddr#` fromOff# off)
{-# INLINE plusOffPtr #-}

copyPtrToPtr :: (MonadPrim s m, Prim a) => Ptr a -> Off a -> Ptr a -> Off a -> Count a -> m ()
copyPtrToPtr srcPtr srcOff dstPtr dstOff c =
  unsafeIOToPrim $
  copyBytes
    (dstPtr `plusOffPtr` dstOff)
    (srcPtr `plusOffPtr` srcOff)
    (fromCount c)
{-# INLINE copyPtrToPtr #-}

movePtrToPtr :: (MonadPrim s m, Prim a) => Ptr a -> Off a -> Ptr a -> Off a -> Count a -> m ()
movePtrToPtr (Ptr srcAddr#) srcOff (Ptr dstAddr#) dstOff c =
  unsafeIOToPrim $
  memmoveAddr#
    srcAddr#
    (fromOff# srcOff)
    dstAddr#
    (fromOff# dstOff)
    (fromCount# c)
{-# INLINE movePtrToPtr #-}


prefetchPtr0 :: MonadPrim s m => Ptr a -> m ()
prefetchPtr0 (Ptr b#) = prim_ (prefetchAddr0# b# 0#)
{-# INLINE prefetchPtr0 #-}

prefetchPtr1 :: MonadPrim s m => Ptr a -> m ()
prefetchPtr1 (Ptr b#) = prim_ (prefetchAddr1# b# 0#)
{-# INLINE prefetchPtr1 #-}

prefetchPtr2 :: MonadPrim s m => Ptr a -> m ()
prefetchPtr2 (Ptr b#) = prim_ (prefetchAddr2# b# 0#)
{-# INLINE prefetchPtr2 #-}

prefetchPtr3 :: MonadPrim s m => Ptr a -> m ()
prefetchPtr3 (Ptr b#) = prim_ (prefetchAddr3# b# 0#)
{-# INLINE prefetchPtr3 #-}

prefetchOffPtr0 :: (MonadPrim s m, Prim a) => Ptr a -> Off a -> m ()
prefetchOffPtr0 (Ptr b#) off = prim_ (prefetchAddr0# b# (fromOff# off))
{-# INLINE prefetchOffPtr0 #-}

prefetchOffPtr1 :: (MonadPrim s m, Prim a) => Ptr a -> Off a -> m ()
prefetchOffPtr1 (Ptr b#) off = prim_ (prefetchAddr1# b# (fromOff# off))
{-# INLINE prefetchOffPtr1 #-}

prefetchOffPtr2 :: (MonadPrim s m, Prim a) => Ptr a -> Off a -> m ()
prefetchOffPtr2 (Ptr b#) off = prim_ (prefetchAddr2# b# (fromOff# off))
{-# INLINE prefetchOffPtr2 #-}

prefetchOffPtr3 :: (MonadPrim s m, Prim a) => Ptr a -> Off a -> m ()
prefetchOffPtr3 (Ptr b#) off = prim_ (prefetchAddr3# b# (fromOff# off))
{-# INLINE prefetchOffPtr3 #-}

-- | Same as `GHC.freeHaskellFunPtr`
--
-- @since 0.1.0
freeHaskellFunPtr :: MonadPrim s m => FunPtr a -> m ()
freeHaskellFunPtr = unsafeIOToPrim . GHC.freeHaskellFunPtr
