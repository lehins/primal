{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
-- |
-- Module      : Data.Prim.Bytes.ForeignPtr
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Memory.ForeignPtr
  ( ForeignPtr(..)
  , withForeignPtr
  , withNoHaltForeignPtr
  -- * Conversion
  -- ** Addr
  , toForeignPtrAddr
  , toForeignPtrMAddr
  , fromForeignPtrAddr
  , fromForeignPtrMAddr
  -- ** Bytes
  , toForeignPtrBytes
  , toForeignPtrMBytes
  ) where

import Control.Prim.Monad
import Control.Prim.Monad.Unsafe
import Data.Prim
import Foreign.Prim
import GHC.ForeignPtr
import Data.Prim.Memory.Bytes
import Data.Prim.Memory.Addr


withForeignPtr :: MonadPrim s m => ForeignPtr a -> (Ptr a -> m b) -> m b
withForeignPtr (ForeignPtr addr# ptrContents) f = do
  r <- f (Ptr addr#)
  r <$ touch ptrContents
{-# INLINE withForeignPtr #-}


withNoHaltForeignPtr ::
     MonadUnliftPrim s m => ForeignPtr a -> (Ptr a -> m b) -> m b
withNoHaltForeignPtr (ForeignPtr addr# ptrContents) f =
  withUnliftPrim ptrContents $ f (Ptr addr#)
{-# INLINE withNoHaltForeignPtr #-}


toForeignPtrAddr :: Addr a -> ForeignPtr a
toForeignPtrAddr (Addr addr# (Bytes ba#)) = ForeignPtr addr# (PlainPtr (unsafeCoerce# ba#))


toForeignPtrMAddr :: MAddr a s -> ForeignPtr a
toForeignPtrMAddr (MAddr addr# (MBytes mba#)) = ForeignPtr addr# (PlainPtr (unsafeCoerce# mba#))

-- | Discarding the original `ForeignPtr` will trigger finalizers that were attached to
-- it, because `Addr` does not retain any finalizers. This is a unsafe cast therefore
-- modification of `ForeignPtr` will be reflected in resulting immutable `Addr`. Pointer
-- created with @malloc@ cannot be converted to `Addr` and will result in `Nothing`
--
-- @since 0.1.0
fromForeignPtrAddr :: ForeignPtr a -> Maybe (Addr a)
fromForeignPtrAddr (ForeignPtr addr# c) =
  case c of
    PlainPtr mba#    -> Just (Addr addr# (unsafeInlineIO (freezeMBytes (MBytes mba#))))
    MallocPtr mba# _ -> Just (Addr addr# (unsafeInlineIO (freezeMBytes (MBytes mba#))))
    _                -> Nothing

-- | Discarding the original ForeignPtr will trigger finalizers that were attached to it,
-- because `MAddr` does not retain any finalizers. Pointer created with @malloc@ cannot be
-- converted to `MAddr` and will result in `Nothing`
--
-- @since 0.1.0
fromForeignPtrMAddr :: ForeignPtr a -> Maybe (MAddr a s)
fromForeignPtrMAddr (ForeignPtr addr# c) =
  case c of
    PlainPtr mba#    -> Just (MAddr addr# (MBytes (unsafeCoerce# mba#)))
    MallocPtr mba# _ -> Just (MAddr addr# (MBytes (unsafeCoerce# mba#)))
    _                -> Nothing


toForeignPtrBytes :: Bytes 'Pin -> ForeignPtr a
toForeignPtrBytes (Bytes ba#) =
  ForeignPtr (byteArrayContents# ba#) (PlainPtr (unsafeCoerce# ba#))


toForeignPtrMBytes :: MBytes 'Pin s -> ForeignPtr a
toForeignPtrMBytes (MBytes mba#) =
  ForeignPtr (byteArrayContents# (unsafeCoerce# mba#)) (PlainPtr (unsafeCoerce# mba#))
