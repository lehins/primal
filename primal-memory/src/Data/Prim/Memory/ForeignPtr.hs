{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
  , PtrAccess(..)
  , withForeignPtr
  , withNoHaltForeignPtr
  , plusForeignPtr
  -- * Conversion
  -- ** Addr
  -- ** Bytes
  , toForeignPtrBytes
  , toForeignPtrMBytes
  ) where

import Control.Prim.Monad
import Data.Prim
import Foreign.Prim
import GHC.ForeignPtr
import Data.Prim.Memory.Bytes.Internal
  ( Bytes
  , MBytes
  , Pinned(..)
  , toForeignPtrBytes
  , toForeignPtrMBytes
  , withNoHaltPtrBytes
  , withNoHaltPtrMBytes
  , withPtrBytes
  , withPtrMBytes
  )
import Data.Prim.Memory.Addr
import Data.Prim.Memory.ByteString


-- | For memory allocated as pinned it is possible to operate on it with a `Ptr`. Any data
-- type that is backed by such memory can have a `PtrAccess` instance. The simplest way is
-- to convert it to a `ForeignPtr` and other functions will come for free.
class PtrAccess s p where
  -- | Convert to `ForeignPtr`.
  toForeignPtr :: MonadPrim s m => p -> m (ForeignPtr a)

  -- | Apply an action the raw memory `Ptr` the data type refers. Type was left ambiguous
  -- intentionaly, so the user can choose how to treat the memory content.
  withPtrAccess :: MonadPrim s m => p -> (Ptr a -> m b) -> m b
  withPtrAccess p action = toForeignPtr p >>= (`withForeignPtr` action)
  {-# INLINE withPtrAccess #-}

  -- | See this GHC Issue[#18061](https://gitlab.haskell.org/ghc/ghc/issues/18061) to learn
  -- why this is needed.
  withNoHaltPtrAccess :: (MonadUnliftPrim s m, PtrAccess s p) => p -> (Ptr a -> m b) -> m b
  withNoHaltPtrAccess p f = do
    ForeignPtr addr# ptrContents <- toForeignPtr p
    withUnliftPrim ptrContents $ f (Ptr addr#)
  {-# INLINE withNoHaltPtrAccess #-}

instance PtrAccess s (ForeignPtr a) where
  toForeignPtr = pure . coerce
  {-# INLINE toForeignPtr #-}

instance PtrAccess s ByteString where
  toForeignPtr (PS ps s _) = pure (coerce ps `plusForeignPtr` s)
  {-# INLINE toForeignPtr #-}

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

instance PtrAccess s (Addr a) where
  toForeignPtr = pure . toForeignPtrAddr . castAddr
  {-# INLINE toForeignPtr #-}
  withPtrAccess addr = withPtrAddr (castAddr addr)
  {-# INLINE withPtrAccess #-}
  withNoHaltPtrAccess addr = withNoHaltPtrAddr (castAddr addr)
  {-# INLINE withNoHaltPtrAccess #-}

instance PtrAccess s (MAddr a s) where
  toForeignPtr = pure . toForeignPtrMAddr . castMAddr
  {-# INLINE toForeignPtr #-}
  withPtrAccess maddr = withPtrMAddr (castMAddr maddr)
  {-# INLINE withPtrAccess #-}
  withNoHaltPtrAccess maddr = withNoHaltPtrMAddr (castMAddr maddr)
  {-# INLINE withNoHaltPtrAccess #-}



withForeignPtr :: MonadPrim s m => ForeignPtr a -> (Ptr a -> m b) -> m b
withForeignPtr (ForeignPtr addr# ptrContents) f = do
  r <- f (Ptr addr#)
  r <$ touch ptrContents
{-# INLINE withForeignPtr #-}

-- | See this GHC Issue[#18061](https://gitlab.haskell.org/ghc/ghc/issues/18061) to learn
-- why this is needed.
withNoHaltForeignPtr ::
     MonadUnliftPrim s m => ForeignPtr a -> (Ptr a -> m b) -> m b
withNoHaltForeignPtr (ForeignPtr addr# ptrContents) f =
  withUnliftPrim ptrContents $ f (Ptr addr#)
{-# INLINE withNoHaltForeignPtr #-}
