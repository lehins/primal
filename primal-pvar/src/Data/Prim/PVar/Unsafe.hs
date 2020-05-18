{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Prim.PVar.Unsafe
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.PVar.Unsafe
  ( PVar(..)
  -- * Creation
  , allocPVar
  , allocPinnedPVar
  , allocAlignedPinnedPVar
  -- * Conversion
  , unsafeWithPtrPVar
  , toPtrPVar
  , unsafeToPtrPVar
  , unsafeToForeignPtrPVar
  -- * Reset
  , zeroPVar
  -- * Raw memory operations
  , setPVar#
  )
  where

import Control.Prim.Monad
import Data.Prim.PVar.Internal
import Data.Prim.Class
import Foreign.Prim
import GHC.ForeignPtr


-- | Convert `PVar` into a `ForeignPtr`, very unsafe if not backed by pinned memory.
--
-- @since 0.1.0
unsafeToForeignPtrPVar :: PVar e s -> ForeignPtr e
unsafeToForeignPtrPVar pvar@(PVar mba#) =
  case unsafeToPtrPVar pvar of
    Ptr addr# -> ForeignPtr addr# (PlainPtr (unsafeCoerce# mba#))
{-# INLINE unsafeToForeignPtrPVar #-}



-- | Unsafe to use with unpinned memory
--
-- @since 0.1.0
unsafeWithPtrPVar ::
     (MonadPrim s m, Prim e)
  => PVar e s
  -> (Ptr e -> m b)
  -> m b
unsafeWithPtrPVar pvar f = do
  r <- f $ unsafeToPtrPVar pvar
  r <$ touch pvar
{-# INLINE unsafeWithPtrPVar #-}



-- | Extract the address to the mutable variable, but only if it is backed by pinned
-- memory. It is unsafe because even with pinned memory because it can be deallocated if
-- associated `PVar` goes out of scope. Use `Data.Prim.PVar.withPtrPVar` or
-- `Data.Prim.PVar.toForeignPtr` instead.
--
-- @since 0.1.0
toPtrPVar :: PVar e s -> Maybe (Ptr e)
toPtrPVar pvar
  | isPinnedPVar pvar = Just $ unsafeToPtrPVar pvar
  | otherwise = Nothing
{-# INLINE toPtrPVar #-}

-- | Fill the contents of mutable variable with byte @c@
--
-- @since 0.1.0
setPVar# ::
     (MonadPrim s m, Prim e)
  => PVar e s
  -> Int# -- ^ Byte value to fill the `PVar` with
  -> m ()
setPVar# pvar@(PVar mba#) a# =
  prim_ (setByteArray# mba# 0# (unInt# (sizeOfPVar pvar)) a#)
{-# INLINE setPVar# #-}

-- | Reset contents of a mutable variable to zero.
--
-- @since 0.1.0
zeroPVar :: (MonadPrim s m, Prim e) => PVar e s -> m ()
zeroPVar pvar = setPVar# pvar 0#
{-# INLINE zeroPVar #-}
