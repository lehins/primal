{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
-- |
-- Module      : Control.Monad.Prim
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Control.Monad.Prim
  ( module Control.Monad.Prim.Internal
  , touch
  , seqPrim
  , RealWorld
  , showsType
  ) where

import GHC.Exts
import Control.Monad.Prim.Internal
import Control.Monad.Prim.Unsafe
import Data.Typeable

-- | Create an action that ensures that the value is still available and garbage collector has
-- not cleaned it up.
touch :: MonadPrim s m => a -> m ()
touch x = unsafeIOToPrim $ prim_ (touch# x)
{-# INLINE touch #-}

-- | Create an action that evaluates a value to weak head normal form. Same
-- as `Control.Exception.evaluate`, except it work in a `MonadPrim`
seqPrim :: MonadPrim s m => a -> m a
seqPrim a = prim (seq# a)

-- | Helper function that convert the type to a string
showsType :: Typeable t => proxy t -> ShowS
showsType = showsTypeRep . typeRep
