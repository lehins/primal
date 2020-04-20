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
  , RealWorld
  , showsType
  ) where

import GHC.Exts
import Control.Monad.Prim.Internal
import Control.Monad.Prim.Unsafe
import Data.Typeable

-- | Helper function that converts a type into a string
showsType :: Typeable t => proxy t -> ShowS
showsType = showsTypeRep . typeRep
