-- |
-- Module      : Control.Prim.Monad
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Control.Prim.Monad
  ( module Control.Prim.Monad.Internal
  , touch
  , seqPrim
  , withPrimBase
  , withUnliftPrim
  , showsType
  ) where

import GHC.Exts
import Control.Prim.Eval
import Control.Prim.Monad.Internal
import Data.Typeable

-- | Helper function that converts a type into a string
showsType :: Typeable t => proxy t -> ShowS
showsType = showsTypeRep . typeRep
