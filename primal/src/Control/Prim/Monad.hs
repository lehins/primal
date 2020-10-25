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
  , evaluate
  , keepAlive
  -- * Re-export
  , module Control.Monad
  ) where

import GHC.Exts
import Control.Prim.Eval
import Control.Prim.Monad.Internal
import Control.Monad
