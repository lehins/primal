-- |
-- Module      : Primal.Mutable.Ord
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Mutable.Ord where

import Primal.Mutable.Eq
import Control.Prim.Monad

class EqMut f => OrdMut f where
  compareMut :: MonadPrim s m => f s -> f s -> m Ordering

