-- |
-- Module      : Primal.Mutable.Eq
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Mutable.Eq where

import Control.Prim.Monad

class EqMut f where
  eqMut :: MonadPrim s m => f s -> f s -> m Bool

