-- |
-- Module      : Primal.Data.Mutable.Eq
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Data.Mutable.Eq where

import Primal.Monad

class EqMut f where
  eqMut :: MonadPrim s m => f s -> f s -> m Bool

