-- |
-- Module      : Primal.Monad
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Monad
  ( module Primal.Monad.Internal
  , eval
  , evalM
  , NFData
  , deepeval
  , deepevalM
  , whenM
  , unlessM
  -- * Re-export
  , module Control.Monad
  , module Primal.Monad.Raises
  ) where

import GHC.Exts
import Primal.Eval
import Primal.Monad.Internal
import Primal.Monad.Raises
import Control.Monad



-- | Similar to `when`, but condional is supplied in a form of monadic action rather than a
-- pure value.
--
-- @since 0.3.0
whenM :: Monad m => m Bool -> m () -> m ()
whenM m action = m >>= \b -> when b action


-- | Similar to `unless`, but condional is supplied in a form of monadic action rather than a
-- pure value.
--
-- @since 0.3.0
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM m action = m >>= \b -> unless b action
