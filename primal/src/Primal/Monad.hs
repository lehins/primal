-- |
-- Module      : Primal.Monad
-- Copyright   : (c) Alexey Kuleshevich 2020-2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module Primal.Monad (
  module Primal.Monad.Internal,
  eval,
  evalM,
  NFData,
  deepeval,
  deepevalM,
  whenM,
  unlessM,

  -- * Re-export
  asIO,
  asST,
  module Control.Monad,
) where

import Control.Monad
import Control.Monad.ST
import Primal.Eval
import Primal.Monad.Internal

-- | Similar to `when`, but condional is supplied in a form of monadic action rather than a
-- pure value.
--
-- @since 0.3.0
whenM :: Monad m => m Bool -> m () -> m ()
whenM m action = m >>= (`when` action)

-- | Similar to `unless`, but condional is supplied in a form of monadic action rather than a
-- pure value.
--
-- @since 0.3.0
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM m action = m >>= (`unless` action)

-- | Restrict the type of a monadic action to `IO`
--
-- @since 1.0.0
asIO :: IO a -> IO a
asIO = id

-- | Restrict the type of a monadic action to `ST`
--
-- @since 1.0.0
asST :: ST s a -> ST s a
asST = id
