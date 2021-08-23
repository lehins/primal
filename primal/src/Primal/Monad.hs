-- |
-- Module      : Primal.Monad
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
{-# LANGUAGE RankNTypes #-}

module Primal.Monad
  ( module Primal.Monad.Internal
  , eval
  , evalM
  , NFData
  , deepeval
  , deepevalM
  , whenM
  , unlessM
  -- * ST
  , tryST
  , tryAllST
  , catchST
  , catchAllST
  -- * Re-export
  , module Control.Monad
  , module Primal.Monad.Raises
  ) where

import Control.Monad
import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Primal.Eval
import Primal.Exception
import Primal.Monad.Internal
import Primal.Monad.Raises



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


-- | Same as `catchSync`, but for `ST`
--
-- @since 1.0.0
catchST ::
     forall e a. Exception e
  => (forall s. ST s a)
  -> (forall s. e -> ST s a)
  -> a
catchST action handle =
  runST (unsafeIOToST (catchSync (unsafeSTToIO action) (unsafeSTToIO . handle)))

-- | Same as `catchSync`, but for `ST`
--
-- @since 1.0.0
catchAllST ::
     forall a. (forall s. ST s a) -> (forall s. SomeException -> ST s a) -> a
catchAllST action handle =
  runST (unsafeIOToST (catchAllSync (unsafeSTToIO action) (unsafeSTToIO . handle)))

-- | Same as `trySync`, but for `ST`. Note that successfully returned value
-- Right can still be bottom.
--
-- @since 1.0.0
tryST ::
     forall e a. Exception e
  => (forall s. ST s a)
  -> Either e a
tryST action = runST (unsafeIOToST (trySync (unsafeSTToIO action)))
{-# INLINE tryST #-}

-- | Same as `trySync`, but for `ST`.
--
-- @since 1.0.0
tryAllST :: forall a. (forall s. ST s a) -> Either SomeException a
tryAllST action = runST (unsafeIOToST (tryAllSync (unsafeSTToIO action)))
