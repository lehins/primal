{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Foreign.Prim.StablePtr
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Foreign.Prim.StablePtr
  ( GHC.StablePtr(..)
  , newStablePtr
  , deRefStablePtr
  , freeStablePtr
  , GHC.castStablePtrToPtr
  , GHC.castPtrToStablePtr
  ) where

import Control.DeepSeq
import Control.Prim.Monad
import qualified GHC.Stable as GHC

instance NFData (GHC.StablePtr a) where
  rnf (GHC.StablePtr _) = ()

instance Show (GHC.StablePtr a) where
  show = show . GHC.castStablePtrToPtr


-- | Same as `GHC.newStablePtr`, but generalized to `MonadPrim`
newStablePtr :: MonadPrim RW m => a -> m (GHC.StablePtr a)
newStablePtr = liftPrimBase . GHC.newStablePtr

-- | Same as `GHC.deRefStablePtr`, but generalized to `MonadPrim`
deRefStablePtr :: MonadPrim RW m => GHC.StablePtr a -> m a
deRefStablePtr = liftPrimBase . GHC.deRefStablePtr

-- | Same as `GHC.freeStablePtr`, but generalized to `MonadPrim`
freeStablePtr :: MonadPrim RW m => GHC.StablePtr a -> m ()
freeStablePtr = liftPrimBase . GHC.freeStablePtr
