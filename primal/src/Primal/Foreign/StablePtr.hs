{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : Primal.Foreign.StablePtr
-- Copyright   : (c) Alexey Kuleshevich 2020-2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module Primal.Foreign.StablePtr
  ( GHC.StablePtr (..)
  , newStablePtr
  , deRefStablePtr
  , freeStablePtr
  , GHC.castStablePtrToPtr
  , GHC.castPtrToStablePtr
  ) where

import Control.DeepSeq
import qualified GHC.Stable as GHC
import Primal.Monad

-- | Orphan in @primal@
instance NFData (GHC.StablePtr a) where
  rnf (GHC.StablePtr _) = ()

-- | Orphan in @primal@
instance Show (GHC.StablePtr a) where
  show = show . GHC.castStablePtrToPtr

-- | Same as `GHC.newStablePtr`, but generalized to `Primal`
newStablePtr :: PrimalIO m => a -> m (GHC.StablePtr a)
newStablePtr = liftP . GHC.newStablePtr

-- | Same as `GHC.deRefStablePtr`, but generalized to `Primal`
deRefStablePtr :: PrimalIO m => GHC.StablePtr a -> m a
deRefStablePtr = liftP . GHC.deRefStablePtr

-- | Same as `GHC.freeStablePtr`, but generalized to `Primal`
freeStablePtr :: PrimalIO m => GHC.StablePtr a -> m ()
freeStablePtr = liftP . GHC.freeStablePtr
