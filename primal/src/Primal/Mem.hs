{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      : Primal.Mem
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Mem
  ( -- * Garbage collection
    performGC
  , performMajorGC
  , performMinorGC
    -- * Allocation counter and limits
  , setAllocationCounter
  , getAllocationCounter
  , enableAllocationLimit
  , disableAllocationLimit
  ) where

import qualified System.Mem as Mem
import Primal.Monad
import Data.Int

-- | Lifted version of `Mem.performGC`
--
-- @since 0.1.0
performGC :: MonadIO m => m ()
performGC = liftIO Mem.performGC

-- | Lifted version of `Mem.performMajorGC`
--
-- @since 0.1.0
performMajorGC :: MonadIO m => m ()
performMajorGC = liftIO Mem.performMajorGC

-- | Lifted version of `Mem.performMinorGC`
--
-- @since 0.1.0
performMinorGC :: MonadIO m => m ()
performMinorGC = liftIO Mem.performMinorGC

-- | Lifted version of `Mem.setAllocationCounter`
--
-- @since 0.1.0
setAllocationCounter :: MonadIO m => Int64 -> m ()
setAllocationCounter = liftIO . Mem.setAllocationCounter

-- | Lifted version of `Mem.getAllocationCounter`
--
-- @since 0.1.0
getAllocationCounter :: MonadIO m => m Int64
getAllocationCounter = liftIO Mem.getAllocationCounter


-- | Lifted version of `Mem.enableAllocationLimit`
--
-- @since 0.1.0
enableAllocationLimit :: MonadIO m => m ()
enableAllocationLimit = liftIO Mem.enableAllocationLimit


-- | Lifted version of `Mem.disableAllocationLimit`
--
-- @since 0.1.0
disableAllocationLimit :: MonadIO m => m ()
disableAllocationLimit = liftIO Mem.disableAllocationLimit
