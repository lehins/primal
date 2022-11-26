{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Primal.Memory.GC
-- Copyright   : (c) Alexey Kuleshevich 2020-2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module Primal.Memory.GC (
  -- * Garbage collection
  performGC,
  performMajorGC,
  performMinorGC,

  -- * Allocation counter and limits
  setAllocationCounter,
  getAllocationCounter,
  enableAllocationLimit,
  disableAllocationLimit,
) where

import Data.Int
import Primal.Monad
import qualified System.Mem as Mem

-- | Lifted version of `Mem.performGC`
--
-- @since 1.0.0
performGC :: PrimalIO m => m ()
performGC = liftIO Mem.performGC

-- | Lifted version of `Mem.performMajorGC`
--
-- @since 1.0.0
performMajorGC :: PrimalIO m => m ()
performMajorGC = liftIO Mem.performMajorGC

-- | Lifted version of `Mem.performMinorGC`
--
-- @since 1.0.0
performMinorGC :: PrimalIO m => m ()
performMinorGC = liftIO Mem.performMinorGC

-- | Lifted version of `Mem.setAllocationCounter`
--
-- @since 1.0.0
setAllocationCounter :: PrimalIO m => Int64 -> m ()
setAllocationCounter = liftIO . Mem.setAllocationCounter

-- | Lifted version of `Mem.getAllocationCounter`
--
-- @since 1.0.0
getAllocationCounter :: PrimalIO m => m Int64
getAllocationCounter = liftIO Mem.getAllocationCounter

-- | Lifted version of `Mem.enableAllocationLimit`
--
-- @since 1.0.0
enableAllocationLimit :: PrimalIO m => m ()
enableAllocationLimit = liftIO Mem.enableAllocationLimit

-- | Lifted version of `Mem.disableAllocationLimit`
--
-- @since 1.0.0
disableAllocationLimit :: PrimalIO m => m ()
disableAllocationLimit = liftIO Mem.disableAllocationLimit
