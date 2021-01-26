-- |
-- Module      : Primal.Array
-- Copyright   : (c) Alexey Kuleshevich 2020-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Array
  ( -- $arrays
    Size(..)
  , module Primal.Array.Boxed
  , module Primal.Array.SmallBoxed
  , module Primal.Array.Unboxed
    -- * Helper functions
  , uninitialized
  , makeMutWith
  , fromListMutWith
  , foldrWithFB
  , eqWith
  , eqWithST
  , compareWith
  , appendWith
  , concatWith
  , cycleWith
  , liftEqWith
  , liftCompareWith
  , liftShowsPrecArray
  ) where

import Primal.Array.Boxed
import Primal.Array.SmallBoxed
import Primal.Array.Unboxed
import Primal.Array.Internal

-- $arrays
--
-- Minimal interface, wrappers around primops
--
-- Indexing and Size type
--
-- As in the rest of the library majority of the functions are unsafe.
--
-- no fusion
--
-- Boxed vs unboxed concept
--
-- Mutable vs Immutable
--
-- Note more features in primal-memory and primal-mutable
