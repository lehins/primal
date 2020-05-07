{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
-- |
-- Module      : Foreign.Prim
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Foreign.Prim
  ( -- * Re-exports
    module Foreign.C.Types
  , module System.Posix.Types
  , module GHC.Exts
#if __GLASGOW_HASKELL__ < 804
  , module GHC.Prim
#endif
  , module GHC.Int
  , module GHC.Word
  -- * Primitive
  , module Foreign.Prim.C
  , module Foreign.Prim.Cmm
  ) where

import Foreign.Prim.C
import Foreign.Prim.Cmm
import Foreign.C.Types
import System.Posix.Types
import GHC.Exts
import GHC.Int
import GHC.Word
#if __GLASGOW_HASKELL__ < 804
import GHC.Prim
  ( addCFinalizerToWeak#
  , deRefWeak#
  , finalizeWeak#
  , mkWeak#
  , mkWeakNoFinalizer#
  )
#endif
