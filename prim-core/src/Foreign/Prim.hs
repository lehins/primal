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
  , module GHC.Exts
  , module GHC.Int
  , module GHC.Word
  -- * Primitive
  , module Foreign.Prim.C
  , module Foreign.Prim.Cmm
  ) where

import Foreign.Prim.C
import Foreign.Prim.Cmm
import Foreign.C.Types
import GHC.Exts
import GHC.Int
import GHC.Word
