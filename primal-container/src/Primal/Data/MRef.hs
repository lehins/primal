-- |
-- Module      : Data.Prim.MRef
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module Data.Prim.MRef (
  module Data.Prim.MRef.Internal,
  module Data.Prim.MRef.Atomic,
) where

import Data.Prim.MRef.Atomic
import Data.Prim.MRef.Internal
