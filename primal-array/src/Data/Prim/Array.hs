{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Prim.Array
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Array
  ( module Data.Prim.Array
  , module Data.Prim.Array.Internal
  , module Data.Prim.Array.Atomic
  ) where

import Control.Prim.Monad
import Data.Prim.Array.Atomic
import Data.Prim.Array.Internal
import qualified Data.Prim.Array.Boxed as Boxed


