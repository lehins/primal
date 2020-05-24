{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Prim.MArray
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.MArray
  ( module Data.Prim.MArray
  , module Data.Prim.MArray.Internal
  , module Data.Prim.MArray.Atomic
  ) where

import Data.Prim.MArray.Atomic
import Data.Prim.MArray.Internal
import qualified Data.Prim.MArray.Boxed as Boxed


