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
  ( MArray
  , Array
  , Elt
  , Size(..)
  , sizeOfArray
  , indexArray
  , thawArray
  , thawCopyArray
  , freezeMArray
  , freezeCopyMArray
  , newMArray
  , newRawMArray
  , readMArray
  , writeMArray
  , setMArray
  , copyArray
  , cloneArray
  , moveMArray
  , cloneMArray
  , getSizeOfMArray
  , shrinkMArray
  , resizeMArray
  , toListArray
  , fromListArray
  , fromListArrayN
  , foldrArray
  , makeArray
  , makeArrayM
  , createArrayM
  , createArrayM_
  , createArrayST
  , createArrayST_
  , makeMArray
  , traverseArray
  , module Data.Prim.MArray.Atomic
  ) where

import Data.Prim
import Data.Prim.MArray.Atomic
import Data.Prim.MArray.Internal


