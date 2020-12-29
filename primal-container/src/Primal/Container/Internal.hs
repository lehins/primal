{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Primal.Container.Internal
-- Copyright   : (c) Alexey Kuleshevich 2020-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Container.Internal where

import Data.Kind
import Primal.Concurrent.MVar
import Primal.Data.Array
import Primal.Data.Ref
import Primal.Memory.Addr
import Primal.Memory.PArray
import qualified Data.STRef as ST

-- | Container element constraint.
type family Elt (c :: k) a :: Constraint

type instance Elt [] a = ()
type instance Elt Maybe a = ()
type instance Elt (Either b) a = ()

type instance Elt Ref a = ()
type instance Elt ST.STRef a = ()
type instance Elt MVar a = ()

type instance Elt BArray a = ()
type instance Elt BMArray a = ()
type instance Elt SBArray a = ()
type instance Elt SBMArray a = ()
type instance Elt UArray a = Prim a
type instance Elt UMArray a = Prim a

type instance Elt Addr a = Prim a
type instance Elt MAddr a = Prim a
type instance Elt (PArray p) a = Prim a
type instance Elt (PMArray p) a = Prim a
