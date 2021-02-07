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
import Primal.Array
import Primal.Ref
import Primal.Memory.Addr
import Primal.Memory.PArray

-- | Container element constraint.
type family Elt (c :: k) a :: Constraint

type instance Elt [] a = ()
type instance Elt Maybe a = ()
type instance Elt (Either b) a = ()

type instance Elt BRef a = ()
type instance Elt MVar a = ()

type instance Elt BArray a = ()
type instance Elt BMArray a = ()
type instance Elt SBArray a = ()
type instance Elt SBMArray a = ()
type instance Elt UArray a = Unbox a
type instance Elt UMArray a = Unbox a

type instance Elt Addr a = Unbox a
type instance Elt MAddr a = Unbox a
type instance Elt (PArray p) a = Unbox a
type instance Elt (PMArray p) a = Unbox a
