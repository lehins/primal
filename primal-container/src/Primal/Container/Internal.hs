{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
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
type family Elt (c :: k) e :: Constraint

type instance Elt [] e = ()
type instance Elt Maybe e = ()
type instance Elt (Either b) e = ()

type instance Elt BRef e = ()
type instance Elt MVar e = ()

type instance Elt BArray e = ()
type instance Elt BMArray e = ()
type instance Elt SBArray e = ()
type instance Elt SBMArray e = ()
type instance Elt UArray e = Unbox e
type instance Elt UMArray e = Unbox e

type instance Elt Addr e = Unbox e
type instance Elt MAddr e = Unbox e
type instance Elt (PArray p) e = Unbox e
type instance Elt (PMArray p) e = Unbox e
