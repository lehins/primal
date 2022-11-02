-- |
-- Module      : Primal.Container.Array
-- Copyright   : (c) Alexey Kuleshevich 2020-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
{-# LANGUAGE LambdaCase #-}

module Primal.Container.Array
  ( module Primal.Container.Array.Internal
  , mapStopMutArray
  , findMutArray
  , ifindMutArray
  , module Primal.Container.Array.Atomic
  ) where

import Data.Maybe
import Primal.Monad
import Primal.Container.Array.Internal
import Primal.Container.Array.Atomic



mapStopMutArray :: (Primal s m, MutArray ma, Elt ma e) => ma e s -> (e -> m Bool) -> m Bool
mapStopMutArray ma f =
  isJust <$> findMutArray ma (\e -> f e >>= \c -> pure $ if c then Just () else Nothing)

findMutArray :: (Primal s m, MutArray ma, Elt ma e) => ma e s -> (e -> m (Maybe a)) -> m (Maybe a)
findMutArray ma f = ifindMutArray ma (const f)

ifindMutArray ::
  (Primal s m, MutArray ma, Elt ma e) =>
  ma e s ->
  (Int -> e -> m (Maybe a)) ->
  m (Maybe a)
ifindMutArray ma f = do
  Size n <- getSizeOfMutArray ma
  let go i
        | i >= n = pure Nothing
        | otherwise =
          readMutArray ma i >>= f i >>= \case
            Nothing -> go (i + 1)
            Just a -> pure $! Just a
  go 0
