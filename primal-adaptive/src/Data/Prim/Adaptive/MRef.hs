{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Prim.Adaptive.MRef
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Adaptive.MRef
  (
    AMRef(..)
  , AdaptiveRep
  --, createAArray
  ) where

import Control.Prim.Monad
import Data.Prim.MArray.Internal
import qualified Data.Prim.MArray.Boxed as Boxed
import qualified Data.Prim.Adaptive.Rep as Rep


newtype AArray a = AArray (Frozen (Rep.AdaptRep Boxed.BoxedMArray a a))

newtype AMArray a s = AMArray (Rep.AdaptRep Boxed.BoxedMArray a a s)

class Mutable (Rep.AdaptRep Boxed.BoxedMArray e e) => AdaptRep e
instance Mutable (Rep.AdaptRep Boxed.BoxedMArray e e) => AdaptRep e


instance AdaptRep e => MRef (AMArray e) where
  type Elt (AMArray e) = Elt (Rep.AdaptRep Boxed.BoxedMArray e e)
  newRawMArray = fmap AMArray . newRawMArray
  {-# INLINE newRawMArray #-}
  readMArray (AMArray ma) = readMArray ma
  {-# INLINE readMArray #-}
  writeMArray (AMArray ma) = writeMArray ma
  {-# INLINE writeMArray #-}


-- createAArray ::
--      (MonadPrim s m, AdaptRep e)
--   => Size
--   -> (AMArray e s -> m b)
--   -> m (b, AArray e)
-- createAArray = createArrayM
