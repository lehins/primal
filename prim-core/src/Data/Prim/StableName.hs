{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Prim.StableName
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.StableName
  ( GHC.StableName(..)
  , makeStableName
  , makeAnyStableName
  , GHC.hashStableName
  , GHC.eqStableName
  ) where

import Control.Prim.Monad
import GHC.Exts
import qualified GHC.StableName as GHC

instance Show (GHC.StableName a) where
  showsPrec n sname =
    case n of
      0 -> inner
      _ -> ('(' :) . inner . (')' :)
    where
      inner = ("StableName " ++) . shows (GHC.hashStableName sname)


-- | Same as `GHC.makeStableName`, but generalized to `MonadPrim`
makeStableName :: MonadPrim RW m => a -> m (GHC.StableName a)
makeStableName = liftPrimBase . GHC.makeStableName

-- | Similar to
-- [`makeDynamicStableName`](http://hackage.haskell.org/package/stable-maps/docs/System-Mem-StableName-Dynamic.html),
-- but returns `GHC.StableName` `Any` and is generalized to `MonadPrim`
makeAnyStableName :: MonadPrim RW m => a -> m (GHC.StableName Any)
makeAnyStableName a =
  prim $ \s ->
    case makeStableName# a s of
      (# s', sn# #) -> (# s', GHC.StableName (unsafeCoerce# sn#) #)
