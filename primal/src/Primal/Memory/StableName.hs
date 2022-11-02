{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Primal.Memory.StableName
-- Copyright   : (c) Alexey Kuleshevich 2020-2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Memory.StableName
  ( StableName(..)
  , makeStableName
  , makeAnyStableName
  , hashStableName
  , eqStableName
  ) where

import Primal.Monad
import GHC.Exts
#if MIN_VERSION_base(4,12,0)
import GHC.StableName (StableName(..), eqStableName, hashStableName)

-- | Orphan instance defined in "Primal.Memory.StableName"
instance Show (StableName a) where
  showsPrec = showPrecStableName

#else

-- | For compatibility with newer ghc versions this is a redifined version of
-- `System.Mem.StableName.StableName`. Prior to @base-4.12.0.0@ constructor was not
-- exported, hence this definition, starting with GHC-8.6 @StableName@ is re-exported from
-- @GHC.StableName@
data StableName a = StableName (StableName# a)

instance Eq (StableName a) where
  (==) = eqStableName

instance Show (StableName a) where
  showsPrec = showPrecStableName

-- | Convert a 'StableName' to an 'Int'.  The 'Int' returned is not
-- necessarily unique; several 'StableName's may map to the same 'Int'
-- (in practice however, the chances of this are small, so the result
-- of 'hashStableName' makes a good hash key).
hashStableName :: StableName a -> Int
hashStableName (StableName sn) = I# (stableNameToInt# sn)

-- | Equality on 'StableName' that does not require that the types of
-- the arguments match.
--
-- @since 0.1.0
eqStableName :: StableName a -> StableName b -> Bool
eqStableName (StableName sn1) (StableName sn2) =
  case eqStableName# sn1 sn2 of
    0# -> False
    _  -> True
#endif

showPrecStableName :: Int -> StableName a -> ShowS
showPrecStableName n sname =
  case n of
    0 -> inner
    _ -> ('(' :) . inner . (')' :)
  where
    inner = ("StableName " ++) . shows (hashStableName sname)

-- | Makes a 'StableName' for an arbitrary object.  The object passed as
-- the first argument is not evaluated by 'makeStableName'.
makeStableName :: Primal RW m => a -> m (StableName a)
makeStableName a =
  primal $ \s ->
    case makeStableName# a s of
      (# s', sn #) -> (# s', StableName sn #)

-- | Similar to
-- [`makeDynamicStableName`](http://hackage.haskell.org/package/stable-maps/docs/System-Mem-StableName-Dynamic.html),
-- but returns `StableName` `Any` and is generalized to `Primal`
makeAnyStableName :: Primal RW m => a -> m (StableName Any)
makeAnyStableName a =
  primal $ \s ->
    case makeStableName# a s of
      (# s', sn# #) -> (# s', StableName (unsafeCoerce# sn#) #)


