{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Primal.Element.Unlift
-- Copyright   : (c) Alexey Kuleshevich 2021-2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Element.Unlift
  ( Unlift(..)
  , MutUnlift(..)
  ) where

import Data.ByteString.Short
import Data.Kind
import qualified Data.Text.Array as T
import Data.Word
import GHC.Exts
import Primal.Array.Unboxed
import Primal.Ref.Unboxed


class Unlift e where
  type UnliftIso e :: Type

  toUnliftIso :: e -> UnliftIso e
  default toUnliftIso :: Coercible e (UnliftIso e) => e -> UnliftIso e
  toUnliftIso = coerce

  fromUnliftIso :: UnliftIso e -> e
  default fromUnliftIso :: Coercible e (UnliftIso e) => UnliftIso e -> e
  fromUnliftIso = coerce

  indexArrayArray# :: ArrayArray# -> Int# -> e
  default indexArrayArray# :: Unlift (UnliftIso e) => ArrayArray# -> Int# -> e
  indexArrayArray# aa# i# = fromUnliftIso (indexArrayArray# aa# i# :: UnliftIso e)
  {-# INLINE indexArrayArray# #-}

  readMutableArrayArray# :: MutableArrayArray# s -> Int# -> State# s -> (# State# s, e #)
  default readMutableArrayArray# ::
    Unlift (UnliftIso e) => MutableArrayArray# s -> Int# -> State# s -> (# State# s, e #)
  readMutableArrayArray# maa# i# s = case readMutableArrayArray# maa# i# s of
                                      (# s', pa :: UnliftIso e #) -> (# s', fromUnliftIso pa #)
  {-# INLINE readMutableArrayArray# #-}

  writeMutableArrayArray# :: MutableArrayArray# s -> Int# -> e -> State# s -> State# s
  default writeMutableArrayArray# ::
    Unlift (UnliftIso e) => MutableArrayArray# s -> Int# -> e -> State# s -> State# s
  writeMutableArrayArray# maa# i# e = writeMutableArrayArray# maa# i# (toUnliftIso e)
  {-# INLINE writeMutableArrayArray# #-}


-- | Example default instance with coercible isomorphism
--
-- > newtype Foo s = Foo (UMArray Int s)
-- > instance MutUnlift Foo where
-- >   type MutUnliftIso Foo = UMArray Int
--
class MutUnlift (me :: Type -> Type) where
  type MutUnliftIso me :: Type -> Type

  toMutUnliftIso :: me s -> MutUnliftIso me s
  default toMutUnliftIso :: Coercible (me s) (MutUnliftIso me s) => me s -> MutUnliftIso me s
  toMutUnliftIso = coerce

  fromMutUnliftIso :: MutUnliftIso me s -> me s
  default fromMutUnliftIso :: Coercible (me s) (MutUnliftIso me s) => MutUnliftIso me s -> me s
  fromMutUnliftIso = coerce

  readMutMutableArrayArray# :: MutableArrayArray# s -> Int# -> State# s -> (# State# s, me s #)
  default readMutMutableArrayArray# ::
    MutUnlift (MutUnliftIso me) => MutableArrayArray# s -> Int# -> State# s -> (# State# s, me s #)
  readMutMutableArrayArray# maa# i# s =
    case readMutMutableArrayArray# maa# i# s of
      (# s', pa :: MutUnliftIso me s #) -> (# s', fromMutUnliftIso pa #)
  {-# INLINE readMutMutableArrayArray# #-}

  writeMutMutableArrayArray# :: MutableArrayArray# s -> Int# -> me s -> State# s -> State# s
  default writeMutMutableArrayArray# ::
    MutUnlift (MutUnliftIso me) => MutableArrayArray# s -> Int# -> me s -> State# s -> State# s
  writeMutMutableArrayArray# maa# i# ma =
    writeMutMutableArrayArray# maa# i# (toMutUnliftIso ma)
  {-# INLINE writeMutMutableArrayArray# #-}


instance Unlift (UArray e) where
  type UnliftIso (UArray e) = UArray e

  indexArrayArray# aa# i# = UArray (indexByteArrayArray# aa# i#)
  {-# INLINE indexArrayArray# #-}
  readMutableArrayArray# maa# i# s = case readByteArrayArray# maa# i# s of
                                      (# s', ba# #) -> (# s', UArray ba# #)
  {-# INLINE readMutableArrayArray# #-}
  writeMutableArrayArray# maa# i# (UArray ba#) = writeByteArrayArray# maa# i# ba#
  {-# INLINE writeMutableArrayArray# #-}

instance Unlift T.Array where
  type UnliftIso T.Array = UArray Word8
  toUnliftIso = fromTextArray
  {-# INLINE toUnliftIso #-}
  fromUnliftIso = toTextArray
  {-# INLINE fromUnliftIso #-}


instance Unlift ShortByteString where
  type UnliftIso ShortByteString = UArray Word8
  toUnliftIso = fromShortByteString
  {-# INLINE toUnliftIso #-}
  fromUnliftIso = toShortByteString
  {-# INLINE fromUnliftIso #-}


instance MutUnlift (UMArray e) where
  type MutUnliftIso (UMArray e) = UMArray e
  readMutMutableArrayArray# maa# i# s = case readMutableByteArrayArray# maa# i# s of
                                          (# s', ba# #) -> (# s', UMArray ba# #)
  {-# INLINE readMutMutableArrayArray# #-}
  writeMutMutableArrayArray# maa# i# (UMArray ba#) = writeMutableByteArrayArray# maa# i# ba#
  {-# INLINE writeMutMutableArrayArray# #-}


instance MutUnlift (URef e) where
  type MutUnliftIso (URef e) = URef e
  readMutMutableArrayArray# maa# i# s = case readMutableByteArrayArray# maa# i# s of
                                          (# s', ba# #) -> (# s', URef ba# #)
  {-# INLINE readMutMutableArrayArray# #-}
  writeMutMutableArrayArray# maa# i# (URef ba#) = writeMutableByteArrayArray# maa# i# ba#
  {-# INLINE writeMutMutableArrayArray# #-}


instance MutUnlift T.MArray where
  type MutUnliftIso T.MArray = UMArray Word8
  toMutUnliftIso = fromTextMArray
  {-# INLINE toMutUnliftIso #-}
  fromMutUnliftIso = toTextMArray
  {-# INLINE fromMutUnliftIso #-}

