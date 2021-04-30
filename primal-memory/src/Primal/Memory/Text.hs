{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Primal.Memory.Text
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Memory.Text
  ( Text(..)
  , MText(..)
  , Array(..)
  , MArray(..)
  , fromTextArrayBytes
  , toTextArrayBytes
  , fromTextMArrayMBytes
  , toTextMArrayMBytes
  ) where

import Data.Text.Array
import Data.Text.Internal as T
import Primal.Mutable.Freeze
import Primal.Unbox
import Primal.Memory.Bytes.Internal
  ( Bytes(..)
  , MBytes(..)
  , MemAlloc(..)
  , MemWrite(..)
  , Pinned(..)
  , fromTextArrayBytes
  , fromTextMArrayMBytes
  , toTextArrayBytes
  , toTextMArrayMBytes
  , allocUnpinnedMBytes
  )

-- | Mutable version of a `Text`
data MText s =
  MText
    {-# UNPACK #-}!(MArray s) -- payload (Word16 elements)
    {-# UNPACK #-}!Int        -- offset (units of Word16, not Char)
    {-# UNPACK #-}!Int        -- length (units of Word16, not Char)


type instance Frozen MText = Text

instance MutFreeze MText where
  thawST (T.Text a o k) = (\ma -> MText ma o k) <$> thawST a
  {-# INLINE thawST #-}
  thawCloneST (T.Text a o k) = do
    let c = Count k :: Count Word16
    ma <- allocMutMemST c
    copyByteOffMutMemST a (toByteOff (Off o :: Off Word16)) ma 0 c
    pure $ MText ma 0 k
  {-# INLINE thawCloneST #-}
  freezeMutST (MText ma o k) = (\a -> T.Text a o k) <$> freezeMutST ma
  {-# INLINE freezeMutST #-}

instance MemWrite MText where
  isSameMutMem (MText a1 o1 n1) (MText a2 o2 n2) = isSameMutMem a1 a2 && o1 == o2 && n1 == n2
  {-# INLINE isSameMutMem #-}
  readByteOffMutMemST (MText a o _) i =
    readByteOffMutMemST a (toByteOff (Off o :: Off Word16) + i)
  {-# INLINE readByteOffMutMemST #-}
  writeByteOffMutMemST (MText a o _) i =
    writeByteOffMutMemST a (toByteOff (Off o :: Off Word16) + i)
  {-# INLINE writeByteOffMutMemST #-}
  moveByteOffToPtrMutMemST (MText a o _) i =
    moveByteOffToPtrMutMemST a (toByteOff (Off o :: Off Word16) + i)
  {-# INLINE moveByteOffToPtrMutMemST #-}
  moveByteOffToMBytesMutMemST (MText a o _) i =
    moveByteOffToMBytesMutMemST a (toByteOff (Off o :: Off Word16) + i)
  {-# INLINE moveByteOffToMBytesMutMemST #-}
  moveByteOffMutMemST src srcOff (MText a o _) i =
    moveByteOffMutMemST src srcOff a (toByteOff (Off o :: Off Word16) + i)
  {-# INLINE moveByteOffMutMemST #-}
  copyByteOffMutMemST src srcOff (MText a o _) i =
    copyByteOffMutMemST src srcOff a (toByteOff (Off o :: Off Word16) + i)
  {-# INLINE copyByteOffMutMemST #-}
  setByteOffMutMemST (MText a o _) i =
    setByteOffMutMemST a (toByteOff (Off o :: Off Word16) + i)
  {-# INLINE setByteOffMutMemST #-}

instance MemAlloc MText where
  getByteCountMutMemST (MText _ _ k) = pure (toByteCount (Count k :: Count Word16))
  {-# INLINE getByteCountMutMemST #-}
  allocMutMemST c = do
    ma <- allocMutMemST c
    pure $ MText ma 0 (unCount (toCount16 c))
  {-# INLINE allocMutMemST #-}
  reallocMutMemST (MText ma o k) c'
    | k' <= k = pure $ MText ma o k'
    | o == 0 = do
        ma' <- reallocMutMemST ma c'
        pure $ MText ma' 0 k'
    | otherwise = do
        ma' <- allocMutMemST c'
        moveByteOffMutMemST ma (toByteOff (Off o :: Off Word16)) ma' 0 (Count k :: Count Word16)
        pure $ MText ma' 0 k'
     where k' = unCount (toCount16 c')
  {-# INLINE reallocMutMemST #-}

toCount16 :: Unbox e => Count e -> Count Word16
toCount16 c = fromByteCount (Count (unCountBytes c))
{-# INLINE toCount16 #-}
