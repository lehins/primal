{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Primal.Memory.Text
-- Copyright   : (c) Alexey Kuleshevich 2020-2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module Primal.Memory.Text (
  Text (..),
  MText (..),
  toIncMText,
  ensurePinnedText,
  ensurePinnedMText,
  Array (..),
  MArray (..),
  fromTextArrayBytes,
  toTextArrayBytes,
  fromTextMArrayMBytes,
  toTextMArrayMBytes,
) where

import Data.Text.Array
import Data.Text.Internal as T
import Primal.Element.Unbox
import Primal.Memory.Internal
import Primal.Monad
import Primal.Mutable.Freeze

-- | This is a text code unit size. Depending on the version of text library it
-- can either be `Word16` or `Word8`. Prior to @text-2.0@ the library was using
-- @utf-16@ for underlying unicode repesentation, while going forward it uses
-- @utf-8@
#if MIN_VERSION_text(2,0,0)
type TCUSize = Word8
#else
type TCUSize = Word16
#endif

-- | Mutable version of a `Text`
data MText (p :: Pinned) s
  = MText
      {-# UNPACK #-} !(MBytes p s) -- payload (TCUSize elements)
      {-# UNPACK #-} !(Off TCUSize) -- offset (units of TCUSize, not Char)
      {-# UNPACK #-} !(Count TCUSize) -- length (units of TCUSize, not Char)

type instance Frozen (MText 'Inc) = Text

instance MutFreeze (MText 'Inc) where
  thawST (T.Text a o k) = (\ma -> MText ma (Off o) (Count k)) <$> thawST (fromTextArrayBytes a)
  {-# INLINE thawST #-}
  thawCloneST (T.Text a o k) = do
    let c = Count k :: Count TCUSize
    ma <- allocMutMemST c
    copyByteOffMutMemST a (toByteOff (Off o :: Off TCUSize)) ma 0 c
    pure $ MText ma 0 c
  {-# INLINE thawCloneST #-}
  freezeMutST (MText ma o k) =
    (\a -> T.Text (toTextArrayBytes a) (unOff o) (unCount k)) <$> freezeMutST ma
  {-# INLINE freezeMutST #-}

instance MemWrite (MText p) where
  accessMutMemST (MText a o _) f g i = accessMutMemST a f g (toByteOff o + i)
  {-# INLINE accessMutMemST #-}
  isSameMutMem (MText a1 o1 n1) (MText a2 o2 n2) = isSameMutMem a1 a2 && o1 == o2 && n1 == n2
  {-# INLINE isSameMutMem #-}
  readByteOffMutMemST (MText a o _) i = readByteOffMutMemST a (toByteOff o + i)
  {-# INLINE readByteOffMutMemST #-}
  writeByteOffMutMemST (MText a o _) i = writeByteOffMutMemST a (toByteOff o + i)
  {-# INLINE writeByteOffMutMemST #-}
  moveByteOffToPtrMutMemST (MText a o _) i = moveByteOffToPtrMutMemST a (toByteOff o + i)
  {-# INLINE moveByteOffToPtrMutMemST #-}
  moveByteOffToMBytesMutMemST (MText a o _) i = moveByteOffToMBytesMutMemST a (toByteOff o + i)
  {-# INLINE moveByteOffToMBytesMutMemST #-}
  moveByteOffMutMemST src srcOff (MText a o _) i =
    moveByteOffMutMemST src srcOff a (toByteOff o + i)
  {-# INLINE moveByteOffMutMemST #-}
  copyByteOffMutMemST src srcOff (MText a o _) i =
    copyByteOffMutMemST src srcOff a (toByteOff o + i)
  {-# INLINE copyByteOffMutMemST #-}
  setByteOffMutMemST (MText a o _) i = setByteOffMutMemST a (toByteOff o + i)
  {-# INLINE setByteOffMutMemST #-}

instance MemAlloc (MText 'Inc) where
  allocMutMemST c = do
    ma <- allocMutMemST c
    pure $ MText ma 0 (toTCUSizeCount c)
  {-# INLINE allocMutMemST #-}
  allocPinnedMutMemST c = do
    ma <- allocPinnedMutMemST c
    pure $ MText ma 0 (toTCUSizeCount c)
  {-# INLINE allocPinnedMutMemST #-}
  allocAlignedPinnedMutMemST c = do
    ma <- allocAlignedPinnedMutMemST c
    pure $ MText ma 0 (toTCUSizeCount c)
  {-# INLINE allocAlignedPinnedMutMemST #-}

instance MemFreeze (MText 'Inc) where
  getByteCountMutMemST (MText _ _ k) = pure (toByteCount k)
  {-# INLINE getByteCountMutMemST #-}
  reallocMutMemST (MText ma o k) c'
    | k' <= k = pure $ MText ma o k'
    | o == 0 = do
        ma' <- reallocMutMemST ma c'
        pure $ MText ma' 0 k'
    | otherwise = do
        ma' <- allocMutMemST c'
        moveByteOffMutMemST ma (toByteOff o) ma' 0 k
        pure $ MText ma' 0 k'
    where
      k' = toTCUSizeCount c'
  {-# INLINE reallocMutMemST #-}

toTCUSizeCount :: Unbox e => Count e -> Count TCUSize
toTCUSizeCount c = fromByteCount (Count (unCountBytes c))
{-# INLINE toTCUSizeCount #-}

ensurePinnedText :: T.Text -> T.Text
ensurePinnedText t = runST $ do
  freezeMutMem . toIncMText =<< ensurePinnedMText =<< thawMem t
{-# INLINE ensurePinnedText #-}

ensurePinnedMText :: (Typeable p, Primal s m) => MText p s -> m (MText 'Pin s)
ensurePinnedMText (MText mb o k) =
  case toPinnedMBytes mb of
    Just mbPin -> pure (MText mbPin o k)
    Nothing -> do
      mb' <- allocMutMem k
      b <- freezeMutMem mb
      copyMem b o mb' 0 k
      pure $ MText mb' 0 k
{-# INLINE ensurePinnedMText #-}

toIncMText :: Typeable p => MText p s -> MText 'Inc s
toIncMText (MText mb o k) = MText (toIncMBytes mb) o k
{-# INLINE toIncMText #-}
