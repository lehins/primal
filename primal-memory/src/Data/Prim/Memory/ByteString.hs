{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
-- |
-- Module      : Data.Prim.Memory.ByteString
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Memory.ByteString
  (
    MByteString(..)
  -- * Conversion
  -- Builder
  , Builder
  , toBuilderBytes
  , fromBuilderBytes
  -- ** ByteString
  , ByteString(..)
  , toByteStringBytes
  , fromByteStringBytes
  , fromLazyByteStringBytes
  , withPtrByteString
  , withNoHaltPtrByteString
  -- ** ShortByteString
  , ShortByteString(..)
  , toShortByteStringBytes
  , fromShortByteStringBytes
  , byteStringConvertError
  ) where

import Control.Monad.ST
import Data.ByteString.Builder
import Data.ByteString.Internal
import Data.ByteString.Short.Internal
import qualified Data.ByteString.Lazy as BSL
import Data.Prim
import Foreign.Prim
import Control.Prim.Monad
import GHC.ForeignPtr
import Data.Prim.Memory.Ptr
import Data.Prim.Memory.Bytes.Internal
  ( Bytes(..)
  , Pinned(..)
  , allocMBytes
  , freezeMBytes
  , byteCountBytes
  , toForeignPtrBytes
  , fromForeignPtrBytes
  , byteStringConvertError
  )

-- | Mutable version of a `ByteString`
newtype MByteString s = MByteString ByteString


-- | /O(1)/ - Cast an immutable `Bytes` to an immutable `ByteString`
--
-- @since 0.1.0
toByteStringBytes :: Bytes 'Pin -> ByteString
toByteStringBytes b = PS (toForeignPtrBytes b) 0 (coerce (byteCountBytes b))
{-# INLINE toByteStringBytes #-}

-- | /O(1)/ - Cast an immutable `Bytes` to an immutable `ShortByteString`
--
-- @since 0.1.0
toShortByteStringBytes :: Bytes p -> ShortByteString
toShortByteStringBytes (Bytes ba#) = SBS ba#
{-# INLINE toShortByteStringBytes #-}

-- | /O(1)/ - Cast an immutable  `ShortByteString` to an immutable `Bytes`
--
-- @since 0.1.0
fromShortByteStringBytes :: ShortByteString -> Bytes 'Inc
fromShortByteStringBytes (SBS ba#) = Bytes ba#
{-# INLINE fromShortByteStringBytes #-}

-- | Convert `Bytes` into a bytestring `Builder`
toBuilderBytes :: Bytes p -> Builder
toBuilderBytes = shortByteString . toShortByteStringBytes
{-# INLINE[1] toBuilderBytes #-}
{-# RULES
"toBuilderBytes" toBuilderBytes = byteString . toByteStringBytes
  #-}

-- | /O(n)/ - Allocate `Bytes` and fill them using the supplied `Builder`
fromBuilderBytes :: Builder -> Bytes 'Pin
fromBuilderBytes b = fromLazyByteStringBytes (toLazyByteString b)
{-# INLINE fromBuilderBytes #-}


-- | /O(n)/ - Allocate `Bytes` and fill them with the contents of a lazy `BSL.ByteString`
fromLazyByteStringBytes :: BSL.ByteString -> Bytes 'Pin
fromLazyByteStringBytes bsl =
  case BSL.toStrict bsl of
    PS fptr _ _ -> either byteStringConvertError id $ fromForeignPtrBytes fptr
{-# INLINE fromLazyByteStringBytes #-}


-- | /O(n)/ - Allocate `Bytes` and fill them with the contents of a strict `ByteString`
fromByteStringBytes :: Typeable p => ByteString -> Bytes p
fromByteStringBytes bs@(PS _ _ n) =
  runST $
  withPtrByteString bs $ \ptr -> do
    let c = Count n :: Count Word8
    mb <- allocMBytes c
    movePtrToMBytes ptr 0 mb 0 c
    freezeMBytes mb
{-# INLINE fromByteStringBytes #-}


withPtrByteString :: MonadPrim s m => ByteString -> (Ptr a -> m b) -> m b
withPtrByteString (PS (ForeignPtr addr# ptrContents) (I# o#) _) f = do
  r <- f (Ptr (addr# `plusAddr#` o#))
  r <$ touch ptrContents
{-# INLINE withPtrByteString #-}


withNoHaltPtrByteString :: MonadUnliftPrim s m => ByteString -> (Ptr a -> m b) -> m b
withNoHaltPtrByteString (PS (ForeignPtr addr# ptrContents) (I# o#) _) f =
  withUnliftPrim ptrContents $ f (Ptr (addr# `plusAddr#` o#))
{-# INLINE withNoHaltPtrByteString #-}
