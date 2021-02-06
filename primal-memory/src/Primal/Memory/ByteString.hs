{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
-- |
-- Module      : Primal.Memory.ByteString
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Memory.ByteString
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
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import Data.ByteString.Internal
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Short.Internal
import GHC.ForeignPtr
import Primal.Eval
import Primal.Foreign
import Primal.Memory.Bytes.Internal (Bytes(..), Pinned(..), allocMBytes,
                                     byteCountBytes, byteStringConvertError,
                                     castForeignPtrToBytes, freezeMBytes,
                                     relaxPinnedBytes, toForeignPtrBytes)
import Primal.Memory.Ptr
import Primal.Monad

-- | Mutable version of a `ByteString`
newtype MByteString s = MByteString ByteString


-- | /O(1)/ - Cast immutable `Bytes` to an immutable `ByteString`
--
-- @since 0.1.0
toByteStringBytes :: Bytes 'Pin -> ByteString
{-# INLINE toByteStringBytes #-}
toByteStringBytes b =
#if MIN_VERSION_bytestring(0,11,0)
  BS (toForeignPtrBytes b) (coerce (byteCountBytes b))
#else
  PS (toForeignPtrBytes b) 0 (coerce (byteCountBytes b))
#endif


-- | /O(1)/ - Cast an immutable `ByteString` to immutable `Bytes`. Only unsliced
-- `ByteString`s that are backed by a `ForeignPtr` allocated on Haskell heap without
-- finilizers can be converted without copy.
--
-- @since 0.2.0
castByteStringBytes :: ByteString -> Either String (Bytes 'Pin)
#if MIN_VERSION_bytestring(0,11,0)
castByteStringBytes (BS fptr n) = do
#else
castByteStringBytes (PS fptr o n) = do
  unless (o == 0) sliceError
#endif
  b <- castForeignPtrToBytes fptr
  unless (unCount (byteCountBytes b) == n) sliceError
  Right b
  where
    sliceError = Left "ByteString was sliced"
{-# INLINE castByteStringBytes #-}


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
fromLazyByteStringBytes = fromByteStringBytes . BSL.toStrict
{-# INLINE fromLazyByteStringBytes #-}


-- | /O(n)/ - Convert a strict `ByteString` to `Bytes`.
fromByteStringBytes :: Typeable p => ByteString -> Bytes p
fromByteStringBytes bs =
  case castByteStringBytes bs of
    Right b -> relaxPinnedBytes b
    Left _ ->
      runST $
      withPtrByteString bs $ \ptr -> do
        let c = Count (BS.length bs) :: Count Word8
        mb <- allocMBytes c
        copyPtrToMBytes ptr 0 mb 0 c
        freezeMBytes mb
{-# INLINE fromByteStringBytes #-}


withPtrByteString :: MonadPrim s m => ByteString -> (Ptr a -> m b) -> m b
#if MIN_VERSION_bytestring(0,11,0)
withPtrByteString (BS (ForeignPtr addr# ptrContents) _) f = do
#else
withPtrByteString (PS (ForeignPtr addr'# ptrContents) (I# o#) _) f = do
  let addr# = addr'# `plusAddr#` o#
#endif
  r <- f (Ptr addr#)
  r <$ touch ptrContents
{-# INLINE withPtrByteString #-}


withNoHaltPtrByteString :: MonadUnliftPrim s m => ByteString -> (Ptr a -> m b) -> m b
#if MIN_VERSION_bytestring(0,11,0)
withNoHaltPtrByteString (BS (ForeignPtr addr# ptrContents) _) f = do
#else
withNoHaltPtrByteString (PS (ForeignPtr addr'# ptrContents) (I# o#) _) f = do
  let addr# = addr'# `plusAddr#` o#
#endif
  keepAlive ptrContents $ f (Ptr addr#)
{-# INLINE withNoHaltPtrByteString #-}
