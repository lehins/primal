{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Primal.Memory.ByteString
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Memory.ByteString
  ( MByteString(..)
  -- * Conversion
  -- Builder
  , Builder
  , toBuilderMem
  , toBuilderBytes
  , fromBuilderBytes
  -- ** ByteString
  , BS.ByteString(..)
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
import qualified Data.ByteString.Builder.Internal as B
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Short.Internal
import GHC.ForeignPtr
import Primal.Eval
import Primal.Foreign
import Primal.Memory.Bytes.Internal
import Primal.Memory.ForeignPtr
import Primal.Memory.Ptr
import Primal.Monad
import Primal.Mutable.Freeze

-- | Mutable version of a `ByteString`
newtype MByteString s = MByteString BS.ByteString

type instance Frozen MByteString = BS.ByteString

instance MutFreeze MByteString where
  thawST bs = pure $! MByteString bs
  {-# INLINE thawST #-}
  thawCloneST bs = do
    let c = Count (BS.length bs) :: Count Word8
    mbs@(MByteString bs') <- allocMutMemST c
    withPtrByteString bs' $ \ptr -> copyByteOffToPtrMemST bs 0 ptr 0 c
    pure mbs
  {-# INLINE thawCloneST #-}
  freezeMutST (MByteString bs) = pure bs
  {-# INLINE freezeMutST #-}


instance MemWrite MByteString where
  isSameMutMem (MByteString bs1) (MByteString bs2) = isSameMem bs1 bs2
  {-# INLINE isSameMutMem #-}
  readOffMutMemST (MByteString mbs) i = withPtrByteString mbs (`readOffPtr` i)
  {-# INLINE readOffMutMemST #-}
  readByteOffMutMemST (MByteString mbs) i =
    withPtrByteString mbs (`readByteOffPtr` i)
  {-# INLINE readByteOffMutMemST #-}
  writeOffMutMemST (MByteString mbs) i a =
    withPtrByteString mbs $ \ptr -> writeOffPtr ptr i a
  {-# INLINE writeOffMutMemST #-}
  writeByteOffMutMemST (MByteString mbs) i a =
    withPtrByteString mbs $ \ptr -> writeByteOffPtr ptr i a
  {-# INLINE writeByteOffMutMemST #-}
  moveByteOffToPtrMutMemST (MByteString fsrc) srcOff dstPtr dstOff c =
    withPtrByteString fsrc $ \srcPtr ->
      moveByteOffPtrToPtr srcPtr srcOff dstPtr dstOff c
  {-# INLINE moveByteOffToPtrMutMemST #-}
  moveByteOffToMBytesMutMemST (MByteString fsrc) srcOff dst dstOff c =
    withPtrByteString fsrc $ \srcPtr ->
      moveByteOffPtrToMBytes srcPtr srcOff dst dstOff c
  {-# INLINE moveByteOffToMBytesMutMemST #-}
  copyByteOffMutMemST src srcOff (MByteString fdst) dstOff c =
    withPtrByteString fdst $ \dstPtr ->
      copyByteOffToPtrMemST src srcOff dstPtr dstOff c
  {-# INLINE copyByteOffMutMemST #-}
  moveByteOffMutMemST src srcOff (MByteString fdst) dstOff c =
    withPtrByteString fdst $ \dstPtr ->
      moveByteOffToPtrMutMemST src srcOff dstPtr dstOff c
  {-# INLINE moveByteOffMutMemST #-}
  setByteOffMutMemST (MByteString mbs) off c a =
    withPtrByteString mbs $ \ptr -> setByteOffPtr ptr off c a
  {-# INLINE setByteOffMutMemST #-}
  setMutMemST (MByteString mbs) off c a =
    withPtrByteString mbs $ \ptr -> setOffPtr ptr off c a
  {-# INLINE setMutMemST #-}


instance MemAlloc MByteString where
  getByteCountMutMemST (MByteString bs) = pure $! Count (BS.length bs)
  {-# INLINE getByteCountMutMemST #-}
  allocMutMemST c = do
    let cb = toByteCount c
    fp <- mallocByteCountPlainForeignPtr cb
    pure $ MByteString (BS.PS fp 0 (coerce cb))
  {-# INLINE allocMutMemST #-}
  reallocMutMemST bsm@(MByteString (BS.PS fp o n)) newc
    | newn > n = defaultReallocMutMem bsm newc
    | otherwise = pure $ MByteString (BS.PS fp o newn)
    where -- constant time slice if we need to reduce the size
      Count newn = toByteCount newc
  {-# INLINE reallocMutMemST #-}

instance MemPtr MByteString where
  toMForeignPtrMem (MByteString bs) =
    case bs of
#if MIN_VERSION_bytestring(0,11,0)
      BS.BS fptr _ -> MForeignPtr (castForeignPtr fptr)
#else
      BS.PS fptr o _ -> MForeignPtr (castForeignPtr (fptr `plusByteOffForeignPtr` Off o))
#endif


-- | /O(1)/ - Cast immutable `Bytes` to an immutable `ByteString`
--
-- @since 0.1.0
toByteStringBytes :: Bytes 'Pin -> BS.ByteString
{-# INLINE toByteStringBytes #-}
toByteStringBytes b =
#if MIN_VERSION_bytestring(0,11,0)
  BS.BS (toForeignPtrBytes b) (coerce (byteCountBytes b))
#else
  BS.PS (toForeignPtrBytes b) 0 (coerce (byteCountBytes b))
#endif


-- | /O(1)/ - Cast an immutable `ByteString` to immutable `Bytes`. Only unsliced
-- `ByteString`s that are backed by a `ForeignPtr` allocated on Haskell heap without
-- finilizers can be converted without copy.
--
-- @since 0.2.0
castByteStringBytes :: BS.ByteString -> Either String (Bytes 'Pin)
#if MIN_VERSION_bytestring(0,11,0)
castByteStringBytes (BS.BS fptr n) = do
#else
castByteStringBytes (BS.PS fptr o n) = do
  unless (o == 0) sliceError
#endif
  b <- castForeignPtrToBytes fptr
  unless (unCount (byteCountBytes b) == n) sliceError
  Right b
  where
    sliceError = Left "ByteString was sliced"
{-# INLINE castByteStringBytes #-}


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
fromByteStringBytes :: Typeable p => BS.ByteString -> Bytes p
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



withNoHaltPtrByteString :: MonadUnliftPrim s m => BS.ByteString -> (Ptr a -> m b) -> m b
#if MIN_VERSION_bytestring(0,11,0)
withNoHaltPtrByteString (BS.BS (ForeignPtr addr# ptrContents) _) f = do
#else
withNoHaltPtrByteString (BS.PS (ForeignPtr addr'# ptrContents) (I# o#) _) f = do
  let addr# = addr'# `plusAddr#` o#
#endif
  keepAlive ptrContents $ f (Ptr addr#)
{-# INLINE withNoHaltPtrByteString #-}



-- | Construct a 'Builder' that copies an immutable read-only memory buffer.
--
-- @since 0.1.0
toBuilderMem :: MemRead mr => mr -> Builder
toBuilderMem = \mr -> B.builder $ toBuilderMemStep mr
{-# INLINE toBuilderMem #-}

-- | Copy the bytes from an immutable read-only memory buffer into the output stream.
toBuilderMemStep ::
     MemRead mr
  => mr -- ^ Input 'SH.ShortByteString'.
  -> (B.BufferRange -> IO (B.BuildSignal e))
  -> (B.BufferRange -> IO (B.BuildSignal e))
toBuilderMemStep !mr k = go 0 (byteCountMem mr)
  where
    go !ip !ipe (B.BufferRange op ope)
      | inpRemaining <= outRemaining = do
        stToIO $ copyByteOffToPtrMemST mr ip op 0 inpRemaining
        let !br' = B.BufferRange (op `plusByteCountPtr` inpRemaining) ope
        k br'
      | otherwise = do
        stToIO $ copyByteOffToPtrMemST mr ip op 0 outRemaining
        let !ip' = ip `offPlusCount` outRemaining
        return $ B.bufferFull 1 ope (go ip' ipe)
      where
        outRemaining = ope `minusByteCountPtr` op
        inpRemaining = ipe `countMinusOff` ip
{-# INLINE toBuilderMemStep #-}


