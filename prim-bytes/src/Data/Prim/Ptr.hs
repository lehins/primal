{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Prim.Ptr
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Ptr
  ( Ptr(..)
  , readPtr
  , readOffPtr
  , writePtr
  , writeOffPtr

  , copyPtrToPtr
  , movePtrToPtr
  , copyPtrToMBytes
  , movePtrToMBytes
  , copyBytesToPtr
  , moveBytesToPtr
  , copyMBytesToPtr
  , moveMBytesToPtr
  ) where


import Data.ByteString (ByteString)
import Data.ByteString.Builder.Prim (word32LE, word64LE)
import Data.ByteString.Builder.Prim.Internal (runF)
import Data.ByteString.Unsafe
import Data.ByteString.Internal
import Data.Proxy
--import Data.ByteString.Internal (ByteString(PS))
import Control.Arrow
import Control.DeepSeq
import Control.Monad
import Control.Monad.Prim
import Control.Monad.Prim.Unsafe
import Control.Monad.ST
import Data.Foldable as Foldable
import Data.Function
import Data.List as List
import Data.Maybe
import Data.Prim
import Data.Prim.Bytes
import Data.Prim.Class
import Data.Prim.Foreign (getSizeofMutableByteArray#, isByteArrayPinned#,
                          isMutableByteArrayPinned#, memmoveAddr#,
                          memmoveMutableByteArray#,
                          memmoveMutableByteArrayFromAddr#,
                          memmoveMutableByteArrayToAddr#)
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Utils
import GHC.Exts hiding (getSizeofMutableByteArray#, isByteArrayPinned#,
                 isMutableByteArrayPinned#)
import GHC.Word
import GHC.ForeignPtr
import Numeric (showHex)



readOffPtr :: (MonadPrim s m, Prim a) => Ptr a -> Int -> m a
readOffPtr (Ptr addr#) (I# i#) = prim (readOffAddr# addr# i#)
{-# INLINE readOffPtr #-}

writeOffPtr :: (MonadPrim s m, Prim a) => Ptr a -> Int -> a -> m ()
writeOffPtr (Ptr addr#) (I# i#) a = prim_ (writeOffAddr# addr# i# a)
{-# INLINE writeOffPtr #-}

readPtr :: (MonadPrim s m, Prim a) => Ptr a -> m a
readPtr (Ptr addr#) = prim (readOffAddr# addr# 0#)
{-# INLINE readPtr #-}

writePtr :: (MonadPrim s m, Prim a) => Ptr a -> Int -> m ()
writePtr (Ptr addr#) a = prim_ (writeOffAddr# addr# 0# a)
{-# INLINE writePtr #-}



copyPtrToPtr8 :: MonadPrim s m => Ptr Word8 -> Int -> Ptr Word8 -> Int -> Count Word8 -> m ()
copyPtrToPtr8 srcPtr srcOff dstPtr dstOff (Count n) =
  unsafeIOToPrim $
  copyBytes (dstPtr `plusPtr` dstOff) (srcPtr `plusPtr` srcOff) n
{-# INLINE copyPtrToPtr8 #-}
{-# RULES "copyPtrToPtr8" copyPtrToPtr = copyPtrToPtr8 #-}

copyPtrToPtr :: (MonadPrim s m, Prim a) => Ptr a -> Int -> Ptr a -> Int -> Count a -> m ()
copyPtrToPtr (Ptr srcAddr#) srcOff (Ptr dstAddr#) dstOff c =
  unsafeIOToPrim $
  copyBytes
    (Ptr (dstAddr# `plusAddr#` fromOff# c dstOff))
    (Ptr (srcAddr# `plusAddr#` fromOff# c srcOff))
    (I# (fromCount# c))
{-# INLINE[0] copyPtrToPtr #-}



movePtrToPtr8 :: MonadPrim s m => Ptr Word8 -> Int -> Ptr Word8 -> Int -> Count Word8 -> m ()
movePtrToPtr8 (Ptr srcAddr#) (I# srcOff#) (Ptr dstAddr#) (I# dstOff#) (Count (I# n#)) =
  unsafeIOToPrim $ memmoveAddr# srcAddr# srcOff# dstAddr# dstOff# n#
{-# INLINE movePtrToPtr8 #-}
{-# RULES "movePtrToPtr8" movePtrToPtr = movePtrToPtr8 #-}

movePtrToPtr :: (MonadPrim s m, Prim a) => Ptr a -> Int -> Ptr a -> Int -> Count a -> m ()
movePtrToPtr (Ptr srcAddr#) srcOff (Ptr dstAddr#) dstOff c =
  unsafeIOToPrim $
  memmoveAddr#
    srcAddr#
    (fromOff# c srcOff)
    dstAddr#
    (fromOff# c dstOff)
    (fromCount# c)
{-# INLINE[0] movePtrToPtr #-}


-- | Offsets are in bytes
copyPtrToMBytes8 ::
     MonadPrim s m => Ptr Word8 -> Int -> MBytes pd s -> Int -> Count Word8 -> m ()
copyPtrToMBytes8 (Ptr srcAddr#) (I# srcOff#) (MBytes dst#) (I# dstOff#) (Count (I# n#)) =
  prim_ (copyAddrToByteArray# (srcAddr# `plusAddr#` srcOff#) dst# dstOff# n#)
{-# INLINE copyPtrToMBytes8 #-}
{-# RULES "copyPtrToMBytes8" copyPtrToMBytes = copyPtrToMBytes8 #-}

copyPtrToMBytes ::
     (MonadPrim s m, Prim a) => Ptr a -> Int -> MBytes pd s -> Int -> Count a -> m ()
copyPtrToMBytes (Ptr srcAddr#) srcOff (MBytes dst#) dstOff c =
  let src# = srcAddr# `plusAddr#` fromOff# c srcOff
   in prim_ $ copyAddrToByteArray# src# dst# (fromOff# c dstOff) (fromCount# c)
{-# INLINE[0] copyPtrToMBytes #-}


copyBytesToPtr8 :: MonadPrim s m => Bytes p -> Int -> Ptr Word8 -> Int -> Count Word8 -> m ()
copyBytesToPtr8 (Bytes src#) (I# srcOff#) (Ptr dstAddr#) (I# dstOff#) (Count (I# n#)) =
  let addr# = dstAddr# `plusAddr#` dstOff#
  in prim_ (copyByteArrayToAddr# src# srcOff# addr# n#)
{-# INLINE copyBytesToPtr8 #-}
{-# RULES "copyBytesToPtr8" copyBytesToPtr = copyBytesToPtr8 #-}

copyBytesToPtr :: (MonadPrim s m, Prim a) => Bytes p -> Int -> Ptr a -> Int -> Count a -> m ()
copyBytesToPtr (Bytes src#) srcOff (Ptr dstAddr#) dstOff c =
  let addr# = dstAddr# `plusAddr#` fromOff# c dstOff
  in prim_ (copyByteArrayToAddr# src# (fromOff# c srcOff) addr# (fromCount# c))
{-# INLINE[0] copyBytesToPtr #-}



copyMBytesToPtr8 :: MonadPrim s m => MBytes p s -> Int -> Ptr Word8 -> Int -> Count Word8 -> m ()
copyMBytesToPtr8 (MBytes src#) (I# srcOff#) (Ptr dstAddr#) (I# dstOff#) (Count (I# n#)) =
  let addr# = dstAddr# `plusAddr#` dstOff#
  in prim_ (copyMutableByteArrayToAddr# src# srcOff# addr# n#)
{-# INLINE copyMBytesToPtr8 #-}
{-# RULES "copyMBytesToPtr8" copyMBytesToPtr = copyMBytesToPtr8 #-}

copyMBytesToPtr :: (MonadPrim s m, Prim a) => MBytes p s -> Int -> Ptr a -> Int -> Count a -> m ()
copyMBytesToPtr (MBytes src#) srcOff (Ptr dstAddr#) dstOff c =
  let addr# = dstAddr# `plusAddr#` fromOff# c dstOff
   in prim_ $
      copyMutableByteArrayToAddr# src# (fromOff# c srcOff) addr# (fromCount# c)
{-# INLINE[0] copyMBytesToPtr #-}




movePtrToMBytes8 :: MonadPrim s m => Ptr Word8 -> Int -> MBytes p s -> Int -> Count Word8 -> m ()
movePtrToMBytes8 (Ptr srcAddr#) (I# srcOff#) (MBytes dst#) (I# dstOff#) (Count (I# n#)) =
  unsafeIOToPrim $
  memmoveMutableByteArrayFromAddr# srcAddr# srcOff# dst# dstOff# n#
{-# INLINE movePtrToMBytes8 #-}
{-# RULES "movePtrToMBytes8" movePtrToMBytes = movePtrToMBytes8 #-}

movePtrToMBytes :: (MonadPrim s m, Prim a) => Ptr a -> Int -> MBytes p s -> Int -> Count a -> m ()
movePtrToMBytes (Ptr srcAddr#) srcOff (MBytes dst#) dstOff c =
  unsafeIOToPrim $
  memmoveMutableByteArrayFromAddr#
    srcAddr#
    (fromOff# c srcOff)
    dst#
    (fromOff# c dstOff)
    (fromCount# c)
{-# INLINE[0] movePtrToMBytes #-}



moveMBytesToPtr8 :: MonadPrim s m => MBytes p s -> Int -> Ptr Word8 -> Int -> Count Word8 -> m ()
moveMBytesToPtr8 (MBytes src#) (I# srcOff#) (Ptr dstAddr#) (I# dstOff#) (Count (I# n#)) =
  unsafeIOToPrim $
  memmoveMutableByteArrayToAddr# src# srcOff# dstAddr# dstOff# n#
{-# INLINE moveMBytesToPtr8 #-}
{-# RULES "moveMBytesToPtr8" moveMBytesToPtr = moveMBytesToPtr8 #-}


moveMBytesToPtr :: (MonadPrim s m, Prim a) => MBytes p s -> Int -> Ptr a -> Int -> Count a -> m ()
moveMBytesToPtr (MBytes src#) srcOff (Ptr dstAddr#) dstOff c =
  unsafeIOToPrim $
  memmoveMutableByteArrayToAddr#
    src#
    (fromOff# c srcOff)
    dstAddr#
    (fromOff# c dstOff)
    (fromCount# c)
{-# INLINE[0] moveMBytesToPtr #-}
