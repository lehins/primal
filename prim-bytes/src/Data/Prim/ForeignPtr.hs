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
  ( ForeignPtr(..)
  , readForeignPtr
  , readOffForeignPtr
  , writeForeignPtr
  , writeOffForeignPtr

  , copyForeignPtrToPtr
  , movePtrToPtr
  , copyPtrToMBytes
  , movePtrToMBytes
  , copyBytesToPtr
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



readOffPtr :: (MonadPrim s m, Prim a) => Ptr a -> Off a -> m a
readOffPtr (Ptr addr#) (Off (I# i#)) = prim (readOffAddr# addr# i#)
{-# INLINE readOffPtr #-}

writeOffPtr :: (MonadPrim s m, Prim a) => Ptr a -> Off a -> a -> m ()
writeOffPtr (Ptr addr#) (Off (I# i#)) a = prim_ (writeOffAddr# addr# i# a)
{-# INLINE writeOffPtr #-}

readPtr :: (MonadPrim s m, Prim a) => Ptr a -> m a
readPtr (Ptr addr#) = prim (readOffAddr# addr# 0#)
{-# INLINE readPtr #-}

writePtr :: (MonadPrim s m, Prim a) => Ptr a -> a -> m ()
writePtr (Ptr addr#) a = prim_ (writeOffAddr# addr# 0# a)
{-# INLINE writePtr #-}

plusPtrOff :: Prim a => Ptr a -> Off a -> Ptr a
plusPtrOff (Ptr addr#) off = Ptr (addr# `plusAddr#` fromOff# off)
{-# INLINE plusPtrOff #-}

-- copyPtrToPtr8 ::
--   MonadPrim s m => Ptr Word8 -> Off Word8 -> Ptr Word8 -> Off Word8 -> Count Word8 -> m ()
-- copyPtrToPtr8 srcPtr srcOff dstPtr dstOff (Count n) =
--   unsafeIOToPrim $
--   copyBytes (dstPtr `plusPtrOff8` dstOff) (srcPtr `plusPtrOff8` srcOff) n
-- {-# INLINE copyPtrToPtr8 #-}
-- {-# RULES "copyPtrToPtr8" copyPtrToPtr = copyPtrToPtr8 #-}

copyPtrToPtr :: (MonadPrim s m, Prim a) => Ptr a -> Off a -> Ptr a -> Off a -> Count a -> m ()
copyPtrToPtr srcPtr srcOff dstPtr dstOff c =
  unsafeIOToPrim $
  copyBytes
    (dstPtr `plusPtrOff` dstOff)
    (srcPtr `plusPtrOff` srcOff)
    (fromCount c)
{-# INLINE[0] copyPtrToPtr #-}



-- movePtrToPtr8 ::
--   MonadPrim s m => Ptr Word8 -> Off Word8 -> Ptr Word8 -> Off Word8 -> Count Word8 -> m ()
-- movePtrToPtr8 (Ptr srcAddr#) (Off (I# srcOff#)) (Ptr dstAddr#) (Off (I# dstOff#)) (Count (I# n#)) =
--   unsafeIOToPrim $ memmoveAddr# srcAddr# srcOff# dstAddr# dstOff# n#
-- {-# INLINE movePtrToPtr8 #-}
-- {-# RULES "movePtrToPtr8" movePtrToPtr = movePtrToPtr8 #-}

movePtrToPtr :: (MonadPrim s m, Prim a) => Ptr a -> Off a -> Ptr a -> Off a -> Count a -> m ()
movePtrToPtr (Ptr srcAddr#) srcOff (Ptr dstAddr#) dstOff c =
  unsafeIOToPrim $
  memmoveAddr#
    srcAddr#
    (fromOff# srcOff)
    dstAddr#
    (fromOff# dstOff)
    (fromCount# c)
{-# INLINE[0] movePtrToPtr #-}


-- -- | Offsets are in bytes
-- copyPtrToMBytes8 ::
--      MonadPrim s m => Ptr Word8 -> Int -> MBytes pd s -> Int -> Count Word8 -> m ()
-- copyPtrToMBytes8 (Ptr srcAddr#) (I# srcOff#) (MBytes dst#) (I# dstOff#) (Count (I# n#)) =
--   prim_ (copyAddrToByteArray# (srcAddr# `plusAddr#` srcOff#) dst# dstOff# n#)
-- {-# INLINE copyPtrToMBytes8 #-}
-- {-# RULES "copyPtrToMBytes8" copyPtrToMBytes = copyPtrToMBytes8 #-}

copyPtrToMBytes ::
     (MonadPrim s m, Prim a) => Ptr a -> Off a -> MBytes pd s -> Off a -> Count a -> m ()
copyPtrToMBytes srcPtr srcOff (MBytes dst#) dstOff c =
  let Ptr addr# = srcPtr `plusPtrOff` srcOff
   in prim_ $ copyAddrToByteArray# addr# dst# (fromOff# dstOff) (fromCount# c)
{-# INLINE copyPtrToMBytes #-}


-- copyBytesToPtr8 ::
--   MonadPrim s m => Bytes p -> Off Word8 -> Ptr Word8 -> Off Word8 -> Count Word8 -> m ()
-- copyBytesToPtr8 (Bytes src#) (Off (I# srcOff#)) dstPtr dstOff (Count (I# n#)) =
--   let Ptr addr# = dstPtr `plusPtrOff8` dstOff
--   in prim_ (copyByteArrayToAddr# src# srcOff# addr# n#)
-- {-# INLINE copyBytesToPtr8 #-}
-- {-# RULES "copyBytesToPtr8" copyBytesToPtr = copyBytesToPtr8 #-}

copyBytesToPtr :: (MonadPrim s m, Prim a) => Bytes p -> Off a -> Ptr a -> Off a -> Count a -> m ()
copyBytesToPtr (Bytes src#) srcOff dstPtr dstOff c =
  let Ptr addr# = dstPtr `plusPtrOff` dstOff
  in prim_ (copyByteArrayToAddr# src# (fromOff# srcOff) addr# (fromCount# c))
{-# INLINE[0] copyBytesToPtr #-}



-- copyMBytesToPtr8 ::
--   MonadPrim s m => MBytes p s -> Off Word8 -> Ptr Word8 -> Off Word8 -> Count Word8 -> m ()
-- copyMBytesToPtr8 (MBytes src#) (Off (I# srcOff#)) dstPtr dstOff (Count (I# n#)) =
--   let Ptr addr# = dstPtr `plusPtrOff8` dstOff
--   in prim_ (copyMutableByteArrayToAddr# src# srcOff# addr# n#)
-- {-# INLINE copyMBytesToPtr8 #-}
-- {-# RULES "copyMBytesToPtr8" copyMBytesToPtr = copyMBytesToPtr8 #-}

copyMBytesToPtr :: (MonadPrim s m, Prim a) => MBytes p s -> Off a -> Ptr a -> Off a -> Count a -> m ()
copyMBytesToPtr (MBytes src#) srcOff dstPtr dstOff c =
  let Ptr addr# = dstPtr `plusPtrOff` dstOff
   in prim_ $
      copyMutableByteArrayToAddr# src# (fromOff# srcOff) addr# (fromCount# c)
{-# INLINE[0] copyMBytesToPtr #-}




-- movePtrToMBytes8 ::
--   MonadPrim s m => Ptr Word8 -> Off Word8 -> MBytes p s -> Off Word8 -> Count Word8 -> m ()
-- movePtrToMBytes8 (Ptr srcAddr#) (Off (I# srcOff#)) (MBytes dst#) (Off (I# dstOff#)) (Count (I# n#)) =
--   unsafeIOToPrim $
--   memmoveMutableByteArrayFromAddr# srcAddr# srcOff# dst# dstOff# n#
-- {-# INLINE movePtrToMBytes8 #-}
-- {-# RULES "movePtrToMBytes8" movePtrToMBytes = movePtrToMBytes8 #-}

movePtrToMBytes :: (MonadPrim s m, Prim a) => Ptr a -> Off a -> MBytes p s -> Off a -> Count a -> m ()
movePtrToMBytes (Ptr srcAddr#) srcOff (MBytes dst#) dstOff c =
  unsafeIOToPrim $
  memmoveMutableByteArrayFromAddr#
    srcAddr#
    (fromOff# srcOff)
    dst#
    (fromOff# dstOff)
    (fromCount# c)
{-# INLINE[0] movePtrToMBytes #-}



-- moveMBytesToPtr8 ::
--   MonadPrim s m => MBytes p s -> Off Word8 -> Ptr Word8 -> Off Word8 -> Count Word8 -> m ()
-- moveMBytesToPtr8 (MBytes src#) (Off (I# srcOff#)) (Ptr dstAddr#) (Off (I# dstOff#)) (Count (I# n#)) =
--   unsafeIOToPrim $
--   memmoveMutableByteArrayToAddr# src# srcOff# dstAddr# dstOff# n#
-- {-# INLINE moveMBytesToPtr8 #-}
-- {-# RULES "moveMBytesToPtr8" moveMBytesToPtr = moveMBytesToPtr8 #-}


moveMBytesToPtr :: (MonadPrim s m, Prim a) => MBytes p s -> Off a -> Ptr a -> Off a -> Count a -> m ()
moveMBytesToPtr (MBytes src#) srcOff (Ptr dstAddr#) dstOff c =
  unsafeIOToPrim $
  memmoveMutableByteArrayToAddr#
    src#
    (fromOff# srcOff)
    dstAddr#
    (fromOff# dstOff)
    (fromCount# c)
{-# INLINE[0] moveMBytesToPtr #-}
