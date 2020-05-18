{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Foreign.Prim
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Foreign.Prim
  ( -- * Missing primitives
    unsafeThawByteArray#
  , mutableByteArrayContents#
  , unsafeThawArrayArray#
  , unInt#
  , unWord#
    -- * Primitive
  , module Foreign.Prim.C
  , module Foreign.Prim.Cmm
    -- * Re-exports
  , module Foreign.C.Types
  , module System.Posix.Types
  , module GHC.Exts
#if __GLASGOW_HASKELL__ < 804
  , module GHC.Prim
#endif
  , module GHC.Int
  , module GHC.Word
  ) where

import Foreign.Prim.C
import Foreign.Prim.Cmm
import Foreign.C.Types
import System.Posix.Types
import GHC.Exts
import GHC.Int
import GHC.Word
#if __GLASGOW_HASKELL__ < 804
import GHC.Prim
  ( addCFinalizerToWeak#
  , deRefWeak#
  , finalizeWeak#
  , mkWeak#
  , mkWeakNoFinalizer#
  )
#endif

unsafeThawByteArray# :: ByteArray# -> State# s -> (# State# s, MutableByteArray# s #)
unsafeThawByteArray# ba# s = (# s, unsafeCoerce# ba# #)
{-# INLINE unsafeThawByteArray# #-}

mutableByteArrayContents# :: MutableByteArray# s -> Addr#
mutableByteArrayContents# mba# = byteArrayContents# (unsafeCoerce# mba#)
{-# INLINE mutableByteArrayContents# #-}


unsafeThawArrayArray# :: ArrayArray# -> State# s -> (# State# s, MutableArrayArray# s #)
unsafeThawArrayArray# ba# s = (# s, unsafeCoerce# ba# #)
{-# INLINE unsafeThawArrayArray# #-}



unInt# :: Int -> Int#
unInt# (I# i#) = i#
{-# INLINE unInt# #-}

unWord# :: Word -> Word#
unWord# (W# w#) = w#
{-# INLINE unWord# #-}
