{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Primal.Foreign
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Foreign
  ( -- * Missing primitives
    unsafeThawByteArray#
  , mutableByteArrayContents#
  , unsafeThawArrayArray#
  , unInt#
  , unWord#
  , touch#
  , keepAlive#
    -- * Primitive
  , module Primal.Foreign.C
  , module Primal.Foreign.Cmm
    -- * Re-exports
  , RW
  , IO(..)
  , unIO
  , unIO_
  , ST(..)
  , unST
  , unST_
  , module Foreign.C.Types
  , module System.Posix.Types
  , module GHC.Exts
#if __GLASGOW_HASKELL__ < 804
  , module GHC.Prim
#endif
  , module GHC.Int
  , module GHC.Word
  ) where

import Primal.Eval
import Primal.Monad.Internal
import Primal.Foreign.C
import Primal.Foreign.Cmm
import Foreign.C.Types
import System.Posix.Types
import GHC.Exts hiding (touch#, keepAlive#)
import GHC.Int
import GHC.Word
import GHC.IO
import GHC.ST
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
