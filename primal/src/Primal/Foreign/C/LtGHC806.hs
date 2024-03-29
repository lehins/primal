{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
-- |
-- Module      : Primal.Foreign.C.LtGHC806
-- Copyright   : (c) Alexey Kuleshevich 2020-2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Foreign.C.LtGHC806
  (
  -- ** GHC 8.6
    indexWord8ArrayAsChar#
  , readWord8ArrayAsChar#
  , writeWord8ArrayAsChar#
  , indexWord8ArrayAsWideChar#
  , readWord8ArrayAsWideChar#
  , writeWord8ArrayAsWideChar#
  , indexWord8ArrayAsAddr#
  , readWord8ArrayAsAddr#
  , writeWord8ArrayAsAddr#
  , indexWord8ArrayAsStablePtr#
  , readWord8ArrayAsStablePtr#
  , writeWord8ArrayAsStablePtr#
  , indexWord8ArrayAsFloat#
  , readWord8ArrayAsFloat#
  , writeWord8ArrayAsFloat#
  , indexWord8ArrayAsDouble#
  , readWord8ArrayAsDouble#
  , writeWord8ArrayAsDouble#
  , indexWord8ArrayAsInt16#
  , readWord8ArrayAsInt16#
  , writeWord8ArrayAsInt16#
  , indexWord8ArrayAsInt32#
  , readWord8ArrayAsInt32#
  , writeWord8ArrayAsInt32#
  , indexWord8ArrayAsInt64#
  , readWord8ArrayAsInt64#
  , writeWord8ArrayAsInt64#
  , indexWord8ArrayAsInt#
  , readWord8ArrayAsInt#
  , writeWord8ArrayAsInt#
  , indexWord8ArrayAsWord16#
  , readWord8ArrayAsWord16#
  , writeWord8ArrayAsWord16#
  , indexWord8ArrayAsWord32#
  , readWord8ArrayAsWord32#
  , writeWord8ArrayAsWord32#
  , indexWord8ArrayAsWord64#
  , readWord8ArrayAsWord64#
  , writeWord8ArrayAsWord64#
  , indexWord8ArrayAsWord#
  , readWord8ArrayAsWord#
  , writeWord8ArrayAsWord#

  , atomicModifyMutVar_#
  , atomicModifyMutVar2#
  , module Primal.Foreign.C.LtGHC802
  ) where

import GHC.Exts
import Primal.Foreign.C.LtGHC802
#if __GLASGOW_HASKELL__ < 806

import GHC.Int
import GHC.Word
import Primal.Foreign.StablePtr
import Primal.Monad.Unsafe

#include "MachDeps.h"
#endif


#if __GLASGOW_HASKELL__ <= 806

-- | Slightly slower reimplementation of newer primops using the old `atomicModifyMutVar#`
--
-- Modify the contents of a `MutVar#`, returning the previous contents and the result of
-- applying the given function to the previous contents.
--
-- /__Warning:__/ this can fail with an unchecked exception.
atomicModifyMutVar_# :: MutVar# s a -> (a -> a) -> State# s -> (# State# s, a, a #)
atomicModifyMutVar_# ref# f s =
  case atomicModifyMutVar# ref# (\a -> let a' = f a in (a', (a', a))) s of
    (# s', (prev, cur) #) -> (# s', prev, cur #)
{-# INLINE atomicModifyMutVar_# #-}

-- | Slightly slower reimplementation of newer primops using the old `atomicModifyMutVar#`
--
-- Modify the contents of a `MutVar#`, returning the previous contents and the result of
-- applying the given function to the previous contents.
--
-- /__Warning:__/ this can fail with an unchecked exception.
atomicModifyMutVar2# :: MutVar# s a -> (a -> (a, b)) -> State# s -> (# State# s, a, (a, b) #)
atomicModifyMutVar2# ref# f s =
  case atomicModifyMutVar# ref# (\a -> let (a', b) = f a in (a', (a', a, b))) s of
    (# s', (prev, cur, artifact) #) -> (# s', prev, (cur, artifact) #)
{-# INLINE atomicModifyMutVar2# #-}

#endif

-- ghc-8.6 (i.e. 806 version) introduced these new functions, for versions before we
-- use their re-implementations in C:
#if __GLASGOW_HASKELL__ < 806

indexWord8ArrayAsChar# :: ByteArray# -> Int# -> Char#
indexWord8ArrayAsChar# = indexCharArray#
{-# INLINE indexWord8ArrayAsChar# #-}

readWord8ArrayAsChar# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Char# #)
readWord8ArrayAsChar# = readCharArray#
{-# INLINE readWord8ArrayAsChar# #-}

writeWord8ArrayAsChar# :: MutableByteArray# d -> Int# -> Char# -> State# d -> State# d
writeWord8ArrayAsChar# = writeCharArray#
{-# INLINE writeWord8ArrayAsChar# #-}

foreign import ccall unsafe "primal_compat.c primal_memread32"
  indexWord8ArrayAsWideChar# :: ByteArray# -> Int# -> Char#

foreign import ccall unsafe "primal_compat.c primal_memread32"
  readWord8ArrayAsWideCharIO# :: MutableByteArray# d -> Int# -> IO Char

readWord8ArrayAsWideChar# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Char# #)
readWord8ArrayAsWideChar# mb# i# s =
  case unsafePrimalState (readWord8ArrayAsWideCharIO# mb# i#) s of
    (# s', C# c# #) -> (# s', c# #)
{-# INLINE readWord8ArrayAsWideChar# #-}

foreign import ccall unsafe "primal_compat.c primal_memwrite32"
  writeWord8ArrayAsWideCharIO# :: MutableByteArray# d -> Int# -> Char# -> IO ()

writeWord8ArrayAsWideChar# :: MutableByteArray# d -> Int# -> Char# -> State# d -> State# d
writeWord8ArrayAsWideChar# mb# i# c# = unsafePrimalState_ (writeWord8ArrayAsWideCharIO# mb# i# c#)
{-# INLINE writeWord8ArrayAsWideChar# #-}

-- Addr#

#if SIZEOF_HSPTR == SIZEOF_INT64
foreign import ccall unsafe "primal_compat.c primal_memread64"
  indexWord8ArrayAsAddr# :: ByteArray# -> Int# -> Addr#

foreign import ccall unsafe "primal_compat.c primal_memread64"
  readWord8ArrayAsPtrIO# :: MutableByteArray# d -> Int# -> IO (Ptr a)

foreign import ccall unsafe "primal_compat.c primal_memwrite64"
  writeWord8ArrayAsAddrIO# :: MutableByteArray# d -> Int# -> Addr# -> IO ()


#elif SIZEOF_HSPTR == SIZEOF_INT32
foreign import ccall unsafe "primal_compat.c primal_memread32"
  indexWord8ArrayAsAddr# :: ByteArray# -> Int# -> Addr#

foreign import ccall unsafe "primal_compat.c primal_memread32"
  readWord8ArrayAsPtrIO# :: MutableByteArray# d -> Int# -> IO (Ptr a)

foreign import ccall unsafe "primal_compat.c primal_memwrite32
  writeWord8ArrayAsAddrIO# :: MutableByteArray# d -> Int# -> Addr# -> IO ()
#else
#error Ptr is of unsupported size SIZEOF_HSPTR
#endif

readWord8ArrayAsAddr# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Addr# #)
readWord8ArrayAsAddr# mb# i# s =
  case unsafePrimalState (readWord8ArrayAsPtrIO# mb# i#) s of
    (# s', Ptr addr# #) -> (# s', addr# #)
{-# INLINE readWord8ArrayAsAddr# #-}

writeWord8ArrayAsAddr# :: MutableByteArray# d -> Int# -> Addr# -> State# d -> State# d
writeWord8ArrayAsAddr# mb# i# addr# = unsafePrimalState_ (writeWord8ArrayAsAddrIO# mb# i# addr#)
{-# INLINE writeWord8ArrayAsAddr# #-}

-- StablePtr#

#if SIZEOF_HSSTABLEPTR == SIZEOF_INT64
foreign import ccall unsafe "primal_compat.c primal_memread64"
  indexWord8ArrayAsStablePtr# :: ByteArray# -> Int# -> StablePtr# a

foreign import ccall unsafe "primal_compat.c primal_memread64"
  readWord8ArrayAsStablePtrIO# :: MutableByteArray# d -> Int# -> IO (StablePtr a)

foreign import ccall unsafe "primal_compat.c primal_memwrite64"
  writeWord8ArrayAsStablePtrIO# :: MutableByteArray# d -> Int# -> StablePtr# a -> IO ()
#elif SIZEOF_HSSTABLEPTR == SIZEOF_INT32
foreign import ccall unsafe "primal_compat.c primal_memread32"
  indexWord8ArrayAsStablePtr# :: ByteArray# -> Int# -> StablePtr# a

foreign import ccall unsafe "primal_compat.c primal_memread32"
  readWord8ArrayAsStablePtrIO# :: MutableByteArray# d -> Int# -> IO (StablePtr a)

foreign import ccall unsafe "primal_compat.c primal_memwrite32
  writeWord8ArrayAsStablePtrIO# :: MutableByteArray# d -> Int# -> StablePtr# a -> IO ()
#else
#error StablePtr is of unsupported size SIZEOF_HSPTR
#endif

readWord8ArrayAsStablePtr# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, StablePtr# a #)
readWord8ArrayAsStablePtr# mb# i# s =
  case unsafePrimalState (readWord8ArrayAsStablePtrIO# mb# i#) s of
    (# s', StablePtr addr# #) -> (# s', addr# #)
{-# INLINE readWord8ArrayAsStablePtr# #-}

writeWord8ArrayAsStablePtr# :: MutableByteArray# d -> Int# -> StablePtr# a -> State# d -> State# d
writeWord8ArrayAsStablePtr# mb# i# addr# = unsafePrimalState_ (writeWord8ArrayAsStablePtrIO# mb# i# addr#)
{-# INLINE writeWord8ArrayAsStablePtr# #-}


-- Float#

foreign import ccall unsafe "primal_compat.c primal_memread_float"
  indexWord8ArrayAsFloat# :: ByteArray# -> Int# -> Float#

foreign import ccall unsafe "primal_compat.c primal_memread_float"
  readWord8ArrayAsFloatIO# :: MutableByteArray# d -> Int# -> IO Float

readWord8ArrayAsFloat# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Float# #)
readWord8ArrayAsFloat# mb# i# s =
  case unsafePrimalState (readWord8ArrayAsFloatIO# mb# i#) s of
    (# s', F# a# #) -> (# s', a# #)
{-# INLINE readWord8ArrayAsFloat# #-}

foreign import ccall unsafe "primal_compat.c primal_memwrite_float"
  writeWord8ArrayAsFloatIO# :: MutableByteArray# d -> Int# -> Float -> IO ()

writeWord8ArrayAsFloat# :: MutableByteArray# d -> Int# -> Float# -> State# d -> State# d
writeWord8ArrayAsFloat# mb# i# a# = unsafePrimalState_ (writeWord8ArrayAsFloatIO# mb# i# (F# a#))
{-# INLINE writeWord8ArrayAsFloat# #-}

-- Double#

foreign import ccall unsafe "primal_compat.c primal_memread_double"
  indexWord8ArrayAsDouble# :: ByteArray# -> Int# -> Double#

foreign import ccall unsafe "primal_compat.c primal_memread_double"
  readWord8ArrayAsDoubleIO# :: MutableByteArray# d -> Int# -> IO Double

readWord8ArrayAsDouble# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Double# #)
readWord8ArrayAsDouble# mb# i# s =
  case unsafePrimalState (readWord8ArrayAsDoubleIO# mb# i#) s of
    (# s', D# a# #) -> (# s', a# #)
{-# INLINE readWord8ArrayAsDouble# #-}

foreign import ccall unsafe "primal_compat.c primal_memwrite_double"
  writeWord8ArrayAsDoubleIO# :: MutableByteArray# d -> Int# -> Double# -> IO ()

writeWord8ArrayAsDouble# :: MutableByteArray# d -> Int# -> Double# -> State# d -> State# d
writeWord8ArrayAsDouble# mb# i# a# = unsafePrimalState_ (writeWord8ArrayAsDoubleIO# mb# i# a#)
{-# INLINE writeWord8ArrayAsDouble# #-}

-- Int16#

foreign import ccall unsafe "primal_compat.c primal_memread16"
  indexWord8ArrayAsInt16 :: ByteArray# -> Int# -> Int16

indexWord8ArrayAsInt16# :: ByteArray# -> Int# -> Int#
indexWord8ArrayAsInt16# ba i = case indexWord8ArrayAsInt16 ba i of
                                 I16# a# -> a#
{-# INLINE indexWord8ArrayAsInt16# #-}

foreign import ccall unsafe "primal_compat.c primal_memread16"
  readWord8ArrayAsInt16IO# :: MutableByteArray# d -> Int# -> IO Int16

readWord8ArrayAsInt16# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Int# #)
readWord8ArrayAsInt16# mb# i# s =
  case unsafePrimalState (readWord8ArrayAsInt16IO# mb# i#) s of
    (# s', I16# a# #) -> (# s', a# #)
{-# INLINE readWord8ArrayAsInt16# #-}

foreign import ccall unsafe "primal_compat.c primal_memwrite16"
  writeWord8ArrayAsInt16IO# :: MutableByteArray# d -> Int# -> Int16 -> IO ()

writeWord8ArrayAsInt16# :: MutableByteArray# d -> Int# -> Int# -> State# d -> State# d
writeWord8ArrayAsInt16# mb# i# a# = unsafePrimalState_ (writeWord8ArrayAsInt16IO# mb# i# (I16# a#))
{-# INLINE writeWord8ArrayAsInt16# #-}


-- Int32#

foreign import ccall unsafe "primal_compat.c primal_memread32"
  indexWord8ArrayAsInt32 :: ByteArray# -> Int# -> Int32

indexWord8ArrayAsInt32# :: ByteArray# -> Int# -> Int#
indexWord8ArrayAsInt32# ba i = case indexWord8ArrayAsInt32 ba i of
                                 I32# a# -> a#
{-# INLINE indexWord8ArrayAsInt32# #-}


foreign import ccall unsafe "primal_compat.c primal_memread32"
  readWord8ArrayAsInt32IO# :: MutableByteArray# d -> Int# -> IO Int32

readWord8ArrayAsInt32# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Int# #)
readWord8ArrayAsInt32# mb# i# s =
  case unsafePrimalState (readWord8ArrayAsInt32IO# mb# i#) s of
    (# s', I32# a# #) -> (# s', a# #)
{-# INLINE readWord8ArrayAsInt32# #-}

foreign import ccall unsafe "primal_compat.c primal_memwrite32"
  writeWord8ArrayAsInt32IO# :: MutableByteArray# d -> Int# -> Int32 -> IO ()

writeWord8ArrayAsInt32# :: MutableByteArray# d -> Int# -> Int# -> State# d -> State# d
writeWord8ArrayAsInt32# mb# i# a# = unsafePrimalState_ (writeWord8ArrayAsInt32IO# mb# i# (I32# a#))
{-# INLINE writeWord8ArrayAsInt32# #-}


-- Int64#

foreign import ccall unsafe "primal_compat.c primal_memread64"
  readWord8ArrayAsInt64IO# :: MutableByteArray# d -> Int# -> IO Int64

#if WORD_SIZE_IN_BITS >= 64
foreign import ccall unsafe "primal_compat.c primal_memread64"
  indexWord8ArrayAsInt64# :: ByteArray# -> Int# -> Int#
foreign import ccall unsafe "primal_compat.c primal_memwrite64"
  writeWord8ArrayAsInt64IO# :: MutableByteArray# d -> Int# -> Int# -> IO ()

readWord8ArrayAsInt64# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Int# #)
writeWord8ArrayAsInt64# :: MutableByteArray# d -> Int# -> Int# -> State# d -> State# d

#else
foreign import ccall unsafe "primal_compat.c primal_memread64"
  indexWord8ArrayAsInt64# :: ByteArray# -> Int# -> Int64#
foreign import ccall unsafe "primal_compat.c primal_memwrite64"
  writeWord8ArrayAsInt64IO# :: MutableByteArray# d -> Int# -> Int64# -> IO ()

readWord8ArrayAsInt64# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Int64# #)
writeWord8ArrayAsInt64# :: MutableByteArray# d -> Int# -> Int64# -> State# d -> State# d
#endif

readWord8ArrayAsInt64# mb# i# s =
  case unsafePrimalState (readWord8ArrayAsInt64IO# mb# i#) s of
    (# s', I64# a# #) -> (# s', a# #)
{-# INLINE readWord8ArrayAsInt64# #-}

writeWord8ArrayAsInt64# mb# i# a# = unsafePrimalState_ (writeWord8ArrayAsInt64IO# mb# i# a#)
{-# INLINE writeWord8ArrayAsInt64# #-}

-- Int#

#if WORD_SIZE_IN_BITS >= 64
foreign import ccall unsafe "primal_compat.c primal_memread64"
  indexWord8ArrayAsInt# :: ByteArray# -> Int# -> Int#

foreign import ccall unsafe "primal_compat.c primal_memread64"
  readWord8ArrayAsIntIO# :: MutableByteArray# d -> Int# -> IO Int

foreign import ccall unsafe "primal_compat.c primal_memwrite64"
  writeWord8ArrayAsIntIO# :: MutableByteArray# d -> Int# -> Int# -> IO ()
#else
foreign import ccall unsafe "primal_compat.c primal_memread32"
  indexWord8ArrayAsInt# :: ByteArray# -> Int# -> Int#

foreign import ccall unsafe "primal_compat.c primal_memread32"
  readWord8ArrayAsIntIO# :: MutableByteArray# d -> Int# -> IO Int64

foreign import ccall unsafe "primal_compat.c primal_memwrite32"
  writeWord8ArrayAsIntIO# :: MutableByteArray# d -> Int# -> Int# -> IO ()
#endif

readWord8ArrayAsInt# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Int# #)
readWord8ArrayAsInt# mb# i# s =
  case unsafePrimalState (readWord8ArrayAsIntIO# mb# i#) s of
    (# s', I# a# #) -> (# s', a# #)
{-# INLINE readWord8ArrayAsInt# #-}

writeWord8ArrayAsInt# :: MutableByteArray# d -> Int# -> Int# -> State# d -> State# d
writeWord8ArrayAsInt# mb# i# a# = unsafePrimalState_ (writeWord8ArrayAsIntIO# mb# i# a#)
{-# INLINE writeWord8ArrayAsInt# #-}

-- Word16#

foreign import ccall unsafe "primal_compat.c primal_memread16"
  indexWord8ArrayAsWord16# :: ByteArray# -> Int# -> Word#

foreign import ccall unsafe "primal_compat.c primal_memread16"
  readWord8ArrayAsWord16IO# :: MutableByteArray# d -> Int# -> IO Word16

readWord8ArrayAsWord16# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Word# #)
readWord8ArrayAsWord16# mb# i# s =
  case unsafePrimalState (readWord8ArrayAsWord16IO# mb# i#) s of
    (# s', W16# a# #) -> (# s', a# #)
{-# INLINE readWord8ArrayAsWord16# #-}

foreign import ccall unsafe "primal_compat.c primal_memwrite16"
  writeWord8ArrayAsWord16IO# :: MutableByteArray# d -> Int# -> Word# -> IO ()

writeWord8ArrayAsWord16# :: MutableByteArray# d -> Int# -> Word# -> State# d -> State# d
writeWord8ArrayAsWord16# mb# i# a# = unsafePrimalState_ (writeWord8ArrayAsWord16IO# mb# i# a#)
{-# INLINE writeWord8ArrayAsWord16# #-}

-- Word32#

foreign import ccall unsafe "primal_compat.c primal_memread32"
  indexWord8ArrayAsWord32# :: ByteArray# -> Int# -> Word#

foreign import ccall unsafe "primal_compat.c primal_memread32"
  readWord8ArrayAsWord32IO# :: MutableByteArray# d -> Int# -> IO Word32

readWord8ArrayAsWord32# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Word# #)
readWord8ArrayAsWord32# mb# i# s =
  case unsafePrimalState (readWord8ArrayAsWord32IO# mb# i#) s of
    (# s', W32# a# #) -> (# s', a# #)
{-# INLINE readWord8ArrayAsWord32# #-}

foreign import ccall unsafe "primal_compat.c primal_memwrite32"
  writeWord8ArrayAsWord32IO# :: MutableByteArray# d -> Int# -> Word# -> IO ()

writeWord8ArrayAsWord32# :: MutableByteArray# d -> Int# -> Word# -> State# d -> State# d
writeWord8ArrayAsWord32# mb# i# a# = unsafePrimalState_ (writeWord8ArrayAsWord32IO# mb# i# a#)
{-# INLINE writeWord8ArrayAsWord32# #-}


-- Word64#

foreign import ccall unsafe "primal_compat.c primal_memread64"
  readWord8ArrayAsWord64IO# :: MutableByteArray# d -> Int# -> IO Word64

#if WORD_SIZE_IN_BITS >= 64
foreign import ccall unsafe "primal_compat.c primal_memread64"
  indexWord8ArrayAsWord64# :: ByteArray# -> Int# -> Word#
foreign import ccall unsafe "primal_compat.c primal_memwrite64"
  writeWord8ArrayAsWord64IO# :: MutableByteArray# d -> Int# -> Word# -> IO ()

readWord8ArrayAsWord64# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Word# #)
writeWord8ArrayAsWord64# :: MutableByteArray# d -> Int# -> Word# -> State# d -> State# d

#else
foreign import ccall unsafe "primal_compat.c primal_memread64"
  indexWord8ArrayAsWord64# :: ByteArray# -> Int# -> Word64#
foreign import ccall unsafe "primal_compat.c primal_memwrite64"
  writeWord8ArrayAsWord64IO# :: MutableByteArray# d -> Int# -> Word64# -> IO ()

readWord8ArrayAsWord64# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Word64# #)
writeWord8ArrayAsWord64# :: MutableByteArray# d -> Int# -> Word64# -> State# d -> State# d
#endif

readWord8ArrayAsWord64# mb# i# s =
  case unsafePrimalState (readWord8ArrayAsWord64IO# mb# i#) s of
    (# s', W64# a# #) -> (# s', a# #)
{-# INLINE readWord8ArrayAsWord64# #-}
writeWord8ArrayAsWord64# mb# i# a# = unsafePrimalState_ (writeWord8ArrayAsWord64IO# mb# i# a#)
{-# INLINE writeWord8ArrayAsWord64# #-}

-- Word#

#if WORD_SIZE_IN_BITS >= 64
foreign import ccall unsafe "primal_compat.c primal_memread64"
  indexWord8ArrayAsWord# :: ByteArray# -> Int# -> Word#

foreign import ccall unsafe "primal_compat.c primal_memread64"
  readWord8ArrayAsWordIO# :: MutableByteArray# d -> Int# -> IO Word

foreign import ccall unsafe "primal_compat.c primal_memwrite64"
  writeWord8ArrayAsWordIO# :: MutableByteArray# d -> Int# -> Word# -> IO ()
#else
foreign import ccall unsafe "primal_compat.c primal_memread32"
  indexWord8ArrayAsWord# :: ByteArray# -> Int# -> Word#

foreign import ccall unsafe "primal_compat.c primal_memread32"
  readWord8ArrayAsWordIO# :: MutableByteArray# d -> Int# -> IO Word64

foreign import ccall unsafe "primal_compat.c primal_memwrite32"
  writeWord8ArrayAsWordIO# :: MutableByteArray# d -> Int# -> Word# -> IO ()
#endif

readWord8ArrayAsWord# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Word# #)
readWord8ArrayAsWord# mb# i# s =
  case unsafePrimalState (readWord8ArrayAsWordIO# mb# i#) s of
    (# s', W# a# #) -> (# s', a# #)
{-# INLINE readWord8ArrayAsWord# #-}

writeWord8ArrayAsWord# :: MutableByteArray# d -> Int# -> Word# -> State# d -> State# d
writeWord8ArrayAsWord# mb# i# a# = unsafePrimalState_ (writeWord8ArrayAsWordIO# mb# i# a#)
{-# INLINE writeWord8ArrayAsWord# #-}

#endif /* __GLASGOW_HASKELL__ < 806 */
