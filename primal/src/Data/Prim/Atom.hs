{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Prim.Atom
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Atom
  ( Atom(..)
  -- * SpinLocks
  , acquireLockByteOffMutableByteArray
  , releaseLockByteOffMutableByteArray
  , acquireLockByteOffAddr
  , releaseLockByteOffAddr
  -- ** Expirimental
  , withLockMutableByteArray
  , withLockOffAddr
  -- * Helpers and testing
  --
  -- Functions below are used for implementing `Atom` instances and are useful for
  -- testing other types as well as defining potential cusom instances
  -- ** Count
  , atomicAddFetchOldMutableByteArrayNum#
  , atomicAddFetchNewMutableByteArrayNum#
  , atomicSubFetchOldMutableByteArrayNum#
  , atomicSubFetchNewMutableByteArrayNum#
  , atomicAddFetchOldOffAddrNum#
  , atomicAddFetchNewOffAddrNum#
  , atomicSubFetchOldOffAddrNum#
  , atomicSubFetchNewOffAddrNum#
  -- ** Bits
  , atomicAndFetchOldMutableByteArrayBits#
  , atomicAndFetchNewMutableByteArrayBits#
  , atomicNandFetchOldMutableByteArrayBits#
  , atomicNandFetchNewMutableByteArrayBits#
  , atomicOrFetchOldMutableByteArrayBits#
  , atomicOrFetchNewMutableByteArrayBits#
  , atomicXorFetchOldMutableByteArrayBits#
  , atomicXorFetchNewMutableByteArrayBits#
  , atomicAndFetchOldOffAddrBits#
  , atomicAndFetchNewOffAddrBits#
  , atomicNandFetchOldOffAddrBits#
  , atomicNandFetchNewOffAddrBits#
  , atomicOrFetchOldOffAddrBits#
  , atomicOrFetchNewOffAddrBits#
  , atomicXorFetchOldOffAddrBits#
  , atomicXorFetchNewOffAddrBits#
  ) where

import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Prim.Monad
import Control.Prim.Monad.Unsafe
import Data.Bits
import Data.Prim.Atomic
import Data.Prim.Class
import Foreign.Prim hiding (Any)
import GHC.IO
import GHC.TypeLits

newtype Atom a = Atom { unAtom :: a }
  deriving (Show, Eq, Ord, Num, Enum, Integral, Real, RealFrac, Fractional, Floating, RealFloat, Bits, NFData)


instance Prim a => Prim (Atom a) where
  type PrimBase (Atom a) = Atom a
  type SizeOf (Atom a) = 1 + SizeOf a
  type Alignment (Atom a) = 1 + Alignment a
  sizeOf# _ = 1# +# sizeOf# (proxy# :: Proxy# a)
  {-# INLINE sizeOf# #-}
  alignment# _ = 1# +# alignment# (proxy# :: Proxy# a)
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i# = Atom (indexByteOffByteArray# ba# (1# +# i#))
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# = indexByteOffByteArray# ba# (i# *# sizeOf# (proxy# :: Proxy# (Atom a)))
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# =
    Atom (indexOffAddr# (addr# `plusAddr#` (1# +# i# *# sizeOf# (proxy# :: Proxy# (Atom a)))) 0#)
  {-# INLINE indexOffAddr# #-}
  readByteOffMutableByteArray# mba# i# s =
    case readByteOffMutableByteArray# mba# (1# +# i#) s of
      (# s', a #) -> (# s', Atom a #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readMutableByteArray# mba# i# =
    readByteOffMutableByteArray# mba# (i# *# sizeOf# (proxy# :: Proxy# (Atom a)))
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# addr# i# s =
    case readOffAddr# (addr# `plusAddr#` (1# +# i# *# sizeOf# (proxy# :: Proxy# (Atom a)))) 0# s of
      (# s', a #) -> (# s', Atom a #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i# (Atom a) s =
    writeByteOffMutableByteArray# mba# i# (0 :: Word8)
      (writeByteOffMutableByteArray# mba# (1# +# i#) a s)
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# (Atom a) s =
    let i0# = i# *# sizeOf# (proxy# :: Proxy# (Atom a))
    in writeByteOffMutableByteArray# mba# i0# (0 :: Word8)
         (writeByteOffMutableByteArray# mba# (1# +# i0#) a s)
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# addr# i# (Atom a) s =
    let i0# = i# *# sizeOf# (proxy# :: Proxy# (Atom a))
    in writeOffAddr# addr# i0# (0 :: Word8) (writeOffAddr# (addr# `plusAddr#` (1# +# i0#)) 0# a s)
  {-# INLINE writeOffAddr# #-}
  setMutableByteArray# = setMutableByteArrayLoop#
  {-# INLINE setMutableByteArray# #-}
  setOffAddr# = setOffAddrLoop#
  {-# INLINE setOffAddr# #-}


acquireLockByteOffMutableByteArray :: MutableByteArray# RealWorld -> Int# -> IO ()
acquireLockByteOffMutableByteArray mba# i# =
  let go = do
        locked <- syncLockTestSetInt8ArrayIO mba# i#
        unless (locked == 0) $ yield >> go
   in go
{-# INLINE acquireLockByteOffMutableByteArray #-}

releaseLockByteOffMutableByteArray :: MutableByteArray# RealWorld -> Int# -> IO ()
releaseLockByteOffMutableByteArray mba# i# = syncLockReleaseInt8ArrayIO mba# i#
{-# INLINE releaseLockByteOffMutableByteArray #-}


acquireLockByteOffAddr :: Addr# -> Int# -> IO ()
acquireLockByteOffAddr addr# i# =
  let go = do
        locked <- syncLockTestSetInt8AddrIO addr# i#
        unless (locked == 0) $ yield >> go
   in go
{-# INLINE acquireLockByteOffAddr #-}

releaseLockByteOffAddr :: Addr#-> Int# -> IO ()
releaseLockByteOffAddr addr# i# = syncLockReleaseInt8AddrIO addr# i#
{-# INLINE releaseLockByteOffAddr #-}


withLockMutableByteArray ::
     forall e b. Prim e
  => MutableByteArray# RealWorld
  -> Int#
  -> (Atom e -> IO (Atom e, b))
  -> IO b
withLockMutableByteArray mba# i# f =
  let li# = i# *# sizeOf# (proxy# :: Proxy# (Atom e))
   in bracket_
        (unsafeUnmask (acquireLockByteOffMutableByteArray mba# li#))
        (releaseLockByteOffMutableByteArray mba# li#) $
      IO $ \s ->
        case readByteOffMutableByteArray# mba# (1# +# li#) s of
          (# s', a #) ->
            case f (Atom a) of
              IO m ->
                case m s' of
                  (# s'', (Atom a', b) #) ->
                    (# writeByteOffMutableByteArray# mba# (1# +# li#) a' s'', b #)
{-# INLINABLE withLockMutableByteArray #-}


-- | Atomic reads on `Atom` require a lock because otherwise any other thread can
-- overwrite the contnts in the midst of reading, resulting in a value with contents
-- from both values part before and part after the write.
atomicReadAtomMutableByteArray ::
     forall e. Prim e
  => MutableByteArray# RealWorld
  -> Int#
  -> IO (Atom e)
atomicReadAtomMutableByteArray mba# i# =
  let li# = i# *# sizeOf# (proxy# :: Proxy# (Atom e))
   in bracket_
        (unsafeUnmask (acquireLockByteOffMutableByteArray mba# li#))
        (releaseLockByteOffMutableByteArray mba# li#)
        (coerce (IO (readByteOffMutableByteArray# mba# (1# +# li#)) :: IO e))
{-# INLINABLE atomicReadAtomMutableByteArray #-}

-- | Values are no longer guaranteed to be one word in size, as such in order for writes
-- to be atomic we require locking.
atomicWriteAtomMutableByteArray ::
     forall e. Prim e
  => MutableByteArray# RealWorld
  -> Int#
  -> Atom e
  -> IO ()
atomicWriteAtomMutableByteArray mba# i# (Atom a) =
  let li# = i# *# sizeOf# (proxy# :: Proxy# (Atom e))
   in bracket_
        (unsafeUnmask (acquireLockByteOffMutableByteArray mba# li#))
        (releaseLockByteOffMutableByteArray mba# li#)
        (prim_ (writeByteOffMutableByteArray# mba# (1# +# li#) a))
{-# INLINABLE atomicWriteAtomMutableByteArray #-}



-- | Same as `atomicReadAtomMutableByteArray`, but for `Addr#` with offset
atomicReadAtomOffAddr ::
     forall e. Prim e
  => Addr#
  -> Int#
  -> IO (Atom e)
atomicReadAtomOffAddr mba# i# =
  let li# = i# *# sizeOf# (proxy# :: Proxy# (Atom e))
   in bracket_
        (unsafeUnmask (acquireLockByteOffAddr mba# li#))
        (releaseLockByteOffAddr mba# li#)
        (coerce (IO (readOffAddr# mba# (1# +# li#)) :: IO e))
{-# INLINABLE atomicReadAtomOffAddr #-}

-- | Same as `atomicWriteAtomMutableByteArray`, but for `Addr#` with offset
atomicWriteAtomOffAddr ::
     forall e. Prim e
  => Addr#
  -> Int#
  -> Atom e
  -> IO ()
atomicWriteAtomOffAddr addr# i# (Atom a) =
  let li# = i# *# sizeOf# (proxy# :: Proxy# (Atom e))
      lockAddr# = addr# `plusAddr#` li#
      valAddr# = lockAddr# `plusAddr#` 1#
   in bracket_
        (unsafeUnmask (acquireLockByteOffAddr lockAddr# 0#))
        (releaseLockByteOffAddr lockAddr# 0#)
        (prim_ (writeOffAddr# valAddr# 0# a))
{-# INLINABLE atomicWriteAtomOffAddr #-}



withLockOffAddr ::
     forall e b. Prim e
  => Addr#
  -> Int#
  -> (Atom e -> IO (Atom e, b))
  -> IO b
withLockOffAddr addr# i# f =
  let li# = i# *# sizeOf# (proxy# :: Proxy# (Atom e))
      offAddr# = addr# `plusAddr#` (1# +# li#)
   in bracket_
        (unsafeUnmask (acquireLockByteOffAddr addr# li#))
        (releaseLockByteOffAddr addr# li#) $
      IO $ \s ->
        case readOffAddr# offAddr# 0# s of
          (# s', a #) ->
            case f (Atom a) of
              IO m ->
                case m s' of
                  (# s'', (Atom a', b) #) ->
                    (# writeOffAddr# offAddr# 0# a' s'', b #)
{-# INLINABLE withLockOffAddr #-}

atomicModifyAtomMutableByteArray# ::
     forall e b s. Prim e
  => MutableByteArray# s
  -> Int#
  -> (Atom e -> (# Atom e, b #))
  -> State# s
  -> (# State# s, b #)
atomicModifyAtomMutableByteArray# mba# i# f =
  let li# = i# *# sizeOf# (proxy# :: Proxy# (Atom e))
      mba'# = unsafeCoerce# mba# :: MutableByteArray# RealWorld
   in unsafePrimBase $
      bracket_
        (unsafeUnmask (acquireLockByteOffMutableByteArray mba'# li#))
        (releaseLockByteOffMutableByteArray mba'# li#) $
      IO $ \s ->
        case readByteOffMutableByteArray# mba'# (1# +# li#) s of
          (# s', a #) ->
            case f (Atom a) of
              (# Atom a', b #) ->
                (# writeByteOffMutableByteArray# mba'# (1# +# li#) a' s', b #)
{-# INLINE atomicModifyAtomMutableByteArray#  #-}

atomicModifyAtomOffAddr# ::
     forall e b s. Prim e
  => Addr#
  -> Int#
  -> (Atom e -> (# Atom e, b #))
  -> State# s
  -> (# State# s, b #)
atomicModifyAtomOffAddr# addr# i# f =
  let li# = i# *# sizeOf# (proxy# :: Proxy# (Atom e))
      offAddr# = addr# `plusAddr#` (1# +# li#)
   in unsafePrimBase $
      bracket_
        (unsafeUnmask (acquireLockByteOffAddr addr# li#))
        (releaseLockByteOffAddr addr# li#) $
      IO $ \s ->
        case readOffAddr# offAddr# 0# s of
          (# s', a #) ->
            case f (Atom a) of
              (# Atom a', b #) ->
                (# writeOffAddr# offAddr# 0# a' s', b #)
{-# INLINE atomicModifyAtomOffAddr# #-}


swapIfEqualVal :: Eq e => Atom e -> Atom e -> Atom e -> (# Atom e, Atom e #)
swapIfEqualVal expected new actual
  | expected == actual = (# new, actual #)
  | otherwise = (# actual, actual #)
{-# INLINE swapIfEqualVal #-}

swapIfEqualBool :: Eq e => Atom e -> Atom e -> Atom e -> (# Atom e, Bool #)
swapIfEqualBool expected new actual
  | expected == actual = (# new, True #)
  | otherwise = (# actual, False #)
{-# INLINE swapIfEqualBool #-}

instance (Eq a, Prim a) => Atomic (Atom a) where
  atomicReadMutableByteArray# mba# i# =
    unsafePrimBase (atomicReadAtomMutableByteArray (unsafeCoerce# mba#) i#)
  {-# INLINABLE atomicReadMutableByteArray# #-}
  atomicWriteMutableByteArray# mba# i# a =
    unsafePrimBase_ (atomicWriteAtomMutableByteArray (unsafeCoerce# mba#) i# a)
  {-# INLINABLE atomicWriteMutableByteArray# #-}
  atomicReadOffAddr# addr# i# = unsafePrimBase (atomicReadAtomOffAddr addr# i#)
  {-# INLINABLE atomicReadOffAddr# #-}
  atomicWriteOffAddr# addr# i# a = unsafePrimBase_ (atomicWriteAtomOffAddr addr# i# a)
  {-# INLINABLE atomicWriteOffAddr# #-}
  casMutableByteArray# mba# i# old new =
    atomicModifyAtomMutableByteArray# mba# i# (swapIfEqualVal old new)
  {-# INLINE casMutableByteArray# #-}
  casOffAddr# addr# i# old new =
    atomicModifyOffAddr# addr# i# (swapIfEqualVal old new)
  {-# INLINE casOffAddr# #-}
  casBoolMutableByteArray# mba# i# old new =
    atomicModifyAtomMutableByteArray# mba# i# (swapIfEqualBool old new)
  {-# INLINE casBoolMutableByteArray# #-}
  casBoolOffAddr# addr# i# old new =
    atomicModifyOffAddr# addr# i# (swapIfEqualBool old new)
  {-# INLINE casBoolOffAddr# #-}
  atomicModifyMutableByteArray# = atomicModifyAtomMutableByteArray#
  {-# INLINE atomicModifyMutableByteArray#  #-}
  atomicModifyOffAddr# = atomicModifyAtomOffAddr#
  {-# INLINE atomicModifyOffAddr#  #-}



atomicAddFetchOldMutableByteArrayNum#
  , atomicAddFetchNewMutableByteArrayNum#
  , atomicSubFetchOldMutableByteArrayNum#
  , atomicSubFetchNewMutableByteArrayNum# ::
     (Num a, Atomic a)
  => MutableByteArray# s
  -> Int#
  -> a
  -> State# s
  -> (# State# s, a #)
atomicAddFetchOldMutableByteArrayNum# mba# i# y =
  atomicModifyMutableByteArray# mba# i# (\x -> (# x + y, x #))
{-# INLINE atomicAddFetchOldMutableByteArrayNum# #-}

atomicAddFetchNewMutableByteArrayNum# mba# i# y =
  atomicModifyMutableByteArray# mba# i# (\x -> let x' = x + y in (# x', x' #))
{-# INLINE atomicAddFetchNewMutableByteArrayNum# #-}

atomicSubFetchOldMutableByteArrayNum# mba# i# y =
  atomicModifyMutableByteArray# mba# i# (\x -> (# x - y, x #))
{-# INLINE atomicSubFetchOldMutableByteArrayNum# #-}

atomicSubFetchNewMutableByteArrayNum# mba# i# y =
  atomicModifyMutableByteArray# mba# i# (\x -> let x' = x - y in (# x', x' #))
{-# INLINE atomicSubFetchNewMutableByteArrayNum# #-}

atomicAddFetchOldOffAddrNum#
  , atomicAddFetchNewOffAddrNum#
  , atomicSubFetchOldOffAddrNum#
  , atomicSubFetchNewOffAddrNum# ::
     (Num a, Atomic a)
  => Addr#
  -> Int#
  -> a
  -> State# s
  -> (# State# s, a #)
atomicAddFetchOldOffAddrNum# addr# i# y =
  atomicModifyOffAddr# addr# i# (\x -> (# x + y, x #))
{-# INLINE atomicAddFetchOldOffAddrNum# #-}
atomicAddFetchNewOffAddrNum# addr# i# y =
  atomicModifyOffAddr# addr# i# (\x -> let x' = x + y in (# x', x' #))
{-# INLINE atomicAddFetchNewOffAddrNum# #-}
atomicSubFetchOldOffAddrNum# addr# i# y =
  atomicModifyOffAddr# addr# i# (\x -> (# x - y, x #))
{-# INLINE atomicSubFetchOldOffAddrNum# #-}
atomicSubFetchNewOffAddrNum# addr# i# y =
  atomicModifyOffAddr# addr# i# (\x -> let x' = x - y in (# x', x' #))
{-# INLINE atomicSubFetchNewOffAddrNum# #-}


instance (Num a, Eq a, Prim a) => AtomicCount (Atom a) where
  atomicAddFetchOldMutableByteArray# = atomicAddFetchOldMutableByteArrayNum#
  {-# INLINE atomicAddFetchOldMutableByteArray# #-}
  atomicAddFetchNewMutableByteArray# = atomicAddFetchNewMutableByteArrayNum#
  {-# INLINE atomicAddFetchNewMutableByteArray# #-}
  atomicSubFetchOldMutableByteArray# = atomicSubFetchOldMutableByteArrayNum#
  {-# INLINE atomicSubFetchOldMutableByteArray# #-}
  atomicSubFetchNewMutableByteArray# = atomicSubFetchNewMutableByteArrayNum#
  {-# INLINE atomicSubFetchNewMutableByteArray# #-}
  atomicAddFetchOldOffAddr# = atomicAddFetchOldOffAddrNum#
  {-# INLINE atomicAddFetchOldOffAddr# #-}
  atomicAddFetchNewOffAddr# = atomicAddFetchNewOffAddrNum#
  {-# INLINE atomicAddFetchNewOffAddr# #-}
  atomicSubFetchOldOffAddr# = atomicSubFetchOldOffAddrNum#
  {-# INLINE atomicSubFetchOldOffAddr# #-}
  atomicSubFetchNewOffAddr# = atomicSubFetchNewOffAddrNum#
  {-# INLINE atomicSubFetchNewOffAddr# #-}

atomicAndFetchOldMutableByteArrayBits#
  , atomicAndFetchNewMutableByteArrayBits#
  , atomicNandFetchOldMutableByteArrayBits#
  , atomicNandFetchNewMutableByteArrayBits#
  , atomicOrFetchOldMutableByteArrayBits#
  , atomicOrFetchNewMutableByteArrayBits#
  , atomicXorFetchOldMutableByteArrayBits#
  , atomicXorFetchNewMutableByteArrayBits# ::
     (Bits a, Atomic a)
  => MutableByteArray# s
  -> Int#
  -> a
  -> State# s
  -> (# State# s, a #)
atomicAndFetchOldMutableByteArrayBits# mba# i# y =
  atomicModifyMutableByteArray# mba# i# (\x -> (# x .&. y, x #))
{-# INLINE atomicAndFetchOldMutableByteArrayBits# #-}
atomicAndFetchNewMutableByteArrayBits# mba# i# y =
  atomicModifyMutableByteArray# mba# i# (\x -> let x' = x .&. y in (# x', x' #))
{-# INLINE atomicAndFetchNewMutableByteArrayBits# #-}
atomicNandFetchOldMutableByteArrayBits# mba# i# y =
  atomicModifyMutableByteArray# mba# i# (\x -> (# complement (x .&. y), x #))
{-# INLINE atomicNandFetchOldMutableByteArrayBits# #-}
atomicNandFetchNewMutableByteArrayBits# mba# i# y =
  atomicModifyMutableByteArray# mba# i# (\x -> let x' = complement (x .&. y) in (# x', x' #))
{-# INLINE atomicNandFetchNewMutableByteArrayBits# #-}
atomicOrFetchOldMutableByteArrayBits# mba# i# y =
  atomicModifyMutableByteArray# mba# i# (\x -> (# x .|. y, x #))
{-# INLINE atomicOrFetchOldMutableByteArrayBits# #-}
atomicOrFetchNewMutableByteArrayBits# mba# i# y =
  atomicModifyMutableByteArray# mba# i# (\x -> let x' = x .|. y in (# x', x' #))
{-# INLINE atomicOrFetchNewMutableByteArrayBits# #-}
atomicXorFetchOldMutableByteArrayBits# mba# i# y =
  atomicModifyMutableByteArray# mba# i# (\x -> (# x `xor` y, x #))
{-# INLINE atomicXorFetchOldMutableByteArrayBits# #-}
atomicXorFetchNewMutableByteArrayBits# mba# i# y =
  atomicModifyMutableByteArray# mba# i# (\x -> let x' = x `xor` y in (# x', x' #))
{-# INLINE atomicXorFetchNewMutableByteArrayBits# #-}

atomicAndFetchOldOffAddrBits#
  , atomicAndFetchNewOffAddrBits#
  , atomicNandFetchOldOffAddrBits#
  , atomicNandFetchNewOffAddrBits#
  , atomicOrFetchOldOffAddrBits#
  , atomicOrFetchNewOffAddrBits#
  , atomicXorFetchOldOffAddrBits#
  , atomicXorFetchNewOffAddrBits# ::
     (Bits a, Atomic a)
  => Addr#
  -> Int#
  -> a
  -> State# s
  -> (# State# s, a #)
atomicAndFetchOldOffAddrBits# addr# i# y =
  atomicModifyOffAddr# addr# i# (\x -> (# x .&. y, x #))
{-# INLINE atomicAndFetchOldOffAddrBits# #-}
atomicAndFetchNewOffAddrBits# addr# i# y =
  atomicModifyOffAddr# addr# i# (\x -> let x' = x .&. y in (# x', x' #))
{-# INLINE atomicAndFetchNewOffAddrBits# #-}
atomicNandFetchOldOffAddrBits# addr# i# y =
  atomicModifyOffAddr# addr# i# (\x -> (# complement (x .&. y), x #))
{-# INLINE atomicNandFetchOldOffAddrBits# #-}
atomicNandFetchNewOffAddrBits# addr# i# y =
  atomicModifyOffAddr# addr# i# (\x -> let x' = complement (x .&. y) in (# x', x' #))
{-# INLINE atomicNandFetchNewOffAddrBits# #-}
atomicOrFetchOldOffAddrBits# addr# i# y =
  atomicModifyOffAddr# addr# i# (\x -> (# x .|. y, x #))
{-# INLINE atomicOrFetchOldOffAddrBits# #-}
atomicOrFetchNewOffAddrBits# addr# i# y =
  atomicModifyOffAddr# addr# i# (\x -> let x' = x .|. y in (# x', x' #))
{-# INLINE atomicOrFetchNewOffAddrBits# #-}
atomicXorFetchOldOffAddrBits# addr# i# y =
  atomicModifyOffAddr# addr# i# (\x -> (# x `xor` y, x #))
{-# INLINE atomicXorFetchOldOffAddrBits# #-}
atomicXorFetchNewOffAddrBits# addr# i# y =
  atomicModifyOffAddr# addr# i# (\x -> let x' = x `xor` y in (# x', x' #))
{-# INLINE atomicXorFetchNewOffAddrBits# #-}


instance (Bits a, Eq a, Prim a) => AtomicBits (Atom a) where
  atomicAndFetchOldMutableByteArray# = atomicAndFetchOldMutableByteArrayBits#
  {-# INLINE atomicAndFetchOldMutableByteArray# #-}
  atomicAndFetchNewMutableByteArray# = atomicAndFetchNewMutableByteArrayBits#
  {-# INLINE atomicAndFetchNewMutableByteArray# #-}
  atomicNandFetchOldMutableByteArray# = atomicNandFetchOldMutableByteArrayBits#
  {-# INLINE atomicNandFetchOldMutableByteArray# #-}
  atomicNandFetchNewMutableByteArray# = atomicNandFetchNewMutableByteArrayBits#
  {-# INLINE atomicNandFetchNewMutableByteArray# #-}
  atomicOrFetchOldMutableByteArray# = atomicOrFetchOldMutableByteArrayBits#
  {-# INLINE atomicOrFetchOldMutableByteArray# #-}
  atomicOrFetchNewMutableByteArray# = atomicOrFetchNewMutableByteArrayBits#
  {-# INLINE atomicOrFetchNewMutableByteArray# #-}
  atomicXorFetchOldMutableByteArray# = atomicXorFetchOldMutableByteArrayBits#
  {-# INLINE atomicXorFetchOldMutableByteArray# #-}
  atomicXorFetchNewMutableByteArray# = atomicXorFetchNewMutableByteArrayBits#
  {-# INLINE atomicXorFetchNewMutableByteArray# #-}
  atomicAndFetchOldOffAddr# = atomicAndFetchOldOffAddrBits#
  {-# INLINE atomicAndFetchOldOffAddr# #-}
  atomicAndFetchNewOffAddr# = atomicAndFetchNewOffAddrBits#
  {-# INLINE atomicAndFetchNewOffAddr# #-}
  atomicNandFetchOldOffAddr# = atomicNandFetchOldOffAddrBits#
  {-# INLINE atomicNandFetchOldOffAddr# #-}
  atomicNandFetchNewOffAddr# = atomicNandFetchNewOffAddrBits#
  {-# INLINE atomicNandFetchNewOffAddr# #-}
  atomicOrFetchOldOffAddr# = atomicOrFetchOldOffAddrBits#
  {-# INLINE atomicOrFetchOldOffAddr# #-}
  atomicOrFetchNewOffAddr# = atomicOrFetchNewOffAddrBits#
  {-# INLINE atomicOrFetchNewOffAddr# #-}
  atomicXorFetchOldOffAddr# = atomicXorFetchOldOffAddrBits#
  {-# INLINE atomicXorFetchOldOffAddr# #-}
  atomicXorFetchNewOffAddr# = atomicXorFetchNewOffAddrBits#
  {-# INLINE atomicXorFetchNewOffAddr# #-}
