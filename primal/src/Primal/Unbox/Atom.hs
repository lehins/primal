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
-- Module      : Primal.Unbox.Atom
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Unbox.Atom
  ( Atom(..)
  -- * SpinLocks
  , acquireLockByteOffMutableByteArray
  , releaseLockByteOffMutableByteArray
  , acquireLockByteOffAddr
  , releaseLockByteOffAddr
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

import Control.DeepSeq
import Primal.Concurrent
import Primal.Exception
import Primal.Monad.Unsafe
import Data.Bits
import Primal.Unbox.Atomic
import Primal.Unbox.Class
import Primal.Foreign hiding (Any)
import GHC.TypeLits

newtype Atom e =
  Atom
    { unAtom :: e
    }
  deriving ( Show
           , Eq
           , Ord
           , Num
           , Enum
           , Integral
           , Real
           , RealFrac
           , Fractional
           , Floating
           , RealFloat
           , Bits
           , NFData
           )


instance Unbox e => Unbox (Atom e) where
  type PrimBase (Atom e) = Atom e
  type SizeOf (Atom e) = 1 + SizeOf e
  type Alignment (Atom e) = 1 + Alignment e
  sizeOf# _ = 1# +# sizeOf# (proxy# :: Proxy# e)
  {-# INLINE sizeOf# #-}
  alignment# _ = 1# +# alignment# (proxy# :: Proxy# e)
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i# = Atom (indexByteOffByteArray# ba# (1# +# i#))
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# = indexByteOffByteArray# ba# (i# *# sizeOf# (proxy# :: Proxy# (Atom e)))
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# =
    Atom (indexOffAddr# (addr# `plusAddr#` (1# +# i# *# sizeOf# (proxy# :: Proxy# (Atom e)))) 0#)
  {-# INLINE indexOffAddr# #-}
  readByteOffMutableByteArray# mba# i# s =
    case readByteOffMutableByteArray# mba# (1# +# i#) s of
      (# s', e #) -> (# s', Atom e #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readMutableByteArray# mba# i# =
    readByteOffMutableByteArray# mba# (i# *# sizeOf# (proxy# :: Proxy# (Atom e)))
  {-# INLINE readMutableByteArray# #-}
  readOffAddr# addr# i# s =
    case readOffAddr# (addr# `plusAddr#` (1# +# i# *# sizeOf# (proxy# :: Proxy# (Atom e)))) 0# s of
      (# s', e #) -> (# s', Atom e #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i# (Atom e) s =
    writeByteOffMutableByteArray# mba# i# (0 :: Word8)
      (writeByteOffMutableByteArray# mba# (1# +# i#) e s)
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# (Atom e) s =
    let i0# = i# *# sizeOf# (proxy# :: Proxy# (Atom e))
    in writeByteOffMutableByteArray# mba# i0# (0 :: Word8)
         (writeByteOffMutableByteArray# mba# (1# +# i0#) e s)
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# addr# i# (Atom e) s =
    let i0# = i# *# sizeOf# (proxy# :: Proxy# (Atom e))
    in writeOffAddr# addr# i0# (0 :: Word8) (writeOffAddr# (addr# `plusAddr#` (1# +# i0#)) 0# e s)
  {-# INLINE writeOffAddr# #-}
  setMutableByteArray# = setMutableByteArrayLoop#
  {-# INLINE setMutableByteArray# #-}
  setOffAddr# = setOffAddrLoop#
  {-# INLINE setOffAddr# #-}


acquireLockByteOffMutableByteArray :: MonadPrim s m => MutableByteArray# s -> Int# -> m ()
acquireLockByteOffMutableByteArray mba# i# =
  let go = do
        locked <- unsafeIOToPrim $ syncLockTestSetInt8ArrayIO mba# i#
        unless (locked == 0) $ yield >> go
   in go
{-# INLINE acquireLockByteOffMutableByteArray #-}

releaseLockByteOffMutableByteArray :: MonadPrim s m => MutableByteArray# s -> Int# -> m ()
releaseLockByteOffMutableByteArray mba# i# =
  unsafeIOToPrim $ syncLockReleaseInt8ArrayIO mba# i#
{-# INLINE releaseLockByteOffMutableByteArray #-}


acquireLockByteOffAddr :: MonadPrim s m => Addr# -> Int# -> m ()
acquireLockByteOffAddr addr# i# =
  let go = do
        locked <- unsafeIOToPrim $ syncLockTestSetInt8AddrIO addr# i#
        unless (locked == 0) $ yield >> go
   in go
{-# INLINE acquireLockByteOffAddr #-}

releaseLockByteOffAddr :: MonadPrim s m => Addr#-> Int# -> m ()
releaseLockByteOffAddr addr# i# = unsafeIOToPrim $ syncLockReleaseInt8AddrIO addr# i#
{-# INLINE releaseLockByteOffAddr #-}

withLockMutableByteArray ::
     forall e a m. (Unbox e, MonadUnliftPrim RW m)
  => MutableByteArray# RealWorld
  -> Int#
  -> (Atom e -> m (Atom e, a))
  -> m a
withLockMutableByteArray mba# i# f =
  let li0# = i# *# sizeOf# (proxy# :: Proxy# (Atom e))
      li# = 1# +# li0#
   in bracket_
        (acquireLockByteOffMutableByteArray mba# li0#)
        (releaseLockByteOffMutableByteArray mba# li0#) $ do
      a <- prim (readByteOffMutableByteArray# mba# li#)
      (Atom a', b) <- f (Atom a)
      b <$ prim_ (writeByteOffMutableByteArray# mba# li# a')
{-# INLINABLE withLockMutableByteArray #-}


-- | Atomic reads on `Atom` require a lock because otherwise any other thread can
-- overwrite the contnts in the midst of reading, resulting in a value with contents
-- from both values part before and part after the write.
atomicReadAtomMutableByteArray ::
     forall e m s. (Unbox e, MonadPrim s m)
  => MutableByteArray# s
  -> Int#
  -> m (Atom e)
atomicReadAtomMutableByteArray mba# i# =
  let li# = i# *# sizeOf# (proxy# :: Proxy# (Atom e))
   in uninterruptibleMaskPrimBase_ $ do
        acquireLockByteOffMutableByteArray mba# li#
        r :: e <- ST (readByteOffMutableByteArray# mba# (1# +# li#))
        coerce r <$ releaseLockByteOffMutableByteArray mba# li#
{-# INLINABLE atomicReadAtomMutableByteArray #-}

-- | Values are no longer guaranteed to be one word in size, as such in order for writes
-- to be atomic we require locking.
atomicWriteAtomMutableByteArray ::
     forall e m s. (Unbox e, MonadPrim s m)
  => MutableByteArray# s
  -> Int#
  -> Atom e
  -> m ()
atomicWriteAtomMutableByteArray mba# i# (Atom a) =
  let li# = i# *# sizeOf# (proxy# :: Proxy# (Atom e))
   in uninterruptibleMaskPrimBase_ $ do
        acquireLockByteOffMutableByteArray mba# li#
        prim_ (writeByteOffMutableByteArray# mba# (1# +# li#) a)
        releaseLockByteOffMutableByteArray mba# li# :: ST s ()
{-# INLINABLE atomicWriteAtomMutableByteArray #-}



-- | Same as `atomicReadAtomMutableByteArray`, but for `Addr#` with offset
atomicReadAtomOffAddr ::
     forall e m s. (Unbox e, MonadPrim s m)
  => Addr#
  -> Int#
  -> m (Atom e)
atomicReadAtomOffAddr mba# i# =
  let li# = i# *# sizeOf# (proxy# :: Proxy# (Atom e))
   in uninterruptibleMaskPrimBase_ $ do
        acquireLockByteOffAddr mba# li#
        r :: e <- ST (readOffAddr# mba# (1# +# li#))
        coerce r <$ releaseLockByteOffAddr mba# li#
{-# INLINABLE atomicReadAtomOffAddr #-}

-- | Same as `atomicWriteAtomMutableByteArray`, but for `Addr#` with offset
atomicWriteAtomOffAddr ::
     forall e m s. (Unbox e, MonadPrim s m)
  => Addr#
  -> Int#
  -> Atom e
  -> m ()
atomicWriteAtomOffAddr addr# i# (Atom a) =
  let li# = i# *# sizeOf# (proxy# :: Proxy# (Atom e))
      lockAddr# = addr# `plusAddr#` li#
      valAddr# = lockAddr# `plusAddr#` 1#
   in uninterruptibleMaskPrimBase_ $ do
        acquireLockByteOffAddr lockAddr# 0#
        prim_ (writeOffAddr# valAddr# 0# a)
        releaseLockByteOffAddr lockAddr# 0# :: ST s ()
{-# INLINABLE atomicWriteAtomOffAddr #-}



withLockOffAddr ::
     forall e b. Unbox e
  => Addr#
  -> Int#
  -> (Atom e -> IO (Atom e, b))
  -> IO b
withLockOffAddr addr# i# f =
  let li# = i# *# sizeOf# (proxy# :: Proxy# (Atom e))
      offAddr# = addr# `plusAddr#` (1# +# li#)
   in bracket_
        (acquireLockByteOffAddr addr# li#)
        (releaseLockByteOffAddr addr# li#) $ do
      a <- prim (readOffAddr# offAddr# 0#)
      (Atom a', b) <- f (Atom a)
      b <$ prim_ (writeOffAddr# offAddr# 0# a')
{-# INLINABLE withLockOffAddr #-}


atomicModifyAtomMutableByteArray ::
     forall e a m s. (Unbox e, MonadPrim s m)
  => MutableByteArray# s
  -> Int#
  -> (Atom e -> (# Atom e, a #))
  -> m a
atomicModifyAtomMutableByteArray mba# i# f =
  let li# = i# *# sizeOf# (proxy# :: Proxy# (Atom e))
   in uninterruptibleMaskPrimBase_ $ do
        acquireLockByteOffMutableByteArray mba# li#
        r <- ST $ \s ->
          case readByteOffMutableByteArray# mba# (1# +# li#) s of
            (# s', a #) ->
              case f (Atom a) of
                (# Atom a', b #) ->
                  (# writeByteOffMutableByteArray# mba# (1# +# li#) a' s', b #)
        r <$ releaseLockByteOffMutableByteArray mba# li#
{-# INLINE atomicModifyAtomMutableByteArray  #-}

atomicModifyAtomOffAddr ::
     forall e a m s. (Unbox e, MonadPrim s m)
  => Addr#
  -> Int#
  -> (Atom e -> (# Atom e, a #))
  -> m a
atomicModifyAtomOffAddr addr# i# f =
  let li# = i# *# sizeOf# (proxy# :: Proxy# (Atom e))
      offAddr# = addr# `plusAddr#` (1# +# li#)
   in uninterruptibleMaskPrimBase_ $ do
        acquireLockByteOffAddr addr# li#
        r <- ST $ \s ->
          case readOffAddr# offAddr# 0# s of
            (# s', a #) ->
              case f (Atom a) of
                (# Atom a', b #) ->
                  (# writeOffAddr# offAddr# 0# a' s', b #)
        r <$ releaseLockByteOffAddr addr# li#
{-# INLINE atomicModifyAtomOffAddr #-}


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

instance (Eq e, Unbox e) => Atomic (Atom e) where
  atomicReadMutableByteArray# mba# i# =
    unST (atomicReadAtomMutableByteArray (unsafeCoerce# mba#) i#)
  {-# INLINABLE atomicReadMutableByteArray# #-}
  atomicWriteMutableByteArray# mba# i# a =
    unST_ (atomicWriteAtomMutableByteArray (unsafeCoerce# mba#) i# a)
  {-# INLINABLE atomicWriteMutableByteArray# #-}
  atomicReadOffAddr# addr# i# = unST (atomicReadAtomOffAddr addr# i#)
  {-# INLINABLE atomicReadOffAddr# #-}
  atomicWriteOffAddr# addr# i# a = unST_ (atomicWriteAtomOffAddr addr# i# a)
  {-# INLINABLE atomicWriteOffAddr# #-}
  casMutableByteArray# mba# i# old new =
    unST (atomicModifyAtomMutableByteArray mba# i# (swapIfEqualVal old new))
  {-# INLINE casMutableByteArray# #-}
  casOffAddr# addr# i# old new =
    atomicModifyOffAddr# addr# i# (swapIfEqualVal old new)
  {-# INLINE casOffAddr# #-}
  casBoolMutableByteArray# mba# i# old new =
    unST (atomicModifyAtomMutableByteArray mba# i# (swapIfEqualBool old new))
  {-# INLINE casBoolMutableByteArray# #-}
  casBoolOffAddr# addr# i# old new =
    atomicModifyOffAddr# addr# i# (swapIfEqualBool old new)
  {-# INLINE casBoolOffAddr# #-}
  atomicModifyMutableByteArray# mba# i# f =
    unST (atomicModifyAtomMutableByteArray mba# i# f)
  {-# INLINE atomicModifyMutableByteArray#  #-}
  atomicModifyOffAddr# addr# i# f = unST (atomicModifyAtomOffAddr addr# i# f)
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
     (Num e, Atomic e)
  => Addr#
  -> Int#
  -> e
  -> State# s
  -> (# State# s, e #)
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


instance (Num e, Eq e, Unbox e) => AtomicCount (Atom e) where
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
     (Bits e, Atomic e)
  => MutableByteArray# s
  -> Int#
  -> e
  -> State# s
  -> (# State# s, e #)
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


instance (Bits e, Eq e, Unbox e) => AtomicBits (Atom e) where
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
