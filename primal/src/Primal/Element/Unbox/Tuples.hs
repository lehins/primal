{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Primal.Element.Unbox.Class
-- Copyright   : (c) Alexey Kuleshevich 2020-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Element.Unbox.Tuples () where


import Primal.Foreign
import GHC.TypeLits as Nats
import Primal.Element.Unbox.Class

instance (Unbox a, Unbox b) => Unbox (a, b) where
  type UnboxIso (a, b) = (a, b)
  type SizeOf (a, b) = SizeOf a + SizeOf b
  type Alignment (a, b) = Alignment a + Alignment b
  sizeOf# _ = sizeOf# (proxy# :: Proxy# a) +# sizeOf# (proxy# :: Proxy# b)
  {-# INLINE sizeOf# #-}
  alignment# _ =
    alignment# (proxy# :: Proxy# a) +# alignment# (proxy# :: Proxy# b)
  {-# INLINE alignment# #-}
  indexByteArray# ba# i# =
    let i0# = i# *# sizeOf# (proxy# :: Proxy# (a, b))
     in indexByteOffByteArray# ba# i0#
  {-# INLINE indexByteArray# #-}
  indexByteOffByteArray# ba# i0# =
    let i1# = i0# +# sizeOf# (proxy# :: Proxy# a)
     in (indexByteOffByteArray# ba# i0#, indexByteOffByteArray# ba# i1#)
  {-# INLINE indexByteOffByteArray# #-}
  indexOffAddr# addr# i# =
    let addr0# = addr# `plusAddr#` (i# *# sizeOf# (proxy# :: Proxy# (a, b)))
        addr1# = addr0# `plusAddr#` sizeOf# (proxy# :: Proxy# a)
     in (indexOffAddr# addr0# 0#, indexOffAddr# addr1# 0#)
  {-# INLINE indexOffAddr# #-}
  readMutableByteArray# mba# i# =
    let i0# = i# *# sizeOf# (proxy# :: Proxy# (a, b))
     in readByteOffMutableByteArray# mba# i0#
  {-# INLINE readMutableByteArray# #-}
  readByteOffMutableByteArray# mba# i0# s =
    let i1# = i0# +# sizeOf# (proxy# :: Proxy# a)
     in case readByteOffMutableByteArray# mba# i0# s of
          (# s', a0 #) ->
            case readByteOffMutableByteArray# mba# i1# s' of
              (# s'', a1 #) -> (# s'', (a0, a1) #)
  {-# INLINE readByteOffMutableByteArray# #-}
  readOffAddr# addr# i# s =
    let addr0# = addr# `plusAddr#` (i# *# sizeOf# (proxy# :: Proxy# (a, b)))
        addr1# = addr0# `plusAddr#` sizeOf# (proxy# :: Proxy# a)
    in case readOffAddr# addr0# 0# s of
         (# s', a0 #) ->
           case readOffAddr# addr1# 0# s' of
             (# s'', a1 #) -> (# s'', (a0, a1) #)
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i0# (a0, a1) s =
    let i1# = i0# +# sizeOf# (proxy# :: Proxy# a)
    in writeByteOffMutableByteArray# mba# i1# a1 (writeByteOffMutableByteArray# mba# i0# a0 s)
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# a =
    let i0# = i# *# sizeOf# (proxy# :: Proxy# (a, b))
    in writeByteOffMutableByteArray# mba# i0# a
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# addr# i# (a0, a1) s =
    let addr0# = addr# `plusAddr#` (i# *# sizeOf# (proxy# :: Proxy# (a, b)))
        addr1# = addr0# `plusAddr#` sizeOf# (proxy# :: Proxy# a)
    in writeOffAddr# addr1# 0# a1 (writeOffAddr# addr0# 0# a0 s)
  {-# INLINE writeOffAddr# #-}
  setByteOffMutableByteArray# = setByteOffMutableByteArrayLoop#
    -- TODO: optimize with rewrite rules?
    --  | a0 == a1 = setByteOffMutableByteArray# mba# (o# *# 2#) (n# *# 2#) a0 s
  {-# INLINE setByteOffMutableByteArray# #-}
  setAddr# = setAddrLoop#
  {-# INLINE setAddr# #-}


instance (Unbox a, Unbox b, Unbox c) => Unbox (a, b, c) where
  type UnboxIso (a, b, c) = (a, b, c)
  type SizeOf (a, b, c) = SizeOf a + SizeOf b + SizeOf c
  type Alignment (a, b, c) = Alignment a + Alignment b + Alignment c
  sizeOf# _ = sizeOf# (proxy# :: Proxy# a)
           +# sizeOf# (proxy# :: Proxy# b)
           +# sizeOf# (proxy# :: Proxy# c)
  {-# INLINE sizeOf# #-}
  alignment# _ = alignment# (proxy# :: Proxy# a)
              +# alignment# (proxy# :: Proxy# b)
              +# alignment# (proxy# :: Proxy# c)
  {-# INLINE alignment# #-}
  indexByteOffByteArray# ba# i0# =
    let i1# = i0# +# sizeOf# (proxy# :: Proxy# a)
        i2# = i1# +# sizeOf# (proxy# :: Proxy# b)
    in ( indexByteOffByteArray# ba# i0#
       , indexByteOffByteArray# ba# i1#
       , indexByteOffByteArray# ba# i2#
       )
  {-# INLINE indexByteOffByteArray# #-}
  indexByteArray# ba# i# =
    let i0# = i# *# sizeOf# (proxy# :: Proxy# (a, b, c))
    in indexByteOffByteArray# ba# i0#
  {-# INLINE indexByteArray# #-}
  indexOffAddr# addr# i# =
    let i0# = i# *# sizeOf# (proxy# :: Proxy# (a, b, c))
        i1# = i0# +# sizeOf# (proxy# :: Proxy# a)
        i2# = i1# +# sizeOf# (proxy# :: Proxy# b)
    in ( indexOffAddr# (addr# `plusAddr#` i0#) 0#
       , indexOffAddr# (addr# `plusAddr#` i1#) 0#
       , indexOffAddr# (addr# `plusAddr#` i2#) 0#
       )
  {-# INLINE indexOffAddr# #-}
  readMutableByteArray# mba# i# =
    let i0# = i# *# sizeOf# (proxy# :: Proxy# (a, b, c))
    in readByteOffMutableByteArray# mba# i0#
  {-# INLINE readMutableByteArray# #-}
  readByteOffMutableByteArray# mba# i0# s =
    let i1# = i0# +# sizeOf# (proxy# :: Proxy# a)
        i2# = i1# +# sizeOf# (proxy# :: Proxy# b)
    in case readByteOffMutableByteArray# mba# i0# s  of { (# s0, a0 #) ->
       case readByteOffMutableByteArray# mba# i1# s0 of { (# s1, a1 #) ->
       case readByteOffMutableByteArray# mba# i2# s1 of { (# s2, a2 #) ->
         (# s2, (a0, a1, a2) #)
       }}}
  {-# INLINE readByteOffMutableByteArray# #-}
  readOffAddr# addr# i# s =
    let addr0# = addr#  `plusAddr#` (i# *# sizeOf# (proxy# :: Proxy# (a, b, c)))
        addr1# = addr0# `plusAddr#` sizeOf# (proxy# :: Proxy# a)
        addr2# = addr1# `plusAddr#` sizeOf# (proxy# :: Proxy# b)
    in case readOffAddr# addr0# 0# s  of { (# s0, a0 #) ->
       case readOffAddr# addr1# 0# s0 of { (# s1, a1 #) ->
       case readOffAddr# addr2# 0# s1 of { (# s2, a2 #) ->
         (# s2, (a0, a1, a2) #)
       }}}
  {-# INLINE readOffAddr# #-}
  writeByteOffMutableByteArray# mba# i0# (a0, a1, a2) s =
    let i1# = i0# +# sizeOf# (proxy# :: Proxy# a)
        i2# = i1# +# sizeOf# (proxy# :: Proxy# b)
    in writeByteOffMutableByteArray# mba# i2# a2
       (writeByteOffMutableByteArray# mba# i1# a1
        (writeByteOffMutableByteArray# mba# i0# a0 s))
  {-# INLINE writeByteOffMutableByteArray# #-}
  writeMutableByteArray# mba# i# a s =
    let i0# = i# *# sizeOf# (proxy# :: Proxy# (a, b, c))
    in writeByteOffMutableByteArray# mba# i0# a s
  {-# INLINE writeMutableByteArray# #-}
  writeOffAddr# addr# i# (a0, a1, a2) s =
    let addr0# = addr#  `plusAddr#` (i# *# sizeOf# (proxy# :: Proxy# (a, b, c)))
        addr1# = addr0# `plusAddr#` sizeOf# (proxy# :: Proxy# a)
        addr2# = addr1# `plusAddr#` sizeOf# (proxy# :: Proxy# b)
    in writeOffAddr# addr2# 0# a2
       (writeOffAddr# addr1# 0# a1
        (writeOffAddr# addr0# 0# a0 s))
  {-# INLINE writeOffAddr# #-}
  setByteOffMutableByteArray# = setByteOffMutableByteArrayLoop#
  {-# INLINE setByteOffMutableByteArray# #-}
  setAddr# = setAddrLoop#
  {-# INLINE setAddr# #-}

-- TODO: Write optimized versions for larger tuples
instance (Unbox a, Unbox b, Unbox c, Unbox d) => Unbox (a, b, c, d) where
  type UnboxIso (a, b, c, d) = ((a, b), (c, d))
  toUnboxIso (a, b, c, d) = ((a, b), (c, d))
  {-# INLINE toUnboxIso #-}
  fromUnboxIso ((a, b), (c, d)) = (a, b, c, d)
  {-# INLINE fromUnboxIso #-}

instance (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e) => Unbox (a, b, c, d, e) where
  type UnboxIso (a, b, c, d, e) = ((a, b), (c, d), e)
  toUnboxIso (a, b, c, d, e) = ((a, b), (c, d), e)
  {-# INLINE toUnboxIso #-}
  fromUnboxIso ((a, b), (c, d), e) = (a, b, c, d, e)
  {-# INLINE fromUnboxIso #-}

instance (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e, Unbox f) => Unbox (a, b, c, d, e, f) where
  type UnboxIso (a, b, c, d, e, f) = ((a, b), (c, d), (e, f))
  toUnboxIso (a, b, c, d, e, f) = ((a, b), (c, d), (e, f))
  {-# INLINE toUnboxIso #-}
  fromUnboxIso ((a, b), (c, d), (e, f)) = (a, b, c, d, e, f)
  {-# INLINE fromUnboxIso #-}

instance (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e, Unbox f, Unbox g) => Unbox (a, b, c, d, e, f, g) where
  type UnboxIso (a, b, c, d, e, f, g) = ((a, b, c), (d, e, f), g)
  toUnboxIso (a, b, c, d, e, f, g) = ((a, b, c), (d, e, f), g)
  {-# INLINE toUnboxIso #-}
  fromUnboxIso ((a, b, c), (d, e, f), g) = (a, b, c, d, e, f, g)
  {-# INLINE fromUnboxIso #-}

instance (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e, Unbox f, Unbox g, Unbox h) =>
  Unbox (a, b, c, d, e, f, g, h) where
  type UnboxIso (a, b, c, d, e, f, g, h) = ((a, b, c), (d, e, f), (g, h))
  toUnboxIso (a, b, c, d, e, f, g, h) = ((a, b, c), (d, e, f), (g, h))
  {-# INLINE toUnboxIso #-}
  fromUnboxIso ((a, b, c), (d, e, f), (g, h)) = (a, b, c, d, e, f, g, h)
  {-# INLINE fromUnboxIso #-}

instance (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e, Unbox f, Unbox g, Unbox h, Unbox i) =>
  Unbox (a, b, c, d, e, f, g, h, i) where
  type UnboxIso (a, b, c, d, e, f, g, h, i) = ((a, b, c), (d, e, f), (g, h, i))
  toUnboxIso (a, b, c, d, e, f, g, h, i) = ((a, b, c), (d, e, f), (g, h, i))
  {-# INLINE toUnboxIso #-}
  fromUnboxIso ((a, b, c), (d, e, f), (g, h, i)) = (a, b, c, d, e, f, g, h, i)
  {-# INLINE fromUnboxIso #-}
