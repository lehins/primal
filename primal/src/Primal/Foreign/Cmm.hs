{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Primal.Foreign.Cmm
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Foreign.Cmm
  ( word32ToFloat#
  , floatToWord32#
  , word64ToDouble#
  , doubleToWord64#
  , getSizeofMutableArray#
  , shrinkMutableArray#
  , resizeMutableArray#
#if __GLASGOW_HASKELL__ < 810
  , getSizeofSmallMutableArray#
  , shrinkSmallMutableArray#
  , resizeSmallMutableArray#
#endif
  ) where


import GHC.Exts

#include "MachDeps.h"

-- | Cast a 32bit Word into a Float
foreign import prim "primal_stg_word32ToFloatzh"
  word32ToFloat# :: Word# -> Float#

-- | Cast a Float into a 32bit Word
foreign import prim "primal_stg_floatToWord32zh"
  floatToWord32# :: Float# -> Word#

-- | Cast a 64bit Word into a Double
foreign import prim "primal_stg_word64ToDoublezh"
#if WORD_SIZE_IN_BITS == 64
  word64ToDouble# :: Word# -> Double#
#else
  word64ToDouble# :: Word64# -> Double#
#endif

-- | Cast a Double into a 64bit Word
foreign import prim "primal_stg_doubleToWord64zh"
#if WORD_SIZE_IN_BITS == 64
  doubleToWord64# :: Double# -> Word#
#else
  doubleToWord64# :: Double# -> Word64#
#endif



getSizeofMutableArray# :: MutableArray# s a -> State# s -> (# State# s, Int# #)
getSizeofMutableArray# sma# s# = (# s#, sizeofMutableArray# sma# #)
{-# INLINE getSizeofMutableArray# #-}


-- | Shrink MutableArray#
foreign import prim "primal_stg_shrinkMutableArrayzh"
  shrinkMutableArrayCmm# :: MutableArray# s a -> Int# -> State# s -> (# State# s, Int# #)

shrinkMutableArray# :: MutableArray# s a -> Int# -> State# s -> State# s
shrinkMutableArray# ma# i# s =
  case shrinkMutableArrayCmm# ma# i# s of
    (# s', _ #) -> s'
{-# INLINE shrinkMutableArray# #-}

resizeMutableArray# ::
     MutableArray# s a -- ^ Array to resize
  -> Int# -- ^ New size of array
  -> a -- ^ Newly created slots initialized to this element. Only used when array is
       -- grown.
  -> State# s
  -> (# State# s, MutableArray# s a #)
resizeMutableArray# arr0 szNew a s0 =
  case getSizeofMutableArray# arr0 s0 of
    (# s1, szOld #) ->
      if isTrue# (szNew <# szOld)
        then case shrinkMutableArrayCmm# arr0 szNew s1 of
               (# s2, _ #) -> (# s2, arr0 #)
        else if isTrue# (szNew ># szOld)
               then case newArray# szNew a s1 of
                      (# s2, arr1 #) ->
                        case copyMutableArray# arr0 0# arr1 0# szOld s2 of
                          s3 -> (# s3, arr1 #)
               else (# s1, arr0 #)
{-# INLINE resizeMutableArray# #-}


#if __GLASGOW_HASKELL__ < 810

getSizeofSmallMutableArray# :: SmallMutableArray# s a -> State# s -> (# State# s, Int# #)
getSizeofSmallMutableArray# sma# s# = (# s#, sizeofSmallMutableArray# sma# #)
{-# INLINE getSizeofSmallMutableArray# #-}

-- | Shrink SmallMutableArray#
foreign import prim "primal_stg_shrinkSmallMutableArrayzh"
  shrinkSmallMutableArrayCmm# :: SmallMutableArray# s a -> Int# -> State# s -> (# State# s, Int# #)

shrinkSmallMutableArray# :: SmallMutableArray# s a -> Int# -> State# s -> State# s
shrinkSmallMutableArray# ma# i# s =
  case shrinkSmallMutableArrayCmm# ma# i# s of
    (# s', _ #) -> s'
{-# INLINE shrinkSmallMutableArray# #-}

resizeSmallMutableArray#
  :: SmallMutableArray# s a -- ^ Array to resize
  -> Int# -- ^ New size of array
  -> a
     -- ^ Newly created slots initialized to this element.
     -- Only used when array is grown.
  -> State# s
  -> (# State# s, SmallMutableArray# s a #)
resizeSmallMutableArray# arr0 szNew a s0 =
  case getSizeofSmallMutableArray# arr0 s0 of
    (# s1, szOld #) ->
      if isTrue# (szNew <# szOld)
        then case shrinkSmallMutableArrayCmm# arr0 szNew s1 of
               (# s2, _ #) -> (# s2, arr0 #)
        else if isTrue# (szNew ># szOld)
               then case newSmallArray# szNew a s1 of
                      (# s2, arr1 #) ->
                        case copySmallMutableArray# arr0 0# arr1 0# szOld s2 of
                          s3 -> (# s3, arr1 #)
               else (# s1, arr0 #)


#endif
