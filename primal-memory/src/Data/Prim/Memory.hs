-- |
-- Module      : Data.Prim.Memory
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Memory
  ( Pinned(..)
  -- * Immutable
  , Bytes
  , MemRead
  , countMem
  , countRemMem
  , indexOffMem
  , eqMem
  , compareMem
  -- * Mutable
  , MBytes
  , MemAlloc(FrozenMem)
  , MemWrite
  , getCountMem
  , getCountRemMem
  , readOffMem
  , writeOffMem
  , setMem
  , copyMem
  , moveMem

  , MemState(..)
  , allocMem
  , allocZeroMem
  , thawMem
  , thawCloneMem
  , thawCopyMem
  , freezeMem
  , freezeCloneMem
  , freezeCopyMem
  , createMemST
  , createMemST_
  , emptyMem
  , singletonMem
  , cycleMemN
  -- * Byte operations
  -- $byteOperations
  -- ** Immutable
  , byteCountMem
  , indexByteOffMem
  , compareByteOffMem
  -- ** Mutable
  , allocByteCountMem
  , getByteCountMem
  , readByteOffMem
  , writeByteOffMem
  , copyByteOffMem
  , moveByteOffMem
  -- * Conversion
  , convertMem
  -- ** List
  , toListMem
  , toListSlackMem
  , toByteListMem
  , fromByteListMem

  , fromListMem
  , fromListMemN
  , loadListMem
  , loadListMem_
  , loadListMemN
  , loadListMemN_
  -- *** Helpers
  , foldrCountMem
  ) where

import Data.Prim.Memory.Internal


-- $byteOperations
--
-- More often than not it is desired to operate on the offset and count of the actual type
-- of intereset we are dealing with in memory. But sometimes it is necessary to specify
-- things in 8bit steps, this is where byte size offsets and counts will come in handy.
