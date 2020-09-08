-- |
-- Module      : Data.Prim.Memory
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Memory
  ( module Data.Prim
  , Pinned(..)
  -- * Immutable
  , Bytes
  , MemRead
  -- ** Size
  , countMem
  , countRemMem
  , byteCountMem
  -- ** Indexing
  , indexOffMem
  , indexByteOffMem
  -- ** Compare
  , eqMem
  , compareMem
  , compareByteOffMem
  , compareByteOffToPtrMem
  , compareByteOffToBytesMem
  -- * Conversion
  , convertMem
  -- ** List
  , toListMem
  , toListSlackMem
  , toByteListMem
  , fromByteListMem

  , fromListMem
  , fromListMemN
  -- *** Helpers
  , foldrCountMem
  -- * Mutable
  , MBytes
  , MemWrite
  , MemAlloc(FrozenMem)
  , MemState(..)
  -- ** Size
  , getCountMem
  , getCountRemMem
  , getByteCountMem
  -- ** Allocate
  , allocByteCountMem
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
  , createZeroMemST
  , createZeroMemST_
  , readOffMem
  , readByteOffMem
  , writeOffMem
  , writeByteOffMem
  , modifyFetchOldMem
  , modifyFetchOldMemM
  , modifyFetchNewMem
  , modifyFetchNewMemM
  , setMem
  -- ** Move
  , moveMem
  , moveByteOffMem
  , moveByteOffToMBytesMem
  , moveByteOffToPtrMem
  -- ** Copy
  , copyMem
  , copyByteOffMem
  , copyByteOffToMBytesMem
  , copyByteOffToPtrMem
  -- *** From List
  , loadListMem
  , loadListMem_
  , loadListMemN
  , loadListMemN_

  , emptyMem
  , singletonMem
  , cycleMemN
  -- * Byte operations
  -- $byteOperations
  ) where

import Data.Prim
import Data.Prim.Memory.Internal


-- $byteOperations
--
-- More often than not it is desired to operate on the offset and count of the actual type
-- of intereset we are dealing with in memory. But sometimes it is necessary to specify
-- things in 8bit steps, this is where byte size offsets and counts will come in handy.
