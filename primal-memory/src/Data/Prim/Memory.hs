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
  -- ** Index
  , indexOffMem
  , indexByteOffMem
  -- ** Copy
  , cloneMem
  , copyMem
  , copyByteOffMem
  , copyByteOffToMBytesMem
  , copyByteOffToPtrMem
  -- ** Compare
  , eqMem
  , compareMem
  , compareByteOffMem
  , compareByteOffToPtrMem
  , compareByteOffToBytesMem
  -- ** Convert
  , convertMem
  -- *** To list
  , toListMem
  , toListSlackMem
  , toByteListMem

  -- *** From list
  , fromListMem
  , fromListMemN
  , fromByteListMem
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
  -- ** Read
  , readOffMem
  , readByteOffMem
  -- ** Write
  , writeOffMem
  , writeByteOffMem
  , setMem
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
  , modifyFetchOldMem
  , modifyFetchOldMemM
  , modifyFetchNewMem
  , modifyFetchNewMemM
  -- ** Move
  , moveMem
  , moveByteOffMem
  , moveByteOffToMBytesMem
  , moveByteOffToPtrMem
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
