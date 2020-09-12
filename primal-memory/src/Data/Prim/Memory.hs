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
  , countMem                  -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , countRemMem               -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , byteCountMem              -- DOC: [x], DOCTEST [x], TEST: [x]
  -- ** Index
  , indexOffMem               -- DOC: [x], DOCTEST [ ], TEST: [x]
  , indexByteOffMem           -- DOC: [x], DOCTEST [ ], TEST: [x]
  -- ** Copy
  , cloneMem                  -- DOC: [x], DOCTEST [x], TEST: [ ]
  , copyMem                   -- DOC: [x], DOCTEST [ ], TEST: [x]
  , copyByteOffMem            -- DOC: [x], DOCTEST [ ], TEST: [ ]
  , copyByteOffToMBytesMem    -- DOC: [x], DOCTEST [ ], TEST: [x]
  , copyByteOffToPtrMem       -- DOC: [x], DOCTEST [ ], TEST: [x]
  -- ** Compare
  , eqMem                     -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , compareMem                -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , compareByteOffMem         -- DOC: [x], DOCTEST [ ], TEST: [ ]
  , compareByteOffToPtrMem    -- DOC: [x], DOCTEST [ ], TEST: [x]
  , compareByteOffToBytesMem  -- DOC: [x], DOCTEST [ ], TEST: [x]
  -- ** Convert
  , convertMem                -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  -- *** To list
  , toListMem                 -- DOC: [x], DOCTEST [x], TEST: [ ]
  , toListSlackMem            -- DOC: [x], DOCTEST [x], TEST: [ ]
  , toByteListMem             -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , foldrCountMem             -- DOC: [x], DOCTEST [-], TEST: [-]
  , showsHexMem               -- DOC: [ ], DOCTEST [ ], TEST: [ ]

  -- *** From list
  , fromListMem               -- DOC: [x], DOCTEST [x], TEST: [ ]
  , fromByteListMem           -- DOC: [x], DOCTEST [x], TEST: [ ]
  , fromListMemN              -- DOC: [x], DOCTEST [x], TEST: [ ]
  , fromListZeroMemN          -- DOC: [x], DOCTEST [x], TEST: [ ]
  , fromListZeroMemN_         -- DOC: [x], DOCTEST [x], TEST: [ ]
  -- * Mutable
  , MBytes
  , MemWrite
  , MemAlloc(FrozenMem)
  , MemState(..)
  -- ** Size
  , getCountMem               -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , getCountRemMem            -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , getByteCountMem           -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  -- ** Read
  , readOffMem                -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , readByteOffMem            -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  -- ** Write
  , writeOffMem               -- DOC: [x], DOCTEST [ ], TEST: [x]
  , writeByteOffMem           -- DOC: [x], DOCTEST [ ], TEST: [x]
  , setMem                    -- DOC: [x], DOCTEST [ ], TEST: [x]
  , modifyFetchOldMem         -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , modifyFetchOldMemM        -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , modifyFetchNewMem         -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , modifyFetchNewMemM        -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  -- ** Allocate
  , allocByteCountMem         -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , allocMem                  -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , allocZeroMem              -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , thawMem                   -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , thawCloneMem              -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , thawCopyMem               -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , freezeMem                 -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , freezeCloneMem            -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , freezeCopyMem             -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , createMemST               -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , createMemST_              -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , createZeroMemST           -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , createZeroMemST_          -- DOC: [x], DOCTEST [x], TEST: [ ]
  , withScrubbedMem           -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  -- ** Move
  , moveMem                   -- DOC: [ ], DOCTEST [ ], TEST: [x]
  , moveByteOffMem            -- DOC: [x], DOCTEST [ ], TEST: [ ]
  , moveByteOffToMBytesMem    -- DOC: [x], DOCTEST [ ], TEST: [x]
  , moveByteOffToPtrMem       -- DOC: [x], DOCTEST [ ], TEST: [x]
  -- *** From List
  , loadListMem               -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , loadListMem_              -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , loadListMemN              -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , loadListMemN_             -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  -- **** With offset
  , loadListOffMem            -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , loadListOffMemN           -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  -- **** With byte offset
  , loadListByteOffMem        -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , loadListByteOffMemN       -- DOC: [ ], DOCTEST [ ], TEST: [ ]

  , emptyMem                  -- DOC: [ ], DOCTEST [ ], TEST: [x]
  , singletonMem              -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , cycleMemN                 -- DOC: [ ], DOCTEST [ ], TEST: [ ]
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
