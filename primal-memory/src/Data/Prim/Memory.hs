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
  , countMem                  -- DOC: [x], DOCTEST [x], TEST: [ ]
  , countRemMem               -- DOC: [x], DOCTEST [x], TEST: [ ]
  , byteCountMem              -- DOC: [x], DOCTEST [ ], TEST: [x]
  -- ** Index
  , indexOffMem               -- DOC: [x], DOCTEST [ ], TEST: [x]
  , indexByteOffMem           -- DOC: [x], DOCTEST [ ], TEST: [x]
  -- ** Construct
  , emptyMem                  -- DOC: [x], DOCTEST [x], TEST: [x]
  , singletonMem              -- DOC: [x], DOCTEST [x], TEST: [ ]
  , cycleMemN                 -- DOC: [x], DOCTEST [x], TEST: [ ]
  , createMemST               -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , createMemST_              -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , createZeroMemST           -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , createZeroMemST_          -- DOC: [x], DOCTEST [x], TEST: [ ]
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
  , toListMem                 -- DOC: [x], DOCTEST [x], TEST: [x]
  , toListSlackMem            -- DOC: [x], DOCTEST [x], TEST: [x]
  , toByteListMem             -- DOC: [x], DOCTEST [x], TEST: [x]
  , foldrCountMem             -- DOC: [x], DOCTEST [-], TEST: [-]
  , showsHexMem               -- DOC: [x], DOCTEST [x], TEST: [ ]

  -- *** From list
  , fromListMem               -- DOC: [x], DOCTEST [x], TEST: [x]
  , fromByteListMem           -- DOC: [x], DOCTEST [x], TEST: [x]
  , fromListMemN              -- DOC: [x], DOCTEST [x], TEST: [x]
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
  , allocMem                  -- DOC: [x], DOCTEST [ ], TEST: [ ]
  , allocZeroMem              -- DOC: [x], DOCTEST [x], TEST: [ ]
  , thawMem                   -- DOC: [x], DOCTEST [ ], TEST: [ ]
  , thawCloneMem              -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , thawCopyMem               -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , freezeMem                 -- DOC: [x], DOCTEST [ ], TEST: [ ]
  , freezeCloneMem            -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , freezeCopyMem             -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , resizeMem                 -- DOC: [x], DOCTEST [ ], TEST: [ ]
  , withScrubbedMem           -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  -- ** Move
  , moveMem                   -- DOC: [ ], DOCTEST [ ], TEST: [x]
  , moveByteOffMem            -- DOC: [x], DOCTEST [ ], TEST: [ ]
  , moveByteOffToMBytesMem    -- DOC: [x], DOCTEST [ ], TEST: [x]
  , moveByteOffToPtrMem       -- DOC: [x], DOCTEST [ ], TEST: [x]
  -- ** Load list
  , loadListMem               -- DOC: [x], DOCTEST [x], TEST: [x]
  , loadListMem_              -- DOC: [x], DOCTEST [x], TEST: [ ]
  , loadListMemN              -- DOC: [x], DOCTEST [ ], TEST: [ ]
  , loadListMemN_             -- DOC: [x], DOCTEST [ ], TEST: [ ]
  -- *** With offset
  , loadListOffMem            -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , loadListOffMemN           -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , loadListByteOffMem        -- DOC: [x], DOCTEST [x], TEST: [ ]
  , loadListByteOffMemN       -- DOC: [x], DOCTEST [x], TEST: [ ]
  ) where

import Data.Prim
import Data.Prim.Memory.Internal
