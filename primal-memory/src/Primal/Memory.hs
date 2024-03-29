-- |
-- Module      : Primal.Memory
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Memory
  ( module Primal.Monad
  , module Primal.Element.Unbox
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
  , copyByteOffMutMem         -- DOC: [x], DOCTEST [ ], TEST: [ ]
  , copyByteOffToMBytesMem    -- DOC: [x], DOCTEST [ ], TEST: [x]
  , copyByteOffToPtrMem       -- DOC: [x], DOCTEST [ ], TEST: [x]
  -- ** Compare
  , eqMem                     -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , eqOffMem                  -- DOC: [x], DOCTEST [ ], TEST: [ ]
  , eqByteMem                 -- DOC: [x], DOCTEST [ ], TEST: [ ]
  , eqByteOffMem              -- DOC: [x], DOCTEST [ ], TEST: [ ]
  , compareMem                -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , compareOffMem             -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , compareByteMem            -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , compareByteOffMem         -- DOC: [x], DOCTEST [ ], TEST: [ ]
  , compareByteOffToPtrMem    -- DOC: [x], DOCTEST [ ], TEST: [x]
  , compareByteOffToBytesMem  -- DOC: [x], DOCTEST [ ], TEST: [x]
  -- ** Convert
  , convertMem                -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  -- *** To list
  , toStringMem
  , toByteListMem             -- DOC: [x], DOCTEST [x], TEST: [x]
  , toListMem                 -- DOC: [x], DOCTEST [x], TEST: [x]
  , toListSlackMem            -- DOC: [x], DOCTEST [x], TEST: [x]
  , foldrCountMem             -- DOC: [x], DOCTEST [-], TEST: [-]
  , showsHexMem               -- DOC: [x], DOCTEST [x], TEST: [ ]

  -- *** From list
  , fromStringMem
  , fromByteListMem           -- DOC: [x], DOCTEST [x], TEST: [x]
  , fromListMem               -- DOC: [x], DOCTEST [x], TEST: [x]
  , fromListMemN              -- DOC: [x], DOCTEST [x], TEST: [x]
  , fromListZeroMemN          -- DOC: [x], DOCTEST [x], TEST: [ ]
  , fromListZeroMemN_         -- DOC: [x], DOCTEST [x], TEST: [ ]
  -- * Mutable
  , MBytes
  , MemWrite
  , MemAlloc
  , MemFreeze
  , Frozen
  , MutFreeze
  -- ** Size
  , getCountMutMem               -- DOC: [x], DOCTEST [x], TEST: [ ]
  , getCountRemMutMem            -- DOC: [x], DOCTEST [x], TEST: [ ]
  , getByteCountMutMem           -- DOC: [x], DOCTEST [x], TEST: [ ]
  -- ** Read
  , readOffMutMem                -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , readByteOffMutMem            -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  -- ** Write
  , writeOffMutMem               -- DOC: [x], DOCTEST [ ], TEST: [x]
  , writeByteOffMutMem           -- DOC: [x], DOCTEST [ ], TEST: [x]
  , setMutMem                    -- DOC: [x], DOCTEST [ ], TEST: [x]
  , setByteOffMutMem             -- DOC: [ ], DOCTEST [ ], TEST: [x]
  , modifyFetchOldMutMem         -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , modifyFetchOldMutMemM        -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , modifyFetchNewMutMem         -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , modifyFetchNewMutMemM        -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  -- ** Allocate
  , allocMutMem                  -- DOC: [x], DOCTEST [ ], TEST: [ ]
  , allocZeroMutMem              -- DOC: [x], DOCTEST [x], TEST: [ ]
  , reallocMutMem                -- DOC: [x], DOCTEST [ ], TEST: [x]
  , withScrubbedMutMem           -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , defaultReallocMutMem         -- DOC: [x], DOCTEST [ ], TEST: [x]
  -- ** Thaw/Freeze
  , thawCloneMem                 -- DOC: [x], DOCTEST [x], TEST: [ ]
  , thawCloneSliceMem                  -- DOC: [x], DOCTEST [x], TEST: [ ]
  , thawMem                      -- DOC: [x], DOCTEST [ ], TEST: [ ]
  , freezeCloneMutMem            -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , freezeCloneSliceMutMem             -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , freezeMutMem                 -- DOC: [x], DOCTEST [ ], TEST: [ ]
  -- ** Move
  , cloneMutMem                  -- DOC: [x], DOCTEST [ ], TEST: [-]
  , moveMutMem                   -- DOC: [ ], DOCTEST [ ], TEST: [x]
  , moveByteOffMutMem            -- DOC: [x], DOCTEST [ ], TEST: [ ]
  , moveByteOffToMBytesMutMem    -- DOC: [x], DOCTEST [ ], TEST: [x]
  , moveByteOffToPtrMutMem       -- DOC: [x], DOCTEST [ ], TEST: [x]
  -- ** Load list
  , loadListMutMem               -- DOC: [x], DOCTEST [x], TEST: [x]
  , loadListMutMem_              -- DOC: [x], DOCTEST [x], TEST: [ ]
  , loadListMutMemN              -- DOC: [x], DOCTEST [ ], TEST: [ ]
  , loadListMutMemN_             -- DOC: [x], DOCTEST [ ], TEST: [ ]
  -- *** With offset
  , loadListOffMutMem            -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , loadListOffMutMemN           -- DOC: [ ], DOCTEST [ ], TEST: [ ]
  , loadListByteOffMutMem        -- DOC: [x], DOCTEST [x], TEST: [ ]
  , loadListByteOffMutMemN       -- DOC: [x], DOCTEST [x], TEST: [ ]
  ) where

import Primal.Monad
import Primal.Element.Unbox
import Primal.Memory.Internal
import Primal.Memory.Fold
import Primal.Mutable.Freeze

