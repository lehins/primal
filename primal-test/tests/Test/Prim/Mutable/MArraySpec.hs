{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Test.Prim.Mutable.MArraySpec
  ( spec
  ) where

import Data.Complex
import Data.Ratio
import Control.Concurrent
import Control.DeepSeq
import Control.Monad
import Control.Prim.Monad
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.List as List
import Data.Monoid
import Data.Functor.Identity
import Data.Prim.Memory.Bytes
import Foreign.Prim hiding (Any)
import Foreign.Prim.Ptr
import Foreign.Prim.StablePtr
import Foreign.Storable
import GHC.IO.Device
import GHC.Fingerprint.Type
import Numeric
import System.Timeout
import Test.Prim
import Test.Prim.Atomic
import Test.Prim.Memory
import Data.Prim.Array.Boxed as B
import Data.Prim.Array.Boxed.Small as SB

spec :: Spec
spec = do
  specMArray @(B.MBArray Integer)
