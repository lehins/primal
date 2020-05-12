module Test.Prim.Common
  ( module X
  , propIO
  ) where

import Test.Hspec as X
import Test.Hspec.QuickCheck as X
import Test.QuickCheck as X hiding ((.&.))
import Test.QuickCheck.Function as X
import Test.QuickCheck.Monadic as X
import Control.Prim.Monad as X
import Data.Prim as X


propIO :: Testable a => IO a -> Property
propIO action = monadicIO $ run action
