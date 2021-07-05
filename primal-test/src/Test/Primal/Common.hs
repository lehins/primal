module Test.Primal.Common
  ( module X
  , propIO
  , assertException
  , assertExceptionIO
  , assertAnySyncException
  , assertAnySyncExceptionIO
  , toStringException
  , ExpectedException(..)
  , impreciseExpectedException
  , assertExpectedException
  , assertExpectedExceptionIO
  ) where

import Control.DeepSeq
import Primal.Monad as X
import Data.Foldable as Foldable
import Data.List as List
import Data.Coerce
--import Primal as X
import Primal.Array
import Primal.Memory.Internal
import qualified Primal.Memory.Text as T
import Data.Proxy as X
import Data.Typeable as X
import Test.Hspec as X hiding (Arg(..))
import Test.Hspec.QuickCheck as X
import Test.QuickCheck as X hiding ((.&.))
import Test.QuickCheck.Function as X
import Test.QuickCheck.Monadic as X
import Primal.Exception


propIO :: Testable a => IO a -> Property
propIO action = monadicIO $ run action


assertException ::
     (Testable b, NFData a, Exception exc)
  => (exc -> b) -- ^ Return True if that is the exception that was expected
  -> a -- ^ Value that should throw an exception, when fully evaluated
  -> Property
assertException isExc = assertExceptionIO isExc . pure


assertAnySyncException :: NFData a => a -> Property
assertAnySyncException = assertAnySyncExceptionIO . pure


assertExceptionIO ::
     (Testable b, NFData a, Exception exc)
  => (exc -> b) -- ^ Return True if that is the exception that was expected
  -> IO a -- ^ IO Action that should throw an exception
  -> Property
assertExceptionIO isExc action =
  monadicIO $
  run $
  catch
    (do res <- action
        res `deepseq` return (counterexample "Did not receive an exception" False))
    (\exc -> displayException exc `deepseq` return (property (isExc exc)))

assertAnySyncExceptionIO :: NFData a => IO a -> Property
assertAnySyncExceptionIO action =
  monadicIO $
  run $
  catchAllSync
    (do res <- action
        res `deepseq` return (counterexample "Did not receive an exception" False))
    (\exc -> displayException exc `deepseq` return (property True))


toStringException :: Either SomeException a -> Either String a
toStringException = either (Left . displayException) Right

assertExpectedExceptionIO ::
     NFData a
  => IO a -- ^ Value that should throw `ExpectedException`, when fully evaluated
  -> Property
assertExpectedExceptionIO = assertExceptionIO (==ExpectedException)

assertExpectedException ::
     NFData a
  => a -- ^ Value that should throw `ExpectedException`, when fully evaluated
  -> Property
assertExpectedException = assertException (==ExpectedException)

impreciseExpectedException :: ImpreciseException -> Bool
impreciseExpectedException (ImpreciseException exc _) =
  fromException exc == Just ExpectedException

data ExpectedException = ExpectedException deriving (Show, Eq)

instance Exception ExpectedException


instance Eq T.Array where
  (==) = eqByteMem

instance Ord T.Array where
  compare = compareByteMem

instance Show T.Array where
  show b =
    Foldable.foldr' ($) "]" $
    ('[' :) : List.intersperse (',' :) (map (("0x" ++) .) (showsHexMem b))


instance Arbitrary Size where
  arbitrary = coerce (arbitrary :: Gen Int)

instance Arbitrary e => Arbitrary (BArray e) where
  arbitrary = fromListBArray <$> arbitrary

instance Arbitrary e => Arbitrary (SBArray e) where
  arbitrary = fromListSBArray <$> arbitrary

instance (Unbox e, Arbitrary e) => Arbitrary (UArray e) where
  arbitrary = fromListUArray <$> arbitrary
