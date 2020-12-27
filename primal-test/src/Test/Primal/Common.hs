module Test.Prim.Common
  ( module X
  , propIO
  , assertException
  , assertExceptionIO
  , assertSomeException
  , assertSomeExceptionIO
  , toStringException
  , ExpectedException(..)
  , assertExpectedException
  , assertExpectedExceptionIO
  ) where

import Control.DeepSeq
import Primal.Monad as X
import Data.Foldable as Foldable
import Data.List as List
import Primal.Prim as X
import Primal.Data.Array
import Primal.Memory.Internal
import qualified Primal.Memory.Text as T
import Data.Proxy as X
import Data.Typeable as X
import Test.Hspec as X hiding (Arg(..))
import Test.Hspec.QuickCheck as X
import Test.QuickCheck as X hiding ((.&.))
import Test.QuickCheck.Function as X
import Test.QuickCheck.Monadic as X
import UnliftIO.Exception (Exception(..), SomeException, catch, catchAny)


propIO :: Testable a => IO a -> Property
propIO action = monadicIO $ run action


assertException ::
     (Testable b, NFData a, Exception exc)
  => (exc -> b) -- ^ Return True if that is the exception that was expected
  -> a -- ^ Value that should throw an exception, when fully evaluated
  -> Property
assertException isExc = assertExceptionIO isExc . pure


assertSomeException :: NFData a => a -> Property
assertSomeException = assertSomeExceptionIO . pure


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

assertSomeExceptionIO :: NFData a => IO a -> Property
assertSomeExceptionIO action =
  monadicIO $
  run $
  catchAny
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

instance Arbitrary a => Arbitrary (BArray a) where
  arbitrary = fromListBArray <$> arbitrary


instance Arbitrary a => Arbitrary (SBArray a) where
  arbitrary = fromListSBArray <$> arbitrary

