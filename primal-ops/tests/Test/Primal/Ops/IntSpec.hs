{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Test.Primal.Ops.IntSpec (spec) where

import GHC.Int
import Primal.Ops.Int
import qualified Primal.Ops.Int.Internal as I
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

prop_testInt2 :: Int -> Int -> Property
prop_testInt2 (I# x#) (I# y#) =
  toInt3Tuple# (I.timesInt2# x# y#) === toInt3Tuple# (timesInt2# x# y#)
  where
    toInt3Tuple# (# t1#, t2#, t3# #) = (I# t1#, I# t2#, I# t3#)

prop_UnaryInt8 :: (I.Int8# -> I.Int8#) -> (Int8# -> Int8#) -> Int8 -> Property
prop_UnaryInt8 f# g# (I8# x#) = I8# (I.toRealInt8# (f# (I.fromRealInt8# x#))) === I8# (g# x#)

prop_BinaryInt8
  :: (I.Int8# -> I.Int8# -> I.Int8#) -> (Int8# -> Int8# -> Int8#) -> Int8 -> Int8 -> Property
prop_BinaryInt8 f# g# (I8# x#) (I8# y#) =
  I8# (I.toRealInt8# (f# (I.fromRealInt8# x#) (I.fromRealInt8# y#))) === I8# (g# x# y#)

prop_BinaryInt8Bool
  :: (I.Int8# -> I.Int8# -> Int#) -> (Int8# -> Int8# -> Int#) -> Int8 -> Int8 -> Property
prop_BinaryInt8Bool f# g# (I8# x#) (I8# y#) =
  I# (f# (I.fromRealInt8# x#) (I.fromRealInt8# y#)) === I# (g# x# y#)

spec :: Spec
spec = do
  xdescribe "Int" $ do
    prop "testInt2" prop_testInt2
  describe "Int8" $ do
    prop "negateInt8#" $ prop_UnaryInt8 I.negateInt8# negateInt8#
    prop "plusInt8#" $ prop_BinaryInt8 I.plusInt8# plusInt8#
    prop "subInt8#" $ prop_BinaryInt8 I.subInt8# subInt8#
    prop "timesInt8#" $ prop_BinaryInt8 I.timesInt8# timesInt8#
    prop "quotInt8#" $ \x y -> y /= 0 ==> prop_BinaryInt8 I.quotInt8# quotInt8# x y
    prop "remInt8#" $ \x y -> y /= 0 ==> prop_BinaryInt8 I.remInt8# remInt8# x y
    prop "quotRemInt8#" $ \(I8# x#) y@(I8# y#) ->
      (y /= 0)
        ==> case I.quotRemInt8# (I.fromRealInt8# x#) (I.fromRealInt8# y#) of
          (# iq#, ir# #) ->
            case quotRemInt8# x# y# of
              (# q#, r# #) ->
                (I8# (I.toRealInt8# iq#), I8# (I.toRealInt8# ir#)) === (I8# q#, I8# r#)
  xdescribe "Int (failing)" $ do
    prop "uncheckedShiftLInt8#" $ \(I8# x#) (NonNegative _i@(I# y#)) ->
      -- (i < 32)
      --   ==>
      I8# (I.toRealInt8# (I.uncheckedShiftLInt8# (I.fromRealInt8# x#) y#))
        === I8# (uncheckedShiftLInt8# x# y#)
    prop "uncheckedShiftRAInt8#" $ \(I8# x#) (NonNegative _i@(I# y#)) ->
      -- (i < 32)
      --   ==>
      I8# (I.toRealInt8# (I.uncheckedShiftRAInt8# (I.fromRealInt8# x#) y#))
        === I8# (uncheckedShiftRAInt8# x# y#)
    prop "uncheckedShiftRLInt8#" $ \(I8# x#) (NonNegative _i@(I# y#)) ->
      -- (i < 32)
      --   ==>
      I8# (I.toRealInt8# (I.uncheckedShiftRLInt8# (I.fromRealInt8# x#) y#))
        === I8# (uncheckedShiftRLInt8# x# y#)
  describe "Int (continue)" $ do
    -- prop "int8ToWord8#" $ \(I8# x#) ->
    --   W8# (W.toRealWord8# (I.int8ToWord8# (I.fromRealInt8# x#))) === W8# (int8ToWord8# x#)
    prop "eqInt8#" $ prop_BinaryInt8Bool I.eqInt8# eqInt8#
    prop "geInt8#" $ prop_BinaryInt8Bool I.geInt8# geInt8#
    prop "gtInt8#" $ prop_BinaryInt8Bool I.gtInt8# gtInt8#
    prop "leInt8#" $ prop_BinaryInt8Bool I.leInt8# leInt8#
    prop "ltInt8#" $ prop_BinaryInt8Bool I.ltInt8# ltInt8#
    prop "neInt8#" $ prop_BinaryInt8Bool I.neInt8# neInt8#
