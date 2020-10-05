{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Prim.Memory
  ( module Test.Prim.Memory
  , module Data.Prim.Memory
  ) where

import Control.Monad
import Control.DeepSeq
import Data.Prim.Memory
import Data.Prim.Memory.Addr
import Data.Prim.Memory.Bytes
import Test.Prim


data Mem a e = Mem [e] (FrozenMem a)

instance (MemRead (FrozenMem a), Eq e) => Eq (Mem a e) where
  Mem xs1 m1 == Mem xs2 m2 = xs1 == xs2 && eqMem m1 m2

instance (Show e, Prim e, MemAlloc a) => Show (Mem a e) where
  show (Mem xs bs) =
    "(Mem " ++ show xs ++ " " ++ show (toListMem bs :: [e]) ++ ")"

instance (MemAlloc a, Prim e, Arbitrary e) => Arbitrary (Mem a e) where
  arbitrary = do
    NonNegative n <- arbitrary
    xs :: [e] <- vectorOf n arbitrary
    pure $
      Mem xs $
      createMemST_ (Count n :: Count e) $ \mem ->
        zipWithM_ (writeOffMem mem) [0 ..] xs

data NEMem a e = NEMem (Off e) [e] (FrozenMem a)

instance (MemRead (FrozenMem a), Eq e) => Eq (NEMem a e) where
  NEMem o1 xs1 m1 == NEMem o2 xs2 m2 = o1 == o2 && xs1 == xs2 && eqMem m1 m2

instance (Show e, Prim e, MemAlloc a) => Show (NEMem a e) where
  show (NEMem o xs bs) =
    "(NEMem " ++ show o ++ " " ++ show xs ++ " " ++ show (toListMem bs :: [e]) ++ ")"

instance (MemAlloc a, Prim e, Arbitrary e) => Arbitrary (NEMem a e) where
  arbitrary = do
    Positive n <- arbitrary
    NonNegative k <- arbitrary
    let i = k `mod` n
    xs :: [e] <- vectorOf n arbitrary
    pure $
      NEMem (Off i) xs $
      createMemST_ (Count n :: Count e) $ \mem ->
        zipWithM_ (writeOffMem mem) [0 ..] xs

neMemCountBefore :: NEMem a e -> Count e
neMemCountBefore (NEMem (Off i) _ _) = Count i

neMemCountAfter :: NEMem a e -> Count e
neMemCountAfter (NEMem (Off i) xs _) = Count (length xs - i)

instance Typeable p => Arbitrary (Bytes p) where
  arbitrary = do
    Mem (_ :: [Word8]) b <- arbitrary
    pure b

instance (Arbitrary e, Prim e) => Arbitrary (Addr e) where
  arbitrary = do
    Mem (_ :: [e]) b <- arbitrary
    pure b

getZeroElement :: forall e m s. (MonadPrim s m, Prim e) => m e
getZeroElement = do
  z :: MBytes 'Inc s <- callocMBytes (1 :: Count e)
  readOffMem z (0 :: Off e)

prop_byteCountMem ::
     forall ma e. (Prim e, MemAlloc ma)
  => Mem ma e
  -> Property
prop_byteCountMem (Mem xs fm) =
  property $ byteCountMem fm `shouldBe` (Count (length xs) * byteCountType @e)

prop_indexOffMem ::
     forall ma e. (Show e, Prim e, Eq e, MemAlloc ma)
  => NEMem ma e
  -> NonNegative Int
  -> APrim
  -> Property
prop_indexOffMem (NEMem off@(Off o) xs fm) (NonNegative k) aPrim =
  conjoin
    [ propIO (indexOffMem fm off `shouldBe` xs !! o)
    , withAPrim aPrim $ \e ->
        let tOff = Off k `offForType` e
          -- test precondition from documentation
         in (unOff (toByteOff tOff) <= unCount (byteCountMem fm - byteCount e)) ==> do
              mm <- thawCloneMem fm
              writeOffMem mm tOff e
              fm' <- freezeMem mm
              indexOffMem fm' tOff `shouldBe` e
    ]

prop_indexByteOffMem ::
     forall ma e. (Show e, Prim e, Eq e, MemAlloc ma)
  => NEMem ma e
  -> NonNegative Int
  -> APrim
  -> Property
prop_indexByteOffMem (NEMem off@(Off o) xs fm) (NonNegative k) aPrim =
  conjoin
    [ propIO (indexByteOffMem fm (toByteOff off) `shouldBe` xs !! o)
    , withAPrim aPrim $ \a ->
        let tOff = Off k
          -- test precondition from documentation
         in (unOff tOff <= unCount (byteCountMem fm - byteCount a)) ==> do
              mm <- thawCloneMem fm
              writeByteOffMem mm tOff a
              fm' <- freezeMem mm
              indexByteOffMem fm' tOff `shouldBe` a
    ]


prop_copyAndCompareByteOffToMBytesMem ::
     forall a e. (Show e, Prim e, Eq e, MemAlloc a)
  => NEMem a e
  -> NonNegative (Off Word8)
  -> NonNegative (Count e)
  -> NonNegative (Off e)
  -> e
  -> Property
prop_copyAndCompareByteOffToMBytesMem (NEMem offSrc xs fm) (NonNegative offByteDst) nn off e =
  propIO $ do
    let count = min (countMem fm - offToCount offSrc) (getNonNegative nn)
        xs' = take (unCount count) $ drop (unOff offSrc) xs
    mb :: MBytes 'Inc RW <- allocMem (toByteCount count + offToCount offByteDst)
    copyByteOffToMBytesMem fm (toByteOff offSrc) mb offByteDst count
    b <- freezeMem mb
    -- Ensure copy was successfull
    forM_ (zip [offByteDst, offByteDst + countToOff (byteCountType @e) ..] xs') $ \ (i, x) ->
      readByteOffMem mb i `shouldReturn` x
    -- Ensure copy was successfull with compare
    compareByteOffToBytesMem fm (toByteOff offSrc) b offByteDst count `shouldBe` EQ
    compareByteOffMem b offByteDst fm (toByteOff offSrc) count `shouldBe` EQ
    -- validate compareByteOffToBytesMem
    when (count > 0) $ do
      let offDelta = countToOff (offToCount (getNonNegative off) `mod` count)
      let eOrdering = compare (singletonBytes (xs !! unOff (offSrc + offDelta)))
                              (singletonBytes e :: Bytes 'Inc)
      writeByteOffMem mb (offByteDst + toByteOff offDelta) e
      b' <- freezeMem mb
      compareByteOffToBytesMem fm (toByteOff offSrc) b' offByteDst count `shouldBe` eOrdering

prop_copyAndCompareByteOffToPtrMem ::
     forall a e. (Show e, Prim e, Eq e, MemAlloc a)
  => NEMem a e
  -> NonNegative (Off Word8)
  -> NonNegative (Count e)
  -> NonNegative (Off e)
  -> e
  -> Property
prop_copyAndCompareByteOffToPtrMem (NEMem offSrc xs fm) (NonNegative offByteDst) nn off e =
  propIO $ do
    let count = min (countMem fm - offToCount offSrc) (getNonNegative nn)
        xs' = take (unCount count) $ drop (unOff offSrc) xs
    maddr :: MAddr e RW <- castMAddr <$> allocMAddr (toByteCount count + offToCount offByteDst)
    withPtrMAddr maddr $ \ ptr -> copyByteOffToPtrMem fm (toByteOff offSrc) ptr offByteDst count
    -- Ensure copy was successfull
    forM_ (zip [offByteDst, offByteDst + countToOff (byteCountType @e) ..] xs') $ \ (i, x) ->
      readByteOffMem maddr i `shouldReturn` x
    -- Ensure copy was successfull with compare
    withPtrMAddr maddr $ \ ptr -> do
      compareByteOffToPtrMem fm (toByteOff offSrc) ptr offByteDst count `shouldReturn` EQ
      addr <- freezeMAddr maddr
      compareByteOffMem addr offByteDst fm (toByteOff offSrc) count `shouldBe` EQ
    -- validate compareByteOffToPtrMem
    when (count > 0) $ do
      let offDelta = countToOff (offToCount (getNonNegative off) `mod` count)
      let eOrdering = compare (singletonBytes (xs !! unOff (offSrc + offDelta)))
                              (singletonBytes e :: Bytes 'Inc)
      writeByteOffMem maddr (offByteDst + toByteOff offDelta) e
      withPtrMAddr maddr $ \ ptr ->
        compareByteOffToPtrMem fm (toByteOff offSrc) ptr offByteDst count `shouldReturn` eOrdering


prop_readWriteOffMem ::
     forall ma e. (Show e, Prim e, Eq e, MemAlloc ma)
  => NEMem ma e
  -> e
  -> NonNegative Int
  -> APrim
  -> Property
prop_readWriteOffMem (NEMem off@(Off o) xs fm) e' (NonNegative k) aPrim =
  conjoin
    [ propIO $ do
        mm <- thawMem fm
        readOffMem mm off `shouldReturn` xs !! o
        writeOffMem mm off e'
        readOffMem mm off `shouldReturn` e'
    , withAPrim aPrim $ \e ->
        let tOff = Off k `offForType` e
          -- test precondition from documentation
         in (unOff (toByteOff tOff) <= unCount (byteCountMem fm - byteCount e)) ==> do
              mm <- thawMem fm
              writeOffMem mm tOff e
              readOffMem mm tOff `shouldReturn` e
    ]


prop_readWriteByteOffMem ::
     forall ma e. (Show e, Prim e, Eq e, MemAlloc ma)
  => NEMem ma e
  -> e
  -> NonNegative Int
  -> APrim
  -> Property
prop_readWriteByteOffMem (NEMem off@(Off o) xs fm) e' (NonNegative k) aPrim =
  conjoin
    [ propIO $ do
        mm <- thawMem fm
        readByteOffMem mm (toByteOff off) `shouldReturn` xs !! o
        writeByteOffMem mm (toByteOff off) e'
        readByteOffMem mm (toByteOff off) `shouldReturn` e'
    , withAPrim aPrim $ \e ->
        let tOff = Off k `offForType` e
          -- test precondition from documentation
         in (unOff (toByteOff tOff) <= unCount (byteCountMem fm - byteCount e)) ==> do
              mm <- thawMem fm
              writeByteOffMem mm (toByteOff tOff) e
              readByteOffMem mm (toByteOff tOff) `shouldReturn` e
    ]

prop_moveByteOffToMBytesMem ::
     forall a e. (Show e, Prim e, Eq e, MemAlloc a)
  => NEMem a e
  -> NonNegative (Off Word8)
  -> NonNegative (Count e)
  -> Property
prop_moveByteOffToMBytesMem (NEMem offSrc xs fm) (NonNegative offByteDst) nn =
  propIO $ do
    let count = min (countMem fm - offToCount offSrc) (getNonNegative nn)
        xs' = take (unCount count) $ drop (unOff offSrc) xs
    mm <- thawMem fm
    mb :: MBytes 'Inc RW <- allocMem (toByteCount count + offToCount offByteDst)
    moveByteOffToMBytesMem mm (toByteOff offSrc) mb offByteDst count
    -- Ensure copy was successfull
    forM_ (zip [offByteDst, offByteDst + countToOff (byteCountType @e) ..] xs') $ \ (i, x) ->
      readByteOffMem mb i `shouldReturn` x


prop_moveByteOffToPtrMem ::
     forall a e. (Show e, Prim e, Eq e, MemAlloc a)
  => NEMem a e
  -> NonNegative (Off Word8)
  -> NonNegative (Count e)
  -> Property
prop_moveByteOffToPtrMem (NEMem offSrc xs fm) (NonNegative offByteDst) nn =
  propIO $ do
    let count = min (countMem fm - offToCount offSrc) (getNonNegative nn)
        xs' = take (unCount count) $ drop (unOff offSrc) xs
    mm <- thawMem fm
    maddr :: MAddr e RW <- castMAddr <$> allocMAddr (toByteCount count + offToCount offByteDst)
    withPtrMAddr maddr $ \ ptr -> moveByteOffToPtrMem mm (toByteOff offSrc) ptr offByteDst count
    -- Ensure copy was successfull
    forM_ (zip [offByteDst, offByteDst + countToOff (byteCountType @e) ..] xs') $ \ (i, x) ->
      readByteOffMem maddr i `shouldReturn` x

prop_copyMem ::
     forall a e. (Show e, Prim e, Eq e, MemAlloc a)
  => NEMem a e
  -> NEMem a e
  -> Property
prop_copyMem (NEMem offSrc xs fmSrc) (NEMem offDst _ fmDst) =
  propIO $ do
    let count = min (countMem fmSrc - offToCount offSrc) (countMem fmDst - offToCount offDst)
        xs' = take (unCount count) $ drop (unOff offSrc) xs
    mmDst <- thawMem fmDst
    copyMem fmSrc offSrc mmDst offDst count
    -- Ensure copy was successfull
    forM_ (zip [offDst, offDst + 1 ..] xs') $ \ (i, x) ->
      readOffMem mmDst i `shouldReturn` x


prop_moveMem ::
     forall a e. (Show e, Prim e, Eq e, MemAlloc a)
  => NEMem a e
  -> NEMem a e
  -> Property
prop_moveMem (NEMem offSrc xs fmSrc) (NEMem offDst _ fmDst) =
  propIO $ do
    let count = min (countMem fmSrc - offToCount offSrc) (countMem fmDst - offToCount offDst)
        xs' = take (unCount count) $ drop (unOff offSrc) xs
    mmSrc <- thawMem fmSrc
    mmDst <- thawMem fmDst
    moveMem mmSrc offSrc mmDst offDst count
    -- Ensure copy was successfull
    forM_ (zip [offDst, offDst + 1 ..] xs') $ \ (i, x) ->
      readOffMem mmDst i `shouldReturn` x
    moveMem mmSrc offSrc mmSrc 0 count
    -- Ensure copy within the same region was successfull
    forM_ (zip [0 ..] xs') $ \ (i, x) ->
      readOffMem mmSrc i `shouldReturn` x


prop_emptyMem ::
     forall a e. (Show e, Prim e, Eq e, MemAlloc a)
  => Mem a e
  -> Property
prop_emptyMem (Mem xs fm') = propIO $ do
  let zc = 0 :: Count e
  m' <- thawMem fm'
  m :: a RW <- allocMem zc
  -- Check zero set and no element evaluation
  z :: e <- getZeroElement
  setMem m 0 0 z
  getCountMem m `shouldReturn` (0 :: Count e)
  getCountRemMem m `shouldReturn` (0 :: Count e, 0)
  getByteCountMem m `shouldReturn` 0
  -- Check zero move
  moveMem m 0 m' 0 zc
  toListMem fm' `shouldBe` xs
  fm <- freezeMem m
  -- Check zero copy
  copyMem fm 0 m' 0 zc
  toListMem fm' `shouldBe` xs
  -- Ensure zero frozen count
  byteCountMem fm `shouldBe` 0
  countMem fm `shouldBe` (0 :: Count e)
  countRemMem fm `shouldBe` (0 :: Count e, 0)
  toListMem fm `shouldBe` ([] :: [e])


prop_setMem ::
     forall a e. (Show e, Prim e, Eq e, MemAlloc a)
  => NEMem a e
  -> e
  -> NonNegative (Count e)
  -> Property
prop_setMem (NEMem off@(Off o) xs fm) e (NonNegative k) =
  propIO $ do
    m <- thawMem fm
    Count n :: Count e <- getCountMem m
    let c = Count (n - o)
        c'@(Count ci') = max 0 (c - k)
    setMem m off c' e
    fm' <- freezeMem m
    let xs' = toListMem fm' :: [e]
    -- ensure memory before offset is unaffected by the set
    take o xs' `shouldBe` take o xs
    -- ensure memory from offset up to size was set to value `e`
    take ci' (drop o xs') `shouldBe` replicate ci' e
    -- ensure memory after offset+size is unaffected by the set
    drop ci' (drop o xs') `shouldBe` drop ci' (drop o xs)

asProxyTypeOf1 :: f a -> proxy a -> f a
asProxyTypeOf1 fa _ = fa

prop_fromListMemN ::
     forall ma e.
     ( MemAlloc ma
     , Show (FrozenMem ma)
     , Eq (FrozenMem ma)
     , Show e
     , Prim e
     , Eq e
     , NFData e
     )
  => NEMem ma e
  -> Positive (Count e)
  -> Property
prop_fromListMemN (NEMem (Off i) xs fm) (Positive n') =
  propIO $ do
    let n = Count $ length xs
        (order', fm' :: FrozenMem ma) = fromListMemN (Count i) xs
        (order'', fm'' :: FrozenMem ma) = fromListMemN (n + n') xs
    fromListMemN n xs `shouldBe` (Left [], fm)
    order' `shouldBe` Left (drop i xs)
    xs `shouldStartWith` toListMem fm'
    order'' `shouldBe` Right n
    let xs' = toListMem fm''
    xs' `deepseq` (xs' `shouldStartWith` xs)

prop_loadListMem ::
     forall ma e. (MemAlloc ma, Show e, Prim e, Eq e)
  => [e]
  -> NonNegative (Count Word8)
  -> Property
prop_loadListMem xs (NonNegative c) =
  propIO $ do
    mm :: ma RW <- allocMem c
    Count n :: Count e <- getCountMem mm
    let offs = [0 ..] :: [Off e]
    loadListMem xs mm >>= \case
      ([], loadedCount) -> do
        loadedCount `shouldBe` Count (length xs)
        zipWithM_ (\i x -> readOffMem mm i `shouldReturn` x) offs xs
      (leftOver, loadedCount) -> do
        leftOver `shouldBe` drop n xs
        loadedCount `shouldBe` Count n
        zipWithM_ (\i x -> readOffMem mm i `shouldReturn` x) (take n offs) xs


prop_reallocMem ::
     forall ma e. (MemAlloc ma, Show e, Prim e, Eq e)
  => Mem ma e
  -> NonNegative Int
  -> APrimType
  -> Property
prop_reallocMem (Mem xs fm) (NonNegative n) pt =
  propIO $ do
    mm <- thawCloneMem fm
    withAPrimType pt $ \ aPrimProxy -> do
      let c' = Count n `countForProxyTypeOf` aPrimProxy
          c8' = toByteCount c'
      c8 <- getByteCountMem mm
      mm' <- reallocMem mm c'
      getByteCountMem mm' `shouldReturn` c8'
      fm' <- freezeMem mm'
      compareMem fm 0 fm' 0 (min c8 c8') `shouldBe` EQ
      let ce' = countMem fm' `countForProxyTypeOf` xs
      zipWithM_ (\off x -> indexOffMem fm' off `shouldBe` x) [0 .. countToOff ce' - 1] xs


memSpec ::
     forall ma e.
     ( Arbitrary e
     , Show e
     , Prim e
     , Eq e
     , NFData e
     , Typeable e
     , Typeable ma
     , MemAlloc ma
     , Eq (FrozenMem ma)
     , Show (FrozenMem ma)
     )
  => Spec
memSpec = do
  let memTypeName = showsType (Proxy :: Proxy (Mem ma e)) ""
  describe memTypeName $ do
    describe "MemRead" $ do
      prop "byteCountMem" $ prop_byteCountMem @ma @e
      prop "indexOffMem" $ prop_indexOffMem @ma @e
      prop "indexByteOffMem" $ prop_indexByteOffMem @ma @e
      prop "copyByteOffToMBytesMem/compareByteOffToBytesMem" $
        prop_copyAndCompareByteOffToMBytesMem @ma @e
      prop "copyByteOffToPtrMem/compareByteOffToPtrMem" $
        prop_copyAndCompareByteOffToPtrMem @ma @e
    describe "MemWrite" $ do
      prop "readWriteOffMem" $ prop_readWriteOffMem @ma @e
      prop "readWriteByteOffMem" $ prop_readWriteByteOffMem @ma @e
      prop "moveByteOffToPtrMem" $ prop_moveByteOffToPtrMem @ma @e
      prop "moveByteOffToMBytesMem" $ prop_moveByteOffToMBytesMem @ma @e
    prop "emptyMem" $ prop_emptyMem @ma @e
    prop "copyMem" $ prop_copyMem @ma @e
    prop "moveMem" $ prop_moveMem @ma @e
    prop "setMem" $ prop_setMem @ma @e
    prop "reallocMem" $ prop_reallocMem @ma @e
    describe "List" $ do
      describe "Conversion" $ do
        prop "toListMem" $ \(Mem xs fm :: Mem ma e) -> toListMem fm === xs
        prop "fromListMem" $ \(Mem xs fm :: Mem ma e) -> fromListMem xs === fm
        prop "fromListMemN" $ prop_fromListMemN @ma @e
      describe "Loading" $ do
        prop "loadListMem" $ prop_loadListMem @ma @e

prop_toListSlackMem ::
     forall ma. MemAlloc ma
  => Count Word8
  -> APrimList
  -> Property
prop_toListSlackMem bCount aPrimList =
  withAPrimList aPrimList $ \xs ->
    byteCountProxy xs > 0 ==>
    propIO $ do
      mm :: ma RW <- allocMem (min bCount (toByteCount (Count (length xs) `asProxyTypeOf1` xs)))
      (_leftover, loadedCount) <- loadListMem xs mm
      (count, slackCount) <- getCountRemMem mm
      loadedCount `shouldBe` count
      -- Load slack with a few known bytes
      let ss8 = [255,254 ..] :: [Word8]
          (xs8, xs8rest) = splitAt (unCount slackCount) ss8
          slackByteOff = countToOff (toByteCount loadedCount)
      loadListByteOffMemN slackCount ss8 mm slackByteOff `shouldReturn`
        (xs8rest, slackCount)
      fm <- freezeMem mm
      toListSlackMem fm `shouldBe` (take (unCount loadedCount) xs, xs8)


memBinarySpec ::
     forall ma. (Typeable ma, MemAlloc ma, Eq (FrozenMem ma), Show (FrozenMem ma))
  => Spec
memBinarySpec = do
  let memTypeName = showsType (Proxy :: Proxy (Mem ma Word8)) "-Binary"
  describe memTypeName $ do
    prop "toByteListMem" $ \(Mem xs fm :: Mem ma Word8) ->
      toByteListMem fm === xs
    prop "fromByteListMem" $ \(Mem xs fm :: Mem ma Word8) ->
      fromByteListMem xs === fm
    prop "toListSlackMem" $ prop_toListSlackMem @ma


-- primSpec ::
--      forall (p :: Pinned) a.
--      (NFData a, Eq a, Show a, Prim a, Arbitrary a, Typeable p, Typeable a)
--   => Spec
-- primSpec = do
--   let bytesTypeName =
--         showsType (Proxy :: Proxy (Bytes p)) .
--         (' ' :) . showsType (Proxy :: Proxy a) $
--         ""
--   describe bytesTypeName $ do
--     describe "memset" $ do
--       prop "empty" $ \(a :: a) -> do
--         mb :: MBytes p RealWorld <- allocMBytes (0 :: Count a)
--         setMBytes mb 0 0 a
--         b <- freezeMBytes mb
--         toListBytes b `shouldBe` ([] :: [a])
--       prop "non-empty" $ \(NEBytes off@(Off o) xs b :: NEBytes p a) (a :: a) -> do
--         mb <- thawBytes b
--         Count n :: Count a <- getCountOfMBytes mb
--         let c = Count (n - o)
--         setMBytes mb off c a
--         zipWithM_ (\i x -> readOffMBytes mb i `shouldReturn` x) [0 ..] (take o xs)
--         forM_ [o .. unCount c - 1] $ \i ->
--             readOffMBytes mb (Off i) `shouldReturn` a
--     describe "List" $ do
--       prop "toListBytes" $ \(NEBytes _ xs b :: NEBytes p a) -> xs === (toListBytes b :: [a])
--       prop "toListBytes+fromBytes" $ \(NEBytes _ xs b :: NEBytes p a) ->
--         let xs' = toListBytes b :: [a]
--         in xs' === xs .&&. fromListBytes xs' === b
--       prop "loadListMBytes" $ \ (xs :: [a]) (b :: Bytes p) -> do
--         mb <- thawBytes b
--         (Count n :: Count a, r) <- getCountRemOfMBytes mb
--         loadListMBytes xs mb >>= \case
--           GT ->
--             zipWithM_ (\i x -> readOffMBytes mb (Off i :: Off a) `shouldReturn` x) [0.. n - 1] xs
--           elt -> do
--             when (elt == EQ) $ r `shouldBe` 0
--             zipWithM_ (\i x -> readOffMBytes mb (i :: Off a) `shouldReturn` x) [0..] xs
--       prop "fromListBytesN" $ \(NEBytes (Off i) xs b :: NEBytes p a) (Positive n') -> do
--         let n = Count $ length xs
--             (order, b') = fromListBytesN n xs
--             (order', b'' :: Bytes p) = fromListBytesN (Count i) xs
--             (order'', b''' :: Bytes p) = fromListBytesN (n + n') xs
--         order `shouldBe` EQ
--         b' `shouldBe` b
--         order' `shouldBe` GT
--         xs `shouldStartWith` toListBytes b''
--         order'' `shouldBe` LT
--         let xs' = toListBytes b'''
--         xs' `deepseq` (xs' `shouldStartWith` xs)
--       prop "concatBytes (empty)" $ \ (NonNegative n) ->
--         concatBytes (replicate n (emptyBytes :: Bytes p)) === (emptyBytes :: Bytes p)
--       prop "concatBytes" $ \ (xs :: [Bytes p]) ->
--         (concatBytes xs :: Bytes p) === fromListBytes (foldMap toList xs)
--     describe "Allocation" $ do
--       prop "resizeMBytes" $ prop_resizeMBytes @p @a
--       prop "singletonBytes" $ \(a :: a) ->
--         indexBytes (singletonBytes a :: Bytes p) 0 === a
--     describe "moveMBytesToMBytes" $ do
--       prop "copyBytesToMBytes" $
--         \(NEBytes i1 _ b1 :: NEBytes p a) (NEBytes i2 _ b2 :: NEBytes p a) -> do
--           let c = min (countBytes b1 - Count (unOff i1)) (countBytes b2 - Count (unOff i2))
--           mb2x <- thawBytes b2
--           copyBytesToMBytes b1 i1 mb2x i2 c
--           by <- withCloneMBytes_ b2 $ \ mb2y -> do
--             mb1 <- thawBytes b1
--             moveMBytesToMBytes mb1 i1 mb2y i2 c
--           bx <- freezeMBytes mb2x
--           bx `shouldBe` by
--       prop "moveInside" $ \(NEBytes i xs b :: NEBytes p a) -> do
--         let c = countBytes b - Count (unOff i)
--         mb <- thawBytes b
--         moveMBytesToMBytes mb i mb 0 c
--         b' <- freezeMBytes mb
--         take (unCount c) (toListBytes b') `shouldBe` drop (unOff i) xs

-- prop_resizeMBytes ::
--      forall p a. (Prim a, Eq a, Show a, Typeable p)
--   => NEBytes p a
--   -> NonNegative Int
--   -> Property
-- prop_resizeMBytes (NEBytes _ xs b) (NonNegative n') =
--   monadicIO $
--   run $ do
--     mb <- thawBytes $ cloneBytes b
--     mbr <- resizeMBytes mb (Count n' :: Count Int)
--     br <- freezeMBytes mbr
--     pure $ conjoin $ zipWith (===) xs (toListBytes br :: [a])


-- primTypeSpec ::
--      forall a. (NFData a, Eq a, Show a, Prim a, Arbitrary a, Typeable a)
--   => Spec
-- primTypeSpec = do
--   primSpec @'Pin @a
--   primSpec @'Inc @a

-- primBinarySpec ::
--      forall (p :: Pinned). (Typeable p)
--   => Spec
-- primBinarySpec = do
--   let bytesTypeName = showsType (Proxy :: Proxy (Bytes p)) ""
--   describe bytesTypeName $ do
--     describe "calloc" $ do
--       prop "callocMBytes" $ \(b :: Bytes p) -> do
--         mb0 <- callocMBytes (countBytes b :: Count Word8)
--         mb <- thawBytes b
--         zeroMBytes mb
--         b0 <- freezeMBytes mb
--         freezeMBytes mb0 `shouldReturn` b0
--     describe "clone" $
--       prop "Bytes" $ \(b :: Bytes p) -> do
--         let bc = cloneBytes b
--         bc `shouldBe` b
--         unless (byteCountBytes b == 0) $ isSameBytes b bc `shouldBe` False
--     describe "isSameBytes" $ do
--       prop "True" $ \(b :: Bytes p) -> isSameBytes b b .&&. b == b
--       it "(empty) True" $
--         isSameBytes emptyBytes emptyBytes .&&. emptyBytes == emptyBytes
--       prop "(non-empty) False" $ \(b1 :: Bytes p) (b2 :: Bytes p) ->
--         not (isEmptyBytes b1 && isEmptyBytes b2) ==> not (isSameBytes b1 b2)
--       prop "Pin (non-empty) False" $ \(b1 :: Bytes p) (b2 :: Bytes 'Pin) ->
--         not (isEmptyBytes b1 && isEmptyBytes b2) ==> not (isSameBytes b1 b2)
--     describe "isSameMBytes" $ do
--       prop "True" $ \(b :: Bytes p) -> do
--         mb <- thawBytes b
--         isSameMBytes mb mb `shouldBe` True
--       it "(empty) True" $ do
--         mb1 <- thawBytes emptyBytes
--         mb2 <- thawBytes emptyBytes
--         isSameMBytes mb1 mb2 `shouldBe` True
--       prop "(non-empty) False" $ \(b1 :: Bytes p) (b2 :: Bytes p) ->
--         monadicIO $
--         run $ do
--           mb1 <- thawBytes b1
--           mb2 <- thawBytes b2
--           pure $
--             not (isEmptyBytes b1 && isEmptyBytes b2) ==>
--             not (isSameMBytes mb1 mb2)
--       prop "Pin (non-empty) False" $ \(b1 :: Bytes p) (b2 :: Bytes 'Pin) ->
--         not (isEmptyBytes b1 && isEmptyBytes b2) ==> not (isSameBytes b1 b2)
--     describe "toList" $ do
--       prop "Inc" $ \(b1 :: Bytes p) (b2 :: Bytes p) ->
--         (b1 == b2) === (toListBytes b1 == (toListBytes b2 :: [Word8]))
--       prop "Inc+Pin" $ \(b1 :: Bytes p) (b2 :: Bytes 'Pin) ->
--         (relaxPinned b1 == relaxPinned b2) ===
--         (toListBytes b1 == (toListBytes b2 :: [Word8]))
--     describe "ensurePinned" $ do
--       prop "Bytes" $ \(b :: Bytes p) ->
--         let b' = ensurePinnedBytes b
--          in isPinnedBytes b'
--       prop "MBytes" $ \(b :: Bytes p) -> do
--         mb <- thawBytes b
--         mb' <- ensurePinnedMBytes mb
--         isPinnedMBytes mb' `shouldBe` True
--     describe "IsList" $ do
--       prop "toList" $ \(NEBytes _ xs b :: NEBytes p Word8) -> xs === toList b
--       prop "fromList . toList" $ \(b :: Bytes p) -> b === fromList (toList b)
--       prop "fromListN . toList" $ \(b :: Bytes p) ->
--         b === fromListN (coerce (byteCountBytes b)) (toList b)
--     describe "Show" $
--       prop "fromList . toList" $ \(b :: Bytes p) ->
--         show b ===
--         BSL8.unpack (toLazyByteString $
--            "[" <>
--            mconcat
--              (List.intersperse "," ["0x" <> word8HexFixed w8 | w8 <- toList b]) <>
--            "]")


-- spec :: Spec
-- spec = do
--   primBinarySpec @'Pin
--   primBinarySpec @'Inc
--   primTypeSpec @(Identity Word)
--   primTypeSpec @(Down Word8)
--   primTypeSpec @(Dual Word16)
--   primTypeSpec @(Sum Word32)
--   primTypeSpec @(Product Word64)
--   primTypeSpec @(Ratio Int)
--   primTypeSpec @(Complex Float)
--   primTypeSpec @Double
--   primTypeSpec @Ordering
--   primTypeSpec @SeekMode
--   primTypeSpec @(Int8, Int16)
--   primTypeSpec @(Int32, Int64, Char)
--   primTypeSpec @((), Ptr (), FunPtr (), StablePtr ())
--   primTypeSpec @(All, Any, Fingerprint, IntPtr, WordPtr)
--   describe "Allocation" $ do
--     describe "Pinned Memory" $ do
--       let mostThreshold = 3248 :: Count Word8
--           leastThreshold = 3277 :: Count Word8
--       -- Experimentally found the threshold to be 3249:
--       --     mostThreshold = 3248
--       --     leastThreshold = 3249
--       -- Documented to be 3277, but seems to be different in practice.
--       -- https://gitlab.haskell.org/ghc/ghc/-/blob/feb852e67e166f752c783978f5fecc3c28c966f9/docs/users_guide/ffi-chap.rst#L1008
--       let pinnedExpectation alloc isPinned = do
--             mb <- alloc
--             isPinnedMBytes mb `shouldBe` isPinned
--             b <- freezeMBytes mb
--             isPinnedBytes b `shouldBe` isPinned
--       it "Small (Inc) - isUnpinned" $
--         pinnedExpectation (allocMBytes mostThreshold :: IO (MBytes 'Inc RealWorld)) False
--       prop "Large (Inc) - isPinned" $ \(NonNegative n) ->
--         pinnedExpectation (allocMBytes (n + leastThreshold) :: IO (MBytes 'Inc RealWorld)) True
--       prop "Inc - isUninned" $ \(NonNegative n) ->
--         (n <= mostThreshold) ==> monadicIO $ run $
--         pinnedExpectation (allocMBytes n :: IO (MBytes 'Inc RealWorld)) False
--       prop "Pin - isPinned" $ \(NonNegative (n :: Count Word8)) ->
--         pinnedExpectation (allocMBytes n :: IO (MBytes 'Pin RealWorld)) True
--       prop "Pin (aligned) - isPinned" $ \(NonNegative (n :: Count Word8)) ->
--         pinnedExpectation (allocAlignedMBytes n) True
--       prop "callocAlignedMBytes" $ \(b :: Bytes 'Pin) -> do
--         mb0 <- callocAlignedMBytes (byteCountBytes b)
--         mb <- thawBytes b
--         zeroMBytes mb
--         b0 <- freezeMBytes mb
--         freezeMBytes mb0 `shouldReturn` b0
--     describe "isSamePinnedBytes" $ do
--       prop "True" $ \(b :: Bytes 'Pin) -> isSamePinnedBytes b b .&&. b == b
--       it "(empty) True" $
--         isSamePinnedBytes emptyBytes emptyBytes .&&. emptyBytes == emptyBytes
--       prop "Pin (non-empty) False" $ \(b1 :: Bytes 'Pin) (b2 :: Bytes 'Pin) ->
--         not (isEmptyBytes b1 && isEmptyBytes b2) ==> not (isSamePinnedBytes b1 b2)
--     describe "Ptr Access" $
--       prop "Test avoidance of GHC bug #18061" $
--         prop_WorkArounBugGHC18061 allocAlignedMBytes withNoHaltPtrMBytes


-- prop_WorkArounBugGHC18061 ::
--      (Count Word32 -> IO t)
--   -> (t -> (Ptr Word32 -> IO a) -> IO ())
--   -> Positive Int
--   -> IO ()
-- prop_WorkArounBugGHC18061 alloc withPtr (Positive n) =
--   void $
--   timeout (1000 * n) $ do
--     replicateM_ 49 $ threadDelay 1
--     fptr <- alloc (4 :: Count Word32)
--     withPtr fptr $ \p ->
--       forever $ do
--         poke p (0xDEADBEEF :: Word32)
--         threadDelay 10
--         x <- peek p
--         unless (x == 0xDEADBEEF) $
--           error ("Heap corruption detected: deadbeef /= " ++ showHex x "")
-- {-# INLINE prop_WorkArounBugGHC18061 #-}

-- Nasty bug:
-- primal-memory> /home/lehins/github/primal/primal-memory/tests/Test/Prim/Memory/BytesSpec.hs:264:7: error:
-- primal-memory>     • No instance for (Arbitrary (Bytes 'Pin))
-- primal-memory>         arising from a use of ‘prop’
-- primal-memory>       There are instances for similar types:
-- primal-memory>         instance Typeable p =>
-- primal-memory>                  Arbitrary (primal-memory-0.1.0.0:Data.Prim.Memory.Bytes.Bytes p)
