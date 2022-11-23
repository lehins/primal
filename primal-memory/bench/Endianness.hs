{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
module Main where

import GHC.ST
import GHC.Exts
import GHC.Word

import qualified Control.Monad.IO.Class as IO
import Control.Monad.Trans.Class
import Control.Monad.Trans.Identity
import Control.Monad.Trans.State.Strict
import Criterion.Main
import Data.Bits
import Foreign.Ptr
import Foreign.Storable
import GHC.Exts
import GHC.IO
import Primal.Memory
import Primal.Memory.Addr
import Primal.Memory.ByteString
import Primal.Memory.Endianness
import System.Random.Stateful hiding (genShortByteStringST)
import Data.ByteString.Builder.Prim (word64BE)
import Data.ByteString.Builder.Prim.Internal (runF)

#include "MachDeps.h"


main :: IO ()
main = do
  let n = 100000000 :: Int
      c = Count n :: Count Word8
      g = mkStdGen 2021
      genLengths =
        -- create 5000 small lengths that are needed for ShortByteString generation
        runStateGen (mkStdGen 2020) $ \g -> replicateM 5000 (uniformRM (16 + 1, 16 + 7) g)
  defaultMain
    [ bgroup "genUniform"
      [ bgroup "Random" [
          env (pure genLengths) $ \ ~(ns, gen) ->
            bench "genShortByteString" $
            nfIO $ runStateGenT gen $ \g' -> mapM (`uniformShortByteString` g') ns
        , env (pure genLengths) $ \ ~(ns, gen) ->
            bench "genShortByteStringIO'" $
            nfIO $ runStateGenT gen $ \g' -> mapM (`genShortByteStringIO'` uniformWord64 g') ns
        , env (pure genLengths) $ \ ~(ns, gen) ->
            bench "genMem (Addr)" $
            nfIO $ runStateGenT gen $ \g' ->
              mapM (\i -> genMem @(MAddr Word8) LE (Count i) (uniformWord64 g')) ns
        ]
      , bgroup "LE"
        [ bench "genShortByteStringIO" $
          whnfIO (runStateGenT_ g (genShortByteStringIO n . uniformWord64))
        , bench "genMem (Addr)" $
          whnfIO (runStateGenT_ g (genMem @(MAddr Word8) LE c . uniformWord64))
        , bench "genMem (Bytes)" $
          whnfIO (runStateGenT_ g (genMem @(MBytes 'Inc) LE c . uniformWord64))
        , bench "genMem (ShortByteString)" $
          whnfIO (runStateGenT_ g (genMem @MShortByteString LE c . uniformWord64))
        ]
      , bgroup "BE"
        [ bench "genShortByteString" $
          whnfIO (runStateGenT_ g (genShortByteStringBE n . uniformWord64))
        , bench "genMem (Addr)" $
          whnfIO (runStateGenT_ g (genMem @(MAddr Word8) BE c . uniformWord64))
        , bench "genMem (Bytes)" $
          whnfIO (runStateGenT_ g (genMem @(MBytes 'Inc) BE c . uniformWord64))
        , bench "genMem (ShortByteString)" $
          whnfIO (runStateGenT_ g (genMem @MShortByteString BE c . uniformWord64))
        ]
      ]
    ]


genMem ::
     (MemFreeze ma, PrimalIO m, Unbox (f Word64))
  => (Word64 -> f Word64)
  -> Count Word8
  -> m Word64
  -> m (Frozen ma)
genMem f c@(Count n0) gen64 = do
  let !n = max 0 n0
      !n64 = Off (n `quot` 8)
      !nrem64 = Off (n `rem` 8)
  m <- allocMutMem c
  let go !i
        | i < n64 = do
          w64 <- gen64
          writeOffMutMem m i (f w64)
          go (i + 1)
        | otherwise = return $ toByteOff i
  lastOff <- go 0
  when (nrem64 > 0) $ do
    w64 <- gen64
    let !nrem64Off = nrem64 + lastOff
        goRem64 !z !i =
          when (i < nrem64Off) $ do
            writeByteOffMutMem m i (fromIntegral z :: Word8)
            goRem64 (z `shiftR` 8) (i + 1)
    goRem64 w64 lastOff
  freezeMutMem m
{-# INLINE genMem #-}


-- | Efficiently generates a sequence of pseudo-random bytes in a platform
-- independent manner.
--
-- @since 1.2.0
genShortByteStringBE ::
     IO.MonadIO m
  => Int -- ^ Number of bytes to generate
  -> m Word64 -- ^ IO action that can generate 8 random bytes at a time
  -> m ShortByteString
genShortByteStringBE n0 gen64 = do
  let !n@(I# n#) = max 0 n0
      !n64 = n `quot` 8
      !nrem64 = n `rem` 8
  MBA mba# <-
    IO.liftIO $
    IO $ \s# ->
      case newPinnedByteArray# n# s# of
        (# s'#, mba# #) -> (# s'#, MBA mba# #)
  let go i ptr
        | i < n64 = do
          w64 <- gen64
          -- Writing 8 bytes at a time in a Little-endian order gives us
          -- platform portability
          IO.liftIO $ runF word64BE w64 ptr
          go (i + 1) (ptr `plusPtr` 8)
        | otherwise = return ptr
  ptr <- go 0 (Ptr (byteArrayContents# (unsafeCoerce# mba#)))
  when (nrem64 > 0) $ do
    w64 <- gen64
    -- In order to not mess up the byte order we write generated Word64 into a
    -- temporary pointer and then copy only the missing bytes over to the array.
    -- It is tempting to simply generate as many bytes as we still need using
    -- smaller generators (eg. uniformWord8), but that would result in
    -- inconsistent tail when total length is slightly varied.
    IO.liftIO $ do
      let goRem64 z i =
            when (i < nrem64) $ do
              pokeByteOff ptr i (fromIntegral z :: Word8)
              goRem64 (z `shiftR` 8) (i + 1)
      goRem64 w64 0
  IO.liftIO $
    IO $ \s# ->
      case unsafeFreezeByteArray# mba# s# of
        (# s'#, ba# #) -> (# s'#, SBS ba# #)
{-# INLINE genShortByteStringBE #-}

data MBA s = MBA (MutableByteArray# s)


genShortByteStringIO' ::
     IO.MonadIO m
  => Int -- ^ Number of bytes to generate
  -> m Word64 -- ^ IO action that can generate 8 random bytes at a time
  -> m ShortByteString
genShortByteStringIO' n0 gen64 = do
  let !n@(I# n#) = max 0 n0
      !n64 = n `quot` 8
      !nrem = n `rem` 8
      !nremStart = n - nrem
  mba@(MBA mba#) <-
    IO.liftIO $ IO $ \s# ->
      case newByteArray# n# s# of
        (# s'#, mba# #) -> (# s'#, MBA mba# #)
  let go i =
        when (i < n64) $ do
          w64 <- gen64
          -- Writing 8 bytes at a time in a Little-endian order gives us
          -- platform portability
          IO.liftIO $ writeWord64LE mba i w64
          go (i + 1)
  go 0
  when (nrem > 0) $ do
    w64 <- gen64
    let goRem32 z i =
          when (i < n) $ do
            writeWord8 mba i (fromIntegral z :: Word8)
            goRem32 (z `shiftR` 8) (i + 1)
    -- In order to not mess up the byte order we write 1 byte at a time in
    -- Little endian order. It is tempting to simply generate as many bytes as we
    -- still need using smaller generators (eg. uniformWord8), but that would
    -- result in inconsistent tail when total length is slightly varied.
    IO.liftIO $
      if nrem >= 4
      then do
           writeWord32LE mba (nremStart `quot` 4) (fromIntegral w64)
           goRem32 (w64 `shiftR` 32) (nremStart + 4)
      else goRem32 w64 nremStart
  IO.liftIO $ IO $ \s# ->
    case unsafeFreezeByteArray# mba# s# of
      (# s'#, ba# #) -> (# s'#, SBS ba# #)
{-# INLINE genShortByteStringIO' #-}

writeWord8 :: MBA RealWorld -> Int -> Word8 -> IO ()
writeWord8 (MBA mba#) (I# i#) (W8# w#) =
  IO $ \s# -> (# writeWord8Array# mba# i# w# s#, () #)
{-# INLINE writeWord8 #-}

-- Architecture independent helpers:

writeWord32LE :: MBA RealWorld -> Int -> Word32 -> IO ()
writeWord32LE (MBA mba#) (I# i#) w =
  IO $ \s# -> (# writeWord32Array# mba# i# wle# s#, () #)
  where
    !(W32# wle#)
      | homeEndian == BigEndian = byteSwap32 w
      | otherwise = w
{-# INLINE writeWord32LE #-}

writeWord64LE :: MBA RealWorld -> Int -> Word64 -> IO ()
writeWord64LE mba@(MBA mba#) i@(I# i#) w64@(W64# w#)
  | wordSizeInBits == 64 = do
    let !wle#
          | homeEndian == BigEndian = byteSwap64# w#
          | otherwise = w#
    IO $ \s# -> (# writeWord64Array# mba# i# wle# s#, () #)
  | otherwise = do
    let !i' = i * 2
    writeWord32LE mba i' (fromIntegral w64)
    writeWord32LE mba (i' + 1) (fromIntegral (w64 `shiftR` 32))
{-# INLINE writeWord64LE #-}

-- genShortByteStringIO' :: Int -> IO Word64 -> IO ShortByteString
-- genShortByteStringIO' n0 gen =
--   stToIO (runIdentityT (genShortByteStringT n0 (IdentityT (unsafeIOToST gen))))
-- {-# INLINE genShortByteStringIO' #-}

-- genShortByteStringT ::
--      (MonadTrans t, Monad (t (ST s)))
--   => Int -- ^ Number of bytes to generate
--   -> t (ST s) Word64 -- ^ IO action that can generate 8 random bytes at a time
--   -> t (ST s) ShortByteString
-- genShortByteStringT n0 gen64 = do
--   let !n@(I# n#) = max 0 n0
--       !n64 = n `quot` 8
--       !nrem = n `rem` 8
--       !nremStart = n - nrem
--   mba@(MBA mba#) <-
--     lift $ ST $ \s# ->
--       case newByteArray# n# s# of
--         (# s'#, mba# #) -> (# s'#, MBA mba# #)
--   let go i =
--         when (i < n64) $ do
--           w64 <- gen64
--           -- Writing 8 bytes at a time in a Little-endian order gives us
--           -- platform portability
--           lift $ writeWord64LE mba i w64
--           go (i + 1)
--   go 0
--   when (nrem > 0) $ do
--     w64 <- gen64
--     (nremStart32, w32) <-
--       lift $
--       if nrem >= 4
--       then (nremStart + 4, w64 `shiftR` 32) <$
--            writeWord32LE mba (nremStart `quot` 4) (fromIntegral w64)
--       else pure (nremStart, w64)
--     -- In order to not mess up the byte order we write 1 byte at a time in
--     -- Little endian order. It is tempting to simply generate as many bytes as we
--     -- still need using smaller generators (eg. uniformWord8), but that would
--     -- result in inconsistent tail when total length is slightly varied.
--     let goRem32 z i =
--           when (i < n) $ do
--             writeWord8 mba i (fromIntegral z :: Word8)
--             goRem32 (z `shiftR` 8) (i + 1)
--     lift $ goRem32 w32 nremStart32
--   lift $ ST $ \s# ->
--     case unsafeFreezeByteArray# mba# s# of
--       (# s'#, ba# #) -> (# s'#, SBS ba# #)
-- {-# INLINE genShortByteStringT #-}

-- st_ :: (State# s -> State# s) -> ST s ()
-- st_ m# = ST $ \s# -> (# m# s#, () #)
-- {-# INLINE st_ #-}

-- writeWord8 :: MBA s -> Int -> Word8 -> ST s ()
-- writeWord8 (MBA mba#) (I# i#) (W8# w#) = st_ (writeWord8Array# mba# i# w#)
-- {-# INLINE writeWord8 #-}

-- writeWord64LE :: MBA s -> Int -> Word64 -> ST s ()
-- {-# INLINE writeWord64LE #-}

-- #if WORD_SIZE_IN_BITS >= 64
-- writeWord64LE (MBA mba#) (I# i#) (W64# w#) =
--   st_ (writeWord64Array# mba# i# wle#)
--   where
-- #ifdef WORDS_BIGENDIAN
--     !wle# = byteSwap64# w#
-- #else
--     !wle# = w#
-- #endif /* WORDS_BIGENDIAN */

-- #else
-- writeWord64LE mba i w64 = do
--   let !i' = i * 2
--   writeWord32LE mba i' (fromIntegral (w64 `shiftR` 32))
--   writeWord32LE mba (i' + 1) (fromIntegral w64)
-- #endif /* WORD_SIZE_IN_BITS */

-- writeWord32LE :: MBA s -> Int -> Word32 -> ST s ()
-- writeWord32LE (MBA mba#) (I# i#) w =
--   st_ (writeWord32Array# mba# i# wle#)
--   where
-- #ifdef WORDS_BIGENDIAN
--     !(W32# wle#) = byteSwap32 w
-- #else
--     !(W32# wle#) = w
-- #endif /* WORD_SIZE_IN_BITS */
-- {-# INLINE writeWord32LE #-}
