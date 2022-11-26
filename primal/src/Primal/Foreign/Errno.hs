{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Primal.Foreign.Errno
-- Copyright   : (c) Alexey Kuleshevich 2020-2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module Primal.Foreign.Errno (
  module Foreign.C.Error,
  getErrno,
  resetErrno,
  throwErrno,
  throwErrnoIf,
  throwErrnoIfMinus1,
  throwErrnoIfMinus1Retry,
  throwErrnoIfMinus1RetryMayBlock,
  throwErrnoIfMinus1RetryMayBlock_,
  throwErrnoIfMinus1Retry_,
  throwErrnoIfMinus1_,
  throwErrnoIfNull,
  throwErrnoIfNullPred,
  throwErrnoIfNullRetry,
  throwErrnoIfNullRetryMayBlock,
  throwErrnoIfRetry,
  throwErrnoIfRetryMayBlock,
  throwErrnoIfRetryMayBlock_,
  throwErrnoIfRetry_,
  throwErrnoIf_,
  throwErrnoPath,
  throwErrnoPathIf,
  throwErrnoPathIfMinus1,
  throwErrnoPathIfMinus1_,
  throwErrnoPathIfNull,
  throwErrnoPathIf_,
) where

import Foreign.C.Error hiding (
  getErrno,
  resetErrno,
  throwErrno,
  throwErrnoIf,
  throwErrnoIfMinus1,
  throwErrnoIfMinus1Retry,
  throwErrnoIfMinus1RetryMayBlock,
  throwErrnoIfMinus1RetryMayBlock_,
  throwErrnoIfMinus1Retry_,
  throwErrnoIfMinus1_,
  throwErrnoIfNull,
  throwErrnoIfNullRetry,
  throwErrnoIfNullRetryMayBlock,
  throwErrnoIfRetry,
  throwErrnoIfRetryMayBlock,
  throwErrnoIfRetryMayBlock_,
  throwErrnoIfRetry_,
  throwErrnoIf_,
  throwErrnoPath,
  throwErrnoPathIf,
  throwErrnoPathIfMinus1,
  throwErrnoPathIfMinus1_,
  throwErrnoPathIfNull,
  throwErrnoPathIf_,
 )
import qualified Foreign.C.Error as GHC
import Foreign.Ptr (Ptr, nullPtr)
import Primal.Exception (raise)
import Primal.Monad
import Primal.Monad.Unsafe

-- TODO: Create an ErrnoException, instead pf relying on IOExcetion. Will require
-- reimplementing all functions in this module

-- | Lifted version of `GHC.throwErrno
getErrno :: Primal s m => m Errno
getErrno = unsafeIOToPrimal GHC.getErrno
{-# INLINE getErrno #-}

-- | Lifted version of `GHC.throwErrno
resetErrno :: Primal RW m => m ()
resetErrno = liftIO GHC.resetErrno
{-# INLINE resetErrno #-}

-- | Lifted version of `GHC.throwErrno
throwErrno :: Primal s m => String -> m Errno
throwErrno = unsafeIOToPrimal . GHC.throwErrno
{-# INLINE throwErrno #-}

-- | Lifted version of `GHC.throwErrnoIf`
throwErrnoIf :: UnliftPrimal s m => (a -> Bool) -> String -> m a -> m a
throwErrnoIf p loc f =
  withRunInST $ \run ->
    unsafeIOToST $ GHC.throwErrnoIf p loc (unsafeSTToPrimal (run f))
{-# INLINE throwErrnoIf #-}

-- | Lifted version of `GHC.throwErrnoIfMinus1`
throwErrnoIfMinus1 :: (Eq a, Num a, UnliftPrimal s m) => String -> m a -> m a
throwErrnoIfMinus1 loc f =
  withRunInST $ \run ->
    unsafeIOToST $ GHC.throwErrnoIfMinus1 loc (unsafeSTToPrimal (run f))
{-# INLINE throwErrnoIfMinus1 #-}

-- | Lifted version of `GHC.throwErrnoIfMinus1Retry`
throwErrnoIfMinus1Retry :: (Eq a, Num a, UnliftPrimal RW m) => String -> m a -> m a
throwErrnoIfMinus1Retry loc f =
  withRunInIO $ \run -> GHC.throwErrnoIfMinus1Retry loc (run f)
{-# INLINE throwErrnoIfMinus1Retry #-}

-- | Lifted version of `GHC.throwErrnoIfMinus1RetryMayBlock`
throwErrnoIfMinus1RetryMayBlock :: (Eq a, Num a, UnliftPrimal RW m) => String -> m a -> m b -> m a
throwErrnoIfMinus1RetryMayBlock loc f g =
  withRunInIO $ \run -> GHC.throwErrnoIfMinus1RetryMayBlock loc (run f) (run g)
{-# INLINE throwErrnoIfMinus1RetryMayBlock #-}

-- | Lifted version of `GHC.throwErrnoIfMinus1RetryMayBlock_`
throwErrnoIfMinus1RetryMayBlock_ :: (Eq a, Num a, UnliftPrimal RW m) => String -> m a -> m b -> m ()
throwErrnoIfMinus1RetryMayBlock_ loc f g =
  withRunInIO $ \run -> GHC.throwErrnoIfMinus1RetryMayBlock_ loc (run f) (run g)
{-# INLINE throwErrnoIfMinus1RetryMayBlock_ #-}

-- | Lifted version of `GHC.throwErrnoIfMinus1Retry_`
throwErrnoIfMinus1Retry_ :: (Eq a, Num a, UnliftPrimal RW m) => String -> m a -> m ()
throwErrnoIfMinus1Retry_ loc f =
  withRunInIO $ \run -> GHC.throwErrnoIfMinus1Retry_ loc (run f)
{-# INLINE throwErrnoIfMinus1Retry_ #-}

-- | Lifted version of `GHC.throwErrnoIfMinus1_`
throwErrnoIfMinus1_ :: (Eq a, Num a, UnliftPrimal s m) => String -> m a -> m ()
throwErrnoIfMinus1_ loc f =
  withRunInST $ \run ->
    unsafeIOToST $ GHC.throwErrnoIfMinus1_ loc (unsafeSTToPrimal (run f))
{-# INLINE throwErrnoIfMinus1_ #-}

-- | Lifted version of `GHC.throwErrnoIfNull`
throwErrnoIfNull :: UnliftPrimal s m => String -> m (Ptr a) -> m (Ptr a)
throwErrnoIfNull loc f =
  withRunInST $ \run ->
    unsafeIOToST $ GHC.throwErrnoIfNull loc (unsafeSTToPrimal (run f))
{-# INLINE throwErrnoIfNull #-}

-- | Just like `throwErrnoIfNull`, but apply a predicate to the received `Errno`.
throwErrnoIfNullPred :: Primal s m => (Errno -> Bool) -> String -> m (Ptr a) -> m (Ptr a)
throwErrnoIfNullPred p loc f = do
  resPtr <- f
  when (resPtr == nullPtr) $ do
    errno <- getErrno
    when (p errno) $ do
      raise (errnoToIOError loc errno Nothing Nothing)
  pure resPtr
{-# INLINE throwErrnoIfNullPred #-}

-- | Lifted version of `GHC.throwErrnoIfNullRetry`
throwErrnoIfNullRetry :: UnliftPrimal RW m => String -> m (Ptr a) -> m (Ptr a)
throwErrnoIfNullRetry loc f =
  withRunInIO $ \run -> GHC.throwErrnoIfNullRetry loc (run f)
{-# INLINE throwErrnoIfNullRetry #-}

-- | Lifted version of `GHC.throwErrnoIfNullRetryMayBlock`
throwErrnoIfNullRetryMayBlock :: UnliftPrimal RW m => String -> m (Ptr a) -> m b -> m (Ptr a)
throwErrnoIfNullRetryMayBlock loc f g =
  withRunInIO $ \run ->
    GHC.throwErrnoIfNullRetryMayBlock loc (run f) (run g)
{-# INLINE throwErrnoIfNullRetryMayBlock #-}

-- | Lifted version of `GHC.throwErrnoIfRetry`
throwErrnoIfRetry :: UnliftPrimal RW m => (a -> Bool) -> String -> m a -> m a
throwErrnoIfRetry p loc f =
  withRunInIO $ \run -> GHC.throwErrnoIfRetry p loc (run f)
{-# INLINE throwErrnoIfRetry #-}

-- | Lifted version of `GHC.throwErrnoIfRetryMayBlock`
throwErrnoIfRetryMayBlock :: UnliftPrimal RW m => (a -> Bool) -> String -> m a -> m b -> m a
throwErrnoIfRetryMayBlock p loc f g =
  withRunInIO $ \run -> GHC.throwErrnoIfRetryMayBlock p loc (run f) (run g)
{-# INLINE throwErrnoIfRetryMayBlock #-}

-- | Lifted version of `GHC.throwErrnoIfRetryMayBlock_`
throwErrnoIfRetryMayBlock_ :: UnliftPrimal RW m => (a -> Bool) -> String -> m a -> m b -> m ()
throwErrnoIfRetryMayBlock_ p loc f g =
  withRunInIO $ \run ->
    GHC.throwErrnoIfRetryMayBlock_ p loc (run f) (run g)
{-# INLINE throwErrnoIfRetryMayBlock_ #-}

-- | Lifted version of `GHC.throwErrnoIfRetry_`
throwErrnoIfRetry_ :: UnliftPrimal RW m => (a -> Bool) -> String -> m a -> m ()
throwErrnoIfRetry_ p loc f =
  withRunInIO $ \run -> GHC.throwErrnoIfRetry_ p loc (run f)
{-# INLINE throwErrnoIfRetry_ #-}

-- | Lifted version of `GHC.throwErrnoIf_`
throwErrnoIf_ :: UnliftPrimal s m => (a -> Bool) -> String -> m a -> m ()
throwErrnoIf_ p loc f =
  withRunInST $ \run ->
    unsafeIOToST $ GHC.throwErrnoIf_ p loc (unsafeSTToPrimal (run f))
{-# INLINE throwErrnoIf_ #-}

-- | Lifted version of `GHC.throwErrnoPath`
throwErrnoPath :: Primal s m => String -> FilePath -> m a
throwErrnoPath loc fp = unsafeIOToPrimal $ GHC.throwErrnoPath loc fp
{-# INLINE throwErrnoPath #-}

-- | Lifted version of `GHC.throwErrnoPathIf`
throwErrnoPathIf :: UnliftPrimal s m => (a -> Bool) -> String -> FilePath -> m a -> m a
throwErrnoPathIf p loc fp f =
  withRunInST $ \run ->
    unsafeIOToST $ GHC.throwErrnoPathIf p loc fp (unsafeSTToPrimal (run f))
{-# INLINE throwErrnoPathIf #-}

-- | Lifted version of `GHC.throwErrnoPathIfMinus1`
throwErrnoPathIfMinus1 :: (Eq a, Num a, UnliftPrimal s m) => String -> FilePath -> m a -> m a
throwErrnoPathIfMinus1 loc fp f =
  withRunInST $ \run ->
    unsafeIOToST $ GHC.throwErrnoPathIfMinus1 loc fp (unsafeSTToPrimal (run f))
{-# INLINE throwErrnoPathIfMinus1 #-}

-- | Lifted version of `GHC.throwErrnoPathIfMinus1_`
throwErrnoPathIfMinus1_ :: (Eq a, Num a, UnliftPrimal s m) => String -> FilePath -> m a -> m ()
throwErrnoPathIfMinus1_ loc fp f =
  withRunInST $ \run ->
    unsafeIOToST $ GHC.throwErrnoPathIfMinus1_ loc fp (unsafeSTToPrimal (run f))
{-# INLINE throwErrnoPathIfMinus1_ #-}

-- | Lifted version of `GHC.throwErrnoPathIfNull`
throwErrnoPathIfNull :: UnliftPrimal s m => String -> FilePath -> m (Ptr a) -> m (Ptr a)
throwErrnoPathIfNull loc fp f =
  withRunInST $ \run ->
    unsafeIOToST $ GHC.throwErrnoPathIfNull loc fp (unsafeSTToPrimal (run f))
{-# INLINE throwErrnoPathIfNull #-}

-- | Lifted version of `GHC.throwErrnoPathIf_`
throwErrnoPathIf_ :: UnliftPrimal s m => (a -> Bool) -> String -> FilePath -> m a -> m ()
throwErrnoPathIf_ p loc fp f =
  withRunInST $ \run ->
    unsafeIOToST $ GHC.throwErrnoPathIf_ p loc fp (unsafeSTToPrimal (run f))
{-# INLINE throwErrnoPathIf_ #-}
