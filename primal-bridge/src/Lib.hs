-- |
-- Module      : Lib
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Lib
  ( someFunc
  ) where

import Data.Array.Storable
import Data.Array.Storable.Internal
import Data.Array.Base

someFunc :: IO ()
someFunc = putStrLn "someFunc"

instance UnliftPrimal RealWorld m => PtrAccess m (StorableArray i a) where
  withPtrAccess sa f =
    withRunInPrimalState $ \run -> withStorableArray sa (run . f . castPtr)
  toForeugnPtr (StorableArray _ _ _ fp) = fp

instance (Ix i, MArray StorableArray e IO) =>
         ReadAccess RealWorld IO (StorableArray i e) where
  countPrim sa = countSize . rangeSize <$> getBounds sa
  readPrim bs i = withPtrAccess bs (`readOffPtr` i)
  copyToMBytes bs srcOff mb dstOff c =
    withPtrAccess bs $ \(Ptr p#) -> copyPtrToMBytes (Ptr p#) srcOff mb dstOff c
  copyToPtr bs srcOff dstPtr dstOff c =
    withPtrAccess bs $ \(Ptr p#) -> copyPtrToPtr (Ptr p#) srcOff dstPtr dstOff c

instance (Ix i, MArray StorableArray e IO) =>
         WriteAccess RealWorld IO (StorableArray i e) where
  writePrim bs i a = withPtrAccess bs (\ ptr -> writeOffPtr ptr i a)
  moveToMBytes bs srcOff mb dstOff c =
    withPtrAccess bs $ \ptr -> movePtrToMBytes ptr srcOff mb dstOff c
  moveToPtr bs srcOff dstPtr dstOff c =
    withPtrAccess bs $ \ptr -> movePtrToPtr ptr srcOff dstPtr dstOff c


instance Alloc IO (StorableArray Int Word8) where
  mallocBytes (Count c) = unsafeNewArray_ (0, c)

