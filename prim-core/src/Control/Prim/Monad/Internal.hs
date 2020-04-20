{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Control.Prim.Monad.Internal
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Control.Prim.Monad.Internal
  ( RW
  , MonadPrim(..)
  , MonadPrimBase(..)
  , MonadUnliftPrim(..)
  , prim_
  , primBase_
  , liftPrimBase
  , primBaseToIO
  , primBaseToST
  ) where

import GHC.Exts
import GHC.IO
import GHC.ST
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Identity (IdentityT(..))
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.RWS.Lazy as Lazy (RWST)
import Control.Monad.Trans.RWS.Strict as Strict (RWST)
import Control.Monad.Trans.State.Lazy as Lazy (StateT)
import Control.Monad.Trans.State.Strict as Strict (StateT)
import Control.Monad.Trans.Writer.Lazy as Lazy (WriterT)
import Control.Monad.Trans.Writer.Strict as Strict (WriterT)

#if MIN_VERSION_transformers(0, 5, 3)
import Control.Monad.Trans.Accum (AccumT)
import Control.Monad.Trans.Select (SelectT)
#if MIN_VERSION_transformers(0, 5, 6)
import Control.Monad.Trans.RWS.CPS as CPS (RWST)
import Control.Monad.Trans.Writer.CPS as CPS (WriterT)
#endif
#endif

-- | A shorter synonym for the magical `RealWorld`
type RW = RealWorld

class MonadPrim s m => MonadPrimBase s m where
  -- | Unwrap a primitive action
  primBase :: m a -> State# s -> (# State# s, a #)

instance MonadPrimBase RealWorld IO where
  primBase (IO m) = m
  {-# INLINE primBase #-}

instance MonadPrimBase s (ST s) where
  primBase (ST m) = m
  {-# INLINE primBase #-}

instance MonadPrimBase s m => MonadPrimBase s (IdentityT m) where
  primBase (IdentityT m) = primBase m
  {-# INLINE primBase #-}


class MonadPrim s m => MonadUnliftPrim s m where
  withRunInPrimBase :: MonadPrimBase s n => ((forall a. m a -> n a) -> n b) -> m b

instance MonadUnliftPrim RealWorld IO where
  withRunInPrimBase inner = liftPrimBase (inner liftPrimBase)
  {-# INLINE withRunInPrimBase #-}

instance MonadUnliftPrim s (ST s) where
  withRunInPrimBase inner = liftPrimBase (inner liftPrimBase)
  {-# INLINE withRunInPrimBase #-}

instance MonadUnliftPrim s m => MonadUnliftPrim s (IdentityT m) where
  withRunInPrimBase inner =
    IdentityT $ withRunInPrimBase $ \run -> inner (run . runIdentityT)
  {-# INLINE withRunInPrimBase #-}

instance MonadUnliftPrim s m => MonadUnliftPrim s (ReaderT r m) where
  withRunInPrimBase inner =
    ReaderT $ \r -> withRunInPrimBase $ \run -> inner (run . flip runReaderT r)
  {-# INLINE withRunInPrimBase #-}


class Monad m => MonadPrim s m | m -> s where
  -- | Construct a primitive action
  prim :: (State# s -> (# State# s, a #)) -> m a

instance MonadPrim RealWorld IO where
  prim = IO
  {-# INLINE prim #-}

instance MonadPrim s (ST s) where
  prim = ST
  {-# INLINE prim #-}


instance MonadPrim s m => MonadPrim s (ContT r m) where
  prim = lift . prim
  {-# INLINE prim #-}

instance MonadPrim s m => MonadPrim s (ExceptT e m) where
  prim = lift . prim
  {-# INLINE prim #-}

instance MonadPrim s m => MonadPrim s (IdentityT m) where
  prim = lift . prim
  {-# INLINE prim #-}

instance MonadPrim s m => MonadPrim s (MaybeT m) where
  prim = lift . prim
  {-# INLINE prim #-}

instance MonadPrim s m => MonadPrim s (ReaderT r m) where
  prim = lift . prim
  {-# INLINE prim #-}


instance (Monoid w, MonadPrim s m) => MonadPrim s (Lazy.RWST r w st m) where
  prim = lift . prim
  {-# INLINE prim #-}
instance (Monoid w, MonadPrim s m) => MonadPrim s (Strict.RWST r w st m) where
  prim = lift . prim
  {-# INLINE prim #-}

instance MonadPrim s m => MonadPrim s (Lazy.StateT st m) where
  prim = lift . prim
  {-# INLINE prim #-}
instance MonadPrim s m => MonadPrim s (Strict.StateT st m) where
  prim = lift . prim
  {-# INLINE prim #-}

instance (Monoid w, MonadPrim s m) => MonadPrim s (Lazy.WriterT w m) where
  prim = lift . prim
  {-# INLINE prim #-}
instance (Monoid w, MonadPrim s m) => MonadPrim s (Strict.WriterT w m) where
  prim = lift . prim
  {-# INLINE prim #-}


#if MIN_VERSION_transformers(0, 5, 3)

instance (Monoid w, MonadPrim s m) => MonadPrim s (AccumT w m) where
  prim = lift . prim
  {-# INLINE prim #-}
instance MonadPrim s m => MonadPrim s (SelectT r m) where
  prim = lift . prim
  {-# INLINE prim #-}

#if MIN_VERSION_transformers(0, 5, 6)

instance (Monoid w, MonadPrim s m) => MonadPrim s (CPS.RWST r w st m) where
  prim = lift . prim
  {-# INLINE prim #-}
instance (Monoid w, MonadPrim s m) => MonadPrim s (CPS.WriterT w m) where
  prim = lift . prim
  {-# INLINE prim #-}

#endif
#endif

primBase_ :: MonadPrimBase s m => m () -> State# s -> State# s
primBase_ m s# = case primBase m s# of
                   (# s'#, () #) -> s'#
{-# INLINE primBase_ #-}

-- | Construct a primitive action that does not return anything.
prim_ :: MonadPrim s m => (State# s -> State# s) -> m ()
prim_ f = prim $ \s# -> (# f s#, () #)
{-# INLINE prim_ #-}

-- | Lift an action from the `MonadPrimBase` to another `MonadPrim` with the same state
-- token.
liftPrimBase :: (MonadPrimBase s n, MonadPrim s m) => n a -> m a
liftPrimBase m = prim (primBase m)
{-# INLINE liftPrimBase #-}

-- | Restrict a `MonadPrimBase` action that works with `RealWorld` to `IO`.
primBaseToIO :: MonadPrimBase RealWorld m => m a -> IO a
primBaseToIO = liftPrimBase
{-# INLINE primBaseToIO #-}

-- | Restrict a `MonadPrimBase` action that works in `ST`.
primBaseToST :: MonadPrimBase s m => m a -> ST s a
primBaseToST = liftPrimBase
{-# INLINE primBaseToST #-}
