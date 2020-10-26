{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  , RealWorld
  , MonadPrim(..)
  , MonadPrimBase(..)
  , MonadUnliftPrim(..)
  , ST
  , unIO
  , unIO_
  , unST
  , unST_
  , runST
  , prim_
  , primBase_
  , withRunInPrimBase
  , runInPrimBase
  , liftPrimIO
  , liftPrimST
  , liftPrimBase
  , primBaseToIO
  , primBaseToST
  ) where

import GHC.Exts
import GHC.IO
import GHC.ST
import Control.Prim.Monad.Throw
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

class MonadThrow m => MonadPrim s m | m -> s where
  -- | Construct a primitive action
  prim :: (State# s -> (# State# s, a #)) -> m a


class MonadPrim s m => MonadUnliftPrim s m where

  withRunInST :: ((forall a. m a -> ST s a) -> ST s b) -> m b

  runInPrimBase1 ::
       (a -> m b)
    -> ( (a -> State# s -> (# State# s, b #)) -> State# s -> (# State# s, c #) )
    -> m c
  runInPrimBase1 m f# = runInPrimBase2 (\_ -> pure ()) m (\_ -> f#)
  {-# INLINE runInPrimBase1 #-}

  runInPrimBase2 ::
       (a -> m b)
    -> (c -> m d)
    -> ( (a -> State# s -> (# State# s, b #))
      -> (c -> State# s -> (# State# s, d #))
      -> State# s -> (# State# s, e #)   )
    -> m e
  runInPrimBase2 m1 m2 f# =
    withRunInST $ \run ->
      ST (f# (\a -> unST (run (m1 a))) (\c -> unST (run (m2 c))))
  {-# INLINE runInPrimBase2 #-}


class MonadUnliftPrim s m => MonadPrimBase s m where
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

runInPrimBase ::
     forall s m a b. MonadUnliftPrim s m
  => m a
  -> ((State# s -> (# State# s, a #)) -> State# s -> (# State# s, b #))
  -> m b
runInPrimBase f g# = runInPrimBase1 (const f) (\f# -> g# (f# ()))
  --withRunInST (\run -> prim (g (primBase (run f))))
{-# INLINE runInPrimBase #-}

withRunInPrimBase ::
     (MonadUnliftPrim s m, MonadPrimBase s n)
  => ((forall a. m a -> n a) -> n b)
  -> m b
withRunInPrimBase inner =
  withRunInST $ \run -> liftPrimBase (inner (liftPrimST . run))
{-# INLINE withRunInPrimBase #-}



instance MonadUnliftPrim RealWorld IO where
  withRunInST inner = coerce (inner liftPrimBase)
  {-# INLINE withRunInST #-}
  runInPrimBase1 io f# = IO (f# (\e -> unIO (io e)))
  {-# INLINE runInPrimBase1 #-}
  runInPrimBase2 io1 io2 f# = IO (f# (\e -> unIO (io1 e)) (\e -> unIO (io2 e)))
  {-# INLINE runInPrimBase2 #-}

instance MonadUnliftPrim s (ST s) where
  withRunInST inner = inner liftPrimBase
  {-# INLINE withRunInST #-}
  runInPrimBase1 st f# = ST (f# (\e -> unST (st e)))
  {-# INLINE runInPrimBase1 #-}
  runInPrimBase2 st1 st2 f# = ST (f# (\e -> unST (st1 e)) (\e -> unST (st2 e)))
  {-# INLINE runInPrimBase2 #-}

instance MonadUnliftPrim s m => MonadUnliftPrim s (IdentityT m) where
  withRunInST inner = IdentityT $ withRunInST $ \run -> inner (run . runIdentityT)
  {-# INLINE withRunInST #-}
  runInPrimBase1 im f# = IdentityT $ runInPrimBase1 (runIdentityT . im) f#
  {-# INLINE runInPrimBase1 #-}
  runInPrimBase2 im1 im2 f# =
    IdentityT $ runInPrimBase2 (runIdentityT . im1) (runIdentityT . im2) f#
  {-# INLINE runInPrimBase2 #-}

instance MonadUnliftPrim s m => MonadUnliftPrim s (ReaderT r m) where
  withRunInST inner = ReaderT $ \r -> withRunInST $ \run -> inner (run . flip runReaderT r)
  {-# INLINE withRunInST #-}
  runInPrimBase1 rm f# =
    ReaderT $ \r -> runInPrimBase1 (\x -> runReaderT (rm x) r) f#
  {-# INLINE runInPrimBase1 #-}
  runInPrimBase2 rm1 rm2 f# =
    ReaderT $ \r -> runInPrimBase2 (\x -> runReaderT (rm1 x) r) (\x -> runReaderT (rm2 x) r) f#
  {-# INLINE runInPrimBase2 #-}


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

instance MonadPrim s m => MonadPrim s (CPS.RWST r w st m) where
  prim = lift . prim
  {-# INLINE prim #-}
instance MonadPrim s m => MonadPrim s (CPS.WriterT w m) where
  prim = lift . prim
  {-# INLINE prim #-}

#endif
#endif

primBase_ :: MonadPrimBase s m => m () -> State# s -> State# s
primBase_ m s = case primBase m s of
                  (# s', () #) -> s'
{-# INLINE primBase_ #-}

-- | Construct a primitive action that does not return anything.
prim_ :: MonadPrim s m => (State# s -> State# s) -> m ()
prim_ f = prim $ \s -> (# f s, () #)
{-# INLINE prim_ #-}

-- | Lift an `IO` action to `MonadPrim` with the `RealWorld` state token. Type restricted
-- synonym for `liftPrimBase`
liftPrimIO :: MonadPrim RW m => IO a -> m a
liftPrimIO (IO m) = prim m
{-# INLINE liftPrimIO #-}

-- | Lift an `ST` action to `MonadPrim` with the same state token. Type restricted synonym
-- for `liftPrimBase`
liftPrimST :: MonadPrim s m => ST s a -> m a
liftPrimST (ST m) = prim m
{-# INLINE liftPrimST #-}

-- | Lift an action from the `MonadPrimBase` to another `MonadPrim` with the same state
-- token.
liftPrimBase :: (MonadPrimBase s n, MonadPrim s m) => n a -> m a
liftPrimBase m = prim (primBase m)
{-# INLINE[0] liftPrimBase #-}
{-# RULES
 "liftPrimBase/id" liftPrimBase = id
 #-}

-- | Restrict a `MonadPrimBase` action that works with `RealWorld` to `IO`.
primBaseToIO :: MonadPrimBase RealWorld m => m a -> IO a
primBaseToIO = liftPrimBase
{-# INLINE primBaseToIO #-}

-- | Restrict a `MonadPrimBase` action that works in `ST`.
primBaseToST :: MonadPrimBase s m => m a -> ST s a
primBaseToST = liftPrimBase
{-# INLINE primBaseToST #-}


-- | Unwrap `ST`
unST :: ST s a -> State# s -> (# State# s, a #)
unST (ST m) = m
{-# INLINE unST #-}


-- | Unwrap `ST` that returns unit
unST_ :: ST s () -> State# s -> State# s
unST_ (ST m) s =
  case m s of
    (# s', () #) -> s'
{-# INLINE unST_ #-}


-- | Unwrap `IO` that returns unit
unIO_ :: IO () -> State# RW -> State# RW
unIO_ (IO m) s =
  case m s of
    (# s', () #) -> s'
{-# INLINE unIO_ #-}
