{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Primal.Monad.Internal
-- Copyright   : (c) Alexey Kuleshevich 2020-2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module Primal.Monad.Internal
  ( RW
  , RealWorld
  , PrimalIO
  , Primal (..)
  , PrimalState (..)
  , UnliftPrimalIO
  , UnliftPrimal (..)
  , ST
  , unIO
  , unIO_
  , unST
  , unST_
  , runST
  , primal_
  , primalState_
  , withRunInIO
  , withRunInPrimalState
  , runInPrimalState
  , liftP
  , liftIO
  , liftST
  , primalStateToIO
  , primalStateToST
  ) where

import Control.Exception (SomeException)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Identity (IdentityT (..))
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.RWS.Lazy as Lazy (RWST)
import Control.Monad.Trans.RWS.Strict as Strict (RWST)
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.State.Lazy as Lazy (StateT)
import Control.Monad.Trans.State.Strict as Strict (StateT)
import Control.Monad.Trans.Writer.Lazy as Lazy (WriterT)
import Control.Monad.Trans.Writer.Strict as Strict (WriterT)
import GHC.Exts
import GHC.IO hiding (liftIO)
import GHC.ST hiding (liftST)
import Primal.Monad.Raises

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

type PrimalIO m = Primal RW m

type UnliftPrimalIO m = UnliftPrimal RW m

class Raises m => Primal s m | m -> s where
  -- | Construct a primal action from a state passing function.
  primal :: (State# s -> (# State# s, a #)) -> m a

class Primal s m => UnliftPrimal s m where
  withRunInST :: ((forall a. m a -> ST s a) -> ST s b) -> m b

  runInPrimalState1
    :: (a -> m b)
    -> ((a -> State# s -> (# State# s, b #)) -> State# s -> (# State# s, c #))
    -> m c
  runInPrimalState1 m f# = runInPrimalState2 (\_ -> pure ()) m (\_ -> f#)
  {-# INLINE runInPrimalState1 #-}

  runInPrimalState2
    :: (a -> m b)
    -> (c -> m d)
    -> ( (a -> State# s -> (# State# s, b #))
         -> (c -> State# s -> (# State# s, d #))
         -> State# s
         -> (# State# s, e #)
       )
    -> m e
  runInPrimalState2 m1 m2 f# =
    withRunInST $ \run ->
      ST (f# (\a -> unST (run (m1 a))) (\c -> unST (run (m2 c))))
  {-# INLINE runInPrimalState2 #-}

-- | This type class provides a way to get the underlying state token passing function
-- from the monadic action. There are only two sensible monads that can have an instance:
-- `IO` and `ST` monads, or any other newtype wrapper around it, like @`IdentityT` `IO`@
-- for example.
--
-- Property:
--
-- @@@
-- 'primal' ('primalState' m) === m
-- @@@
class UnliftPrimal s m => PrimalState s m where
  -- | Get the state passing function from the primal action
  primalState :: m a -> State# s -> (# State# s, a #)

instance PrimalState RealWorld IO where
  primalState (IO m) = m
  {-# INLINE primalState #-}

instance PrimalState s (ST s) where
  primalState (ST m) = m
  {-# INLINE primalState #-}

instance PrimalState s m => PrimalState s (IdentityT m) where
  primalState (IdentityT m) = primalState m
  {-# INLINE primalState #-}

runInPrimalState
  :: forall s m a b
   . UnliftPrimal s m
  => m a
  -> ((State# s -> (# State# s, a #)) -> State# s -> (# State# s, b #))
  -> m b
runInPrimalState f g# = runInPrimalState1 (const f) (\f# -> g# (f# ()))
{-# INLINE runInPrimalState #-}

withRunInIO
  :: forall m b
   . UnliftPrimal RW m
  => ((forall a. m a -> IO a) -> IO b)
  -> m b
withRunInIO f = withRunInST $ \run -> coerce (f (coerce . run))
{-# INLINE withRunInIO #-}

withRunInPrimalState
  :: (UnliftPrimal s m, PrimalState s n)
  => ((forall a. m a -> n a) -> n b)
  -> m b
withRunInPrimalState inner =
  withRunInST $ \run -> liftP (inner (liftST . run))
{-# INLINE withRunInPrimalState #-}

instance UnliftPrimal RealWorld IO where
  withRunInST inner = coerce (inner liftP)
  {-# INLINE withRunInST #-}
  runInPrimalState1 io f# = IO (f# (\e -> unIO (io e)))
  {-# INLINE runInPrimalState1 #-}
  runInPrimalState2 io1 io2 f# = IO (f# (\e -> unIO (io1 e)) (\e -> unIO (io2 e)))
  {-# INLINE runInPrimalState2 #-}

instance UnliftPrimal s (ST s) where
  withRunInST inner = inner liftP
  {-# INLINE withRunInST #-}
  runInPrimalState1 st f# = ST (f# (\e -> unST (st e)))
  {-# INLINE runInPrimalState1 #-}
  runInPrimalState2 st1 st2 f# = ST (f# (\e -> unST (st1 e)) (\e -> unST (st2 e)))
  {-# INLINE runInPrimalState2 #-}

instance UnliftPrimal s m => UnliftPrimal s (IdentityT m) where
  withRunInST inner = IdentityT $ withRunInST $ \run -> inner (run . runIdentityT)
  {-# INLINE withRunInST #-}
  runInPrimalState1 im f# = IdentityT $ runInPrimalState1 (runIdentityT . im) f#
  {-# INLINE runInPrimalState1 #-}
  runInPrimalState2 im1 im2 f# =
    IdentityT $ runInPrimalState2 (runIdentityT . im1) (runIdentityT . im2) f#
  {-# INLINE runInPrimalState2 #-}

instance UnliftPrimal s m => UnliftPrimal s (ReaderT r m) where
  withRunInST inner = ReaderT $ \r -> withRunInST $ \run -> inner (run . flip runReaderT r)
  {-# INLINE withRunInST #-}
  runInPrimalState1 rm f# =
    ReaderT $ \r -> runInPrimalState1 (\x -> runReaderT (rm x) r) f#
  {-# INLINE runInPrimalState1 #-}
  runInPrimalState2 rm1 rm2 f# =
    ReaderT $ \r -> runInPrimalState2 (\x -> runReaderT (rm1 x) r) (\x -> runReaderT (rm2 x) r) f#
  {-# INLINE runInPrimalState2 #-}

instance Primal RealWorld IO where
  primal = IO
  {-# INLINE primal #-}

instance Primal s (ST s) where
  primal = ST
  {-# INLINE primal #-}

instance Primal s m => Primal s (ContT r m) where
  primal = lift . primal
  {-# INLINE primal #-}

instance (e ~ SomeException, Primal s m) => Primal s (ExceptT e m) where
  primal = lift . primal
  {-# INLINE primal #-}

instance Primal s m => Primal s (IdentityT m) where
  primal = lift . primal
  {-# INLINE primal #-}

instance Primal s m => Primal s (MaybeT m) where
  primal = lift . primal
  {-# INLINE primal #-}

instance Primal s m => Primal s (ReaderT r m) where
  primal = lift . primal
  {-# INLINE primal #-}

instance (Monoid w, Primal s m) => Primal s (Lazy.RWST r w st m) where
  primal = lift . primal
  {-# INLINE primal #-}
instance (Monoid w, Primal s m) => Primal s (Strict.RWST r w st m) where
  primal = lift . primal
  {-# INLINE primal #-}

instance Primal s m => Primal s (Lazy.StateT st m) where
  primal = lift . primal
  {-# INLINE primal #-}
instance Primal s m => Primal s (Strict.StateT st m) where
  primal = lift . primal
  {-# INLINE primal #-}

instance (Monoid w, Primal s m) => Primal s (Lazy.WriterT w m) where
  primal = lift . primal
  {-# INLINE primal #-}
instance (Monoid w, Primal s m) => Primal s (Strict.WriterT w m) where
  primal = lift . primal
  {-# INLINE primal #-}

#if MIN_VERSION_transformers(0, 5, 3)

instance (Monoid w, Primal s m) => Primal s (AccumT w m) where
  primal = lift . primal
  {-# INLINE primal #-}
instance Primal s m => Primal s (SelectT r m) where
  primal = lift . primal
  {-# INLINE primal #-}

#if MIN_VERSION_transformers(0, 5, 6)

instance Primal s m => Primal s (CPS.RWST r w st m) where
  primal = lift . primal
  {-# INLINE primal #-}
instance Primal s m => Primal s (CPS.WriterT w m) where
  primal = lift . primal
  {-# INLINE primal #-}

#endif
#endif

-- | Get out the inner state token passing function from the `PrimalState` monad action.
--
-- @since 1.0.0
primalState_ :: PrimalState s m => m () -> State# s -> State# s
primalState_ m s =
  case primalState m s of
    (# s', () #) -> s'
{-# INLINE primalState_ #-}

-- | Construct a primitive action that returns unit. Useful for working with effectful
-- primops.
--
-- @since 1.0.0
primal_ :: Primal s m => (State# s -> State# s) -> m ()
primal_ f = primal $ \s -> (# f s, () #)
{-# INLINE primal_ #-}

-- | Lift an `IO` action to `Primal` with the `RealWorld` state token. Type restricted
-- synonym for `liftP`
liftIO :: Primal RW m => IO a -> m a
liftIO (IO m) = primal m
{-# INLINE liftIO #-}

-- | Lift an `ST` action to `Primal` with the same state token. Type restricted synonym
-- for `liftP`
liftST :: Primal s m => ST s a -> m a
liftST (ST m) = primal m
{-# INLINE liftST #-}

-- | Lift an action from the `PrimalState` monad to a different `Primal` monad with the
-- same state token.
liftP :: (PrimalState s n, Primal s m) => n a -> m a
liftP m = primal (primalState m)
{-# INLINE [0] liftP #-}

{-# RULES
"liftP/id" liftP = id
  #-}

-- | Restrict a `PrimalState` action that works with `RealWorld` to `IO`.
primalStateToIO :: PrimalState RealWorld m => m a -> IO a
primalStateToIO = liftP
{-# INLINE primalStateToIO #-}

-- | Restrict a `PrimalState` action that works in `ST`.
primalStateToST :: PrimalState s m => m a -> ST s a
primalStateToST = liftP
{-# INLINE primalStateToST #-}

-- | Unwrap `ST` action
unST :: ST s a -> State# s -> (# State# s, a #)
unST (ST m) = m
{-# INLINE unST #-}

-- | Unwrap `ST` action that returns unit
unST_ :: ST s () -> State# s -> State# s
unST_ (ST m) s =
  case m s of
    (# s', () #) -> s'
{-# INLINE unST_ #-}

-- | Unwrap `IO` action that returns unit
unIO_ :: IO () -> State# RW -> State# RW
unIO_ (IO m) s =
  case m s of
    (# s', () #) -> s'
{-# INLINE unIO_ #-}
