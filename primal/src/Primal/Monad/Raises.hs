{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Primal.Monad.Raises
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Monad.Raises
  ( Raises(..)
  , raiseLeft
  , handleExceptT
  ) where

import Control.Exception
import Control.Monad.ST
import Control.Monad.ST.Unsafe
import GHC.Conc.Sync (STM(..))
import GHC.Exts
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Except (ExceptT(..), throwE, catchE)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.Maybe (MaybeT(..))
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

-- | Handle an exception of a specific type and re-raise any other exception
-- into the `Raises` monad.
--
-- @since 1.0.0
handleExceptT :: (Exception e, Raises m) => ExceptT SomeException m a -> ExceptT e m a
handleExceptT m =
  catchE m $ \exc ->
    case fromException exc of
      Just e -> throwE e
      Nothing -> lift $ raiseM exc
{-# INLINE handleExceptT #-}

-- | A class for monads in which exceptions may be thrown.
--
-- Instances should obey the following law:
--
-- > raiseM e >> x = raiseM e
--
-- In other words, throwing an exception short-circuits the rest of the monadic
-- computation.
--
-- === Note
--
-- This is an almost identical class to
-- [MonadThrow](https://hackage.haskell.org/package/exceptions/docs/Control-Monad-Catch.html#t:MonadThrow)
-- from @exceptions@ package. The reason why it was copied, instead of a direct dependency
-- on the aforementioned package is because @MonadCatch@ and @MonadMask@ are not right
-- abstractions for exception handling in presence of concurrency and also because
-- instances for such transformers as `MaybeT` and `ExceptT` are flawed.
class Monad m => Raises m where
  -- | Throw an exception. Note that this throws when this action is run in
  -- the monad @m@, not when it is applied. It is a generalization of
  -- "Primal.Exception"'s 'Primal.Exception.throw'.
  --
  raiseM :: Exception e => e -> m a

instance Raises Maybe where
  raiseM _ = Nothing

instance e ~ SomeException => Raises (Either e) where
  raiseM = Left . toException

instance Raises IO where
  raiseM = throwIO

instance Raises (ST s) where
  raiseM e = unsafeIOToST $ throwIO e

instance Raises STM where
  raiseM e = STM $ raiseIO# (toException e)


instance Raises m => Raises (ContT r m) where
  raiseM = lift . raiseM

instance (e ~ SomeException, Monad m) => Raises (ExceptT e m) where
  raiseM e = ExceptT (pure (Left (toException e)))

instance Raises m => Raises (IdentityT m) where
  raiseM = lift . raiseM

instance Monad m => Raises (MaybeT m) where
  raiseM _ = MaybeT (pure Nothing)

instance Raises m => Raises (ReaderT r m) where
  raiseM = lift . raiseM

instance (Monoid w, Raises m) => Raises (Lazy.RWST r w s m) where
  raiseM = lift . raiseM

instance (Monoid w, Raises m) => Raises (Strict.RWST r w s m) where
  raiseM = lift . raiseM

instance Raises m => Raises (Lazy.StateT s m) where
  raiseM = lift . raiseM

instance Raises m => Raises (Strict.StateT s m) where
  raiseM = lift . raiseM

instance (Monoid w, Raises m) => Raises (Lazy.WriterT w m) where
  raiseM = lift . raiseM

instance (Monoid w, Raises m) => Raises (Strict.WriterT w m) where
  raiseM = lift . raiseM

#if MIN_VERSION_transformers(0, 5, 3)

instance (Monoid w, Raises m) => Raises (AccumT w m) where
  raiseM = lift . raiseM
instance Raises m => Raises (SelectT r m) where
  raiseM = lift . raiseM

#if MIN_VERSION_transformers(0, 5, 6)

instance Raises m => Raises (CPS.RWST r w st m) where
  raiseM = lift . raiseM
instance Raises m => Raises (CPS.WriterT w m) where
  raiseM = lift . raiseM

#endif
#endif


-- | Raise an exception when it is supplied with Left or return a value unmodified upon Right.
--
-- @since 1.0.0
raiseLeft :: (Exception e, Raises m) => Either e a -> m a
raiseLeft =
  \case
    Left exc -> raiseM exc
    Right res -> pure res
