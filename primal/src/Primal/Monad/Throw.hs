{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Primal.Monad.Throw
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Primal.Monad.Throw
  ( MonadThrow(..)
  ) where

import Control.Exception
import Control.Monad.ST
import Control.Monad.ST.Unsafe
import GHC.Conc.Sync (STM(..))
import GHC.Exts
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Except (ExceptT(..))
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


-- | A class for monads in which exceptions may be thrown.
--
-- Instances should obey the following law:
--
-- > throwM e >> x = throwM e
--
-- In other words, throwing an exception short-circuits the rest of the monadic
-- computation.
--
-- === Note
--
-- This is an identical class to
-- [MonadThrow](https://hackage.haskell.org/package/exceptions/docs/Control-Monad-Catch.html#t:MonadThrow)
-- from @exceptions@ package. The reason why it was copied, instead of a direct dependency
-- on the aforementioned package is because @MonadCatch@ and @MonadMask@ are not right
-- abstractions for exception handling in presence of concurrency and also because
-- instances for such transformers as `MaybeT` and `ExceptT` are flawed.
class Monad m => MonadThrow m where
  -- | Throw an exception. Note that this throws when this action is run in
  -- the monad @m@, not when it is applied. It is a generalization of
  -- "Control.Prim.Exception"'s 'Control.Prim.Exception.throw'.
  --
  throwM :: Exception e => e -> m a

instance MonadThrow Maybe where
  throwM _ = Nothing

instance e ~ SomeException => MonadThrow (Either e) where
  throwM = Left . toException

instance MonadThrow IO where
  throwM = throwIO

instance MonadThrow (ST s) where
  throwM e = unsafeIOToST $ throwIO e

instance MonadThrow STM where
  throwM e = STM $ raiseIO# (toException e)


instance MonadThrow m => MonadThrow (ContT r m) where
  throwM = lift . throwM

instance (e ~ SomeException, Monad m) => MonadThrow (ExceptT e m) where
  throwM e = ExceptT (pure (Left (toException e)))

instance MonadThrow m => MonadThrow (IdentityT m) where
  throwM = lift . throwM

instance Monad m => MonadThrow (MaybeT m) where
  throwM _ = MaybeT (pure Nothing)

instance MonadThrow m => MonadThrow (ReaderT r m) where
  throwM = lift . throwM

instance (Monoid w, MonadThrow m) => MonadThrow (Lazy.RWST r w s m) where
  throwM = lift . throwM

instance (Monoid w, MonadThrow m) => MonadThrow (Strict.RWST r w s m) where
  throwM = lift . throwM

instance MonadThrow m => MonadThrow (Lazy.StateT s m) where
  throwM = lift . throwM

instance MonadThrow m => MonadThrow (Strict.StateT s m) where
  throwM = lift . throwM

instance (Monoid w, MonadThrow m) => MonadThrow (Lazy.WriterT w m) where
  throwM = lift . throwM

instance (Monoid w, MonadThrow m) => MonadThrow (Strict.WriterT w m) where
  throwM = lift . throwM

#if MIN_VERSION_transformers(0, 5, 3)

instance (Monoid w, MonadThrow m) => MonadThrow (AccumT w m) where
  throwM = lift . throwM
instance MonadThrow m => MonadThrow (SelectT r m) where
  throwM = lift . throwM

#if MIN_VERSION_transformers(0, 5, 6)

instance MonadThrow m => MonadThrow (CPS.RWST r w st m) where
  throwM = lift . throwM
instance MonadThrow m => MonadThrow (CPS.WriterT w m) where
  throwM = lift . throwM

#endif
#endif
