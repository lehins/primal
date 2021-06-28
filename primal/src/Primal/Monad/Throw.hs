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
  ( Throws(..)
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
-- This is an almost identical class to
-- [Throws](https://hackage.haskell.org/package/exceptions/docs/Control-Monad-Catch.html#t:Throws)
-- from @exceptions@ package. The reason why it was copied, instead of a direct dependency
-- on the aforementioned package is because @MonadCatch@ and @MonadMask@ are not right
-- abstractions for exception handling in presence of concurrency and also because
-- instances for such transformers as `MaybeT` and `ExceptT` are flawed.
class Monad m => Throws m where
  -- | Throw an exception. Note that this throws when this action is run in
  -- the monad @m@, not when it is applied. It is a generalization of
  -- "Primal.Exception"'s 'Primal.Exception.throw'.
  --
  throwM :: Exception e => e -> m a

instance Throws Maybe where
  throwM _ = Nothing

instance e ~ SomeException => Throws (Either e) where
  throwM = Left . toException

instance Throws IO where
  throwM = throwIO

instance Throws (ST s) where
  throwM e = unsafeIOToST $ throwIO e

instance Throws STM where
  throwM e = STM $ raiseIO# (toException e)


instance Throws m => Throws (ContT r m) where
  throwM = lift . throwM

instance (e ~ SomeException, Monad m) => Throws (ExceptT e m) where
  throwM e = ExceptT (pure (Left (toException e)))

instance Throws m => Throws (IdentityT m) where
  throwM = lift . throwM

instance Monad m => Throws (MaybeT m) where
  throwM _ = MaybeT (pure Nothing)

instance Throws m => Throws (ReaderT r m) where
  throwM = lift . throwM

instance (Monoid w, Throws m) => Throws (Lazy.RWST r w s m) where
  throwM = lift . throwM

instance (Monoid w, Throws m) => Throws (Strict.RWST r w s m) where
  throwM = lift . throwM

instance Throws m => Throws (Lazy.StateT s m) where
  throwM = lift . throwM

instance Throws m => Throws (Strict.StateT s m) where
  throwM = lift . throwM

instance (Monoid w, Throws m) => Throws (Lazy.WriterT w m) where
  throwM = lift . throwM

instance (Monoid w, Throws m) => Throws (Strict.WriterT w m) where
  throwM = lift . throwM

#if MIN_VERSION_transformers(0, 5, 3)

instance (Monoid w, Throws m) => Throws (AccumT w m) where
  throwM = lift . throwM
instance Throws m => Throws (SelectT r m) where
  throwM = lift . throwM

#if MIN_VERSION_transformers(0, 5, 6)

instance Throws m => Throws (CPS.RWST r w st m) where
  throwM = lift . throwM
instance Throws m => Throws (CPS.WriterT w m) where
  throwM = lift . throwM

#endif
#endif
