module Control.Monad.Random.Interface

import Control.Monad.Error.Either
import Control.Monad.Maybe
import Control.Monad.Reader.Reader
import Control.Monad.RWS.CPS
import Control.Monad.State.State
import Control.Monad.Trans
import Control.Monad.Writer.CPS

import public System.Random.Pure

%default total

public export
interface Monad m => MonadRandom m where
  getRandom  : Random a => m a
  getRandomR : Random a => (a, a) -> m a

  independent : m a -> m a

--- Lifting implementations for standard transformers ---

export
MonadRandom m => MonadRandom (EitherT e m) where
  getRandom  = lift getRandom
  getRandomR = lift . getRandomR
  independent $ MkEitherT x = MkEitherT $ independent x

export
MonadRandom m => MonadRandom (MaybeT m) where
  getRandom  = lift getRandom
  getRandomR = lift . getRandomR
  independent $ MkMaybeT x = MkMaybeT $ independent x

export
MonadRandom m => MonadRandom (ReaderT r m) where
  getRandom  = lift getRandom
  getRandomR = lift . getRandomR
  independent $ MkReaderT x = MkReaderT $ independent . x

export
MonadRandom m => MonadRandom (RWST r w s m) where
  getRandom  = lift getRandom
  getRandomR = lift . getRandomR
  independent $ MkRWST x = MkRWST $ \r, s, w => independent $ x r s w

export
MonadRandom m => MonadRandom (StateT s m) where
  getRandom  = lift getRandom
  getRandomR = lift . getRandomR
  independent $ ST x = ST $ independent . x

export
MonadRandom m => MonadRandom (WriterT s m) where
  getRandom  = lift getRandom
  getRandomR = lift . getRandomR
  independent $ MkWriterT x = MkWriterT $ independent . x
