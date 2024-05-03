module Control.Monad.Random.Random

import Data.Ref

import Control.Monad.Error.Interface
import public Control.Monad.Identity
import public Control.Monad.Random.Interface
import Control.Monad.Reader.Interface
import Control.Monad.State
import Control.Monad.Writer.Interface

import System.Random.Pure
import System.Random.Pure.StdGen

%default total

--- Transformer ---

public export
data RandomT : (Type -> Type) -> Type -> Type where
  MkRandomT : (forall g. RandomGen g => StateT g m a) -> RandomT m a

public export %inline
unRandomT : RandomGen g => RandomT m a -> StateT g m a
unRandomT $ MkRandomT f = f

public export %inline
runRandomT : RandomGen g => g -> RandomT m a -> m (g, a)
runRandomT g = runStateT g . unRandomT

public export %inline
execRandomT : RandomGen g => Functor m => g -> RandomT m a -> m g
execRandomT g = execStateT g . unRandomT

public export %inline
evalRandomT : RandomGen g => Functor m => g -> RandomT m a -> m a
evalRandomT g = evalStateT g . unRandomT

--- Special runners ---

-- Runs on freshly initialised random seed each time
public export %inline
evalRandomIO : HasIO io => RandomT io a -> io a
evalRandomIO r = evalRandomT !initStdGen r

public export %inline
evalRandomRef : RandomGen g => Monad m => Ref m r => r g -> RandomT m a -> m a
evalRandomRef ref rand = do
  (g', x) <- runRandomT !(readRef ref) rand
  writeRef ref g' $> x

--- Standard implementations ---

public export %inline
Functor m => Functor (RandomT m) where
  map f x = MkRandomT $ map f $ unRandomT x

public export %inline
Monad m => Applicative (RandomT m) where
  pure x = MkRandomT $ pure x
  f <*> x = MkRandomT $ unRandomT f <*> unRandomT x

public export %inline
Alternative m => Monad m => Alternative (RandomT m) where
  empty = MkRandomT empty
  x <|> y = MkRandomT $ unRandomT x <|> unRandomT y

public export %inline
Monad m => Monad (RandomT m) where
  x >>= f = MkRandomT $ unRandomT x >>= unRandomT . f

public export %inline
MonadTrans RandomT where
  lift x = MkRandomT $ lift x

public export %inline
HasIO m => HasIO (RandomT m) where
  liftIO x = MkRandomT $ liftIO x

public export %inline
MonadReader r m => MonadReader r (RandomT m) where
  ask = lift ask
  local f (MkRandomT x) = MkRandomT $ local f x

public export %inline
MonadWriter w m => MonadWriter w (RandomT m) where
  tell = lift . tell
  pass (MkRandomT x) = MkRandomT $ pass x
  listen (MkRandomT x) = MkRandomT $ listen x

public export %inline
MonadState s m => MonadState s (RandomT m) where
  put = lift . put
  get = lift get

public export %inline
MonadError e m => MonadError e (RandomT m) where
  throwError = lift . throwError
  catchError (MkRandomT x) h = MkRandomT $ catchError x $ unRandomT . h

--- `MonadRandom` implementation ---

export
Monad m => MonadRandom (RandomT m) where
  getRandom = MkRandomT random'
  getRandomR bounds = MkRandomT $ randomR' bounds
  independent subR = MkRandomT $ do
    (g', g'') <- split <$> get
    put g'' *> unRandomT subR <* put g'

--- Simple monad ---

-- Name was chosen to not to clash with the `Random` interface.
public export
Rand : Type -> Type
Rand = RandomT Identity

public export %inline
runRandom : RandomGen g => g -> Rand a -> (g, a)
runRandom g = runState g . unRandomT

public export %inline
execRandom : RandomGen g => g -> Rand a -> g
execRandom g = execState g . unRandomT

public export %inline
evalRandom : RandomGen g => g -> Rand a -> a
evalRandom g = evalState g . unRandomT
