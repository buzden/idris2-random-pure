module Control.Monad.Random.Interface

import public System.Random.Pure

%default total

public export
interface Monad m => MonadRandom m where
  getRandom  : Random a => m a
  getRandomR : Random a => (a, a) -> m a

  independent : m a -> m a
