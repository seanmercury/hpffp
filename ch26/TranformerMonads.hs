class Monad m => MonadReader r m | m -> r where
    ask   :: m r

newtype Reader r a =
  Reader { runReader :: r -> a }

newtype Writer w a =
  Writer { runWriter :: (a, w) }



newtype State s a =
  State { runState :: s -> (a, s) }


newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

newtype WriterT w m a =
  WriterT { runWriterT :: m (a, w) }

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

newtype EitherT e m a = 
  EitherT { runEitherT :: m (Either e a) }

newtype ExceptT e m a =
  ExceptT { runExceptT :: m (Either e a) }

type MyIdentity a = IdentityT Identity a
type Maybe a = MaybeT Identity a
type Either e a = EitherT e Identity a
type Reader r a = ReaderT e Identity a
type State s a = StateT s Identity a

