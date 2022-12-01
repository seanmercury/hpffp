import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader



-- newtype ReaderT r m a =
--   ReaderT { runReaderT :: r -> m a }

-- newtype MaybeT m a =
--   MaybeT { runMaybeT :: m (Maybe a) }

-- newtype MaybeT (ExceptT String (ReaderT () IO)) a =
--   MaybeT { runMaybeT :: ExceptT String (ReaderT () IO) (Maybe a) }

-- newtype ExceptT e m a =
--   ExceptT { runExceptT :: m (Either e a) }


embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = MaybeT (ExceptT (ReaderT (const ( return (Right (Just 1))))))