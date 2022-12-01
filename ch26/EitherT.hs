import Data.Either.HT

newtype EitherT e m a = 
  EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
  fmap f (EitherT ema) = EitherT $ (fmap . fmap) f ema

instance Applicative m => Applicative (EitherT e m) where
  pure x = EitherT (pure (pure x))
  (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  (<*>) (EitherT emf) (EitherT ema) = EitherT $ (<*>) <$> emf <*> ema

instance Monad m => Monad (EitherT e m) where
  return = pure
  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  (>>=) (EitherT ema) f = EitherT $ do
    v <- ema  -- ema :: m (Either e a)
    case v of
      Left x -> return (Left x)
      Right y -> runEitherT (f y)

-- transformer version of swapEither
swapEitherT :: (Functor m, Monad m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ do
  v <- ema  -- ema :: m (Either e a)
  return (swap v)

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT famc fbmc (EitherT amb) = do
  v <- amb
  case v of
    Left x -> famc x
    Right y -> fbmc y