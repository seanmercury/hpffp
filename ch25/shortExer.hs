{-# LANGUAGE InstanceSigs #-}

import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs 

rev :: [Char] -> [Char]
rev xs = reverse xs


composed :: [Char] -> [Char] 
composed = cap . rev

fmapped :: [Char] -> [Char] 
fmapped = cap <$> rev 

newtype Compose f g a = 
  Compose { getCompose :: f (g a) } deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) =
    Compose $ (fmap . fmap) f fga

newtype Constant a b =
  Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant k) = Constant k

newtype Constant' b a =
  Constant' { getConstant' :: a } deriving (Eq, Show)

instance Functor (Constant' b) where
  fmap f (Constant' k) = Constant' (f k)

data Tuple a b = Tuple a b deriving (Eq, Show)


instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose $ (pure . pure) a

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose f) <*> (Compose a) = Compose $ (<*>) <$> f <*> a

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap :: Monoid m => (a -> m) -> (Compose f g) a -> m
  foldMap f (Compose a) = (foldMap . foldMap) f a 

instance (Traversable k, Traversable l) => Traversable (Compose k l) where
  traverse :: Applicative f => (a -> f b) -> (Compose k l) a -> f ((Compose k l) b)
  traverse f (Compose a) = Compose <$> (traverse . traverse) f a



newtype ReaderT c m a = ReaderT (c -> m a)

runReaderT :: ReaderT c m a -> (c -> m a)
runReaderT (ReaderT f) = f

instance Functor m => Functor (ReaderT c m) where
  fmap = undefined "..." -- boilerplate, exact same as from before

instance Applicative m => Applicative (ReaderT c m) where
  pure = undefined "..." -- boilerplate, exact same as from before
  (<*>) = undefined "..." -- boilerplate, exact same as from before

instance Monad m => Monad (ReaderT c m) where
  (>>=) = undefined "..." -- boilerplate, exact same as from before

class Monad m => MonadReader c m where
  ask :: m c

instance Monad m => MonadReader c (ReaderT c m) where
  ask = ReaderT $ pure