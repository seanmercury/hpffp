import Control.Monad

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where 
  fmap :: (b -> c) -> Sum a b -> Sum a c
  fmap _ (First x) = First x
  fmap f (Second y) = Second (f y)

instance Applicative (Sum a) where 
  pure = Second
  (<*>) _ (First x) = First x
  (<*>) (First x) _ = First x
  (<*>) (Second f) (Second x) = Second (f x)

-- instance Monad (Sum a) where 
--   return = pure
--   (>>=) :: Sum a b -> (b -> Sum a c) -> Sum a c
--   (>>=) (First x) _ = First x
--   (>>=) (Second x) f = join $ Second (f x)

instance Monad (Sum a) where 
  return = pure
  (>>=) :: Sum a b -> (b -> Sum a c) -> Sum a c
  (>>=) (First x) _ = First x
  (>>=) (Second x) f = f x