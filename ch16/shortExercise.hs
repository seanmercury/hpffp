data Sum a b = First a
              | Second b deriving (Eq, Show)

instance Functor (Sum a) where 
  fmap :: (a -> b) -> Sum a -> Sum b
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)


