data Possibly a = LolNope
                  | Yeppers a deriving (Eq, Show)

instance Functor Possibly where 
  fmap :: (a -> b) -> Possibly a -> Possibly b
  fmap _ LolNope = LolNope
  fmap f (Yeppers x) = Yeppers f x
