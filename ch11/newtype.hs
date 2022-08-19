class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

-- ghci> tooMany 23

-- <interactive>:38:1: error:
--     • Ambiguous type variable ‘a0’ arising from a use of ‘tooMany’


-- ghci> tooMany (23::Int)
-- False

newtype Goats = Goats Int deriving Show

instance TooMany Goats where
  tooMany (Goats n) = n > 43

-- ghci> tooMany (Goats 23)
-- False

-- newtype CustomType = CustomType (Int, String) deriving Show

-- instance TooMany CustomType where
--   tooMany (CustomType n) = fst n > 42 || snd n > "a"


-- Reusing the TooMany type class, write an instance of the type class
-- for the type (Int, String). This will require adding a language
-- pragma named FlexibleInstances4 if you do not use a newtype—
-- GHC will tell you what to do. ???????????  TODO  - ask Daniel
instance TooMany (Int, String) where
  tooMany n = fst n > 42 || snd n > "a"

-- instance TooMany (Int, Int) where
--   tooMany n = tooMany (fst n + snd n )

-- instance TooMany (Int, Int) where
--   tooMany (n, m) = tooMany (n + m )

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (n, m) = tooMany (n+m)