import Test.QuickCheck


functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity x = fmap id x == x

functorCompose :: (Functor f, Eq (f c)) => 
                                            (a -> b)
                                            -> (b -> c) 
                                            -> f a
                                            -> Bool
functorCompose f g x =
  (fmap g (fmap f x)) == (fmap (g . f) x)                                            



newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap :: (a -> b) -> Identity a -> Identity b
  fmap f (Identity a) = Identity $ f a

instance Show (String -> String) where
  show _ = "String->String"

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = getGen where
    getGen = do
      a <- arbitrary
      return (Identity a)



data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap :: (a -> b) -> (Pair a) -> (Pair b)
  fmap f (Pair x y) = Pair (f x) (f y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = getGen where
    getGen = do
      a <- arbitrary
      return (Pair a a)


      

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap :: (x -> y) -> Two a x -> Two a y
  fmap f (Two x y) = Two x (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = getGen where
    getGen = do
      a <- arbitrary
      b <- arbitrary
      return (Two a b)


data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap :: (x -> y) -> Three a b x -> Three a b y
  fmap f (Three x y z) = Three x y (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = getGen where
    getGen = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      return (Three a b c)

data Three' a b = Three' a b b deriving (Eq, Show)
instance Functor (Three' a) where
  fmap :: (x -> y) -> Three' a x -> Three' a y
  fmap f (Three' x y z) = Three' x (f y) (f z)


instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = getGen where
    getGen = do
      a <- arbitrary
      b <- arbitrary
      return (Three' a b b)

data Four a b c d = Four a b c d deriving (Eq, Show)
instance Functor (Four a b c) where
  fmap :: (x -> y) -> (Four a b c x) -> (Four a b c y)
  fmap f (Four x y z w) = Four x y z (f w)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = getGen where
    getGen = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      d <- arbitrary
      return (Four a b c d)

data Four' a b = Four' a a a b deriving (Eq, Show)
instance Functor (Four' a) where
  fmap :: (x -> y) -> (Four' a x) -> (Four' a y)
  fmap f (Four' x y z w) = Four' x y z (f w) 

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = getGen where
    getGen = do
      a <- arbitrary
      b <- arbitrary
      return (Four' a a a b)

main :: IO ()
main = do
  putStrLn "Identity a"
  quickCheck (functorIdentity :: Identity String -> Bool)
  quickCheck (functorCompose :: (String -> String) -> (String -> String) -> Identity String -> Bool)

  putStrLn "Pair a"
  quickCheck (functorIdentity :: Pair String -> Bool)
  quickCheck (functorCompose :: (String -> String) -> (String -> String) -> Pair String -> Bool)
  
  putStrLn "Two a b"
  quickCheck (functorIdentity :: Two String String -> Bool)
  quickCheck (functorCompose :: (String -> String) -> (String -> String) -> Two String String -> Bool)
  
  putStrLn "Three a b c"
  quickCheck (functorIdentity :: Three String String String -> Bool)
  quickCheck (functorCompose :: (String -> String) -> (String -> String) -> Three String String String -> Bool)
  
  putStrLn "Three' a b"
  quickCheck (functorIdentity :: Three' String String -> Bool)
  quickCheck (functorCompose :: (String -> String) -> (String -> String) -> Three String String String -> Bool)
  
  putStrLn "Four a b c d"
  quickCheck (functorIdentity :: Four String String String String -> Bool)
  quickCheck (functorCompose :: (String -> String) -> (String -> String) -> Four String String String String -> Bool)
  
  putStrLn "Four' a b"
  quickCheck (functorIdentity :: Four' String String -> Bool)
  quickCheck (functorCompose :: (String -> String) -> (String -> String) -> Four' String String -> Bool)

