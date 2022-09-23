import Control.Applicative
import Test.Hspec
import Test.QuickCheck (arbitrary, Arbitrary, frequency)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid
import Data.Semigroup
import Control.Applicative (liftA3)

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap :: (a -> b) -> Pair a -> Pair b
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure :: a -> Pair a
  pure x = Pair x x 

  (<*>) :: Pair (a -> b) -> Pair a -> Pair b
  (<*>) (Pair x y) (Pair x' y') = Pair (x x') (y y')

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

instance (Eq a) => EqProp (Pair a) where
  (=-=) = eq





data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap :: (b -> c) -> Two a b -> Two a c
  fmap f (Two x y) = Two x (f y)

instance Monoid a => Applicative (Two a) where
  pure :: b -> Two a b
  pure x = Two mempty x

  (<*>) :: Two a (x -> y) -> Two a x -> Two a y
  (<*>) (Two a f) (Two b v) = Two (a <> b) (f v) 

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq


data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (<*>) (Three a b f) (Three c d v) = Three (a <> c) (b <> d) (f v) 

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z







data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Monoid a) => Applicative (Three' a) where
  pure x = Three' mempty x x
  (<*>) (Three' x f1 f2) (Three' y v1 v2) = Three' (x <> y) (f1 v1) (f2 v2)

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three' x y z





data Four a b c d = Four a b c d
-- skipping this one because it's too similar to Two and Three




data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' w x y z) = Four' w x y (f z)

instance Monoid a => Applicative (Four' a) where
  pure x = Four' mempty mempty mempty x 
  (<*>) (Four' x y z f) (Four' x' y' z' v) = Four' (x <> x') (y <> y') (z <> z') (f v)

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Four' w x y z



stops :: String
stops = "pbtdkg" 

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos x y z = (,,) <$> x <*> y <*> z -- liftA3 (,,) x y z

main :: IO ()
main = do
  quickBatch $ functor (undefined :: Pair (Int, Int, Int))
  quickBatch $ applicative (undefined :: Pair (Int, Int, Int))
  quickBatch $ functor (undefined :: Two String (Int, Int, Int))
  quickBatch $ applicative (undefined :: Two String (Int, Int, Int))
  quickBatch $ functor (undefined :: Three String String (Int, Int, Int))
  quickBatch $ applicative (undefined :: Three String String (Int, Int, Int))
  quickBatch $ functor (undefined :: Three' String (Int, Int, Int))
  quickBatch $ applicative (undefined :: Three' String (Int, Int, Int))
  quickBatch $ functor (undefined :: Four' String (Int, Int, Int))
  quickBatch $ applicative (undefined :: Four' String (Int, Int, Int))
