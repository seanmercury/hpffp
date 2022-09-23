import Control.Applicative
import Test.Hspec
import Test.QuickCheck (arbitrary, Arbitrary, frequency)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid
import Data.Semigroup

data Validation e a = Failure e
                      | Success a deriving (Eq, Show)

-- same as Either
instance Functor (Validation e) where 
  fmap f (Failure x) = Failure x
  fmap f (Success y) = Success (f y)

-- This is different
instance Monoid e => Applicative (Validation e) where
  pure :: a -> Validation e a
  pure = Success 

  (<*>) :: Validation e (a -> b) -> Validation e a -> Validation e b
  (<*>) (Failure x) (Failure y) = Failure (x <> y)
  (<*>) (Failure x) (Success y) = Failure x
  (<*>) (Success f) (Failure y) = Failure y
  (<*>) (Success f) (Success y) = Success (f y)


instance (Eq a, Eq b) => EqProp (Validation a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = frequency [(1, Failure <$> arbitrary), (4, Success <$> arbitrary)]  


main :: IO ()
main = do
  quickBatch $ functor (undefined :: Validation String (Int, Int, Int))
  quickBatch $ applicative (undefined :: Validation String (Int, Int, Int))