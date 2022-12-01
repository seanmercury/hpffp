{-# LANGUAGE InstanceSigs #-}

import Data.Char (toUpper)
import Control.Applicative (liftA2)

cap :: [Char] -> [Char]
cap xs = map toUpper xs 

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char] 
composed = cap . rev

fmapped :: [Char] -> [Char] 
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
  c <- cap
  r <- rev
  return (c, r)

tupled'' :: [Char] -> ([Char], [Char])
tupled'' = cap >>= \c -> rev >>= \r -> return (c, r)

twiceWhenEven :: [Integer] -> [Integer] 
twiceWhenEven xs = do
  x <- xs 
  if even x then [x*x, x*x] else []

newtype Reader r a =
  Reader { runReader :: r -> a }

instance Functor (Reader r) where 
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ \r -> f (ra r)

ask :: Reader a a
ask = Reader id

newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person {
  humanName :: HumanName, 
  dogName :: DogName, 
  address :: Address
  } deriving (Eq, Show)

data Dog = Dog {
  dogsName :: DogName, 
  dogsAddress :: Address 
  } deriving (Eq, Show)

pers :: Person
pers = Person (HumanName "Big Bird") 
              (DogName "Barkley") 
              (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen") 
                (DogName "Papu")
                (Address "Austin")

getDog :: Person -> Dog 
getDog p = Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

getDogR' :: Person -> Dog
getDogR' = liftA2 Dog dogName address

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c 
myLiftA2 f fa fb = f <$> fa <*> fb


asks :: (r -> a) -> Reader r a 
asks f = Reader f

instance Applicative (Reader r) where 
  pure :: a -> Reader r a
  pure a = Reader $ const a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r $ ra r

instance Monad (Reader r) where 
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b 
  (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb (ra r)) r