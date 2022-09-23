module MaybeMonoid where

import Test.QuickCheck
import Optional

newtype First' a =
  First' { getFirst' :: Optional a } 
  deriving (Eq, Show)

instance Semigroup (First' a) where
  (<>) (First' Nada) (First' (Only b)) = First' (Only b)
  (<>) (First' (Only b)) (First' Nada) = First' (Only b)
  (<>) (First' (Only a)) (First' (Only b)) = (First' (Only a))
  (<>) (First' Nada) (First' Nada) = First' Nada

instance Monoid (First' a) where
  mempty = (First' Nada)

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId =
  First' String -> Bool

genOptional :: Arbitrary a => Gen (Optional a)
genOptional = do
  a <- arbitrary
  frequency [ (1, return Nada) , (19, return (Only a)) ]

genFirst' :: Arbitrary a => Gen (First' a)
genFirst' = do
  a <- arbitrary
  return (First' a)

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = genOptional

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = genFirst'

monoidAssoc :: FirstMappend
monoidAssoc a b c = do 
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: FstId
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: FstId
monoidRightIdentity a = (a <> mempty) == a

main :: IO () 
main = do
  quickCheck (monoidAssoc :: FirstMappend) 
  quickCheck (monoidLeftIdentity :: FstId) 
  quickCheck (monoidRightIdentity :: FstId)