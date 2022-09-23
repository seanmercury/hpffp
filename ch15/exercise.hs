import Test.QuickCheck hiding (Success, Failure)
import Data.Semigroup 

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool



newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)

genIdentity :: Arbitrary a => Gen (Identity a) 
genIdentity = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = genIdentity

type IdentityAssoc a =
  (Identity a) -> (Identity a)-> (Identity a) -> Bool



data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two c d) = Two (a <> c) (b <> d)

genTwo :: (Arbitrary a, Arbitrary b) => Gen (Two a b) 
genTwo = do
  a <- arbitrary
  b <- arbitrary
  return (Two a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = genTwo


type TwoAssoc a b =
  (Two a b) -> (Two a b)-> (Two a b) -> Bool




data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a b c) <> (Three d e f) = Three (a <> d) (b <> e) (c <> f)

genThree :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c) 
genThree = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (Three a b c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = genThree


type ThreeAssoc a b c =
  (Three a b c) -> (Three a b c)-> (Three a b c) -> Bool




data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four a b c d) <> (Four e f g h) = Four (a <> e) (b <> f) (c <> g) (d <> h)

genFour :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d) 
genFour = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return (Four a b c d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = genFour


type FourAssoc a b c d =
  (Four a b c d) -> (Four a b c d)-> (Four a b c d) -> Bool


newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj a) <> (BoolConj b) = BoolConj (a && b) 
  
genBool :: (Arbitrary BoolConj) => Gen BoolConj
genBool = do
  a <- arbitrary
  return (BoolConj a)

instance Arbitrary BoolConj where
  arbitrary = genBool

type BoolConjAssoc a =
  (BoolConj) -> (BoolConj)-> (BoolConj) -> Bool




newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)


instance Semigroup BoolDisj where
  (BoolDisj a) <> (BoolDisj b) = BoolDisj (a || b) 

genBool' :: (Arbitrary BoolDisj) => Gen BoolDisj
genBool' = do
  a <- arbitrary
  return (BoolDisj a)

instance Arbitrary BoolDisj where
  arbitrary = genBool'

type BoolDisjAssoc a =
  (BoolDisj) -> (BoolDisj)-> (BoolDisj) -> Bool




data Or a b = 
  Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  Fst a <> Snd b = Snd b
  Fst a <> Fst b = Fst b
  Snd a <> Fst b = Snd a
  Snd a <> Snd b = Snd a

genOr :: (Arbitrary a, Arbitrary b) => Gen (Or a b)
genOr = do
  a <- arbitrary
  b <- arbitrary
  frequency [(2, return (Fst a)), (2, return (Snd b))]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = genOr

type OrAssoc a b =
  (Or a b) -> (Or a b)-> (Or a b) -> Bool




newtype Combine a b = Combine { unCombine :: (a -> b) } 

instance Show (Combine a b) where 
  show (Combine _) = "Combine"

instance (Semigroup b) => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (\x -> f x <> g x)


genCombine ::  (CoArbitrary a, Arbitrary b) => Gen (Combine a b)
genCombine = do
  f <- arbitrary  -- f :: (a -> b)
  return (Combine f)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = genCombine
    
type CombineAssoc a b =
  (Combine a b) -> (Combine a b)-> (Combine a b) -> a -> Bool

semigroupCombAssoc :: (Eq b, Semigroup b) => Combine a b -> Combine a b -> Combine a b -> a -> Bool
semigroupCombAssoc f g h a = unCombine (f <> (g <> h)) a == unCombine ((f <> g) <> h) a




newtype Comp a = Comp { unComp :: (a -> a) }

instance Show (Comp a) where 
  show (Comp _) = "Comp"

instance Semigroup a => Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (\x -> f x <> g x)

genComp ::  (CoArbitrary a, Arbitrary a) => Gen (Comp a)
genComp = do
  f <- arbitrary  -- f :: (a -> a)
  return (Comp f)

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = genComp
    
type CompAssoc a =
  (Comp a) -> (Comp a)-> (Comp a) -> a -> Bool

semigroupCompAssoc :: (Eq a, Semigroup a) => Comp a -> Comp a -> Comp a -> a -> Bool
semigroupCompAssoc f g h a = unComp (f <> (g <> h)) a == unComp ((f <> g) <> h) a




data Validation a b = Failure a | Success b deriving (Eq, Show)
instance Semigroup a => Semigroup (Validation a b) where
  (<>) (Success a) _ = Success a
  (<>) (Failure a) (Failure b) = Failure (a <> b)
  (<>) (Failure a) (Success b) = Success b

genValid :: (Arbitrary a, Arbitrary b) => Gen (Validation a b)
genValid = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ Failure a, return $ Success b]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = genValid

type ValidAssoc a b =
  (Validation a b) -> (Validation a b)-> (Validation a b) -> Bool



-- Monoid exercise

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a


instance Monoid Trivial where 
  mempty = Trivial
  mappend = (<>)

type TrivialMonoidAssoc = Trivial -> Trivial -> Trivial -> Bool
type TrivialMonoidLeftIdentity = Trivial -> Bool
type TrivialMonoidRightIdentity = Trivial -> Bool




instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

type IdentityMonoidAssoc a = Identity a -> Identity a -> Identity a -> Bool
type IdentityMonoidLeftIdentity a = Identity a -> Bool
type IdentityMonoidRightIdentity a = Identity a -> Bool





instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)


type TwoMonoidAssoc a b =
  (Two a b) -> (Two a b)-> (Two a b) -> Bool
type TwoMonoidLeftIdentity a b = Two a b -> Bool
type TwoMonoidRightIdentity a b = Two a b -> Bool



instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)
  
type BoolConjMonoidAssoc =
  (BoolConj) -> (BoolConj)-> (BoolConj) -> Bool
type BoolConjMonoidLeftIdentity = BoolConj -> Bool
type BoolConjMonoidRightIdentity = BoolConj -> Bool




instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)
  
type BoolDisjMonoidAssoc =
  (BoolDisj) -> (BoolDisj)-> (BoolDisj) -> Bool
type BoolDisjMonoidLeftIdentity = BoolDisj -> Bool
type BoolDisjMonoidRightIdentity = BoolDisj -> Bool




instance Monoid b => Monoid (Combine a b) where
  mempty = Combine mempty
  mappend = (<>)
  
type CombMonoidAssoc a b =
  (Combine a b) -> (Combine a b) -> (Combine a b) -> a -> Bool

monoidCombAssoc :: (Eq b, Monoid b) => Combine a b -> Combine a b -> Combine a b -> a -> Bool
monoidCombAssoc f g h a = unCombine (f <> (g <> h)) a == unCombine ((f <> g) <> h) a

type CombMonoidLeftIdentity a b = Combine a b -> a -> Bool
monoidCombLeftIdentity :: (Eq b, Monoid b) => Combine a b -> a -> Bool
monoidCombLeftIdentity f a = unCombine (mempty <> f) a == unCombine f a

type CombMonoidRightIdentity a b = Combine a b -> a -> Bool
monoidCombRightIdentity :: (Eq b, Monoid b) => Combine a b -> a -> Bool
monoidCombRightIdentity f a = unCombine (f <> mempty) a == unCombine f a




instance Monoid a => Monoid (Comp a) where
  mempty = Comp mempty
  mappend = (<>)
type CompMonoidAssoc a =
  (Comp a) -> (Comp a)-> (Comp a) -> a -> Bool

monoidCompAssoc :: (Eq a, Monoid a) => Comp a -> Comp a -> Comp a -> a -> Bool
monoidCompAssoc f g h a = unComp (f <> (g <> h)) a == unComp ((f <> g) <> h) a

type CompMonoidLeftIdentity a = Comp a -> a -> Bool
monoidCompLeftIdentity :: (Eq a, Monoid a) => Comp a -> a -> Bool
monoidCompLeftIdentity f a = unComp (mempty <> f) a == unComp f a

type CompMonoidRightIdentity a = Comp a -> a -> Bool
monoidCompRightIdentity :: (Eq a, Monoid a) => Comp a -> a -> Bool
monoidCompRightIdentity f a = unComp (f <> mempty) a == unComp f a


main :: IO () 
main = do
  putStrLn "Semigroup"
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc String)
  quickCheck (semigroupAssoc :: TwoAssoc String String)
  quickCheck (semigroupAssoc :: ThreeAssoc String String String)
  quickCheck (semigroupAssoc :: FourAssoc String String String String)
  quickCheck (semigroupAssoc :: BoolConjAssoc Bool)
  quickCheck (semigroupAssoc :: BoolDisjAssoc Bool)
  quickCheck (semigroupAssoc :: OrAssoc String String)
  quickCheck (semigroupCombAssoc :: CombineAssoc Int (Sum Int))
  quickCheck (semigroupCompAssoc :: CompAssoc (Sum Int))
  quickCheck (semigroupAssoc :: ValidAssoc String String)

  -- Monoid
  putStrLn "Monoid"

  quickCheck (monoidAssoc :: TrivialMonoidAssoc)
  quickCheck (monoidLeftIdentity :: TrivialMonoidLeftIdentity)
  quickCheck (monoidRightIdentity :: TrivialMonoidRightIdentity)
  
  quickCheck (monoidAssoc :: IdentityMonoidAssoc String)
  quickCheck (monoidLeftIdentity :: IdentityMonoidLeftIdentity String)
  quickCheck (monoidRightIdentity :: IdentityMonoidRightIdentity String)

  
  quickCheck (monoidAssoc :: TwoMonoidAssoc String String)
  quickCheck (monoidLeftIdentity :: TwoMonoidLeftIdentity String String)
  quickCheck (monoidRightIdentity :: TwoMonoidRightIdentity String String)
  
  quickCheck (monoidAssoc :: BoolConjMonoidAssoc)
  quickCheck (monoidLeftIdentity :: BoolConjMonoidLeftIdentity)
  quickCheck (monoidRightIdentity :: BoolConjMonoidRightIdentity)
  
  quickCheck (monoidAssoc :: BoolDisjMonoidAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisjMonoidLeftIdentity)
  quickCheck (monoidRightIdentity :: BoolDisjMonoidRightIdentity)

  quickCheck (monoidCombAssoc :: CombMonoidAssoc Int (Sum Int))
  quickCheck (monoidCombLeftIdentity :: CombMonoidLeftIdentity Int (Sum Int))
  quickCheck (monoidCombRightIdentity :: CombMonoidRightIdentity Int (Sum Int))

  quickCheck (monoidCompAssoc :: CompMonoidAssoc (Sum Int))
  quickCheck (monoidCompLeftIdentity :: CompMonoidLeftIdentity (Sum Int))
  quickCheck (monoidCompRightIdentity :: CompMonoidRightIdentity (Sum Int))


