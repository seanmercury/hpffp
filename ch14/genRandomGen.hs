import Test.QuickCheck

data Fool =
  Fulse
  | Frue
  deriving (Eq, Show)

data Fool' =
  Fulse'
  | Frue'
  deriving (Eq, Show)

genFool :: Gen Fool
genFool = oneof [return Fulse, return Frue]

instance Arbitrary Fool where
  arbitrary = genFool

genFool' :: Gen Fool'
genFool' = frequency [(1, return Frue'), (2, return Fulse')]

instance Arbitrary Fool' where
  arbitrary = genFool'