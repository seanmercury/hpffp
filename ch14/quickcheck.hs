module QuickCheckTest where

import Data.List (sort)
import Test.QuickCheck

half x = x / 2
halfIdentity = (*2) . half

halfIdentityProp :: Double -> Bool
halfIdentityProp x = halfIdentity x == x 

testHalfIdentity :: IO ()
testHalfIdentity = quickCheck halfIdentityProp

-- for any list you apply sort to,
-- this property should hold 
listOrdered :: (Ord a) => [a] -> Bool 
listOrdered xs = 
  snd $ foldr go (Nothing, True) xs 
  where 
    go _ status@(_, False) = status 
    go y (Nothing, t) = (Just y, t)
    go y (Just x, t) = (Just y, x >= y)

listOrderedProp :: [Int] -> Bool
listOrderedProp a = listOrdered $ sort a

testListOrdered :: IO ()
testListOrdered = quickCheck listOrderedProp 

plusAssociative x y z =
  x + (y + z) == (x + y) + z
plusCommutative x y = 
  x + y == y + x

type TestType = Integer
plusAssociativeProp :: TestType -> TestType -> TestType -> Bool
plusAssociativeProp x y z = x + (y + z) == (x + y) + z
 
plusCommutativeProp :: TestType -> TestType -> Bool
plusCommutativeProp x y = x + y == y + x

testPlusAssociative :: IO ()
testPlusAssociative = quickCheck plusAssociativeProp

testPlusCommutative :: IO ()
testPlusCommutative = quickCheck plusCommutativeProp


mulAssociative x y z =
  x * (y * z) == (x * y) * z
mulCommutative x y = 
  x * y == y * x

mulAssociativeProp :: TestType -> TestType -> TestType -> Bool
mulAssociativeProp x y z = x * (y * z) == (x * y) * z
 
mulCommutativeProp :: TestType -> TestType -> Bool
mulCommutativeProp x y = x * y == y * x

testMulAssociative :: IO ()
testMulAssociative = quickCheck mulAssociativeProp

testMulCommutative :: IO ()
testMulCommutative = quickCheck mulCommutativeProp



-- (quot x y) * y + (rem x y) == x 
-- (div x y) * y + (mod x y) == x

type TestType5 = Int

quotRem' x y = (quot x y) * y + (rem x y)
divMod' x y = (div x y) * y + (mod x y)

quotRemProp :: TestType5 -> (NonZero TestType5) -> Bool
quotRemProp a (NonZero b) = quotRem' a b == a

divModProp :: TestType5 -> (NonZero TestType5) -> Bool
divModProp a (NonZero b) = divMod' a b == a

divModProp' :: Gen Bool
divModProp' = do
  a <- arbitrary :: Gen Int
  b <- elements [1..(maxBound :: Int)]
  return (divMod' a b == a)

testQuotRem :: IO ()
testQuotRem = quickCheck quotRemProp

testDivMod :: IO ()
testDivMod = quickCheck divModProp



-- (x ^ y) ^ z == x ^ (y ^ z) ??
-- (x ^ y) == (y ^ x) ??

powerCommProp :: TestType5 -> TestType5 -> Bool
powerCommProp x y = x ^ y == y ^ x

powerAssocProp :: TestType5 -> TestType5 -> TestType5 -> Bool
powerAssocProp x y z= (x ^ y) ^ z == x ^ (y ^ z)

testPowerComm = quickCheck powerCommProp
testPowerAssoc = quickCheck powerAssocProp

-- reverse . reverse == id

reverseTwiceProp :: [TestType5] -> Bool
reverseTwiceProp x = (reverse . reverse $ x) == x

testReverseTwice = quickCheck reverseTwiceProp

-- f $ a = f a

dollarProp :: Gen Bool
dollarProp = do
  f <- arbitrary :: Gen (TestType5 -> TestType5)
  a <- arbitrary :: Gen TestType5
  return ((f $ a) == f a)

testDollar = quickCheck dollarProp


-- f . g = \x -> f (g x)

dotProp :: Gen Bool
dotProp = do
  f <- arbitrary :: Gen (TestType5 -> TestType5)
  g <- arbitrary :: Gen (TestType5 -> TestType5)
  x <- arbitrary :: Gen TestType5
  return ((f . g $ x) == f (g x) )

testDot = quickCheck dotProp


-- foldr (:) == (++)
semicolProp :: [TestType5] -> Bool
semicolProp x = foldr (:) [] x == dothis x
  where
    dothis [] = []
    dothis (x:[]) = [x]
    dothis (x:xs) = [x] ++ dothis xs

testSemicol = quickCheck semicolProp

-- foldr (++) [] == concat
doublePlusProp :: [[TestType5]] -> Bool
doublePlusProp x = foldr (++) [] x == concat x

testDoublePlus = quickCheck doublePlusProp


-- f n xs = length (take n xs) == n
takeAndLengthProp :: Int -> [TestType5] -> Bool
takeAndLengthProp a b = length (take a b) == a

testTakeAndLength = quickCheck takeAndLengthProp

-- f x = (read (show x)) == x
showAndReadProp :: TestType -> Bool
showAndReadProp x = (read (show x)) == x

testShowAndRead = quickCheck showAndReadProp

square x = x * x
squareIdentity = square . sqrt

squareIdentityProp :: Double -> Bool
squareIdentityProp x = squareIdentity x == x

testSquareIdentity = quickCheck squareIdentityProp