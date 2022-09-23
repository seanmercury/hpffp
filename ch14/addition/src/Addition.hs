module Addition where

import Test.Hspec
import Test.QuickCheck

-- sayHello :: IO ()
-- sayHello = putStrLn "hello!"

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where
    go n d count
        | n < d = (count, n)
        | otherwise = go (n - d) d (count + 1)

mul :: (Integral a) => a -> a -> a
mul a b
    | a == 0 || b == 0 = 0
    | otherwise = recursive a b 1
  where
    recursive sum b count
        | count < b = recursive (sum + a) b (count + 1)
        | otherwise = sum

main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "1 + 1 is greater than 1" $ do
            (1 + 1) > 1 `shouldBe` True

        it "2 + 2 is equal to 4" $ do
            2 + 2 `shouldBe` 4

    describe "Division" $ do
        it "15 divided by 3 is 5" $ do
            (15 `dividedBy` 3) `shouldBe` (5, 0)

        it "22 divided by 5 is 4 remainder 2" $ do
            (22 `dividedBy` 5) `shouldBe` (4, 2)

    describe "Multiplication" $ do
        it "3 times 5 is 15" $ do
            (3 `mul` 5) `shouldBe` 15

        it "3 times 0 is 0" $ do
            (3 `mul` 0) `shouldBe` 0

    describe "QuickCheck" $ do
        it "x + 1 is always greater than x" $ do
          property $ \x -> x + 1 > (x :: Int)

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]

oneThroughThree' :: Gen Int
oneThroughThree' = elements [1, 2, 2, 2, 2, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a,b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Just a, Nothing]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (4, return $ Just a),
              (1, return Nothing)]

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x+1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

-- runQc' :: IO ()
-- runQc' = property prop_additionGreater