-- Numbers into words
module WordNumber where
import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n 
  | n == 0 = "zero"
  | n == 1 = "one"
  | n == 2 = "two"
  | n == 3 = "three"
  | n == 4 = "four"
  | n == 5 = "five"
  | n == 6 = "six"
  | n == 7 = "seven"
  | n == 8 = "eight"
  | n == 9 = "nine"
  | otherwise = error "invalid  "

nModTen :: Int -> Int
nModTen n = n `mod` 10

nDivTen :: Int -> Int
nDivTen n = n `div` 10

digits :: Int -> [Int]
digits n = parse n []
  where parse n result 
          | n > 10 = parse (nDivTen n) $ (nModTen n):result
          | otherwise = n:result

wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" $ map digitToWord (digits n)