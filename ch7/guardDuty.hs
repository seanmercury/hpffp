avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade2 :: (Fractional a, Ord a) => a -> Char

avgGrade x 
  | y >= 0.9 ='A' 
  | y >= 0.8 ='B' 
  | y >= 0.7 ='C' 
  | y >= 0.59 = 'D' 
  | y < 0.59 = 'F' 
  where y = x / 100

avgGrade2 x 
  | otherwise ='F' 
  | y >= 0.9 ='A' 
  | y >= 0.8 ='B' 
  | y >= 0.7 ='C' 
  | y >= 0.59 = 'D' 
  | y < 0.59 = 'F' 
  where y = x / 100


-- ex1. pattern match redundant warning & 'F'

pal :: [Char] -> Bool
pal xs
  | xs == reverse xs = True 
  | otherwise = Fals

-- Ex 3. checking palindrome
-- Ex 4. [Char] 
-- Ex 5. Bool


numbers :: (Eq x, Ord x, Num x) => x -> x
numbers x
  | x < 0 = -1
  | x == 0 = 0 
  | x > 0 = 1




e
