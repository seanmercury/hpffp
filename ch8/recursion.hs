module Recursion where

{-
1. Write out the steps for reducing dividedBy 15 2 to its final answer 
according to the Haskell code.
-}

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

-- dividedBy 15 2
-- go 15 2 0 (n = 15, d = 2, count = 0)
-- go 13 2 1
-- go 11 2 2
-- go 9 2 3
-- go 7 2 4
-- go 5 2 5
-- go 3 2 6
-- go 1 2 7
-- (7, 1)

{- 
2. Write a function that recursively sums all numbers from 1 to n,
 n being the argument. So if n is 5, you’d add 1 + 2 + 3 + 4 + 5 to
 get 15. The type should be (Eq a, Num a) => a -> a.
-}

sum :: (Eq a, Num a) => a -> a
sum n = cal n 1 0
  where cal n curr sum
          | curr /= n = cal n (curr + 1) (sum + curr)
          | otherwise = sum + curr

{- 
3. Write a function that multiplies two integral numbers using
recursive summation. The type should be (Integral a) => a ->
a -> a.
-}

mul :: (Integral a) => a -> a -> a
mul a b 
  | a == 0 || b == 0 = 0
  | otherwise = recursive a b 1
      where recursive sum b count
              | count < b = recursive (sum + a) b (count + 1)
              | otherwise = sum
