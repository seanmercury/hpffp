fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x

-- 1.
fibs1 = take 20 fibs

-- 2.
fibs2 = takeWhile (<100) fibs

-- 3. 
factorial :: Int -> [Int]
factorial n = take n getFact
  where getFact = scanl (*) 1 [2..]