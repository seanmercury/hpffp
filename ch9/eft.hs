eftBool :: Bool -> Bool -> [Bool]
eftBool start end = dothis start end start [start]
  where dothis start end curr result
          | curr >= end || curr == maxBound = result
          | otherwise = dothis (succ curr) end (succ curr) (result ++ ([(succ curr)]))

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd start end
  | start < end = start : eftOrd (succ start) end 
  | start == end = [start]
  | otherwise = []

eftInt :: Int -> Int -> [Int]
eftInt start end
  | start < end = start : eftInt (succ start) end
  | start == end = [start]
  | start > end = []

eftChar :: Char -> Char -> [Char]
eftChar start end
  | start < end = start : eftChar (succ start) end
  | start == end = [start]
  | start > end = []

-- sol2
-- since all methods are to be implemented with the same logic, extract out common code

eftType :: (Ord a, Eq a, Enum a) => a -> a -> [a]
eftType start end
  | start < end = start : eftType (succ start) end
  | start == end = [start]
  | start > end = []