module FixingDividedBy where

data DividedResult = Result Integer
                    | DividedByZero deriving Show

dividedBy :: Integral a => a -> a -> DividedResult
dividedBy num denom 
  | denom == 0 = DividedByZero    -- denominator 0
  | num < 0 && denom < 0 = Result (go (-num) (-denom) 0) 
  | num < 0 = Result (-1 * go (abs num) denom 0)
  | denom < 0 = Result (-1 * go num (-denom) 0 )
  | otherwise = Result (go num denom 0 )
    where go n d count    -- last param of go is changeSign for the result.
            | n < d = count
            | otherwise = go (n - d) d (count + 1) 

-- Daniel's sol - TODO Review
dividedBy2 :: Integral a => a -> a -> DividedResult
dividedBy2 num denom
  | denom == 0 = DivideByZero
  | num >= 0 && denom > 0 = Result (go num denom 0)
  | otherwise = Result (signum num * signum denom * go (abs num) (abs denom) 0)
  where
  go n d count
    | n < d = count
    | otherwise = go (n - d) d (count + 1)

-- Daniel's pattern matching sol - TODO review
dividedBy3 :: Integral a => a -> a -> DividedResult
dividedBy3 num denom =
  case (denom == 0, num >= 0 && denom > 0) of
    (True, _) -> DivideByZero
    (_, True) -> Result (go num denom 0)
    _ -> Result (signum num * signum denom * go (abs num) (abs denom) 0)
  where
  go n d count
    | n < d = count
    | otherwise = go (n - d) d (count + 1)

-- Daniel's 
dividedBy4 :: Integral a => a -> a -> DividedResult
dividedBy4 num denom =
  case denom of
    0 -> DivideByZero
    d | num >= 0 && d > 0 -> Result (go num denom 0)
      | otherwise -> Result (signum num * signum denom * go (abs num) (abs denom) 0)
  where
  go n d count
    | n < d = count
    | otherwise = go (n - d) d (count + 1)