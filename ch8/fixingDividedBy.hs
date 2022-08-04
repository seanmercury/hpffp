module FixingDividedBy where

data DividedResult = Result Integer
                    | DividedByZero deriving Show

dividedBy :: Integral a => a -> a -> DividedResult
dividedBy num denom 
  | denom == 0 = DividedByZero    -- denominator 0
  | num < 0 && denom < 0 = go (-num) (-denom) 0 False
  | num < 0 = go (-num) denom 0 True
  | denom < 0 = go num (-denom) 0 True
  | otherwise = go num denom 0 False
    where go n d count changeSign   -- last param of go is changeSign for the result.
            | n < d = if not changeSign then Result count else Result (-count)
            | otherwise = go (n - d) d (count + 1) changeSign

