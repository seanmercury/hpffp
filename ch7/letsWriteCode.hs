module LetsWriteCode where

-- 1

tensDigit :: Integral a => a -> a 
tensDigit x = d
  where xLast = x `div` 10
        d = xLast `mod` 10


tensDigit2 :: Integral a => a -> a 
tensDigit2 x = d
  where xLast = fst $ x `divMod` 10
        d = snd $ xLast `divMod` 10

tensDigit3 :: Integral a => a -> a 
tensDigit3 x = d
  where (val1, _) = x `divMod` 10
        (_, d) = val1 `divMod` 10

hunsD :: Integral a => a -> a
hunsD x = d
  where (val1, _) = x `divMod` 100
        (_, d) = val1 `divMod` 10


-- 2

foldBool3 :: a -> a -> Bool -> a 
foldBool3 x _ False = x 
foldBool3 _ y True = y


foldBool1 :: a -> a -> Bool -> a 
foldBool1 a b c = 
  case c of
    True -> b
    False -> a

foldBool2 :: a -> a -> Bool -> a 
foldBool2 a b c 
  | c == True = b
  | c == False = a

foldBool3 :: a -> a -> Bool -> a 
foldBool3 a b c 
  | c = b
  | not c = a


-- 3
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c) 
  
