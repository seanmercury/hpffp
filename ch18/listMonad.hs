twiceWhenEven :: [Int] -> [Int]
twiceWhenEven xs = do
  x <- xs
  if even x then [x*x, x*x] else [x*x]

twiceWhenEven' :: [Int] -> [Int]
twiceWhenEven' xs = do
  x <- xs
  if even x then [x*x, x*x] else []