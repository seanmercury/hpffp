-- direct recursion, not using &&
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) =
  if x == False
  then False
  else myAnd xs

-- direct recursion, using &&
myAnd2 :: [Bool] -> Bool
myAnd2 [] = True
myAnd2 (x:xs) = x && myAnd2 xs

-- fold, not point-free
myAnd3 :: [Bool] -> Bool
myAnd3 = foldr
        (\a b ->
          if a == False
          then False
          else b) True

-- fold, both myAnd and the folding
-- function are point-free now
myAnd4 :: [Bool] -> Bool
myAnd4 = foldr (&&) True


-- direct recursion, not using &&
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = case x of
                True -> True
                False -> myOr xs

-- direct recursion, using ||
myOr2 :: [Bool] -> Bool
myOr2 [] = False
myOr2 (x:xs) = x || myOr2 xs 

-- fold, not point-free
myOr3 :: [Bool] -> Bool
myOr3 = foldr (\x y -> x || y) False

-- fold, both myAnd and the folding
-- function are point-free now
myOr4 :: [Bool] -> Bool
myOr4 = foldr (||) False




-- direct recursion, not using ||
myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs) = case f x of
                   True -> True
                   False -> myAny f xs

-- direct recursion, using ||
myAny2 :: (a -> Bool) -> [a] -> Bool
myAny2 f [] = False
myAny2 f (x:xs) = f x || myAny f xs


-- fold, not point-free
myAny3 :: (a -> Bool) -> [a] -> Bool
myAny3 f = foldr (\x y -> f x || y) False 

-- fold, both myAnd and the folding
-- function are point-free now
myAny4 :: (a -> Bool) -> [a] -> Bool
myAny4 f a = foldr (||) False (map f a)


-- direct recursion, not using ||
myElem :: Eq a => a -> [a] -> Bool
myElem a [] = False
myElem a (b:bs) = if a == b then True else myElem a bs

-- direct recursion, using ||
myElem2 :: Eq a => a -> [a] -> Bool
myElem2 a [] = False
myElem2 a (b:bs) = a == b || myElem2 a bs

-- fold, not point-free
myElem3 :: Eq a => a -> [a] -> Bool
myElem3 a = foldr (\x y -> x == a) False

-- fold, both myAnd and the folding
-- function are point-free now
myElem4 :: Eq a => a -> [a] -> Bool
myElem4 a b = foldr (||) False (map (a==) b)




myReverse :: [a] -> [a]
myReverse [] = []
myReverse (a:as) = myReverse as ++ [a]

myReverse2 :: [a] -> [a]
myReverse2 = foldr (\x y -> y ++ [x]) []

myReverse3 :: [a] -> [a]
myReverse3 = foldl (flip (:)) [] 

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x -> if f x then (x:) else id) [] 

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap (\x -> x)
