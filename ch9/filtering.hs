-- 2. 
-- length . filter (\x -> x `mod` 3 == 0) $ [1..30]

-- 3.
myFilter :: String -> [String]
myFilter a = filter (\x -> x /= "a" && x /= "an" && x /= "the") (parse a)
  where parse a
          | length a > 0 = takeWhile(/=' ') a : parse (dropWhile(==' ') (dropWhile(/=' ') a))
          | otherwise = []

myFilter2 :: String -> [String]
myFilter2 a = filter (\x -> x /= "a" && x /= "an" && x /= "the") $ words a

myFilter3 :: String -> [String]
myFilter3 a = filter (\x -> not $ elem x ["a", "an", "the"]) $ words a
