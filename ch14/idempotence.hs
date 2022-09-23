import Data.Char
import Data.List

capitalizeWord :: String -> String
capitalizeWord []     = []
capitalizeWord (c:cs) = toUpper c : cs

twice f = f . f
fourTimes = twice . twice

f x =
  (capitalizeWord x
  == twice capitalizeWord x)
  &&
  (capitalizeWord x
  == fourTimes capitalizeWord x)

f' x = 
  (sort x
  == twice sort x)
  &&
  (sort x
  == fourTimes sort x)

capitalizeWordIdempotentProp :: String -> Bool
capitalizeWordIdempotentProp x =
  (capitalizeWord x == twice capitalizeWord x) &&
  (capitalizeWord x == fourTimes capitalizeWord x)

sortIdempotentStringProp :: [String] -> Bool
sortIdempotentStringProp x =
  (sort x == twice sort x) && (sort x == fourTimes sort x)