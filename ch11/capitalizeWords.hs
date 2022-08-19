import Data.Char

capitalizeWords :: String -> [(String, String)]
capitalizeWords w = dothis $ words w
  where 
    dothis [] = []
    dothis (a:as) = (a, (toUpper $ head a) : tail a) : dothis as