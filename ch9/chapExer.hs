import Data.Char

extractUpper :: String -> String
extractUpper i = filter (\x -> isUpper x) i

extractUpper2 :: String -> String
extractUpper2 = filter isUpper

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

capAll :: String -> String
capAll [] = []
capAll (x:xs) = toUpper x : capAll xs

capWHead :: String -> Char
capWHead [] = ' '  -- TODO 
capWHead i = toUpper . head $ i

capWHead2 :: String -> Char
-- capWHead2 [] = '\0'
capWHead2 = toUpper . head
