-- 1 a. 
stops = "pbtdkg"
vowels = "aeiou" 

combinationa :: String -> String -> [(Char, Char, Char)]
combinationa a b = [(a', b', c') | a' <- a, b' <- b, c' <- a]

-- 1 b.
combinationb :: String -> String -> [(Char, Char, Char)]
combinationb a b = [(a', b', c') | a' <- a, b' <- b, c' <- a, a' == 'p']


nouns = ["dog", "cup", "computer", "camera", "book", "cake"]
verbs = ["drops", "barks", "eat", "drive", "go"]

-- 1 c.
combinationc :: [String] -> [String] -> [String]
combinationc a b = [a' ++ " " ++ b' ++ " " ++ c' | a' <- a, b' <- b, c' <- a]

-- 2.
seekritFunc :: String -> Int
seekritFunc x =
  div (sum (map length (words x))) (length (words x))

-- 3.
seekritFunc2 :: String -> Double
seekritFunc2 x =
  (/) (fromIntegral (sum (map length (words x) ) ) ) (fromIntegral (length (words x)))