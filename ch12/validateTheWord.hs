newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord a = let result = count a in
  if (snd result) > (fst result) then Just (Word' a) else Nothing
  where
    count :: String -> (Int, Int)
    count [] = (0,0)
    count (x:xs) = if elem x vowels then (1 + (fst (count xs)), snd (count xs)) else (fst (count xs), 1 + (snd (count xs)))

-- count :: String -> (Int, Int)
-- count [] = (0,0)
-- count (x:xs) = if elem x vowels then (1 + (fst (count xs)), snd (count xs)) else (fst (count xs), 1 + (snd (count xs)))