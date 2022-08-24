import Data.Maybe
import Data.List

notThe :: String -> Maybe String
notThe a = if a == "the" then Nothing else Just a

replaceThe :: String -> String
replaceThe a = concat $ intersperse " " ( map (dothis . notThe) (words a) )
  where dothis a
          | a == Nothing = "a"
          | otherwise = fromJust a

vowel = ['a', 'e', 'i', 'o', 'u']

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel a = dothis $ words a
  where 
    dothis (_:[]) = 0
    dothis (a:a1:as) = case notThe a of
      Just a -> dothis (a1:as)
      otherwise -> if (elem (a1 !! 0) vowel) then 1 + dothis (a1:as) else dothis (a1:as)
    

countVowels :: String -> Integer
countVowels a = fromIntegral . length $ filter (\x -> elem x vowel) a

