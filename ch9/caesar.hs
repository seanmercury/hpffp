module Cipher where

import Data.Char

startLow = ord 'a'
startUpper = ord 'A'
numberOfAlpha = 26

shift :: Char -> Int -> Char
shift c i 
  | c >= 'a' && c <= 'z' = chr $ (+) startLow (((ord c - startLow) + i) `mod` numberOfAlpha)
  | otherwise = chr $ (+) startUpper (((ord c - startUpper) + i) `mod` numberOfAlpha)

caesar :: String -> Int -> String
caesar [] _ = []
caesar (x:xs) i = shift x i : caesar xs i

unshift :: Char -> Int -> Char
unshift c i 
  | c >= 'a' && c <= 'z' = chr $ (+) startLow (((ord c - startLow) - i) `mod` numberOfAlpha)
  | otherwise = chr $ (+) startUpper (((ord c - startUpper) - i) `mod` numberOfAlpha)

unCaesar :: String -> Int -> String
unCaesar [] _ = []
unCaesar (x:xs) i = unshift x i : unCaesar xs i


-- just using shift function
unCaesar2 :: String -> Int -> String
unCaesar2 [] _ = []
unCaesar2 (x:xs) i = shift x (-i) : unCaesar2 xs (i)


