module Cipher2 where

import Data.Char
import Data.List

startLow = ord 'a'
startUpper = ord 'A'
numberOfAlpha = 26

shift :: Char -> Int -> Char
shift c i 
  | c >= 'a' && c <= 'z' = chr $ (+) startLow (((ord c - startLow) + i) `mod` numberOfAlpha)
  | otherwise = chr $ (+) startUpper (((ord c - startUpper) + i) `mod` numberOfAlpha)


unshift :: Char -> Int -> Char
unshift c i 
  | c >= 'a' && c <= 'z' = chr $ (+) startLow (((ord c - startLow) - i) `mod` numberOfAlpha)
  | otherwise = chr $ (+) startUpper (((ord c - startUpper) - i) `mod` numberOfAlpha)

genKey :: String -> Int -> String
genKey key keySize = 
  let result = take (keySize `div` (length key) + 1) $ (repeat key) in
    concat result

    -- "this thing has to be encoded  "
    -- "ALLY"
    -- "ALLYALLYALLYALLYALLYALLYALLYALLY"

genTuple :: String -> String ->[(Char, Char)]
genTuple [] key = []
genTuple x [] = []
genTuple (x:xs) (k:ks) = if x == ' ' then (x, ' ') : genTuple xs ks else (x, k) : genTuple xs ks


caesar :: String -> String -> String
caesar input key = map dothis $ genTuple input $ genKey key (length input)
  where dothis (a, b) = shift a (ord b - ord ' ')