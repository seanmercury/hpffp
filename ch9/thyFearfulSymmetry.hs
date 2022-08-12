-- 1
myWords :: String -> [String]
myWords a 
  | length a > 0 = takeWhile(/=' ') a : myWords (dropWhile(==' ') (dropWhile(/=' ') a))
  | otherwise = []

-- 2
firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines a
  | length a > 0 = takeWhile(/='\n') a : myLines (dropWhile(=='\n') (dropWhile(/='\n') a))
  | otherwise = []

-- 3
myParameterized :: Char -> String -> [String]
myParameterized character string
  | length string > 0 = takeWhile(/= character) string : myParameterized character (dropWhile(== character) (dropWhile(/= character) string))
  | otherwise = []

myWords2 :: String -> [String]
myWords2 a = myParameterized ' ' a

myLines2 :: String -> [String]
myLines2 a = myParameterized '\n' a

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main = do
  print $ myWords "test this thing"
  print $ "Are myLines equal? " ++ show (myLines sentences == shouldEqual)
  print $ "Are myLines2 equal? " ++ show (myLines2 sentences == shouldEqual)

