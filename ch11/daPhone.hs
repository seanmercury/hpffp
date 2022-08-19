import Data.List
import Data.Maybe
import Data.Char

-- validButtons = "1234567890*#"
type Digit = Char

data Btn = Btn Digit [Char]

data DaPhone = DaPhone [Btn]

phone = DaPhone [Btn '1' ['1']
            , Btn '2' ['a', 'b', 'c', '2']
            , Btn '3' ['d', 'e', 'f', '3']
            , Btn '4' ['g', 'h', 'i', '4']
            , Btn '5' ['j', 'k', 'l', '5']
            , Btn '6' ['m', 'n', 'o', '6']
            , Btn '7' ['p', 'q', 'r', 's', '7']
            , Btn '8' ['t', 'u', 'v', '8']
            , Btn '9' ['w', 'x', 'y', 'z', '9']
            , Btn '0' ['+', ' ', '0']
            , Btn '*' ['*', '^']
            , Btn '#' ['#', '.', ',']
          ]

convo :: [String]
convo =
  ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol OK. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "OK. Do u think I am pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn"]

-- Valid presses: 1 and up
type Presses = Int

-- -- assuming the default phone definition
-- -- 'a' -> [('2', 1)]
-- -- 'A' -> [('*', 1), ('2', 1)]
reverseTaps :: DaPhone -> Char -> (Digit, Presses)
reverseTaps (DaPhone btns) c = 
  let process (x:xs) c = if contains x c then getAllComb x c else reverseTaps (DaPhone xs) c in 
    process btns c
  where 
    getAllComb (Btn dig chars) c' = (c', (fromMaybe (-1) $ elemIndex c' chars) + 1)
    contains (Btn dig chars) c = elem c chars

reverseTaps2 :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps2 (DaPhone btns) c = 
  let process (x:xs) c = if contains x c then [getAllComb x c] else reverseTaps2 (DaPhone xs) c in 
    if isUpper c then ('*', 1) : process btns (toLower c) else process btns c
  where 
    getAllComb (Btn dig chars) c' = (c', (fromMaybe (-1) $ elemIndex c' chars) + 1)
    contains (Btn dig chars) c = elem c chars



cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead dp [] = []
cellPhonesDead dp (c:cs) = reverseTaps2 dp c ++ cellPhonesDead dp cs

-- cellPhonesDead phone $ concat convo

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd

-- fingerTaps . cellPhonesDead phone $ concat convo

mostPopularLetter :: String -> Char
mostPopularLetter s = head (head (reverse (sortBy sortByFun (getGroup s))))
  where 
    getGroup = groupBy (\x y -> x == y) . sort 
    sortByFun = \x y -> compare (length x) (length y)

mostPopularLetterCost :: String -> Presses
mostPopularLetterCost s = fingerTaps (reverseTaps2 phone (mostPopularLetter s))


-- this should be the same as mostPopularLetter???
coolestLtr :: [String] -> Char
coolestLtr = undefined
  
coolestWord :: [String] -> String
coolestWord s = head (head (reverse (sortBy sortByFun (getGroup s))))
  where 
    getGroup = groupBy (\x y -> x == y) . sort 
    sortByFun = \x y -> compare (length x) (length y)