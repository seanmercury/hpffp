import Data.Char

capitalizeWord :: String -> String
capitalizeWord (w:ws) = toUpper w : ws

capitalizeParagraph :: String -> String
capitalizeParagraph x = 
  -- this let .. in capitalize the first word and unword the result.
  let resultFromCapitalizethis = (capitalizethis $ words x) in
    unwords $ (capitalizeWord $ head resultFromCapitalizethis) : (tail resultFromCapitalizethis)
  where 
    capitalizethis :: [String] -> [String]
    capitalizethis [] = []
    capitalizethis (a:as:as') = case last a == '.' of
                                  True -> a : (capitalizeWord as) : capitalizethis as'
                                  False -> a : capitalizethis (as : as')
    capitalizethis (a:as) = a : capitalizethis as

-- capitalizethis :: [String] -> [String]
-- capitalizethis [] = []
-- capitalizethis (a:as:as') = case last a == '.' of
--                               True -> a : (capitalizeWord as) : capitalizethis as'
--                               False -> a : capitalizethis (as : as')
-- capitalizethis (a:as) = a : capitalizethis as