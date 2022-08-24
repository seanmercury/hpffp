import System.Exit (exitSuccess) 
import Control.Monad
import Data.Char

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (line1 == reverse line1) of
    True -> do
      putStrLn "It's a palindrome!"
      exitSuccess
    False -> putStrLn "Nope!"

palindrome2 :: IO ()
palindrome2 = forever $ do
  line1 <- getLine
  case (dothis line1 == reverse (dothis line1)) of
    True -> do
      putStrLn "It's a palindrome!"
      exitSuccess
    False -> putStrLn "Nope!"
    where 
      dothis = map toLower . filter isAlphaNum


type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
  NameEmpty
  | AgeEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
                  "Name was: " ++ show name ++
                  " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStr "Name : "
  name <- getLine
  putStr "Age : "
  age <- getLine
  case mkPerson name (read age) of
    Left a -> putStrLn $ show a
    Right b -> putStrLn $ "Yay! Successfully got a person:" ++ show b