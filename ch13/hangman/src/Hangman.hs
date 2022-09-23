module Hangman where

import Control.Monad (forever) -- [1]
-- [3]

import Data.List (intersperse) -- [4]
import Data.Maybe (isJust)
import System.Exit (exitSuccess) -- [5]
import System.Random (randomRIO) -- [7]

type WordList = [String]

allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = filter gameLength <$> allWords
  where
    gameLength w =
        let l = length (w :: String)
         in l >= minWordLength
                && l < maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
    randomIndex <- randomRIO (0, length wl)
    -- fill this part in ^^^
    return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle
    = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
    show (Puzzle word discovered guessed) =
        intersperse
            ' '
            (fmap renderPuzzleChar discovered)
            ++ " Guessed so far: "
            ++ guessed
            ++ " Just use this in develop: "
            ++ word

freshPuzzle :: String -> Puzzle
freshPuzzle a = Puzzle a (map (const Nothing) a) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _) a = a `elem` s

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g) a = a `elem` g

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just a) = a
renderPuzzleChar Nothing = '_'

fillInCharacter :: Puzzle -> Char -> Puzzle
-- fillInCharacter (Puzzle word filledInSoFar s) c = Puzzle word newFilledInSoFar (c : s)
--   where
--     zipper guessed wordChar guessChar = if wordChar == guessed
--                                         then Just wordChar
--                                         else guessChar
--     newFilledInSoFar = zipWith (zipper c) word filledInSoFar

-- 13.14 Chap Exercise
fillInCharacter (Puzzle word filledInSoFar s) c = Puzzle word newFilledInSoFar (if Just c `elem` newFilledInSoFar then s else c : s)
  where
    zipper guessed wordChar guessChar =
        if wordChar == guessed
            then Just wordChar
            else guessChar
    newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
        (_, True) -> do
            putStrLn "You already guessed that character, pick something else!"
            return puzzle
        (True, _) -> do
            putStrLn "This character was in the word, filling in the word accordingly"
            return (fillInCharacter puzzle guess)
        (False, _) -> do
            putStrLn "This character wasn't in the word, try again."
            return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
    Control.Monad.when (length guessed > 7)
        $ do putStrLn "You lose!"
          putStrLn
        $ "The word was: "
            ++ wordToGuess
                exitSuccess

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
    Control.Monad.when (all isJust filledInSoFar) $
        do putStrLn "You win!"
          exitSuccess

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $
        "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _ -> putStrLn "Your guess must be a single character"
