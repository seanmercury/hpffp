module Main (main) where

import System.IO (BufferMode(NoBuffering),
                  hSetBuffering,
                  stdout) -- [6]
import Data.Char (toLower) -- [2]
import Hangman

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
