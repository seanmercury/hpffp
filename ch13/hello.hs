module Main where
import System.IO

sayHello :: String -> IO ()
sayHello name =
  putStrLn ("Hi " ++ name ++ "!")

-- main :: IO ()
-- main = do
--   -- hSetBuffering stdout NoBuffering
--   putStr "Please input your name: "
--   name <- getLine
--   sayHello name

main :: IO ()
main = do 
        c <- getChar
        c' <- getChar
        if c == c'
        then putStrLn "True"
        else return ()