module Main where

import Lib
import Data.List
import System.Environment
import System.IO

main :: IO ()
main = do
    -- Get the input file and pass it to the parsing function
    [file] <- getArgs
    parseTree <- parseProgram file

    -- If Left, error. If Right, success
    case parseTree of
        Left _ -> putStrLn "ERROR"
        Right prog -> putStr $ runCompiler prog