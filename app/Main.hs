module Main where

import Lib
import Data.List
import System.Environment
import System.IO

main :: IO ()
main = do
    -- Get the input file and pass it to the parsing function
    [file] <- getArgs
    parseTree <- parseTokens file

    -- If Left, error. If Right, success
    case parseTree of
        Left e -> hPrint stderr e
        Right t -> putStrLn $ intercalate "\n" $ map show t