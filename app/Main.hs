module Main where

import Lib
import Data.List
import System.Environment

main :: IO ()
main = do
    [file] <- getArgs
    parseTree <- parseTokens file
    case parseTree of
        Left e -> print e
        Right t -> putStr $ intercalate "\n" $ map show t