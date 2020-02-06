module Main where

import Lib
import System.Environment

main :: IO ()
main = do
    [file] <- getArgs
    parseTree <- parseProgram file
    case parseTree of
        Left e -> print e
        Right t -> print t