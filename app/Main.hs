module Main where

import Lib
import Text.Parsec
import Text.Parsec.Char
import qualified Data.Text as T

main :: IO ()
main = do
    input <- T.pack <$> getLine
    print $ parse r' "" input