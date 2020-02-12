module Types where

import Data.List

data TokenType = IDENTIFIER | INTLITERAL | FLOATLITERAL | STRINGLITERAL | KEYWORD | OPERATOR deriving (Show)
data ParseToken = ParseToken TokenType String

instance Show ParseToken where
    show (ParseToken t s) = "Token Type: " ++ show t ++ "\nValue: " ++ s