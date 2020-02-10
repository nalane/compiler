module Types where

import Data.List

data TokenType = IDENTIFIER | INTLITERAL | FLOATLITERAL | STRINGLITERAL | KEYWORD | OPERATOR deriving (Show)
data ParseTree = ParseNode [ParseTree] | ParseToken TokenType String

instance Show ParseTree where
    show (ParseNode l) = concatMap show l
    show (ParseToken t s) = "Token Type: " ++ show t ++ "\nValue: " ++ s