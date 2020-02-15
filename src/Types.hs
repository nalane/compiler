module Types where

import Data.List

data TokenType = IDENTIFIER | INTLITERAL | FLOATLITERAL | STRINGLITERAL | KEYWORD | OPERATOR deriving (Show, Eq)
data ParseTree = ParseNode [ParseTree] | ParseToken TokenType String deriving (Eq)

instance Show ParseTree where
    show (ParseNode t) = intercalate "\n" $ map show $ filter (\x -> x /= ParseNode []) t
    show (ParseToken t s) = "Token Type: " ++ show t ++ "\nValue: " ++ s