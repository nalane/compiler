module Parsing () where

import Text.Parsec
import Text.Parsec.Text
import Types

id' :: Parsec Id
id' = do
    firstLetter <- letter
    rest <- many (letter <|> number)
    return $ Id (firstLetter:rest)

varType :: Parsec VarType
varType = do
    t <- string "FLOAT" <|> string "INT"
    case t of
        "FLOAT" -> return $ VarTypeFloat t
        "INT" -> return $ VarTypeInt t

str :: Parsec Str
str = do
    char '"'
    lit <- many $ noneOf "\""
    char '"'
    return $ Str $ StringLit ("\"" ++ lit ++ "\"")

stringDecl :: Parsec StringDecl
stringDecl = do
    spaces
    string "STRING"
    spaces
    name <- id'
    spaces
    string ":="
    spaces
    lit <- str
    spaces
    char ';'
    return $ StringDecl "STRING" name (Operator ":=") lit (Operator ";")

decl :: Parsec Decl
decl = do


pgmBody :: Program PgmBody
pgmBody = do
    declarations <- decl
    spaces
    functions <- func_declarations
    PgmBody declarations <$> functions

program :: Parsec Program
program = do
    string "PROGRAM"
    spaces
    name <- id'
    spaces
    string "BEGIN"
    spaces
    pgm_body <- pgmBody
    spaces
    string "END"
    return $ Program (Keyword "PROGRAM") name (Keyword "BEGIN") pgm_body $ Keyword "END"