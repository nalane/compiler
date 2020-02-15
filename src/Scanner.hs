module Scanner (
    identifier, intLiteral, floatLiteral, stringLiteral, keyword, operator
) where

import Types

import Data.Text
import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Char
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

keywords = [ "PROGRAM", "BEGIN", "END", "FUNCTION", "READ"
           , "WRITE", "IF", "ELSE", "ENDIF", "WHILE"
           , "ENDWHILE", "CONTINUE", "BREAK", "RETURN"
           , "INT", "VOID", "STRING", "FLOAT"
           ]

operators = [ ":=", "+", "-", "*", "/", "=", "!=", "<=", ">="
            , "<", ">", "(", ")", ";", ","
            ]

-- Define the language
tokenDef =
    LanguageDef { Token.commentStart = ""
             , Token.commentEnd = ""
             , Token.commentLine = "--"
             , Token.nestedComments = False
             , Token.identStart = letter
             , Token.identLetter = alphaNum
             , Token.opStart = oneOf ""
             , Token.opLetter = oneOf ""
             , Token.reservedNames = keywords
             , Token.reservedOpNames = operators
             , Token.caseSensitive = True
             }
lexer = Token.makeTokenParser tokenDef

keyword :: String -> Parser ParseTree
keyword x = do
    Token.reserved lexer x
    return $ ParseToken KEYWORD x

operator :: String -> Parser ParseTree
operator x = do
    Token.reservedOp lexer x
    return $ ParseToken OPERATOR x

identifier :: Parser ParseTree
identifier = ParseToken IDENTIFIER <$> Token.identifier lexer

intLiteral :: Parser ParseTree
intLiteral = ParseToken INTLITERAL . show <$> Token.natural lexer

-- The default float literal function provided by Parsec will produce an
-- actual Double value, rather than a string of the token. This is problematic
-- when trying to print, as, for eaxample, 0.001 will print as 1e-3. Therefore,
-- we must write our own float-parsing function.
floatLiteral :: Parser ParseTree
floatLiteral = Token.lexeme lexer (do
    first <- many digit
    char '.'
    second <- many1 digit
    return $ ParseToken FLOATLITERAL (first ++ "." ++ second))

-- Similarly, strings will be parsed with escaped characters being substituted.
-- Thus, for example, the string "\n" will become an actual newline. Therefore,
-- we must also write our own string parsing function.
stringLiteral :: Parser ParseTree
stringLiteral = Token.lexeme lexer (do
    char '"'
    x <- many $ noneOf "\""
    char '"'
    return $ ParseToken STRINGLITERAL ("\"" ++ x ++ "\""))
