module Scanner (
    parseTokens,
    identifier, intLiteral, floatLiteral, stringLiteral, keyword, operator
) where

import Types

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

tokenDef =
    emptyDef { Token.commentStart = ""
             , Token.commentEnd = ""
             , Token.commentLine = "--"
             , Token.nestedComments = False
             , Token.identStart = letter
             , Token.identLetter = alphaNum
             , Token.opLetter = oneOf ""
             , Token.reservedNames = keywords
             , Token.reservedOpNames = operators
             , Token.caseSensitive = True
             }

lexer = Token.makeTokenParser tokenDef

keyword = foldr ((<|>) . (\ x -> try (Token.reserved lexer x >> return x))) (fail "Could not match any keyword") keywords
operator = foldr ((<|>) . (\ x -> try (Token.reservedOp lexer x >> return x))) (fail "Could not match any operators") operators
identifier = Token.identifier lexer
intLiteral = Token.natural lexer
floatLiteral = do
    first <- many digit
    char '.'
    second <- many1 digit
    return (first ++ "." ++ second)
stringLiteral = do
    char '"'
    x <- many $ noneOf "\""
    char '"'
    return ("\"" ++ x ++ "\"")

tokenTypes = [KEYWORD, OPERATOR, IDENTIFIER, FLOATLITERAL, INTLITERAL, STRINGLITERAL]
tokenParsers = [keyword, operator, identifier, floatLiteral, show <$> intLiteral, stringLiteral]
tok = foldr (<|>) (fail "Could not identify next token") $ zipWith (\t p -> try (ParseToken t <$> p)) tokenTypes tokenParsers

parseTokens :: String -> IO (Either ParseError [ParseTree])
parseTokens filename = parse (do
    toks <- many tok
    eof
    return toks) filename <$> readFile filename