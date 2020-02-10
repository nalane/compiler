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

operators = [ ":=", "+", "-", "*", "/", "=", "!=", "<"
            , ">", "(", ")", ";", ",", "<=", ">="
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

identifier = Token.identifier lexer
intLiteral = Token.natural lexer
floatLiteral = Token.float lexer
keyword = foldr ((<|>) . (\ x -> try (Token.reserved lexer x >> return x))) (Token.reserved lexer (head keywords) >> return (head keywords)) $ tail keywords
operator = foldr ((<|>) . (\ x -> try (Token.reservedOp lexer x >> return x))) (Token.reservedOp lexer (head operators) >> return (head operators)) $ tail operators
stringLiteral = do
    char '"'
    x <- many $ noneOf "\""
    char '"'
    return ("\"" ++ x ++ "\"")

tokenTypes = [KEYWORD, IDENTIFIER, OPERATOR, FLOATLITERAL, INTLITERAL, STRINGLITERAL]
tokenParsers = [keyword, identifier, operator, show <$> floatLiteral, show <$> intLiteral, stringLiteral]
tok = foldr (<|>) (fail "Could not identify next token") $ zipWith (\t p -> try (ParseToken t <$> p)) tokenTypes tokenParsers

parseTokens :: String -> IO (Either ParseError [ParseTree])
parseTokens filename = parse (do
    toks <- many tok
    eof
    return toks) filename <$> readFile filename