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

-- Define the language
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

-- The reserved and reservedOp functions in Parsec do not return the matched
-- token. Instead, they take the expected token as input and succeed or fail.
-- Therefore, we must iterate through each token; if it succeeds, return that
-- token. If all fail, then we error.
keyword = foldr ((<|>) . parseReserved) (fail failMsg) keywords where
    failMsg = "Could not match any keyword"
    parseReserved x = try (Token.reserved lexer x >> return x)
operator = foldr ((<|>) . parseOperator) (fail failMsg) operators where
    failMsg = "Could not match any operators"
    parseOperator x = try (Token.reservedOp lexer x >> return x)

-- The identifier and natural functions created by Parsec from the language
-- definition are adequate for identifiers and int literals, respectively.
identifier = Token.identifier lexer
intLiteral = show <$> Token.natural lexer

-- The default float literal function provided by Parsec will produce an
-- actual Double value, rather than a string of the token. This is problematic
-- when trying to print, as, for eaxample, 0.001 will print as 1e-3. Therefore,
-- we must write our own float-parsing function.
floatLiteral = do
    first <- many digit
    char '.'
    second <- many1 digit
    return (first ++ "." ++ second)

-- Similarly, strings will be parsed with escaped characters being substituted.
-- Thus, for example, the string "\n" will become an actual newline. Therefore,
-- we must also write our own string parsing function.
stringLiteral = do
    char '"'
    x <- many $ noneOf "\""
    char '"'
    return ("\"" ++ x ++ "\"")

-- As above, we cycle through each type of token. If a parser succeeds,
-- we consider it a match. If all parsers fail, it is an error.
tokenTypes = [KEYWORD, OPERATOR, IDENTIFIER, FLOATLITERAL, INTLITERAL, STRINGLITERAL]
tokenParsers = [keyword, operator, identifier, floatLiteral, intLiteral, stringLiteral]
tok = foldr (<|>) (fail "Could not identify next token") tokGens where
    tokGens = zipWith (\t p -> try (ParseToken t <$> p)) tokenTypes tokenParsers

-- Read the complete contents of the file, then pass it into the parser.
-- At this stage, the parser just gives us a list of tokens until we 
-- reach the end of the file.
parseTokens :: String -> IO (Either ParseError [ParseTree])
parseTokens filename = parse (do
    toks <- many tok
    eof
    return toks) filename <$> readFile filename