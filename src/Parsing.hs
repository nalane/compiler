module Parsing (
    parseProgram
) where

import Prelude hiding (id)
import Text.Parsec
import Text.Parsec.Text
import Types

ignore' :: Parser ()
ignore' = do
    spaces
    try (do
        string "--"
        many $ noneOf "\n"
        ignore') <|> return ()

parens' :: Parser ParseTree -> Parser [ParseTree]
parens' middle = do
    spaces
    char '('
    spaces
    m <- middle
    spaces
    char ')'
    spaces
    return [ParseToken OPERATOR "(", m, ParseToken OPERATOR ")"]

id :: Parser ParseTree
id = do
    firstLetter <- letter
    rest <- many (letter <|> digit)
    return $ ParseToken IDENTIFIER (firstLetter:rest)

id' :: Parser ParseTree
id' = do
    spaces
    name <- id
    spaces
    return name

str :: Parser ParseTree
str = do
    char '"'
    lit <- many $ noneOf "\""
    char '"'
    return $ ParseToken STRINGLITERAL ("\"" ++ lit ++ "\"")

assign' :: Parser [ParseTree]
assign' = do
    name <- id'
    string ":="
    spaces
    return [name, ParseToken OPERATOR ":="]

stringDecl :: Parser ParseTree
stringDecl = do
    spaces
    string "STRING"
    a <- assign'
    lit <- str
    spaces
    char ';'
    return $ ParseNode ([ParseToken KEYWORD "STRING"] ++ a ++ [lit, ParseToken OPERATOR ";"])

varType :: Parser ParseTree
varType = do
    t <- string "FLOAT" <|> string "INT"
    return $ ParseToken KEYWORD t

idTail :: Parser ParseTree
idTail = try parseIdTail <|> return (ParseNode []) where
    parseIdTail = do
        char ','
        name <- id'
        rest <- idTail
        return $ ParseNode [ParseToken OPERATOR ",", name, rest]

idList :: Parser ParseTree
idList = do
    name <- id'
    rest <- idTail
    return $ ParseNode [name, rest]

varDecl :: Parser ParseTree
varDecl = do
    spaces
    t <- varType
    spaces
    l <- idList
    spaces
    char ';'
    return $ ParseNode [t, l, ParseToken OPERATOR ";"]

decl :: Parser ParseTree
decl = try parseStringDecl <|> try parseVarDecl <|> return (ParseNode []) where
    parseStringDecl = do
        ignore'
        s <- stringDecl
        ignore'
        d <- decl
        return $ ParseNode [s, d]
    parseVarDecl = do
        ignore'
        v <- varDecl
        ignore'
        d <- decl
        return $ ParseNode [v, d]

anyType :: Parser ParseTree
anyType = try varType <|> (do
    string "VOID"
    return $ ParseToken KEYWORD "VOID")

paramDecl :: Parser ParseTree
paramDecl = do
    t <- varType
    name <- id'
    return $ ParseNode [t, name]

paramDeclTail :: Parser ParseTree
paramDeclTail = try parseParamDeclTail <|> return (ParseNode []) where
    parseParamDeclTail = do
        char ','
        spaces
        p <- paramDecl
        rest <- paramDeclTail
        return $ ParseNode [ParseToken OPERATOR ",", p, rest]

paramDeclList :: Parser ParseTree
paramDeclList = try parseParamDeclList <|> return (ParseNode []) where
    parseParamDeclList = do
        p <- paramDecl
        rest <- paramDeclTail
        return $ ParseNode [p, rest]

intLiteral :: Parser ParseTree
intLiteral = do
    num <- many1 digit
    return $ ParseToken INTLITERAL num

floatLiteral :: Parser ParseTree
floatLiteral = do
    first <- many digit
    char '.'
    second <- many1 digit
    return $ ParseToken FLOATLITERAL (first ++ "." ++ second)

primary :: Parser ParseTree
primary = try parseExpr <|> try id' <|> try floatLiteral <|> intLiteral where
    parseExpr = do
        l <- parens' expr
        return $ ParseNode l

exprListTail :: Parser ParseTree
exprListTail = try parseExprListTail <|> return (ParseNode []) where
    parseExprListTail = do
        char ','
        e <- expr'
        rest <- exprListTail
        return $ ParseNode [ParseToken OPERATOR ",", e, rest]

exprList :: Parser ParseTree
exprList = try parseExprList <|> return (ParseNode []) where
    parseExprList = do
        e <- expr
        spaces
        rest <- exprListTail
        return $ ParseNode [e, rest]

callExpr :: Parser ParseTree
callExpr = do
    name <- id'
    char '('
    spaces
    l <- exprList
    spaces
    char ')'
    return $ ParseNode [name, ParseToken OPERATOR "(", l, ParseToken OPERATOR ")"]

postfixExpr :: Parser ParseTree
postfixExpr = try callExpr <|> primary

mulOp :: Parser ParseTree
mulOp = do
    o <- string "*" <|> string "/"
    return $ ParseToken OPERATOR o

factorPrefix :: Parser ParseTree
factorPrefix = try parseFactorPrefix <|> return (ParseNode []) where
    parseFactorPrefix = do
        pre <- factorPrefix
        spaces
        post <- postfixExpr
        spaces
        m <- mulOp
        return $ ParseNode [pre, post, m]

factor :: Parser ParseTree
factor = do
    post <- postfixExpr
    spaces
    worker post where
        worker tree = try parseFactor <|> return tree where
            parseFactor = do
                m <- mulOp
                spaces
                post2 <- postfixExpr
                spaces
                worker $ ParseNode [tree, m, post2]

addOp :: Parser ParseTree
addOp = do
    o <- string "+" <|> string "-"
    return $ ParseToken OPERATOR o

exprPrefix :: Parser ParseTree
exprPrefix = try parseExprPrefix <|> return (ParseNode []) where
    parseExprPrefix = do
        p <- exprPrefix
        spaces
        f <- factor
        spaces
        a <- addOp
        return $ ParseNode [p, f, a]

expr' :: Parser ParseTree
expr' = do
    spaces
    e <- expr
    spaces
    return e

expr :: Parser ParseTree
expr = do
    f <- factor
    spaces
    worker f where
        worker tree = try parseExpr <|> return tree where
            parseExpr = do
                a <- addOp
                spaces
                f2 <- factor
                spaces
                worker $ ParseNode [tree, a, f2]

assignExpr :: Parser ParseTree
assignExpr = do
    a <- assign'
    e <- expr
    return $ ParseNode (a ++ [e])

assignStmt :: Parser ParseTree
assignStmt = do
    e <- assignExpr
    spaces
    char ';'
    return $ ParseNode [e, ParseToken OPERATOR ";"]

readStmt :: Parser ParseTree
readStmt = do
    string "READ"
    l <- parens' idList
    char ';'
    return $ ParseNode ([ParseToken KEYWORD "READ"] ++ l ++ [ParseToken OPERATOR ";"])

writeStmt :: Parser ParseTree
writeStmt = do
    string "WRITE"
    l <- parens' idList
    char ';'
    return $ ParseNode ([ParseToken KEYWORD "WRITE"] ++ l ++ [ParseToken OPERATOR ";"])

returnStmt :: Parser ParseTree
returnStmt = do
    string "RETURN"
    e <- expr'
    char ';'
    return $ ParseNode [ParseToken KEYWORD "RETURN", e, ParseToken OPERATOR ";"]

baseStmt :: Parser ParseTree
baseStmt = try assignStmt <|> try readStmt <|> try writeStmt <|> returnStmt

compop :: Parser ParseTree
compop = do
    c <- try (string "<=") <|> try (string "!=") <|> try (string ">=") <|> try (string "<") <|> try (string ">") <|> string "=" 
    return $ ParseToken OPERATOR c

cond :: Parser ParseTree
cond = do
    e1 <- expr
    spaces
    c <- compop
    spaces
    e2 <- expr
    return $ ParseNode [e1, c, e2]

decl' :: Parser [ParseTree]
decl' = do
    d <- decl
    ignore'
    s <- stmtList
    return [d, s]

elsePart :: Parser ParseTree
elsePart = try parseElsePart <|> return (ParseNode []) where
    parseElsePart = do
        string "ELSE"
        spaces
        ds <- decl'
        return $ ParseNode (ParseToken KEYWORD "ELSE" : ds)

ifWhile' :: Parser [ParseTree]
ifWhile' = do
    l <- parens' cond
    ds <- decl'
    spaces
    return (l ++ ds)

ifStmt :: Parser ParseTree
ifStmt = do
    string "IF"
    iw <- ifWhile'
    e <- elsePart
    spaces
    string "ENDIF"
    return $ ParseNode ([ParseToken KEYWORD "IF"] ++ iw ++ [e, ParseToken KEYWORD "ENDIF"])

whileStmt :: Parser ParseTree
whileStmt = do
    string "WHILE"
    iw <- ifWhile'
    string "ENDWHILE"
    return $ ParseNode ([ParseToken KEYWORD "WHILE"] ++ iw ++ [ParseToken KEYWORD "ENDWHILE"])

stmt :: Parser ParseTree
stmt = try baseStmt <|> try ifStmt <|> whileStmt

stmtList :: Parser ParseTree
stmtList = try parseStmtList <|> return (ParseNode []) where
    parseStmtList = do
        ignore'
        s <- stmt
        ignore'
        rest <- stmtList
        return $ ParseNode [s, rest]

funcBody :: Parser ParseTree
funcBody = ParseNode <$> decl'

funcDecl :: Parser ParseTree
funcDecl = do
    ignore'
    string "FUNCTION"
    spaces
    t <- anyType
    name <- id'
    char '('
    ps <- paramDeclList
    char ')'
    ignore'
    string "BEGIN"
    spaces
    f <- funcBody
    spaces
    string "END"
    return $ ParseNode [ParseToken KEYWORD "FUNCTION", t, name, ParseToken OPERATOR "(", ps, ParseToken OPERATOR ")", ParseToken KEYWORD "BEGIN", f, ParseToken KEYWORD "END"]

funcDeclarations :: Parser ParseTree
funcDeclarations = try parseFuncDeclarations <|> return (ParseNode []) where
    parseFuncDeclarations = do
        f <- funcDecl
        ignore'
        d <- funcDeclarations
        return $ ParseNode [f, d]

pgmBody :: Parser ParseTree
pgmBody = do
    d <- decl
    ignore'
    f <- funcDeclarations
    return $ ParseNode [d, f]

program :: Parser ParseTree
program = do
    ignore'
    string "PROGRAM"
    ignore'
    name <- id'
    ignore'
    string "BEGIN"
    ignore'
    p <- pgmBody
    ignore'
    string "END"
    ignore'
    return $ ParseNode [ParseToken KEYWORD "PROGRAM", name, ParseToken KEYWORD "BEGIN", p, ParseToken KEYWORD "END"]

parseProgram :: String -> IO (Either ParseError ParseTree)
parseProgram = parseFromFile program