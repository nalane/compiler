module Parser (
    parseProgram
) where

import Prelude hiding (id)
import Text.Parsec
import Text.Parsec.Text
import Types
import Scanner

parens' :: Parser ParseTree -> Parser [ParseTree]
parens' middle = do
    left <- operator "("
    m <- middle
    right <- operator ")"
    return [left, m, right]

assign' :: Parser [ParseTree]
assign' = do
    name <- identifier
    op <- operator ":="
    return [name, op]

decl' :: Parser [ParseTree]
decl' = do
    d <- decl
    s <- stmtList
    return [d, s]

ifWhile' :: Parser [ParseTree]
ifWhile' = do
    l <- parens' cond
    ds <- decl'
    return (l ++ ds)

------------------------------

stringDecl :: Parser ParseTree
stringDecl = do
    key <- keyword "STRING"
    a <- assign'
    lit <- stringLiteral
    semi <- operator ";"
    return $ ParseNode (key:(a ++ [lit, semi]))

varType :: Parser ParseTree
varType = try (keyword "FLOAT") <|> keyword "INT"

idTail :: Parser ParseTree
idTail = try parseIdTail <|> return (ParseNode []) where
    parseIdTail = do
        comma <- operator ","
        name <- identifier
        rest <- idTail
        return $ ParseNode [comma, name, rest]

idList :: Parser ParseTree
idList = do
    name <- identifier
    rest <- idTail
    return $ ParseNode [name, rest]

varDecl :: Parser ParseTree
varDecl = do
    t <- varType
    l <- idList
    op <- operator ";"
    return $ ParseNode [t, l, op]

decl :: Parser ParseTree
decl = try parseStringDecl <|> try parseVarDecl <|> return (ParseNode []) where
    parseStringDecl = do
        s <- stringDecl
        d <- decl
        return $ ParseNode [s, d]
    parseVarDecl = do
        v <- varDecl
        d <- decl
        return $ ParseNode [v, d]

anyType :: Parser ParseTree
anyType = try varType <|> keyword "VOID"

paramDecl :: Parser ParseTree
paramDecl = do
    t <- varType
    name <- identifier
    return $ ParseNode [t, name]

paramDeclTail :: Parser ParseTree
paramDeclTail = try parseParamDeclTail <|> return (ParseNode []) where
    parseParamDeclTail = do
        op <- operator ","
        p <- paramDecl
        rest <- paramDeclTail
        return $ ParseNode [op, p, rest]

paramDeclList :: Parser ParseTree
paramDeclList = try parseParamDeclList <|> return (ParseNode []) where
    parseParamDeclList = do
        p <- paramDecl
        rest <- paramDeclTail
        return $ ParseNode [p, rest]

primary :: Parser ParseTree
primary = try parseExpr <|> try identifier <|> try floatLiteral <|> intLiteral where
    parseExpr = ParseNode <$> parens' expr

exprListTail :: Parser ParseTree
exprListTail = try parseExprListTail <|> return (ParseNode []) where
    parseExprListTail = do
        op <- operator ","
        e <- expr
        rest <- exprListTail
        return $ ParseNode [op, e, rest]

exprList :: Parser ParseTree
exprList = try parseExprList <|> return (ParseNode []) where
    parseExprList = do
        e <- expr
        rest <- exprListTail
        return $ ParseNode [e, rest]

callExpr :: Parser ParseTree
callExpr = do
    name <- identifier
    l <- parens' exprList
    return $ ParseNode (name:l)

postfixExpr :: Parser ParseTree
postfixExpr = try callExpr <|> primary

mulOp :: Parser ParseTree
mulOp = try (operator "*") <|> operator "/"

factor :: Parser ParseTree
factor = worker $ ParseNode [] where
    worker tree = do
        post <- postfixExpr
        try (do
            m <- mulOp
            worker $ ParseNode [tree, post, m]) <|> return (ParseNode [tree, post])

addOp :: Parser ParseTree
addOp = try (operator "+") <|> operator "-"

expr :: Parser ParseTree
expr = worker $ ParseNode [] where
    worker tree = do
        f <- factor
        try (do
            a <- addOp
            worker $ ParseNode [tree, f, a]) <|> return (ParseNode [tree, f])

assignExpr :: Parser ParseTree
assignExpr = do
    a <- assign'
    e <- expr
    return $ ParseNode (a ++ [e])

assignStmt :: Parser ParseTree
assignStmt = do
    e <- assignExpr
    semi <- operator ";"
    return $ ParseNode [e, semi]

readStmt :: Parser ParseTree
readStmt = do
    read <- keyword "READ"
    l <- parens' idList
    semi <- operator ";"
    return $ ParseNode (read:(l ++ [semi]))

writeStmt :: Parser ParseTree
writeStmt = do
    write <- keyword "WRITE"
    l <- parens' idList
    semi <- operator ";"
    return $ ParseNode (write:(l ++ [semi]))

returnStmt :: Parser ParseTree
returnStmt = do
    ret <- keyword "RETURN"
    e <- expr
    semi <- operator ";"
    return $ ParseNode [ret, e, semi]

baseStmt :: Parser ParseTree
baseStmt = try assignStmt <|> try readStmt <|> try writeStmt <|> returnStmt

compop :: Parser ParseTree
compop = 
    try (operator "<=") <|> 
    try (operator "!=") <|>
    try (operator ">=") <|>
    try (operator "<") <|>
    try (operator ">") <|> 
    operator "=" 

cond :: Parser ParseTree
cond = do
    e1 <- expr
    c <- compop
    e2 <- expr
    return $ ParseNode [e1, c, e2]

elsePart :: Parser ParseTree
elsePart = try parseElsePart <|> return (ParseNode []) where
    parseElsePart = do
        key <- keyword "ELSE"
        ds <- decl'
        return $ ParseNode (key : ds)

ifStmt :: Parser ParseTree
ifStmt = do
    i <- keyword "IF"
    iw <- ifWhile'
    e <- elsePart
    end <- keyword "ENDIF"
    return $ ParseNode (i:(iw ++ [e, end]))

whileStmt :: Parser ParseTree
whileStmt = do
    w <- keyword "WHILE"
    iw <- ifWhile'
    end <- keyword "ENDWHILE"
    return $ ParseNode (w:(iw ++ [end]))

stmt :: Parser ParseTree
stmt = try baseStmt <|> try ifStmt <|> whileStmt

stmtList :: Parser ParseTree
stmtList = try parseStmtList <|> return (ParseNode []) where
    parseStmtList = do
        s <- stmt
        rest <- stmtList
        return $ ParseNode [s, rest]

funcBody :: Parser ParseTree
funcBody = ParseNode <$> decl'

funcDecl :: Parser ParseTree
funcDecl = do
    func <- keyword "FUNCTION"
    t <- anyType
    name <- identifier
    left <- operator "("
    ps <- paramDeclList
    right <- operator ")"
    begin <- keyword "BEGIN"
    f <- funcBody
    end <- keyword "END"
    return $ ParseNode [func, t, name, left, ps, right, begin, f, end]

funcDeclarations :: Parser ParseTree
funcDeclarations = try parseFuncDeclarations <|> return (ParseNode []) where
    parseFuncDeclarations = do
        f <- funcDecl
        d <- funcDeclarations
        return $ ParseNode [f, d]

pgmBody :: Parser ParseTree
pgmBody = do
    d <- decl
    f <- funcDeclarations
    return $ ParseNode [d, f]

program :: Parser ParseTree
program = do
    prog <- keyword "PROGRAM"
    name <- identifier
    begin <- keyword "BEGIN"
    p <- pgmBody
    end <- keyword "END"
    return $ ParseNode [prog, name, begin, p, end]

parseProgram :: String -> IO (Either ParseError ParseTree)
parseProgram = parseFromFile program