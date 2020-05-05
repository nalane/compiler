module Parser (
    parseProgram
) where

import Prelude hiding (id)
import Text.Parsec
import Text.Parsec.Text
import qualified Data.Text.IO as T
import Types
import Scanner

stringDecl :: Parser Declaration
stringDecl = do
    keyword "STRING"
    name <- identifier
    operator ":="
    lit <- stringLiteral
    operator ";"
    return $ StringDeclaration name lit

varType :: Parser ParseToken
varType = try (keyword "FLOAT") <|> keyword "INT"

idList :: Parser [ParseToken]
idList = commaSep identifier

varDecl :: Parser [Declaration]
varDecl = do
    (ParseToken _ t) <- varType
    l <- idList
    operator ";"
    return $ case t of
        "FLOAT" -> map FloatDeclaration l
        "INT" -> map IntDeclaration l

decl :: Parser [Declaration]
decl = worker [] where
    worker list = 
        try (do
            s <- stringDecl
            worker (list ++ [s])) <|>
        try (do
            v <- varDecl
            worker (list ++ v)) <|>
        return list

anyType :: Parser ParseToken
anyType = try varType <|> keyword "VOID"

paramDecl :: Parser Declaration
paramDecl = do
    (ParseToken _ t) <- varType
    name <- identifier
    return $ case t of
        "FLOAT" -> FloatDeclaration name
        "INT" -> IntDeclaration name

paramDeclList :: Parser [Declaration]
paramDeclList = commaSep paramDecl

primary :: Parser PostFix
primary = 
    try (ParenExpr <$> parens expr) <|> 
    try (Identifier <$> identifier) <|> 
    try (FloatLiteral <$> floatLiteral) <|> 
    (IntLiteral <$> intLiteral)

exprList :: Parser [Expression]
exprList = commaSep expr

callExpr :: Parser PostFix
callExpr = do
    name <- identifier
    l <- parens exprList
    return $ CallExpr name l

postfixExpr :: Parser PostFix
postfixExpr = try callExpr <|> primary

mulOp :: Parser MulOp
mulOp = try (operator "*" >> return Times) <|> (operator "/" >> return Div)

factor :: Parser Factor
factor = do
    post <- postfixExpr
    worker $ FactLeaf post where
        worker left = try (do
            m <- mulOp
            right <- postfixExpr
            worker $ FactNode left m $ FactLeaf right) <|> return left

addOp :: Parser AddOp
addOp = try (operator "+" >> return Plus) <|> (operator "-" >> return Minus) 

expr :: Parser Expression
expr = do
    f <- factor
    worker $ ExprLeaf f where
        worker left = try (do
            a <- addOp
            right <- factor
            worker $ ExprNode left a $ ExprLeaf right) <|> return left

assignExpr :: Parser Statement
assignExpr = do
    name <- identifier
    operator ":="
    AssignStatement name <$> expr

assignStmt :: Parser Statement
assignStmt = do
    e <- assignExpr
    operator ";"
    return e

readStmt :: Parser Statement
readStmt = do
    keyword "READ"
    l <- parens idList
    operator ";"
    return $ ReadStatement l

writeStmt :: Parser Statement
writeStmt = do
    keyword "WRITE"
    l <- parens idList
    operator ";"
    return $ WriteStatement l

returnStmt :: Parser Statement
returnStmt = do
    keyword "RETURN"
    e <- expr
    operator ";"
    return $ ReturnStatement e

baseStmt :: Parser Statement
baseStmt = try assignStmt <|> try readStmt <|> try writeStmt <|> returnStmt

compop :: Parser CompOp
compop = choice $ zipWith (\o t -> try (operator o) >> return t) ops types where
    ops = ["<=", "!=", ">=", "<", ">", "="]
    types = [LessEqual, NotEqual, GreatEqual, Less, Great, Equal]

cond :: Parser Condition
cond = do
    e1 <- expr
    c <- compop
    Condition e1 c <$> expr

elsePart :: Parser (Maybe ElseStatement)
elsePart = try parseElsePart <|> return Nothing where
    parseElsePart = do
        key <- keyword "ELSE"
        d <- decl
        Just . ElseStatement d <$> stmtList

ifStmt :: Parser Statement
ifStmt = do
    keyword "IF"
    c <- parens cond
    d <- decl
    s <- stmtList
    e <- elsePart
    keyword "ENDIF"
    return $ IfStatement c d s e

whileStmt :: Parser Statement
whileStmt = do
    keyword "WHILE"
    c <- parens cond
    d <- decl
    s <- stmtList
    keyword "ENDWHILE"
    return $ WhileStatement c d s

stmt :: Parser Statement
stmt = try baseStmt <|> try ifStmt <|> whileStmt

stmtList :: Parser [Statement]
stmtList = worker [] where
    worker list = try (do
        s <- stmt
        worker (list ++ [s])) <|> return list

funcBody :: Parser FuncBody
funcBody = do
    d <- decl
    FuncBody d <$> stmtList

funcDecl :: Parser FuncDeclaration
funcDecl = do
    keyword "FUNCTION"
    t <- anyType
    name <- identifier
    ps <- parens paramDeclList
    keyword "BEGIN"
    f <- funcBody
    keyword "END"
    return $ FuncDeclaration t name ps f

funcDeclarations :: Parser [FuncDeclaration]
funcDeclarations = worker [] where
    worker list = try (do
        f <- funcDecl
        worker (list ++ [f])) <|> return list

pgmBody :: Parser ProgramBody
pgmBody = do
    d <- decl
    ProgramBody d <$> funcDeclarations

program :: Parser Program
program = do
    keyword "PROGRAM"
    name <- identifier
    keyword "BEGIN"
    p <- pgmBody
    keyword "END"
    return $ Program name p

parseProgram :: String -> IO (Either ParseError Program)
parseProgram filename = do
    t <- T.readFile filename
    return $ parse program filename t