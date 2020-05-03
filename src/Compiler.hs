{-# LANGUAGE TemplateHaskell #-}

module Compiler (
    runCompiler
) where

import Control.Lens
import Control.Monad
import Types
import Data.List

data TableEntry = IntVar String | FloatVar String | StringVar String String
instance Show TableEntry where
    show (IntVar name) = "name " ++ name ++ " type INT"
    show (FloatVar name) = "name " ++ name ++ " type FLOAT"
    show (StringVar name val) = "name " ++ name ++ " type STRING value " ++ val 

entryName :: TableEntry -> String
entryName (IntVar n) = n
entryName (FloatVar n) = n
entryName (StringVar n _) = n

data VarTable = VarTable String [TableEntry]
instance Show VarTable where
    show (VarTable name []) = "Symbol table " ++ name
    show (VarTable name entries) = "Symbol table " ++ name ++ "\n" ++ intercalate "\n" (map show entries)

data CompilerState = CompilerState {
    _tableStack :: [VarTable],
    _tableCount :: Int,
    _tempCount :: Int,
    _ir :: [String],
    _output :: String
}
makeLenses ''CompilerState

initState = CompilerState [] 0 0 [] ""



newtype Compiler a = Compiler (CompilerState -> Either String (CompilerState, a))

runCompiler :: Program -> String
runCompiler p =
    let (Compiler c) = compile p
    in case c initState of
        Left e -> e
        Right (s, _) -> s^.output

instance Functor Compiler where
    fmap f (Compiler m) = Compiler $ \s -> 
        case m s of
            Left e -> Left e
            Right (newS, val) -> Right (newS, f val)
        
instance Applicative Compiler where
    pure v = Compiler $ \s -> Right (s, v)
    (<*>) (Compiler f) (Compiler v) = Compiler $ \s ->
        case f s of
            Left e -> Left e
            Right (s1, func) ->
                case v s1 of
                    Left e -> Left e
                    Right (s2, val) -> Right (s2, func val)

instance Monad Compiler where
    (>>=) (Compiler m) f = Compiler $ \s ->
        case m s of
            Left e -> Left e
            Right (newS, val) -> newM newS where
                (Compiler newM) = f val



compilerError :: String -> Compiler a
compilerError msg = Compiler $ \_ -> Left msg

getState :: ALens' CompilerState a -> Compiler a
getState l = Compiler $ \s -> Right (s, s^#l)

setState :: ALens' CompilerState a -> a -> Compiler ()
setState l v = Compiler $ \s -> Right (storing l v s, ())



addEntry :: TableEntry -> Compiler ()
addEntry newVar = do
    tables <- getState tableStack
    let (VarTable name top) = head tables
    let newName = entryName newVar
    let failed = newName `elem` map entryName top

    let newTable = VarTable name (top ++ [newVar])
    let newStack = newTable : tail tables

    if failed
        then compilerError ("DECLARATION ERROR " ++ newName)
        else setState tableStack newStack

getEntry :: String -> Compiler TableEntry
getEntry name = getState tableStack >>= worker where
    worker [] = compilerError ("VARIABLE " ++ name ++ " NOT FOUND")
    worker (top:rest) = do
        let (VarTable _ entries) = top
        let matches = filter (\e -> entryName e == name) entries
        if null matches
            then worker rest
            else return $ head matches

addTable :: String -> Compiler ()
addTable name = do
    tables <- getState tableStack
    setState tableStack (VarTable name [] : tables)

removeTable :: Compiler ()
removeTable = do
    tables <- getState tableStack
    setState tableStack $ tail tables

newTemp :: Compiler String
newTemp = do
    count <- getState tempCount
    setState tempCount (count + 1)
    return ("T" ++ (show count))

addBlock :: Compiler ()
addBlock = do
    count <- getState tableCount
    setState tableCount (count + 1)

addIr :: String -> Compiler ()
addIr newIr = do
    oldIr <- getState ir
    setState ir (oldIr ++ [newIr])

addOutput :: String -> Compiler ()
addOutput out = do
    oldOutput <- getState output
    setState output (oldOutput ++ "\n" ++ out)



blockHelper :: [Declaration] -> [Statement] -> Compiler ()
blockHelper decls stmts = do
    addBlock
    count <- getState tableCount
    addTable ("BLOCK " ++ show count)
    mapM_ compile decls

    mapM_ compile stmts

    removeTable

factHelper :: Factor -> Compiler TableEntry
factHelper (FactLeaf pf) = do
    case pf of
        (ParenExpr expr) -> exprHelper expr
        (Identifier (ParseToken _ name)) -> getEntry name
        (FloatLiteral (ParseToken _ val)) -> return $ FloatVar val
        (IntLiteral (ParseToken _ val)) -> return $ IntVar val
        _ -> compilerError "Func calls undefined"
factHelper (FactNode l op r) = do 
    lEntry <- factHelper l
    rEntry <- factHelper r
    opName <- case op of
        Times -> return "MULT"
        Div -> return "DIV"
    t <- case (lEntry, rEntry) of
        ((IntVar _), (IntVar _)) -> return "I"
        _ -> return "F"
    tmp <- newTemp

    let fullOp = opName ++ t
    addIr $ intercalate " " [fullOp, entryName lEntry, entryName rEntry, tmp]

    case t of
        "I" -> return $ IntVar tmp
        "F" -> return $ FloatVar tmp

exprHelper :: Expression -> Compiler TableEntry
exprHelper (ExprLeaf f) = factHelper f
exprHelper (ExprNode l op r) = do
    lEntry <- exprHelper l
    rEntry <- exprHelper r
    opName <- case op of
        Plus -> return "ADD"
        Minus -> return "SUB"
    t <- case (lEntry, rEntry) of
        ((IntVar _), (IntVar _)) -> return "I"
        _ -> return "F"
    tmp <- newTemp

    let fullOp = opName ++ t
    addIr $ intercalate " " [fullOp, entryName lEntry, entryName rEntry, tmp]

    case t of
        "I" -> return $ IntVar tmp
        "F" -> return $ FloatVar tmp

class Compilable a where
    compile :: a -> Compiler ()

instance Compilable Declaration where
    compile (StringDeclaration (ParseToken _ name) (ParseToken _ val)) = addEntry $ StringVar name val
    compile (IntDeclaration (ParseToken _ name)) = addEntry $ IntVar name
    compile (FloatDeclaration (ParseToken _ name)) = addEntry $ FloatVar name

instance Compilable ElseStatement where
    compile (ElseStatement decls stmts) = blockHelper decls stmts

instance Compilable Statement where
    compile (AssignStatement (ParseToken t resName) expr) = do
        tmp <- exprHelper expr
        case tmp of
            (IntVar tmpName) -> addIr ("STOREI " ++ tmpName ++ " " ++ resName)
            (FloatVar tmpName) -> addIr ("STOREF " ++ tmpName ++ " " ++ resName)

    compile (ReadStatement tokens) =
        forM_ tokens $ \(ParseToken _ name) -> do
            token <- getEntry name
            case token of
                IntVar _ -> addIr ("READI " ++ name)
                FloatVar _ -> addIr ("READF " ++ name)

    compile (WriteStatement tokens) =
        forM_ tokens $ \(ParseToken _ name) -> do
            token <- getEntry name
            case token of
                IntVar _ -> addIr ("WRITEI " ++ name)
                FloatVar _ -> addIr ("WRITEF " ++ name)
                StringVar _ _ -> addIr ("WRITES " ++ name)

    compile (IfStatement _ decls stmts maybeElse) = do
        blockHelper decls stmts
        forM_ maybeElse compile

    compile (WhileStatement _ decls stmts) = blockHelper decls stmts

    compile _ = return ()

instance Compilable FuncBody where
    compile (FuncBody decls stmts) = do
        mapM_ compile decls
        mapM_ compile stmts

instance Compilable FuncDeclaration where
    compile (FuncDeclaration _ (ParseToken _ name) decls funcBody) = do
        addTable name
        mapM_ compile decls
        compile funcBody
        removeTable

instance Compilable ProgramBody where
    compile (ProgramBody decls funcDecls) = do
        addTable "GLOBAL"
        mapM_ compile decls

        mapM_ compile funcDecls

instance Compilable Program where
    compile (Program _ prgBody) = do
        compile prgBody
        intermediate <- getState ir
        setState output $ intercalate "\n" intermediate
