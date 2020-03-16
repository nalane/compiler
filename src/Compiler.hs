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
    _tableCount :: Int,
    _output :: String,
    _tableStack :: [VarTable]
}
makeLenses ''CompilerState

initState = CompilerState 0 "" []



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

getState :: ALens' CompilerState a -> Compiler a
getState l = Compiler $ \s -> Right (s, s^#l)

addEntry :: TableEntry -> Compiler ()
addEntry newVar = Compiler $ \s ->
    let tables = s^.tableStack
        (VarTable name top) = head tables
        newName = entryName newVar
        failed = newName `elem` map entryName top

        newTable = VarTable name (top ++ [newVar])
        newStack = newTable : tail tables
    in if failed 
        then Left ("DECLARATION ERROR " ++ newName) 
        else Right (set tableStack newStack s, ())

addTable :: String -> Compiler ()
addTable name = Compiler $ \s ->
    let tables = s^.tableStack
        newStack = VarTable name [] : tables
    in Right (set tableStack newStack s, ())

addBlock :: Compiler ()
addBlock = Compiler $ \s ->
    Right (over tableCount (+1) s, ())

addOutput :: String -> Compiler ()
addOutput out = Compiler $ \s ->
    let oldOutput = s^.output
        newOutput = oldOutput ++ out ++ "\n\n"
    in Right (set output newOutput s, ())

writeTable :: Compiler ()
writeTable = do
    tables <- getState tableStack
    addOutput $ show $ head tables


compileWriteHelper :: Compilable a => [a] -> Compiler ()
compileWriteHelper l = do
    mapM_ compile l
    writeTable

blockHelper :: [Declaration] -> [Statement] -> Compiler ()
blockHelper decls stmts = do
    addBlock
    count <- getState tableCount
    addTable ("BLOCK " ++ show count)
    compileWriteHelper decls

    mapM_ compile stmts

class Compilable a where
    compile :: a -> Compiler ()

instance Compilable Declaration where
    compile (StringDeclaration (ParseToken _ name) (ParseToken _ val)) = addEntry $ StringVar name val
    compile (IntDeclaration (ParseToken _ name)) = addEntry $ IntVar name
    compile (FloatDeclaration (ParseToken _ name)) = addEntry $ FloatVar name

instance Compilable ElseStatement where
    compile (ElseStatement decls stmts) = blockHelper decls stmts

instance Compilable Statement where
    compile (IfStatement _ decls stmts maybeElse) = do
        blockHelper decls stmts
        forM_ maybeElse compile

    compile (WhileStatement _ decls stmts) = blockHelper decls stmts

    compile _ = return ()

instance Compilable FuncBody where
    compile (FuncBody decls stmts) = do
        compileWriteHelper decls
        mapM_ compile stmts

instance Compilable FuncDeclaration where
    compile (FuncDeclaration _ (ParseToken _ name) decls funcBody) = do
        addTable name
        mapM_ compile decls
        compile funcBody

instance Compilable ProgramBody where
    compile (ProgramBody decls funcDecls) = do
        addTable "GLOBAL"
        compileWriteHelper decls

        mapM_ compile funcDecls

instance Compilable Program where
    compile (Program _ prgBody) = compile prgBody