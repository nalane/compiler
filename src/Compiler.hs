{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

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



newtype Compiler a = Compiler (CompilerState -> (CompilerState, a))

runCompiler :: Program -> String
runCompiler p =
    let (Compiler c) = compile p
        (s, _) = c initState
    in s^.output

instance Functor Compiler where
    fmap f (Compiler m) = Compiler $ \s -> 
        let (newS, val) = m s
        in (newS, f val)
        
instance Applicative Compiler where
    pure v = Compiler (, v)
    (<*>) (Compiler f) (Compiler v) = Compiler $ \s ->
        let (s1, func) = f s
            (s2, val) = v s1
        in (s2, func val)

instance Monad Compiler where
    (>>=) (Compiler m) f = Compiler $ \s ->
        let (newS, val) = m s
            (Compiler newM) = f val
        in newM newS

getState :: ALens' CompilerState a -> Compiler a
getState l = Compiler $ \s -> (s, s^#l)

addEntry :: TableEntry -> Compiler ()
addEntry newVar = Compiler $ \s ->
    let tables = s^.tableStack
        (VarTable name top) = head tables
        newTable = VarTable name (top ++ [newVar])
        newStack = newTable : tail tables
    in (set tableStack newStack s, ())

addTable :: String -> Compiler ()
addTable name = Compiler $ \s ->
    let tables = s^.tableStack
        newStack = VarTable name [] : tables
    in (set tableStack newStack s, ())

addBlock :: Compiler ()
addBlock = Compiler $ \s ->
    (over tableCount (+1) s, ())

addOutput :: String -> Compiler ()
addOutput out = Compiler $ \s ->
    let oldOutput = s^.output
        newOutput = oldOutput ++ out ++ "\n\n"
    in (set output newOutput s, ())

writeTable :: Compiler ()
writeTable = do
    tables <- getState tableStack
    addOutput $ show $ head tables



class Compilable a where
    compile :: a -> Compiler ()

instance Compilable Declaration where
    compile (StringDeclaration (ParseToken _ name) (ParseToken _ val)) = addEntry $ StringVar name val
    compile (IntDeclaration (ParseToken _ name)) = addEntry $ IntVar name
    compile (FloatDeclaration (ParseToken _ name)) = addEntry $ FloatVar name

instance Compilable ElseStatement where
    compile (ElseStatement decls stmts) = do
        addBlock
        count <- getState tableCount
        addTable ("BLOCK " ++ show count)
        mapM_ compile decls
        writeTable

        mapM_ compile stmts

instance Compilable Statement where
    compile (IfStatement _ decls stmts maybeElse) = do
        addBlock
        count <- getState tableCount
        addTable ("BLOCK " ++ show count)
        mapM_ compile decls
        writeTable

        mapM_ compile stmts
        forM_ maybeElse compile

    compile (WhileStatement _ decls stmts) = do
        addBlock
        count <- getState tableCount
        addTable ("BLOCK " ++ show count)
        mapM_ compile decls
        writeTable

        mapM_ compile stmts

    compile _ = return ()

instance Compilable FuncBody where
    compile (FuncBody decls stmts) = do
        mapM_ compile decls
        writeTable
        mapM_ compile stmts

instance Compilable FuncDeclaration where
    compile (FuncDeclaration _ (ParseToken _ name) decls funcBody) = do
        addTable name
        mapM_ compile decls
        compile funcBody

instance Compilable ProgramBody where
    compile (ProgramBody decls funcDecls) = do
        addTable "GLOBAL"
        mapM_ compile decls
        writeTable

        mapM_ compile funcDecls

instance Compilable Program where
    compile (Program _ prgBody) = compile prgBody