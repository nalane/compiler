{-# LANGUAGE TemplateHaskell #-}

module Compiler (
    runCompiler
) where

import Control.Lens
import Control.Monad
import Data.List
import Data.Char
import qualified Data.Map as Map

import Types
import IR

-- Describes a variable
data TableEntry = IntVar String | FloatVar String | StringVar String String
instance Show TableEntry where
    show (IntVar name) = "name " ++ name ++ " type INT"
    show (FloatVar name) = "name " ++ name ++ " type FLOAT"
    show (StringVar name val) = "name " ++ name ++ " type STRING value " ++ val 

entryName :: TableEntry -> String
entryName (IntVar n) = n
entryName (FloatVar n) = n
entryName (StringVar n _) = n

-- Describes a variable table for a context
data VarTable = VarTable String [TableEntry]
instance Show VarTable where
    show (VarTable name []) = "Symbol table " ++ name
    show (VarTable name entries) = "Symbol table " ++ name ++ "\n" ++ intercalate "\n" (map show entries)

-- State of the compiler
data CompilerState = CompilerState {
    _tableStack :: [VarTable],
    _tableCount :: Int,
    _tempCount :: Int,
    _ir :: [String],
    _output :: String,
    _constants :: Map.Map String String
}
makeLenses ''CompilerState

initState = CompilerState [] 0 0 [] "" Map.empty



-- Compiler monad
newtype Compiler a = Compiler (CompilerState -> Either String (CompilerState, a))

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

-- Given a parse tree, runs the compiler
runCompiler :: Program -> String
runCompiler p =
    let (Compiler c) = compile p
    in case c initState of
        Left e -> e
        Right (s, _) -> s^.output



-- Enter into an error state
compilerError :: String -> Compiler a
compilerError msg = Compiler $ \_ -> Left msg

-- Get a value from the copmpiler state
getState :: ALens' CompilerState a -> Compiler a
getState l = Compiler $ \s -> Right (s, s^#l)

-- Set a value for the compiler state
setState :: ALens' CompilerState a -> a -> Compiler ()
setState l v = Compiler $ \s -> Right (storing l v s, ())



-- Add a new variable to the table on the top of the stack
addEntry :: TableEntry -> Compiler ()
addEntry newVar = do
    tables <- getState tableStack
    let (VarTable name top) = head tables
    let newName = entryName newVar
    let failed = newName `elem` map entryName top

    let newTable = VarTable name (top ++ [newVar])
    let newStack = newTable : tail tables

    code <- case newVar of
        IntVar name -> return ("VAR " ++ name)
        FloatVar name -> return ("VAR " ++ name)
        StringVar name val -> return ("STR " ++ name ++ " " ++ val)

    if failed
        then compilerError ("DECLARATION ERROR " ++ newName)
        else do
            setState tableStack newStack
            addIr code

-- Get a variable's entry in the symbol table.
-- If it's not there, errors
getEntry :: String -> Compiler TableEntry
getEntry name = getState tableStack >>= worker where
    worker [] = compilerError ("VARIABLE " ++ name ++ " NOT FOUND")
    worker (top:rest) = do
        let (VarTable _ entries) = top
        let matches = filter (\e -> entryName e == name) entries
        if null matches
            then worker rest
            else return $ head matches

-- Push a new variable table onto the stack
addTable :: String -> Compiler ()
addTable name = do
    tables <- getState tableStack
    setState tableStack (VarTable name [] : tables)

-- Pop a variable table from the stack
removeTable :: Compiler ()
removeTable = do
    tables <- getState tableStack
    setState tableStack $ tail tables

-- Create a new temporary variable
newTemp :: Compiler String
newTemp = do
    count <- getState tempCount
    setState tempCount (count + 1)
    let name = "T" ++ (show count)
    return name

-- Creates a new block for naming the vartable for a while loop or if statement
addBlock :: Compiler ()
addBlock = do
    count <- getState tableCount
    setState tableCount (count + 1)

-- Add an IR instruction
addIr :: String -> Compiler ()
addIr newIr = do
    oldIr <- getState ir
    setState ir (oldIr ++ [newIr])

-- Add an assembly instruction
addOutput :: String -> Compiler ()
addOutput out = do
    oldOutput <- getState output
    setState output (oldOutput ++ "\n" ++ out)



-- Refactoring of the new block code
blockHelper :: [Declaration] -> [Statement] -> Compiler ()
blockHelper decls stmts = do
    addBlock
    count <- getState tableCount
    addTable ("BLOCK " ++ show count)
    mapM_ compile decls

    mapM_ compile stmts

    removeTable

-- Creates a new factor
factHelper :: Factor -> Compiler TableEntry
factHelper (FactLeaf pf) =
    case pf of
        (ParenExpr expr) -> exprHelper expr
        (FloatLiteral (ParseToken _ val)) -> do
            tmpName <- newTemp
            addIr $ unwords ["STOREF", val, tmpName]
            return $ FloatVar tmpName
        (IntLiteral (ParseToken _ val)) -> do
            tmpName <- newTemp
            addIr $ unwords ["STOREI", val, tmpName]
            return $ IntVar tmpName
        (Identifier (ParseToken _ name)) -> getEntry name
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
    addIr $ unwords [fullOp, entryName lEntry, entryName rEntry, tmp]

    case t of
        "I" -> return $ IntVar tmp
        "F" -> return $ FloatVar tmp

-- Creates a new expression
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
    addIr $ unwords [fullOp, entryName lEntry, entryName rEntry, tmp]

    case t of
        "I" -> return $ IntVar tmp
        "F" -> return $ FloatVar tmp



-- Defines compile function for applicable elements of the parse tree
class Compilable a where
    compile :: a -> Compiler ()

-- Compile a variable declaration
instance Compilable Declaration where
    compile (StringDeclaration (ParseToken _ name) (ParseToken _ val)) = addEntry $ StringVar name val
    compile (IntDeclaration (ParseToken _ name)) = addEntry $ IntVar name
    compile (FloatDeclaration (ParseToken _ name)) = addEntry $ FloatVar name

-- Compile an else statement
instance Compilable ElseStatement where
    compile (ElseStatement decls stmts) = blockHelper decls stmts

-- Compile various statements
instance Compilable Statement where
    -- Assignments take the variable on the right and store them to the left
    compile (AssignStatement (ParseToken t lhsName) expr) = do
        res <- exprHelper expr
        let resName = entryName res
        let firstChar = head $ entryName res

        -- If the stored item is a number or a temp, just store it
        if isDigit firstChar || firstChar == '.' || (head resName == 'T' && all isDigit (tail resName)) then
            case res of
                (IntVar tmpName) -> addIr ("STOREI " ++ tmpName ++ " " ++ lhsName)
                (FloatVar tmpName) -> addIr ("STOREF " ++ tmpName ++ " " ++ lhsName)

        -- Otherwise, we're copying a variable to another one; create an intermediate.
        else do
            tmpName <- newTemp
            case res of
                (IntVar resName) -> do
                    addIr ("STOREI " ++ resName ++ " " ++ tmpName)
                    addIr ("STOREI " ++ tmpName ++ " " ++ lhsName)
                (FloatVar resName) -> do
                    addIr ("STOREF " ++ resName ++ " " ++ tmpName)
                    addIr ("STOREF " ++ tmpName ++ " " ++ lhsName)

    -- For each item in the read, generate a read instruction
    compile (ReadStatement tokens) =
        forM_ tokens $ \(ParseToken _ name) -> do
            token <- getEntry name
            case token of
                IntVar _ -> addIr ("READI " ++ name)
                FloatVar _ -> addIr ("READF " ++ name)

    -- For each item in the write, generate a write instruction
    compile (WriteStatement tokens) =
        forM_ tokens $ \(ParseToken _ name) -> do
            token <- getEntry name
            case token of
                IntVar _ -> addIr ("WRITEI " ++ name)
                FloatVar _ -> addIr ("WRITEF " ++ name)
                StringVar _ _ -> addIr ("WRITES " ++ name)

    -- If statements are handled with the block helper. If there is an else statement, handle that.
    compile (IfStatement _ decls stmts maybeElse) = do
        blockHelper decls stmts
        forM_ maybeElse compile

    -- While statements are simple blocks.
    compile (WhileStatement _ decls stmts) = blockHelper decls stmts

    compile _ = return ()

-- Function bodies are compiled by compiling the variable declarations, then the statements
instance Compilable FuncBody where
    compile (FuncBody decls stmts) = do
        mapM_ compile decls
        mapM_ compile stmts

-- Function declarations create a new var table and add the arguments to it.
-- Remove the table when done
instance Compilable FuncDeclaration where
    compile (FuncDeclaration _ (ParseToken _ name) decls funcBody) = do
        addTable name
        mapM_ compile decls
        compile funcBody
        removeTable

-- Create a global var table and add the declarations to it
-- Then, compile the functions
instance Compilable ProgramBody where
    compile (ProgramBody decls funcDecls) = do
        addTable "GLOBAL"
        mapM_ compile decls

        mapM_ compile funcDecls

instance Compilable Program where
    compile (Program _ prgBody) = do
        -- Generate the IR from the code
        compile prgBody
        intermediate <- getState ir

        -- Print the IR for debugging
        forM_ intermediate (\code -> addOutput (';':code))

        -- Optimize the IR and convert it to assembly
        forM_ intermediate $ \code -> do
            cs <- getState constants

            if "STORE" `isPrefixOf` code then do
                let [_, l, r] = words code

                -- Constants are just added to the constants table
                if head l == '.' || (isDigit $ head l) then setState constants $ Map.insert r l cs

                -- If the origin is recognized in the constants table, make the result a const too
                -- Otherwise, no optimization
                else case Map.lookup l cs of
                    (Just val) -> setState constants $ Map.insert r val cs
                    Nothing -> mapM_ addOutput $ translateIR code

            else if "WRITE" `isPrefixOf` code then do
                let [inst, var] = words code

                -- If the var is a constant, move it to a memory location before writing
                case Map.lookup var cs of
                    (Just val) -> do
                        storeType <- case last inst of
                            'I' -> return "STOREI"
                            'F' -> return "STOREF"
                        mapM_ addOutput $ translateIR $ unwords [storeType, val, var]
                    Nothing -> return ()
                    
                mapM_ addOutput $ translateIR code

            -- If a variable has been read, it is no longer constant
            else if "READ" `isPrefixOf` code then do
                let [_, var] = words code
                setState constants $ Map.delete var cs
                mapM_ addOutput $ translateIR code

            -- New variables and strings can't be optimized
            else if "VAR" `isPrefixOf` code || ("STR" `isPrefixOf` code) then mapM_ addOutput $ translateIR code

            -- These are all the math operations
            else do
                let [inst, lhs, rhs, res] = words code
                case [Map.lookup lhs cs, Map.lookup rhs cs] of
                    -- If the inputs are constants, perform math op and make result constant
                    [(Just val1), (Just val2)] -> do
                        -- Float op case
                        if last inst == 'F' then do
                            let v1 = read val1 :: Float
                            let v2 = read val2 :: Float
                            op <- case init inst of
                                "ADD" -> return (+)
                                "SUB" -> return (-)
                                "MULT" -> return (*)
                                "DIV" -> return (/)
                            setState constants $ Map.insert res (show (v1 `op` v2)) cs
                        
                        -- Int op case
                        else if last inst == 'I' then do 
                            let v1 = read val1 :: Int
                            let v2 = read val2 :: Int
                            op <- case init inst of
                                "ADD" -> return (+)
                                "SUB" -> return (-)
                                "MULT" -> return (*)
                                "DIV" -> return div
                            setState constants $ Map.insert res (show (v1 `op` v2)) cs

                        -- Neither float or int, error
                        else compilerError ("Unknown instruction: " ++ code)

                    -- If one of the ops is a constant, put its val in the instruction
                    [(Just val1), Nothing] -> mapM_ addOutput $ translateIR $ unwords [inst, val1, rhs, res]
                    [Nothing, (Just val2)] -> mapM_ addOutput $ translateIR $ unwords [inst, lhs, val2, res]

                    -- No optimization
                    [Nothing, Nothing] -> mapM_ addOutput $ translateIR code

        addOutput "sys halt"