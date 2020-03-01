module Tables (
    entry, symbolTableTree
) where

import Compiler
import Types
import Data.List

class SymbolTable a where
    symbolTableTree :: a -> String

instance SymbolTable Declaration where
    symbolTableTree (StringDeclaration (ParseToken _ name) (ParseToken _ val)) = 
        "name " ++ name ++ " type STRING value " ++ val
    symbolTableTree (IntDeclaration (ParseToken _ name)) =
        "name " ++ name ++ " type INT"
    symbolTableTree (FloatDeclaration (ParseToken _ name)) =
        "name " ++ name ++ " type FLOAT"

instance SymbolTable Program where
    symbolTableTree (Program _ pgmBody) = symbolTableTree pgmBody

instance SymbolTable ProgramBody where
    symbolTableTree (ProgramBody decs funcs) = 
        "Symbol table GLOBAL\n" ++ 
        (intercalate "\n" $ map symbolTableTree decs) ++ "\n" ++ 
        (intercalate "\n" $ map symbolTableTree funcs)

instance SymbolTable FuncDeclaration where
    symbolTableTree (FuncDeclaration _ (ParseToken _ name) decs body) =
        "Symbol table " ++ name ++ "\n" ++ 
        (intercalate "\n" $ symbolTableTree body)

instance SymbolTable FuncBody where
    symbolTableTree (FuncBody)