module Types where

import Data.List
import Data.Maybe

-- Define tokens and their types
data TokenType = 
    IDENTIFIER | INTLITERAL | FLOATLITERAL | 
    STRINGLITERAL | KEYWORD | OPERATOR deriving (Show, Eq)
data ParseToken = ParseToken TokenType String deriving (Eq)

-- Define abstract syntax tree (root is Program)
data Declaration = 
    StringDeclaration ParseToken ParseToken | 
    IntDeclaration ParseToken | 
    FloatDeclaration ParseToken
data PostFix = 
    CallExpr ParseToken [Expression] |
    ParenExpr Expression | 
    Identifier ParseToken |
    FloatLiteral ParseToken |
    IntLiteral ParseToken
data MulOp = Times | Div
data Factor = FactLeaf PostFix | FactNode Factor MulOp Factor
data AddOp = Plus | Minus
data Expression = ExprLeaf Factor | ExprNode Expression AddOp Expression
data CompOp = LessEqual | NotEqual | GreatEqual | Less | Great | Equal
data Condition = Condition Expression CompOp Expression
data ElseStatement = ElseStatement [Declaration] [Statement]
data Statement = 
    AssignStatement ParseToken Expression |
    ReadStatement [ParseToken] |
    WriteStatement [ParseToken] |
    ReturnStatement Expression |
    IfStatement Condition [Declaration] [Statement] (Maybe ElseStatement) |
    WhileStatement Condition [Declaration] [Statement]
data FuncBody = FuncBody [Declaration] [Statement]
data FuncDeclaration = FuncDeclaration ParseToken ParseToken [Declaration] FuncBody
data ProgramBody = ProgramBody [Declaration] [FuncDeclaration]
data Program = Program ParseToken ProgramBody