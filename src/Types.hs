module Types where

data TokenType = IDENTIFIER | INTLITERAL | FLOATLITERAL | STRINGLITERAL | COMMENT | KEYWORD | OPERATOR deriving (Show)
data Token a = Token a String 
newtype Id = Token IDENTIFIER
newtype Keyword = Token KEYWORD
newtype Operator = Token OPERATOR
newtype Str = Token STRINGLITERAL
newtype IntLit = Token INTLITERAL
newtype FloatLit = Token FLOATLITERAL
newtype StringLit = Token STRINGLITERAL

data VarType = VarTypeFloat Keyword | VarTypeInt Keyword
data AnyType = AnyTypeVarType VarType | AnyTypeVoid Keyword

newtype Str = Str StringLit
data StringDecl = StringDecl Keyword Id Operator Str Operator Decl
data IdTail = IdTail Operator Id IdTail | IdTailEmpty
data IdList = IdList Id IdTail
data VarDecl = VarDecl VarType IdList Decl
data Decl = DeclStringDecl StringDecl | DeclVarDecl VarDecl | DeclEmpty

data ParamDecl = ParamDecl VarType Id
data ParamDeclTail = ParamDeclTail Operator ParamDecl ParamDeclTail | ParamDeclTailEmpty
data ParamDeclList = ParamDeclList ParamDecl ParamDeclTail | ParamDeclListEmpty

data AddOp = AddOpPlus Operator | AddOpMinus Operator
data MulOp = MulOpTimes Operator | MulOpDiv Operator
data Primary = PrimaryExpr Operator Expr Operator | PrimaryId Id | PrimaryInt IntLit | PrimaryFloat FloatLit
data ExprListTail = ExprListTail Operator Expr ExprListTail | ExprListTailEmpty
data ExprList = ExprList Expr ExprListTail | ExprListEmpty
data CallExpr = CallExpr Id Operator ExprList Operator
data PostfixExpr = PostfixExprPrimary Primary | PostfixExprCallExpr CallExpr
data FactorPrefix = FactorPrefix FactorPrefix PostfixExpr MulOp | FactorPrefixEmpty
data Factor = Factor FactorPrefix PostfixExpr
data ExprPrefix = ExprPrefix ExprPrefix Factor AddOp | ExprPrefixEmpty
data Expr = Expr ExprPrefix Factor

data CompOp = CompOpLT Operator | CompOpGT Operator | CompOpEQ Operator | CompOpNE Operator | CompOpLE Operator | CompOpGE Operator
data Cond = Cond Expr CompOp Expr
data ElsePart = ElsePart Operator Decl StmtList | ElsePartEmpty
data IfStmt = IfStmt Keyword Operator Cond Operator Decl StmtList ElsePart Keyword
data WhileStmt = WhileStmt Keyword Operator Cond Operator Decl StmtList Operator

data AssignExpr = AssignExpr Id Operator Expr
data AssignStmt = AssignStmt AssignExpr Operator
data ReadStmt = ReadStmt Keyword Operator IdList Operator Operator
data WriteStmt = WriteStmt Keyword Operator IdList Operator Operator
data ReturnStmt = ReturnStmt Keyword Expr Operator

data BaseStmt = BaseStmtAssignStmt AssignStmt | BaseStmtReadStmt ReadStmt | BaseStmtWriteStmt WriteStmt | BaseStmtReturnStmt ReturnStmt 
data Stmt = StmtBaseStmt BaseStmt | StmtIfStmt IfStmt | StmtWhileStmt WhileStmt
data StmtList = StmtList Stmt StmtList | StmtListEmpty

data FuncBody = FuncBody Decl StmtList
data FuncDecl = FuncDecl Keyword AnyType Id Operator ParamDeclList Operator Keyword FuncBody Keyword
data FuncDeclarations = FuncDeclarations FuncDecl FuncDeclarations | FuncDeclarationsEmpty

data PgmBody = PgmBody Decl FuncDeclarations
data Program = Program Keyword Id Keyword PgmBody Keyword