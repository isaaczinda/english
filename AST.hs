module AST where

data BinOp = Plus | Times | Minus | Over
    deriving (Eq, Show)

data Literal =
        FloatLiteral Float |
        -- [VarDec] declares variables used in where statement
        FuncLiteral String Expr [Assignment]
    deriving (Eq, Show)

data Assignment = Assignment String Expr
    deriving (Eq, Show)

data Expr =
        Math Expr BinOp Expr |
        Var String |
        Literal Literal |

        -- condition, true, false
        If Expr Expr Expr |
        Apply Expr Expr
    deriving (Eq, Show)

data Statement =
        Assign Assignment |
        Print Expr
    deriving (Eq, Show)
