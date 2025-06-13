module Compile.AST (
    AST (..),
    Stmt (..),
    Expr (..),
    Op (..),
    Type (..),
    showAsgnOp,
    posPretty,
) where

import Data.List (intercalate)
import Text.Megaparsec (SourcePos, sourcePosPretty)

data Type
    = IntType
    | BoolType
    deriving (Show, Eq)

data AST
    = Block [Stmt] SourcePos

data Stmt
    = Decl Type String SourcePos
    | Init Type String Expr SourcePos
    | Asgn String AsgnOp Expr SourcePos
    | Ret Expr SourcePos
    | If Expr Stmt (Maybe Stmt) SourcePos
    | While Expr Stmt SourcePos
    | For (Maybe Stmt) (Maybe Expr) (Maybe Stmt) Stmt SourcePos
    | Break SourcePos
    | Continue SourcePos
    | BlockStmt [Stmt] SourcePos

data Expr
    = IntExpr String SourcePos
    | BoolExpr Bool SourcePos
    | Ident String SourcePos
    | UnExpr Op Expr
    | BinExpr Op Expr Expr
    | TernaryExpr Expr Expr Expr SourcePos

type AsgnOp = Maybe Op

data Op
    = Mul | Add | Sub | Div | Neg | Mod | Nop
    | Lt | Le | Gt | Ge | Eq | Ne
    | And | Or | Not
    | BitAnd | BitOr | BitXor | BitNot
    | Shl | Shr
    deriving (Eq)

posPretty :: SourcePos -> String
posPretty = sourcePosPretty

instance Show AST where
    show (Block stmts _) =
        "Block: {\n" ++ intercalate "\n" (map show stmts) ++ "\n}"

instance Show Stmt where
    show (Decl typ name _) = "Decl: " ++ show typ ++ " " ++ name
    show (Init typ name e _) = "Init: " ++ show typ ++ " " ++ name ++ " = " ++ show e
    show (Asgn name op e _) =
        "Assign: " ++ name ++ " " ++ show' op ++ " " ++ show e
      where
        show' (Just o) = show o ++ "="
        show' Nothing = "="
    show (Ret e _) = "Return: " ++ show e
    show (If cond thenStmt elseStmt _) = 
        "If: " ++ show cond ++ " then " ++ show thenStmt ++ 
        maybe "" (\e -> " else " ++ show e) elseStmt
    show (While cond body _) = "While: " ++ show cond ++ " do " ++ show body
    show (For forInit cond step body _) = 
        "For: " ++ show forInit ++ "; " ++ show cond ++ "; " ++ show step ++ " do " ++ show body
    show (Break _) = "Break"
    show (Continue _) = "Continue"
    show (BlockStmt stmts _) = "Block: {\n" ++ intercalate "\n" (map show stmts) ++ "\n}"

instance Show Expr where
    show (IntExpr i _) = i
    show (BoolExpr b _) = if b then "true" else "false"
    show (Ident name _) = name
    show (UnExpr op e) = "(" ++ show op ++ " " ++ show e ++ ")"
    show (BinExpr op lhs rhs) =
        "(" ++ show lhs ++ " " ++ show op ++ " " ++ show rhs ++ ")"
    show (TernaryExpr cond thenExpr elseExpr _) =
        "(" ++ show cond ++ " ? " ++ show thenExpr ++ " : " ++ show elseExpr ++ ")"

instance Show Op where
    show Mul = "*"
    show Add = "+"
    show Sub = "-"
    show Div = "/"
    show Neg = "-"
    show Mod = "%"
    show Nop = "[nop]"
    show Lt = "<"
    show Le = "<="
    show Gt = ">"
    show Ge = ">="
    show Eq = "=="
    show Ne = "!="
    show And = "&&"
    show Or = "||"
    show Not = "!"
    show BitAnd = "&"
    show BitOr = "|"
    show BitXor = "^"
    show BitNot = "~"
    show Shl = "<<"
    show Shr = ">>"

showAsgnOp :: AsgnOp -> String
showAsgnOp (Just op) = " " ++ show op ++ "= "
showAsgnOp _ = " = "