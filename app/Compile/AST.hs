module Compile.AST (
    AST (..),
    Stmt (..),
    Expr (..),
    Op (..),
    showAsgnOp,
    posPretty,
    -- remove prettyPrintAST from export list
) where

import Text.Megaparsec

data AST
    = Block [Stmt] SourcePos

data Stmt
    = Decl String SourcePos
    | Init String Expr SourcePos
    | Asgn String AsgnOp Expr SourcePos
    | Ret Expr SourcePos

data Expr
    = IntExpr String SourcePos
    | Ident String SourcePos
    | UnExpr Op Expr
    | BinExpr Op Expr Expr

-- Nothing means =, Just is for +=, %=, ...
type AsgnOp = Maybe Op

data Op
    = Mul
    | Add
    | Sub
    | Div
    | Neg
    | Mod
    | Nop

-- re-exported for convenience
posPretty :: SourcePos -> String
posPretty = sourcePosPretty

-- Enhanced Show instances for prettier output
instance Show AST where
    show (Block stmts _) =
        "Program {\n" ++ indent (concatMap show stmts) ++ "}"

instance Show Stmt where
    show (Decl name pos) =
        "Declaration: int "
            ++ name
            ++ ";\n"
            ++ "  (at "
            ++ posPretty pos
            ++ ")\n"
    show (Init name expr pos) =
        "Initialization: int "
            ++ name
            ++ " = "
            ++ show expr
            ++ ";\n"
            ++ "  (at "
            ++ posPretty pos
            ++ ")\n"
    show (Asgn name op expr pos) =
        "Assignment: "
            ++ name
            ++ showAsgnOp op
            ++ show expr
            ++ ";\n"
            ++ "  (at "
            ++ posPretty pos
            ++ ")\n"
    show (Ret expr pos) =
        "Return: return "
            ++ show expr
            ++ ";\n"
            ++ "  (at "
            ++ posPretty pos
            ++ ")\n"

instance Show Expr where
    show (IntExpr val _) = val
    show (Ident name _) = name
    show (UnExpr op expr) = "(" ++ show op ++ show expr ++ ")"
    show (BinExpr op expr1 expr2) =
        "(" ++ show expr1 ++ " " ++ show op ++ " " ++ show expr2 ++ ")"

instance Show Op where
    show Mul = "*"
    show Add = "+"
    show Sub = "-"
    show Div = "/"
    show Neg = "-"
    show Mod = "%"
    show Nop = "[nop]"

showAsgnOp :: AsgnOp -> String
showAsgnOp (Just op) = " " ++ show op ++ "= "
showAsgnOp Nothing = " = "

-- Helper function to indent a block of text
indent :: String -> String
indent = unlines . map ("  " ++) . lines
