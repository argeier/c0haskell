module Compile.Semantic (
    semanticAnalysis,
) where

import Compile.AST (AST (..), Expr (..), Stmt (..), Type (..), Op (..), posPretty)
import Compile.Parser (parseNumber)
import Error (L1ExceptT, semanticFail)

import Control.Monad.State (
    MonadState (get, put),
    MonadTrans (lift),
    StateT,
    evalStateT,
    execStateT,
 )

import Control.Monad (unless, when)

import qualified Data.Map as Map

data VariableStatus
    = Declared Type  -- Now includes type information
    | Initialized Type
    deriving (Show, Eq)

-- You might want to keep track of some location information as well at some point
type Namespace = Map.Map String VariableStatus

-- Context for tracking loops and returns
data SemanticContext = SemanticContext
    { variables :: Namespace
    , inLoop :: Bool  -- Track if we're inside a loop (for break/continue)
    } deriving (Show)

type L1Semantic = StateT SemanticContext L1ExceptT

-- A little wrapper so we don't have to ($ lift) everywhere inside the StateT
semanticFail' :: String -> L1Semantic a
semanticFail' = lift . semanticFail

semanticAnalysis :: AST -> L1ExceptT ()
semanticAnalysis ast = do
    ctx <- varStatusAnalysis ast
    evalStateT (checkReturns ast) ctx

-- right now an AST is just a list of statements
varStatusAnalysis :: AST -> L1ExceptT SemanticContext
varStatusAnalysis (Block stmts _) = do
    finalCtx <- execStateT (mapM_ checkStmt stmts) (SemanticContext Map.empty False)
    return finalCtx

-- Get current namespace
getNamespace :: L1Semantic Namespace
getNamespace = variables <$> get

-- Update namespace
putNamespace :: Namespace -> L1Semantic ()
putNamespace ns = do
    ctx <- get
    put ctx { variables = ns }

-- Check if we're in a loop
isInLoop :: L1Semantic Bool
isInLoop = inLoop <$> get

-- Set loop context
withLoop :: Bool -> L1Semantic a -> L1Semantic a
withLoop loopStatus action = do
    ctx <- get
    put ctx { inLoop = loopStatus }
    result <- action
    ctx' <- get
    put ctx' { inLoop = inLoop ctx }  -- Restore original loop status
    return result

-- Extended statement checking for L2
checkStmt :: Stmt -> L1Semantic ()
checkStmt (Decl typ name pos) = do
    ns <- getNamespace
    let isDeclared = Map.member name ns
    when isDeclared $
        semanticFail' $
            "Variable " ++ name ++ " redeclared at: " ++ posPretty pos
    putNamespace $ Map.insert name (Declared typ) ns

checkStmt (Init typ name e pos) = do
    ns <- getNamespace
    let isDeclared = Map.member name ns
    when isDeclared $
        semanticFail' $
            "Variable " ++ name ++ " redeclared (initialized) at: " ++ posPretty pos
    -- Type check the expression
    exprType <- checkExprType e
    unless (typ == exprType) $
        semanticFail' $
            "Type mismatch in initialization of " ++ name ++ " at: " ++ posPretty pos
                ++ ". Expected " ++ show typ ++ " but got " ++ show exprType
    putNamespace $ Map.insert name (Initialized typ) ns

checkStmt (Asgn name op e pos) = do
    ns <- getNamespace
    case op of
        Nothing -> do
            -- Assignment with `=`
            -- If we assign to a variable with `=`, it has to be either declared or initialized
            case Map.lookup name ns of
                Nothing ->
                    semanticFail' $
                        "Trying to assign to undeclared variable "
                            ++ name
                            ++ " at: "
                            ++ posPretty pos
                Just (Declared varType) -> do
                    exprType <- checkExprType e
                    unless (varType == exprType) $
                        semanticFail' $
                            "Type mismatch in assignment to " ++ name ++ " at: " ++ posPretty pos
                                ++ ". Expected " ++ show varType ++ " but got " ++ show exprType
                    putNamespace $ Map.insert name (Initialized varType) ns
                Just (Initialized varType) -> do
                    exprType <- checkExprType e
                    unless (varType == exprType) $
                        semanticFail' $
                            "Type mismatch in assignment to " ++ name ++ " at: " ++ posPretty pos
                                ++ ". Expected " ++ show varType ++ " but got " ++ show exprType
        Just _ ->
            -- Assinging with op, e.g. `x += 3`,
            -- for this x needs to be initialized, not just declared
            case Map.lookup name ns of
                Just (Initialized varType) -> do
                    exprType <- checkExprType e
                    -- For compound assignment, both operands should have compatible types
                    unless (varType == exprType && varType == IntType) $
                        semanticFail' $
                            "Type mismatch in compound assignment to " ++ name ++ " at: " ++ posPretty pos
                                ++ ". Both operands must be int"
                _ ->
                    semanticFail' $
                        "Trying to assignOp to uninitialized variable "
                            ++ name
                            ++ " at: "
                            ++ posPretty pos

checkStmt (Ret e _) = do
    exprType <- checkExprType e
    unless (exprType == IntType) $
        semanticFail' "Return statement must return an int"

-- New L2 control flow statements
checkStmt (If cond thenStmt elseStmt _) = do
    condType <- checkExprType cond
    unless (condType == BoolType) $
        semanticFail' "If condition must be a boolean expression"
    checkStmt thenStmt
    case elseStmt of
        Just stmt -> checkStmt stmt
        Nothing -> return ()

checkStmt (While cond body _) = do
    condType <- checkExprType cond
    unless (condType == BoolType) $
        semanticFail' "While condition must be a boolean expression"
    withLoop True $ checkStmt body

checkStmt (For maybeInit maybeCond maybeStep body _) = do
    -- Check initializer
    case maybeInit of
        Just initStmt -> checkStmt initStmt
        Nothing -> return ()
    
    -- Check condition
    case maybeCond of
        Just condExpr -> do
            condType <- checkExprType condExpr
            unless (condType == BoolType) $
                semanticFail' "For loop condition must be a boolean expression"
        Nothing -> return ()
    
    -- Check step and body in loop context
    withLoop True $ do
        case maybeStep of
            Just stepStmt -> checkStmt stepStmt
            Nothing -> return ()
        checkStmt body

checkStmt (Break pos) = do
    inLoopNow <- isInLoop
    unless inLoopNow $
        semanticFail' $ "Break statement outside of loop at: " ++ posPretty pos

checkStmt (Continue pos) = do
    inLoopNow <- isInLoop
    unless inLoopNow $
        semanticFail' $ "Continue statement outside of loop at: " ++ posPretty pos

checkStmt (BlockStmt stmts _) = mapM_ checkStmt stmts

-- Type checking for expressions
checkExprType :: Expr -> L1Semantic Type
checkExprType (IntExpr str pos) = do
    -- Check that literals are in bounds
    let res = parseNumber str
    case res of
        Left e -> do
            semanticFail' $ "Error in " ++ posPretty pos ++ e
        Right _ -> return IntType

checkExprType (BoolExpr _ _) = return BoolType

checkExprType (Ident name pos) = do
    ns <- getNamespace
    case Map.lookup name ns of
        Just (Initialized typ) -> return typ
        Just (Declared _) ->
            semanticFail' $
                "Variable "
                    ++ name
                    ++ " used without initialization at: "
                    ++ posPretty pos
        Nothing ->
            semanticFail' $
                "Undeclared variable "
                    ++ name
                    ++ " used at: "
                    ++ posPretty pos

checkExprType (UnExpr op e) = do
    exprType <- checkExprType e
    case op of
        Neg -> do
            unless (exprType == IntType) $
                semanticFail' "Unary minus can only be applied to int"
            return IntType
        Not -> do
            unless (exprType == BoolType) $
                semanticFail' "Logical not can only be applied to bool"
            return BoolType
        BitNot -> do
            unless (exprType == IntType) $
                semanticFail' "Bitwise not can only be applied to int"
            return IntType
        _ -> semanticFail' $ "Unsupported unary operator: " ++ show op

checkExprType (BinExpr op lhs rhs) = do
    lhsType <- checkExprType lhs
    rhsType <- checkExprType rhs
    case op of
        -- Arithmetic operators
        Add -> arithmeticOp lhsType rhsType
        Mul -> arithmeticOp lhsType rhsType
        Sub -> arithmeticOp lhsType rhsType  
        Div -> arithmeticOp lhsType rhsType
        Mod -> arithmeticOp lhsType rhsType
        
        -- Comparison operators
        Lt -> comparisonOp lhsType rhsType
        Le -> comparisonOp lhsType rhsType
        Gt -> comparisonOp lhsType rhsType
        Ge -> comparisonOp lhsType rhsType
        
        -- Equality operators (polymorphic)
        Eq -> equalityOp lhsType rhsType
        Ne -> equalityOp lhsType rhsType
        
        -- Logical operators
        And -> logicalOp lhsType rhsType
        Or -> logicalOp lhsType rhsType
        
        -- Bitwise operators
        BitAnd -> bitwiseOp lhsType rhsType
        BitOr -> bitwiseOp lhsType rhsType
        BitXor -> bitwiseOp lhsType rhsType
        
        -- Shift operators
        Shl -> shiftOp lhsType rhsType
        Shr -> shiftOp lhsType rhsType
        
        _ -> semanticFail' $ "Unsupported binary operator: " ++ show op
  where
    arithmeticOp lType rType = do
        unless (lType == IntType && rType == IntType) $
            semanticFail' $ "Arithmetic operator " ++ show op ++ " requires int operands"
        return IntType
    
    comparisonOp lType rType = do
        unless (lType == IntType && rType == IntType) $
            semanticFail' $ "Comparison operator " ++ show op ++ " requires int operands"
        return BoolType
    
    equalityOp lType rType = do
        unless (lType == rType) $
            semanticFail' $ "Equality operator " ++ show op ++ " requires operands of the same type"
        return BoolType
    
    logicalOp lType rType = do
        unless (lType == BoolType && rType == BoolType) $
            semanticFail' $ "Logical operator " ++ show op ++ " requires bool operands"
        return BoolType
    
    bitwiseOp lType rType = do
        unless (lType == IntType && rType == IntType) $
            semanticFail' $ "Bitwise operator " ++ show op ++ " requires int operands"
        return IntType
    
    shiftOp lType rType = do
        unless (lType == IntType && rType == IntType) $
            semanticFail' $ "Shift operator " ++ show op ++ " requires int operands"
        return IntType

checkExprType (TernaryExpr cond thenExpr elseExpr _) = do
    condType <- checkExprType cond
    unless (condType == BoolType) $
        semanticFail' "Ternary operator condition must be bool"
    
    thenType <- checkExprType thenExpr
    elseType <- checkExprType elseExpr
    unless (thenType == elseType) $
        semanticFail' "Ternary operator branches must have the same type"
    
    return thenType

checkReturns :: AST -> L1Semantic ()
checkReturns (Block stmts _) = do
    let returns = stmtsReturn stmts
    unless returns $ semanticFail' "Program does not return"

-- Check if a list of statements returns
stmtsReturn :: [Stmt] -> Bool
stmtsReturn [] = False
stmtsReturn (stmt:rest) = 
    if stmtReturns stmt 
    then True  -- This statement returns, so the sequence returns
    else stmtsReturn rest  -- Check the rest

-- Check if a single statement returns (following L2 spec)
stmtReturns :: Stmt -> Bool
stmtReturns (Decl _ _ _) = False
stmtReturns (Init _ _ _ _) = False  
stmtReturns (Asgn _ _ _ _) = False
stmtReturns (Ret _ _) = True
stmtReturns (If _ thenStmt elseStmt _) = 
    case elseStmt of
        Just elseS -> stmtReturns thenStmt && stmtReturns elseS  -- Both branches must return
        Nothing -> False  -- If without else doesn't guarantee return
stmtReturns (While _ _ _) = False  -- While loops don't guarantee return (might not execute)
stmtReturns (For _ _ _ _ _) = False  -- For loops don't guarantee return  
stmtReturns (Break _) = False
stmtReturns (Continue _) = False
stmtReturns (BlockStmt stmts _) = stmtsReturn stmts