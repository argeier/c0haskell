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
    = Declared Type
    | Initialized Type
    deriving (Show, Eq)

type Namespace = Map.Map String VariableStatus

data SemanticContext = SemanticContext
    { variables :: Namespace
    , inLoop :: Bool
    } deriving (Show)

type L1Semantic = StateT SemanticContext L1ExceptT

semanticFail' :: String -> L1Semantic a
semanticFail' = lift . semanticFail

semanticAnalysis :: AST -> L1ExceptT ()
semanticAnalysis ast = do
    ctx <- varStatusAnalysis ast
    evalStateT (checkReturns ast) ctx

varStatusAnalysis :: AST -> L1ExceptT SemanticContext
varStatusAnalysis (Block stmts _) = do
    finalCtx <- execStateT (checkStmtsUntilReturn stmts) (SemanticContext Map.empty False)
    return finalCtx

getNamespace :: L1Semantic Namespace
getNamespace = variables <$> get

putNamespace :: Namespace -> L1Semantic ()
putNamespace ns = do
    ctx <- get
    put ctx { variables = ns }

isInLoop :: L1Semantic Bool
isInLoop = inLoop <$> get

withLoop :: Bool -> L1Semantic a -> L1Semantic a
withLoop loopStatus action = do
    ctx <- get
    put ctx { inLoop = loopStatus }
    result <- action
    ctx' <- get
    put ctx' { inLoop = inLoop ctx }
    return result
    
withScope :: L1Semantic a -> L1Semantic a
withScope action = do
    originalNamespace <- getNamespace
    result <- action
    putNamespace originalNamespace
    return result

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
            case Map.lookup name ns of
                Just (Initialized varType) -> do
                    exprType <- checkExprType e
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

checkStmt (If cond thenStmt elseStmt _) = do
    condType <- checkExprType cond
    unless (condType == BoolType) $
        semanticFail' "If condition must be a boolean expression"
    
    case elseStmt of
        Just elseS -> do
            let thenAssigned = findAssignedVars thenStmt
            let elseAssigned = findAssignedVars elseS
            let bothAssigned = [v | v <- thenAssigned, v `elem` elseAssigned]
            checkStmt thenStmt
            checkStmt elseS
            ns <- getNamespace
            let updatedNs = foldr markInitialized ns bothAssigned
            putNamespace updatedNs
            
        Nothing -> do
            checkStmt thenStmt
  where
    markInitialized varName ns = 
        case Map.lookup varName ns of
            Just (Declared typ) -> Map.insert varName (Initialized typ) ns
            _ -> ns

checkStmt (While cond body _) = do
    condType <- checkExprType cond
    unless (condType == BoolType) $
        semanticFail' "While condition must be a boolean expression"
    withLoop True $ checkStmt body

checkStmt (For maybeInit maybeCond maybeStep body _) = do
    let needsScope = case maybeInit of
                       Just (Decl _ _ _) -> True
                       Just (Init _ _ _ _) -> True  
                       _ -> False
    
    let checkForLoop = do
          case maybeInit of
              Just initStmt -> checkStmt initStmt
              Nothing -> return ()

          case maybeCond of
              Just condExpr -> do
                  condType <- checkExprType condExpr
                  unless (condType == BoolType) $
                      semanticFail' "For loop condition must be a boolean expression"
              Nothing -> return ()

          withLoop True $ do
              checkStmt body
              
              case maybeStep of
                  Just (Decl _ _ pos) ->
                      semanticFail' $ "Declaration not allowed in for-loop step clause at: " ++ posPretty pos
                  Just (Init _ _ _ pos) ->
                      semanticFail' $ "Declaration not allowed in for-loop step clause at: " ++ posPretty pos
                  Just stepStmt ->
                      checkStmt stepStmt
                  Nothing ->
                      return ()
    
    if needsScope
        then withScope checkForLoop
        else checkForLoop

checkStmt (Break pos) = do
    inLoopNow <- isInLoop
    unless inLoopNow $
        semanticFail' $ "Break statement outside of loop at: " ++ posPretty pos

checkStmt (Continue pos) = do
    inLoopNow <- isInLoop
    unless inLoopNow $
        semanticFail' $ "Continue statement outside of loop at: " ++ posPretty pos

checkStmt (BlockStmt stmts _) = withScope $ mapM_ checkStmt stmts

checkExprType :: Expr -> L1Semantic Type
checkExprType (IntExpr str pos) = do
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
        Add -> arithmeticOp lhsType rhsType
        Mul -> arithmeticOp lhsType rhsType
        Sub -> arithmeticOp lhsType rhsType  
        Div -> arithmeticOp lhsType rhsType
        Mod -> arithmeticOp lhsType rhsType
        
        Lt -> comparisonOp lhsType rhsType
        Le -> comparisonOp lhsType rhsType
        Gt -> comparisonOp lhsType rhsType
        Ge -> comparisonOp lhsType rhsType
        
        Eq -> equalityOp lhsType rhsType
        Ne -> equalityOp lhsType rhsType
        
        And -> logicalOp lhsType rhsType
        Or -> logicalOp lhsType rhsType
        
        BitAnd -> bitwiseOp lhsType rhsType
        BitOr -> bitwiseOp lhsType rhsType
        BitXor -> bitwiseOp lhsType rhsType
        
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

stmtsReturn :: [Stmt] -> Bool
stmtsReturn [] = False
stmtsReturn (stmt:rest) = 
    if stmtReturns stmt 
    then True
    else stmtsReturn rest

stmtReturns :: Stmt -> Bool
stmtReturns (Decl _ _ _) = False
stmtReturns (Init _ _ _ _) = False  
stmtReturns (Asgn _ _ _ _) = False
stmtReturns (Ret _ _) = True
stmtReturns (If _ thenStmt elseStmt _) = 
    case elseStmt of
        Just elseS -> stmtReturns thenStmt && stmtReturns elseS
        Nothing -> False
stmtReturns (While _ _ _) = False
stmtReturns (For _ _ _ _ _) = False  
stmtReturns (Break _) = False
stmtReturns (Continue _) = False
stmtReturns (BlockStmt stmts _) = stmtsReturn stmts


findAssignedVars :: Stmt -> [String]
findAssignedVars (Asgn name Nothing _ _) = [name]
findAssignedVars (Asgn name (Just _) _ _) = [name]
findAssignedVars (If _ thenStmt elseStmt _) = 
    findAssignedVars thenStmt ++ maybe [] findAssignedVars elseStmt
findAssignedVars (While _ body _) = findAssignedVars body
findAssignedVars (For _ _ _ body _) = findAssignedVars body
findAssignedVars (BlockStmt stmts _) = concatMap findAssignedVars stmts
findAssignedVars _ = []


checkStmtsUntilReturn :: [Stmt] -> L1Semantic ()
checkStmtsUntilReturn [] = return ()
checkStmtsUntilReturn (stmt : rest) = do
    isReturn <- checkStmtReturns stmt
    if isReturn
        then return ()
        else checkStmtsUntilReturn rest

checkStmtReturns :: Stmt -> L1Semantic Bool
checkStmtReturns stmt@(Ret _ _) = do
    checkStmt stmt
    return True
checkStmtReturns stmt@(If _ thenStmt elseStmt _) = do
    checkStmt stmt
    let thenReturns = stmtReturns thenStmt
    let elseReturns = maybe False stmtReturns elseStmt
    return (thenReturns && elseReturns)
checkStmtReturns stmt@(BlockStmt stmts _) = do
    checkStmt stmt
    return (stmtsReturn stmts)
checkStmtReturns stmt = do
    checkStmt stmt
    return False