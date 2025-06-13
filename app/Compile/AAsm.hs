module Compile.AAsm (
    codeGen,
) where

import Compile.AST (AST (..), Expr (..), Op (..), Stmt (..))
import Compile.Parser (parseNumber)

import Control.Monad.State (State, execState, get, gets, modify, put)
import Control.Monad (unless)
import Data.Bits ((.&.), (.|.), xor, complement, shiftL, shiftR)
import qualified Data.Map as Map

type Register = Integer
type VarName = String
type Label = String

type AAsmAlloc = Map.Map VarName Register
type ConstEnv = Map.Map VarName Integer

data CodeGenState = CodeGenState
    { regMap :: AAsmAlloc
    , constMap :: ConstEnv
    , nextReg :: Register
    , nextLabel :: Integer  -- For generating unique labels
    , breakLabels :: [Label]  -- Stack of break labels for nested loops
    , continueLabels :: [Label]  -- Stack of continue labels for nested loops
    , code :: [String]
    }

type CodeGen a = State CodeGenState a

codeGen :: AST -> [String]
codeGen (Block stmts _) =
    code $ execState (genBlock stmts) (CodeGenState Map.empty Map.empty 0 0 [] [] [])

regName :: Register -> String
regName n = "%" ++ show n

freshReg :: CodeGen Register
freshReg = do
    s <- get
    let r = nextReg s
    put s{nextReg = r + 1}
    return r

-- Generate a fresh label
freshLabel :: String -> CodeGen Label
freshLabel prefix = do
    s <- get
    let n = nextLabel s
    put s{nextLabel = n + 1}
    return $ prefix ++ show n

assignVar :: VarName -> Register -> CodeGen ()
assignVar name r = modify $ \s ->
    s{regMap = Map.insert name r (regMap s)}

bindConst :: VarName -> Integer -> CodeGen ()
bindConst name v = modify $ \s ->
    s{constMap = Map.insert name v (constMap s)}

clearConst :: VarName -> CodeGen ()
clearConst name = modify $ \s ->
    s{constMap = Map.delete name (constMap s)}

lookupVar :: VarName -> CodeGen Register
lookupVar name = do
    m <- gets regMap
    case Map.lookup name m of
        Just r -> return r
        Nothing -> error $ "lookupVar: variable not found: " ++ name

emit :: String -> CodeGen ()
emit instr = modify $ \s -> s{code = code s ++ [instr]}

-- Push break/continue labels for loops
pushLoopLabels :: Label -> Label -> CodeGen ()
pushLoopLabels breakLbl continueLbl = modify $ \s ->
    s { breakLabels = breakLbl : breakLabels s
      , continueLabels = continueLbl : continueLabels s
      }

-- Pop break/continue labels
popLoopLabels :: CodeGen ()
popLoopLabels = modify $ \s ->
    s { breakLabels = case breakLabels s of
                        (_:rest) -> rest
                        [] -> []
      , continueLabels = case continueLabels s of
                           (_:rest) -> rest
                           [] -> []
      }

genBlock :: [Stmt] -> CodeGen ()
genBlock [] = return ()
genBlock (stmt : rest) = do
    isReturn <- genStmt stmt
    if isReturn
        then return ()
        else genBlock rest

genStmt :: Stmt -> CodeGen Bool
genStmt (Decl _ name _) = do
    freshReg >>= assignVar name
    return False

genStmt (Init _ name e _) = do
    -- Be conservative about constant folding in Init statements
    -- Only do constant folding if the expression contains no variable references
    if containsVariableRef e
    then do
        r' <- genExpr e
        assignVar name r'
        clearConst name
    else do
        -- Safe to use constant folding - no variables involved
        me <- constEval e
        r <- freshReg
        case me of
            Just v -> do
                emit $ regName r ++ " = " ++ show v
                assignVar name r
                bindConst name v
            Nothing -> do
                r' <- genExpr e
                assignVar name r'
                clearConst name
    return False

genStmt (Asgn name Nothing e _) = do
    -- Check if the expression references the variable being assigned
    -- If so, clear the constant first to avoid incorrect constant folding
    if exprReferencesVar name e
    then do
        clearConst name  -- Clear before evaluation to prevent self-reference issues
        varReg <- lookupVar name  -- Get current register for the variable
        r' <- genExpr e
        -- Emit assignment back to the variable's register to maintain consistency in loops
        emit $ regName varReg ++ " = " ++ regName r'
    else do
        -- Safe to use constant folding
        me <- constEval e
        r <- freshReg
        case me of
            Just v -> do
                emit $ regName r ++ " = " ++ show v
                assignVar name r
                bindConst name v
            Nothing -> do
                r' <- genExpr e
                assignVar name r'
                clearConst name
    return False

genStmt (Asgn name (Just op) e _) = do
    r' <- genExpr e
    lhs <- lookupVar name
    emit $ regName lhs ++ " " ++ show op ++ "= " ++ regName r'
    clearConst name
    return False

genStmt (Ret e _) = do
    r <- genExpr e
    emit ("ret " ++ regName r)
    return True

-- New L2 control flow statements
genStmt (If cond thenStmt elseStmt _) = do
    condReg <- genExpr cond
    elseLbl <- freshLabel "else_"
    endLbl <- freshLabel "end_if_"
    
    -- Jump to else if condition is false (0)
    emit $ "if " ++ regName condReg ++ " == 0 goto " ++ elseLbl
    
    -- Generate then branch
    thenReturns <- genStmt thenStmt
    
    -- Only jump to end if the then branch didn't return
    unless thenReturns $ emit $ "goto " ++ endLbl
    
    -- Else branch
    emit $ elseLbl ++ ":"
    elseReturns <- case elseStmt of
        Just stmt -> genStmt stmt
        Nothing -> return False
    
    -- Only emit end label if at least one branch doesn't return
    unless (thenReturns && elseReturns) $ emit $ endLbl ++ ":"
    
    return (thenReturns && elseReturns)

genStmt (While cond body _) = do
    startLbl <- freshLabel "while_start_"
    endLbl <- freshLabel "while_end_"
    
    pushLoopLabels endLbl startLbl  -- break goes to end, continue goes to start
    
    emit $ startLbl ++ ":"
    condReg <- genExpr cond
    emit $ "if " ++ regName condReg ++ " == 0 goto " ++ endLbl
    
    _ <- genStmt body
    emit $ "goto " ++ startLbl
    emit $ endLbl ++ ":"
    
    popLoopLabels
    return False

genStmt (For maybeInit maybeCond maybeStep body _) = do
    startLbl <- freshLabel "for_start_"
    stepLbl <- freshLabel "for_step_"
    endLbl <- freshLabel "for_end_"
    
    -- Generate init
    case maybeInit of
        Just initStmt -> do _ <- genStmt initStmt; return ()
        Nothing -> return ()
    
    pushLoopLabels endLbl stepLbl  -- break goes to end, continue goes to step
    
    emit $ startLbl ++ ":"
    
    -- Generate condition check
    case maybeCond of
        Just condExpr -> do
            condReg <- genExpr condExpr
            emit $ "if " ++ regName condReg ++ " == 0 goto " ++ endLbl
        Nothing -> return ()  -- Infinite loop if no condition
    
    -- Generate body
    _ <- genStmt body
    
    -- Step label and step statement
    emit $ stepLbl ++ ":"
    case maybeStep of
        Just stepStmt -> do _ <- genStmt stepStmt; return ()
        Nothing -> return ()
    
    emit $ "goto " ++ startLbl
    emit $ endLbl ++ ":"
    
    popLoopLabels
    return False

genStmt (Break _) = do
    labels <- gets breakLabels
    case labels of
        (lbl:_) -> emit $ "goto " ++ lbl
        [] -> error "Break outside of loop"
    return False

genStmt (Continue _) = do
    labels <- gets continueLabels
    case labels of
        (lbl:_) -> emit $ "goto " ++ lbl
        [] -> error "Continue outside of loop"
    return False

genStmt (BlockStmt stmts _) = do
    genBlock stmts
    return False

toInt32 :: Integer -> Integer
toInt32 x =
    let w = x .&. 0xffffffff
     in if w >= 0x80000000 then w - 0x100000000 else w

-- Extended constant evaluation for L2
constEval :: Expr -> CodeGen (Maybe Integer)
constEval (IntExpr s _) =
    case parseNumber s of
        Left _ -> return Nothing
        Right v -> return (Just (toInt32 v))

constEval (BoolExpr b _) = return (Just (if b then 1 else 0))

constEval (Ident name _) = gets (Map.lookup name . constMap)

constEval (UnExpr Neg e) = do
    me <- constEval e
    return (toInt32 . negate <$> me)

constEval (UnExpr Not e) = do
    me <- constEval e
    return ((\v -> if v == 0 then 1 else 0) <$> me)

constEval (UnExpr BitNot e) = do
    me <- constEval e
    return (toInt32 . complement <$> me)

constEval (UnExpr _ _) = return Nothing

constEval (BinExpr op l r) = do
    ml <- constEval l
    mr <- constEval r
    return $ case (ml, mr) of
        (Just v1, Just v2) ->
            case op of
                Add -> Just $ toInt32 (v1 + v2)
                Sub -> Just $ toInt32 (v1 - v2)
                Mul -> Just $ toInt32 (v1 * v2)
                Div ->
                    if (v2 == 0) || (v1 == (-(2 ^ (31 :: Integer))) && v2 == -1)
                        then Nothing
                        else Just $ toInt32 (v1 `quot` v2)
                Mod ->
                    if v2 == 0
                        then Nothing
                        else Just $ toInt32 (v1 `rem` v2)
                -- Comparison operators
                Lt -> Just $ if v1 < v2 then 1 else 0
                Le -> Just $ if v1 <= v2 then 1 else 0
                Gt -> Just $ if v1 > v2 then 1 else 0
                Ge -> Just $ if v1 >= v2 then 1 else 0
                Eq -> Just $ if v1 == v2 then 1 else 0
                Ne -> Just $ if v1 /= v2 then 1 else 0
                -- Logical operators
                And -> Just $ if v1 /= 0 && v2 /= 0 then 1 else 0
                Or -> Just $ if v1 /= 0 || v2 /= 0 then 1 else 0
                -- Bitwise operators
                BitAnd -> Just $ toInt32 (v1 .&. v2)
                BitOr -> Just $ toInt32 (v1 .|. v2)
                BitXor -> Just $ toInt32 (v1 `xor` v2)
                -- Shift operators (mask to 5 bits as per spec)
                Shl -> Just $ toInt32 (v1 `shiftL` fromIntegral (v2 .&. 0x1f))
                Shr -> Just $ toInt32 (v1 `shiftR` fromIntegral (v2 .&. 0x1f))
                _ -> Nothing
        _ -> Nothing

-- Simple ternary constant evaluation
constEval (TernaryExpr cond thenExpr elseExpr _) = do
    mc <- constEval cond
    case mc of
        Just c -> if c /= 0 
                  then constEval thenExpr 
                  else constEval elseExpr
        Nothing -> return Nothing

genExpr :: Expr -> CodeGen Register
genExpr (IntExpr s _) =
    case parseNumber s of
        Left err -> error err
        Right v -> do
            r <- freshReg
            emit $ regName r ++ " = " ++ show (toInt32 v)
            return r

genExpr (BoolExpr b _) = do
    r <- freshReg
    emit $ regName r ++ " = " ++ if b then "1" else "0"
    return r

genExpr (Ident name _) = lookupVar name

genExpr (UnExpr op e) = do
    r1 <- genExpr e
    r <- freshReg
    emit $ regName r ++ " = " ++ show op ++ " " ++ regName r1
    return r

genExpr (BinExpr op e1 e2) = do
    r1 <- genExpr e1
    r2 <- genExpr e2
    r <- freshReg
    emit $ regName r ++ " = " ++ regName r1 ++ " " ++ show op ++ " " ++ regName r2
    return r

-- Generate code for ternary operator
genExpr (TernaryExpr cond thenExpr elseExpr _) = do
    condReg <- genExpr cond
    thenLbl <- freshLabel "ternary_then_"
    elseLbl <- freshLabel "ternary_else_"
    endLbl <- freshLabel "ternary_end_"
    resultReg <- freshReg
    
    -- Jump to else if condition is false
    emit $ "if " ++ regName condReg ++ " == 0 goto " ++ elseLbl
    
    -- Then branch
    emit $ thenLbl ++ ":"
    thenReg <- genExpr thenExpr
    emit $ regName resultReg ++ " = " ++ regName thenReg
    emit $ "goto " ++ endLbl
    
    -- Else branch
    emit $ elseLbl ++ ":"
    elseReg <- genExpr elseExpr
    emit $ regName resultReg ++ " = " ++ regName elseReg
    
    emit $ endLbl ++ ":"
    return resultReg

-- Check if an expression references a specific variable
exprReferencesVar :: VarName -> Expr -> Bool
exprReferencesVar var (Ident name _) = var == name
exprReferencesVar var (UnExpr _ e) = exprReferencesVar var e
exprReferencesVar var (BinExpr _ e1 e2) = exprReferencesVar var e1 || exprReferencesVar var e2
exprReferencesVar var (TernaryExpr e1 e2 e3 _) = exprReferencesVar var e1 || exprReferencesVar var e2 || exprReferencesVar var e3
exprReferencesVar _ _ = False

-- Check if an expression contains any variable references
containsVariableRef :: Expr -> Bool
containsVariableRef (Ident _ _) = True
containsVariableRef (UnExpr _ e) = containsVariableRef e
containsVariableRef (BinExpr _ e1 e2) = containsVariableRef e1 || containsVariableRef e2
containsVariableRef (TernaryExpr e1 e2 e3 _) = containsVariableRef e1 || containsVariableRef e2 || containsVariableRef e3
containsVariableRef _ = False