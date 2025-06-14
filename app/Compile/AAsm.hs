module Compile.AAsm (
    codeGen,
) where

import Compile.AST (AST (..), Expr (..), Op (..), Stmt (..))
import Compile.Parser (parseNumber)

import Control.Monad.State (State, execState, get, gets, modify, put)
import Control.Monad (unless, void)
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
    , nextLabel :: Integer
    , breakLabels :: [Label]
    , continueLabels :: [Label]
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

pushLoopLabels :: Label -> Label -> CodeGen ()
pushLoopLabels breakLbl continueLbl = modify $ \s ->
    s { breakLabels = breakLbl : breakLabels s
      , continueLabels = continueLbl : continueLabels s
      }

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
    destReg <- freshReg
    assignVar name destReg

    case e of
        Ident _ _ -> do
            srcReg <- genExpr e
            emit $ regName destReg ++ " = " ++ regName srcReg
            clearConst name
        _ -> do
            me <- constEval e
            case me of
                Just v -> do
                    emit $ regName destReg ++ " = " ++ show v
                    bindConst name v
                Nothing -> do
                    srcReg <- genExpr e
                    emit $ regName destReg ++ " = " ++ regName srcReg
                    clearConst name
    return False

genStmt (Asgn name Nothing e _) = do
    let usesItself = varUsedInExpr name e
    me <- if usesItself then return Nothing else constEval e
    destReg <- lookupVar name
    
    case me of
        Just v -> do
            emit $ regName destReg ++ " = " ++ show v
            bindConst name v
        Nothing -> do
            srcReg <- genExpr e
            emit $ regName destReg ++ " = " ++ regName srcReg
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

genStmt (If cond thenStmt elseStmt _) = do
    condReg <- genExpr cond
    elseLbl <- freshLabel "else_"
    endLbl <- freshLabel "end_if_"
    
    originalConstMap <- gets constMap

    emit $ "if " ++ regName condReg ++ " == 0 goto " ++ elseLbl
    
    thenReturns <- genStmt thenStmt
    constMapAfterThen <- gets constMap
    
    case elseStmt of
        Just _ -> unless thenReturns $ emit $ "goto " ++ endLbl
        Nothing -> return ()
    
    emit $ elseLbl ++ ":"
    modify $ \s -> s { constMap = originalConstMap }
    
    (elseReturns, constMapAfterElse) <- case elseStmt of
        Just stmt -> do
            r <- genStmt stmt
            cm <- gets constMap
            return (r, cm)
        Nothing -> return (False, originalConstMap)

    unless (thenReturns && elseReturns) $ emit $ endLbl ++ ":"
    let finalConstMap = Map.filterWithKey (\k v -> Map.lookup k constMapAfterElse == Just v) constMapAfterThen
    modify $ \s -> s { constMap = finalConstMap }
    
    return (thenReturns && elseReturns)

genStmt (While cond body _) = do
    startLbl <- freshLabel "while_start_"
    endLbl <- freshLabel "while_end_"
    let bodyModified = findModifiedVars body
    mapM_ clearConst bodyModified
    
    pushLoopLabels endLbl startLbl
    
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
    modify $ \s -> s { constMap = Map.empty }
    case maybeInit of
        Just initStmt -> void $ genStmt initStmt
        Nothing -> return ()
    let bodyModified = maybe [] findModifiedVars maybeStep ++ findModifiedVars body
    mapM_ clearConst bodyModified
    pushLoopLabels endLbl stepLbl
    emit $ startLbl ++ ":"
    case maybeCond of
        Just condExpr -> do
            condReg <- genExpr condExpr
            emit $ "if " ++ regName condReg ++ " == 0 goto " ++ endLbl
        Nothing -> return ()
    void $ genStmt body
    case maybeStep of
        Just _ -> emit $ "goto " ++ stepLbl
        Nothing -> return ()
    emit $ stepLbl ++ ":"
    case maybeStep of
        Just stepStmt -> void $ genStmt stepStmt
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
                Div -> Nothing
                Mod -> Nothing
                Lt -> Just $ if v1 < v2 then 1 else 0
                Le -> Just $ if v1 <= v2 then 1 else 0
                Gt -> Just $ if v1 > v2 then 1 else 0
                Ge -> Just $ if v1 >= v2 then 1 else 0
                Eq -> Just $ if v1 == v2 then 1 else 0
                Ne -> Just $ if v1 /= v2 then 1 else 0
                And -> Just $ if v1 /= 0 && v2 /= 0 then 1 else 0
                Or -> Just $ if v1 /= 0 || v2 /= 0 then 1 else 0
                BitAnd -> Just $ toInt32 (v1 .&. v2)
                BitOr -> Just $ toInt32 (v1 .|. v2)
                BitXor -> Just $ toInt32 (v1 `xor` v2)
                Shl -> Just $ toInt32 (v1 `shiftL` fromIntegral (v2 .&. 0x1f))
                Shr -> Just $ toInt32 (v1 `shiftR` fromIntegral (v2 .&. 0x1f))
                _ -> Nothing
        _ -> Nothing

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

genExpr (BinExpr op e1 e2) = case op of
    And -> genAnd e1 e2
    Or  -> genOr e1 e2
    _   -> do
        r1 <- genExpr e1
        r2 <- genExpr e2
        r <- freshReg
        emit $ regName r ++ " = " ++ regName r1 ++ " " ++ show op ++ " " ++ regName r2
        return r

genExpr (TernaryExpr cond thenExpr elseExpr _) = do
    condReg <- genExpr cond
    thenLbl <- freshLabel "ternary_then_"
    elseLbl <- freshLabel "ternary_else_"
    endLbl <- freshLabel "ternary_end_"
    resultReg <- freshReg
    
    emit $ "if " ++ regName condReg ++ " == 0 goto " ++ elseLbl
    
    emit $ thenLbl ++ ":"
    thenReg <- genExpr thenExpr
    emit $ regName resultReg ++ " = " ++ regName thenReg
    emit $ "goto " ++ endLbl
    
    emit $ elseLbl ++ ":"
    elseReg <- genExpr elseExpr
    emit $ regName resultReg ++ " = " ++ regName elseReg
    
    emit $ endLbl ++ ":"
    return resultReg

genAnd :: Expr -> Expr -> CodeGen Register
genAnd e1 e2 = do
    resultReg <- freshReg
    falseLbl <- freshLabel "and_false_"
    endLbl <- freshLabel "and_end_"

    r1 <- genExpr e1
    emit $ "if " ++ regName r1 ++ " == 0 goto " ++ falseLbl

    r2 <- genExpr e2
    emit $ "if " ++ regName r2 ++ " == 0 goto " ++ falseLbl

    emit $ regName resultReg ++ " = 1"
    emit $ "goto " ++ endLbl

    emit $ falseLbl ++ ":"
    emit $ regName resultReg ++ " = 0"

    emit $ endLbl ++ ":"
    return resultReg

genOr :: Expr -> Expr -> CodeGen Register
genOr e1 e2 = do
    resultReg <- freshReg
    checkRhsLbl <- freshLabel "or_rhs_"
    trueLbl <- freshLabel "or_true_"
    falseLbl <- freshLabel "or_false_"
    endLbl <- freshLabel "or_end_"

    r1 <- genExpr e1
    emit $ "if " ++ regName r1 ++ " == 0 goto " ++ checkRhsLbl
    emit $ "goto " ++ trueLbl

    emit $ checkRhsLbl ++ ":"
    r2 <- genExpr e2
    emit $ "if " ++ regName r2 ++ " == 0 goto " ++ falseLbl
    emit $ "goto " ++ trueLbl

    emit $ falseLbl ++ ":"
    emit $ regName resultReg ++ " = 0"
    emit $ "goto " ++ endLbl

    emit $ trueLbl ++ ":"
    emit $ regName resultReg ++ " = 1"

    emit $ endLbl ++ ":"
    return resultReg


varUsedInExpr :: String -> Expr -> Bool
varUsedInExpr name (Ident n _) = name == n
varUsedInExpr name (BinExpr _ e1 e2) = varUsedInExpr name e1 || varUsedInExpr name e2
varUsedInExpr name (UnExpr _ e) = varUsedInExpr name e
varUsedInExpr name (TernaryExpr e1 e2 e3 _) = varUsedInExpr name e1 || varUsedInExpr name e2 || varUsedInExpr name e3
varUsedInExpr _ _ = False

findModifiedVars :: Stmt -> [String]
findModifiedVars (Asgn name _ _ _) = [name]
findModifiedVars (Init _ name _ _) = [name]
findModifiedVars (If _ thenStmt elseStmt _) = 
    findModifiedVars thenStmt ++ maybe [] findModifiedVars elseStmt  
findModifiedVars (While _ body _) = findModifiedVars body
findModifiedVars (For _ _ _ body _) = findModifiedVars body
findModifiedVars (BlockStmt stmts _) = concatMap findModifiedVars stmts
findModifiedVars _ = []