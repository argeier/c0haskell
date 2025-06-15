module Compile.AAsm (
    codeGen,
    AAsmProgram,
    AAsmInstruction (..),
    Terminator (..),
    BasicBlock (..),
    Register,
    Label,
) where

import qualified Compile.AST as AST
import Compile.AST (Op(..))
import Compile.Parser (parseNumber)

import Control.Monad (unless, void)
import Control.Monad.State (State, execState, get, gets, modify, put)
import Data.Bits ((.&.), (.|.), complement, shiftL, shiftR, xor)
import qualified Data.Map.Strict as Map
import Data.List (intercalate)
import Data.Maybe (isJust)

type Register = Integer
type VarName = String
type Label = String

data AAsmInstruction
    = AAsmBinOp Op Register Register Register
    | AAsmUnOp Op Register Register
    | AAsmAsgnOp Op Register Register
    | AAsmMove Register Register
    | AAsmLoadImm Register Integer
    deriving (Eq)

data Terminator
    = Ret Register
    | Goto Label
    | CondGoto Register Label Label
    deriving (Eq)

data BasicBlock = BasicBlock
    { blockLabel :: Label
    , blockInstructions :: [AAsmInstruction]
    , blockTerminator :: Terminator
    }
    deriving (Eq)

type AAsmProgram = [BasicBlock]

data FlatInstruction
    = FInstr AAsmInstruction
    | FTerm Terminator
    | FLabel Label
    deriving (Eq)

instance Show AAsmInstruction where
    show (AAsmBinOp op res src1 src2) = unwords [regName res, "=", regName src1, show op, regName src2]
    show (AAsmUnOp op res src)       = unwords [regName res, "=", show op, regName src]
    show (AAsmAsgnOp op dest src)    = unwords [regName dest, show op ++ "=", regName src]
    show (AAsmMove dest src)         = unwords [regName dest, "=", regName src]
    show (AAsmLoadImm dest val)      = unwords [regName dest, "=", show val]

instance Show Terminator where
    show (Ret reg)               = "ret " ++ regName reg
    show (Goto l)                = "goto " ++ l
    show (CondGoto reg lTrue lFalse) = unwords ["if", regName reg, "== 0", "goto", lTrue, "; else", lFalse]

instance Show BasicBlock where
    show (BasicBlock lbl instrs term) =
        lbl ++ ":\n" ++
        unlines (map (("  " ++) . show) instrs) ++
        "  " ++ show term ++ "\n"

type AAsmAlloc = Map.Map VarName Register
type ConstEnv = Map.Map VarName Integer

data CodeGenState = CodeGenState
    { regMap :: AAsmAlloc
    , constMap :: ConstEnv
    , nextReg :: Register
    , nextLabel :: Integer
    , breakLabels :: [Label]
    , continueLabels :: [Label]
    , code :: [FlatInstruction]
    }

type CodeGen a = State CodeGenState a

initialCodeGenState :: CodeGenState
initialCodeGenState = CodeGenState Map.empty Map.empty 0 0 [] [] []

codeGen :: AST.AST -> AAsmProgram
codeGen (AST.Block stmts _) =
    let flatCode = reverse . code $ execState (genBlock stmts) initialCodeGenState
    in partitionIntoBlocks flatCode

partitionIntoBlocks :: [FlatInstruction] -> AAsmProgram
partitionIntoBlocks flatCode =
    let prefixedCode = case flatCode of
            (FLabel _ : _) -> flatCode
            _              -> FLabel "entry" : flatCode
    in toBlocks prefixedCode
  where
    toBlocks :: [FlatInstruction] -> [BasicBlock]
    toBlocks [] = []
    toBlocks (FLabel l : rest) =
        let (instrs, remainingCode) = span isInstruction rest
            (term, afterTerm) = case remainingCode of
                (FTerm t : xs) -> (t, xs)
                (FLabel nextL : _) -> (Goto nextL, remainingCode)
                [] -> (Ret 0, [])
                _ -> error "AAsm.partitionIntoBlocks: block not properly terminated"
            typedInstrs = [i | FInstr i <- instrs]
        in BasicBlock l typedInstrs term : toBlocks afterTerm
    toBlocks _ = error "Code does not start with a label. This should not happen due to the prefixing logic."

    isInstruction (FInstr _) = True
    isInstruction _          = False

regName :: Register -> String
regName n = "%" ++ show n

freshReg :: CodeGen Register
freshReg = gets nextReg <* modify (\s -> s { nextReg = nextReg s + 1 })

freshLabel :: String -> CodeGen Label
freshLabel prefix = do
    n <- gets nextLabel
    modify $ \s -> s { nextLabel = n + 1 }
    return $ prefix ++ show n

assignVar :: VarName -> Register -> CodeGen ()
assignVar name r = modify $ \s -> s { regMap = Map.insert name r (regMap s) }

bindConst :: VarName -> Integer -> CodeGen ()
bindConst name v = modify $ \s -> s { constMap = Map.insert name v (constMap s) }

clearConst :: VarName -> CodeGen ()
clearConst name = modify $ \s -> s { constMap = Map.delete name (constMap s) }

lookupVar :: VarName -> CodeGen Register
lookupVar name = gets (regMap) >>= \m -> case Map.lookup name m of
    Just r -> return r
    Nothing -> error $ "lookupVar: variable not found: " ++ name

emit :: FlatInstruction -> CodeGen ()
emit instr = modify $ \s -> s { code = instr : code s }

pushLoopLabels :: Label -> Label -> CodeGen ()
pushLoopLabels breakLbl continueLbl = modify $ \s -> s { breakLabels = breakLbl : breakLabels s, continueLabels = continueLbl : continueLabels s }

popLoopLabels :: CodeGen ()
popLoopLabels = modify $ \s ->
    let newBreaks    = case breakLabels s of (_:xs) -> xs; [] -> []
        newContinues = case continueLabels s of (_:xs) -> xs; [] -> []
    in s { breakLabels = newBreaks, continueLabels = newContinues }

genBlock :: [AST.Stmt] -> CodeGen Bool
genBlock [] = return False
genBlock (stmt : rest) = do
    branches <- genStmt stmt
    if branches
        then return True
        else genBlock rest

genStmt :: AST.Stmt -> CodeGen Bool
genStmt (AST.Decl _ name _) = freshReg >>= assignVar name >> return False

genStmt (AST.Init _ name e _) = do
    destReg <- freshReg
    assignVar name destReg
    me <- constEval e
    case me of
        Just v  -> emit (FInstr $ AAsmLoadImm destReg v) >> bindConst name v
        Nothing -> genExpr e >>= \srcReg -> emit (FInstr $ AAsmMove destReg srcReg) >> clearConst name
    return False

genStmt (AST.Asgn name Nothing e _) = do
    destReg <- lookupVar name
    me <- constEval e
    case me of
        Just v  -> emit (FInstr $ AAsmLoadImm destReg v) >> bindConst name v
        Nothing -> genExpr e >>= \srcReg -> emit (FInstr $ AAsmMove destReg srcReg) >> clearConst name
    return False

genStmt (AST.Asgn name (Just op) e _) = do
    r' <- genExpr e
    lhs <- lookupVar name
    emit (FInstr $ AAsmAsgnOp op lhs r')
    clearConst name
    return False

genStmt (AST.Ret e _) = genExpr e >>= \r -> emit (FTerm $ Ret r) >> return True

genStmt (AST.If cond thenStmt elseStmt _) = do
    condReg <- genExpr cond
    thenLbl <- freshLabel "then_"
    elseLbl <- freshLabel "else_"
    endLbl <- freshLabel "end_if_"

    originalConstMap <- gets constMap

    let falseBranchTarget = if isJust elseStmt then elseLbl else endLbl
    emit $ FTerm $ CondGoto condReg falseBranchTarget thenLbl

    emit $ FLabel thenLbl
    returnedInThen <- genStmt thenStmt
    constMapAfterThen <- gets constMap
    unless returnedInThen $ emit $ FTerm $ Goto endLbl
    
    modify $ \s -> s { constMap = originalConstMap }
    (returnedInElse, constMapAfterElse) <- case elseStmt of
        Just s -> do
            emit $ FLabel elseLbl
            returned <- genStmt s
            cm <- gets constMap
            unless returned $ emit $ FTerm $ Goto endLbl
            return (returned, cm)
        Nothing -> return (False, originalConstMap)

    let finalConstMap = Map.filterWithKey (\k v -> Map.lookup k constMapAfterElse == Just v) constMapAfterThen
    modify $ \s -> s { constMap = finalConstMap }

    unless (returnedInThen && returnedInElse) $ emit $ FLabel endLbl
    return (returnedInThen && returnedInElse)

genStmt (AST.While cond body _) = do
    startLbl <- freshLabel "while_start_"
    bodyLbl <- freshLabel "while_body_"
    endLbl <- freshLabel "while_end_"

    mapM_ clearConst (findModifiedVars body)
    
    pushLoopLabels endLbl startLbl
    emit $ FTerm $ Goto startLbl
    emit $ FLabel startLbl
    condReg <- genExpr cond
    emit $ FTerm $ CondGoto condReg endLbl bodyLbl
    emit $ FLabel bodyLbl
    
    bodyReturns <- genStmt body
    unless bodyReturns $ emit $ FTerm $ Goto startLbl

    emit $ FLabel endLbl
    popLoopLabels
    return False

genStmt (AST.For maybeInit maybeCond maybeStep body _) = do
    startLbl <- freshLabel "for_start_"
    bodyLbl <- freshLabel "for_body_"
    stepLbl <- freshLabel "for_step_"
    endLbl <- freshLabel "for_end_"

    case maybeInit of
        Just initStmt -> void $ genStmt initStmt
        Nothing -> return ()

    let modifiedInLoop = findModifiedVars body ++ maybe [] findModifiedVars maybeStep
    mapM_ clearConst modifiedInLoop

    pushLoopLabels endLbl stepLbl
    emit $ FTerm $ Goto startLbl
    emit $ FLabel startLbl
    case maybeCond of
        Just condExpr -> do
            condReg <- genExpr condExpr
            emit $ FTerm $ CondGoto condReg endLbl bodyLbl
        Nothing -> emit (FTerm (Goto bodyLbl))
    emit $ FLabel bodyLbl
    
    bodyReturns <- genStmt body
    unless bodyReturns $ emit $ FTerm $ Goto stepLbl

    emit $ FLabel stepLbl
    stepReturns <- case maybeStep of
        Just stepStmt -> genStmt stepStmt
        Nothing -> return False
    unless stepReturns $ emit $ FTerm $ Goto startLbl

    emit $ FLabel endLbl
    popLoopLabels
    return False

genStmt (AST.Break _) = gets breakLabels >>= \ls -> case ls of (l:_) -> emit (FTerm $ Goto l) >> return True; [] -> error "Break outside loop"
genStmt (AST.Continue _) = gets continueLabels >>= \ls -> case ls of (l:_) -> emit (FTerm $ Goto l) >> return True; [] -> error "Continue outside loop"
genStmt (AST.BlockStmt stmts _) = genBlock stmts

toInt32 :: Integer -> Integer
toInt32 x = let w = x .&. 0xffffffff in if w >= 0x80000000 then w - 0x100000000 else w

constEval :: AST.Expr -> CodeGen (Maybe Integer)
constEval (AST.IntExpr s _) = case parseNumber s of Left _ -> return Nothing; Right v -> return (Just (toInt32 v))
constEval (AST.BoolExpr b _) = return (Just (if b then 1 else 0))
constEval (AST.Ident name _) = gets (Map.lookup name . constMap)
constEval (AST.UnExpr op e) = fmap (constEval' op) (constEval e)
    where constEval' Neg (Just v) = Just $ toInt32 (negate v)
          constEval' Not (Just v) = Just $ if v == 0 then 1 else 0
          constEval' BitNot (Just v) = Just $ toInt32 (complement v)
          constEval' _ _ = Nothing
constEval (AST.BinExpr op l r) = do
    ml <- constEval l
    mr <- constEval r
    return $ case (ml, mr) of
        (Just v1, Just v2) -> case op of
            Add -> Just $ toInt32 (v1 + v2)
            Sub -> Just $ toInt32 (v1 - v2)
            Mul -> Just $ toInt32 (v1 * v2)
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
            -- Mask shift amounts to 5 bits to match x86 behavior
            Shl -> Just $ toInt32 (v1 `shiftL` (fromInteger v2 .&. 31))
            Shr -> Just $ toInt32 (v1 `shiftR` (fromInteger v2 .&. 31))
            _ -> Nothing
        _ -> Nothing
constEval (AST.TernaryExpr c t e _) = do
    mc <- constEval c
    case mc of
      Just 0 -> constEval e
      Just _ -> constEval t
      Nothing -> return Nothing

genExpr :: AST.Expr -> CodeGen Register
genExpr e = do
    me <- constEval e
    case me of
        Just v -> freshReg >>= \r -> emit (FInstr $ AAsmLoadImm r v) >> return r
        Nothing -> genExprNoConst e

genExprNoConst :: AST.Expr -> CodeGen Register
genExprNoConst (AST.BinExpr AST.And e1 e2) = genAnd e1 e2
genExprNoConst (AST.BinExpr AST.Or e1 e2) = genOr e1 e2
genExprNoConst (AST.BinExpr op e1 e2) = do
    r1 <- genExpr e1
    r2 <- genExpr e2
    r <- freshReg
    emit $ FInstr $ AAsmBinOp op r r1 r2
    return r
genExprNoConst (AST.Ident name _) = lookupVar name
genExprNoConst (AST.UnExpr op e) = do
    r1 <- genExpr e
    r <- freshReg
    emit $ FInstr $ AAsmUnOp op r r1
    return r
genExprNoConst (AST.TernaryExpr cond thenExpr elseExpr _) = do
    condReg <- genExpr cond
    thenLbl <- freshLabel "ternary_then_"
    elseLbl <- freshLabel "ternary_else_"
    endLbl <- freshLabel "ternary_end_"
    resultReg <- freshReg
    emit $ FTerm $ CondGoto condReg elseLbl thenLbl
    emit $ FLabel thenLbl
    thenReg <- genExpr thenExpr
    emit $ FInstr $ AAsmMove resultReg thenReg
    emit $ FTerm $ Goto endLbl
    emit $ FLabel elseLbl
    elseReg <- genExpr elseExpr
    emit $ FInstr $ AAsmMove resultReg elseReg
    emit $ FLabel endLbl
    return resultReg
genExprNoConst e = error $ "genExprNoConst called on an expression that should not be here: " ++ show e

genAnd :: AST.Expr -> AST.Expr -> CodeGen Register
genAnd e1 e2 = do
    resultReg <- freshReg
    rhsLbl <- freshLabel "and_rhs_"
    falseLbl <- freshLabel "and_false_"
    endLbl <- freshLabel "and_end_"

    r1 <- genExpr e1
    emit $ FTerm $ CondGoto r1 falseLbl rhsLbl

    emit $ FLabel rhsLbl
    r2 <- genExpr e2
    emit $ FInstr $ AAsmMove resultReg r2
    emit $ FTerm $ Goto endLbl

    emit $ FLabel falseLbl
    emit $ FInstr $ AAsmLoadImm resultReg 0
    emit $ FTerm $ Goto endLbl

    emit $ FLabel endLbl
    return resultReg

genOr :: AST.Expr -> AST.Expr -> CodeGen Register
genOr e1 e2 = do
    resultReg <- freshReg
    rhsLbl <- freshLabel "or_rhs_"
    trueLbl <- freshLabel "or_true_"
    endLbl <- freshLabel "or_end_"

    r1 <- genExpr e1
    emit $ FTerm $ CondGoto r1 rhsLbl trueLbl

    emit $ FLabel rhsLbl
    r2 <- genExpr e2
    emit $ FInstr $ AAsmMove resultReg r2
    emit $ FTerm $ Goto endLbl

    emit $ FLabel trueLbl
    emit $ FInstr $ AAsmLoadImm resultReg 1
    emit $ FTerm $ Goto endLbl

    emit $ FLabel endLbl
    return resultReg

findModifiedVars :: AST.Stmt -> [String]
findModifiedVars (AST.Asgn name _ _ _) = [name]
findModifiedVars (AST.Init _ name _ _) = [name]
findModifiedVars (AST.If _ thenS elseS _) = findModifiedVars thenS ++ maybe [] findModifiedVars elseS
findModifiedVars (AST.While _ body _) = findModifiedVars body
findModifiedVars (AST.For _ _ stepS body _) = maybe [] findModifiedVars stepS ++ findModifiedVars body
findModifiedVars (AST.BlockStmt stmts _) = concatMap findModifiedVars stmts
findModifiedVars _ = []