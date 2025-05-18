module Compile.AAsm (
    codeGen,
) where

import Compile.AST (AST (..), Expr (..), Op (..), Stmt (..))
import Compile.Parser (parseNumber)

import Control.Monad.State (State, execState, get, gets, modify, put)
import Data.Bits ((.&.)) -- for 32-bit wrapping
import qualified Data.Map as Map

type Register = Integer
type VarName = String

-- AAsm register allocation and constant environment
-- so we can fold qualified expressions with correct 32-bit semantics

type AAsmAlloc = Map.Map VarName Register

type ConstEnv = Map.Map VarName Integer

data CodeGenState = CodeGenState
    { regMap :: AAsmAlloc
    , constMap :: ConstEnv
    , nextReg :: Register
    , code :: [String]
    }

type CodeGen a = State CodeGenState a

-- Entry point: start with empty maps\ ncodeGen :: AST -> [String]
codeGen (Block stmts _) =
    code $ execState (genBlock stmts) (CodeGenState Map.empty Map.empty 0 [])

-- Helpers
regName :: Register -> String
regName n = "%" ++ show n

freshReg :: CodeGen Register
freshReg = do
    s <- get
    let r = nextReg s
    put s{nextReg = r + 1}
    return r

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

genBlock :: [Stmt] -> CodeGen ()
genBlock = mapM_ genStmt

-- Simulate C99 32-bit signed overflow semantics
toInt32 :: Integer -> Integer
toInt32 x =
    let w = x .&. 0xffffffff
     in if w >= 0x80000000 then w - 0x100000000 else w

-- Try constant-folding with correct 32-bit wrap
constEval :: Expr -> CodeGen (Maybe Integer)
constEval (IntExpr s _) =
    case parseNumber s of
        Left _ -> return Nothing
        Right v -> return (Just (toInt32 v))
constEval (Ident name _) = gets (Map.lookup name . constMap)
constEval (UnExpr Neg e) = do
    me <- constEval e
    return (toInt32 . negate <$> me)
constEval (BinExpr op l r) = do
    ml <- constEval l
    mr <- constEval r
    return $ case (ml, mr) of
        (Just v1, Just v2) ->
            let raw = case op of
                    Add -> v1 + v2
                    Sub -> v1 - v2
                    Mul -> v1 * v2
                    Div -> v1 `quot` v2
                    Mod -> v1 `rem` v2
                    _ -> error "unsupported op in constant evaluation"
                res = toInt32 raw
             in Just res
        _ -> Nothing

-- Statement codegen
genStmt :: Stmt -> CodeGen ()
genStmt (Decl name _) = freshReg >>= assignVar name
genStmt (Init name e _) = do
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
genStmt (Asgn name Nothing e _) = do
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
genStmt (Asgn name (Just op) e _) = do
    -- compound ops: clear any const assumption
    r' <- genExpr e
    lhs <- lookupVar name
    emit $ regName lhs ++ " " ++ show op ++ "= " ++ regName r'
    clearConst name
genStmt (Ret e _) = genExpr e >>= \r -> emit ("ret " ++ regName r)

-- Expression codegen (fallback)
genExpr :: Expr -> CodeGen Register
genExpr (IntExpr s _) =
    case parseNumber s of
        Left err -> error err
        Right v -> do
            r <- freshReg
            emit $ regName r ++ " = " ++ show (toInt32 v)
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
