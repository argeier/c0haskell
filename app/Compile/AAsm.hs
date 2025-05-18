module Compile.AAsm (
    codeGen,
) where

import Compile.AST (AST (..), Expr (..), Op (..), Stmt (..))
import Compile.Parser (parseNumber)

import Control.Monad.State (State, execState, get, gets, modify, put)
import Data.Bits ((.&.))
import qualified Data.Map as Map

type Register = Integer
type VarName = String

type AAsmAlloc = Map.Map VarName Register
type ConstEnv = Map.Map VarName Integer

data CodeGenState = CodeGenState
    { regMap :: AAsmAlloc
    , constMap :: ConstEnv
    , nextReg :: Register
    , code :: [String]
    }

type CodeGen a = State CodeGenState a

codeGen :: AST -> [String]
codeGen (Block stmts _) =
    code $ execState (genBlock stmts) (CodeGenState Map.empty Map.empty 0 [])

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
genBlock [] = return ()
genBlock (stmt : rest) = do
    isReturn <- genStmt stmt
    if isReturn
        then return ()
        else genBlock rest

genStmt :: Stmt -> CodeGen Bool
genStmt (Decl name _) = do
    freshReg >>= assignVar name
    return False
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
    return False
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

toInt32 :: Integer -> Integer
toInt32 x =
    let w = x .&. 0xffffffff
     in if w >= 0x80000000 then w - 0x100000000 else w

-- In AAsm.hs
constEval :: Expr -> CodeGen (Maybe Integer)
constEval (IntExpr s _) =
    case parseNumber s of
        Left _ -> return Nothing
        Right v -> return (Just (toInt32 v))
constEval (Ident name _) = gets (Map.lookup name . constMap)
constEval (UnExpr Neg e) = do
    me <- constEval e
    return (toInt32 . negate <$> me)
constEval (UnExpr _ _) = return Nothing -- Other unary ops not const-evaluable or don't exist
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
                        then
                            Nothing
                        else
                            Just $ toInt32 (v1 `quot` v2)
                Mod ->
                    if v2 == 0
                        then Nothing
                        else Just $ toInt32 (v1 `rem` v2)
                _ -> error "unsupported binary op in constant evaluation"
        _ -> Nothing

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