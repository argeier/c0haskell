module Compile.X86 (
    generateX86,
) where

import Compile.AAsm (AAsmProgram, BasicBlock(..), Terminator(..), AAsmInstruction(..), Register)
import Compile.Coloring (Allocation)
import Compile.AST (Op(..))

import Control.Monad.State (State, execState, gets, modify)
import qualified Data.Map.Strict as Map
import Text.Printf (printf)

data X86State = X86State
    { allocation   :: Allocation
    , spillCount   :: Int
    , instructions :: [String]
    }

type X86 a = State X86State a

generateX86 :: AAsmProgram -> Allocation -> [String]
generateX86 prog alloc =
    let spills = Map.filter isSpill alloc
        spillSlots = Map.size spills
        stackBytes = if spillSlots == 0 then 0 else ((spillSlots + 1) `div` 2) * 16

        initialState = X86State { allocation = alloc, spillCount = spillSlots, instructions = [] }
        finalState = execState (translateProgram prog) initialState

        prologue = genPrologue stackBytes
        body = reverse $ instructions finalState
    in prologue ++ body
  where
    isSpill (Left _) = True
    isSpill _        = False

genPrologue :: Int -> [String]
genPrologue stackBytes =
    [ ".intel_syntax noprefix"
    , ".text"
    , ".global main"
    , "main:"
    , "\tcall _main"
    , "\tmov edi, eax"
    , "\tmov rax, 60"
    , "\tsyscall"
    , "_main:"
    , "\tpush rbp"
    , "\tmov rbp, rsp"
    ] ++ (if stackBytes > 0 then [printf "\tsub rsp, %d" stackBytes] else [])

genEpilogue :: Int -> [String]
genEpilogue stackBytes = (if stackBytes > 0 then [printf "\tadd rsp, %d" stackBytes] else []) ++
    [ "\tpop rbp"
    , "\tret"
    ]

emit :: String -> X86 ()
emit instr = modify $ \s -> s { instructions = instr : instructions s }

translateProgram :: AAsmProgram -> X86 ()
translateProgram = mapM_ translateBlock

translateBlock :: BasicBlock -> X86 ()
translateBlock block = do
    emit $ blockLabel block ++ ":"
    mapM_ translateInstruction (blockInstructions block)
    translateTerminator (blockTerminator block)

getOperand :: Register -> X86 String
getOperand reg = do
    alloc <- gets allocation
    case Map.lookup reg alloc of
        Just (Right physReg) -> return $ to32BitRegName physReg
        Just (Left spillLoc) -> return $ printf "DWORD PTR [rbp-%d]" (spillLoc * 8)
        Nothing              -> error $ "Register " ++ show reg ++ " not found in allocation map."

translateInstruction :: AAsmInstruction -> X86 ()
translateInstruction (AAsmLoadImm dest val) = do
    destOp <- getOperand dest
    emit $ printf "\tmov %s, %d" destOp val

translateInstruction (AAsmMove dest src) = do
    destOp <- getOperand dest
    srcOp <- getOperand src
    if isMem destOp && isMem srcOp
        then do
            emit $ printf "\tmov eax, %s" srcOp
            emit $ printf "\tmov %s, eax" destOp
        else if destOp /= srcOp then
            emit $ printf "\tmov %s, %s" destOp srcOp
        else
            return ()

translateInstruction (AAsmBinOp op dest src1 src2) = case op of
    Add -> translateBinOp "add" dest src1 src2
    Sub -> translateBinOp "sub" dest src1 src2
    Mul -> translateBinOp "imul" dest src1 src2
    Div -> translateDivMod "idiv" dest src1 src2
    Mod -> translateDivMod "idiv_mod" dest src1 src2
    BitAnd -> translateBinOp "and" dest src1 src2
    BitOr -> translateBinOp "or" dest src1 src2
    BitXor -> translateBinOp "xor" dest src1 src2
    Shl -> translateShift "sal" dest src1 src2
    Shr -> translateShift "sar" dest src1 src2
    Eq -> translateComparison "sete" dest src1 src2
    Ne -> translateComparison "setne" dest src1 src2
    Lt -> translateComparison "setl" dest src1 src2
    Le -> translateComparison "setle" dest src1 src2
    Gt -> translateComparison "setg" dest src1 src2
    Ge -> translateComparison "setge" dest src1 src2
    _ -> error $ "Unsupported binary operator in X86 backend: " ++ show op

translateInstruction (AAsmUnOp op dest src) = do
    destOp <- getOperand dest
    srcOp <- getOperand src
    if destOp /= srcOp then emit $ printf "\tmov %s, %s" destOp srcOp else return ()
    case op of
        Neg -> emit $ printf "\tneg %s" destOp
        BitNot -> emit $ printf "\tnot %s" destOp
        Not -> do
            emit $ printf "\tcmp %s, 0" destOp
            emit "\tsete al"
            emit "\tmovzx eax, al"
            emit $ printf "\tmov %s, eax" destOp
        _ -> error $ "Unsupported unary operator in X86 backend: " ++ show op

translateInstruction (AAsmAsgnOp op dest src) = case op of
    Add    -> translateBinOp "add"    dest dest src
    Sub    -> translateBinOp "sub"    dest dest src
    Mul    -> translateBinOp "imul"   dest dest src
    BitAnd -> translateBinOp "and"    dest dest src
    BitOr  -> translateBinOp "or"     dest dest src
    BitXor -> translateBinOp "xor"    dest dest src
    Shl    -> translateShift "sal"   dest dest src
    Shr    -> translateShift "sar"   dest dest src
    Div    -> translateDivMod "idiv"  dest dest src
    Mod    -> translateDivMod "idiv_mod" dest dest src
    _      -> error $ "Unsupported op-assign in X86 backend: " ++ show op

translateBinOp :: String -> Register -> Register -> Register -> X86 ()
translateBinOp instr dest src1 src2 = do
    destOp <- getOperand dest
    src1Op <- getOperand src1
    src2Op <- getOperand src2

    emit $ printf "\tmov eax, %s" src1Op

    if isMem src2Op then do
        emit $ printf "\tmov ecx, %s" src2Op
        emit $ printf "\t%s eax, ecx" instr
    else
        emit $ printf "\t%s eax, %s" instr src2Op
    
    if destOp /= "eax" then emit $ printf "\tmov %s, eax" destOp else return ()

translateComparison :: String -> Register -> Register -> Register -> X86 ()
translateComparison setInstr dest src1 src2 = do
    destOp <- getOperand dest
    src1Op <- getOperand src1
    src2Op <- getOperand src2

    emit $ printf "\tmov eax, %s" src1Op
    emit $ printf "\tcmp eax, %s" src2Op
    emit $ printf "\t%s al" setInstr
    emit "\tmovzx eax, al"
    if destOp /= "eax" then emit $ printf "\tmov %s, eax" destOp else return ()

translateDivMod :: String -> Register -> Register -> Register -> X86 ()
translateDivMod instr dest dividend divisor = do
    dividendOp <- getOperand dividend
    divisorOp <- getOperand divisor
    destOp <- getOperand dest

    emit "\tpush rax"
    emit "\tpush rdx"
    emit $ printf "\tmov eax, %s" dividendOp
    emit "\tcdq"
    if isImm divisorOp
      then do
          emit $ printf "\tmov ecx, %s" divisorOp
          emit "\tidiv ecx"
      else
          emit $ printf "\tidiv %s" divisorOp
    case instr of
        "idiv"     -> emit $ printf "\tmov %s, eax" destOp
        "idiv_mod" -> emit $ printf "\tmov %s, edx" destOp
        _          -> return ()
    emit "\tpop rdx"
    emit "\tpop rax"

translateShift :: String -> Register -> Register -> Register -> X86 ()
translateShift instr dest src1 src2 = do
    destOp <- getOperand dest
    src1Op <- getOperand src1
    src2Op <- getOperand src2

    emit $ printf "\tmov eax, %s" src1Op
    emit "\tpush rcx"
    emit $ printf "\tmov ecx, %s" src2Op
    emit $ printf "\t%s eax, cl" instr
    emit "\tpop rcx"

    if destOp /= "eax" then emit $ printf "\tmov %s, eax" destOp else return ()

translateTerminator :: Terminator -> X86 ()
translateTerminator (Ret reg) = do
    regOp <- getOperand reg
    if regOp /= "eax" then emit $ printf "\tmov eax, %s" regOp else return ()
    
    spillSlots <- gets spillCount
    let stackBytes = if spillSlots == 0 then 0 else ((spillSlots + 1) `div` 2) * 16
    let epilogue = genEpilogue stackBytes
    mapM_ emit epilogue

translateTerminator (Goto label) = emit $ printf "\tjmp %s" label

translateTerminator (CondGoto reg lTrue lFalse) = do
    regOp <- getOperand reg
    emit $ printf "\tcmp %s, 0" regOp
    emit $ printf "\tje %s" lTrue
    emit $ printf "\tjmp %s" lFalse

isMem :: String -> Bool
isMem = ("PTR" `elem`) . words

isImm :: String -> Bool
isImm s = case reads s :: [(Integer, String)] of
    [(_, "")] -> True
    _         -> False

to32BitRegName :: String -> String
to32BitRegName "rax" = "eax"; to32BitRegName "rbx" = "ebx"
to32BitRegName "rcx" = "ecx"; to32BitRegName "rdx" = "edx"
to32BitRegName "rsi" = "esi"; to32BitRegName "rdi" = "edi"
to32BitRegName "r8" = "r8d"; to32BitRegName "r9" = "r9d"
to32BitRegName "r10" = "r10d"; to32BitRegName "r11" = "r11d"
to32BitRegName "r12" = "r12d"; to32BitRegName "r13" = "r13d"
to32BitRegName "r14" = "r14d"; to32BitRegName "r15" = "r15d"
to32BitRegName "rbp" = "ebp"; to32BitRegName "rsp" = "esp"
to32BitRegName other = other