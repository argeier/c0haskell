module Compile.RegisterAllocation (
    allocateRegisters,
) where

import qualified Compile.X86 as X86
import Control.Monad.State (
    MonadState (get, put),
    State,
    evalState,
 )
import Data.List (nub)
import qualified Data.Map as Map

-- Virtual register identifier
type VirtualReg = Int

-- Physical register or stack location
data Location
    = PhysReg X86.Register
    | StackSlot Int -- Offset from base pointer
    deriving (Show, Eq)

-- Register allocation state
data AllocState = AllocState
    { regMap :: Map.Map VirtualReg Location -- Virtual to physical mapping
    , freeRegs :: [X86.Register] -- Available physical registers
    , nextOffset :: Int -- Next available stack offset
    , spilledRegs :: [VirtualReg] -- Which registers were spilled
    }
    deriving (Show)

-- Main entry point: allocate registers for a sequence of instructions
allocateRegisters :: [X86.Instruction] -> [X86.Instruction]
allocateRegisters instrs =
    evalState (allocateProgram instrs) initialState
  where
    initialState =
        AllocState
            { regMap = Map.empty
            , freeRegs = X86.allocatableRegisters
            , nextOffset = -8 -- Start at -8(%rbp)
            , spilledRegs = []
            }

-- Process entire instruction sequence
allocateProgram :: [X86.Instruction] -> State AllocState [X86.Instruction]
allocateProgram instrs = do
    -- First, collect all virtual registers
    let vregs = collectVirtualRegs instrs

    -- Allocate locations for all virtual registers
    mapM_ allocateLocation vregs

    -- Transform the instructions using the allocation
    processedInstrs <- mapM processInstruction instrs

    -- Get prologue and epilogue instructions
    prologue <- generatePrologue
    epilogue <- generateEpilogue

    -- Add debug info
    state <- get
    let debugInfo = [X86.Comment $ "Register allocation: " ++ show (regMap state)]

    -- Return complete transformed program with epilogue BEFORE any ret instructions
    return $ prologue ++ debugInfo ++ insertEpilogueBeforeRet (concat processedInstrs) epilogue

-- Insert epilogue before any ret instruction
insertEpilogueBeforeRet :: [X86.Instruction] -> [X86.Instruction] -> [X86.Instruction]
insertEpilogueBeforeRet [] epilogue = epilogue
insertEpilogueBeforeRet (instr : instrs) epilogue =
    case instr of
        X86.Ret -> epilogue ++ [X86.Ret] ++ insertEpilogueBeforeRet instrs []
        _ -> instr : insertEpilogueBeforeRet instrs epilogue

-- Collect all virtual registers used in the program
collectVirtualRegs :: [X86.Instruction] -> [VirtualReg]
collectVirtualRegs instrs = nub $ concatMap getVRegsFromInstr instrs
  where
    getVRegsFromInstr :: X86.Instruction -> [VirtualReg]
    getVRegsFromInstr instr = case instr of
        X86.MovQ src dst -> getVRegsFromOp src ++ getVRegsFromOp dst
        X86.AddQ src dst -> getVRegsFromOp src ++ getVRegsFromOp dst
        X86.SubQ src dst -> getVRegsFromOp src ++ getVRegsFromOp dst
        X86.ImulQ src dst -> getVRegsFromOp src ++ getVRegsFromOp dst
        X86.IdivQ op -> getVRegsFromOp op
        X86.NegQ op -> getVRegsFromOp op
        X86.PushQ op -> getVRegsFromOp op
        X86.PopQ op -> getVRegsFromOp op
        X86.XorQ src dst -> getVRegsFromOp src ++ getVRegsFromOp dst
        X86.CmpQ src dst -> getVRegsFromOp src ++ getVRegsFromOp dst
        _ -> []

    getVRegsFromOp :: X86.Operand -> [VirtualReg]
    getVRegsFromOp op = case op of
        X86.Reg r -> case getVirtualRegFromPhysical r of
            Just vreg -> [vreg]
            Nothing -> []
        X86.Mem r _ -> case getVirtualRegFromPhysical r of
            Just vreg -> [vreg]
            Nothing -> []
        _ -> []

    -- Extract virtual register from physical register (if any)
    -- This is based on convention from instruction selection
    getVirtualRegFromPhysical :: X86.Register -> Maybe VirtualReg
    getVirtualRegFromPhysical reg =
        case reg of
            X86.RAX -> Just 0
            X86.RBX -> Just 1
            X86.RCX -> Just 2
            X86.RDX -> Just 3
            X86.RSI -> Just 4
            X86.RDI -> Just 5
            X86.R8 -> Just 6
            X86.R9 -> Just 7
            X86.R10 -> Just 8
            X86.R11 -> Just 9
            X86.R12 -> Just 10
            X86.R13 -> Just 11
            X86.R14 -> Just 12
            X86.R15 -> Just 13
            _ -> Nothing

-- Allocate a location for a virtual register
allocateLocation :: VirtualReg -> State AllocState ()
allocateLocation vreg = do
    state <- get

    -- Check if already allocated
    case Map.lookup vreg (regMap state) of
        Just _ -> return () -- Already allocated
        Nothing ->
            -- Try to allocate a physical register
            if null (freeRegs state)
                then do
                    -- Need to spill to stack
                    let offset = nextOffset state
                        newState =
                            state
                                { regMap = Map.insert vreg (StackSlot offset) (regMap state)
                                , nextOffset = offset - 8 -- Decrement by 8 bytes (size of a register)
                                , spilledRegs = vreg : spilledRegs state
                                }
                    put newState
                else do
                    -- Allocate a physical register
                    let physReg = head (freeRegs state)
                        newState =
                            state
                                { regMap = Map.insert vreg (PhysReg physReg) (regMap state)
                                , freeRegs = tail (freeRegs state)
                                }
                    put newState

-- Generate function prologue based on used stack space
generatePrologue :: State AllocState [X86.Instruction]
generatePrologue = do
    state <- get
    let stackSize = abs (nextOffset state) + 8 -- Amount of stack space needed
        alignedSize = ((stackSize + 15) `div` 16) * 16 -- Align to 16 bytes
    return
        [ X86.PushQ (X86.Reg X86.RBP)
        , X86.MovQ (X86.Reg X86.RSP) (X86.Reg X86.RBP)
        , X86.SubQ (X86.Imm alignedSize) (X86.Reg X86.RSP)
        , X86.Comment "Register allocation begins"
        ]

-- Generate function epilogue
generateEpilogue :: State AllocState [X86.Instruction]
generateEpilogue = do
    return
        [ X86.Comment "Register allocation ends"
        , X86.MovQ (X86.Reg X86.RBP) (X86.Reg X86.RSP)
        , X86.PopQ (X86.Reg X86.RBP)
        ]

-- Process an instruction to use allocated registers
processInstruction :: X86.Instruction -> State AllocState [X86.Instruction]
processInstruction instr = do
    case instr of
        X86.MovQ src dst -> do
            newSrc <- translateOperand src
            newDst <- translateOperand dst
            return [X86.MovQ newSrc newDst]
        X86.AddQ src dst -> do
            newSrc <- translateOperand src
            newDst <- translateOperand dst
            return [X86.AddQ newSrc newDst]
        X86.SubQ src dst -> do
            newSrc <- translateOperand src
            newDst <- translateOperand dst
            return [X86.SubQ newSrc newDst]
        X86.ImulQ src dst -> do
            newSrc <- translateOperand src
            newDst <- translateOperand dst
            return [X86.ImulQ newSrc newDst]
        X86.IdivQ op -> do
            newOp <- translateOperand op
            return [X86.IdivQ newOp]
        X86.Cqto ->
            return [X86.Cqto]
        X86.NegQ op -> do
            newOp <- translateOperand op
            return [X86.NegQ newOp]
        X86.XorQ src dst -> do
            newSrc <- translateOperand src
            newDst <- translateOperand dst
            return [X86.XorQ newSrc newDst]
        X86.CmpQ src dst -> do
            newSrc <- translateOperand src
            newDst <- translateOperand dst
            return [X86.CmpQ newSrc newDst]
        X86.PushQ op -> do
            newOp <- translateOperand op
            return [X86.PushQ newOp]
        X86.PopQ op -> do
            newOp <- translateOperand op
            return [X86.PopQ newOp]
        X86.Label lbl ->
            return [X86.Label lbl]
        X86.Ret ->
            return [X86.Ret]
        X86.Comment cmt ->
            return [X86.Comment cmt]
        _ ->
            return [X86.Comment $ "Unhandled instruction: " ++ show instr]

-- Translate operand based on register allocation
translateOperand :: X86.Operand -> State AllocState X86.Operand
translateOperand op = case op of
    X86.Reg reg -> do
        case getVirtualRegFromPhysical reg of
            Just vreg -> do
                state <- get
                case Map.lookup vreg (regMap state) of
                    Just (PhysReg physReg) ->
                        return $ X86.Reg physReg
                    Just (StackSlot offset) ->
                        return $ X86.Mem X86.RBP (Just offset)
                    Nothing ->
                        return op -- Unallocated register (probably fixed like RAX)
            Nothing ->
                return op -- Not a virtual register
    _ ->
        return op -- Other operands unchanged
  where
    -- Extract virtual register from physical register (same as above)
    getVirtualRegFromPhysical :: X86.Register -> Maybe VirtualReg
    getVirtualRegFromPhysical reg =
        case reg of
            X86.RAX -> Just 0
            X86.RBX -> Just 1
            X86.RCX -> Just 2
            X86.RDX -> Just 3
            X86.RSI -> Just 4
            X86.RDI -> Just 5
            X86.R8 -> Just 6
            X86.R9 -> Just 7
            X86.R10 -> Just 8
            X86.R11 -> Just 9
            X86.R12 -> Just 10
            X86.R13 -> Just 11
            X86.R14 -> Just 12
            X86.R15 -> Just 13
            _ -> Nothing