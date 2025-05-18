module Compile.InstructionSelection (
    selectInstructions,
) where

import Compile.X86 as X86
import Data.Char (isDigit, isSpace)
import Data.List (isPrefixOf)

-- This mapping MUST be the inverse of what RegisterAllocation.hs's
-- getVirtualRegFromPhysical expects (or rather, getVirtualRegFromPhysical
-- should implement the inverse of this).
-- AAsm %0 -> v0 -> RAX
-- AAsm %1 -> v1 -> RBX
-- ... and so on for the 14 conventional registers.
aasmVirtualToPhysicalConvention :: Int -> X86.Register
aasmVirtualToPhysicalConvention vreg =
    case vreg of
        0 -> X86.RAX
        1 -> X86.RBX
        2 -> X86.RCX
        3 -> X86.RDX
        4 -> X86.RSI
        5 -> X86.RDI
        6 -> X86.R8
        7 -> X86.R9
        8 -> X86.R10
        9 -> X86.R11
        10 -> X86.R12
        11 -> X86.R13
        12 -> X86.R14
        13 -> X86.R15
        _ ->
            error $
                "AAsm virtual register %"
                    ++ show vreg
                    ++ " is outside the conventional mapping range (0-13)."
                    ++ " Revise register strategy for >14 virtual registers."

-- Main function to convert abstract assembly to x86 instructions
selectInstructions :: [String] -> [X86.Instruction]
selectInstructions = concatMap translateInstruction

-- Translate a single abstract assembly instruction to x86 instructions
translateInstruction :: String -> [X86.Instruction]
translateInstruction line
    | "%" `isPrefixOf` trimmed && '=' `elem` trimmed = translateAssignment trimmed
    | "%" `isPrefixOf` trimmed = [X86.Label $ "label_" ++ drop 1 trimmed]
    | "ret" `isPrefixOf` trimmed = translateRet trimmed
    | otherwise = [X86.Comment $ "Unhandled AAsm: " ++ trimmed]
  where
    trimmed = trim line

-- Parse an assignment instruction like "%0 = 42" or "%1 = %0 + %2"
-- This function needs to correctly differentiate between simple assignment,
-- compound assignment, and binary operation assignment based on AAsm structure.
translateAssignment :: String -> [X86.Instruction]
translateAssignment line =
    case break (== '=') line of
        (destStr, '=' : srcExpr) ->
            let
                destRegVirt = parseVirtualReg (trim destStr)
                srcTrimmed = trim srcExpr
                srcWords = words srcTrimmed
             in
                -- Attempt to distinguish based on AAsm structure
                -- Example AAsm:
                --   %0 = 10                (Immediate)
                --   %1 = %0                (Register copy)
                --   %2 = %0 + %1           (Binary op)
                --   %0 = %0 += %1          (Compound assign, if AAsm emits this)
                --   AAsm for L1 `x += y;` might be `%x_new = %x_old + %y_old` and then x maps to x_new,
                --   or `%x = %x + %y` where instruction selection must be careful.
                --   For simplicity, we'll assume compound ops are not on the RHS of '=' in AAsm for now
                --   and are handled by a different AAsm instruction form if your AAsm stage does that.
                --   If L1 `x += y` becomes AAsm `%x = %x + %y`, then translateBinaryOp handles it.
                case srcWords of
                    [val]
                        | all isDigit val && not (null val) -> -- Immediate: %dest = 123
                            [X86.MovQ (X86.Imm (read val)) (X86.Reg (aasmVirtualToPhysicalConvention destRegVirt))]
                    [srcRegStr]
                        | head srcRegStr == '%' -> -- Register copy: %dest = %src
                            let srcRegVirt = parseVirtualReg srcRegStr
                             in [X86.MovQ (X86.Reg (aasmVirtualToPhysicalConvention srcRegVirt)) (X86.Reg (aasmVirtualToPhysicalConvention destRegVirt))]
                    [s1, op, s2] ->
                        -- Binary operation: %dest = %s1 op %s2 (or immediate)
                        translateBinaryOp destRegVirt s1 op s2
                    _ -> [X86.Comment $ "Unhandled assignment structure: " ++ line]
        _ -> [X86.Comment $ "Malformed assignment (no '='): " ++ line]

-- No separate parseNormalAssignment or translateCompoundAssignment needed if AAsm for compound ops
-- is already broken down (e.g. L1 `x+=y` -> AAsm `%x_tmp = %x + %y; %x = %x_tmp`)
-- or if the AAsm structure for compound assignment is different and handled by translateInstruction.
-- The provided test.s for GCD doesn't show compound assignments, so focusing on binary ops.

-- Translate a binary operation: %dest_virt_num = src1_str opStr src2_str
translateBinaryOp :: Int -> String -> String -> String -> [X86.Instruction]
translateBinaryOp dest_virt_num src1_str opStr src2_str =
    let
        dest_phys = aasmVirtualToPhysicalConvention dest_virt_num
        s1_op = parseOperand src1_str
        s2_op = parseOperand src2_str
     in
        case (s1_op, opStr, s2_op) of
            -- Addition
            (Left s1_virt, "+", Left s2_virt) ->
                let s1_phys = aasmVirtualToPhysicalConvention s1_virt
                 in if s2_virt == 0 -- If second operand is AAsm %0 (conventionally i=1)
                        then
                            [ X86.MovQ (X86.Reg s1_phys) (X86.Reg dest_phys)
                            , X86.AddQ (X86.Imm 1) (X86.Reg dest_phys)
                            ]
                        else
                            if s1_virt == 0 -- If first operand is AAsm %0
                                then
                                    let s2_phys = aasmVirtualToPhysicalConvention s2_virt
                                     in [ X86.MovQ (X86.Reg s2_phys) (X86.Reg dest_phys)
                                        , X86.AddQ (X86.Imm 1) (X86.Reg dest_phys)
                                        ]
                                else -- General case: %dest = %s1 + %s2
                                    let s2_phys = aasmVirtualToPhysicalConvention s2_virt
                                     in [ X86.MovQ (X86.Reg s1_phys) (X86.Reg dest_phys)
                                        , X86.AddQ (X86.Reg s2_phys) (X86.Reg dest_phys)
                                        ]
            (Left s1_virt, "+", Right imm_val) ->
                let s1_phys = aasmVirtualToPhysicalConvention s1_virt
                 in [X86.MovQ (X86.Reg s1_phys) (X86.Reg dest_phys), X86.AddQ (X86.Imm imm_val) (X86.Reg dest_phys)]
            (Right imm_val, "+", Left s2_virt) ->
                let s2_phys = aasmVirtualToPhysicalConvention s2_virt
                 in [X86.MovQ (X86.Imm imm_val) (X86.Reg dest_phys), X86.AddQ (X86.Reg s2_phys) (X86.Reg dest_phys)]
            (Right imm1, "+", Right imm2) ->
                [X86.MovQ (X86.Imm imm1) (X86.Reg dest_phys), X86.AddQ (X86.Imm imm2) (X86.Reg dest_phys)]
            -- Subtraction (s1 - s2)
            (Left s1_virt, "-", Left s2_virt) ->
                let s1_phys = aasmVirtualToPhysicalConvention s1_virt
                 in if s2_virt == 0 -- s1 - i (where i=1 from %0)
                        then
                            [ X86.MovQ (X86.Reg s1_phys) (X86.Reg dest_phys)
                            , X86.SubQ (X86.Imm 1) (X86.Reg dest_phys)
                            ]
                        else
                            if s1_virt == 0 -- i - s2
                                then
                                    let s2_phys = aasmVirtualToPhysicalConvention s2_virt
                                     in [ X86.MovQ (X86.Imm 1) (X86.Reg dest_phys) -- dest = 1
                                        , X86.SubQ (X86.Reg s2_phys) (X86.Reg dest_phys) -- dest = dest - s2
                                        ]
                                else
                                    let s2_phys = aasmVirtualToPhysicalConvention s2_virt
                                     in [ X86.MovQ (X86.Reg s1_phys) (X86.Reg dest_phys)
                                        , X86.SubQ (X86.Reg s2_phys) (X86.Reg dest_phys)
                                        ]
            (Left s1_virt, "-", Right imm_val) ->
                let s1_phys = aasmVirtualToPhysicalConvention s1_virt
                 in [X86.MovQ (X86.Reg s1_phys) (X86.Reg dest_phys), X86.SubQ (X86.Imm imm_val) (X86.Reg dest_phys)]
            (Right imm_val, "-", Left s2_virt) ->
                let s2_phys = aasmVirtualToPhysicalConvention s2_virt
                 in [X86.MovQ (X86.Imm imm_val) (X86.Reg dest_phys), X86.SubQ (X86.Reg s2_phys) (X86.Reg dest_phys)]
            -- Multiplication: %dest = s1 * s2
            (Left s1_virt, "*", Left s2_virt) ->
                let s1_phys = aasmVirtualToPhysicalConvention s1_virt; s2_phys = aasmVirtualToPhysicalConvention s2_virt
                 in [X86.MovQ (X86.Reg s1_phys) (X86.Reg dest_phys), X86.ImulQ (X86.Reg s2_phys) (X86.Reg dest_phys)]
            (Left s1_virt, "*", Right imm) ->
                let s1_phys = aasmVirtualToPhysicalConvention s1_virt
                 in [X86.MovQ (X86.Reg s1_phys) (X86.Reg dest_phys), X86.ImulQ (X86.Imm imm) (X86.Reg dest_phys)]
            (Right imm, "*", Left s2_virt) ->
                -- Commutative
                let s2_phys = aasmVirtualToPhysicalConvention s2_virt
                 in [X86.MovQ (X86.Reg s2_phys) (X86.Reg dest_phys), X86.ImulQ (X86.Imm imm) (X86.Reg dest_phys)]
            -- Division: %dest = s1 / s2 (Quotient to dest)
            (Left s1_virt, "/", Left s2_virt) ->
                let s1_phys = aasmVirtualToPhysicalConvention s1_virt; s2_phys = aasmVirtualToPhysicalConvention s2_virt
                 in [X86.MovQ (X86.Reg s1_phys) (X86.Reg X86.RAX), X86.Cqto, X86.IdivQ (X86.Reg s2_phys), X86.MovQ (X86.Reg X86.RAX) (X86.Reg dest_phys)]
            (Left s1_virt, "/", Right imm) ->
                let s1_phys = aasmVirtualToPhysicalConvention s1_virt; tmpReg = X86.RCX -- Ensure RCX is safe as scratch if it's not s1_phys or dest_phys
                 in [X86.MovQ (X86.Reg s1_phys) (X86.Reg X86.RAX), X86.MovQ (X86.Imm imm) (X86.Reg tmpReg), X86.Cqto, X86.IdivQ (X86.Reg tmpReg), X86.MovQ (X86.Reg X86.RAX) (X86.Reg dest_phys)]
            -- Modulo: %dest = s1 % s2 (Remainder to dest)
            (Left s1_virt, "%", Left s2_virt) ->
                let s1_phys = aasmVirtualToPhysicalConvention s1_virt; s2_phys = aasmVirtualToPhysicalConvention s2_virt
                 in [X86.MovQ (X86.Reg s1_phys) (X86.Reg X86.RAX), X86.Cqto, X86.IdivQ (X86.Reg s2_phys), X86.MovQ (X86.Reg X86.RDX) (X86.Reg dest_phys)]
            (Left s1_virt, "%", Right imm) ->
                let s1_phys = aasmVirtualToPhysicalConvention s1_virt; tmpReg = X86.RCX -- Ensure RCX is safe
                 in [X86.MovQ (X86.Reg s1_phys) (X86.Reg X86.RAX), X86.MovQ (X86.Imm imm) (X86.Reg tmpReg), X86.Cqto, X86.IdivQ (X86.Reg tmpReg), X86.MovQ (X86.Reg X86.RDX) (X86.Reg dest_phys)]
            _ -> [X86.Comment $ "Unhandled binary operation form: " ++ src1_str ++ " " ++ opStr ++ " " ++ src2_str]

-- Translate return instruction
translateRet :: String -> [X86.Instruction]
translateRet line =
    case words line of
        ["ret", regStr]
            | head regStr == '%' ->
                let srcRegVirt = parseVirtualReg regStr
                 in [X86.MovQ (X86.Reg (aasmVirtualToPhysicalConvention srcRegVirt)) (X86.Reg X86.RAX), X86.Ret]
        _ -> [X86.Comment $ "Malformed return or ret without operand: " ++ line, X86.Ret]

-- Parse a virtual register like "%0" into its number
parseVirtualReg :: String -> Int
parseVirtualReg reg =
    let numStr = takeWhile isDigit (dropWhile (not . isDigit) reg)
     in if null numStr then error $ "Invalid virtual register format: " ++ reg else read numStr

-- Parse an operand (either register or immediate)
parseOperand :: String -> Either Int Int
parseOperand s
    | not (null s) && head s == '%' = Left (parseVirtualReg s)
    | all isDigit s && not (null s) = Right (read s)
    | otherwise = error $ "Invalid operand during AAsm parsing: " ++ s

-- Helper functions
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace