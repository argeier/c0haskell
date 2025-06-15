module Compile.Interference (
    buildGraph,
    Graph,
    Register
) where

import Compile.AAsm (AAsmProgram, BasicBlock(..), Terminator(..), AAsmInstruction(..), Register)
import Compile.Liveness (LiveOutMap)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- Adjacency list: each register maps to a set of registers it interferes with
type Graph = Map.Map Register (Set.Set Register)

buildGraph :: AAsmProgram -> LiveOutMap -> Graph
buildGraph prog liveOutMap =
    let allPairs = concatMap (processBlock liveOutMap) prog
        allRegisters = collectAllRegisters prog
        graphWithEdges = buildSymmetricGraph allPairs
    -- Ensure every register has an entry, even if it has no interferences
    in ensureAllNodes (Map.keysSet allRegisters) graphWithEdges

processBlock :: LiveOutMap -> BasicBlock -> [(Register, Register)]
processBlock liveOutMap block =
    let -- Start backward walk with live-out registers
        live = liveOutMap Map.! (blockLabel block)
        (termPairs, liveAfterTerm) = processTerminator live (blockTerminator block)
        (instrPairs, _) = foldr processInstruction ([], liveAfterTerm) (blockInstructions block)
    in termPairs ++ instrPairs

-- Returns interference pairs and live set *before* this instruction
processInstruction :: AAsmInstruction -> ([(Register, Register)], Set.Set Register) -> ([(Register, Register)], Set.Set Register)
processInstruction instr (pairs, live) =
    let defs = getDef instr
        uses = getUse instr
        isMove = case instr of AAsmMove _ _ -> True; _ -> False

        newPairs = do
            d <- Set.toList defs
            l <- Set.toList live
            let isMoveSrc = isMove && l `Set.member` uses
            if d /= l && not isMoveSrc
                then return (d, l)
                else []
        liveBefore = uses `Set.union` (live `Set.difference` defs)
    in (newPairs ++ pairs, liveBefore)

-- Terminators only use registers, they don't define any
processTerminator :: Set.Set Register -> Terminator -> ([(Register, Register)], Set.Set Register)
processTerminator live term =
    let uses = case term of
            Ret reg -> Set.singleton reg
            CondGoto reg _ _ -> Set.singleton reg
            _ -> Set.empty
        liveBefore = uses `Set.union` live
    in ([], liveBefore)

getDef :: AAsmInstruction -> Set.Set Register
getDef (AAsmBinOp _ res _ _)   = Set.singleton res
getDef (AAsmUnOp _ res _)      = Set.singleton res
getDef (AAsmAsgnOp _ dest _)   = Set.singleton dest
getDef (AAsmMove dest _)       = Set.singleton dest
getDef (AAsmLoadImm dest _)    = Set.singleton dest

getUse :: AAsmInstruction -> Set.Set Register
getUse (AAsmBinOp _ _ src1 src2) = Set.fromList [src1, src2]
getUse (AAsmUnOp _ _ src)        = Set.singleton src
getUse (AAsmAsgnOp _ dest src)   = Set.fromList [dest, src]
getUse (AAsmMove _ src)          = Set.singleton src
getUse (AAsmLoadImm _ _)         = Set.empty

-- Builds symmetric graph from interference pairs
buildSymmetricGraph :: [(Register, Register)] -> Graph
buildSymmetricGraph pairs = foldl' addEdge Map.empty pairs
  where
    addEdge g (u, v) =
        let g' = Map.insertWith Set.union u (Set.singleton v) g
        in Map.insertWith Set.union v (Set.singleton u) g'

-- Gathers every register to ensure they all get a node in the graph
collectAllRegisters :: AAsmProgram -> Graph
collectAllRegisters = foldl' (flip (Map.unionWith Set.union)) Map.empty . map getBlockRegs
  where
    getBlockRegs block =
        let instrRegs = concatMap getInstrRegs (blockInstructions block)
            termRegs = getTermRegs (blockTerminator block)
        in Map.fromListWith Set.union [ (r, Set.empty) | r <- instrRegs ++ termRegs ]
    getInstrRegs instr = Set.toList (getDef instr `Set.union` getUse instr)
    getTermRegs term = case term of
        Ret r -> [r]; CondGoto r _ _ -> [r]; _ -> []

-- Adds empty adjacency sets for registers with no interferences
ensureAllNodes :: Set.Set Register -> Graph -> Graph
ensureAllNodes allNodes g = Map.union g (Map.fromSet (const Set.empty) allNodes)