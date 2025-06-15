module Compile.Liveness (
    livenessAnalysis,
    LiveOutMap,
    Register
) where

import Compile.AAsm (AAsmProgram, BasicBlock(..), Terminator(..), AAsmInstruction(..), Register, Label)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type LiveOutMap = Map.Map Label (Set.Set Register)
type CFG = Map.Map Label BasicBlock

data UseDef = UseDef
    { useSet :: Set.Set Register
    , defSet :: Set.Set Register
    } deriving (Show, Eq)

type UseDefMap = Map.Map Label UseDef

livenessAnalysis :: AAsmProgram -> LiveOutMap
livenessAnalysis prog =
    let cfg = buildCFG prog
        useDefMap = Map.map calculateUseDef cfg
        initialLiveOut = Map.map (const Set.empty) cfg
    in solveLiveness initialLiveOut cfg useDefMap

buildCFG :: AAsmProgram -> CFG
buildCFG prog = Map.fromList $ map (\block -> (blockLabel block, block)) prog

calculateUseDef :: BasicBlock -> UseDef
calculateUseDef block =
    let termUses = getTermUse (blockTerminator block)
        (use, def) = foldr processInstruction (termUses, Set.empty) (blockInstructions block)
    in UseDef use def
  where
    processInstruction :: AAsmInstruction -> (Set.Set Register, Set.Set Register) -> (Set.Set Register, Set.Set Register)
    processInstruction instr (currentUse, currentDef) =
        let instrUse = getUse instr
            instrDef = getDef instr
            newUse = instrUse `Set.union` (currentUse `Set.difference` instrDef)
            newDef = instrDef `Set.union` currentDef
        in (newUse, newDef)

getUse :: AAsmInstruction -> Set.Set Register
getUse (AAsmBinOp _ _ src1 src2) = Set.fromList [src1, src2]
getUse (AAsmUnOp _ _ src)        = Set.singleton src
getUse (AAsmAsgnOp _ dest src)   = Set.fromList [dest, src]
getUse (AAsmMove _ src)          = Set.singleton src
getUse (AAsmLoadImm _ _)         = Set.empty

getTermUse :: Terminator -> Set.Set Register
getTermUse (Ret reg) = Set.singleton reg
getTermUse (CondGoto reg _ _) = Set.singleton reg
getTermUse (Goto _) = Set.empty

getDef :: AAsmInstruction -> Set.Set Register
getDef (AAsmBinOp _ res _ _)   = Set.singleton res
getDef (AAsmUnOp _ res _)      = Set.singleton res
getDef (AAsmAsgnOp _ dest _)   = Set.singleton dest
getDef (AAsmMove dest _)       = Set.singleton dest
getDef (AAsmLoadImm dest _)    = Set.singleton dest

solveLiveness :: LiveOutMap -> CFG -> UseDefMap -> LiveOutMap
solveLiveness currentLiveOut cfg useDefMap =
    let newLiveOut = stepLiveness currentLiveOut cfg useDefMap
    in if newLiveOut == currentLiveOut
       then newLiveOut
       else solveLiveness newLiveOut cfg useDefMap

stepLiveness :: LiveOutMap -> CFG -> UseDefMap -> LiveOutMap
stepLiveness currentLiveOut cfg useDefMap =
    Map.map calculateLiveOut cfg
  where
    calculateLiveOut :: BasicBlock -> Set.Set Register
    calculateLiveOut block =
        let successorLabels = getSuccessors (blockTerminator block)
            liveInsOfSuccessors = map getLiveInOfSuccessor successorLabels
        in Set.unions liveInsOfSuccessors

    getLiveInOfSuccessor :: Label -> Set.Set Register
    getLiveInOfSuccessor succLabel =
        let succUseDef = useDefMap Map.! succLabel
            succLiveOut = currentLiveOut Map.! succLabel
        in useSet succUseDef `Set.union` (succLiveOut `Set.difference` defSet succUseDef)

getSuccessors :: Terminator -> [Label]
getSuccessors (Ret _) = []
getSuccessors (Goto l) = [l]
getSuccessors (CondGoto _ lTrue lFalse) = [lTrue, lFalse]