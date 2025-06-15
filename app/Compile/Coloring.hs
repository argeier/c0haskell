module Compile.Coloring (
    colorGraph,
    Allocation,
    PhysicalRegister,
    SpillLocation
) where

import Compile.Interference (Graph, Register)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Maybe (fromJust, catMaybes)

-- Each virtual register maps to either a physical register or a stack slot
type PhysicalRegister = String
type SpillLocation = Int
type Allocation = Map.Map Register (Either SpillLocation PhysicalRegister)

-- General-purpose, callee-saved registers in x86-64 System V ABI
physicalRegisters :: [PhysicalRegister]
physicalRegisters = ["rbx", "r12", "r13", "r14", "r15"]

k :: Int
k = length physicalRegisters

colorGraph :: Graph -> Allocation
colorGraph graph =
    let -- Phase 1: Simplify the graph to build a stack of nodes to color
        (coloringStack, spilledNodes) = simplifyGraph graph
        -- Phase 2: Assign colors to nodes on the stack
        initialAllocation = Map.fromSet (const $ Left 0) spilledNodes
    in selectPhase graph coloringStack initialAllocation

simplifyGraph :: Graph -> ([Register], Set.Set Register)
simplifyGraph initialGraph = go initialGraph [] Set.empty
  where
    go :: Graph -> [Register] -> Set.Set Register -> ([Register], Set.Set Register)
    go graph stack spills
        | Map.null graph = (stack, spills)
        | otherwise =
            case findSimplifiableNode graph of
                Just nodeToSimplify ->
                    let newGraph = removeNode nodeToSimplify graph
                        newStack = nodeToSimplify : stack
                    in go newGraph newStack spills
                Nothing ->
                    let nodeToSpill = chooseSpillCandidate graph
                        newGraph = removeNode nodeToSpill graph
                        newStack = nodeToSpill : stack
                        newSpills = Set.insert nodeToSpill spills
                    in go newGraph newStack newSpills

findSimplifiableNode :: Graph -> Maybe Register
findSimplifiableNode = fst . Map.foldlWithKey' findNode (Nothing, k)
  where
    findNode acc@(_, bestDegree) node neighbors =
        let degree = Set.size neighbors
        in if degree < bestDegree
           then (Just node, degree)
           else acc

-- Heuristic: pick the node with the highest degree
chooseSpillCandidate :: Graph -> Register
chooseSpillCandidate graph =
    case sortBy (flip $ comparing snd) $ Map.toList $ Map.map Set.size graph of
      ((node, _):_) -> node
      [] -> error "chooseSpillCandidate called on an empty graph"

removeNode :: Register -> Graph -> Graph
removeNode node graph =
    let neighbors = fromJust $ Map.lookup node graph
        graphWithoutNode = Map.delete node graph
    in foldr (\neighbor g -> Map.adjust (Set.delete node) neighbor g) graphWithoutNode neighbors

selectPhase :: Graph -> [Register] -> Allocation -> Allocation
selectPhase _ [] allocation = assignSpillLocations allocation
selectPhase originalGraph (reg : regs) initialAllocation =
    let isPreSpilled = case Map.lookup reg initialAllocation of
                         Just (Left _) -> True
                         _             -> False

        newAllocation = if isPreSpilled
            then initialAllocation
            else
                let neighborColors = getNeighborColors reg initialAllocation
                    maybeColor = findColor neighborColors
                in case maybeColor of
                    Just color -> Map.insert reg (Right color) initialAllocation
                    Nothing -> Map.insert reg (Left 0) initialAllocation

    in selectPhase originalGraph regs newAllocation
  where
    getNeighborColors :: Register -> Allocation -> Set.Set PhysicalRegister
    getNeighborColors r alloc =
        let neighborNodes = fromJust $ Map.lookup r originalGraph
            allocations = catMaybes $ map (`Map.lookup` alloc) (Set.toList neighborNodes)
        in Set.fromList [c | Right c <- allocations]

findColor :: Set.Set PhysicalRegister -> Maybe PhysicalRegister
findColor usedColors =
    let available = filter (`Set.notMember` usedColors) physicalRegisters
    in case available of
        (c:_) -> Just c
        []    -> Nothing

-- Assign concrete stack slots to spilled nodes
assignSpillLocations :: Allocation -> Allocation
assignSpillLocations allocation =
    let (spilled, colored) = Map.partition isSpill allocation
        spillSlots = zip (Map.keys spilled) [1..]
        spillAllocation = Map.fromList $ map (\(r, i) -> (r, Left i)) spillSlots
    in colored `Map.union` spillAllocation
  where
    isSpill (Left _) = True
    isSpill _        = False