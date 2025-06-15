module Compile (
    Job (..),
    compile,
) where

import Compile.AAsm (codeGen, AAsmProgram, BasicBlock(..), Register)
import Compile.Parser (parseAST)
import Compile.Semantic (semanticAnalysis)
import Compile.X86 (generateX86)
import Error (C0ExceptT, generalFail)
import System.Exit (ExitCode (..))
import System.FilePath (replaceExtension)
import System.Process (system)

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))

import Compile.Liveness (livenessAnalysis, LiveOutMap)
import Compile.Interference (buildGraph, Graph)
import Compile.Coloring (colorGraph, Allocation)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (intercalate)

data Job = Job
    { src :: FilePath
    , out :: FilePath
    , astOut :: Maybe FilePath
    , aasmOut :: Maybe FilePath
    , livenessOut :: Maybe FilePath
    , graphOut :: Maybe FilePath
    , allocOut :: Maybe FilePath
    }
    deriving (Show)

compile :: Job -> C0ExceptT ()
compile job = do
    ast <- parseAST $ src job
    case astOut job of
        Just astFile -> liftIO $ writeFile astFile (show ast)
        Nothing -> return ()
    semanticAnalysis ast

    let aasmProg = codeGen ast
    case aasmOut job of
        Just aasmFile -> liftIO $ writeFile aasmFile (show aasmProg)
        Nothing -> return ()

    let liveness = livenessAnalysis aasmProg
    case livenessOut job of
        Just livenessFile -> liftIO $ writeFile livenessFile (showLiveness aasmProg liveness)
        Nothing -> return ()

    let graph = buildGraph aasmProg liveness
    case graphOut job of
        Just graphFile -> liftIO $ writeFile graphFile (showGraph graph)
        Nothing -> return ()

    let allocation = colorGraph graph
    case allocOut job of
        Just allocFile -> liftIO $ writeFile allocFile (show allocation)
        Nothing -> return ()

    let x86code = generateX86 aasmProg allocation

    let asmFile = replaceExtension (out job) "s"
    liftIO $ writeFile asmFile (unlines x86code)
    exitCode <- liftIO $ system $ "gcc " ++ asmFile ++ " -o " ++ out job
    when (exitCode /= ExitSuccess) $ generalFail "Assembly or linking failed" 1

showLiveness :: AAsmProgram -> LiveOutMap -> String
showLiveness prog liveMap = unlines $ map showBlock prog
  where
    showBlock :: BasicBlock -> String
    showBlock block =
        let label = blockLabel block
            liveOutSet = Map.findWithDefault Set.empty label liveMap
            liveOutStr = "\n  ; live-out: " ++ show (map regName (Set.toList liveOutSet))
        in show block ++ liveOutStr

showGraph :: Graph -> String
showGraph graph = unlines $ map showNode (Map.toList graph)
  where
    showNode :: (Register, Set.Set Register) -> String
    showNode (reg, neighbors) =
        regName reg ++ " -> " ++ (intercalate ", " . map regName . Set.toList) neighbors

regName :: Register -> String
regName n = "%" ++ show n