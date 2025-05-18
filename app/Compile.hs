module Compile (
    Job (..),
    compile,
) where

import Compile.AAsm (codeGen)
import Compile.Parser (parseAST)
import Compile.Semantic (semanticAnalysis)
import Compile.X86 (generateX86)
import Error (L1ExceptT, generalFail)
import System.Exit (ExitCode (..))
import System.FilePath (replaceExtension)
import System.Process (system)

import Control.Monad (when)
import Control.Monad.IO.Class

data Job = Job
    { src :: FilePath
    , out :: FilePath
    , astOut :: Maybe FilePath
    , aasmOut :: Maybe FilePath
    }
    deriving (Show)

compile :: Job -> L1ExceptT ()
compile job = do
    ast <- parseAST $ src job
    case astOut job of
        Just astFile -> liftIO $ writeFile astFile (show ast)
        Nothing -> return ()
    semanticAnalysis ast
    let aasm = codeGen ast
    case aasmOut job of
        Just aasmFile -> liftIO $ writeFile aasmFile (unlines aasm)
        Nothing -> return ()
    let x86code = generateX86 aasm
    let asmFile = replaceExtension (out job) "s"
    liftIO $ writeFile asmFile (unlines x86code)
    -- Assemble and link using gcc
    exitCode <- liftIO $ system $ "gcc " ++ asmFile ++ " -o " ++ out job
    when (exitCode /= ExitSuccess) $ generalFail "Assembly or linking failed" 1
