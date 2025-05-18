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
    }
    deriving (Show)

compile :: Job -> L1ExceptT ()
compile job = do
    ast <- parseAST $ src job
    semanticAnalysis ast
    let aasm = codeGen ast
    let x86code = generateX86 aasm
    let asmFile = replaceExtension (out job) "s"
    liftIO $ writeFile asmFile (unlines x86code)
    -- Assemble and link using gcc
    exitCode <- liftIO $ system $ "gcc " ++ asmFile ++ " -o " ++ out job
    when (exitCode /= ExitSuccess) $ generalFail "Assembly or linking failed" 1
