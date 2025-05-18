module Compile (
    Job (..),
    compile,
) where

import Compile.AAsm (codeGen)
import Compile.AST (AST)
import Compile.InstructionSelection (selectInstructions)
import Compile.Parser (parseAST)
import Compile.RegisterAllocation (allocateRegisters)
import Compile.Semantic (semanticAnalysis)
import Compile.X86 (X86Program (..), emitProgram)
import Error (L1ExceptT)

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))

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
    forM_ (astOut job) $ \astFile -> do
        liftIO $ putStrLn $ "Writing AST to " ++ astFile
        writeAST ast astFile
    semanticAnalysis ast
    let abstractAsm = codeGen ast
    forM_ (aasmOut job) $ \aasmFile -> do
        liftIO $ putStrLn $ "Writing Abstract Assembly to " ++ aasmFile
        liftIO $ writeFile aasmFile (unlines abstractAsm)
    let x86Instructions = selectInstructions abstractAsm
    let allocatedInstructions = allocateRegisters x86Instructions
    let program =
            X86Program
                { globals = ["_main"]
                , text = allocatedInstructions
                }
    let assembly = emitProgram program
    let asmFile = out job ++ ".s"
    liftIO $ writeFile asmFile assembly
    liftIO $ putStrLn $ "Assembly written to " ++ asmFile
    liftIO $ putStrLn $ "To compile: gcc -o " ++ out job ++ " " ++ asmFile

    return ()

writeAST :: AST -> FilePath -> L1ExceptT ()
writeAST ast file = liftIO $ do
    putStrLn $ "Writing AST to file: " ++ file
    writeFile file (show ast)