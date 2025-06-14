module Args (
    jobParser,
    validateJob,
) where

import Compile (Job (..), src)
import Error (C0ExceptT, generalFail)
import System.Directory (doesFileExist)

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Options.Applicative (
    Parser,
    ParserInfo,
    argument,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    optional,
    progDesc,
    str,
    strOption,
    (<**>),
 )

jobP :: Parser Job
jobP =
    Job
        <$> argument str (metavar "INPUT" <> help "Input file to process")
        <*> argument str (metavar "OUTPUT" <> help "Name for the output file")
        <*> optional
            ( strOption
                ( long "dump-ast"
                    <> metavar "AST_OUTPUT"
                    <> help "Dump AST to this file"
                )
            )
        <*> optional
            ( strOption
                ( long "dump-aasm"
                    <> metavar "AASM_OUTPUT"
                    <> help "Dump abstract assembly to this file"
                )
            )

jobParser :: ParserInfo Job
jobParser =
    info
        (jobP <**> helper)
        ( fullDesc
            <> progDesc "Compile C0 programs to a simple abstract assembly language"
            <> header "An simple starter compiler for the C0 language"
        )

validateJob :: Job -> C0ExceptT Job
validateJob job = do
    let sourceFile = src job
    exists <- liftIO $ doesFileExist sourceFile
    unless exists $ generalFail ("File " ++ sourceFile ++ " does not exist :(") 1
    return job
