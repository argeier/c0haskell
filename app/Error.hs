module Error (
    C0ExceptT,
    generalFail,
    parserFail,
    semanticFail,
    dieWithError,
) where

import Control.Monad.Except (ExceptT, throwError)
import qualified System.Exit as Exit
import System.IO (hPutStrLn, stderr)

-- Predefined exit codes signaling compiler status
parserErrorCode :: Int
parserErrorCode = 42

semanticErrorCode :: Int
semanticErrorCode = 7

-- Error message and exit code
data C0Error
    = Error String Int
    | ParserError String
    | SemanticError String
    deriving (Show)

type C0ExceptT = ExceptT C0Error IO

-- Convenienve functions to throw exceptions
generalFail :: String -> Int -> C0ExceptT a
generalFail msg code = throwError $ Error msg code

parserFail :: String -> C0ExceptT a
parserFail = throwError . ParserError

semanticFail :: String -> C0ExceptT a
semanticFail = throwError . SemanticError

-- Exit with an error message and a return code
dieWithError :: C0Error -> IO ()
dieWithError (Error msg code) = do
    hPutStrLn stderr msg
    Exit.exitWith $ Exit.ExitFailure code
dieWithError (ParserError msg) = dieWithError (Error msg parserErrorCode)
dieWithError (SemanticError msg) = dieWithError (Error msg semanticErrorCode)
