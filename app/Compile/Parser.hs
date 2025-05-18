module Compile.Parser (
    parseAST,
    parseNumber,
) where

import Compile.AST (AST (..), Expr (..), Op (..), Stmt (..))
import Error (L1ExceptT, parserFail)

import Control.Monad.Combinators.Expr (
    Operator (InfixL, Prefix),
    makeExprParser,
 )
import Control.Monad.IO.Class (liftIO)
import Data.Char (isAlpha, isAlphaNum, isAscii)
import Data.Functor (void)
import Data.Int (Int32)
import Data.Void (Void)
import Numeric (showHex)

import Text.Megaparsec (
    MonadParsec (eof, notFollowedBy, try),
    Parsec,
    between,
    choice,
    chunk,
    errorBundlePretty,
    getSourcePos,
    many,
    oneOf,
    parse,
    satisfy,
    some,
    (<?>),
    (<|>),
 )
import Text.Megaparsec.Char (
    char,
    digitChar,
    hexDigitChar,
    string,
 )
import qualified Text.Megaparsec.Char.Lexer as L

parseAST :: FilePath -> L1ExceptT AST
parseAST path = do
    text <- liftIO $ readFile path
    case parse astParser path text of
        Left err -> parserFail $ errorBundlePretty err
        Right ast -> return ast

parseNumber :: String -> Either String Integer
parseNumber s = do
    case parse number "<literal>" s of
        Left err -> Left $ errorBundlePretty err
        Right n -> Right n

type Parser = Parsec Void String

-- Custom whitespace parser that only accepts specific whitespace characters
whitespaceChar :: Parser Char
whitespaceChar =
    choice
        [ -- Only allow space, tab, newline, carriage return
          char ' '
        , char '\t'
        , char '\n'
        , char '\r'
        ]
        <?> "whitespace character"

whitespace :: Parser ()
whitespace = void $ some whitespaceChar

astParser :: Parser AST
astParser = do
    sc
    -- this parses `int main()` literally, like in the L1 grammar
    reserved "int"
    reserved "main"
    parens $ pure ()
    mainBlock <- braces $ do
        pos <- getSourcePos
        stmts <- many stmt
        return $ Block stmts pos
    sc -- Consume trailing whitespace
    eof
    return mainBlock

stmt :: Parser Stmt
stmt = do
    s <- try decl <|> try simp <|> ret
    semi
    return s

decl :: Parser Stmt
decl = try declInit <|> declNoInit

declNoInit :: Parser Stmt
declNoInit = do
    pos <- getSourcePos
    reserved "int"
    name <- identifier
    return $ Decl name pos

declInit :: Parser Stmt
declInit = do
    pos <- getSourcePos
    reserved "int"
    name <- identifier
    void $ symbol "="
    e <- expr
    return $ Init name e pos

simp :: Parser Stmt
simp = do
    pos <- getSourcePos
    name <- lvalue
    op <- asnOp
    e <- expr
    return $ Asgn name op e pos

asnOp :: Parser (Maybe Op)
asnOp =
    do
        op <- operator
        case op of
            "+=" -> pure (Just Add)
            "*=" -> pure (Just Mul)
            "-=" -> pure (Just Sub)
            "/=" -> pure (Just Div)
            "%=" -> pure (Just Mod)
            "=" -> pure Nothing
            x -> fail $ "Nonexistent assignment operator: " ++ x
        <?> "assignment operator"

ret :: Parser Stmt
ret = do
    pos <- getSourcePos
    reserved "return"
    e <- expr
    return $ Ret e pos

expr' :: Parser Expr
expr' = parens expr <|> intExpr <|> identExpr

intExpr :: Parser Expr
intExpr = do
    pos <- getSourcePos
    str <- numberLiteral
    return $ IntExpr str pos

identExpr :: Parser Expr
identExpr = do
    pos <- getSourcePos
    name <- identifier
    return $ Ident name pos

opTable :: [[Operator Parser Expr]]
opTable =
    [ [Prefix manyUnaryOp]
    ,
        [ InfixL (BinExpr Mul <$ symbol "*")
        , InfixL (BinExpr Div <$ symbol "/")
        , InfixL (BinExpr Mod <$ symbol "%")
        ]
    , [InfixL (BinExpr Add <$ symbol "+"), InfixL (BinExpr Sub <$ symbol "-")]
    ]
  where
    -- this allows us to parse `---x` as `-(-(-x))`
    -- makeExprParser doesn't do this by default
    manyUnaryOp = foldr1 (.) <$> some (UnExpr Neg <$ symbol "-")

expr :: Parser Expr
expr = makeExprParser expr' opTable <?> "expression"

-- Lexer starts here, probably worth moving to its own file at some point
sc :: Parser ()
sc = L.space whitespace lineComment blockComment
  where
    lineComment = L.skipLineComment "//"
    blockComment = L.skipBlockCommentNested "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

semi :: Parser ()
semi = void $ symbol ";"

numberLiteral :: Parser String
numberLiteral = lexeme (try hexLiteral <|> decLiteral <?> "number")

-- We want to reject leading zeroes, but `0` itself should of course be accepted
decLiteral :: Parser String
decLiteral = string "0" <|> (:) <$> oneOf ['1' .. '9'] <*> many digitChar

hexLiteral :: Parser String
hexLiteral = do
    void $ (chunk "0x" <|> chunk "0X")
    digits <- some hexDigitChar
    return ("0x" ++ digits)

number :: Parser Integer
number = try hexadecimal <|> decimal <?> "number"

decimal :: Parser Integer
decimal = do
    n <- lexeme L.decimal
    notFollowedBy asciiAlphaNumChar
    if n < maxInt
        then return n
        else
            if n == maxInt
                then return (-maxInt)
                else fail $ "Decimal literal out of bounds: " ++ show n
  where
    maxInt = 2 ^ (31 :: Integer)

hexadecimal :: Parser Integer
hexadecimal = do
    void $ chunk "0x"
    n <- lexeme L.hexadecimal
    if n > maxHex
        then fail $ "Hexadecimal literal out of bounds: " ++ "0x" ++ showHex n ""
        else return $ toInteger (fromInteger n :: Int32)
  where
    maxHex = 0xFFFFFFFF

reserved :: String -> Parser ()
reserved w = void $ lexeme (string w <* notFollowedBy identLetter)

reservedWords :: [String]
reservedWords =
    [ "alloc"
    , "alloc_array"
    , "assert"
    , "bool"
    , "break"
    , "char"
    , "continue"
    , "else"
    , "false"
    , "for"
    , "if"
    , "int"
    , "NULL"
    , "print"
    , "read"
    , "return"
    , "string"
    , "struct"
    , "true"
    , "void"
    , "while"
    ]

-- Operations
opStart :: Parser Char
opStart = oneOf "=+-*/%&^|<>!~"

opLetter :: Parser Char
opLetter = oneOf "=&|<>"

operator :: Parser String
operator = lexeme ((:) <$> opStart <*> many opLetter)

-- ASCII-only character parsers
asciiLetterChar :: Parser Char
asciiLetterChar = satisfy (\c -> isAscii c && isAlpha c) <?> "ASCII letter"

asciiAlphaNumChar :: Parser Char
asciiAlphaNumChar = satisfy (\c -> isAscii c && isAlphaNum c) <?> "ASCII letter or digit"

-- Identifiers
identStart :: Parser Char
identStart = asciiLetterChar <|> char '_'

identLetter :: Parser Char
identLetter = asciiAlphaNumChar <|> char '_'

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> identStart <*> many identLetter
    check x =
        if x `elem` reservedWords
            then fail (x ++ " is reserved")
            else return x

lvalue :: Parser String
lvalue = try identifier <|> parens lvalue <?> "lvalue"