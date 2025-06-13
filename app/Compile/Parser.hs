module Compile.Parser (
    parseAST,
    parseNumber,
) where

import Compile.AST (AST (..), Expr (..), Op (..), Stmt (..), Type (..))
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
    optional,
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

whitespaceChar :: Parser Char
whitespaceChar =
    choice
        [ char ' '
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
    reserved "int"
    reserved "main"
    parens $ pure ()
    mainBlock <- braces $ do
        pos <- getSourcePos
        stmts <- many (stmt <?> "statement in main block")
        return $ Block stmts pos
    sc
    eof
    return mainBlock

parseType :: Parser Type
parseType = choice
    [ IntType <$ reserved "int"
    , BoolType <$ reserved "bool"
    ] <?> "type"

stmt :: Parser Stmt
stmt = choice
    [ try forStmt
    , try ifStmt
    , try whileStmt
    , try breakStmt
    , try continueStmt
    , try blockStmt
    , semicolonStmt
    ] <?> "statement"
  where
    semicolonStmt = do
        s <- choice [try decl, try simp, ret]
        semi
        return s

blockStmt :: Parser Stmt
blockStmt = do
    pos <- getSourcePos
    stmts <- braces (many stmt)
    return $ BlockStmt stmts pos

ifStmt :: Parser Stmt
ifStmt = do
    pos <- getSourcePos
    reserved "if"
    cond <- parens expr <?> "if condition"
    thenStmt <- stmt <?> "then statement"
    elseStmt <- optional (do
        reserved "else" <?> "else keyword"
        stmt <?> "else statement")
    return $ If cond thenStmt elseStmt pos

whileStmt :: Parser Stmt
whileStmt = do
    pos <- getSourcePos
    reserved "while"
    cond <- parens expr
    body <- stmt
    return $ While cond body pos

forStmt :: Parser Stmt
forStmt = do
    pos <- getSourcePos
    reserved "for"
    void $ symbol "("
    forInit <- optional (choice [try decl, simp]) <?> "for loop initializer"
    semi
    cond <- optional expr <?> "for loop condition"
    semi
    step <- optional (choice [try decl, simp]) <?> "for loop step"
    void $ symbol ")"
    body <- stmt <?> "for loop body"
    return $ For forInit cond step body pos

breakStmt :: Parser Stmt
breakStmt = do
    pos <- getSourcePos
    reserved "break"
    semi
    return $ Break pos

continueStmt :: Parser Stmt
continueStmt = do
    pos <- getSourcePos
    reserved "continue"
    semi
    return $ Continue pos

decl :: Parser Stmt
decl = try declInit <|> declNoInit

declNoInit :: Parser Stmt
declNoInit = do
    pos <- getSourcePos
    typ <- parseType
    name <- identifier
    return $ Decl typ name pos

declInit :: Parser Stmt
declInit = do
    pos <- getSourcePos
    typ <- parseType
    name <- identifier
    void $ symbol "="
    e <- expr
    return $ Init typ name e pos

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
            "&=" -> pure (Just BitAnd)
            "^=" -> pure (Just BitXor)
            "|=" -> pure (Just BitOr)
            "<<=" -> pure (Just Shl)
            ">>=" -> pure (Just Shr)
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
expr' = choice
    [ parens expr
    , boolExpr
    , intExpr
    , identExpr
    ]

intExpr :: Parser Expr
intExpr = do
    pos <- getSourcePos
    str <- numberLiteral
    return $ IntExpr str pos

boolExpr :: Parser Expr
boolExpr = do
    pos <- getSourcePos
    b <- choice
        [ True <$ reserved "true"
        , False <$ reserved "false"
        ]
    return $ BoolExpr b pos

identExpr :: Parser Expr
identExpr = do
    pos <- getSourcePos
    name <- identifier
    return $ Ident name pos

opTable :: [[Operator Parser Expr]]
opTable =
    [ [Prefix manyUnaryOp]
    , [ InfixL (BinExpr Mul <$ symbol "*")
      , InfixL (BinExpr Div <$ symbol "/")
      , InfixL (BinExpr Mod <$ symbol "%")
      ]
    , [ InfixL (BinExpr Add <$ symbol "+")
      , InfixL (BinExpr Sub <$ symbol "-")
      ]
    , [ InfixL (BinExpr Shl <$ try (symbol "<<"))
      , InfixL (BinExpr Shr <$ try (symbol ">>"))
      ]
    , [ InfixL (BinExpr Le <$ try (symbol "<="))
      , InfixL (BinExpr Ge <$ try (symbol ">="))
      , InfixL (BinExpr Lt <$ symbol "<")
      , InfixL (BinExpr Gt <$ symbol ">")
      ]
    , [ InfixL (BinExpr Eq <$ try (symbol "=="))
      , InfixL (BinExpr Ne <$ try (symbol "!="))
      ]
    , [InfixL (BinExpr BitAnd <$ try (symbol "&" <* notFollowedBy (char '&')))]
    , [InfixL (BinExpr BitXor <$ symbol "^")]
    , [InfixL (BinExpr BitOr <$ try (symbol "|" <* notFollowedBy (char '|')))]
    , [InfixL (BinExpr And <$ try (symbol "&&"))]
    , [InfixL (BinExpr Or <$ try (symbol "||"))]
    ]
  where
    manyUnaryOp = foldr1 (.) <$> some unaryOpChoice
    unaryOpChoice = choice 
        [ UnExpr Neg <$ symbol "-"
        , UnExpr Not <$ symbol "!"
        , UnExpr BitNot <$ symbol "~"
        ]

expr :: Parser Expr
expr = ternaryExpr <?> "expression"

ternaryExpr :: Parser Expr
ternaryExpr = do
    e1 <- makeExprParser expr' opTable
    pos <- getSourcePos
    maybeTernary <- optional $ do
        void $ symbol "?"
        e2 <- ternaryExpr
        void $ symbol ":"
        e3 <- ternaryExpr
        return $ TernaryExpr e1 e2 e3 pos
    case maybeTernary of
        Nothing -> return e1
        Just ternary -> return ternary

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

decLiteral :: Parser String
decLiteral = string "0" <|> (:) <$> oneOf ['1' .. '9'] <*> many digitChar

hexLiteral :: Parser String
hexLiteral = do
    void (chunk "0x" <|> chunk "0X")
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

opStart :: Parser Char
opStart = oneOf "=+-*/%&^|<>!~"

opLetter :: Parser Char
opLetter = oneOf "=&|<>"

operator :: Parser String
operator = lexeme ((:) <$> opStart <*> many opLetter)

asciiLetterChar :: Parser Char
asciiLetterChar = satisfy (\c -> isAscii c && isAlpha c) <?> "ASCII letter"

asciiAlphaNumChar :: Parser Char
asciiAlphaNumChar = satisfy (\c -> isAscii c && isAlphaNum c) <?> "ASCII letter or digit"

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