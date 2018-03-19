module Lexer where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text.Lazy as L
import qualified Text.Megaparsec.Char.Lexer as Tok
import qualified Text.Megaparsec.Expr as Ex

import Data.Functor.Identity

import Syntax

type Op a = Ex.Operator Parser Expr
type Operators a = [[Op a]]

type Parser = Parsec String String


reservedNames :: [String]
reservedNames = [
    "let",
    "in",
    "fix",
    "rec",
    "if",
    "then",
    "else",
    "isnt",
    "is"
  ]

reservedOps :: [String]
reservedOps = [
    "->",
    "\\",
    "+",
    "*",
    "-",
    "=",
    "isnt",
    "is"
  ]

customSpace :: Parser ()
customSpace = string " " *> return ()

sc :: Parser ()
sc = Tok.space customSpace lineCmnt blockCmnt
  where
    lineCmnt  = Tok.skipLineComment "#"
    blockCmnt = Tok.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = Tok.lexeme sc

-- lexer :: Tok.GenTokenParser L.Text () Identity
-- lexer = Tok.makeTokenParser $ Tok.LanguageDef
--   { Tok.commentStart    = "{-"
--   , Tok.commentEnd      = "-}"
--   , Tok.commentLine     = "#"
--   , Tok.nestedComments  = True
--   , Tok.identStart      = letter
--   , Tok.identLetter     = alphaNum <|> oneOf "_'"
--   , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
--   , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
--   , Tok.reservedNames   = reservedNames
--   , Tok.reservedOpNames = reservedOps
--   , Tok.caseSensitive   = True
--   }

reserved :: String -> Parser ()
reserved w = lexeme (string w *> notFollowedBy alphaNumChar)

-- rws :: [String] -- list of reserved words
-- rws = ["if","then","else","while","do","skip","true","false","not","and","or"]

reservedOp :: String -> Parser ()
reservedOp x = if x `elem` reservedOps
  then reserved x
  else fail $ "WOOT"

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = sc >> (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` reservedNames
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

symbol :: String -> Parser String
symbol = Tok.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- semiSep :: Parser a -> Parser [a]
-- semiSep = sepBy semi

semi :: Parser String
semi = symbol ";"

integer :: Parser Integer
integer = lexeme Tok.decimal

contents :: Parser a -> Parser a
contents p = do
  sc
  r <- p
  eof
  return r
