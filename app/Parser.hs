{-# LANGUAGE OverloadedStrings #-}

module Parser (
  parseExpr,
  parseModule,
  Binding(..)
) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Expr as Ex
import qualified Text.Megaparsec.Char.Lexer as Tok

import qualified Data.Text.Lazy as L

import Lexer
import Syntax

import Data.Functor.Identity

variable :: Parser Expr
variable = do
  x <- identifier
  return (Var x)

number :: Parser Expr
number = do
  n <- integer
  return (Lit (LInt (fromIntegral n)))

bool :: Parser Expr
bool = (reserved "True" >> return (Lit (LBool True)))
    <|> (reserved "False" >> return (Lit (LBool False)))

lambda :: Parser Expr
lambda = do
  args <-  parens $ sepBy identifier ","
  symbol "->"
  body <- expr
  return $ foldr Lam body args

ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- expr
  reserved "then" <|> reserved "=>"
  tr <- expr
  reserved "else"
  fl <- expr
  return (If cond tr fl)

aexp :: Parser Expr
aexp =
   try lambda
  <|> parens expr
  <|> bool
  <|> number
  <|> ifthen
  <|> variable

term :: Parser Expr
term = aexp >>= \x ->
                ( some aexp >>= \xs -> return (foldl App x xs))
                <|> return x

table :: Operators Expr
table = [
    [
      Ex.InfixL ((Op Mul) <$ symbol"*")
    ],
    [
      Ex.InfixL ((Op Add) <$ symbol "+")
    , Ex.InfixL ((Op Sub) <$ symbol "-")
    ],
    [
      Ex.InfixL ((Op NEql) <$ symbol "isnt")
    , Ex.InfixL ((Op NEql) <$ symbol "!=")
    , Ex.InfixL ((Op Eql) <$ symbol "is")
    , Ex.InfixL ((Op Eql) <$ symbol "==")
    ]
  ]

expr :: Parser Expr
expr = Ex.makeExprParser term table

type Binding = (String, Expr)

fdecl :: Parser Binding
fdecl = do
  name <- identifier
  symbol "="
  -- args <-  parens $ sepBy identifier ","
  -- symbol "->"
  body <- expr
  return $ (name, body)

val :: Parser Binding
val = do
  ex <- expr
  return ("it", ex)

decl :: Parser Binding
decl = try fdecl <|> val

top :: Parser Binding
top = do
  x <- decl
  -- optional semi
  some eol
  return x

modl ::  Parser [Binding]
modl =  do
  -- many eol
  res <- some top
  return res

parseExpr :: L.Text -> Either (ParseError Char String)  Binding
parseExpr input = parse (contents decl) "<stdin>" $ (L.unpack input)

parseModule :: L.Text -> Either (ParseError Char String) [Binding]
parseModule input = parse modl "lol" $ (L.unpack input) ++ "\n"
