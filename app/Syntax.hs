module Syntax where

type Var = String

data Expr
  = Var Var
  | App Expr Expr
  | Lam Var Expr
  | Let Var Expr
  | Lit Lit
  | If Expr Expr Expr
  -- | Fix Expr
  | Op Binop Expr Expr
  deriving (Show, Eq, Ord)

data Lit
  = LInt Int
  | LBool Bool
  deriving (Show, Eq, Ord)

data Binop = Add | Sub | Mul | Eql | NEql
  deriving (Eq, Ord, Show)

data Program = Program [Decl] deriving Eq

type Decl = (String, Expr)
