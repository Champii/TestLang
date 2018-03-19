module Eval (
  runEval,

  TermEnv,
  emptyTmenv
) where

import Syntax

import Control.Monad.Identity
import qualified Data.Map as Map

data Value
  = VInt Int
  | VBool Bool
  | VClosure String Expr TermEnv

type TermEnv = Map.Map String Value
type Interpreter t = Identity t

emptyTmenv :: TermEnv
emptyTmenv = Map.empty

instance Show Value where
  show (VInt n) = show n
  show (VBool n) = show n
  -- show VClosure{} = "<<closure>>"
  show (VClosure s ex te) = "<<closure>>: " ++ s ++ show ex ++ show te

eval :: TermEnv -> Expr -> Interpreter Value
eval env expr = case expr of
  Lit (LInt k)  -> return $ VInt k
  Lit (LBool k) -> return $ VBool k

  Var x -> do
    let Just v = Map.lookup x env
    return v

  Op op a b -> do
    VInt a' <- eval env a
    VInt b' <- eval env b
    return $ (binop op) a' b'

  Lam x body ->
    return (VClosure x body env)

  App fun arg -> do
    VClosure x body clo <- eval env fun
    argv <- eval env arg
    let nenv = Map.insert x argv clo
    eval nenv body

  Let x e -> do
    e' <- eval env e
    let nenv = Map.insert x e' env
    -- eval nenv body
    return e'

  If cond tr fl -> do
    VBool br <- eval env cond
    if br == True
      then eval env tr
      else eval env fl

  -- Fix e -> do
  --   eval env (App e (Fix e))

binop :: Binop -> Int -> Int -> Value
binop Add a b = VInt $ a + b
binop Mul a b = VInt $ a * b
binop Sub a b = VInt $ a - b
binop Eql a b = VBool $ a == b
binop NEql a b = VBool $ a /= b

runEval :: TermEnv -> String -> Expr -> (Value, TermEnv)
runEval env nm ex =
  let res = runIdentity (eval env ex) in
  (res, Map.insert nm res env)
