module Eval (eval) where
import Syntax
eval env e = case e of
 EInt n _ -> Right (VInt n)
 EBool b _ -> Right (VBool b)
 EVar n _ -> maybe (Left ("unbound variable " ++ n)) Right (lookup n env)
 ELam n body _ -> Right (VClosure n body env)
 ELet n value body _ -> do v <- eval env value; eval ((n,v):env) body
 EIf c a b _ -> do
  v <- eval env c
  case v of
   VBool True -> eval env a
   VBool False -> eval env b
   _ -> Left "if condition must be Bool"
 EBin op a b _ -> do x <- eval env a; y <- eval env b; binary op x y
 EApp f x _ -> do
  fn <- eval env f
  arg <- eval env x
  case fn of
   VClosure n body closure -> eval ((n,arg):closure) body
   _ -> Left "attempted to call a non-function"
 where
 binary op a b = case (op,a,b) of
  (Add,VInt x,VInt y) -> Right (VInt (x+y))
  (Sub,VInt x,VInt y) -> Right (VInt (x-y))
  (Mul,VInt x,VInt y) -> Right (VInt (x*y))
  (Div,VInt _,VInt 0) -> Left "division by zero"
  (Div,VInt x,VInt y) -> Right (VInt (x `div` y))
  (Equal,VInt x,VInt y) -> Right (VBool (x==y))
  (Equal,VBool x,VBool y) -> Right (VBool (x==y))
  (Less,VInt x,VInt y) -> Right (VBool (x<y))
  (Greater,VInt x,VInt y) -> Right (VBool (x>y))
  _ -> Left "invalid operands for operator"
