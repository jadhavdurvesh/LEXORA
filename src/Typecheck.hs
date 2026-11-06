module Typecheck (infer) where
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Syntax
type Subst = M.Map Int Type
type Scheme = ([Int],Type)
type TypeEnv = M.Map String Scheme
type Infer a = StateT Int (Either String) a
apply s t = case t of
 TVar n -> maybe t (apply s) (M.lookup n s)
 TFun a b -> TFun (apply s a) (apply s b)
 _ -> t
applyScheme s (vs,t) = (vs,apply (foldr M.delete s vs) t)
free t = case t of
 TVar n -> S.singleton n
 TFun a b -> S.union (free a) (free b)
 _ -> S.empty
freeEnv = S.unions . map (free . snd) . M.elems
fresh = do n <- get; put (n+1); pure (TVar n)
unify a b = case (a,b) of
 (TInt,TInt) -> pure M.empty
 (TBool,TBool) -> pure M.empty
 (TVar n,t) -> bind n t
 (t,TVar n) -> bind n t
 (TFun a1 a2,TFun b1 b2) -> do
  s1 <- unify a1 b1
  s2 <- unify (apply s1 a2) (apply s1 b2)
  pure (M.union s2 s1)
 _ -> lift (Left ("type mismatch: " ++ show a ++ " versus " ++ show b))
 where
 bind n t | t == TVar n = pure M.empty
          | S.member n (free t) = lift (Left ("infinite type: t" ++ show n ++ " occurs in " ++ show t))
          | otherwise = pure (M.singleton n t)
generalize env t = (S.toList (S.difference (free t) (freeEnv env)),t)
instantiate (vs,t) = do
 freshTypes <- mapM (const fresh) vs
 pure (apply (M.fromList (zip vs freshTypes)) t)
infer e = evalStateT (go M.empty e) 0
go env e = case e of
 EInt _ _ -> pure TInt
 EBool _ _ -> pure TBool
 EVar n s -> maybe (lift (Left (show (spanLine s) ++ ":" ++ show (spanColumn s) ++ ": unbound variable " ++ n))) instantiate (M.lookup n env)
 EBin op a b _ -> do
  ta <- go env a
  tb <- go env b
  case op of
   Add -> numeric ta tb
   Sub -> numeric ta tb
   Mul -> numeric ta tb
   Div -> numeric ta tb
   Equal -> unify ta tb >> pure TBool
   Less -> numeric ta tb >> pure TBool
   Greater -> numeric ta tb >> pure TBool
 EIf c a b _ -> do
  tc <- go env c
  _ <- unify tc TBool
  ta <- go env a
  tb <- go env b
  _ <- unify ta tb
  pure ta
 ELet n v body _ -> do
  tv <- go env v
  go (M.insert n (generalize env tv) env) body
 ELam n body _ -> do
  arg <- fresh
  result <- go (M.insert n ([],arg) env) body
  pure (TFun arg result)
 EApp f x _ -> do
  tf <- go env f
  tx <- go env x
  result <- fresh
  _ <- unify tf (TFun tx result)
  pure result
 where
 numeric a b = unify a TInt >> unify b TInt >> pure TInt
