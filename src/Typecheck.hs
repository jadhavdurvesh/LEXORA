module Typecheck (infer) where

import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Syntax

type Subst = M.Map Int Type
type Scheme = ([Int], Type)
type TypeEnv = M.Map String Scheme
type Infer a = StateT Int (Either String) a

apply :: Subst -> Type -> Type
apply s ty = case ty of
  TVar n -> maybe ty (apply s) (M.lookup n s)
  TFun a b -> TFun (apply s a) (apply s b)
  _ -> ty

applyScheme :: Subst -> Scheme -> Scheme
applyScheme s (vars, ty) = (vars, apply (foldr M.delete s vars) ty)

applyEnv :: Subst -> TypeEnv -> TypeEnv
applyEnv s = M.map (applyScheme s)

free :: Type -> S.Set Int
free ty = case ty of
  TVar n -> S.singleton n
  TFun a b -> S.union (free a) (free b)
  _ -> S.empty

freeEnv :: TypeEnv -> S.Set Int
freeEnv = S.unions . map (free . snd) . M.elems

fresh :: Infer Type
fresh = do
  n <- get
  put (n + 1)
  pure (TVar n)

compose :: Subst -> Subst -> Subst
compose newer older = M.union (M.map (apply newer) older) newer

unify :: Type -> Type -> Infer Subst
unify left right = case (left, right) of
  (TInt, TInt) -> pure M.empty
  (TBool, TBool) -> pure M.empty
  (TVar n, ty) -> bind n ty
  (ty, TVar n) -> bind n ty
  (TFun a1 a2, TFun b1 b2) -> do
    s1 <- unify a1 b1
    s2 <- unify (apply s1 a2) (apply s1 b2)
    pure (compose s2 s1)
  _ -> lift (Left ("type mismatch: " ++ show left ++ " versus " ++ show right))
  where
    bind n ty
      | ty == TVar n = pure M.empty
      | S.member n (free ty) = lift (Left ("infinite type: t" ++ show n ++ " occurs in " ++ show ty))
      | otherwise = pure (M.singleton n ty)

generalize :: TypeEnv -> Type -> Scheme
generalize env ty = (S.toList (S.difference (free ty) (freeEnv env)), ty)

instantiate :: Scheme -> Infer Type
instantiate (vars, ty) = do
  replacements <- mapM (const fresh) vars
  pure (apply (M.fromList (zip vars replacements)) ty)

infer :: Expr -> Either String Type
infer expression = evalStateT action 0
  where
    action = do
      (sub, ty) <- inferExpr M.empty expression
      pure (apply sub ty)

inferExpr :: TypeEnv -> Expr -> Infer (Subst, Type)
inferExpr env expression = case expression of
  EInt _ _ -> pure (M.empty, TInt)
  EBool _ _ -> pure (M.empty, TBool)
  EVar name span' -> case M.lookup name env of
    Nothing -> lift (Left (show (spanLine span') ++ ":" ++ show (spanColumn span') ++ ": unbound variable " ++ name))
    Just scheme -> do
      ty <- instantiate scheme
      pure (M.empty, ty)
  EBin op left right _ -> do
    (s1, leftTy) <- inferExpr env left
    (s2, rightTy) <- inferExpr (applyEnv s1 env) right
    let current = compose s2 s1
    case op of
      Add -> numeric current leftTy rightTy
      Sub -> numeric current leftTy rightTy
      Mul -> numeric current leftTy rightTy
      Div -> numeric current leftTy rightTy
      Less -> do
        sn <- numeric current leftTy rightTy
        pure (sn, TBool)
      Greater -> do
        sn <- numeric current leftTy rightTy
        pure (sn, TBool)
      Equal -> do
        se <- unify (apply current leftTy) (apply current rightTy)
        pure (compose se current, TBool)
  EIf condition yes no _ -> do
    (s1, conditionTy) <- inferExpr env condition
    sc <- unify (apply s1 conditionTy) TBool
    let s2 = compose sc s1
    (s3, yesTy) <- inferExpr (applyEnv s2 env) yes
    let s4 = compose s3 s2
    (s5, noTy) <- inferExpr (applyEnv s4 env) no
    sb <- unify (apply s5 yesTy) (apply s5 noTy)
    let resultSub = compose sb (compose s5 s4)
    pure (resultSub, apply resultSub yesTy)
  ELet name value body _ -> do
    (s1, valueTy) <- inferExpr env value
    let env1 = applyEnv s1 env
        scheme = generalize env1 (apply s1 valueTy)
    (s2, bodyTy) <- inferExpr (M.insert name scheme env1) body
    pure (compose s2 s1, apply s2 bodyTy)
  ELam name body _ -> do
    argumentTy <- fresh
    let env1 = M.insert name ([], argumentTy) env
    (s1, bodyTy) <- inferExpr env1 body
    pure (s1, TFun (apply s1 argumentTy) (apply s1 bodyTy))
  EApp fn arg _ -> do
    (s1, fnTy) <- inferExpr env fn
    (s2, argTy) <- inferExpr (applyEnv s1 env) arg
    resultTy <- fresh
    s3 <- unify (apply (compose s2 s1) fnTy) (TFun (apply s2 argTy) resultTy)
    let finalSub = compose s3 (compose s2 s1)
    pure (finalSub, apply s3 resultTy)
  where
    numeric current a b = do
      s1 <- unify (apply current a) TInt
      let next = compose s1 current
      s2 <- unify (apply next b) TInt
      pure (compose s2 next, TInt)
