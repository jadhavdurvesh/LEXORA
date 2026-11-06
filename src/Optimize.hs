module Optimize (optimize) where

import Syntax

optimize :: Expr -> Expr
optimize expression = case expression of
  EInt _ _ -> expression
  EBool _ _ -> expression
  EVar _ _ -> expression
  ELam name body span' -> ELam name (optimize body) span'
  EApp fn arg span' -> EApp (optimize fn) (optimize arg) span'
  ELet name value body span' ->
    ELet name (optimize value) (optimize body) span'
  EIf condition yes no span' ->
    let c = optimize condition
        a = optimize yes
        b = optimize no
    in case c of
      EBool True _ -> a
      EBool False _ -> b
      _ -> EIf c a b span'
  EBin op left right span' ->
    let a = optimize left
        b = optimize right
    in foldBinary op a b span'

foldBinary :: BinOp -> Expr -> Expr -> Span -> Expr
foldBinary op (EInt a _) (EInt b _) span' = case op of
  Add -> EInt (a+b) span'
  Sub -> EInt (a-b) span'
  Mul -> EInt (a*b) span'
  Div | b /= 0 -> EInt (a `div` b) span'
  Equal -> EBool (a==b) span'
  Less -> EBool (a<b) span'
  Greater -> EBool (a>b) span'
  _ -> EBin op (EInt a span') (EInt b span') span'
foldBinary Equal (EBool a _) (EBool b _) span' = EBool (a==b) span'
foldBinary op a b span' = EBin op a b span'
