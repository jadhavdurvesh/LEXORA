module Syntax where

data Span = Span { spanLine :: !Int, spanColumn :: !Int } deriving (Eq, Ord, Show)
data TokenKind = TkInt Integer | TkIdent String | TkLet | TkIn | TkIf | TkThen | TkElse | TkFn | TkTrue | TkFalse | TkPlus | TkMinus | TkStar | TkSlash | TkEq | TkEqEq | TkLess | TkGreater | TkFatArrow | TkLParen | TkRParen | TkEOF deriving (Eq, Show)
data Token = Token { tokenKind :: !TokenKind, tokenSpan :: !Span } deriving (Eq, Show)
data Type = TInt | TBool | TFun Type Type | TVar Int deriving (Eq, Ord)
instance Show Type where
 show TInt = "Int"
 show TBool = "Bool"
 show (TVar n) = "t" ++ show n
 show (TFun a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
data BinOp = Add | Sub | Mul | Div | Equal | Less | Greater deriving (Eq, Show)
data Expr = EInt Integer Span | EBool Bool Span | EVar String Span | EBin BinOp Expr Expr Span | EIf Expr Expr Expr Span | ELet String Expr Expr Span | ELam String Expr Span | EApp Expr Expr Span deriving (Eq, Show)
exprSpan :: Expr -> Span
exprSpan e = case e of
 EInt _ s -> s
 EBool _ s -> s
 EVar _ s -> s
 EBin _ _ _ s -> s
 EIf _ _ _ s -> s
 ELet _ _ _ s -> s
 ELam _ _ s -> s
 EApp _ _ s -> s
data Value = VInt Integer | VBool Bool | VClosure String Expr Env
type Env = [(String, Value)]
instance Show Value where
 show (VInt n) = show n
 show (VBool b) = if b then "true" else "false"
 show (VClosure n _ _) = "<function " ++ n ++ ">"
