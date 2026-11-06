module Parser (parseExpr) where
import Syntax
type P a = [Token] -> Either String (a,[Token])
parseExpr ts = do
 (e,rest) <- low ts
 case rest of
  [Token TkEOF _] -> Right e
  t:_ -> Left (at t ++ "unexpected token " ++ show (tokenKind t))
  [] -> Right e
low ts = case ts of
 Token TkLet s:xs -> do
  (n,r1) <- ident xs
  r2 <- expect TkEq r1
  (v,r3) <- low r2
  r4 <- expect TkIn r3
  (b,r5) <- low r4
  Right (ELet n v b s,r5)
 Token TkIf s:xs -> do
  (c,r1) <- low xs
  r2 <- expect TkThen r1
  (a,r3) <- low r2
  r4 <- expect TkElse r3
  (b,r5) <- low r4
  Right (EIf c a b s,r5)
 Token TkFn s:xs -> do
  (n,r1) <- ident xs
  r2 <- expect TkFatArrow r1
  (b,r3) <- low r2
  Right (ELam n b s,r3)
 _ -> comparison ts
comparison ts = do
 (a,r) <- chain add [(TkEqEq,Equal),(TkLess,Less),(TkGreater,Greater)] ts
 Right (a,r)
add = chain mul [(TkPlus,Add),(TkMinus,Sub)]
mul = chain app [(TkStar,Mul),(TkSlash,Div)]
app ts = do
 (f,r) <- atom ts
 more f r
 where
 more f (t:rest) | starts (tokenKind t) = do
  (x,r) <- atom (t:rest)
  more (EApp f x (exprSpan f)) r
 more f r = Right (f,r)
 starts k = case k of
  TkInt _ -> True
  TkIdent _ -> True
  TkTrue -> True
  TkFalse -> True
  TkLParen -> True
  _ -> False
atom ts = case ts of
 Token (TkInt n) s:r -> Right (EInt n s,r)
 Token TkTrue s:r -> Right (EBool True s,r)
 Token TkFalse s:r -> Right (EBool False s,r)
 Token (TkIdent n) s:r -> Right (EVar n s,r)
 Token TkLParen _:r -> do
  (e,r1) <- low r
  r2 <- expect TkRParen r1
  Right (e,r2)
 t:_ -> Left (at t ++ "expected expression")
 [] -> Left "unexpected end of input"
chain sub ops ts = do
 (first,r) <- sub ts
 loop first r
 where
 loop left (Token k s:rest) = case lookup k ops of
  Nothing -> Right (left,Token k s:rest)
  Just op -> do
   (right,r) <- sub rest
   loop (EBin op left right s) r
 loop left r = Right (left,r)
ident (Token (TkIdent n) _:r) = Right (n,r)
ident (t:_) = Left (at t ++ "expected identifier")
ident [] = Left "expected identifier"
expect k (Token actual s:r) | k == actual = Right r
expect k (Token actual s:_) = Left (show (spanLine s) ++ ":" ++ show (spanColumn s) ++ ": expected " ++ show k ++ ", found " ++ show actual)
expect k [] = Left ("expected " ++ show k ++ " at end of input")
at t = show (spanLine (tokenSpan t)) ++ ":" ++ show (spanColumn (tokenSpan t)) ++ ": "
