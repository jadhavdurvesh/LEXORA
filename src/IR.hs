module IR (Instr(..),compileIR,renderIR) where
import Syntax
data Instr = PushInt Integer | PushBool Bool | Load String | Store String | AddI | SubI | MulI | DivI | EqI | LtI | GtI | JumpIfFalse String | Jump String | Label String | MakeClosure String | Call | Return deriving (Eq,Show)
compileIR :: Expr -> [Instr]
compileIR expr = fst (go 0 expr)
 where
 go n e = case e of
  EInt x _ -> ([PushInt x],n)
  EBool x _ -> ([PushBool x],n)
  EVar x _ -> ([Load x],n)
  EBin op a b _ -> let (ia,n1)=go n a; (ib,n2)=go n1 b in (ia++ib++[opInstr op],n2)
  ELet name value body _ -> let (iv,n1)=go n value; (ib,n2)=go n1 body in (iv++[Store name]++ib,n2)
  ELam name body _ -> let (ib,n1)=go n body in ([MakeClosure name]++ib++[Return],n1)
  EApp f x _ -> let (ifn,n1)=go n f; (ix,n2)=go n1 x in (ifn++ix++[Call],n2)
  EIf c a b _ ->
   let lElse="L_else_"++show n; lEnd="L_end_"++show n
       (ic,n1)=go (n+1) c; (ia,n2)=go n1 a; (ib,n3)=go n2 b
   in (ic++[JumpIfFalse lElse]++ia++[Jump lEnd,Label lElse]++ib++[Label lEnd],n3)
 opInstr Add=AddI
 opInstr Sub=SubI
 opInstr Mul=MulI
 opInstr Div=DivI
 opInstr Equal=EqI
 opInstr Less=LtI
 opInstr Greater=GtI
renderIR :: [Instr] -> String
renderIR = unlines . map show
