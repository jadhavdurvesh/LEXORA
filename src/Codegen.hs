module Codegen (emitLLVM) where
import IR
-- This is an inspectable backend-neutral stack IR listing, not LLVM bitcode.
emitLLVM instructions = unlines ("; LEXORA stack IR; native LLVM backend is a future milestone" : map render instructions)
 where
 render instruction = case instruction of
  PushInt n -> "  push i64 " ++ show n
  PushBool b -> "  push i1 " ++ if b then "true" else "false"
  Load name -> "  load " ++ name
  Store name -> "  store " ++ name
  AddI -> "  add i64"
  SubI -> "  sub i64"
  MulI -> "  mul i64"
  DivI -> "  sdiv i64"
  EqI -> "  icmp eq"
  LtI -> "  icmp slt"
  GtI -> "  icmp sgt"
  JumpIfFalse label -> "  br_if_false " ++ label
  Jump label -> "  br " ++ label
  Label label -> label ++ ":"
  MakeClosure name -> "  closure " ++ name
  Call -> "  call"
  Return -> "  ret"
