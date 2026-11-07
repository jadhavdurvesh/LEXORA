module Main where
import Control.Monad (unless)
import Lexer
import Parser
import Typecheck
import Eval
import Syntax
import Optimize
import IR
import IRVerify
import VM

parse source = lexSource source >>= parseExpr
assert label condition = unless condition (error ("FAILED: " ++ label))
run source = case parse source of
 Left err -> Left err
 Right expr -> case infer expr of
  Left err -> Left err
  Right _ -> eval [] expr

main :: IO ()
main = do
 let good = ["1 + 2 * 3","let id = fn x => x in id 42","if true then 7 else 9","(fn x => x + 1) 4","let id = fn x => x in if id true then id 1 else id 2"]
 mapM_ (\source -> case run source of
  Left err -> error (source ++ ": " ++ err)
  Right _ -> pure ()) good

 case parse "fn x => x + 1" of
  Right expr -> assert "lambda parameter is inferred as Int" (infer expr == Right (TFun TInt TInt))
  Left err -> error err

 case parse "let id = fn x => x in if id true then id 1 else id 2" of
  Right expr -> assert "let polymorphism supports Bool and Int" (infer expr == Right TInt)
  Left err -> error err

 case parse "if 1 then true else false" of
  Right expr -> assert "reject non-boolean condition" (case infer expr of Left _ -> True; _ -> False)
  Left err -> error err

 case parse "if ((fn x => x) true) then 11 else 22" of
  Right expr -> assert "condition inference propagates substitutions" (infer expr == Right TInt)
  Left err -> error err

 case parse "let apply = fn f => fn x => f x in apply (fn n => n + 1) 41" of
  Right expr -> assert "nested higher-order application infers" (infer expr == Right TInt)
  Left err -> error err

 case parse "fn x => x x" of
  Right expr -> assert "occurs check rejects infinite types" (case infer expr of Left _ -> True; _ -> False)
  Left err -> error err

 case run "1 / 0" of
  Left err -> assert "division by zero is a runtime error" (err == "division by zero")
  Right _ -> error "division by zero unexpectedly succeeded"

 case parse "if true then 2 + 3 * 4 else 99" of
  Right expr -> assert "constant folding and branch elimination" (prettyExpr (optimize expr) == "14")
  Left err -> error err

 case parse "1 / 0" of
  Right expr -> assert "optimizer preserves division by zero" (case optimize expr of EBin Div _ _ _ -> True; _ -> False)
  Left err -> error err

 case verifyLabels [Label "entry", JumpIfFalse "else", Jump "end", Label "else", Label "end"] of
  Right () -> pure ()
  Left err -> error ("valid IR labels rejected: " ++ err)
 case verifyLabels [Jump "missing"] of
  Left _ -> pure ()
  Right () -> error "undefined IR branch target unexpectedly accepted"
 case verifyLabels [Label "dup", Label "dup"] of
  Left _ -> pure ()
  Right () -> error "duplicate IR label unexpectedly accepted"
 case parse "if true then 1 else 2" of
  Right expr -> assert "compiled conditional IR has valid labels" (verifyLabels (compileIR expr) == Right ())
  Left err -> error err
 case parse "if true then 1 else 2" of
  Right expr -> assert "compiled conditional IR has consistent stack heights" (verifyStack (compileIR expr) == Right ())
  Left err -> error err
 assert "IR verifier rejects stack underflow" (case verifyStack [AddI] of Left _ -> True; _ -> False)
 assert "IR verifier rejects inconsistent branch merge" (case verifyStack [PushBool True, JumpIfFalse "else", PushInt 1, Jump "end", Label "else", PushInt 2, PushInt 3, Label "end"] of Left _ -> True; _ -> False)


 assert "VM executes integer arithmetic" (runProgram [PushInt 6, PushInt 7, MulI] == Right (RInt 42))
 assert "VM executes conditional branch" (runProgram [PushBool False, JumpIfFalse "else", PushInt 1, Jump "end", Label "else", PushInt 2, Label "end"] == Right (RInt 2))
 assert "VM executes variable store and load" (runProgram [PushInt 9, Store "answer", Load "answer"] == Right (RInt 9))
 assert "VM detects division by zero" (runProgram [PushInt 10, PushInt 0, DivI] == Left DivisionByZero)
 assert "VM enforces execution fuel" (runProgramWithFuel 8 [Label "loop", Jump "loop"] == Left FuelExhausted)
 assert "VM rejects undefined labels" (runProgram [Jump "missing"] == Left (MissingLabel "missing"))

 putStrLn "LEXORA tests passed"
