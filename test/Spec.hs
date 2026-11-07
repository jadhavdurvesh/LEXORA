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

 putStrLn "LEXORA tests passed"
