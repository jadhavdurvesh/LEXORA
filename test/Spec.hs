module Main where
import Control.Monad (unless)
import Lexer
import Parser
import Typecheck
import Eval
import Syntax

parse source = lexSource source >>= parseExpr
assert label condition = unless condition (error ("FAILED: " ++ label))
main :: IO ()
main = do
 let good = ["1 + 2 * 3","let id = fn x => x in id 42","if true then 7 else 9","(fn x => x + 1) 4"]
 mapM_ (\source -> case parse source of
  Left err -> error err
  Right expr -> case infer expr of
   Left err -> error err
   Right _ -> case eval [] expr of
    Left err -> error err
    Right _ -> pure ()) good
 case parse "if 1 then true else false" of
  Right expr -> assert "reject non-boolean condition" (case infer expr of Left _ -> True; _ -> False)
  Left err -> error err
 case parse "1 / 0" of
  Right expr -> assert "division by zero is a runtime error" (case eval [] expr of Left _ -> True; _ -> False)
  Left err -> error err
 putStrLn "LEXORA tests passed"
