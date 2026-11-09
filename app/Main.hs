module Main where
import System.Environment (getArgs)
import System.IO (hFlush, isEOF, stdout)
import Syntax (prettyExpr)
import Lexer
import Parser
import Typecheck
import Eval
import IR
import Codegen
import Optimize
import IRVerify
import VM

process :: String -> IO ()
process source = case lexSource source >>= parseExpr of
 Left err -> putStrLn ("syntax error: " ++ err)
 Right expr -> case infer expr of
  Left err -> putStrLn ("type error: " ++ err)
  Right ty -> do
   putStrLn ("type: " ++ show ty)
   case eval [] expr of
    Left err -> putStrLn ("runtime error: " ++ err)
    Right value -> putStrLn ("value: " ++ show value)
   let optimized = optimize expr
   putStrLn "optimized expression:"
   putStrLn (prettyExpr optimized)
   putStrLn "intermediate representation:"
   putStr (renderIR (compileIR optimized))
   let bytecode = compileIR optimized
   putStrLn "backend listing (not native LLVM):"
   putStr (emitLLVM bytecode)
   case verifyStack bytecode of
    Left err -> putStrLn ("IR verification error: " ++ err)
    Right () -> case runProgram bytecode of
     Left err -> putStrLn ("VM runtime error: " ++ show err)
     Right value -> putStrLn ("VM result: " ++ show value)

main :: IO ()
main = do
 args <- getArgs
 case args of
  [] -> repl
  [path] -> readFile path >>= process
  _ -> putStrLn "Usage: lexora [source-file]"

repl :: IO ()
repl = do
 putStrLn "LEXORA 0.1 — typed functional language (Ctrl-D to exit, :quit to exit)"
 loop
 where
 loop = do
  putStr "lexora> "
  hFlush stdout
  eof <- isEOF
  if eof then putStrLn "" else do
   line <- getLine
   if line == ":quit" then pure () else process line >> loop
