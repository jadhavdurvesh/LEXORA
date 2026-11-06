module Lexer (lexSource) where
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Syntax

lexSource :: String -> Either String [Token]
lexSource = go 1 1
 where
  go l c [] = Right [Token TkEOF (Span l c)]
  go l c (x:xs)
   | isSpace x = if x == '\n' then go (l+1) 1 xs else go l (c+1) xs
   | isDigit x = let (ds,rest)=span isDigit (x:xs) in (Token (TkInt (read ds)) (Span l c):) <$> go l (c+length ds) rest
   | isAlpha x || x == '_' = let (w,rest)=span (\z -> isAlphaNum z || z == '_') (x:xs) in (Token (kw w) (Span l c):) <$> go l (c+length w) rest
   | otherwise = case x:xs of
      '=':'>':r -> emit l c TkFatArrow 2 r
      '=':'=':r -> emit l c TkEqEq 2 r
      '+':r -> emit l c TkPlus 1 r
      '-':r -> emit l c TkMinus 1 r
      '*':r -> emit l c TkStar 1 r
      '/':r -> emit l c TkSlash 1 r
      '=':r -> emit l c TkEq 1 r
      '<':r -> emit l c TkLess 1 r
      '>':r -> emit l c TkGreater 1 r
      '(':r -> emit l c TkLParen 1 r
      ')':r -> emit l c TkRParen 1 r
      _ -> Left (show l ++ ":" ++ show c ++ ": unexpected character " ++ [x])
  emit l c kind width rest = (Token kind (Span l c):) <$> go l (c+width) rest
  kw word = case word of
   "let" -> TkLet
   "in" -> TkIn
   "if" -> TkIf
   "then" -> TkThen
   "else" -> TkElse
   "fn" -> TkFn
   "true" -> TkTrue
   "false" -> TkFalse
   _ -> TkIdent word
