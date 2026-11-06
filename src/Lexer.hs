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
    '=':'>':r -> emit TkFatArrow 2 r
    '=':'=':r -> emit TkEqEq 2 r
    '+':r -> emit TkPlus 1 r
    '-':r -> emit TkMinus 1 r
    '*':r -> emit TkStar 1 r
    '/':r -> emit TkSlash 1 r
    '=':r -> emit TkEq 1 r
    '<':r -> emit TkLess 1 r
    '>':r -> emit TkGreater 1 r
    '(':r -> emit TkLParen 1 r
    ')':r -> emit TkRParen 1 r
    _ -> Left (show l ++ ":" ++ show c ++ ": unexpected character " ++ [x])
  where emit k n r = (Token k (Span l c):) <$> go l (c+n) r
 kw w = case w of
  "let" -> TkLet
  "in" -> TkIn
  "if" -> TkIf
  "then" -> TkThen
  "else" -> TkElse
  "fn" -> TkFn
  "true" -> TkTrue
  "false" -> TkFalse
  _ -> TkIdent w
