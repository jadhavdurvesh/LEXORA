module IRVerify (verifyLabels) where

import qualified Data.Set as S
import IR

-- Validate the control-flow label namespace before a backend consumes IR.
-- Stack-effect and closure-region verification are intentionally separate
-- future passes; this pass guarantees branch targets are well-formed.
verifyLabels :: [Instr] -> Either String ()
verifyLabels instructions = do
  let labels = [name | Label name <- instructions]
      duplicates = duplicatesOf labels
      targets = [name | Jump name <- instructions] ++
                [name | JumpIfFalse name <- instructions]
      known = S.fromList labels
      missing = S.toList (S.difference (S.fromList targets) known)
  case duplicates of
    name:_ -> Left ("duplicate IR label: " ++ name)
    [] -> case missing of
      name:_ -> Left ("undefined IR branch target: " ++ name)
      [] -> Right ()
  where
    duplicatesOf = go S.empty S.empty
    go _ _ [] = []
    go seen emitted (x:xs)
      | S.member x seen =
          if S.member x emitted then go seen emitted xs
          else x : go seen (S.insert x emitted) xs
      | otherwise = go (S.insert x seen) emitted xs
